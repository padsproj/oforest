open Forest_types
open Forest_utility
open Asttypes
open Parsetree
open Ast_helper
open Ppx_type_generators
open Ppx_function_generators

(* Inititalization step *)

let init loc = 
  let cursortype =
    [%type: (unit -> ('a * 'b * CursorMonad.cost)) * (('a * 'b) -> Forest.manifest) * int ][@metaloc loc]
  in
  (* Needs to return a costset -> ('b,costset) *)
  let load_cursor = [%expr let (loadf,_,_) = cursor in 
                        fun c -> let (r,m,nc) = loadf () in
                                  ((r,m),CursorMonad.cost_op c nc)
                 ][@metaloc loc] 
  in
  let manifest_cursor = [%expr 
    let (_,manifest,_) = cursor in
    (fun c -> (manifest (rep,md),c))
  ][@metaloc loc]
  in
  [%str 
   open Forest
   open Core
   module CursorMonad = Forest.CursorMonad(CostMon)
   open CursorMonad
   open CursorMonad.Let_syntax
   type ('a,'b) cursor = [%t cursortype]
   let load (cursor : ('a, 'b) cursor) : ('a * 'b) CursorMonad.t = [%e load_cursor]
   let manifest
       (cursor : ('a, 'b) cursor)
       ((rep,md) : ('a * 'b))
       : Forest.manifest CursorMonad.t = [%e manifest_cursor]
   let get_cursor_id ((_,_,id) : ('a, 'b) cursor) = id
   let cursor_id_to_manifest_table : (int, Forest.manifest) Hashtbl.t = Int.Table.create ()
   let cursor_id = ref 0
  ][@metaloc loc]

(* Auxiliary functions *)
    
(* Checks if there is a delay in a forest_node ast *)
let rec delay_checker (fast : forest_node ast) : bool =
  let fast,loc = get_NaL fast in
  match fast with
  | Thunked(_) -> true
  | File 
  | Link
  | Var(_)
  | Pads(_) -> false
  | Option(fast)
  | Predicate(fast,_) 
  | Url(fast)
  | Comprehension(_,fast,_) 
  | PathExp(_,fast) -> delay_checker fast
  | Directory (dlist) -> List.fold_left (fun acc (_,fast) -> acc || delay_checker fast) false dlist
  | SkinApp(_,_) -> raise_loc_err loc "thunk_checker: Skin applications should not exist here."

     
(* Generates a dependency graph with a list of varnames and their forest_asts at every part that needs
 * something forced.
 * We observe that we should only ever need to force something at a thunk or a directory node,
 * which simplifies the map we need to generate immensely. 
 *)
let rec dependency_grapher (vlist : varname list) (fast : forest_node ast)  : forest_node ast =
  let e,loc = get_NaL fast in
  match e with
  | File 
  | Link 
  | Var _
  | Pads _ -> fast

  | Option(fast) ->
     let newfast = dependency_grapher vlist fast in
     mk_p_ast loc newfast.payload @@ Option newfast
  | Url(fast) -> 
     let newfast = dependency_grapher vlist fast in
     mk_p_ast loc newfast.payload @@ Url newfast

  | Thunked(fast) -> 
     let newfast = dependency_grapher vlist fast in
     mk_ast loc @@ Thunked newfast

  | Predicate(fast,p) -> 
     let newfast = dependency_grapher vlist fast in
     let deplist = List.filter (fun var ->
       find_ident_in_str var p || find_ident_in_str (md_name var) p
     ) vlist
     in
     let depNode =
       match newfast.payload,deplist with
       | PNone,[] -> PNone
       | PRec,[] -> PNone
       | PNone,_  -> PVList(deplist)
       | PRec,_  -> PVList(deplist)
       | PVList(d),_ -> PVList(d @ deplist)
     in
     mk_p_ast loc depNode @@ Predicate (newfast,p)
  | Comprehension(m,fast,pgList) -> 
     let newfast = dependency_grapher vlist fast in
     let deplist = List.fold_left (fun acc pg ->
       match pg with
       | Generator(_,_,InList(_,aq))
       | Guard(_,aq)                 ->
          List.filter (fun var ->
            find_ident_in_str var aq || find_ident_in_str (md_name var) aq
          ) vlist
       | _ -> acc
     ) [] pgList
     in
     let depNode =
       match newfast.payload,deplist with
       | PNone,[] -> PNone
       | PRec,[] -> PNone
       | PNone,_  -> PVList(deplist)
       | PRec,_  -> PVList(deplist)
       | PVList(d),_ -> PVList(d @ deplist)
     in
     mk_p_ast loc depNode @@ Comprehension (m,newfast,pgList)
  | PathExp(pe,fast) -> 
     let newfast = dependency_grapher vlist fast in
     let deplist =
       match pe with
       | OC_Path(_,p) ->
          List.filter (fun var ->
            find_ident_in_str var p || find_ident_in_str (md_name var) p
          ) vlist
       | Variable(_,vname) ->
          List.filter (fun var -> vname = var || vname = md_name var) vlist
       | _ -> []
     in
     let depNode =
       match newfast.payload,deplist with
       | PNone,[] -> PNone
       | PRec,[] -> PNone
       | PNone,_  -> PVList(deplist)
       | PRec,_  -> PVList(deplist)
       | PVList(d),_ -> PVList(d @ deplist)
     in
     mk_p_ast loc depNode @@ PathExp (pe,newfast)

  | Directory (dlist) -> 
     let _,newdlist = List.fold_left (fun (vacc,facc) (lbli,expi) -> 
       let newfast = dependency_grapher vacc expi in
       if delay_checker expi
       then (lbli :: vacc,(lbli,newfast) :: facc)
       else (vacc,(lbli,newfast) :: facc)
     ) (vlist,[]) dlist
     in
     mk_ast loc @@ Directory (List.rev newdlist)
  | SkinApp(_,_) -> raise_loc_err loc "thunk_checker: Skin applications should not exist here."

     
let rec fix_exp (fast : forest_node ast)  : forest_node ast =
  let e,loc = get_NaL fast in
  match e with
  | Thunked(f) ->
     let e2,l2 = get_NaL f in
     begin
       match e2 with
       | PathExp(ptype,f2) ->
          begin match f2.node with
          (* Thunk inside path expression, remove outer thunk *) 
          | Thunked _ -> fix_exp f
          (* Normal path expression, move thunking inside *)
          | _ -> mk_ast l2 @@ PathExp (ptype,fix_exp (mk_ast loc (Thunked f2)))
          end
       | Thunked(f2) -> fix_exp f (* Double thunk, remove outer layer *)
       | _ -> fast (* Normal thunk *)
     end
  | Url fast -> mk_ast loc @@ Url(fix_exp fast)
  | Option fast -> mk_ast loc @@ Option (fix_exp fast)
  | Comprehension(c,fast,pl) -> mk_ast loc @@ Comprehension (c,fix_exp fast,pl)
  | PathExp(pt,fast) -> mk_ast loc @@ PathExp (pt,fix_exp fast)
  | Predicate(fast,aquot) -> mk_ast loc @@ Predicate (fix_exp fast,aquot)
  | Directory(dlist) ->
     let new_dlist = List.map (fun (x,f) -> (x,fix_exp f)) dlist in
     mk_ast loc @@ Directory new_dlist
  | SkinApp _ -> raise_loc_err loc "fix_exp: Skin applications should not exist here."
  | _ -> fast
  
     
let def_generator location (flist : (varname * forest_node ast) list) : structure =
  let def_gen
      ((name,e) : (varname * forest_node ast))
      (types,lets)
      : (type_declaration list * structure) =
    let location = get_loc e in
    let e = Skins.doSkinning (name,e) in
    let e = fix_exp e in (* Fixes path expressions *)
    let _ = Hashtbl.replace forestTbl name e in
    let e =
      match e.node with
      | Thunked(e)      -> e
      | _               -> e
    in
    let e = dependency_grapher [] e in
    let representations, representation = representation_type_generator e in 
    let mds, md = md_type_generator e in
    let representation_declaration =
      typ_make_type_decl location (representation_name name) ~manifest:representation
    in
    let md_declaration = typ_make_type_decl location (md_name name) ~manifest:md in
    let types =
      mds @ [md_declaration] @ representations @ [representation_declaration] @ types
    in
    let load_function_type = 
      [%type: filepath -> 
            ([%t typ_make_constr location (representation_name name)]
             * [%t typ_make_constr location (md_name name)])
      ][@metaloc location]
    in
    let new_function_type =
      [%type:filepath -> 
            ([%t typ_make_constr location (representation_name name)],
             [%t typ_make_constr location (md_name name)])
              cursor
      ][@metaloc location]
    in
    let cost_function_type = 
      [%type: ([%t typ_make_constr location (representation_name name)]
               * [%t typ_make_constr location (md_name name)])  
          -> CursorMonad.cost
      ][@metaloc location]
    in
    let manifest_function_type = 
      [%type: ?swapDirectory:filepath ->
            ([%t typ_make_constr location (representation_name name)]
             * [%t typ_make_constr location (md_name name)])
            ->  Forest.manifest
      ][@metaloc location]
    in
    let costFunction = 
      [%stri
       let rec ([%p pat_make_var location (cost_name name)] : [%t cost_function_type]) =
         [%e cost_function_generator e name]
      ][@metaloc location]
    in
    let manifestFunction = 
      [%stri
       let rec ([%p pat_make_var location (manifest_name name)] : [%t manifest_function_type]) =
         [%e manifest_function_generator false e name]
      ][@metaloc location]
    in
    let loadFunction =  
      [%stri
       let rec ([%p pat_make_var location (load_name name)] : [%t load_function_type]) =
         [%e load_function_generator e name]
       and [%p pat_make_var location (new_nameR name)] : [%t new_function_type] =
         [%e new_function_generator ~first:true e name]
       and [%p pat_make_var location (new_name name)] =
         fun path -> return ([%e exp_make_ident location (new_nameR name)] path)
      ][@metaloc location]
    in
    types, costFunction :: manifestFunction :: loadFunction :: lets 
  in
  let types, lets = List.fold_right def_gen flist ([],[]) in
  (Str.type_ ~loc:location Recursive types) :: lets
