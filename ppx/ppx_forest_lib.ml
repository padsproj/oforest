(* TODO:
open Ppx_pads
*)
open All_types
open Utility
open Asttypes
open Parsetree
open Ast_helper

(* TODO: Make sure thunks work right with predicates and path expressions:
 * E.g. For predicates, force corretly if there's a 'this' or make <f> where p = <f where p>?
 * For Path exps, make _ :: <f> = <_ :: f>?
 *)


(* Inititalization step *)

let init loc = 
  let cursortype = [%type: (unit -> ('a * 'b * CursorMonad.cost)) * (('a * 'b) -> manifest) ][@metaloc loc] in
  (* Need's to return a costset -> ('b,costset) *)
  let load_cur = [%expr let (loadf,_) = cur in 
                        fun c -> let (r,m,nc) = loadf () in
                                  ((r,m),CursorMonad.cost_op c nc)
                 ][@metaloc loc] 
  in
  let mani_cur = [%expr 
    let (_,mani) = cur in
    (fun c -> (mani (rep,md),c))
  ][@metaloc loc]
  in
  [%str 
   open Forest
   module CursorMonad = Forest.CursorMonad(CostMon)
   open CursorMonad  
   (* TODO: Is there a way to make this abstract without breaking all things? *)
   type ('a,'b) cursor = [%t cursortype]
   let load (cur : ('a,'b) cursor) : ('a * 'b) CursorMonad.t = [%e load_cur]
   let manifest (cur : ('a,'b) cursor) ((rep,md) : ('a * 'b)) :manifest CursorMonad.t = [%e mani_cur]
  ][@metaloc loc]

(* Generation functions *)


let rec forest_rep_gen (e: forest_node ast) : (type_declaration list * core_type) =  
  let e,loc = get_NaL e in
  match e with 
  | Thunked(fast) ->
    let decl, field = forest_rep_gen fast in
    let mddecl,md = forest_md_gen fast in
    let cursortype = [%type: ([%t field],[%t md]) cursor ][@metaloc loc] in
    (decl@mddecl, cursortype)
  | Option(fast) -> 
    let decl, field = forest_rep_gen fast in
    (decl, [%type: [%t field] option ][@metaloc loc])
  | Predicate(fast,_) 
  | Url(fast)
  | PathExp(_,fast) ->
    forest_rep_gen fast
  | File ->
    [], [%type: string ][@metaloc loc]
  | Link ->
    [], [%type: filepath ][@metaloc loc] (* Representing a filepath here *)
  | Var(vname) -> 
    [],  typ_make_constr loc (rep_name vname) (*Typ.var (rep_name vname)) *)
  | Pads(vname) ->
    [],  typ_make_constr loc (pads_rep_name vname)
  | Comprehension(Map,fast,_) ->
    let decli,typi = forest_rep_gen fast in
    (decli,[%type: [%t typi] PathMap.t ][@metaloc loc])
  | Comprehension(List,fast,_) ->
    let decli,typi = forest_rep_gen fast in
    (decli,[%type: [%t typi] list ][@metaloc loc])
  | Directory (dlist) ->
    let decls,fields = 
      List.fold_right (fun (labeli,fasti) (decls,fields) ->
        let decli, typi = forest_rep_gen fasti in 
        let fieldi = typ_make_field loc labeli typi in
        (decli@decls, fieldi::fields)) dlist ([],[])
    in
    let name = freshF () in 
    let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
    (decls @ [recType],  typ_make_constr loc name)
  | SkinApp(_,_) -> raise_loc_err loc "forest_rep_gen: Skin applications should not exist here."

and forest_md_gen (e: forest_node ast) : (type_declaration list * core_type) =  
  let e,loc = get_NaL e in
  match e with 
  | Link
  | File
  | Thunked(_) ->
    ([], [%type: unit Forest.forest_md][@metaloc loc])
  | Option(fast) -> 
    let decl, field = forest_md_gen fast in
    (decl, [%type: ([%t field] option) Forest.forest_md ][@metaloc loc])
  | Predicate(fast,_) 
  | Url(fast)
  | PathExp(_,fast) ->
    forest_md_gen fast
  | Var(vname) -> 
    ([], typ_make_constr loc (md_name vname))
  | Pads(vname) ->
    ([], [%type: [%t typ_make_constr loc (pads_md_name vname)]  Forest.forest_md ][@metaloc loc]) 
  | Comprehension(Map,fast,_) ->
    let decli,typi = forest_md_gen fast in
    (decli,[%type: ([%t typi] PathMap.t) Forest.forest_md][@metaloc loc])
  | Comprehension(List,fast,_) ->
    let decli,typi = forest_md_gen fast in
    (decli,[%type: ([%t typi] list) Forest.forest_md ][@metaloc loc])
  | Directory (dlist) ->
    let decls,fields = (*Maybe fold_left? *)
      List.fold_right (fun (labeli,fasti) (decls,fields) ->
        let decli, typi = forest_md_gen fasti in 
        let fieldi = typ_make_field loc (md_name labeli) typi in
        (decli@decls, fieldi::fields)) dlist ([],[])
    in
    let name = freshF () in 
    let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
    (decls @ [recType], [%type: [%t typ_make_constr loc name] Forest.forest_md ][@metaloc loc])
     
  | SkinApp(_,_) -> raise_loc_err loc "forest_md_gen: Skin applications should not exist here."

let rec forest_cost_gen (e : forest_node ast) (vName : string) : Parsetree.expression =
  let e,loc = get_NaL e in
 match e with
  | SkinApp(_,_) -> raise_loc_err loc "forest_cost_gen: Skin applications should not exist here."
  | Var(x) -> exp_make_ident loc (cost_name x) 
  | Thunked(_) 
  | Pads(_)
  | Url(_) -> [%expr fun (r,m) -> CursorMonad.cost_id ][@metaloc loc]

  | PathExp(_,e) 
  | Predicate(e,_) -> forest_cost_gen e vName 

  | File -> [%expr CursorMonad.cost_file ][@metaloc loc]
  | Link -> [%expr CursorMonad.cost_link ][@metaloc loc]

  | Option(e) ->
     [%expr fun (r,m) ->
       match (r,m.data) with 
       | (None,_) -> CursorMonad.cost_id
       | (_,None) -> failwith "Option contained a representation, but no metadata. Should be impossible."
       | (Some(r),Some(m)) -> [%e forest_cost_gen e vName] (r,m)
     ][@metaloc loc]

  (* TODO: Since we do not require commutativity, we need to make sure the
     ordering is correct here *)
  | Comprehension(Map,e,_) ->
     [%expr fun (r,m) -> 
       PathMap.fold (fun key m acc -> 
         let r = PathMap.find key r in
         let c = [%e forest_cost_gen e vName] (r,m) in
         CursorMonad.cost_op acc c
       ) m.data CursorMonad.cost_id
     ][@metaloc loc]
  | Comprehension(List,e,_) ->
     [%expr fun (r,m) -> 
       List.fold_left2 (fun acc r m -> 
         let c = [%e forest_cost_gen e vName] (r,m) in
         CursorMonad.cost_op acc c
       )  CursorMonad.cost_id r (m : [%t Typ.var ~loc vName] forest_md).data
     ][@metaloc loc]
  | Directory (dlist) -> 
     let field_list = List.fold_left (fun acc (lbli,e) ->
       let loc = get_loc e in
       [%expr [%e exp_make_string loc lbli] :: [%e acc]][@metaloc loc]
     ) ([%expr []][@metaloc loc]) dlist 
     in
     let sumExp = List.fold_left (fun acc (lbli,e) -> 
       let loc = get_loc e in
       [%expr CursorMonad.cost_op [%e acc] [%e exp_make_ident loc lbli]][@metaloc loc]
     ) ([%expr CursorMonad.cost_id][@metaloc loc]) dlist
     in
     let last_exp = 
       [%expr let cost1 = [%e sumExp] in 
              CursorMonad.cost_op cost1 (CursorMonad.cost_dir ([%e field_list],m))
       ][@metaloc loc]
     in
     let assignments = List.fold_right (fun (lbli,expi) acc ->
       let loc = get_loc expi in
       [%expr let  [%p pat_make_var loc lbli] =
                ([%e forest_cost_gen expi lbli]) 
                  ([%e exp_make_field_n loc "r" lbli],
                   [%e exp_make_field loc (exp_make_field_n loc "m" "data") (md_name lbli)])
              in [%e acc]
       ][@metaloc loc]
     ) dlist last_exp
     in
     [%expr fun (r,m) -> 
       [%e assignments]
     ][@metaloc loc]

and forest_new_gen ?first:(first=false) (e: forest_node ast) (vName : string) : Parsetree.expression =
  let fNode,loc = get_NaL e in
  match fNode with
  | SkinApp(_,_) -> raise_loc_err loc "forest_new_gen: Skin applications should not exist here."
  | Var(x) -> exp_make_ident loc (new_nameR x) 
  | Thunked(e) -> forest_new_gen e vName
  (*
  | PathExp( ptype,Var(l2,x)) -> 
    (match ptype with
    | Constant(pathi) ->
      [%expr: (fun path -> $lid:new_nameR x$ (Filename.concat path $str:pathi$ ))] 
    | Variable(pathi) ->
      [%expr: (fun path -> $lid:new_nameR x$ (Filename.concat path $lid:pathi$ ))] 
    | OCaml_Expr(e) -> 
      [%expr: (fun path -> $lid:new_nameR x$ (Filename.concat path $e$ ))] 
      )
     *)
  | File
  | Pads(_)
  | Link 
  | PathExp(_,_)
  | Option(_)
  | Directory (_)
  | Predicate(_,_) 
  | Url(_) 
  | Comprehension(_,_,_) ->
       [%expr
           (fun path ->
             let lfunc () =
               let (rep,md) = 
                 [%e
		  let final_e = if first then exp_make_ident loc (load_name vName) else forest_load_gen e vName in  
                     match e.payload with
                     | PVList(deplist) -> 
                        List.fold_right (fun v acc ->
                          let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
                          [%expr let [%p names] = [%e exp_make_ident loc @@ ucur_name v] () in 
                                 [%e acc]][@metaloc loc]
                        ) deplist final_e
		     | _ -> final_e						 
                 ] path
               in
               let cost = 
                 [%e if first then exp_make_ident loc (cost_name vName) else forest_cost_gen e vName] (rep,md)
               in
               (rep,md,cost)
             in
             let mani (r,m) = 
               (* TODO: Make sure 'inside' variable should be true *)
               let manifest = 
                 [%e if first then exp_make_ident loc (manifest_name vName) else forest_manifest_gen true e vName] (r,m)
               in
               manifest 
               (*
               let sfunc ?dirname:(dirname="") ?basename:(basename="") () = 
               (* Makes the store function update dethunked value basically *)
                 if dirname = "" && basename = ""
                 then manifest.storeFunc ()
                 else if dirname = ""
                 then manifest.storeFunc ~basename:basename ()
                 else if basename = ""
                 then manifest.storeFunc ~dirname:dirname ()
                 else manifest.storeFunc ~basename:basename ~dirname:dirname ()
                 in
                 {tmppath = manifest.tmppath; storeFunc = manifest.sfunc; errors = manifest.errors}
               *)
             in
             (lfunc,mani))
       ][@metaloc loc]

and forest_load_gen (e: forest_node ast) (vName : string) : Parsetree.expression = 
  let e,loc = get_NaL e in
  let add_timing e = 
    [%expr fun path -> let currTime = Core.Time.now () in 
                        let (r,m) = [%e e] in
                        (r, { m with load_time = Core.Time.abs_diff currTime (Core.Time.now ()) } ) ] [@metaloc loc]
  in
  match e with
  | File -> [%expr Forest.load_file ] [@metaloc loc]
  | Link -> [%expr Forest.load_link ] [@metaloc loc]
  | Var(x) -> exp_make_ident loc (load_name x)
  | Pads(s) -> add_timing
    [%expr PadsLoader.pads_load_for_forest
        [%e exp_make_ident loc (pads_parse_name s)] 
        path ][@metaloc loc]
  | Option(e) ->
    add_timing
    [%expr
        if Sys.file_exists path
        then let (r,m) =  ([%e forest_load_gen e vName] path) in
             let new_md = { num_errors = m.num_errors;
                            error_msg = m.error_msg;
                            info = Forest.get_md_info path;
                            load_time = no_time;
                            data=Some(m)}
             in
             (Some r,new_md)
        else
          (None, Forest.empty_md None path) ][@metaloc loc]
  | PathExp(ptype,e) ->
     add_timing
       (match ptype with
       | Constant(loc,pathi) -> 
          [%expr [%e forest_load_gen e vName] (Filename.concat path [%e exp_make_string loc pathi])][@metaloc loc]
       | Variable(loc,pathi) ->
          [%expr [%e forest_load_gen e vName] (Filename.concat path [%e exp_make_ident loc pathi])][@metaloc loc]
       | OC_Path(loc,pathi) ->
          [%expr [%e forest_load_gen e vName] (Filename.concat path [%e exp_make_ocaml loc pathi])][@metaloc loc]
       )
  | Predicate(e,b) ->
    add_timing
    [%expr
        begin [@warning "-26"]
        let (this,this_md) = [%e forest_load_gen e vName] path in
        let this_att = match this_md.info with
          | Some(info) -> info
          | None -> Forest.get_att_info path
        in
        if [%e exp_make_ocaml loc b] then (this,this_md) else 
          let new_md = {this_md with 
            error_msg = "Predicate failure" ::  this_md.error_msg;
            num_errors =  this_md.num_errors + 1}
          in
          (this,new_md)
      end][@metaloc loc]
  | Directory (dlist) ->
    let ctype = [%type: ([%t typ_make_constr loc (rep_name vName)] * 
                             [%t typ_make_constr loc (md_name vName)])
                   ][@metaloc loc]
    in

(* 
*)

    let rep_assgn = List.map (fun (lbli,_) -> lbli,lbli) dlist in 
    let md_assgn = List.map (fun (lbli,_) -> (md_name lbli),(md_name lbli)) dlist in
    let numErr = List.fold_left (fun acc (lbli,e) ->
      let loc = get_loc e in
      [%expr [%e exp_make_field_n loc (md_name lbli) "num_errors"] + [%e acc]][@metaloc loc]
    ) ([%expr 0][@metaloc loc]) dlist
    in
    let errMsg = List.fold_left (fun acc (lbli,e) ->  
      let loc = get_loc e in
      [%expr [%e exp_make_field_n loc (md_name lbli) "error_msg"] @ [%e acc] ][@metaloc loc]
    ) ([%expr []][@metaloc loc]) dlist
    in
    let init_exp = 
      [%expr let (r,m) : [%t ctype] = 
               ([%e exp_make_record_s loc rep_assgn], 
                { num_errors = [%e numErr];
                  error_msg = [%e errMsg];
		  info = Forest.get_md_info path; 
                  load_time = no_time;
		  data = [%e exp_make_record_s loc md_assgn] })
             in (r,m)][@metaloc loc]
    in
    let final_exp = 
      List.fold_right
        (fun (lbli,expi) acc ->
          let loc = get_loc expi in
          let names = [%pat? ([%p pat_make_var loc lbli],[%p pat_make_var loc (md_name lbli)])][@metaloc loc] in
	  let final_e = [%expr ([%e forest_load_gen expi vName] path,
				(fun () -> [%e forest_uninc_load_gen expi vName] path)) ][@metaloc loc]
	  in
	  let final_e =
	    match expi.payload with
            | PVList(deplist) -> 
               List.fold_right (fun v acc ->
		 let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
		 [%expr let [%p names] = [%e exp_make_ident loc @@ ucur_name v] () in 
			[%e acc]][@metaloc loc]
               ) deplist final_e
            | _ -> final_e
	  in
	  [%expr let ([%p names],[%p pat_make_var loc (ucur_name lbli)]) = [%e final_e] in 
		 [%e acc]][@metaloc loc]
        )
        dlist
        init_exp
    in
    add_timing
    [%expr
          if Sys.file_exists path && Sys.is_directory path
          then [%e final_exp]
          else failwith (Printf.sprintf "Path %s is not a directory" path)          
      ][@metaloc loc]

  | Comprehension(cType,e,pgList) -> 
     let load_e = forest_load_gen e vName in
     let vlist,fListBool = List.fold_right (fun gen (acc,bacc) -> match gen with
       | Generator(_,bvar,Matches(_)) -> bvar :: acc, true
       | Generator(_,bvar,InList(_)) -> bvar :: acc, bacc
       | _ -> (acc,bacc)) pgList ([],false)
     in 
     let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
     let make_exp pg (vacc,eacc) =
       match pg with
       | Generator(loc,mvar,Matches(locm,reg)) ->
          let cexpr = [%expr let [%p pat_make_var loc (list_name mvar)] =  List.fold_left (fun acc fname ->
            if regMatch fname 
            then fname :: acc 
            else acc
          ) [] fileList in [%e eacc]][@metaloc loc]
          in
          (mvar::vacc,
            match reg with
            | Regex(loc,reg) -> [%expr let regMatch = Forest.regexp_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr] ][@metaloc loc]
            | Glob(loc,reg) ->  [%expr let regMatch = Forest.glob_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr]][@metaloc loc]
          )
       | Generator(loc,mvar,InList(locm, lst)) -> (* lst should be an expression that evaluates to a list *)
          (mvar::vacc,
           [%expr let [%p pat_make_var loc (list_name mvar)] = [%e exp_make_ocaml locm lst] in [%e eacc]][@metaloc loc])
       | Guard(loc,aq) ->         
	  let depVars = List.fold_left (fun acc bvar ->
            if find_ident_in_str (att_name bvar) aq
            then bvar :: acc
            else if find_ident_in_str (md_name bvar) aq 
            then bvar :: acc
            else if find_ident_in_str bvar aq 
            then bvar :: acc
            else acc
	  ) [] (diff vlist vacc) (* Because we fold right, we need to check the ones we haven't seen yet *)
	  in
	  let bigExpr = 
            let pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple =
              (* Are there 0, 1, or more dependent variables? *)
              match depVars with
              | [] -> raise_loc_err loc "Comprehension guards must relate to some generated variable" 
                      (*TODO: Not sure if this is the right thing to do *)
              | id :: [] ->
                 let pListTuple = pat_make_var loc (list_name id) in
                 let emptyAccTuple =  [%expr []][@metaloc loc] in
                 let pAccTuple = pat_make_var loc (acc_name id) in
                 let falseTuple = exp_make_ident loc (acc_name id) in
                 let trueTuple =  
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              | _ ->
                 let pListTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (list_name id)) depVars) in
                 let emptyAccTuple =  Exp.tuple ~loc (List.map (fun id -> [%expr []][@metaloc loc]) depVars) in
                 let pAccTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (acc_name id)) depVars) in
                 let falseTuple = Exp.tuple ~loc (List.map (fun id -> exp_make_ident loc (acc_name id)) depVars) in
                 let trueTuple =  Exp.tuple ~loc (List.map (fun id -> 
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 ) depVars)  
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              in
              let bigExpr =
	        List.fold_left (fun accEx bvar ->  
	          let att = find_ident_in_str (att_name bvar) aq in
	          let rep = find_ident_in_str bvar aq in
	          let md = find_ident_in_str (md_name bvar) aq in
	          let newExp = if att
		    then [%expr let [%p pat_make_var loc (att_name bvar)] = 
                                  Forest.get_att_info (Filename.concat path [%e exp_make_ident loc bvar]) 
                                in [%e accEx]][@metaloc loc]
		    else accEx
	          in
                  let names = Pat.tuple ~loc [pat_make_var loc bvar; pat_make_var loc (md_name bvar)] in
	          let newExp = if rep || md
		    then [%expr let [%p names] = [%e forest_load_gen e vName] path in [%e newExp]][@metaloc loc]
                    else newExp
	          in
	          [%expr List.fold_left (fun [%p pAccTuple] [%p pat_make_var loc bvar] -> [%e newExp])
                      [%e falseTuple] [%e exp_make_ident loc (list_name bvar)]][@metaloc loc]
	        ) ([%expr if [%e exp_make_ocaml loc aq] then [%e trueTuple] else [%e falseTuple] ][@metaloc loc]) depVars
              in
              [%expr let [%p pAccTuple] = [%e emptyAccTuple] in
                     let [%p pListTuple] = [%e bigExpr] in 
                     [%e eacc]][@metaloc loc]             
	  in
	  vacc, bigExpr
     in
    (* Things dependent on if it's a map or a list:
     * initExp: This is the expression that does the actual loading and makes a single rep and md and adds
     * them to the accumulator
     * initAcc: This is the initial accumulator (empty pathmaps or empty lists)
     * new_rm: This determines how to get the final main md by tallying up errors and such as well as the rep.
     *)
    let (initExp,initAcc,new_rm) = 
      match cType with
      | Map ->
         ([%expr let (r,m) = [%e load_e] path in 
                 match m.info with 
                 | None -> failwith "Loading failed in comprehension"
                 | Some(info) ->  (PathMap.add info.full_path r rep,PathMap.add info.full_path m md) ][@metaloc loc],
          [%expr (PathMap.empty,PathMap.empty) ][@metaloc loc],
          [%expr (rep,{
            num_errors = PathMap.fold (fun _ md a -> md.num_errors + a) md 0; 
            error_msg =  PathMap.fold (fun _ md a -> md.error_msg @ a) md [];
            info = Forest.get_md_info path;
            load_time = no_time;
            data = md;
          }) ][@metaloc loc])
      | List ->
         ([%expr let (r,m) = [%e load_e] path in (r::rep,m::md)][@metaloc loc],
          [%expr ([],[]) ][@metaloc loc],
          [%expr (List.rev rep,{
            num_errors = List.fold_left (fun a md -> md.num_errors + a) 0 md; 
            error_msg =  List.fold_left (fun a md -> md.error_msg @ a) [] md;
            info = Forest.get_md_info path;
            load_time = no_time;
            data = List.rev md;
          }) ][@metaloc loc])
    in
    (* Creates the folds that does the final accumulation. Does one fold per generator
     * and in the inner most fold it does the loading of a single rep and md (initExp).
     *)
    let get_reps_mds_exp = 
        List.fold_right (fun bv expAcc ->
          [%expr List.fold_left (fun (rep,md) [%p pat_make_var loc bv] ->
            [%e expAcc]
          ) [%e initAcc] [%e exp_make_ident loc (list_name bv)]][@metaloc loc])
          vlist initExp 
    in
    let _,mid_exp = List.fold_right make_exp 
      pgList
      ([],[%expr let (rep,md) = [%e get_reps_mds_exp] in [%e new_rm]][@metaloc loc])
    in
    (* If we need a file list (because we have a matching), we get it first *)
    add_timing 
      (if fListBool
       then [%expr let fileList = Array.to_list (Sys.readdir path) in [%e  mid_exp]][@metaloc loc]
       else mid_exp)
  | Url(e) -> add_timing 
     [%expr (* Note that it assumes you're getting a directory if it's more than 1 file *)
      let tmpdir = Filename.temp_file "forest-" "" in
      let _ = Sys.remove tmpdir in
      let _ = Unix.mkdir tmpdir 0o770 in
      let errCode = Sys.command ("wget -P " ^ tmpdir ^ " " ^ path) in
      let fNames = Array.to_list (Sys.readdir tmpdir) in
      let path = if List.length fNames > 1 then tmpdir else Filename.concat tmpdir (List.hd fNames) in (*TODO: What if it fails? *)
      let (r,md) = [%e forest_load_gen e vName] path in
      let newmd = if errCode = 0 then md
        else {data = md.data;
              info = md.info;
              error_msg = [ (Printf.sprintf "Undetermined wget error with code %d" errCode) :: md.error_msg ];
              load_time = no_time;
              num_errors = md.num_errors + 1}
      in
      (r,newmd) ][@metaloc loc] (*TODO: FIX THIS/Make sure it is correct *)
  | Thunked(e) -> 
     add_timing [%expr ([%e forest_new_gen e vName] path,Forest.unit_md path) ][@metaloc loc]
  | SkinApp(_,_) -> raise_loc_err loc "forest_load_gen: Skin applications should not exist here."


and forest_uninc_load_gen (e: forest_node ast) (vName : string) : Parsetree.expression = 
  let e,loc = get_NaL e in
  let add_timing e = 
    [%expr fun path -> let currTime = Core.Time.now () in 
                        let (r,m) = [%e e] in
                        (r, { m with load_time = Core.Time.abs_diff currTime (Core.Time.now ()) } ) ] [@metaloc loc]
  in
  match e with
  | File -> [%expr Forest.load_file ] [@metaloc loc]
  | Link -> [%expr Forest.load_link ] [@metaloc loc]
  | Var(x) -> exp_make_ident loc (load_name x)
  | Pads(s) -> add_timing
    [%expr PadsLoader.pads_load_for_forest
        [%e exp_make_ident loc (pads_parse_name s)] 
        path ][@metaloc loc]
  | Option(e) ->
    add_timing
    [%expr
        if Sys.file_exists path
        then let (r,m) =  ([%e forest_uninc_load_gen e vName] path) in
             let new_md = { num_errors = m.num_errors;
                            error_msg = m.error_msg;
                            info = Forest.get_md_info path;
                            load_time = no_time;
                            data=Some(m)}
             in
             (Some r,new_md)
        else
          (None, Forest.empty_md None path) ][@metaloc loc]
  | PathExp(ptype,e) ->
     add_timing
       (match ptype with
       | Constant(loc,pathi) -> 
          [%expr [%e forest_uninc_load_gen e vName] (Filename.concat path [%e exp_make_string loc pathi])][@metaloc loc]
       | Variable(loc,pathi) ->
          [%expr [%e forest_uninc_load_gen e vName] (Filename.concat path [%e exp_make_ident loc pathi])][@metaloc loc]
       | OC_Path(loc,pathi) ->
          [%expr [%e forest_uninc_load_gen e vName] (Filename.concat path [%e exp_make_ocaml loc pathi])][@metaloc loc]
       )
  | Predicate(e,b) ->
    add_timing
    [%expr
        begin [@warning "-26"]
        let (this,this_md) = [%e forest_uninc_load_gen e vName] path in
        let this_att = match this_md.info with
          | Some(info) -> info
          | None -> Forest.get_att_info path
        in
        if [%e exp_make_ocaml loc b] then (this,this_md) else 
          let new_md = {this_md with 
            error_msg = "Predicate failure" ::  this_md.error_msg;
            num_errors =  this_md.num_errors + 1}
          in
          (this,new_md)
      end][@metaloc loc]
  | Directory (dlist) ->
    let ctype = [%type: ([%t typ_make_constr loc (rep_name vName)] * 
                             [%t typ_make_constr loc (md_name vName)])
                   ][@metaloc loc]
    in
    let rep_assgn = List.map (fun (lbli,_) -> lbli,lbli) dlist in 
    let md_assgn = List.map (fun (lbli,_) -> (md_name lbli),(md_name lbli)) dlist in
    let numErr = List.fold_left (fun acc (lbli,e) ->
      let loc = get_loc e in 
      [%expr [%e exp_make_field_n loc (md_name lbli) "num_errors"] + [%e acc]][@metaloc loc]
    ) ([%expr 0][@metaloc loc]) dlist
    in
    let errMsg = List.fold_left (fun acc (lbli,e) ->  
      let loc = get_loc e in 
      [%expr [%e exp_make_field_n loc (md_name lbli) "error_msg"] @ [%e acc] ][@metaloc loc]
    ) ([%expr []][@metaloc loc]) dlist
    in
    let init_exp = 
      [%expr let (r,m) : [%t ctype] = 
               ([%e exp_make_record_s loc rep_assgn], 
                { num_errors = [%e numErr];
                  error_msg = [%e errMsg];
		  info = Forest.get_md_info path; 
                  load_time = no_time;
		  data = [%e exp_make_record_s loc md_assgn] })
             in (r,m)][@metaloc loc]
    in
    let final_exp = 
      List.fold_right
        (fun (lbli,expi) acc ->
          let loc = get_loc expi in 
          let names = [%pat? ([%p pat_make_var loc lbli],[%p pat_make_var loc (md_name lbli)])][@metaloc loc] in
          [%expr let [%p names] = [%e forest_uninc_load_gen expi vName] path in [%e acc]][@metaloc loc]
        )
        dlist
        init_exp
    in
    add_timing
    [%expr
          if Sys.file_exists path && Sys.is_directory path
          then [%e final_exp]
          else failwith (Printf.sprintf "Path %s is not a directory" path)          
      ][@metaloc loc]

  | Comprehension(cType,e,pgList) -> 
     let load_e = forest_uninc_load_gen e vName in
     let vlist,fListBool = List.fold_right (fun gen (acc,bacc) -> match gen with
       | Generator(_,bvar,Matches(_)) -> bvar :: acc, true
       | Generator(_,bvar,InList(_)) -> bvar :: acc, bacc
       | _ -> (acc,bacc)) pgList ([],false)
     in 
     let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
     let make_exp pg (vacc,eacc) =
       match pg with
       | Generator(loc,mvar,Matches(locm,reg)) ->
          let cexpr = [%expr let [%p pat_make_var loc (list_name mvar)] =  List.fold_left (fun acc fname ->
            if regMatch fname 
            then fname :: acc 
            else acc
          ) [] fileList in [%e eacc]][@metaloc loc]
          in
          (mvar::vacc,
            match reg with
            | Regex(loc,reg) -> [%expr let regMatch = Forest.regexp_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr] ][@metaloc loc]
            | Glob(loc,reg) ->  [%expr let regMatch = Forest.glob_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr]][@metaloc loc]
          )
       | Generator(loc,mvar,InList(locm, lst)) -> (* lst should be an expression that evaluates to a list *)
          (mvar::vacc,
           [%expr let [%p pat_make_var loc (list_name mvar)] = [%e exp_make_ocaml locm lst] in [%e eacc]][@metaloc loc])
       | Guard(loc,aq) ->         
	  let depVars = List.fold_left (fun acc bvar ->
            if find_ident_in_str (att_name bvar) aq
            then bvar :: acc
            else if find_ident_in_str (md_name bvar) aq 
            then bvar :: acc
            else if find_ident_in_str bvar aq 
            then bvar :: acc
            else acc
	  ) [] (diff vlist vacc) (* Because we fold right, we need to check the ones we haven't seen yet *)
	  in
	  let bigExpr = 
            let pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple =
              (* Are there 0, 1, or more dependent variables? *)
              match depVars with
              | [] -> raise_loc_err loc "Comprehension guards must relate to some generated variable" 
                      (*TODO: Not sure if this is the right thing to do *)
              | id :: [] ->
                 let pListTuple = pat_make_var loc (list_name id) in
                 let emptyAccTuple =  [%expr []][@metaloc loc] in
                 let pAccTuple = pat_make_var loc (acc_name id) in
                 let falseTuple = exp_make_ident loc (acc_name id) in
                 let trueTuple =  
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              | _ ->
                 let pListTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (list_name id)) depVars) in
                 let emptyAccTuple =  Exp.tuple ~loc (List.map (fun id -> [%expr []][@metaloc loc]) depVars) in
                 let pAccTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (acc_name id)) depVars) in
                 let falseTuple = Exp.tuple ~loc (List.map (fun id -> exp_make_ident loc (acc_name id)) depVars) in
                 let trueTuple =  Exp.tuple ~loc (List.map (fun id -> 
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 ) depVars)  
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              in
              let bigExpr =
	        List.fold_left (fun accEx bvar ->  
	          let att = find_ident_in_str (att_name bvar) aq in
	          let rep = find_ident_in_str bvar aq in
	          let md = find_ident_in_str (md_name bvar) aq in
	          let newExp = if att
		    then [%expr let [%p pat_make_var loc (att_name bvar)] = 
                                  Forest.get_att_info (Filename.concat path [%e exp_make_ident loc bvar]) 
                                in [%e accEx]][@metaloc loc]
		    else accEx
	          in
                  let names = Pat.tuple ~loc [pat_make_var loc bvar; pat_make_var loc (md_name bvar)] in
	          let newExp = if rep || md
		    then [%expr let [%p names] = [%e forest_uninc_load_gen e vName] path in [%e newExp]][@metaloc loc]
                    else newExp
	          in
	          [%expr List.fold_left (fun [%p pAccTuple] [%p pat_make_var loc bvar] -> [%e newExp])
                      [%e falseTuple] [%e exp_make_ident loc (list_name bvar)]][@metaloc loc]
	        ) ([%expr if [%e exp_make_ocaml loc aq] then [%e trueTuple] else [%e falseTuple] ][@metaloc loc]) depVars
              in
              [%expr let [%p pAccTuple] = [%e emptyAccTuple] in
                     let [%p pListTuple] = [%e bigExpr] in 
                     [%e eacc]][@metaloc loc]             
	  in
	  vacc, bigExpr
     in
    (* Things dependent on if it's a map or a list:
     * initExp: This is the expression that does the actual loading and makes a single rep and md and adds
     * them to the accumulator
     * initAcc: This is the initial accumulator (empty pathmaps or empty lists)
     * new_rm: This determines how to get the final main md by tallying up errors and such as well as the rep.
     *)
    let (initExp,initAcc,new_rm) = 
      match cType with
      | Map ->
         ([%expr let (r,m) = [%e load_e] path in 
                 match m.info with 
                 | None -> failwith "Loading failed in comprehension"
                 | Some(info) ->  (PathMap.add info.full_path r rep,PathMap.add info.full_path m md) ][@metaloc loc],
          [%expr (PathMap.empty,PathMap.empty) ][@metaloc loc],
          [%expr (rep,{
            num_errors = PathMap.fold (fun _ md a -> md.num_errors + a) md 0; 
            error_msg =  PathMap.fold (fun _ md a -> md.error_msg @ a) md [];
            info = Forest.get_md_info path;
            load_time = no_time;
            data = md;
          }) ][@metaloc loc])
      | List ->
         ([%expr let (r,m) = [%e load_e] path in (r::rep,m::md)][@metaloc loc],
          [%expr ([],[]) ][@metaloc loc],
          [%expr (List.rev rep,{
            num_errors = List.fold_left (fun a md -> md.num_errors + a) 0 md; 
            error_msg =  List.fold_left (fun a md -> md.error_msg @ a) [] md;
            info = Forest.get_md_info path;
            load_time = no_time;
            data = List.rev md;
          }) ][@metaloc loc])
    in
    (* Creates the folds that does the final accumulation. Does one fold per generator
     * and in the inner most fold it does the loading of a single rep and md (initExp).
     *)
    let get_reps_mds_exp = 
        List.fold_right (fun bv expAcc ->
          [%expr List.fold_left (fun (rep,md) [%p pat_make_var loc bv] ->
            [%e expAcc]
          ) [%e initAcc] [%e exp_make_ident loc (list_name bv)]][@metaloc loc])
          vlist initExp 
    in
    let _,mid_exp = List.fold_right make_exp 
      pgList
      ([],[%expr let (rep,md) = [%e get_reps_mds_exp] in [%e new_rm]][@metaloc loc])
    in
    (* If we need a file list (because we have a matching), we get it first *)
    add_timing 
      (if fListBool
       then [%expr let fileList = Array.to_list (Sys.readdir path) in [%e  mid_exp]][@metaloc loc]
       else mid_exp)
  | Url(e) -> add_timing 
     [%expr (* Note that it assumes you're getting a directory if it's more than 1 file *)
      let tmpdir = Filename.temp_file "forest-" "" in
      let _ = Sys.remove tmpdir in
      let _ = Unix.mkdir tmpdir 0o770 in
      let errCode = Sys.command ("wget -P " ^ tmpdir ^ " " ^ path) in
      let fNames = Array.to_list (Sys.readdir tmpdir) in
      let path = if List.length fNames > 1 then tmpdir else Filename.concat tmpdir (List.hd fNames) in (*TODO: What if it fails? *)
      let (r,md) = [%e forest_uninc_load_gen e vName] path in
      let newmd = if errCode = 0 then md
        else {data = md.data;
              info = md.info;
              error_msg = [ (Printf.sprintf "Undetermined wget error with code %d" errCode) :: md.error_msg ];
              load_time = no_time;
              num_errors = md.num_errors + 1}
      in
      (r,newmd) ][@metaloc loc] (*TODO: FIX THIS/Make sure it is correct *)
  | Thunked(e) ->
     let final_e = forest_uninc_load_gen e vName in
     begin
       match e.payload with
       | PVList(deplist) -> 
          List.fold_right (fun v acc ->
	    let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
	    [%expr let [%p names] = [%e exp_make_ident loc @@ ucur_name v] () in 
		   [%e acc]][@metaloc loc]
          ) deplist final_e
       | _ -> final_e
     end
  | SkinApp(_,_) -> raise_loc_err loc "forest_load_gen: Skin applications should not exist here."

and forest_manifest_gen (inside:bool) (e: forest_node ast) (vName : string) : Parsetree.expression =
  let e,loc = get_NaL e in
  (* TODO: Figure out what kinda errors we might have and implement them
   * - MD errors (No path, no permissions, etc? Or invalid of all the above) - Make a check_md function
   * -- Also check that dirname exists?
   * TODO: Fix storage during errors
   *)
  let main_exp = match e with
      | SkinApp(_,_) -> raise_loc_err loc "forest_manifest_gen: Skin applications should not exist here."
      | Thunked(_) -> [%expr ()][@metaloc loc] (* Will get thrown away *)
      | Var(x) -> [%expr [%e exp_make_ident loc (manifest_name x)] ~tmpdir:tmpdir (rep,md)][@metaloc loc]
      | PathExp(ptype,e) -> [%expr [%e forest_manifest_gen true e vName] ~tmpdir:tmpdir (rep,md)][@metaloc loc] 
      (*TODO: Should this depend on ptype and whatnot, or is that all already in the meta data? I think it is...*)
      | File ->
         [%expr
          let dirname = Filename.dirname info.full_path in
          let basename = Filename.basename info.full_path in
          let tmppath = Filename.concat tmpdir basename in
          let _ = Forest.store_file (rep,md) tmppath in
          let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () =
            let storepath = Filename.concat dirname basename in
            let _ = if Sys.file_exists storepath then Sys.remove storepath else () in
            Sys.rename tmppath storepath
          in
          { errors = []; storeFunc = sfunc; tmppath = tmpdir }
         ][@metaloc loc]
      | Link ->
         [%expr
          let dirname = Filename.dirname info.full_path in
          let basename = Filename.basename info.full_path in
          let tmppath = Filename.concat tmpdir basename in
          let _ = Forest.store_link (rep,md) tmppath in
          (* TODO: if not (Sys.file_exists rep) then Need to indicate failure here: nothing to link with *)
          (* TODO: This can generate errors we should catch *)
          (* TODO: Note that store_link may have issues with relative pathing... Maybe not though. 
           * Do we want to indicate failure if they try to link with something not existent?
           * Requires some extra info about the pathing...
           * Re-look at store_link 
           *)
          let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () =
            let storepath = Filename.concat dirname basename in
            let _ = if Sys.file_exists storepath then Unix.unlink storepath else () in
            Sys.rename tmppath storepath
          in
          { errors = []; storeFunc = sfunc; tmppath = tmpdir }
         ][@metaloc loc]
      | Url(e) ->
          [%expr
           let mani = [%e forest_manifest_gen true e vName] ~tmpdir:tmpdir (rep,md) in
           let sfunc ?dirname:(dirname="") ?basename:(basename="") () =
             if dirname = "" || basename = ""
             then Printf.eprintf "Impossible to store a URL back to its initial path. Use store_at function.\nDoing nothing.\n"
             else mani.storeFunc ~dirname:dirname ~basename:basename ()
           in
           { errors = mani.errors; storeFunc = sfunc; tmppath = mani.tmppath }
          ][@metaloc loc]
      | Pads(x) ->
         [%expr
          let dirname = Filename.dirname info.full_path in
          let basename = Filename.basename info.full_path in
          let mani = [%e exp_make_ident loc (pads_manifest_name x)] (rep, md.data) in
          let errors = List.map (fun y -> (basename, PadsError y)) mani.pads_man_errors in
          let tmppath = Filename.concat tmpdir basename in
          let _ = PadsLoader.pads_store mani tmppath in
          let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () =
            let storepath = Filename.concat dirname basename in
            let _ = if Sys.file_exists storepath then Sys.remove storepath else () in
            Sys.rename tmppath storepath
          in 
          { errors = errors; storeFunc = sfunc; tmppath = tmpdir }
         ][@metaloc loc]
      | Predicate(e,b) -> [%expr [%e forest_manifest_gen true e vName] ~tmpdir:tmpdir (rep, md)][@metaloc loc]
         (* TODO: Add checks back in
         [%expr
             begin [@warning "-26"]
             let (this,this_md) = (rep,md) in
             let this_att = match this_md.info with
               | Some(info) -> info
               | None -> empty_info ""
             in
             let mani = [%e forest_manifest_gen true e vName] ~tmpdir:tmpdir (rep, md) in
             if [%e exp_make_ocaml loc b] 
             then mani 
             else 
               {storeFunc = mani.storeFunc;
                tmppath = mani.tmppath;
                errors = ("",PredicateFail) :: mani.errors
               }
             end][@metaloc loc] 
         *)
      | Option(e) ->
         [%expr
          let dirname = Filename.dirname info.full_path in
          let basename = Filename.basename info.full_path in
          match (rep, md.data) with 
          |  (None, _) -> 
              let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () =
                let storepath = Filename.concat dirname basename in
                if Sys.file_exists storepath then Sys.remove storepath else ()
              in
              { errors = []; storeFunc = sfunc; tmppath = tmpdir }
          | (Some r, Some m) -> [%e forest_manifest_gen true e vName] ~tmpdir:tmpdir (r, m)
          | (Some r, None) ->  
             let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () = () in
             { errors = [(basename,Opt_MD_Rep_Inconsistency)] ;storeFunc = sfunc; tmppath = tmpdir }
         ][@metaloc loc]
      | Comprehension(map,e,plist) -> 
         let exp =
         [%expr 
          let dirname = Filename.dirname info.full_path in
          let basename = Filename.basename info.full_path in
          let maniList = List.map ([%e forest_manifest_gen true e vName] ~tmpdir:tmpdir) (List.combine repList mdList) in
          let errlist = List.fold_left (fun acc m -> m.errors @ acc) [] maniList in
          let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () = 
            let storepath = Filename.concat dirname basename in
            [%e 
             let e,loc = get_NaL e in
             match e with
            | Comprehension(_) -> 
               [%expr List.iter (fun m -> m.storeFunc ~dirname:dirname ~basename:basename ()) maniList][@metaloc loc]
            | _ -> 
               [%expr List.iter (fun m -> m.storeFunc ~dirname:storepath ()) maniList][@metaloc loc]
            ]
          in
          {errors = errlist; storeFunc = sfunc; tmppath = tmpdir} 
         ][@metaloc loc]
         in
         if map = Map
         then [%expr 
               let repList = List.map snd (PathMap.bindings rep) in
               let mdList = List.map snd (PathMap.bindings md.data) in
               [%e exp]][@metaloc loc]
         else [%expr 
               let repList = rep in
               let mdList = md.data in
               [%e exp]][@metaloc loc]
      | Directory (dlist) ->  
         let final_mani = 
           [%expr 
            let sfunc ?dirname:(dirname=dirname) ?basename:(basename=basename) () =
              let storepath = Filename.concat dirname basename in
              if Sys.file_exists storepath
              then if Sys.is_directory storepath
                then 
                  [%e List.fold_right
                    (fun (lbli,expi) rep -> 
                      let (expi,loc) = get_NaL expi in
                      match expi with
                      | Comprehension(_) ->
                         [%expr let _ = [%e exp_make_field_n loc (manifest_name lbli) "storeFunc"] 
                                  ~dirname:dirname ~basename:basename () in [%e rep]
                         ][@metaloc loc]
                      | _ ->  
                         [%expr let _ = [%e exp_make_field_n loc (manifest_name lbli) "storeFunc"] 
                                  ~dirname:storepath () in [%e rep]
                         ][@metaloc loc]
                    ) dlist [%expr Unix.rmdir tmppath][@metaloc loc] ]
                else failwith (Printf.sprintf "Non-directory file exists at path %s" storepath)
              else Sys.rename tmppath storepath
            in
            {errors = errlist; storeFunc = sfunc; tmppath = oldtmpdir } 
           ][@metaloc loc]
         in
         [%expr
          let dirname = Filename.dirname info.full_path in
          let basename = Filename.basename info.full_path in  
          let tmppath = Filename.concat tmpdir basename in
          let errlist = if Sys.file_exists info.full_path
            then if Sys.is_directory info.full_path
              then []
              else [(basename,Dir_Filename_Overlap)]
            else [] in
          let _ = Unix.mkdir tmppath info.permissions in
          let oldtmpdir = tmpdir in
          let tmpdir = tmppath in
          [%e List.fold_right
              (fun (lbli,expi) rep ->
                let loc = get_loc expi in
                let names = [%pat? ([%p pat_make_var loc lbli],[%p pat_make_var loc (md_name lbli)])][@metaloc loc] in
                
                [%expr 
                 let [%p names] = 
                           ([%e exp_make_field_n loc "rep" lbli],
                            [%e exp_make_field loc (exp_make_field_n loc "md" "data") (md_name lbli)]) in
                 let [%p pat_make_var loc (manifest_name lbli)] = 
                         [%e forest_manifest_gen true expi vName] ~tmpdir:tmpdir 
                           ([%e exp_make_ident loc lbli],
                            [%e exp_make_ident loc (md_name lbli)])
                       in 
                      let errlist = errlist @ [%e exp_make_field_n loc (manifest_name lbli) "errors"] in [%e rep]
                ][@metaloc loc]
              ) dlist final_mani]
          ][@metaloc loc]    
    in
    match e with
    | Var(name) -> [%expr fun ?tmpdir:(tmpdir="") (rep,md) -> [%e main_exp]][@metaloc loc]
    | Thunked(_) ->
       [%expr (fun ?tmpdir:(tmpdir="") (rep,md) -> 
         let sfunc ?dirname:(dirname="") ?basename:(basename="") () = () in
         { errors = [] ;storeFunc = sfunc; tmppath = tmpdir }) 
       ][@metaloc loc]
    | _ -> 
       let main_setup =
         [%expr
             match md.info with
             (* TODO: This is probably not quite right. What do we want to store? What path do we associate the log error with? *)
             | None -> 
                let sfunc ?dirname:(dirname="") ?basename:(basename="") () = () in
                { errors = [("",MD_Missing_Info)] ;storeFunc = sfunc; tmppath = tmpdir }
             | Some(info) -> [%e main_exp]
         ][@metaloc loc]
       in
       let init_setup = 
         if inside 
         then [%expr (fun ?tmpdir:(tmpdir="") (rep,md) -> [%e main_setup]) ][@metaloc loc] 
         else 
           [%expr (fun ?tmpdir:(tmpdir="") (rep,md) -> 
             let tmpdir = if tmpdir = ""
               then
                 let tmpdir = Filename.temp_file "forest-" "" in
                 let _ = Sys.remove tmpdir in
                 let _ = Unix.mkdir tmpdir 0o770 in (* TODO: What permissions do we want here *)
                 tmpdir
               else
                 tmpdir
             in
             [%e main_setup])
           ][@metaloc loc]
       in
       init_setup


(* Checks if there is a delay in a forest_node ast *)
let rec delay_checker (fast : forest_node ast) : bool =
  let fast,loc = get_NaL fast in
  match fast with
  | Thunked(_) -> true
  | File 
  | Link
  (* TODO: Should we be more clever about vars? Checking them for
     delays for example? In that case, need to also make them load
     unincrementally in the special load func*)
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
  | File -> mk_ast loc File
  | Link -> mk_ast loc Link
  | Var(x) -> mk_ast loc @@ Var x
  | Pads(x) -> mk_ast loc @@ Pads x

  | Option(fast) ->
     let newfast = dependency_grapher vlist fast in
     mk_p_ast loc newfast.payload @@ Option newfast
  | Url(fast) -> 
     let newfast = dependency_grapher vlist fast in
     mk_p_ast loc newfast.payload @@ Url newfast

  | Thunked(fast) -> 
     let newfast = dependency_grapher vlist fast in
     mk_ast loc @@ Thunked newfast

  (* Do something special if this is thunked?
  | Predicate(Thunked(fast),p) *)
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
       (* Will break if multiple directories use same names in one desc *)
       then (lbli :: vacc,(lbli,newfast) :: facc)
       else (vacc,(lbli,newfast) :: facc)
     ) (vlist,[]) dlist
     in
     mk_ast loc @@ Directory (List.rev newdlist)
  | SkinApp(_,_) -> raise_loc_err loc "thunk_checker: Skin applications should not exist here."

let def_generator loc  (flist : (varname * forest_node ast) list) : structure =
  let def_gen ((name,e) : (varname * forest_node ast)) (tlist,llist)  : (type_declaration list * structure) =
    let loc = get_loc e in
    let e = Skins.doSkinning loc (name,e) in
(* TODO: Decide if you still want to fix up path expressions
  let e = fexp_map 
  (fun e ->
  match e with
  | Thunked(_loc,e) -> 
  begin
  match e with
  | PathExp(_,ptype,Thunked _) -> e 
  | PathExp(_,ptype,e) -> PathExp(_loc,ptype,Thunked(_loc,e))
  | _ -> Thunked(_loc,e)
  end
  | _ -> e
  ) e
  in
*)
    let _ = Hashtbl.replace Utility.forestTbl name e in
    let e =
      match e.node with
      | Thunked(e)      -> e
      | _               -> e
    in
    let e = dependency_grapher [] e in
    let reps, rep = forest_rep_gen e in 
    let mds, md = forest_md_gen e in
    let rep_decl = typ_make_type_decl loc (rep_name name) ~manifest:rep in
    let md_decl = typ_make_type_decl loc (md_name name) ~manifest:md in
    let new_tlist = reps @ [rep_decl] @ mds @ [md_decl] @ tlist in
    let load_typ = 
      [%type: filepath -> 
            ([%t typ_make_constr loc (rep_name name)] * [%t typ_make_constr loc (md_name name)])
      ][@metaloc loc]
    in
    let new_typ =
      [%type:filepath -> 
            ([%t typ_make_constr loc (rep_name name)], [%t typ_make_constr loc (md_name name)]) cursor
      ][@metaloc loc]
    in
    let cost_typ = 
      [%type: ([%t typ_make_constr loc (rep_name name)] * [%t typ_make_constr loc (md_name name)])  
          -> CursorMonad.cost
      ][@metaloc loc]
    in
    let mani_typ = 
      [%type: ?tmpdir:filepath ->
            ([%t typ_make_constr loc (rep_name name)] * [%t typ_make_constr loc (md_name name)])
            -> manifest
      ][@metaloc loc]
    in
    let cost = 
      [%stri
       let rec ([%p pat_make_var loc (cost_name name)] : [%t cost_typ]) = [%e forest_cost_gen e name]
      ][@metaloc loc]
    in
    let mani = 
      [%stri
       let rec ([%p pat_make_var loc (manifest_name name)] : [%t mani_typ]) = [%e forest_manifest_gen false e name]
      ][@metaloc loc]
    in
    let load =  
      [%stri
       let rec ([%p pat_make_var loc (load_name name)] : [%t load_typ]) = [%e forest_load_gen e name]
       and [%p pat_make_var loc (new_nameR name)] : [%t new_typ] = [%e forest_new_gen ~first:true e name]
       and [%p pat_make_var loc (new_name name)] = fun path -> return ([%e exp_make_ident loc (new_nameR name)] path)
      ][@metaloc loc]
    in
    (* TODO: Locate this error and add the warning string where it goes *)
    let warning_stri =
      Str.attribute ~loc @@ Ast_mapper.attribute_of_warning loc 
        "Warning 22: A delayed dependency is automatically forced since its dependent is not delayed" 
    in
    new_tlist, cost :: mani :: load :: llist 
  in
  let types, lets = List.fold_right def_gen flist ([],[]) in
  (Str.type_ ~loc Recursive types) :: lets




  
(* Useful for testing purposes
  let locc = !default_loc in
  let type_decl = {
    ptype_name = { txt = "x"; loc = locc };
    ptype_params = [];
    ptype_cstrs = [];
    ptype_kind = Ptype_abstract;
    ptype_private = Public;
    ptype_manifest = Some(typ_make_constr "Noo");
    ptype_attributes = [];
    ptype_loc = locc
  }
  in
  (Str.type_ ?loc Recursive [ type_decl ]) ::
 *)

(*
  match e with
    | Special(_loc,Print(e)) ->  
      let fdist =
        match e with
        | Var(_loc,x) -> if Hashtbl.mem forestTbl x then Hashtbl.find forestTbl x else lFail _loc (Printf.sprintf "%s is not a forest description" x)
        | _ -> (_loc,name,e)
      in
      let e = doSkinning fdist in
      [%str_item:value $lid:name$ = $str: forest_print_desc ~name:name e$ ] 
*)



	(* OLD DEP_CHECK
          match expi.payload with
          | None -> [%expr let [%p names] = [%e forest_load_gen expi vName] path in [%e acc]][@metaloc loc]
          | Some(deplist) -> 
             let final_e =
               List.fold_right (fun (v,e) acc ->
                 let rec rem_path e =
                   match e.fast_node with
                   | PathExp(_,ne) -> rem_path ne
                   | _ -> e
                 in
                 let newe = rem_path e in
                 let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
                 [%expr 
                     match [%e exp_make_field_n loc (md_name v) "info"] with
                     | None -> failwith "Dependency checking in Load: This should hopefully never happen"
                     | Some(info) ->
                        let [%p names] = [%e forest_uninc_load_gen newe vName] info.full_path in 
                        [%e acc]][@metaloc loc]
               ) deplist [%expr [%e forest_load_gen expi vName] path][@metaloc loc]
             in
           [%expr let [%p names] = [%e final_e] in [%e acc]][@metaloc loc]
	*)          
