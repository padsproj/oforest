open All_types
open Asttypes
open Parsetree
open Ast_helper
open Utility

(* Helper functions *)

let rec check_uniticity (past : pads_node ast) : bool =
  let (e,loc) = get_NaL past in
  match e with
  | Pint 
  | Pfloat
  | Pstring _ 
  | Pconst (PFRE _) 
  | Ppred _
  | Plist _ 
  | Precord _ 
  | Pdatatype _ -> false
  | Pconst _ -> true
  | Ptuple (p1,p2) -> check_uniticity p1 && check_uniticity p2
  | Pvar x ->
     if Hashtbl.mem padsUnitTbl x
     then Hashtbl.find padsUnitTbl x
     else raise_loc_err loc (Printf.sprintf "Pads description %s not defined" x)

let rec listify_tuple (past : pads_node ast) : pads_node ast list =
  let (e,loc) = get_NaL past in
  match e with
  | Ptuple (p1,p2) -> p1 :: (listify_tuple p2)
  | _ -> [past]
  

let pf_converter (loc : loc) : pads_fixed -> Parsetree.expression = function
  | PFInt n -> [%expr PTI [%e exp_make_int loc n]][@metaloc loc]
  | PFStr s -> [%expr PTS [%e exp_make_string loc s]][@metaloc loc]
  | PFRE s -> [%expr PTRE [%e exp_make_ocaml loc s]][@metaloc loc]
  | PFEOF -> [%expr PTEOF][@metaloc loc]


(* Main functions *)

let rec parse_gen (past : pads_node ast) : Parsetree.expression =
  let (e,loc) = get_NaL past in
  match e with
  | Pint -> [%expr PadsParser.parse_int][@metaloc loc]
  | Pfloat -> [%expr PadsParser.parse_float][@metaloc loc]
  | Pstring t -> [%expr PadsParser.parse_pstring [%e pf_converter loc t]][@metaloc loc]
  | Pconst (PFRE re) -> [%expr PadsParser.parse_regex [%e exp_make_ocaml loc re]][@metaloc loc]
  | Pconst t -> [%expr PadsParser.parse_string [%e pf_converter loc t]][@metaloc loc]
  | Pvar x -> exp_make_ident loc (pads_parse_s_name x)
  | Ppred(x,past,cond) ->
     (* begin [@warning "-26"] *)
     (* TODO: Should add better error info probably *)
     let repp,mdp = (pat_make_var loc x),(pat_make_var loc (pads_md_name x)) in
     let repe,mde = (exp_make_ident loc x),(exp_make_ident loc (pads_md_name x)) in
     [%expr (fun state ->
       let ([%p repp],[%p mdp],state) = [%e parse_gen past] state in
       if [%e exp_make_ocaml loc cond]
       then ([%e repe],[%e mde],state)
       else 
         let this_md = [%e mde] in
         ([%e repe],
          {this_md with
            pads_num_errors = this_md.pads_num_errors + 1;
            pads_error_msg = "Predicate failure" :: this_md.pads_error_msg;
          }, state)
      )][@metaloc loc]
  | Plist(past,sep,term) ->
     [%expr PadsParser.parse_list [%e parse_gen past] [%e pf_converter loc sep] [%e pf_converter loc term]][@metaloc loc]
  | Precord(rlist) ->
     let named_rlist = List.filter (fun re ->
       match re with
       | Unnamed _ -> false
       | Named _ -> true
     ) rlist
     in
     let named_rlist = List.map (fun re ->
       match re with
       | Unnamed _ -> raise_loc_err loc "Precord parsing: Filter didn't work properly."
       | Named (x,past) -> (x,past)
     ) named_rlist
     in
    let rep_assgn = List.map (fun (lbli,_) -> lbli,lbli) named_rlist in 
    let md_assgn = List.map (fun (lbli,_) -> (pads_md_name lbli),(pads_md_name lbli)) named_rlist in
    let numErr = List.fold_left (fun acc (lbli,_) ->  
      [%expr [%e exp_make_field_n loc (pads_md_name lbli) "pads_num_errors"] + [%e acc]][@metaloc loc]
    ) ([%expr rest_md.pads_num_errors][@metaloc loc]) named_rlist
    in
    let errMsg = List.fold_left (fun acc (lbli,_) ->  
      [%expr [%e exp_make_field_n loc (pads_md_name lbli) "pads_error_msg"] @ [%e acc] ][@metaloc loc]
    ) ([%expr rest_md.pads_error_msg][@metaloc loc]) named_rlist
    in
    let extra = List.fold_left (fun acc (lbli,_) ->  
      [%expr [%e exp_make_field_n loc (pads_md_name lbli) "pads_extra"] @ [%e acc] ][@metaloc loc]
    ) ([%expr rest_md.pads_extra][@metaloc loc]) named_rlist
    in
    let init_exp = 
      [%expr let (r,m)  = 
               ([%e exp_make_record_s loc rep_assgn], 
                { pads_num_errors = [%e numErr];
                  pads_error_msg = [%e errMsg];
		  pads_data = [%e exp_make_record_s loc md_assgn] ;
                  pads_extra = [%e extra] })
             in (r,m,state)][@metaloc loc]
    in
    let final_exp = 
      List.fold_right
        (fun re acc ->
          match re with
          | Unnamed past ->
             [%expr let (this,this_md,state) = [%e parse_gen past] state in
                    let rest_md =
                      { pads_num_errors = rest_md.pads_num_errors + this_md.pads_num_errors;
                        pads_error_msg = rest_md.pads_error_msg @ this_md.pads_error_msg;
                        pads_data = ();
                        pads_extra = rest_md.pads_extra @ this_md.pads_extra }
                    in
                      [%e acc]
             ][@metaloc loc]
          | Named (x,past) ->
             let names = [%pat? ([%p pat_make_var loc x],[%p pat_make_var loc (pads_md_name x)],state)][@metaloc loc] in
	     [%expr let [%p names] = [%e parse_gen past] state in 
		    [%e acc]][@metaloc loc]
        )
        rlist init_exp
    in
        [%expr (fun state ->
          let rest_md = Pads.empty_md () in
          [%e final_exp]
        )][@metaloc loc]
  | Pdatatype vlist ->
     let nvlist = List.map (fun (name,past) -> (name,parse_gen past)) vlist in
     let split_list l =
       match List.rev l with
       | [] -> raise_loc_err loc "Parse_gen: Should be impossible for a datatype to not have any variants"
       | hd :: tl -> (hd,List.rev tl)
     in
     let ((name,e),nvlist) = split_list nvlist in
     let init =
       [%expr let (rep,md,ns) = [%e e] state in
              ([%e exp_make_construct loc ~exp:(exp_make_ident loc "rep") name],
               { md with pads_data =  [%e exp_make_construct loc ~exp:(exp_make_ident loc "md") (pads_md_name name)]},                 
               ns)][@metaloc loc]
     in
     let final_exp = 
       List.fold_right (fun (name,e) acc ->
         [%expr let (rep,md,ns) = [%e e] state in
                if md.pads_num_errors = 0
                then
                  ([%e exp_make_construct loc ~exp:(exp_make_ident loc "rep") name],
                   { md with pads_data =  [%e exp_make_construct loc ~exp:(exp_make_ident loc "md") (pads_md_name name)]},                 
                   ns)
                else [%e acc]][@metaloc loc]
       ) nvlist init
     in
     [%expr (fun state ->
       [%e final_exp]
     )][@metaloc loc]
  | Ptuple (p1,p2) -> 
     let b1 = check_uniticity p1 in
     let b2 = check_uniticity p2 in
     let exp = 
       if b1 && b2 (* Both are unit type *)
       then
         [%expr
          let (rep1,md1,state) = [%e parse_gen p1] state in
          let (rep2,md2,state) = [%e parse_gen p2] state in
          ((),
           {pads_num_errors = md1.pads_num_errors + md2.pads_num_errors;
            pads_error_msg = md1.pads_error_msg @ md2.pads_error_msg;
            pads_data = ();
            pads_extra = md1.pads_extra @ md2.pads_extra;             
           },
           state)
         ][@metaloc loc]
       else if b1 (* Only p1 is unit type *)
       then 
         [%expr
          let (rep1,md1,state) = [%e parse_gen p1] state in
          let (rep2,md2,state) = [%e parse_gen p2] state in
          (rep2,
           {pads_num_errors = md1.pads_num_errors + md2.pads_num_errors;
            pads_error_msg = md1.pads_error_msg @ md2.pads_error_msg;
            pads_data = md2.pads_data;
            pads_extra = md1.pads_extra @ md2.pads_extra;             
           },
           state)
         ][@metaloc loc]
       else if b2 (* Only p2 is unit type *)
       then 
         [%expr
          let (rep1,md1,state) = [%e parse_gen p1] state in
          let (rep2,md2,state) = [%e parse_gen p2] state in
          (rep1,
           {pads_num_errors = md1.pads_num_errors + md2.pads_num_errors;
            pads_error_msg = md1.pads_error_msg @ md2.pads_error_msg;
            pads_data = md1.pads_data;
            pads_extra = md1.pads_extra @ md2.pads_extra;             
           },
           state)
         ][@metaloc loc]
       else (* Neither are unit type, so we don't wanna throw away either *)
         let rec aggreg name op n =
           let agg = aggreg name op in
           if n > 1
           then [%expr [%e op] [%e agg (n-1)] [%e  exp_make_field_n loc (Printf.sprintf "md%d" n)  name]][@metaloc loc]
           else exp_make_field_n loc (Printf.sprintf "md%d" n)  name
         in
         let plist = listify_tuple past in
         let plist = List.rev @@ fst @@ List.fold_left (fun (acc,n) p -> ((p,n)::acc),(n+1)) ([],1) plist in
         let tlist = List.filter (fun (p,_) -> not (check_uniticity p)) plist in
         let rep =  exp_make_tup loc @@ List.map (fun (_,n) -> exp_make_ident loc @@ Printf.sprintf "rep%d" n) tlist in
         let md = 
           [%expr
               { pads_num_errors = [%e aggreg "pads_num_errors" ([%expr (+) ][@metaloc loc]) (List.length plist)];
                 pads_error_msg = [%e aggreg "pads_error_msg" ([%expr (@)][@metaloc loc]) (List.length plist)];
                 pads_data = [%e exp_make_tup loc @@ List.map (fun (_,n) -> exp_make_ident loc @@ Printf.sprintf "md%d" n) tlist];
                 pads_extra = [%e aggreg "pads_extra" ([%expr (@)][@metaloc loc]) (List.length plist)];
               }][@metaloc loc]
             in
         let init =
           [%expr
               ([%e rep],[%e md],state)][@metaloc loc]
            in 
         List.fold_right (fun (past,n) acc ->
           let names = [%pat? ([%p pat_make_var loc @@ Printf.sprintf "rep%d" n],
                               [%p pat_make_var loc @@ Printf.sprintf "md%d" n],
                               state)][@metaloc loc]
           in
           [%expr let [%p names] = [%e parse_gen past] state in
                  [%e acc]][@metaloc loc]
         ) plist init
     in
     [%expr (fun state ->
       [%e exp]
      )][@metaloc loc]


let rec default_rep_gen (past : pads_node ast) : Parsetree.expression =
  let (e,loc) = get_NaL past in
  match e with
  | Pint      -> [%expr 0]
  | Pfloat    -> [%expr 0.0]
  | Pstring _ -> [%expr ""]
  | Pconst (PFRE re) ->
     [%expr match [%e exp_make_ocaml loc re] with
     | RE _ -> ""
     | REd (_,d) -> d][@metaloc loc]
  | Pconst _ -> [%expr ()] [@metaloc loc]
  | Pvar x -> exp_make_ident loc (pads_default_rep_name x)
  | Ppred (_,past,_) -> default_rep_gen past
  | Precord(rlist) ->
     let fields =
       List.fold_right (fun re fields ->
	 match re with
	 | Unnamed _ -> fields
	 | Named (x,past) -> (x,default_rep_gen past) :: fields) rlist []
     in
     if fields = []
     then [%expr ()] [@metaloc loc]
     else exp_make_record loc fields
  | Plist(past,_,t) ->
     begin
       match t with
       | PFInt n ->
	  let def = default_rep_gen past in
	  let rec gen_default n =
	    if n <= 0 then [%expr []][@metaloc loc]
	    else [%expr [%e def] :: [%e gen_default (n-1)]][@metaloc loc]
	  in
	  gen_default n
       | _ -> [%expr []][@metaloc loc]
     end
  | Ptuple (p1,p2) ->
     let b1 = check_uniticity p1 in
     let b2 = check_uniticity p2 in
     if b1 && b2 (* Both are unit type *)
     then  [%expr ()][@metaloc loc]
     else if b1 (* Only p1 is unit type *)
     then default_rep_gen p2
     else if b2 (* Only p2 is unit type *)
     then default_rep_gen p1
     else (* Neither are unit type, so we don't wanna throw away either *)
       let plist = listify_tuple past in
       let plist = List.filter (fun p -> not @@ check_uniticity p) plist in
       let elist = List.map default_rep_gen plist in
       exp_make_tup loc elist
  | Pdatatype vlist ->
     match vlist with
     | (name,past)::_ -> exp_make_construct loc ~exp:(default_rep_gen past) name
     | [] -> raise_loc_err loc "default_rep_gen: Should be impossible for a datatype to not have any variants"
        
let rec default_md_gen (past : pads_node ast) : Parsetree.expression =
  let (e,loc) = get_NaL past in
  match e with
  | Pint
  | Pfloat
  | Pstring _
  | Pconst _ -> [%expr Pads.empty_md ()][@metaloc loc]
  | Pvar x -> exp_make_ident loc (pads_default_md_name x)
  | Ppred (_,past,_) -> default_md_gen past
  | Precord(rlist) ->
     let fields =
       List.fold_right (fun re fields ->
	 match re with
	 | Unnamed _ -> fields
	 | Named (x,past) -> (pads_md_name x,default_md_gen past) :: fields) rlist []
     in
     if fields = []
     then [%expr Pads.empty_md ()] [@metaloc loc]
     else [%expr Pads.empty_md [%e exp_make_record loc fields]][@metaloc loc]
  | Plist(past,_,t) ->
     begin
       match t with
       | PFInt n ->
	  let def = default_md_gen past in
	  let rec gen_default n =
	    if n <= 0 then [%expr []][@metaloc loc]
	    else [%expr [%e def] :: [%e gen_default (n-1)]][@metaloc loc]
	  in
	  [%expr Pads.empty_md ([%e gen_default n])][@metaloc loc]
       | _ -> [%expr Pads.empty_md []][@metaloc loc]
     end
  | Ptuple (p1,p2) ->
     let b1 = check_uniticity p1 in
     let b2 = check_uniticity p2 in
     if b1 && b2 (* Both are unit type *)
     then  [%expr  Pads.empty_md ()][@metaloc loc]
     else if b1 (* Only p1 is unit type *)
     then default_md_gen p2
     else if b2 (* Only p2 is unit type *)
     then default_md_gen p1
     else (* Neither are unit type, so we don't wanna throw away either *)
       let plist = listify_tuple past in
       let plist = List.filter (fun p -> not @@ check_uniticity p) plist in
       let elist = List.map default_md_gen plist in
       [%expr Pads.empty_md [%e  exp_make_tup loc elist]][@metaloc loc]
  | Pdatatype vlist ->
     match vlist with
     | (name,past)::_ -> [%expr Pads.empty_md [%e exp_make_construct loc ~exp:(default_md_gen past) (pads_md_name name)]][@metaloc loc]
     | [] -> raise_loc_err loc "default_rep_gen: Should be impossible for a datatype to not have any variants"
       
let rec pads_rep_type_gen (past : pads_node ast) : (type_declaration list * core_type) =
  let (e,loc) = get_NaL past in
  match e with
  | Pint      -> [], [%type: int][@metaloc loc]
  | Pfloat    -> [], [%type : float][@metaloc loc]
  | Pstring _
  | Pconst (PFRE _) -> [], [%type: string] [@metaloc loc]
  | Pconst _ -> [], [%type: unit] [@metaloc loc]
  | Ppred (_,past,_) -> pads_rep_type_gen past
  | Pvar(vname) ->  [],  typ_make_constr loc (pads_rep_name vname) 
  | Plist(past,_,_) -> 
    let decli,typi = pads_rep_type_gen past in
    (decli,[%type: [%t typi] list][@metaloc loc])
  | Precord (rlist) ->
    let decls,fields = 
      List.fold_right (fun re (decls,fields) ->
	match re with
	| Unnamed _ -> (decls,fields)
	| Named (x,past) -> 
           let decli, typi = pads_rep_type_gen past in 
           let fieldi = typ_make_field loc x typi in
        (decli@decls, fieldi::fields)) rlist ([],[])
    in
    if fields = []
    then decls,[%type: unit][@metaloc loc]
    else
      let name = freshP () in
      let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
      (decls @ [recType],  typ_make_constr loc name)
  | Pdatatype vlist ->
     let decls,clist = List.fold_right (fun (name,past) (decls, clist) ->
       let decli, typi = pads_rep_type_gen past in
       let constr =  typ_make_variant loc name ~args:(Pcstr_tuple [typi]) in
       decli@decls,constr::clist
     ) vlist ([],[])
     in
     let name = freshP () in
     let vdecl = typ_make_type_decl loc ~kind:(Ptype_variant clist) name in
     (decls @ [vdecl],  typ_make_constr loc name)
  | Ptuple(p1,p2) ->
     let b1 = check_uniticity p1 in
     let b2 = check_uniticity p2 in
     if b1 && b2 (* Both are unit type *)
     then  [], [%type: unit] [@metaloc loc]
     else if b1 (* Only p1 is unit type *)
     then pads_rep_type_gen p2
     else if b2 (* Only p2 is unit type *)
     then pads_rep_type_gen p1
     else (* Neither are unit type, so we don't wanna throw away either *)
       let plist = listify_tuple past in
       let plist = List.filter (fun p -> not @@ check_uniticity p) plist in
       let (dlist,tlist) = List.fold_right (fun past (dlist,tlist) ->
         let d1,t1 = pads_rep_type_gen past in
         (d1 @ dlist),(t1::tlist)
       ) plist ([],[])
       in
       dlist,typ_make_tup loc tlist

     
let rec pads_mani_type_gen (past : pads_node ast) : (type_declaration list * core_type) =
  let (e,loc) = get_NaL past in
  match e with
  | Pint     
  | Pfloat   
  | Pstring _
  | Pconst _ -> [], [%type: unit Pads.padsManifest][@metaloc loc]
  | Ppred (_,past,_) -> pads_mani_type_gen past
  | Pvar(vname) ->  [],  typ_make_constr loc (pads_manifest_name vname) 
  | Plist(past,_,_) -> 
    let decli,typi = pads_mani_type_gen past in
    (decli,[%type: ([%t typi] list) Pads.padsManifest][@metaloc loc])
  | Precord (rlist) ->
    let decls,fields = 
      List.fold_right (fun re (decls,fields) ->
	match re with
	| Unnamed _ -> (decls,fields)
	| Named (x,past) -> 
           let decli, typi = pads_mani_type_gen past in 
           let fieldi = typ_make_field loc (pads_manifest_name x) typi in
        (decli@decls, fieldi::fields)) rlist ([],[])
    in
    if fields = []
    then decls,[%type: unit Pads.padsManifest][@metaloc loc]
    else
      let name = freshP () in 
      let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
      (decls @ [recType], [%type: [%t typ_make_constr loc name] Pads.padsManifest ][@metaloc loc])
  | Pdatatype vlist ->
     let decls,clist = List.fold_right (fun (name,past) (decls, clist) ->
       let decli, typi = pads_mani_type_gen past in
       let constr =  typ_make_variant loc (pads_manifest_name name) ~args:(Pcstr_tuple [typi]) in
       decli@decls,constr::clist
     ) vlist ([],[])
     in
     let name = freshP () in
     let vdecl = typ_make_type_decl loc ~kind:(Ptype_variant clist) name in
     (decls @ [vdecl],  [%type: [%t typ_make_constr loc name] Pads.padsManifest][@metaloc loc])
  | Ptuple (p1,p2) -> 
     let b1 = check_uniticity p1 in
     let b2 = check_uniticity p2 in
     if b1 && b2 (* Both are unit type *)
     then  [], [%type: unit Pads.padsManifest] [@metaloc loc]
     else if b1 (* Only p1 is unit type *)
     then pads_mani_type_gen p2
     else if b2 (* Only p2 is unit type *)
     then pads_mani_type_gen p1
     else (* Neither are unit type, so we don't wanna throw away either *)
       let plist = listify_tuple past in
       let plist = List.filter (fun p -> not @@ check_uniticity p) plist in
       let (dlist,tlist) = List.fold_right (fun past (dlist,tlist) ->
         let d1,t1 = pads_mani_type_gen past in
         (d1 @ dlist),(t1::tlist)
       ) plist ([],[])
       in
       dlist,[%type: [%t typ_make_tup loc tlist] Pads.padsManifest]

let rec pads_md_type_gen (past : pads_node ast) : (type_declaration list * core_type) = 
  let (e,loc) = get_NaL past in
  match e with
  | Pint
  | Pfloat
  | Pstring _ 
  | Pconst _  -> [], [%type: unit Pads.pads_md][@metaloc loc]
  | Ppred (_,past,_) -> pads_md_type_gen past
  | Pvar(vname) ->  [],  typ_make_constr loc (pads_md_name vname)
  | Plist(past,_,_) -> 
    let decli,typi = pads_md_type_gen past in
    (decli,[%type: ([%t typi] list) Pads.pads_md ][@metaloc loc])
  | Precord (rlist) ->
    let decls,fields = 
      List.fold_right (fun re (decls,fields) ->
	match re with
	| Unnamed _ -> (decls,fields)
	| Named (x,past) -> 
           let decli, typi = pads_md_type_gen past in 
           let fieldi = typ_make_field loc (pads_md_name x) typi in
        (decli@decls, fieldi::fields)) rlist ([],[])
    in
    if fields = []
    then decls,[%type: unit Pads.pads_md][@metaloc loc]
    else
      let name = freshP () in 
      let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
      (decls @ [recType], [%type: [%t typ_make_constr loc name] Pads.pads_md ][@metaloc loc])
  | Pdatatype vlist ->
     let decls,clist = List.fold_right (fun (name,past) (decls, clist) ->
       let decli, typi = pads_md_type_gen past in
       let constr =  typ_make_variant loc (pads_md_name name) ~args:(Pcstr_tuple [typi]) in
       decli@decls,constr::clist
     ) vlist ([],[])
     in
     let name = freshP () in
     let vdecl = typ_make_type_decl loc ~kind:(Ptype_variant clist) name in
     (decls @ [vdecl],  [%type: [%t typ_make_constr loc name] Pads.pads_md][@metaloc loc])
  | Ptuple (p1,p2) -> 
     let b1 = check_uniticity p1 in
     let b2 = check_uniticity p2 in
     if b1 && b2 (* Both are unit type *)
     then  [], [%type: unit Pads.pads_md] [@metaloc loc]
     else if b1 (* Only p1 is unit type *)
     then pads_md_type_gen p2
     else if b2 (* Only p2 is unit type *)
     then pads_md_type_gen p1
     else (* Neither are unit type, so we don't wanna throw away either *)
       let plist = listify_tuple past in
       let plist = List.filter (fun p -> not @@ check_uniticity p) plist in
       let (dlist,tlist) = List.fold_right (fun past (dlist,tlist) ->
         let d1,t1 = pads_md_type_gen past in
         (d1 @ dlist),(t1::tlist)
       ) plist ([],[])
       in
       dlist,[%type: [%t typ_make_tup loc tlist] Pads.pads_md]

let rec pads_to_string (past : pads_node ast) : Parsetree.expression = 
  let (e,loc) = get_NaL past in
  let exp = 
    match e with
    | Pint -> [%expr Buffer.add_string buf @@ string_of_int rep][@metaloc loc]
    | Pfloat -> [%expr Buffer.add_string buf @@ string_of_float rep][@metaloc loc]
    | Pstring (PFStr s) -> [%expr Buffer.add_string buf rep; Buffer.add_string buf [%e exp_make_string loc s]][@metaloc loc]
    | Pstring (PFRE re) -> [%expr Buffer.add_string buf rep][@metaloc loc]
(* TODO: Add back to above if you make regex terminations consume
  Buffer.add_string buf [%e default_rep_gen (mk_ast loc @@ Pconst (PFRE re))]*)
    | Pstring _ -> [%expr Buffer.add_string buf rep][@metaloc loc]
    | Pconst (PFInt n) -> [%expr Buffer.add_string buf [%e exp_make_string loc (string_of_int n)]][@metaloc loc]
    | Pconst (PFStr s) -> [%expr Buffer.add_string buf [%e exp_make_string loc s]][@metaloc loc]
    | Pconst (PFRE _) -> [%expr Buffer.add_string buf rep][@metaloc loc]
    | Pconst PFEOF -> [%expr ()][@metaloc loc]
    | Ppred (_,past,_) -> [%expr [%e pads_to_string past] buf (rep,md)][@metaloc loc]
    | Pvar(vname) -> [%expr [%e exp_make_ident loc (pads_to_buffer_name vname)] buf (rep,md)][@metaloc loc]
    | Plist(past,sep,term) ->
       let s = match term with
         | PFEOF
         | PFInt _ -> exp_make_string loc ""
         | PFRE re -> default_rep_gen @@ mk_ast loc (Pconst (PFRE re))
         | PFStr s -> exp_make_string loc s
       in
       [%expr PadsParser.list_to_buf [%e pads_to_string past] [%e pf_converter loc sep] buf (rep,md.pads_data);
        Buffer.add_string buf [%e s]][@metaloc loc]
    | Precord (rlist) ->
       List.fold_right (fun re acc ->
         match re with
         | Unnamed past ->
            let loc = get_loc past in
            [%expr [%e pads_to_string past] buf ([%e default_rep_gen past],[%e default_md_gen past]);
             [%e acc]][@metaloc loc]
         | Named (x,past) -> 
            let loc = get_loc past in
            [%expr [%e pads_to_string past] buf
                ([%e exp_make_field_n loc "rep" x],[%e exp_make_field loc (exp_make_field_n loc "md" "pads_data") (pads_md_name x)]);
             [%e acc]][@metaloc loc]
       ) rlist [%expr ()][@metaloc loc]
    | Pdatatype vlist ->
       let failCase =
           {pc_lhs = Pat.any ~loc ();
            pc_guard = None;
            pc_rhs = [%expr failwith "PADS Error: Rep and metadata type don't match for loaded pdatatype"][@metaloc loc]}
       in
       let clist =
         List.fold_right (fun (name,past) acc ->
           {pc_lhs = [%pat? ([%p pat_make_construct loc ~pat:(pat_make_var loc "rep") name],
                             [%p pat_make_construct loc ~pat:(pat_make_var loc "md") (pads_md_name name)])][@metaloc loc];
            pc_guard = None;
            pc_rhs = [%expr [%e pads_to_string past] buf (rep,md)][@metaloc loc] ;
           } :: acc
         ) vlist [failCase]
       in
       exp_make_match loc ([%expr (rep,md.pads_data)][@metaloc loc]) clist
    | Ptuple (p1,p2) -> 
       let b1 = check_uniticity p1 in
       let b2 = check_uniticity p2 in
       if b1 && b2 (* Both are unit type *)
       then
         [%expr [%e pads_to_string p1] buf ((),[%e default_md_gen p1]);
          [%e pads_to_string p2] buf ((),[%e default_md_gen p2])
         ][@metaloc loc]
       else if b1 (* Only p1 is unit type *)
       then 
         [%expr [%e pads_to_string p1] buf ((),[%e default_md_gen p1]);
          [%e pads_to_string p2] buf (rep,md)
         ][@metaloc loc]
       else if b2 (* Only p2 is unit type *)
       then 
         [%expr [%e pads_to_string p1] buf (rep,md);
          [%e pads_to_string p2] buf ((),[%e default_md_gen p2])
         ][@metaloc loc]
       else (* Neither are unit type, so we don't wanna throw away either *)
         let plist = listify_tuple past in
         let plist = List.rev @@ fst @@ List.fold_left (fun (acc,n) p -> ((p,n)::acc),(n+1)) ([],1) plist in
         let tlist = List.filter (fun (p,_) -> not @@ check_uniticity p) plist in
         let repT = pat_make_tup loc @@ List.map (fun (_,n) -> pat_make_var loc @@ Printf.sprintf "rep%d" n) tlist in
         let mdT = pat_make_tup loc @@ List.map (fun (_,n) -> pat_make_var loc @@ Printf.sprintf "md%d" n) tlist in
         let exp = 
           List.fold_right (fun (past,n) acc ->
             if check_uniticity past
             then
               [%expr [%e pads_to_string past] buf ((),[%e default_md_gen past]);
                [%e acc]][@metaloc loc]
             else
               [%expr [%e pads_to_string past] buf ([%e exp_make_ident loc  @@ Printf.sprintf "rep%d" n],
                                                    [%e exp_make_ident loc  @@ Printf.sprintf "md%d" n]);
                [%e acc]][@metaloc loc]
           ) plist @@ [%expr ()][@metaloc loc]
         in
         [%expr let ([%p repT],[%p mdT]) = (rep,md.pads_data) in
                [%e exp]
         ][@metaloc loc]
  in
  [%expr (fun buf (rep,md) -> [%e exp])][@metaloc loc]

let rec pads_manifest (past : pads_node ast) : Parsetree.expression = 
  let (e,loc) = get_NaL past in
  let exp = 
    match e with
    | Pint -> [%expr Pads.make_mani (string_of_int rep) ()][@metaloc loc]
    | Pfloat -> [%expr Pads.make_mani (string_of_float rep) ()][@metaloc loc]
    | Pstring (PFStr s) -> [%expr Pads.make_mani (rep ^ [%e exp_make_string loc s]) ()][@metaloc loc]
    | Pstring (PFRE re) -> [%expr Pads.make_mani rep ()][@metaloc loc]
    (* TODO: Add back to above if you make regex terminations consume
     * ^ [%e default_rep_gen (mk_ast loc @@ Pconst (PFRE re))] *)
    (* TODO: Fix fixed length str: <:expr< if String.length rep = $int:string_of_int n$ then [] else [ListLengthError] >> *)
    | Pstring _ -> [%expr Pads.make_mani rep ()][@metaloc loc]
    | Pconst (PFInt n) -> [%expr Pads.make_mani [%e exp_make_string loc (string_of_int n)] ()][@metaloc loc]
    | Pconst (PFStr s) -> [%expr Pads.make_mani [%e exp_make_string loc s] ()][@metaloc loc]
    | Pconst PFEOF -> [%expr Pads.make_mani "" ()][@metaloc loc]
    | Pconst (PFRE _) -> [%expr Pads.make_mani rep ()][@metaloc loc]
       (* TODO: Add regex check 
          let regex = Str.regexp r in
          let matches = Str.string_match regex rep 0 && Str.matched_string rep = rep in
          if matches then [] else [RegexMatchError r]] >>
       *)
    | Ppred (_,past,_) -> [%expr [%e pads_manifest past] (rep,md)][@metaloc loc]
    | Pvar(vname) -> [%expr [%e exp_make_ident loc (pads_manifest_name vname)] (rep,md)][@metaloc loc]
    | Plist(past,sep,term) ->
       (* TODO: Check list length is right. Check if there's a mismatch between rep and md length *)
       let s = match term with
         | PFEOF
         | PFInt _ -> exp_make_string loc ""
         | PFRE re -> default_rep_gen @@ mk_ast loc (Pconst (PFRE re))
         | PFStr s -> exp_make_string loc s
       in
       [%expr
        let m = List.map [%e pads_manifest past] (List.combine rep md.pads_data) in
        let buf = Buffer.create 1024 in
        let _ = PadsParser.list_to_buf (fun buf (m,_) -> Buffer.add_string buf m.pads_str) [%e pf_converter loc sep] buf (m,m) in
        let _ = Buffer.add_string buf [%e s] in
        let s = Buffer.contents buf in
        let errList = List.concat (List.map (fun m -> m.pads_man_errors) m) in
        { pads_man_errors = errList;
          pads_str = s;
          pads_manifest = m;
        }][@metaloc loc]
    | Precord (rlist) ->
       let named_rlist = List.filter (fun re ->
         match re with
         | Unnamed _ -> false
         | Named _ -> true
       ) rlist
       in
       let named_rlist = List.map (fun re ->
         match re with
         | Unnamed _ -> raise_loc_err loc "Precord manifest: Filter didn't work properly."
         | Named (x,past) -> (x,past)
       ) named_rlist
       in
       let mani_assgn = List.map (fun (lbli,_) -> (pads_manifest_name lbli),(pads_manifest_name lbli)) named_rlist in
       let errMsg = List.fold_left (fun acc (lbli,_) ->  
         [%expr [%e exp_make_field_n loc (pads_manifest_name lbli) "pads_man_errors"] @ [%e acc] ][@metaloc loc]
       ) ([%expr []][@metaloc loc]) named_rlist
       in
       let end_exp =
         [%expr
             { pads_man_errors = [%e errMsg];
               pads_str = Buffer.contents buf;
	       pads_manifest = [%e exp_make_record_s loc mani_assgn]}][@metaloc loc]
       in
       let final_exp = 
         List.fold_right (fun re acc ->
           match re with
           | Unnamed past ->
              let loc = get_loc past in
              [%expr let _ = [%e pads_to_string past] buf ([%e default_rep_gen past],[%e default_md_gen past]) in
                     [%e acc]][@metaloc loc]
           | Named (x,past) -> 
              let loc = get_loc past in
              let ePair = [%expr ([%e exp_make_field_n loc "rep" x],
                                  [%e exp_make_field loc (exp_make_field_n loc "md" "pads_data") (pads_md_name x)])][@metaloc loc] in
              [%expr
               let [%p pat_make_var loc (pads_manifest_name x)] = [%e pads_manifest past]  [%e ePair] in
               let _ = Buffer.add_string buf [%e exp_make_field_n loc (pads_manifest_name x) "pads_str"] in
               [%e acc]][@metaloc loc]
         ) rlist end_exp
       in
       [%expr let buf = Buffer.create 1024 in
              [%e final_exp]][@metaloc loc]
      
    | Pdatatype vlist ->
       let failCase =
           {pc_lhs = Pat.any ~loc ();
            pc_guard = None;
            pc_rhs = [%expr failwith "PADS Error: Rep and metadata type don't match for loaded pdatatype"][@metaloc loc]}
       in
       let clist =
         List.fold_right (fun (name,past) acc ->
           {pc_lhs = [%pat? ([%p pat_make_construct loc ~pat:(pat_make_var loc "rep") name],
                             [%p pat_make_construct loc ~pat:(pat_make_var loc "md") (pads_md_name name)])][@metaloc loc];
            pc_guard = None;
            pc_rhs = [%expr let mani = [%e pads_manifest past] (rep,md) in
                            { mani with pads_manifest = [%e exp_make_construct loc ~exp:(exp_make_ident loc "mani") (pads_manifest_name name)]}][@metaloc loc] ;
           } :: acc
         ) vlist [failCase]
       in
       exp_make_match loc ([%expr (rep,md.pads_data)][@metaloc loc]) clist
    | Ptuple (p1,p2) -> 
       let b1 = check_uniticity p1 in
       let b2 = check_uniticity p2 in
       if b1 && b2 (* Both are unit type *)
       then
         [%expr
             let m1 = [%e pads_manifest p1] ((),[%e default_md_gen p1]) in
             let m2 = [%e pads_manifest p2] ((),[%e default_md_gen p2]) in
             { pads_man_errors = m1.pads_man_errors @ m2.pads_man_errors;
               pads_str = m1.pads_str ^ m2.pads_str;
               pads_manifest = ()}
         ][@metaloc loc]
       else if b1 (* Only p1 is unit type *)
       then 
         [%expr
             let m1 = [%e pads_manifest p1] ((),[%e default_md_gen p1]) in
             let m2 = [%e pads_manifest p2] (rep,md) in
             { pads_man_errors = m1.pads_man_errors @ m2.pads_man_errors;
               pads_str = m1.pads_str ^ m2.pads_str;
               pads_manifest = m2.pads_manifest}
         ][@metaloc loc]
       else if b2 (* Only p2 is unit type *)
       then 
         [%expr
             let m1 = [%e pads_manifest p1] (rep,md) in
             let m2 = [%e pads_manifest p2] ((),[%e default_md_gen p2]) in
             { pads_man_errors = m1.pads_man_errors @ m2.pads_man_errors;
               pads_str = m1.pads_str ^ m2.pads_str;
               pads_manifest = m1.pads_manifest}
         ][@metaloc loc]
       else (* Neither are unit type, so we don't wanna throw away either *)
         let rec aggreg name op n =
           let agg = aggreg name op in
           if n > 1
           then [%expr [%e op] [%e agg (n-1)] [%e  exp_make_field_n loc (Printf.sprintf "man%d" n)  name]][@metaloc loc]
           else exp_make_field_n loc (Printf.sprintf "man%d" n)  name
         in
         let plist = listify_tuple past in
         let plist = List.rev @@ fst @@ List.fold_left (fun (acc,n) p -> ((p,n)::acc),(n+1)) ([],1) plist in
         let tlist = List.filter (fun (p,_) -> not @@ check_uniticity p) plist in
         let repT = pat_make_tup loc @@ List.map (fun (_,n) -> pat_make_var loc @@ Printf.sprintf "rep%d" n) tlist in
         let mdT = pat_make_tup loc @@ List.map (fun (_,n) -> pat_make_var loc @@ Printf.sprintf "md%d" n) tlist in
         let man = 
           [%expr
               { pads_man_errors = [%e aggreg "pads_man_errors" ([%expr (@) ][@metaloc loc]) (List.length plist)];
                 pads_str = [%e aggreg "pads_str" ([%expr (^)][@metaloc loc]) (List.length plist)];
                 pads_manifest = [%e exp_make_tup loc @@ List.map (fun (_,n) -> exp_make_ident loc @@ Printf.sprintf "man%d" n) tlist]
               }][@metaloc loc]
             in
         let exp = 
           List.fold_right (fun (past,n) acc ->
             if check_uniticity past
             then
               [%expr
                let [%p pat_make_var loc @@ Printf.sprintf "man%d" n] = [%e pads_manifest past] ((),[%e default_md_gen past]) in
                [%e acc]][@metaloc loc]
             else
               [%expr
                let [%p pat_make_var loc @@ Printf.sprintf "man%d" n] =
                  [%e pads_manifest past] ([%e exp_make_ident loc  @@ Printf.sprintf "rep%d" n],
                                           [%e exp_make_ident loc  @@ Printf.sprintf "md%d" n])
                in [%e acc]][@metaloc loc]
           ) plist man
         in
         [%expr let ([%p repT],[%p mdT]) = (rep,md.pads_data) in
                [%e exp]
         ][@metaloc loc]
  in
  [%expr (fun (rep,md) -> [%e exp])][@metaloc loc]

     
(* Generates the OCaml AST from PADS AST *)
let def_generator loc (plist : (string * pads_node ast) list) : structure =
  (* tlist - type list, llist - let list (structure items) *)
  let def_gen ((name,past) : string * pads_node ast) (tlist,llist) : (type_declaration list * structure) =
    let loc = past.loc in
    let reps,rep = pads_rep_type_gen past in
    let mds, md = pads_md_type_gen past in 
    let manis, mani = pads_mani_type_gen past in 
    let rep_decl = typ_make_type_decl loc (pads_rep_name name) ~manifest:rep in
    let md_decl = typ_make_type_decl loc (pads_md_name name) ~manifest:md in
    let mani_decl = typ_make_type_decl loc (pads_manifest_name name) ~manifest:mani in
    let new_tlist = manis @ [mani_decl] @ reps @ [rep_decl] @ mds @ [md_decl] @ tlist in
    
    let def_rep = default_rep_gen past in
    let def_md = default_md_gen past in
    let default_rep =
      [%stri
       let [%p pat_make_var loc (pads_default_rep_name name)] = [%e def_rep]
      ][@metaloc loc] in
    let default_md =
      [%stri
       let [%p pat_make_var loc (pads_default_md_name name)] = [%e def_md]
      ][@metaloc loc] in

    let parse_s_typ =
      [%type: ([%t typ_make_constr loc (pads_rep_name name)],[%t typ_make_constr loc (pads_md_name name)]) PadsParser.pads_parser
      ][@metaloc loc] in
    let parse_typ =
      [%type: filepath -> ([%t typ_make_constr loc (pads_rep_name name)] * [%t typ_make_constr loc (pads_md_name name)]) 
      ][@metaloc loc] in

    let parse_s =
      [%stri let [%p pat_make_var loc (pads_parse_s_name name)] : [%t parse_s_typ] = [%e parse_gen past]
      ][@metaloc loc]
    in
    let parse =
      [%stri let [%p pat_make_var loc (pads_parse_name name)] : [%t parse_typ] =
               PadsLoader.pads_load [%e def_rep] [%e def_md] [%e exp_make_ident loc (pads_parse_s_name name)]
      ][@metaloc loc]
    in

    let to_buffer_typ = 
      [%type: Buffer.t -> ([%t typ_make_constr loc (pads_rep_name name)] * [%t typ_make_constr loc (pads_md_name name)]) -> unit
      ][@metaloc loc] in
    let to_string_typ = 
      [%type: ([%t typ_make_constr loc (pads_rep_name name)] * [%t typ_make_constr loc (pads_md_name name)]) -> string
      ][@metaloc loc] in
    
    let to_buffer =
      [%stri let [%p pat_make_var loc (pads_to_buffer_name name)] : [%t to_buffer_typ] = [%e pads_to_string past]
      ][@metaloc loc]
    in
    let to_string =
      [%stri let [%p pat_make_var loc (pads_to_string_name name)] : [%t to_string_typ] =
               (fun (rep,md) ->
                 let buf = Buffer.create 1024 in
                 let _ = [%e exp_make_ident loc (pads_to_buffer_name name)] buf (rep,md) in
                 Buffer.contents buf)
      ][@metaloc loc]
    in

    let mani_typ = 
      [%type: ([%t typ_make_constr loc (pads_rep_name name)] * [%t typ_make_constr loc (pads_md_name name)]) ->
            [%t typ_make_constr loc (pads_manifest_name name)]][@metaloc loc] in
    let manifest =
      [%stri let [%p pat_make_var loc (pads_manifest_name name)] : [%t mani_typ] = [%e pads_manifest past]
      ][@metaloc loc]
    in
    new_tlist,  (manifest :: to_buffer :: to_string :: default_rep :: default_md :: parse_s :: parse :: llist)
  in

  let types, lets = List.fold_right def_gen plist ([], []) in
  (Str.type_ ~loc Recursive types) :: lets (* What is this? *)
