open Forest_types
open Forest_utility
open Asttypes
open Parsetree
open Ast_helper

let rec representation_type_generator e =
  let e,loc = get_NaL e in
  match e with 
  | Thunked(fast) ->
    let declarations, field = representation_type_generator fast in
    let md_declarations, md = md_type_generator fast in
    let _, manifest = manifest_type_generator fast in
    let cursortype = [%type: ([%t field],[%t md], [%t manifest]) cursor ][@metaloc loc] in
    (declarations @ md_declarations, cursortype)
  | Option(fast) -> 
    let declarations, field = representation_type_generator fast in
    (declarations, [%type: [%t field] option ][@metaloc loc])
  | Predicate(fast,_) 
  | Url(fast)
  | PathExp(_,fast) ->
    representation_type_generator fast
  | File ->
    [], [%type: string ][@metaloc loc]
  | Link ->
    [], [%type: filepath ][@metaloc loc]
  | Var(vname) -> 
    [],  typ_make_constr loc (representation_name vname)
  | Pads(vname) ->
    [],  typ_make_constr loc (pads_representation_name vname)
  | Comprehension(Map,fast,_) ->
    let declarationsi,typi = representation_type_generator fast in
    (declarationsi,[%type: [%t typi] PathMap.t ][@metaloc loc])
  | Comprehension(List,fast,_) ->
    let declarationsi,typi = representation_type_generator fast in
    (declarationsi,[%type: [%t typi] list ][@metaloc loc])
  | Directory (dlist) ->
    let declarations,fields = 
      List.fold_right (fun (labeli,fasti) (declarations,fields) ->
        let declarationsi, typi = representation_type_generator fasti in 
        let fieldi = typ_make_field loc labeli typi in
        (declarationsi@declarations, fieldi::fields)) dlist ([],[])
    in
    let name = fresh () in 
    let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
    (declarations @ [recType],  typ_make_constr loc name)
  | SkinApp(_,_) ->
     raise_loc_err loc "representation_type_generator: Skin applications should not exist here."

and md_type_generator e =
  let e,loc = get_NaL e in
  match e with 
  | Link
  | File
  | Thunked(_) ->
    ([], [%type: unit Forest.forest_md][@metaloc loc])
  | Option(fast) -> 
    let decl, field = md_type_generator fast in
    (decl, [%type: ([%t field] option) Forest.forest_md ][@metaloc loc])
  | Predicate(fast,_) 
  | Url(fast)
  | PathExp(_,fast) ->
    md_type_generator fast
  | Var(vname) -> 
    ([], typ_make_constr loc (md_name vname))
  | Pads(vname) ->
    ([], [%type: [%t typ_make_constr loc (pads_md_name vname)]  Forest.forest_md ][@metaloc loc]) 
  | Comprehension(Map,fast,_) ->
    let decli,typi = md_type_generator fast in
    (decli,[%type: ([%t typi] PathMap.t) Forest.forest_md][@metaloc loc])
  | Comprehension(List,fast,_) ->
    let decli,typi = md_type_generator fast in
    (decli,[%type: ([%t typi] list) Forest.forest_md ][@metaloc loc])
  | Directory (dlist) ->
    let decls,fields = 
      List.fold_right (fun (labeli,fasti) (decls,fields) ->
        let decli, typi = md_type_generator fasti in 
        let fieldi = typ_make_field loc (md_name labeli) typi in
        (decli@decls, fieldi::fields)) dlist ([],[])
    in
    let name = fresh () in 
    let recType = typ_make_type_decl loc ~kind:(Ptype_record fields) name in
    (decls @ [recType], [%type: [%t typ_make_constr loc name] Forest.forest_md ][@metaloc loc])
  | SkinApp(_,_) ->
     raise_loc_err loc "md_type_generator: Skin applications should not exist here."

and manifest_type_generator forest_ast =
  let expression,location = get_NaL forest_ast in
  match expression with 
  | Link
  | File -> 
    ([], [%type: unit Forest.manifest][@metaloc location])
  | Thunked (forest_ast)
  | Predicate(forest_ast,_) 
  | Url(forest_ast)
  | Option(forest_ast)
  | PathExp(_,forest_ast) -> manifest_type_generator forest_ast
  | Var(vname) -> 
    ([], typ_make_constr location (manifest_name vname))
  | Pads(vname) ->
     ([],
      [%type: [%t typ_make_constr location (pads_manifest_name vname)] Forest.manifest]
        [@metaloc location]) 
  | Comprehension(Map,forest_ast,_) ->
    let decli,typi = manifest_type_generator forest_ast in
    (decli,[%type: ([%t typi] PathMap.t) Forest.manifest][@metaloc location])
  | Comprehension(List,forest_ast,_) ->
    let decli,typi = manifest_type_generator forest_ast in
    (decli,[%type: ([%t typi] list) Forest.manifest ][@metaloc location])
  | Directory (dlist) ->
    let decls,fields = 
      List.fold_right (fun (labeli,forest_asti) (decls,fields) ->
        let decli, typi = manifest_type_generator forest_asti in 
        let fieldi = typ_make_field location (manifest_name labeli) typi in
        (decli@decls, fieldi::fields)) dlist ([],[])
    in
    let name = fresh () in 
    let recType = typ_make_type_decl location ~kind:(Ptype_record fields) name in
    (decls @ [recType],
     [%type: [%t typ_make_constr location name] Forest.manifest ][@metaloc location])   
  | SkinApp(_,_) ->
     raise_loc_err location "manifest_type_generator: Skin applications should not exist here."
