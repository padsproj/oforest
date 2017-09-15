open Asttypes
open Parsetree
open Ast_helper
open Forest_types

(* GLOBALS *)

let debug = false
let firstDef = ref true
let fresh_cell = ref 0

let skinTbl : (varname,skin_node ast) Hashtbl.t = Hashtbl.create 13
let forestTbl : (varname,forest_node ast) Hashtbl.t = Hashtbl.create 13
  
(* Ast_Helpers *)

let make_ast_lid loc (name : string) : Longident.t Asttypes.loc =
  {txt = (Longident.parse name); loc}

let make_ast_str loc (name : string) : string Asttypes.loc =
  {txt = name; loc = loc}

let exp_make_ident loc (name : string) : Parsetree.expression =
  Exp.ident ~loc (make_ast_lid loc name)

let exp_make_string loc (str : string) : Parsetree.expression =
  Exp.constant ~loc (Const.string str)

let exp_make_int loc (integer : int) : Parsetree.expression =
  Exp.constant ~loc (Const.int integer)
    
let exp_make_construct loc ?exp (uid : string) : Parsetree.expression =
  Exp.construct ~loc (make_ast_lid loc uid) exp

let exp_make_field loc (exp : Parsetree.expression) (field : string) : Parsetree.expression =
  Exp.field ~loc exp (make_ast_lid loc field)

let exp_make_field_n loc (var : string) (field : string) : Parsetree.expression =
  exp_make_field loc (exp_make_ident loc var) field
    
let exp_make_record loc (vlist : (string * Parsetree.expression) list) : Parsetree.expression =
  let field_list = List.map (fun (l,r) -> (make_ast_lid loc l),r) vlist in
  Exp.record ~loc field_list None
    
let exp_make_record_s loc (vlist : (string * string) list) : Parsetree.expression =
  exp_make_record loc (List.map (fun (s1,s2) -> (s1,exp_make_ident loc s2)) vlist)

let exp_make_match loc (exp : Parsetree.expression) (clist : Parsetree.case list) =
  Exp.match_ ~loc exp clist
    
let exp_make_tup loc (list : Parsetree.expression list) : Parsetree.expression =
  Exp.tuple ~loc list

let pat_make_var loc (name : string) : Parsetree.pattern =
  Pat.var ~loc (make_ast_str loc name)
    
let pat_make_construct loc ?pat (uid : string) : Parsetree.pattern =
  Pat.construct ~loc (make_ast_lid loc uid) pat
    
let pat_make_tup loc (list : Parsetree.pattern list) : Parsetree.pattern =
  Pat.tuple ~loc list

let typ_make_type_decl loc ?manifest ?kind (name : string)  : type_declaration =
  Type.mk ~loc ?manifest ?kind (make_ast_str loc name)

let typ_make_field loc (name : string) : (core_type -> label_declaration) =
  Type.field ~loc (make_ast_str loc name)

let typ_make_constr loc (name : string) : core_type =
  Typ.constr ~loc (make_ast_lid loc name) []

let typ_make_variant loc ?args ?res (name: string) : constructor_declaration =
  Type.constructor ~loc ?args ?res (make_ast_str loc name)
    
let typ_make_tup loc (tlist : core_type list) : Parsetree.core_type =
  Typ.tuple ~loc tlist

let exp_make_ocaml loc (str : string) : Parsetree.expression =
  let open Lexing in
  let open Location in
  let lexbuf = Lexing.from_string str in
  let lexbuf = {lexbuf with
    lex_start_p = loc.loc_start;
    lex_curr_p = loc.loc_start;
  } in
  Parse.expression lexbuf

(* Errors *)

let raise_loc_err loc (s: string) =
     let err = Location.error ~loc s in
     raise (Location.Error(err))

(* Forest helpers *)

let debug_out s = if debug then s else ""

let get_NaL (ast : 'a ast) : ('a * loc) = ast.node, ast.loc
let get_loc (a : 'a ast) : Location.t = a.loc
    
let mk_p_ast :  loc ->  Forest_types.fPayload -> 'a -> 'a ast = Forest_types.mk_p_ast

let mk_ast : loc -> 'a -> 'a ast = Forest_types.mk_ast
  
let find_ident_in_str (name : string) (expr : string) : bool =
  let re = Re_str.regexp (Printf.sprintf "\\b%s\\b" name) in
  try 
    let _ = Re_str.search_forward re expr 0 in
    true
  with _ -> false

let fresh () = 
  incr fresh_cell;
  Printf.sprintf "forest_%d" !fresh_cell

(* Forest Name functions *)
let representation_name = Printf.sprintf "%s_rep"
let md_name = Printf.sprintf "%s_md"
let load_name = Printf.sprintf "%s_load"
let list_name =  Printf.sprintf "%s_list"
let att_name =  Printf.sprintf "%s_att"
let acc_name =  Printf.sprintf "%s_acc"
let new_name =  Printf.sprintf "%s_new"
let new_nameR =  Printf.sprintf "%s_newRec"
let cost_name =  Printf.sprintf "%s_cost"
let manifest_name = Printf.sprintf "%s_manifest"
let empty_manifest_name = Printf.sprintf "%s_empty_manifest"
let ucur_name =  Printf.sprintf "%s_ucur"

(* Pads Name functions *)
let pads_representation_name = representation_name
let pads_md_name = md_name
let pads_parse_name = Printf.sprintf "%s_parse"
let pads_manifest_name = manifest_name
let pads_empty_manifest_name = empty_manifest_name
