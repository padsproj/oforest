open Asttypes
open Parsetree
open Ast_helper
open All_types

(* GLOBALS *)

let debug = true (* TODO: Make false for final version *)
let firstDef = ref true
let fresh_cellF = ref 0
let fresh_cellP = ref 0

let skinTbl : (varname,skin_node ast) Hashtbl.t = Hashtbl.create 13
let forestTbl : (varname,forest_node ast) Hashtbl.t = Hashtbl.create 13
let padsUnitTbl : (varname,bool) Hashtbl.t = Hashtbl.create 13



(* HELPER FUNCTIONS *)

let debug_out s = if debug then s else ""

let get_NaL (ast : 'a ast) : ('a * loc) = ast.node, ast.loc

  
let mk_ast (loc : loc) (node : 'a) : 'a ast = 
  { node; loc; payload = PNone; }
    
let mk_p_ast (loc : loc) (payload : fPayload) (node : 'a) : 'a ast = 
  { node; loc; payload}

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

(* It is non-obvious why we can't use Str or Forest in here... *)

let find_ident_in_str (name : string) (expr : string) : bool =
  let re = Re_str.regexp (Printf.sprintf "\\b%s\\b" name) in
  try 
    let _ = Re_str.search_forward re expr 0 in
    true
  with _ -> false

let get_loc (a : 'a ast) : Location.t = a.loc

let freshF () = 
  incr fresh_cellF;
  Printf.sprintf "forest_%d" !fresh_cellF

let freshP () = 
  incr fresh_cellP;
  Printf.sprintf "pads_%d" !fresh_cellP

(* Forest Name functions *)
let rep_name = Printf.sprintf "%s_rep"
let md_name = Printf.sprintf "%s_md"
let load_name = Printf.sprintf "%s_load"
let list_name =  Printf.sprintf "%s_list"
let att_name =  Printf.sprintf "%s_att"
let acc_name =  Printf.sprintf "%s_acc"
let new_name =  Printf.sprintf "%s_new"
let new_nameR =  Printf.sprintf "%s_newRec"
let cost_name =  Printf.sprintf "%s_cost"
let manifest_name = Printf.sprintf "%s_manifest"
let ucur_name =  Printf.sprintf "%s_ucur"


(* Pads Name functions *)
let pads_rep_name = rep_name
let pads_md_name = md_name
let pads_default_rep_name = Printf.sprintf "%s_default_rep"
let pads_default_md_name = Printf.sprintf "%s_default_md"
let pads_parse_name = Printf.sprintf "%s_parse"
let pads_parse_s_name = Printf.sprintf "%s_parse_state"
let pads_manifest_name = Printf.sprintf "%s_manifest"
let pads_to_string_name = Printf.sprintf "%s_to_string"
let pads_to_buffer_name = Printf.sprintf "%s_to_buffer"


(*
let rrep_name = rep_name
let rmd_name = md_name
let prep_name s = Printf.sprintf "%s_prep" s
let pmd_name s = Printf.sprintf "%s_pmd" s
*)

(*

let incload_name s = Printf.sprintf "%s_incload" s
let store_name s = Printf.sprintf "%s_store" s
let incstore_name s = Printf.sprintf "%s_incstore" s
let new_nameR s =  Printf.sprintf "%s_newRec" s
let cost_name s =  Printf.sprintf "%s_cost" s
let inccost_name s =  Printf.sprintf "%s_inccost" s
let incmanifest_name s = Printf.sprintf "%s_incmanifest" s
*)
