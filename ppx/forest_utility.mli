(** Forest_utility defines a number of global variables and functions mainly used by
    Ppx_forest_lib and skins.
    
    Uses Forest_types, Asttypes, Parsetree, and Ast_helper.    
*)

open Forest_types

(** {2 Globals} *)

val debug : bool

(** [debug] is used for debugging. Some extra debug information is printed when
    it it true *)
  
val firstDef : bool ref

(** The first time a Forest description is expanded, some universal types and
    code is spit out. This ref makes it only happen once. *)
  
val fresh_cell : int ref

(** Used for making new forest variables *)

  
val skinTbl : (Forest_types.varname, Forest_types.skin_node Forest_types.ast) Hashtbl.t
val forestTbl : (Forest_types.varname, Forest_types.forest_node Forest_types.ast) Hashtbl.t
  
(** These hash tables are mainly used for finding named skins and forest asts
    respectively for the purpunitoses of skinning. *)
  
(** {2 AST Functions } *)
  
val make_ast_lid : Location.t -> string -> Longident.t Asttypes.loc
val make_ast_str : Location.t -> string -> string Asttypes.loc
val exp_make_ident : Ast_helper.loc -> string -> Parsetree.expression
val exp_make_string : Ast_helper.loc -> string -> Parsetree.expression
val exp_make_int : Ast_helper.loc -> int -> Parsetree.expression
val exp_make_construct :  Ast_helper.loc -> ?exp:Parsetree.expression -> string -> Parsetree.expression
val exp_make_field : Ast_helper.loc -> Parsetree.expression -> string -> Parsetree.expression
val exp_make_field_n : Ast_helper.loc -> string -> string -> Parsetree.expression
val exp_make_record : Ast_helper.loc -> (string * Parsetree.expression) list -> Parsetree.expression
val exp_make_record_s : Ast_helper.loc -> (string * string) list -> Parsetree.expression
val exp_make_match : Ast_helper.loc ->  Parsetree.expression -> Parsetree.case list -> Parsetree.expression
val exp_make_tup : Ast_helper.loc -> Parsetree.expression list -> Parsetree.expression
val pat_make_var : Ast_helper.loc -> string -> Parsetree.pattern
val pat_make_construct : Ast_helper.loc -> ?pat:Parsetree.pattern -> string -> Parsetree.pattern
val pat_make_tup : Ast_helper.loc -> Parsetree.pattern list -> Parsetree.pattern
val typ_make_type_decl : Ast_helper.loc -> ?manifest:Parsetree.core_type -> ?kind:Parsetree.type_kind -> string -> Parsetree.type_declaration
val typ_make_field : Ast_helper.loc -> string -> Parsetree.core_type -> Parsetree.label_declaration
val typ_make_constr : Ast_helper.loc -> string -> Parsetree.core_type
val typ_make_variant : Ast_helper.loc -> ?args:Parsetree.constructor_arguments -> ?res:Parsetree.core_type -> string -> Parsetree.constructor_declaration
val typ_make_tup : Ast_helper.loc -> Parsetree.core_type list -> Parsetree.core_type
val exp_make_ocaml : Location.t -> string -> Parsetree.expression

(** These functions are all used to make OCaml AST nodes and identifiers of
    various types, usually from strings and other AST nodes *)
  
(** {2 Helper Functions} *)
  
val raise_loc_err : Location.t -> string -> 'a

(** Raises an error with better location information *)

val debug_out : string -> string

(** [debug] [s] is [s] if debug is true and "" otherwise. Used with error
    messages. *)

val get_NaL : 'a Forest_types.ast -> 'a * Forest_types.loc
val get_loc : 'a Forest_types.ast -> Location.t

(** Gets the node and location or just the location out from an ast
    structure. *)
  
val mk_p_ast : Forest_types.loc -> Forest_types.fPayload -> 'a -> 'a Forest_types.ast
val mk_ast : Forest_types.loc -> 'a -> 'a Forest_types.ast

(** Create new asts with and without a payload respectively *)
  
val find_ident_in_str : string -> string -> bool

(** Checks for an identifier in a string *)
  
val fresh : unit -> string

(** Gets a fresh forest identifier *)
  
val representation_name : string -> string
val md_name : string -> string
val load_name : string -> string
val list_name : string -> string
val att_name : string -> string
val acc_name : string -> string
val new_name : string -> string
val new_nameR : string -> string
val cost_name : string -> string
val manifest_name : string -> string
val empty_manifest_name : string -> string
val ucur_name : string -> string
val pads_representation_name : string -> string
val pads_md_name : string -> string
val pads_parse_name : string -> string
val pads_manifest_name : string -> string
val pads_empty_manifest_name : string -> string

(** Defines the names of various types and functions based on the name of the
    description *)
