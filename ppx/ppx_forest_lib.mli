(** Ppx_forest_lib is where the magic happens. This is the library that turns a
    Forest AST into an OCaml AST. *)

(** {2 Main Functions} *)

val forest_rep_gen : Forest_types.forest_node Forest_types.ast -> Parsetree.type_declaration list * Parsetree.core_type

val forest_md_gen : Forest_types.forest_node Forest_types.ast -> Parsetree.type_declaration list * Parsetree.core_type

(** These functions take a Forest AST and generate the representation and
    metadata type corresponding to it *)
  
val forest_load_gen : Forest_types.forest_node Forest_types.ast -> string -> Parsetree.expression
val forest_manifest_gen : bool -> Forest_types.forest_node Forest_types.ast -> string -> Parsetree.expression
val forest_new_gen : ?first:bool -> Forest_types.forest_node Forest_types.ast -> string -> Parsetree.expression

(** These functions take a Forest AST and generate the load, manifest, and new
    functions respectively. The 'first' boolean is required to determine if we
    are making a nested call or not (which needs to be handled slightly
    differently). *)
  
val forest_cost_gen :  Forest_types.forest_node Forest_types.ast ->  Forest_types.varname -> Parsetree.expression

(** Generates logic for calculating the costs of loading the given Forest AST *)
  
val forest_uninc_load_gen : Forest_types.forest_node Forest_types.ast -> string -> Parsetree.expression

(** Generates an unincremental load function from a Forest AST. Used for forcing
    delays *)

(** {2 Analysis/Helper Functions} *)
  
val init : Location.t -> Parsetree.structure_item list

(** Generates an OCaml AST the first time the forest extension is used, placed
    before anything else. Opens various modules, defines the CursorMonad based on
    the cost monoid CostMon and declares generic load and manifest functions in this
    monad *)
  
val delay_checker : Forest_types.forest_node Forest_types.ast -> bool

(** Checks if there are any delays in the Forest AST *)
  
val dependency_grapher : Forest_types.varname list -> Forest_types.forest_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast

(** Figures out the various dependencies in a Forest ASTs, storing the important
    parts as payloads in the AST nodes. *)
  
val fix_exp : Forest_types.forest_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast

(** Turns a given Forest AST into a cleaner Forest AST by removing double delays
    and rearranging how paths are delayed *)

val def_generator : Ast_helper.loc -> (Forest_types.varname * Forest_types.forest_node Forest_types.ast) list -> Parsetree.structure

(** Calls all the other functions to turn a Forest AST into an OCaml AST *)
