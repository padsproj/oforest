(** Ppx_forest_lib is where the magic happens. This is the library that turns a
    Forest AST into an OCaml AST. Calls out to Ppx_type_generators and Ppx_function_generators. *)

open Forest_types

(** {2 Analysis/Helper Functions} *)
  
val init : Location.t -> Parsetree.structure_item list

(** Generates an OCaml AST the first time the forest extension is used, placed
    before anything else. Opens various modules, defines the CursorMonad based on
    the cost monoid CostMon and declares generic load and manifest functions in this
    monad *)
  
val delay_checker : forest_node ast -> bool

(** Checks if there are any delays in the Forest AST *)
  
val dependency_grapher : varname list -> forest_node ast -> forest_node ast

(** Figures out the various dependencies in a Forest ASTs, storing the important
    parts as payloads in the AST nodes. *)
  
val fix_exp : forest_node ast -> forest_node ast

(** Turns a given Forest AST into a cleaner Forest AST by removing double delays
    and rearranging how paths are delayed *)

(** {2 Main function} *)
  
val def_generator : Ast_helper.loc -> (varname * forest_node ast) list -> Parsetree.structure

(** Calls all the other functions to turn a Forest AST into an OCaml AST *)
