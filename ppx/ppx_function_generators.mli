(** Ppx_function_generators is the library that generates all of the loading, storing, 
    and auxiliary functions that the Forest language translates into *)

open Forest_types
open Parsetree
  
val load_function_generator: forest_node ast -> varname -> expression
val manifest_function_generator: bool -> forest_node ast -> varname -> expression
val new_function_generator: ?first:bool -> forest_node ast -> varname -> expression

(** These functions take a Forest AST and generate the load, manifest, and new
    functions respectively. The 'first' boolean is required to determine if we
    are making a nested call or not (which needs to be handled slightly
    differently). *)
  
val cost_function_generator: forest_node ast -> varname -> expression

(** Generates logic for calculating the costs of loading the given Forest AST *)

val empty_manifest_generator: forest_node ast -> expression

(** Generates an empty manifest based on the given Forest AST *)
