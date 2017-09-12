(** Ppx_type_generators is the library that generates all of the types
    that the Forest language translates into *)

open Forest_types
open Parsetree

val representation_type_generator: forest_node ast -> type_declaration list * core_type

val md_type_generator: forest_node ast -> type_declaration list * core_type

val manifest_type_generator: forest_node ast -> type_declaration list * core_type
    
(** These functions take a Forest AST and generate the representation, metadata,
    and manifest types corresponding to it *)
