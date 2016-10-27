(** Ppx_forest defines the [forest_mapper], which finds structures with [forest]
    and [skin] extensions and expands them to OCaml ASTs.
*)

val forest_mapper : string list -> Ast_mapper.mapper
