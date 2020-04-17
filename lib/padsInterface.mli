(** PadsInterface provides an interface between Forest and Pads 

    Uses Pads, PadsParser, and Forest.
*)

val pads_to_forest : 'a Pads.pads_md -> Pads.filepath -> 'a Pads.pads_md Forest.forest_md

(** [pads_to_forest] turns a PADS metadata into a Forest metadata and
    should be used for pre-existing files *)

val new_pads_to_forest : 'a Pads.pads_md -> Pads.filepath -> 'a Pads.pads_md Forest.forest_md

(** [new_pads_to_forest] turns a PADS metadata into a Forest metadata and
    should be used for not-yet-existing files *)
  
val load_for_forest :  (Pads.filepath -> ('a * 'b Pads.pads_md)) -> Pads.filepath -> ('a * 'b Pads.pads_md Forest.forest_md)

(** [load_for_forest] takes a PADS parsing function and a filepath and
    gets back a Forest representation of the parsed data *)
  
val pads_store : 'a Pads.padsManifest -> Pads.filepath -> unit

(** [pads_store] stores data from a PADS manifest to the given filepath *)
