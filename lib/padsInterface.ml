open Pads
open Forest

let pads_to_forest (md : 'a Pads.pads_md) (path : Pads.filepath) : ('a Pads.pads_md Forest.forest_md) =
  { num_errors = md.pads_num_errors;
    error_msg = md.pads_error_msg;
    data = md;
    load_time = Forest.no_time;
    info = Forest.get_md_info path
  }

let load_for_forest (parse : Pads.filepath -> ('a * 'b Pads.pads_md))
    (path : Pads.filepath) : ('a * 'b Pads.pads_md Forest.forest_md) =
  let (rep,md) = parse path in (rep, pads_to_forest md path)

let pads_store = PadsParser.pads_store
