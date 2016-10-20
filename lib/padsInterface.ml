open Pads
open PadsParser
open Forest

let pads_to_forest (md : 'a pads_md) (path : string) : ('a pads_md Forest.forest_md) =
  { num_errors = md.pads_num_errors;
    error_msg = md.pads_error_msg;
    data = md;
    load_time = no_time;
    info = Forest.get_md_info path
  }

let load_for_forest (parse : filepath -> ('a * 'b pads_md)) (path : filepath) : ('a * 'b pads_md forest_md) =
  let (rep,md) = parse path in (rep, pads_to_forest md path)

let pads_store mani path =
  let data = mani.pads_str in
  Core.Std.Out_channel.write_all path ~data
