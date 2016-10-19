open Forest
open Pads
open PadsParser

let pads_load (def_rep : 'a) (def_md : 'b pads_md) (parse : ('a, 'b pads_md) pads_parser) (path : string) : ('a * 'b pads_md) =
  match read_file path with
  | Error l -> (def_rep, {def_md with 
    pads_num_errors = def_md.pads_num_errors+1; 
    pads_error_msg = l @ def_md.pads_error_msg})
  | Contents s ->
      (*Printf.printf "Lines in %s: %d\n" path (List.length s); *)
     let (rep, md, final) = parse (new_state s) in
     (* TODO: Add extra text to md *)
     (rep,md)
      (* TODO: Don't think I care if there's extra text 
      if final.rest = [] && String.length final.current = final.loc.character - 1 then (rep, md)
      else (rep, 
        {md with pads_num_errors=md.pads_num_errors + 1; 
        pads_error_msg = (Printf.sprintf "Extra text %s:" final.current 
         ^ (String.concat "\n" final.rest)) :: md.pads_error_msg})
      *)

let pads_to_forest (md : 'a pads_md) (path : string) : ('a pads_md Forest.forest_md) =
  { num_errors = md.pads_num_errors;
    error_msg = md.pads_error_msg;
    data = md;
    load_time = no_time;
    info = Forest.get_md_info path
  }

let pads_load_for_forest (parse : filepath -> ('a * 'b pads_md)) (path : filepath) : ('a * 'b pads_md forest_md) =
  let (rep,md) = parse path in (rep, pads_to_forest md path)
  
let pads_store mani path =
  let data = mani.pads_str in
  Core.Std.Out_channel.write_all path ~data
