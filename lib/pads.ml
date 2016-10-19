(* Types! *)

type 'a pads_md =
  { pads_num_errors : int;
    pads_error_msg : string list;
    pads_data : 'a;
    pads_extra : (string * string) list
  }

type filepath = string


(* Regular expressions.
 * Snd part of REd is default rep *)
type pads_re = 
  | RE of string
  | REd of string * string


      
(* Often used as terminators *)
type pads_constant =
  | PTI of int
  | PTS of string
  | PTRE of pads_re
  | PTEOF

      
type read_result = 
  | Error of string list 
  | Contents of string list

type pads_manifest_errors =
| RegexMatchError of string
| ListLengthError
| VariantMismatchError
| ListLengthMismatchError

type 'a padsManifest =
  { pads_man_errors : pads_manifest_errors list;
    pads_str : string;
    pads_manifest : 'a}

(* FUNCTIONS! *)

let empty_md x = 
  { pads_num_errors = 0;
    pads_error_msg = [];
    pads_data = x;
    pads_extra = []
  }

let error_md l x =
  { pads_num_errors = List.length l;
    pads_error_msg = l;
    pads_data = x;
    pads_extra = []
  }

let make_mani s m =
  { pads_man_errors = [];
    pads_str = s;
    pads_manifest = m}

(* TODO: See if you can replace with Str *)
let sub_starts_with s1 s2 = 
  String.length s1 >= String.length s2 &&
  String.sub s1 0 (String.length s2) = s2

let read_file (path:string) : read_result =
  if not (Sys.file_exists path) then
    Error [Printf.sprintf "No such file: %s" path]
  else if Sys.is_directory path then
    Error [Printf.sprintf "Is a directory: %s" path]
  else 
    (*
    let l = Core.Std.In_channel.read_all path in
    *)
    let ch = open_in path in
    let l = Core.Std.In_channel.input_lines ~fix_win_eol:false ch in
    let _ = close_in ch in
    Contents l 
