open Swat
open Pads
open Forest

<:pads<
rchKV = { "REACH"; stringLn }
rchKVLines = List rchKV sep "" terminator EOF
rchKVFile = {
  List stringLn length 9;
  rchKVInfo = rchKVLines
}

>>


<:forest<

rchNoParse = file

rchKVParse = Pads rchKVFile

rchFullParse = Pads reachFile

>>

let get_time load_func file =
  let (r,m) = load_func file in
  m.load_time

let get_times input =
  let full_time = get_time rchFullParse_load input in
  let kv_time = get_time rchKVParse_load input in
  let none_time = get_time rchNoParse_load input in
  (full_time, kv_time, none_time)

let () =
  if Array.length Sys.argv <> 2
  then 
    let _ = Printf.eprintf "swatTimeParse takes 1 argument: filename" in
    exit 1
  else
    let input = Sys.argv.(1) in
    let (full, kv, file) = get_times input in
    let ts = Core.Time.Span.to_string in
    Printf.printf "%s %s %s" (ts full) (ts kv) (ts file)
