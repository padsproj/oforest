(* 
Use Makefile in examples directory

Desugar:
   ./desugar.sh oopsla/oopsla.ml

Compile:
   make oopsla

This example uses data from:
https://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-level-data
*)

open Pads
open Forest
open Printf
open String

module CostMon = CostUnitMon

let lines = Str.split (Str.regexp "\n")

let location = RE "[A-Z]+"
let gender = RE "[MF]"
let name = RE "[A-Za-z]+"
let nL = REd ("\r?\n","\n")

[%%pads {|
 ptype item = { location : $location$; ",";
                gender : $gender$; ",";
                year : Pint; ",";
                name : $name$; ",";
                freq : Pint }

 ptype items = item Plist($nL$,EOF)
|} ]
  
[%%forest {| 
  d = directory { files is [ f :: pads items | f <- matches GL "*.TXT" ] }

  d_inc = directory { files is [ f :: <pads items> | f <- matches GL "*.TXT" ] }
|} ]


let search pred print list =
  try
    let x = List.find pred list in
    Printf.printf "%s\n%!" (print x)
  with Not_found ->
    Printf.printf "Not found\n%!"

let string_contains s x =
  let rx = Str.regexp (Printf.sprintf ".*%s.*" x) in
  Str.string_match rx s 0 

let search_strings name list =
  let pred = (fun s -> string_contains s name) in
  let print = (fun s -> s) in
  search pred print list

let search_items name items =
  let pred = (fun i -> string_contains i.name name) in
  let print = (fun i -> item_to_string (i, item_default_md)) in
  search pred print items
  
let read_lines (fn:string) : string list =
  let r = ref [] in
  let ch = open_in fn in
  try
    while true do
      r := input_line ch :: !r
    done;
    []
  with End_of_file ->
    close_in ch;
    List.rev !r

let find_classic dir name =
  let%bind cur = d_new dir in 
  let%bind (r,md) = load cur in
  if md.num_errors = 0 then
    return (List.iter (search_items name) r.files)
  else
    return (List.iter (fun s -> Printf.printf "%s\n%!" s) md.error_msg)

let find_incremental dir name = 
  let%bind cur = d_inc_new dir in 
  let%bind (r,md) = load cur in
  if md.num_errors = 0 then
    let (fr,fmd) = Forest.sort_comp_path (r.files,md.data.files_md) in
    let rec loop = function
      | [] -> return ()
      | c::rest ->
         let%bind (items,md) = load c in
         search_items name items;
         loop rest in 
    loop fr
  else
    return (List.iter (fun s -> Printf.printf "%s\n%!" s) md.error_msg)

let find_manual (dir:string) (name:string) : unit = 
  let files : string array = Sys.readdir dir in 
  Array.iter
    (fun file ->
     let lines = read_lines (Printf.sprintf "%s/%s" dir file) in 
     search_strings name lines)
    files 

let usage () = 
  Printf.printf "usage: %s [options] <dir> <name>\n" Sys.argv.(0);
  Printf.printf "  -manual : Manual OCaml\n";
  Printf.printf "  -classic : Classic Forest\n";
  Printf.printf "  -incremental : Incremental Forest\n";
  exit 1
  
let _ =
  if Array.length Sys.argv < 4 then
    usage ()
  else
    let cmd = Sys.argv.(1) in
    let dir = Sys.argv.(2) in
    let name = Sys.argv.(3) in 
    match cmd with
    | "-classic" -> run @@ find_classic dir name
    | "-incremental" -> run @@ find_incremental dir name
    | "-manual" -> run @@ (find_manual dir name; return ())
    | _ -> usage ()
                 
