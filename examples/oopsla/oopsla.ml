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

let name = RE "[A-Za-z]+"
let nL = REd ("\r?\n","\n")
  (*
let gender = RE "[MF]"
let freq = RE "[0-9]+";;
  *)
   
[%%pads {|

 pdatatype gender =
 | Male of "M"
 | Female of "F"

 ptype record = { name : $name$; ","; gender : gender; ","; freq : Pint }
 ptype yob = record Plist($nL$,EOF)

|} ]
  
[%%forest {| 
  d = directory { files is [ f :: pads yob | f <- matches GL "*.txt" ] }
  d_inc = directory { files is [ f :: <pads yob> | f <- matches GL "*.txt" ] }
|} ]

  
let find name =
  let%bind cur = d_new "oopsla/names/" in
  let%bind (r,md) = load cur in
  if md.num_errors = 0 then
    return (List.iter
      (function
        | [] -> Printf.printf "Empty\n";
        | h::_ ->
           let gen =
             match h.gender
             with
             | Male _ -> "M"
             | Female _ -> "F"
           in
           Printf.printf "{ name = %s, gender = %s, freq = %d }\n%!" h.name gen h.freq
      )
      r.files)
  else
    return (List.iter (fun s -> Printf.printf "%s\n" s) md.error_msg)

let find_inc name =
  let%bind cur = d_inc_new "oopsla/names/" in
  let%bind (r,md) = load cur in
  if md.num_errors = 0 then
    let (fr,fmd) = Forest.sort_comp_path (r.files,md.data.files_md) in
    List.fold_right
      (fun c mon ->
        let%bind (r,md) = load c in
        match r with 
        | [] -> Printf.printf "Empty\n";
          mon
        | h::_ ->
           let gen =
             match h.gender
             with
             | Male _ -> "M"
             | Female _ -> "F"
           in
             Printf.printf "{ name = %s, gender = %s, freq = %d }\n%!" h.name gen h.freq;
             mon
      )
       fr (return ())
  else
    return (List.iter (fun s -> Printf.printf "%s\n" s) md.error_msg)

let _ = run @@
  (if Array.length Sys.argv > 1 && Sys.argv.(1) = "-inc" then
    find_inc
  else
    find)
  "oopsla/names/yob2000.txt"
  
