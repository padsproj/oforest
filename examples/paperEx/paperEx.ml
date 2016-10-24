(* 
Use Makefile in examples directory

Desugar:
    ./desugar.sh paperEx/paperEx.ml

Compile:
   make paperEx
*)

open Forest

let exName = "paperEx"

let lines = Str.split (Str.regexp "\n") ;;
module CostMon = Forest.CostNameMon

[%%skin {|

  d1Skin = {_,[<>]}
  d2Skin = data(<>)
  d3Skin = {<>,<>}
  d4Skin = {~,_}

  delayAll = <>; map(delayAll)
  delayFiles = <>|file + map(delayFiles)

|}]

[%%forest {| 
  d = directory { 
        index is "index.txt" :: file;
        data is [ "data" :: f :: file | f <- $lines index$ ] 
      }
  
  d1 = d @ d1Skin
  d2 = d @ d2Skin
  d3 = d @ d3Skin
  d4 = d @ d4Skin
|}]

open Printf
open String

let get_ex_dir () = (sprintf "%s/%s/%s" (Sys.getcwd ()) exName "exDir")

let classic path =
  let (rep,md) = d_load path in 
  if md.num_errors = 0 then
    let go f s = printf "%s: %d\n" f (length s) in
    List.iter2 go (lines rep.index) rep.data
  else
    let error = String.concat "\n" md.error_msg in
    failwith (Printf.sprintf "%s" error)
      
let inc path =
  d1_new path >>= 
  load >>= fun (r, md) ->
  if md.num_errors = 0 then
    let go f cur acc =
      load cur >>= fun (s,_) ->
      printf "%s: %d\n" f (length s);
      acc
    in
    List.fold_right2 go (lines r.index) r.data (return ())
  else
    let error = String.concat "\n" md.error_msg in
    failwith (Printf.sprintf "%s" error)

let _ =
  let _ = classic (get_ex_dir ()) in
  let (_,c) = run (inc @@ get_ex_dir ()) in
  List.iter (fun (name,num) -> Printf.printf "%s:%d\n" name num) c
