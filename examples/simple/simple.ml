(* 
Use Makefile in examples directory

Desugar:
   make simpleD

Compile:
   make simpleC

Run:
   make simpleR

*)

open Forest

module CostMon = CostNameMon
let exName = "simple";;

[%%forest {| 

  ofile = file option

  d = directory { foo is "ack" :: <file> ;
                  baz is "bar" :: ofile
                }
|}]


let get_ex_dir () = (Printf.sprintf "%s/%s/%s" (Sys.getcwd ()) exName "exDir")

let _ = 
  run (
  d_new (get_ex_dir ()) >>= fun dcur ->
  load dcur >>= fun (rep,md) ->
  load rep.foo >>= fun (str,_) ->
  return (Printf.printf "%s\n" str))
