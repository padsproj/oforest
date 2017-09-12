(* 
Use Makefile in examples directory

Desugar:
    ./desugar.sh simple/simple.ml

Compile:
   make simple
*)

open Forest
open Core

module CostMon = CostNameMon
let exName = "simple";;

[%%forest {| 

  f = file

  ofile = file option

  d = directory { foo is "ack" :: <file> ;
                  baz is "bar" :: ofile
                }
 |}]
    


let get_ex_dir () = Filename.concat  (Sys.getcwd ()) "dev/simpleTest/exDir"

let _ =
  let monad = 
    Filename.concat (get_ex_dir ()) "ack"
    |> f_new
    >>= load
    >>= fun (str, _) ->
    return (Printf.printf "%s\n" str)
  in
  run monad
    
let _ = 
  run (
  d_new (get_ex_dir ()) >>= fun dcur ->
  load dcur >>= fun (rep,md) ->
  load rep.foo >>= fun (str,_) ->
  return (Printf.printf "%s\n" str))
