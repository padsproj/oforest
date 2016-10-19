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
let exName = "simple_list_files";;

[%%forest {| 

  ofile = file option

  d = directory { foo is [f :: file | f <- matches GL "*"] ;
                  baz is "bar" :: ofile
                }
|}]


let get_ex_dir () = (Printf.sprintf "%s/%s/%s" (Sys.getcwd ()) exName "exDir")

let _ = 
  run (
  d_new (get_ex_dir ()) >>= fun dcur ->
  load dcur >>= fun (rep,md) ->
 	List.iter (fun md -> match md.info with
	| None -> ()
	| Some info -> Printf.printf "%s\n" info.full_path
	) md.data.foo_md.data;
	return ())
