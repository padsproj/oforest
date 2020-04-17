(* 
Use Makefile in examples directory

Desugar:
    ./desugar.sh simple/simple.ml

Compile:
   make simple
*)

open Forest

module CostMon = CostNameMon
let exName = "simple";;

[%%forest {| 

  ofile = file option

  d = directory { foo is "ack" :: <file> ;
                  baz is "bar" :: ofile
                }

  test = [x :: file | x <- matches RE ".*"]
|}]


let get_ex_dir () = (Printf.sprintf "%s/%s/%s" (Sys.getcwd ()) exName "exDir")

let _ = 
  run (
  d_new (get_ex_dir ()) >>= fun dcur ->
  load dcur >>= fun (rep,md) ->
  load rep.foo >>= fun (str,_) ->
  return (Printf.printf "%s\n" str))

let add_bar () =
  let open Core in
  Printf.printf "Adding bar!\n";
  let (rep,md) = test_load (get_ex_dir ()) in
  let goal_path = (get_ex_dir () |> Filename.realpath) ^/ "bar"  in
  let new_rep,new_md =
    ("a string" :: rep,
      { md with data = (empty_md () goal_path) :: md.data}
    )
  in
  test_manifest (new_rep,new_md) |> store

let rem_bar () =
  let open Core in
  Printf.printf "Removing bar!\n";
  let (rep,md) = test_load (get_ex_dir ()) in
  let (rep,data) = 
    List.zip_exn rep md.data
    |> List.filter ~f:(fun (_,md) -> 
        let name = get_path_exn md |> Filename.basename in
        not (String.equal "bar" name))
    |> List.unzip
  in
  List.iter rep ~f:print_endline;
  Printf.printf "Number of reps,mds: %d,%d\n" (List.length rep) (List.length data);
  test_manifest (rep,{md with data}) |> store
let change_bar = 
  let open Core in
  let goal_path = (get_ex_dir () |> Filename.realpath) ^/ "bar"  in
  match Sys.file_exists goal_path with
  | `No -> add_bar ()
  | `Yes -> rem_bar ()
  | `Unknown -> failwith "How is this unknown?"
