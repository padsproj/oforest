(* 
Use Makefile in examples directory

Desugar:
    ./desugar.sh simpleShell/simpleShell.ml

Compile:
   make simpleShell
*)
[%%skin {|
  uniSkin = [<>] + map(uniSkin)
  
|}]

module CostMon = Forest.CostNameMon

[%%forest {|

  universal = directory { asc is [ f :: file | f <- matches GL "*", $get_kind f_att = AsciiK$];
                          bin is [ b :: file | b <- matches GL "*", $get_kind b_att = BinaryK$];
                          sym is [ l :: link | l <- matches GL "*", $get_kind l_att = SymK$];
                          dir is map [ d :: universal | d <- matches GL "*", $get_kind d_att = DirectoryK$] 
                        }

  universal_inc = universal@uniSkin

|}]

type command =
  | Exit
  | Empty
  | Ls of string
  | Cd of string
  | NoCmd

(* Helpers *)
      
let get_path md =
  match md.info with
  | None -> ""
  | Some info -> info.full_path

let ls_inc (md : universal_inc_md) options = 
  let md_list = List.concat [md.data.asc_md.data;md.data.bin_md.data;md.data.sym_md.data] in
  let files = List.map get_path md_list in
  let files = List.rev_append files @@ List.map (fun (_,md) -> get_path md)
    @@ PathMap.bindings md.data.dir_md.data in
  if files <> []
  then
    let files = List.map Filename.basename files in
    let files = String.concat " " files in
    Sys.command @@ Printf.sprintf "ls %s -d %s" files options
  else -1
    
let parse_input input =
  let sList = Str.split (Str.regexp " +") input in
  match sList with
  | [] -> Empty
  | hd :: tl ->
     if hd = "exit"
     then Exit
     else if hd = "ls"
     then Ls (String.concat " " tl)
     else if hd = "cd"
     then Cd (String.concat " " tl)
     else NoCmd

(* Main program *)
    
let rec main_loop cdir ((rep,md) : (universal_inc_rep * universal_inc_md)) =
  let _ = Printf.printf "> %!" in
  let input = input_line stdin in
  let cmd = parse_input input in
  match cmd with
  | Exit -> return 0
  | Empty -> main_loop cdir (rep,md)
  | NoCmd -> Printf.printf "Error: Command not recognized\n%!";
    main_loop cdir (rep,md)
  | Ls arg ->
     let _ = ls_inc md arg in
     main_loop cdir (rep,md)
  | Cd arg ->
     let ndir = Filename.concat cdir arg in
     if arg = ".."
     then return 1
     else if arg = "."
     then main_loop cdir (rep,md)
     else if PathMap.mem ndir rep.dir
     then 
       let%bind (nrep,nmd) = load (PathMap.find ndir rep.dir) in
       let _ = Sys.chdir ndir in
       let%bind code = main_loop ndir (nrep,nmd) in
       if code = 1
       then
         let _ = Sys.chdir cdir in
         main_loop cdir (rep,md)
       else return code
     else 
       let _ = Printf.printf "%s is not a valid directory.\n" arg in
       main_loop cdir (rep,md)
    
let main =
  let _ = Printf.printf "This shell recognizes ls,cd, and exit commands\n%!" in
  let cdir = if Array.length Sys.argv > 1
    then
      let cdir = Sys.argv.(1) in
      if Filename.is_relative cdir
      then Filename.concat (Sys.getcwd ()) cdir
      else cdir
    else Sys.getcwd ()
  in
  let _ =  Sys.chdir cdir in
  run (
    universal_inc_new cdir >>=
      load >>= main_loop cdir
  )


    
(* Unincremental stuff, which is unused
let exDir =
  let cwd =  Sys.getcwd () in
  let d1 = Filename.concat cwd "examples/simpleShell/dir" in
  if Sys.file_exists d1
  then d1
  else Filename.concat cwd "simpleShell/dir"
    
let ls_uninc md options =
  let md_list = List.concat [md.data.asc_md.data;md.data.bin_md.data;md.data.sym_md.data] in
  let files = List.map get_path md_list in 
  let files = List.rev_append files @@ List.map (fun (_,md) -> get_path md)
    @@ PathMap.bindings md.data.dir_md.data in
  if files <> []
  then
    let files = List.map Filename.basename files in
    let files = String.concat " " files in
    Sys.command @@ Printf.sprintf "ls %s -d %s" files options
  else -1

  
let main_uninc () =
  let (rep,md) = universal_load exDir in
  ls_uninc md "-l"
 *)
