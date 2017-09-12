(* 
Use Makefile in examples directory

Desugar:
    ./desugar.sh universal/universal.ml

Compile:
   make universal
*)

module CostMon = Forest.CostNameMon

  
[%%skin {|
  uniSkin = dir([<>])
|}]

[%%forest {|
  universal = directory { asc is [ f :: file | f <- matches RE ".*", $get_kind f_att = AsciiK$];
                          bin is [ b :: file | b <- matches RE ".*", $get_kind b_att = BinaryK$];
                          sym is [ l :: link | l <- matches RE ".*", $get_kind l_att = SymK$];
                          dir is [ d :: universal | d <- matches RE ".*", $get_kind d_att = DirectoryK$]
                        }

  universal_inc = universal @ uniSkin

|}]

  
let get_dir path = 
  if Filename.is_relative path
  then Filename.concat (Sys.getcwd ())  path
  else path

let rec trawl_univI cur nMax =
  let rec trawl_internal cur n =
    if n >= nMax
    then return ()
    else begin
      load cur >>= fun ((r,r_md) : (universal_inc_rep * universal_inc_md)) ->
      let dirName = match r_md.info with
        | None -> "Error: No directory?"
        | Some info -> info.full_path
      in
      let _ = Printf.printf "Dir: %s\n" dirName in
      let _ = Forest.print_md_errors r_md in
      let _ = Printf.printf "File contents:\n" in
      let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.asc in
      List.fold_left (fun acc d -> 
        trawl_internal d (n+1) >>= fun _ ->
        acc                          
      ) (return ()) r.dir 
    end
  in
  trawl_internal cur 0

    
(* The next two functions are there to show how storing works.
 * They will go through up to nMax = depth levels and add a line/remove a line
 * respectively from each ascii file they find. The second should only be used after the first.
 *)

let rec add_lineI cur nMax =
  let rec add_lineInternal cur n =
    if n >= nMax
    then return ()
    else begin
      load cur >>= fun ((r,r_md) : (universal_inc_rep * universal_inc_md)) ->
      let r = {r with asc = (List.map (fun s ->Bytes.cat s "\nAdded Line!") r.asc)} in
      manifest cur (r,r_md) >>= fun mani ->
      let _ = if List.length mani.errors > 0 then Forest.print_manifest_errors mani else store mani in
      List.fold_left (fun acc d -> 
        add_lineInternal d (n+1) >>= fun _ ->
        acc                          
      ) (return ()) r.dir
    end
  in
  add_lineInternal cur 0

let rec remove_lineI cur nMax = 
  let rec remove_lineInternal cur n =
    if n >= nMax
    then return ()
    else begin
      load cur >>= fun ((r,r_md) : (universal_inc_rep * universal_inc_md)) ->
      let r = {r with asc = (List.map (fun s ->
        let pos = Str.search_forward (Str.regexp_string "\nAdded Line!") s 0 in
        Bytes.sub s 0 pos
      ) r.asc)}
      in
      manifest cur (r,r_md) >>= fun mani ->
      let _ = if List.length mani.errors > 0 then Forest.print_manifest_errors mani else store mani in
      List.fold_left (fun acc d -> 
        remove_lineInternal d (n+1) >>= fun _ ->
        acc                          
      ) (return ()) r.dir
    end
  in
  remove_lineInternal cur 0

    
let trawl_univ (r : universal_rep) nMax =
  let rec trawl_internal (r : universal_rep) n =
    if n >= nMax
    then ()
    else
      let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.asc in
      List.iter (fun d -> trawl_internal d (n+1)) r.dir
  in
  trawl_internal r 0
    
let incremental directory nMax = 
  universal_inc_new directory >>= fun cur ->
  trawl_univI cur nMax >>| fun () ->
  Printf.printf "\n"
    
let unIncremental directory nMax =
  universal_new directory >>= load >>| fun (r,md) ->
  let _ = trawl_univ r nMax in
  Printf.printf "\n"

let main () =
  if Array.length Sys.argv < 2
  then begin 
    Printf.printf "Usage: %s <directory> [<mode>] [<depth>]\n" Sys.argv.(0);
    Printf.printf "Mode is inc or classic. inc is default\n";
    Printf.printf "Depth is how deep into the folder we should traverse. 2 is default.\n";
    exit 1;
  end
  else
    let inc = 
      if Array.length Sys.argv >= 3 then Sys.argv.(2) <> "classic" else true
    in
    let depth =
      if Array.length Sys.argv >= 4 then int_of_string (Sys.argv.(3)) else 2
    in
    let dir = get_dir Sys.argv.(1) in
    run (
      if inc
      then incremental dir depth
      else unIncremental dir depth)
      
let _ = main ()
