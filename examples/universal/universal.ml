(* 
Should be executed from main oforest directory

Desugar:
  ./desugar.sh examples/featTesting/test.ml

Compile:
   ./compileEx.sh examples/featTesting/test.ml

Run:
   ./test

*)

module CostMon = Forest.CostNameMon

[%%forest {|
  universal = directory { asc is [ f :: file | f <- matches RE ".*", $get_kind f_att = AsciiK$];
                          bin is [ b :: file | b <- matches RE ".*", $get_kind b_att = BinaryK$];
                          sym is [ l :: link | l <- matches RE ".*", $get_kind l_att = SymK$];
                          dir is [ <d :: universal> | d <- matches RE ".*", $get_kind d_att = DirectoryK$]
                        }
|}]

(* Currently Examples directory 
 * Sadly, most directories seem to get 'too many open files' error (EMFILE) when doing the unincremental version...*)

let directory = Printf.sprintf "%s/.." (Sys.getcwd ())

let rec trawl_univI cur nMax =
  let rec trawl_internal cur n =
    if n >= nMax
    then return ()
    else begin
      load cur >>= fun (r,r_md) -> (*
      let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.asc in
      let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.sym in
                                   *)
      List.fold_left (fun acc d -> 
        trawl_internal d (n+1) >>= fun _ ->
        acc                          
      ) (return ()) r.dir 
    end
  in
  trawl_internal cur 0

let incremental () = 
  universal_new directory >>= fun cur ->
  trawl_univI cur 2 >>= fun () ->
  return ()

let _ = 
  let (_,c) = run (incremental ()) in
  List.iter (fun (name,num) -> Printf.printf "%s:%d\n" name num) c
