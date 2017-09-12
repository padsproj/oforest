(* 
Should be executed from main oforest directory

Desugar:
  ./desugar.sh examples/featTesting/test.ml

Compile:
   ./compileEx.sh examples/featTesting/test.ml

Run:
   ./test

*)

(* Start with this line to define a cost model *)
module CostMon = Forest.CostNameMon

let exName = "featTesting";;

[%%skin {|
  idSkin = _
  uniSkin2 = dir([<>])
  uniSkin = {_,_,_,[<>|rec]}
  skin4 = dir([<>|rec]) ; bin([~])
  skin5 = (< > option) + { <>,_} + <>
  skinTest = { <>,_,_};{ ><,_,_}

  delayAll = <>;map(delayAll)
  testSkin1 = <>|typeof ofile;map(testSkin1)  

  mgtSkin = delayAll;foo( > < );baz( > < )
  testSkin2 = ~;map(testSkin2)   
  skin3 =  >< |file;asc ( [ <> ]); map(skin3)
|}]


let lines = Str.split (Str.regexp "\n") ;;

[%%forest {| 

  super = directory {f is "woo" :: <file>;
                     g is [x :: <file where $this = f$> | x <- $["woo"]$]}

  test_desc = directory { b is "hey" :: file;
                          a is "noo" :: file
                          } where $get_kind this_att == DirectoryK$ 

  path_file = "foo" :: file

  ofile = file option

  d = directory { foo is "index.txt" :: file;
                  s is map [l :: link | l <- $["some_file"]$ ];
                  noFiles is [ f :: file | f <- matches GL "*"] where $List.length this = 0$;
                  baz is "test" :: ofile
                }

   universal = directory { asc is [ f :: file | f <- matches GL "*", $get_kind f_att = AsciiK$];
                           bin is [ b :: file | b <- matches RE ".*", $get_kind b_att = BinaryK$];
                           sym is [ l :: link | l <- matches RE ".*", $get_kind l_att = SymK$];
                           dir is [ d :: universal | d <- matches RE ".*", $get_kind d_att = DirectoryK$] 
                         }
(*   
   universal_inc = directory { asc is [ f :: file | f <- matches RE ".*", $get_kind f_att = AsciiK$];
                           bin is [ b :: file | b <- matches RE ".*", $get_kind b_att = BinaryK$];
                           sym is [ l :: link | l <- matches RE ".*", $get_kind l_att = SymK$];
                           dir is [ d :: <universal_inc> | d <- matches RE ".*", $get_kind d_att = DirectoryK$]
                         }
*)
   new_ofile = d @ mgtSkin


  universal_inc = universal@uniSkin2
(*

  let s = new_ofile

    urlTest = URL file
 *)
|}]


let get_ex_dir () = (Printf.sprintf "%s/examples/%s/%s" (Sys.getcwd ()) exName "tstdir")

let rec trawl_univI (cur :  (universal_inc_rep, universal_inc_md) cursor) =
  load cur >>= fun ((r,r_md) : (universal_inc_rep * universal_inc_md)) ->
  let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.asc in
  let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.sym in
  List.fold_left (fun acc d -> 
    trawl_univI d >>= fun _ ->
    acc                          
  ) (return ()) r.dir


let rec add_lineI cur =
  load cur >>= fun ((r,r_md) : (universal_inc_rep * universal_inc_md)) ->
  let r = {r with asc = (List.map (fun s ->Bytes.cat s "\nAdded Line!") r.asc)} in
  manifest cur (r,r_md) >>= fun mani ->
  let _ = if List.length mani.errors > 0 then print_manifest_errors mani else store mani in
  List.fold_left (fun acc d ->
    add_lineI d >>= fun _ ->
    acc) (return ()) r.dir
  

let rec remove_linesI cur = 
  load cur >>= fun ((r,r_md) : (universal_inc_rep * universal_inc_md)) ->
  let r = {r with asc = (List.map (fun s ->
    let pos = Str.search_forward (Str.regexp_string "\nAdded Line!") s 0 in
    Bytes.sub s 0 pos
  ) r.asc)}
  in
  manifest cur (r,r_md) >>= fun mani ->
  let _ = if List.length mani.errors > 0 then print_manifest_errors mani else store mani in
  List.fold_left (fun acc d ->
    remove_linesI d >>= fun _ ->
    acc) (return ()) r.dir

let incremental () = 
  universal_inc_new (get_ex_dir ()) >>= fun cur ->
  trawl_univI cur >>= fun () ->
  get_cost (add_lineI cur) >>= fun c ->
  let _ = List.iter (fun (name,num) -> Printf.printf "%s:%d\n" name num) c in
  let _ = Printf.printf "\n" in
  trawl_univI cur >>= fun () ->
  remove_linesI cur >>| fun () ->
  Printf.printf "\nForest:Removed lines again\n"

let _ =
  (*
  let (r,m) = urlTest_load "http://db.gamefaqs.com/console/ps2/file/final_fantasy_x_2_chocobo.txt" in
  Printf.printf "%s\n" r;*)
  let (_,c) = run (incremental ()) in
  List.iter (fun (name,num) -> Printf.printf "%s:%d\n" name num) c


(* Unincremental Manifest test   *)
(*
let _ = 
  let (r,m) = d_load (get_ex_dir ()) in
  let r = {r with foo_rep = Bytes.cat r.foo_rep "\nAdded Line!"} in
  let mani = d_manifest (r,m) in
  let _ = if List.length mani.errors > 0 then print_errors mani else store_at mani "/tmp/yay" in
  let pos = Str.search_forward (Str.regexp_string "\nAdded Line!") r.foo_rep 0 in
  let r = {r with foo_rep = Bytes.sub r.foo_rep 0 pos} in
  let mani = d_manifest (r,m) in
  if List.length mani.errors > 0 then print_errors mani else store_at mani "/tmp/yay"


let rec add_line r = {r with asc_rep = (List.map (fun s ->Bytes.cat s "\nAdded Line!") r.asc_rep); dir_rep = List.map add_line r.dir_rep} 

let rec remove_lines r = {r with asc_rep = (List.map (fun s ->
    let pos = Str.search_forward (Str.regexp_string "\nAdded Line!") s 0 in
    Bytes.sub s 0 pos
  ) r.asc_rep); dir_rep = List.map remove_lines r.dir_rep}

let rec trawl_univ r =
  let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.asc_rep in
  List.iter (fun d -> trawl_univ d) r.dir_rep

let rec trawl_univ_links r =
  let _ = List.iter (fun s -> Printf.printf "%s\n" s) r.sym_rep in
  List.iter (fun d -> trawl_univ_links d) r.dir_rep

let unIncremental () = 
  let s,s_md = universal_load (get_ex_dir ()) in
  let _ = trawl_univ s in
  let s = add_line s in
  let mani = universal_manifest (s,s_md) in

  let _ = if List.length mani.errors > 0 then print_errors mani else store mani in
  let s,s_md = universal_load (get_ex_dir ()) in
  let _ = trawl_univ s in
  let s = remove_lines s in
  let mani = universal_manifest (s,s_md) in
  let _ = if List.length mani.errors > 0 then print_errors mani else store mani in
  print_string "Links\n";
  trawl_univ_links s 

let _ = unIncremental ()

*)


(*
  Printf.printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n" s1 s2 s3 s4 s5 s6 s7
*)




(*
  d_new (get_ex_dir ()) >>= fun dcur ->
  load dcur >>= fun (prep,pmd) ->
  load (prep.foo) >>= fun (str,_) ->
  return (Printf.printf "%s\n" str)
*)

(*
<:skin<
  uniSkin = <universal>
  testSkin = <foo> -> >file<
  testSkin2 = <f>
  testSkin3 = <bin> -> <b> -> <option>
>>
<:forest<
  d = directory { foo is "ack" :: <file> }

  test_f = directory { noo is "boo" :: d }

  ofile = file option

  test_list = Map [ f :: file option | f <- in $["myfile"]$ ]

  textFiles = [ f :: file | f <- matches GL "*",g <- matches RE ".*",$get_kind f_att = AsciiK && get_kind g_att = AsciiK$]


  universal_inc = compose universal uniSkin

  links = directory { li is "foo" :: link }

test1 = compose d testSkin
test2 = compose textFiles testSkin2
test3 = compose universal testSkin3

  >>
(*
<:skin<
   skin1 = <file>
   skin2 = <comprehension>
   skin3 =  >file<; asc -> <file>
   skin4 = <universal>; <b> -> >file<
   skin5 = <sym> -> <l>
   skin6 = delay
   skin7 = skin4 ; skin5
>>

<:forest<
  universal_1 = compose universal skin1
  universal_2 = compose universal skin2
  universal_3 = compose universal skin3
  universal_4 = compose universal skin4
  universal_5 = compose universal skin5
  universal_6 = compose universal skin6
  universal_7 = compose universal skin7

let s1 = universal_1
let s2 = universal_2
let s3 = universal_3
let s4 = universal_4
let s5 = universal_5
let s6 = universal_6
let s7 = universal_7
>>
*)
*)
