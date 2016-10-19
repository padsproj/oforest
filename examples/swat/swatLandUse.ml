open Forest
open Swat
open CursorMonad
open Pads

[%%pads {|

ptype outOrganicNLine = {$ws$; "ORGANIC N ="; $ws$; outOrganicNVal : Pfloat; outUnits : stringLn}

pdatatype outStdLine = 
 | OutNitrogen of outOrganicNLine 
 | OutStd of stringLn

ptype outStd = outStdLine Plist("",EOF)

|}]

[%%forest {|
outputStd = pads outStd
|}]

let dir = Filename.concat (Sys.getcwd ())  "../../swatData/TxtInOut_currentnew"
let outDir = Filename.concat (Sys.getcwd ())  "LUResults"

let organic_fert_index = 44

let rec updateMgtSchedule (schedule : mgtScheduleLines_rep) (id : int) : mgtScheduleLines_rep * bool =
  let idString = Printf.sprintf "%4d" id in
  match schedule with
  | [] -> [], false
  | (MgtOther _ as m) :: t
  | (MgtBegin _ as m) :: t -> 
      let new_schedule, updated = updateMgtSchedule t id in
      m :: new_schedule, updated
  | (MgtFert f) :: t -> MgtFert { f with fertID = idString } :: t, true
  | MgtAuto a :: t -> MgtAuto {a with autoFertID = idString } :: t, true
  | MgtCont c :: t -> MgtCont {c with contFertID = idString } :: t, true

let runSWAT () =
  let start = Core.Time.now () in
  if (Sys.command "./rswat2012.exe" != 0) then failwith "Swat failed";
  let finish = Core.Time.now() in
  (Core.Time.abs_diff finish start)

let rec process_batch batch md n = 
  match batch with
  | [] -> []
  | (name, mgt)::t ->
    if n = 0 then batch
    else
      let new_schedule, changed = updateMgtSchedule mgt.mgtSchedule organic_fert_index in
      if changed then 
        let new_mgt = {mgt with mgtSchedule = new_schedule} in
        let mgt_md = PathMap.find name md in
        store (mgt_manifest (new_mgt, mgt_md));
        process_batch t md (n - 1)
      else process_batch t md n

let get_nitrogen () =
  let (rep, md) = outputStd_load "output.std" in
  let rec get_nitrogen l =
    match l with
    | [] -> failwith "Nitrogen line not found"
    | OutStd _::t -> get_nitrogen t
    | OutNitrogen o::t -> o.outOrganicNVal in
  get_nitrogen rep

let skinned_main n =
  Sys.chdir dir;
  let out_file = open_out (Filename.concat outDir "swatLandUseResults.txt") in
  let time_file = open_out (Filename.concat outDir (Printf.sprintf "swatLandUseTime.skinned.%d" n)) in
  let start_time = Core.Time.now () in
  let total_SWAT_time = ref Core.Time.Span.zero in
  (*
  let _ = test_cio () in
  let _ = test_fig () in
  let _ = test_sub () in
  let _ = test_pcp () in
  let _ = test_tmp () in
  let _ = test_crop () in
  let _ = test_till () in
  let _ = test_urban () in
  let _ = test_wus () in
  let _ = test_wgn () in
  let _ = test_chm () in
  let _ = test_pest () in
  let _ = test_fert () in
  let _ = test_sol () in
  let _ = test_flow () in
  let _ = test_reach () in
  let _ = test_swat () in
  let _ = exit 1 in
  *)
  let _ = run (
    swatILU_new dir >>= load >>|
        (fun (r, m) ->
          let mgt_bindings = ref (PathMap.bindings r.mgts) in
          let iteration = ref 0 in
          let mgt_md = m.data.mgts_md.data in
          while !mgt_bindings != [] do
            mgt_bindings := process_batch (!mgt_bindings) mgt_md 50;
            incr iteration;
            total_SWAT_time := Core.Time.Span.(+) (runSWAT ()) (!total_SWAT_time);
            output_string out_file (Printf.sprintf "%d %f" (!iteration * 50) (get_nitrogen ()));
            output_string out_file "\n";
            flush out_file
          done
        )
  )
  in
  let end_time = Core.Time.now () in
  let total_time = Core.Time.abs_diff end_time start_time in
  let ts = Core.Time.Span.to_string in
  Printf.fprintf time_file "%s %s %s" (ts total_time) (ts (Core.Time.Span.(-) total_time !total_SWAT_time)) (ts !total_SWAT_time);
  close_out out_file;
  close_out time_file

let unskinned_main n =
  Sys.chdir dir;
  let out_file = open_out (Filename.concat outDir "swatLandUseResults.txt") in
  let time_file = open_out (Filename.concat outDir (Printf.sprintf "swatLandUseTime.unskinned.%d" n)) in
  let start_time = Core.Time.now () in
  let total_SWAT_time = ref Core.Time.Span.zero in
  let _ = run
    (swatIn_new "." >>= load >>|
        fun (r, m) -> 
     let mgt_bindings = ref (PathMap.bindings r.mgts) in
     let iteration = ref 0 in
     let mgt_md = m.data.mgts_md.data in
     while !mgt_bindings != [] do
       mgt_bindings := process_batch (!mgt_bindings) mgt_md 50;
       incr iteration;
       total_SWAT_time := Core.Time.Span.(+) (runSWAT ()) (!total_SWAT_time);
       output_string out_file (Printf.sprintf "%d %f" (!iteration * 50) (get_nitrogen ()));
       output_string out_file "\n";
       flush out_file
     done
    )
  in
  let end_time = Core.Time.now () in
  let total_time = Core.Time.abs_diff end_time start_time in
  let ts = Core.Time.Span.to_string in
  Printf.fprintf time_file "%s %s %s" (ts total_time) (ts (Core.Time.Span.(-) total_time !total_SWAT_time)) (ts !total_SWAT_time);
  close_out out_file;
  close_out time_file


let main =
  let skinned = 
    try int_of_string Sys.argv.(1) <> 0
    with 
      Failure s -> 
        if s = "int_of_string" 
        then failwith "Argument must be integer (0 or 1)"
        else failwith (Printf.sprintf "Error: %s" s) 
    | Invalid_argument s -> 
       if s = "index out of bounds"
       then failwith "Missing command line argument (0 or 1)" 
       else failwith (Printf.sprintf "Error: %s" s) 
  in 
  let num = int_of_string Sys.argv.(2) in
  if skinned then skinned_main num
  else unskinned_main num
