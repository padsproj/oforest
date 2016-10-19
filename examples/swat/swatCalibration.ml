open Forest
open Swat
open CursorMonad

let dir = Filename.concat (Sys.getcwd ())  "../../swatData/TxtInOut_currentnew"
let outDir = Filename.concat (Sys.getcwd ())  "calibResults"

let flow_file = Filename.concat dir "FC_flowgage.csv"
let rch_file = Filename.concat dir "output.rch"


let range_GW_DELAY = (0.5, 1000.)
let range_ALHPA_BF = (0.5, 1.5)
let range_GWQMN = (0., 500.)
let range_GW_REVAP = (0.0, 0.2)
let range_REVAPMN = (0., 500.)
let range_RCHRG_DP = (0., 1.)

let range_SFTMP = (-5., 5.)
let range_SMTMP = (-5., 5.)
let range_SMFMX = (-5., 5.0)
let range_SMFMN = (-5., 5.0)
let range_TIMP = (0., 4.)
let range_SURLAG = (0., 15.)

let range_ESCO = (0.1, 1.)
let range_EPCO = (0.1, 1.)

let best_GW_DELAY = 82.410
let best_ALPHA_BF = 0.152
let best_GWQMN = 29.154
let best_GW_REVAP = 0.192
let best_REVAPMN = 443.955
let best_RCHRG_DP = 0.107
let best_SFTMP = -0.424
let best_SMTMP = 3.286
let best_SMFMX = 1.843
let best_SMFMN = 3.611
let best_TIMP = 0.553
let best_SURLAG = 0.246
let best_ESCO = 0.583
let best_EPCO = 0.955

let rec update_nth l f n =
  match l with
  | [] -> failwith "nth failed"
  | (SwatValue (SwatFloat sf) as h)::t -> 
      if n = 0 then (SwatValue (SwatFloat {sf with floatValue = f}))::t
      else h::(update_nth t f (n-1))
  | (SwatValue (_) as h)::t ->
      if n = 0 then l
      else h::(update_nth t f (n-1))
  | (SwatLine sl as h)::t -> (* Shouldn't ever reach this case *)
      if n = 0 then l
      else h::(update_nth t f (n-1))

let extractSwatFloatValue v =
  match v with
  | SwatValue (SwatFloat {floatValue = f}) -> f
  | SwatValue _ -> failwith "Incorrect format"
  | SwatLine s -> print_endline s.str; failwith "whoops"

let get_GW_DELAY g = extractSwatFloatValue (List.nth g.gwParams 2)
let get_ALPHA_BF g = extractSwatFloatValue (List.nth g.gwParams 3)
let get_GWQMN g = extractSwatFloatValue (List.nth g.gwParams 4)
let get_GW_REVAP g = extractSwatFloatValue (List.nth g.gwParams 5)
let get_REVAPMN g = extractSwatFloatValue (List.nth g.gwParams 6)
let get_RCHRG_DP g = extractSwatFloatValue (List.nth g.gwParams 7)

let set_GW_DELAY f g = {g with gwParams = update_nth g.gwParams f 2}
let set_ALPHA_BF f g = {g with gwParams = update_nth g.gwParams f 3}
let set_GWQMN f g = {g with gwParams = update_nth g.gwParams f 4}
let set_GW_REVAP f g = {g with gwParams = update_nth g.gwParams f 5}
let set_REVAPMN f g = {g with gwParams = update_nth g.gwParams f 6}
let set_RCHRG_DP f g = {g with gwParams = update_nth g.gwParams f 7}

let get_SFTMP b = extractSwatFloatValue (List.nth b.bsnWaterParams 0)
let get_SMTMP b = extractSwatFloatValue (List.nth b.bsnWaterParams 1)
let get_SMFMX b = extractSwatFloatValue (List.nth b.bsnWaterParams 2)
let get_SMFMN b = extractSwatFloatValue (List.nth b.bsnWaterParams 3)
let get_TIMP b = extractSwatFloatValue (List.nth b.bsnWaterParams 4)
let get_SURLAG b = extractSwatFloatValue (List.nth b.bsnRunoffParams 2)

let set_SFTMP f b = {b with bsnWaterParams = update_nth b.bsnWaterParams f 0}
let set_SMTMP f b = {b with bsnWaterParams = update_nth b.bsnWaterParams f 1}
let set_SMFMX f b = {b with bsnWaterParams = update_nth b.bsnWaterParams f 2}
let set_SMFMN f b = {b with bsnWaterParams = update_nth b.bsnWaterParams f 3}
let set_TIMP f b = {b with bsnWaterParams = update_nth b.bsnWaterParams f 4}
let set_SURLAG f b = {b with bsnRunoffParams = update_nth b.bsnRunoffParams f 2}

let get_ESCO h = extractSwatFloatValue (List.nth h.hruParams 8)
let get_EPCO h = extractSwatFloatValue (List.nth h.hruParams 9)

let set_ESCO f h = {h with hruParams = update_nth h.hruParams f 8}
let set_EPCO f h = {h with hruParams = update_nth h.hruParams f 9}

(* Swapped to parsing floats
let parseExp cl =
  let s = String.trim cl in
  let coeff = (float_of_string (String.sub s 0 6)) in
  let exp = (float_of_string (String.sub s 7 3)) in
  coeff *. (10. ** exp)

*)
let rec getDailyFlow (r : reachLines_rep) : float list =
  match r with
  | _::_::basin3::t ->
      let flow = basin3.reachFlowOut in
      flow::(getDailyFlow t)
  | _ -> []

(* Note: reach data in m^3/s, flow data in cubic feet per second *)
let getNashSutcliffe (r : float list) (f : flowLines_rep) : float =
  let total_number = List.length r in
  let total_flow = List.fold_left (fun acc fl -> acc +. fl.flowValue) 0. f in
  let average = total_flow /. (float_of_int total_number) in
  let denom = 
    List.fold_left (fun acc fl -> 
      let diff = fl.flowValue -. average in
      acc +. diff *. diff) 0. f in
  let num =
    List.fold_left2 (fun acc fl r ->
      let diff = fl.flowValue -. r in
      acc +. diff *. diff) 0. f r in
  1. -. num /. denom

let runSWAT () =
  let start = Core.Time.now () in
  if (Sys.command "./rswat2012.exe" != 0) then failwith "Swat failed";
  let finish = Core.Time.now() in
  (Core.Time.abs_diff finish start)

(* Note: output.rch starts in 1972, flowgage in 1979. So need to advance seven years on reach to align them. *)
let test_NSE () =
  let (flow, flow_md) = flow_load flow_file in  
  let (reach, reach_md) = reach_load rch_file in
  let reach_daily = getDailyFlow reach.reachInfo in
  let first_year_flow = take 365 flow.flowInfo in
  let first_year_reach = take 365 (drop 2557 reach_daily) in
  (* Should result in a value > 0 if run on calibrated data *)
  Printf.printf "%f\n" (getNashSutcliffe first_year_reach first_year_flow)

let possible_SURLAG = [|0.246;2.246;4.246;6.246;8.246;10.246|]
let possible_GW_DELAY = [|82.4; 182.4; 282.4; 382.4; 482.4; 582.4|]
let possible_ALPHA_BF = [|0.2;0.4;0.5;0.6;0.8;1.0|]
let possible_ESCO = [|0.1;0.3;0.4;0.6;0.7;0.9|]

let set_best_bsn bsn = 
  (set_SFTMP best_SFTMP (set_SMTMP best_SMTMP (set_SMFMX best_SMFMX 
    (set_SMFMN best_SMFMN (set_TIMP best_TIMP (set_SURLAG best_SURLAG bsn))))))

let set_best_gw gw =
  (set_GW_DELAY best_GW_DELAY (set_ALPHA_BF best_ALPHA_BF (set_GWQMN best_GWQMN 
    (set_GW_REVAP best_GW_REVAP (set_REVAPMN best_REVAPMN (set_RCHRG_DP best_RCHRG_DP gw))))))

let set_best_hru hru =
  (set_EPCO best_EPCO (set_ESCO best_ESCO hru))

let update_hru val_ESCO hru = set_ESCO val_ESCO hru
let update_gw val_GW_DELAY val_ALPHA_BF gw =
  set_GW_DELAY val_GW_DELAY (set_ALPHA_BF val_ALPHA_BF gw)
let update_bsn val_SURLAG bsn = set_SURLAG val_SURLAG bsn

let start_time = Core.Time.now ()

let _ =
  if Array.length Sys.argv < 5 then failwith "Program needs at least four arguments"

let skinned = 
  try (int_of_string Sys.argv.(1) != 0)
  with Failure s ->
        if s = "int_of_string" 
        then failwith "Argument must be integer"
        else failwith (Printf.sprintf "Error: %s" s) 

let (surlag_index, gw_delay_index, alpha_bf_index) = 
  try (int_of_string Sys.argv.(2), int_of_string Sys.argv.(3), int_of_string Sys.argv.(4))
  with Failure s ->
        if s = "int_of_string" 
        then failwith "Argument must be integer"
        else failwith (Printf.sprintf "Error: %s" s) 

let (real_flow, real_flow_md) = flow_load flow_file

let get_calibration_nse () =
  let (rch, rch_md) = reach_load rch_file in
  let daily_calc_flow = getDailyFlow rch.reachInfo in
  let interval_flow = take 6208 real_flow.flowInfo in
  let interval_calc_flow = take 6208 (drop 2557 daily_calc_flow) in
  getNashSutcliffe interval_calc_flow interval_flow

let runCalibrationUnskinned surlag gw_delay alpha_bf =
  Sys.chdir dir;
  let total_SWAT_time = ref Core.Time.Span.zero in
  total_SWAT_time := Core.Time.Span.(+) (!total_SWAT_time) (runSWAT ());
  (swatIn_new dir >>= load >>|
    fun (swat, swat_md) ->
      let best_swat = ref swat in
      let best_nse = ref (get_calibration_nse ()) in
      let update_values i =
        let new_bsn = update_bsn surlag (!best_swat).bsn in
        let new_hrus = List.map (update_hru possible_ESCO.(i)) (!best_swat.hrus) in
        let new_gws = List.map (update_gw gw_delay alpha_bf) (!best_swat.gws) in
        store (bsn_manifest (new_bsn, swat_md.data.bsn_md));
        List.iter2 (fun rep md -> store (hru_manifest (rep, md))) new_hrus swat_md.data.hrus_md.data;
        List.iter2 (fun rep md -> store (gw_manifest (rep, md))) new_gws swat_md.data.gws_md.data;
        {!best_swat with bsn = new_bsn; hrus = new_hrus; gws = new_gws} in
      for i = 0 to Array.length possible_ESCO - 1 do
        Printf.printf "Iteration %d: best NSE %f\n" i (!best_nse);
        let new_swat = update_values i in
        total_SWAT_time := Core.Time.Span.(+) (runSWAT ()) (!total_SWAT_time);
        let new_nse = get_calibration_nse () in
        let nse_name = Filename.concat outDir (Printf.sprintf "nse.%s.%d.%d.%d.%d" "unskinned" surlag_index gw_delay_index alpha_bf_index i) in
        (*
        let rch_name = Filename.concat outDir (Printf.sprintf "output.rch.%s.%d.%d.%d.%d" "unskinned" surlag_index gw_delay_index alpha_bf_index i) in
        Sys.rename "output.rch" rch_name;
        *)
        let nse = open_out nse_name in
        Printf.fprintf nse "%f" new_nse; close_out nse;
        if !best_nse < new_nse
        then (best_swat := new_swat; best_nse := new_nse) else ()
      done;
      let end_time = Core.Time.now () in
      let time_name = Filename.concat outDir (Printf.sprintf "time.%s.%d.%d.%d" "unskinned" surlag_index gw_delay_index alpha_bf_index) in
      let total_time = Core.Time.abs_diff end_time start_time in
      let time = open_out time_name in
      let ts = Core.Time.Span.to_string in
      Printf.fprintf time "%s %s %s" (ts total_time) (ts (Core.Time.Span.(-) total_time !total_SWAT_time)) (ts !total_SWAT_time);
      close_out time;
      Printf.printf "Best NSE: %f\n" (!best_nse);) ()


let runCalibrationSkinned surlag gw_delay alpha_bf =
  Sys.chdir dir;
  let total_SWAT_time = ref Core.Time.Span.zero in
  total_SWAT_time := Core.Time.Span.(+) (!total_SWAT_time) (runSWAT ());
  (swatICB_new dir >>= load >>|
    fun (swat, swat_md) ->
      let best_swat = ref swat in
      let best_nse = ref (get_calibration_nse ()) in
      let update_values i =
        let new_bsn = update_bsn surlag (!best_swat).bsn in
        let new_hrus = List.map (update_hru possible_ESCO.(i)) (!best_swat.hrus) in
        let new_gws = List.map (update_gw gw_delay alpha_bf) (!best_swat.gws) in
        store (bsn_manifest (new_bsn, swat_md.data.bsn_md));
        List.iter2 (fun rep md -> store (hru_manifest (rep, md))) new_hrus swat_md.data.hrus_md.data;
        List.iter2 (fun rep md -> store (gw_manifest (rep, md))) new_gws swat_md.data.gws_md.data;
        {!best_swat with bsn = new_bsn; hrus = new_hrus; gws = new_gws} in
      for i = 0 to Array.length possible_ESCO - 1 do
        Printf.printf "Iteration %d: best NSE %f\n" i (!best_nse);
        let new_swat = update_values i in
        total_SWAT_time := Core.Time.Span.(+) (runSWAT ()) (!total_SWAT_time);
        let new_nse = get_calibration_nse () in
        let nse_name = Filename.concat outDir (Printf.sprintf "nse.%s.%d.%d.%d.%d" "skinned" surlag_index gw_delay_index alpha_bf_index i) in
        (*
        let rch_name = Filename.concat outDir (Printf.sprintf "output.rch.%s.%d.%d.%d.%d" "skinned" surlag_index gw_delay_index alpha_bf_index i) in
        Sys.rename "output.rch" rch_name; *)
        let nse = open_out nse_name in
        Printf.fprintf nse "%f" new_nse; close_out nse;
        if !best_nse < new_nse
        then (best_swat := new_swat; best_nse := new_nse) else ()
      done;
      let end_time = Core.Time.now () in
      let time_name = Filename.concat outDir (Printf.sprintf "time.%s.%d.%d.%d" "skinned" surlag_index gw_delay_index alpha_bf_index) in
      let total_time = Core.Time.abs_diff end_time start_time in
      let time = open_out time_name in
      let ts = Core.Time.Span.to_string in
      Printf.fprintf time "%s %s %s" (ts total_time) (ts (Core.Time.Span.(-) total_time !total_SWAT_time)) (ts !total_SWAT_time);
      close_out time;
      Printf.printf "Best NSE: %f\n" (!best_nse);) ()

let main =
  try
    let val_SURLAG = possible_SURLAG.(surlag_index) in
    let val_GW_DELAY = possible_GW_DELAY.(gw_delay_index) in
    let val_ALPHA_BF = possible_ALPHA_BF.(alpha_bf_index) in
    let nse = get_calibration_nse () in
    let _ = Printf.printf "%g\n" nse in
    if skinned then runCalibrationSkinned val_SURLAG val_GW_DELAY val_ALPHA_BF
    else runCalibrationUnskinned val_SURLAG val_GW_DELAY val_ALPHA_BF
  with 
    | Invalid_argument s -> 
       if s = "index out of bounds"
       then failwith "Failure: arguments need to be in range of searchable values"
       else failwith (Printf.sprintf "Error: %s" s) 
