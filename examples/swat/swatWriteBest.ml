open Forest
open Swat
open CursorMonad

let dir = "/home/dilorenzo/everything/research/swatData/111317356/"
let outDir = "/home/dilorenzo/everything/research/oforest/examples/swat/res/"

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

let set_best_bsn bsn = 
  (set_SFTMP best_SFTMP (set_SMTMP best_SMTMP (set_SMFMX best_SMFMX 
    (set_SMFMN best_SMFMN (set_TIMP best_TIMP (set_SURLAG best_SURLAG bsn))))))

let set_best_gw gw =
  (set_GW_DELAY best_GW_DELAY (set_ALPHA_BF best_ALPHA_BF (set_GWQMN best_GWQMN 
    (set_GW_REVAP best_GW_REVAP (set_REVAPMN best_REVAPMN (set_RCHRG_DP best_RCHRG_DP gw))))))

let set_best_hru hru =
  (set_EPCO best_EPCO (set_ESCO best_ESCO hru))

let main =
  Sys.chdir dir;
  (swatForCalib_new dir >>= load >>|
    fun (swat, swat_md) ->
      let new_bsn = set_best_bsn swat.bsn_prep in
      let new_hrus = List.map (set_best_hru) swat.hrus_prep in
      let new_gws = List.map (set_best_gw) swat.gws_prep in
      store (bsn_manifest (new_bsn, swat_md.data.bsn_pmd));
      List.iter2 (fun rep md -> store (hru_manifest (rep, md))) new_hrus swat_md.data.hrus_pmd.data;
      List.iter2 (fun rep md -> store (gw_manifest (rep, md))) new_gws swat_md.data.gws_pmd.data) ()

