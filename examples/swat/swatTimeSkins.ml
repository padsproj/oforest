open Forest
open Pads
open Swat
open CursorMonad

let dir = Filename.concat (Sys.getcwd ())  "../../swatData/TxtInOut_currentnew"

let get_t d f = 
  fst @@ 
  run 
    (f d >>= load
     >>| fun (r,m) ->
     m.load_time)
  
let get_time f = get_t dir f

let main =
  let fullDIn = get_time swatCheckPred_new in
  let fullDOut = get_time swatOutDelay_new in
  let useIn = get_time swatForLandUse_new in
  let useOut = get_time swatOutLandUse_new in
  let fullUnDIn = get_time swat_new in
  let fullUnDOut = get_time swatOut_new in
  let ts = Core.Time.Span.to_string in
  let t = Core.Time.now () in
  let t1 = Core.Time.add t fullDIn in
  let t1 = Core.Time.add t1 fullDOut in
  let t1 = Core.Time.abs_diff t t1 in
  let t2 = Core.Time.add t useIn in
  let t2 = Core.Time.add t2 useOut in
  let t2 = Core.Time.abs_diff t t2 in
  let t3 = Core.Time.add t useIn in
  let t3 = Core.Time.add t3 fullUnDOut in
  let t3 = Core.Time.abs_diff t t3 in
  let t4 = Core.Time.add t fullUnDIn in
  let t4 = Core.Time.add t4 useOut in
  let t4 = Core.Time.abs_diff t t4 in
  let t5 = Core.Time.add t fullUnDIn in
  let t5 = Core.Time.add t5 fullUnDOut in
  let t5 = Core.Time.abs_diff t t5 in
  Printf.printf "%s %s %s %s %s" (ts t1) (ts t2) (ts t3) (ts t4) (ts t5);
