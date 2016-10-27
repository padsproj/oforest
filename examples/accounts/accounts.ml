(* 
Use Makefile in examples directory

Desugar:
   ./desugar.sh accounts/accounts.ml

Compile:
   make accounts
*)

open Pads
open Forest

let is_int s =
  try (let _ = (int_of_string (String.trim s)) in true) with
  | Failure _ -> false
  | _ -> false

module CostMon = CostUnitMon
     
[%%pads {|

  ptype acc = Pint

|}]

[%%forest {|

accounts = map [ acc :: pads acc | acc <- matches GL "*.acc" ]

client = directory {
  checking is "checking" :: accounts;
  savings is "savings" :: accounts
}

bank = map [ clis :: <client> | clis <- matches GL "*" ]

|}]

let exDir = Filename.concat (Sys.getcwd ()) "accounts/accountsTesting"

(* Bank functions *)

let get_client_from_bank bank client =
  let%bind (b,b_md) = load bank in
  let cliPath = Filename.concat exDir client in
  if PathMap.mem cliPath b
  then return @@ PathMap.find cliPath b
  else
    failwith (Printf.sprintf "Error: Client %s does not exist\n" client)

let name_from_md md =
  match md.info with
    | Some (info) -> String.capitalize_ascii @@ Filename.basename info.full_path
    | None -> "N/A"
  
    
let balance client =
  let checking_bal = PathMap.fold (fun k v acc -> v + acc) client.checking 0 in
  let savings_bal = PathMap.fold (fun k v acc -> v + acc) client.savings 0 in
  return (checking_bal + savings_bal)


let withdraw_from_accounts accounts amount =
  PathMap.fold (fun k amount (map, to_withdraw) -> 
    if to_withdraw = 0 then (map, 0)
    else if to_withdraw > amount 
    then (PathMap.add k 0 map, to_withdraw - amount)
    else (PathMap.add k (amount - to_withdraw) map, 0)
  ) accounts (accounts, amount)

let withdraw client amount =
  let%bind (crep,cmd) = load client in
  let%bind bal = balance crep in
  let cliCap = name_from_md cmd in
  if bal < amount
  then
    let _ =
      Printf.printf "%s does not have enough money in their account.\n" cliCap in
    let _ =
      Printf.printf "%s has %d and tried to withdraw %d.\n" cliCap bal amount in
    return ()
  else
    let (checking, to_withdraw) =
      withdraw_from_accounts crep.checking amount in
    let (savings, _) =
      withdraw_from_accounts crep.savings to_withdraw in
    let new_crep = {checking;savings} in
    manifest client (new_crep, cmd) >>= fun mani ->
    if List.length mani.errors > 0
    then
      let _ = Forest.print_mani_errors mani in
      return ()
    else
      return @@ store mani

let transfer from toCli amount =
  withdraw from amount >>= fun () ->
  withdraw toCli (-amount)

      
(* Main *)
  
let _ =
  if Array.length Sys.argv < 4
  then begin 
    Printf.printf "Usage: %s <from_client> <to_client> <to_transfer>\n" Sys.argv.(0);
    Printf.printf "Valid clients are 'richard' and 'jonathan'\n";
    Printf.printf "To_transfer is an integer\n";
    exit 1;
  end
  else
    let fromName = Sys.argv.(1) in
    let toName = Sys.argv.(2) in
    let transAmt = int_of_string (Sys.argv.(3)) in
    let fromCap = String.capitalize_ascii fromName in
    let toCap = String.capitalize_ascii toName in
    run (bank_new exDir >>= fun bank ->
         get_client_from_bank bank fromName >>= fun fromCur ->
         load fromCur >>= fun (from,_) ->
         balance from >>= fun bal ->
         let _ = Printf.printf "%s's balance: %i\n"  fromCap bal
         in
         get_client_from_bank bank toName >>= fun toCur ->
         load toCur >>= fun (toCli,_) ->
         balance toCli >>= fun bal ->
         let _ = Printf.printf "%s's balance: %i\n"  toCap bal
         in
         transfer fromCur toCur transAmt >>= fun () ->
         let _ = print_endline "" in
         load fromCur >>= fun (from,_) ->
         balance from >>= fun bal ->
         let _ = Printf.printf "%s's new balance: %i\n"  fromCap bal
         in
         load toCur >>= fun (toCli,_) ->
         balance toCli >>= fun bal ->
         let _ = Printf.printf "%s's new balance: %i\n"  toCap bal
         in
         return () 
    )
      
