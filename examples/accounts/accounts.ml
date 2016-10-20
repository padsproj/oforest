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

account = pads acc

accounts = map [ acc :: account | acc <- matches GL "*.acc" ]

client = directory {
  checking is "checking" :: accounts;
  savings is "savings" :: accounts
}

bank = directory {
  clients is map [ clis :: <client> | clis <- matches GL "*" ]
}

|}]

let inc_total_balance (client : client_rep) =
  let checking_balance =
    PathMap.fold (fun k v acc -> acc + v) client.checking 0 in
  let savings_balance =
    PathMap.fold (fun k v acc -> acc + v) client.savings 0 in
  checking_balance + savings_balance

let total_balance client =
  let checking_balance =
    PathMap.fold (fun k v acc -> acc + v) client.checking 0 in
  let savings_balance =
    PathMap.fold (fun k v acc -> acc + v) client.savings 0 in
  checking_balance + savings_balance

let balance path client =
  let (rep, md) = bank_load path in
  let client_rep = PathMap.find client rep.clients in
  total_balance client_rep

let inc_balance (path : string) (client : string) : int CurMonad.t =
  bank_new path >>= fun cur ->
  load cur >>= fun (b, b_md) ->
  let cl_cur = PathMap.find client b.clients_prep in
  load cl_cur >>| fun (cl, cl_md) ->
  inc_total_balance cl

let withdraw_from_accounts accounts amount =
  PathMap.fold (fun k v (m, to_withdraw) -> 
    let amount = v in
      if to_withdraw = 0 then (m, to_withdraw)
        else if to_withdraw > amount 
          then (PathMap.add k 0 m, to_withdraw - amount)
          else (PathMap.add k (amount - to_withdraw) m, 0)) accounts (accounts, amount)

let withdraw path client amount =
  let (rep, md) = bank_load path in
  let client_rep = PathMap.find client rep.clients_rep in
  let total = total_balance client_rep in
  if total < amount then ()
  else 
    let (new_checking, to_withdraw) = 
      withdraw_from_accounts client_rep.checking_rep amount in 
    let (new_savings, _) =
      withdraw_from_accounts client_rep.savings_rep to_withdraw in
    let new_clients = 
      PathMap.add client 
                  {checking_rep=new_checking; savings_rep=new_savings}
                  rep.clients_rep in
    let mani = bank_manifest ({clients_rep=new_clients},md) in
    if List.length mani.errors > 0 then print_errors mani else store mani

let inc_withdraw path client amount =
  bank_new path >>= fun cur ->
  load cur >>= fun (b, b_md) ->
  let cl_cur = PathMap.find client b.clients_prep in
  load cl_cur >>= fun (cl, cl_md) ->
  let total = inc_total_balance cl in
  if total < amount then return ()
  else 
    let (new_checking, to_withdraw) =
      withdraw_from_accounts cl.checking_prep amount in
    let (new_savings, _) =
      withdraw_from_accounts cl.savings_prep to_withdraw in
    manifest cl_cur ({checking_prep=new_checking; savings_prep=new_savings}, cl_md) >>| fun mani ->
  if List.length mani.errors > 0 then print_errors mani else store mani

let () =
  let dir = Printf.sprintf "%s/accounts/%s" (Sys.getcwd ()) "accountsTesting" in
  print_string "Richard's balance: "; print_int (balance dir (dir ^ "/richard")); print_endline "";
  withdraw dir (dir ^ "/richard") 10;
  print_endline "Withdrew 10";
  print_string "Richard's new balance: "; print_int (balance dir (dir ^ "/richard")); print_endline "";
  print_string "Richard's new balance (incrementally): "; print_int (inc_balance dir (dir ^ "/richard") ()); print_endline "";
  inc_withdraw dir (dir ^ "/richard") 10 ();
  print_endline "Incrementally withdrew 10";
  print_string "Richard's new balance: "; print_int (balance dir (dir ^ "/richard")); print_endline "";
