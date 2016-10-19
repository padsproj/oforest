open Pads

(* Types! *)
type loc =
  { line : int;
    character : int;
  }

type pads_parse_state =
  { current : string;
    rest : string list;
    loc : loc;
  }

type ('a, 'b) pads_parser = pads_parse_state -> ('a * 'b * pads_parse_state)

(* Constants *)

let start_loc = { line = 1; character = 1 }

(* Functions! *)

let new_line loc = { line = loc.line + 1; character = 1 }

let inc_chars n loc = { loc with character = loc.character + n }

let inc_line state =
  match state.rest with
  | [] -> { current=""; rest=[]; loc= state.loc}
  | hd :: tl -> { current=hd; rest=tl; loc = new_line state.loc}
  
let rec update_state (state : pads_parse_state) (n : int)  =
  let new_s = Str.string_after state.current n in
  if new_s = ""
  then inc_line state
  else
    { state with current = new_s;
      loc = inc_chars n state.loc }

let new_state sl =
  let sl = List.map (fun s -> s ^ "\n") sl in
  { current = List.hd sl; rest = List.tl sl; loc = start_loc }


let gen_err_msg s state =
  Printf.sprintf "Line %d, character %d: %s" state.loc.line state.loc.character s

(*
let rem_newline s =
  try
    let i = String.index s '\n' in
    String.sub s 0 (i + 1)
  with | _ -> s
*)

(* TODO: Make sure this is a good end_of_file test *)
let check_eof (state : pads_parse_state) = state.current = "" && state.rest = []

let parse_string (s : pads_constant) (state : pads_parse_state) =
  match s with
  | PTI n ->
     let s = string_of_int n in
       if Pads.sub_starts_with state.current s
       then ((), empty_md (), update_state state (String.length s))
       else ((), error_md [gen_err_msg (Printf.sprintf "%s not found" (String.escaped s)) state] (), state)
  | PTS s ->
       if Pads.sub_starts_with state.current s
       then ((), empty_md (), update_state state (String.length s))
       else ((), error_md [gen_err_msg (Printf.sprintf "%s not found" (String.escaped s)) state] (), state)
  | PTRE reg -> (* Only used for list separators (hopefully) *)
     let s,d = 
       match reg with
       | RE s  -> s,""
       | REd (s, d) -> s,d
     in
     let compiled_regex = Str.regexp s in 
     if Str.string_match compiled_regex state.current 0
     then let matched = Str.matched_string state.current in
          ((), empty_md (), update_state state (String.length matched))
     else ((), error_md [gen_err_msg (Printf.sprintf "%s not matched" (String.escaped s)) state] (), state)
  | PTEOF -> 
     if check_eof state
     then ((), empty_md (), state)
     else ((), error_md [gen_err_msg (Printf.sprintf "EOF not found" ) state] (), state)

let parse_regex reg =
  let s,d = 
    match reg with
    | RE s  -> s,""
    | REd (s, d) -> s,d
  in
  let compiled_regex = Str.regexp s in 
  fun state ->
    if Str.string_match compiled_regex state.current 0
    then let matched = Str.matched_string state.current in
         (matched, empty_md (), update_state state (String.length matched))
    else (d, error_md [gen_err_msg (Printf.sprintf "%s not matched" (String.escaped s)) state] (), state)
     
let parse_pstring (s : pads_constant) (state : pads_parse_state) =
  match s with
  (* TODO: Not clear what the right thing is here, but for the sake of Richard's PADS,
   * I probably need this to check that n chars isn't farther ahead than \n 
   *)
  | PTI n ->
     if String.length state.current >= n
     then (String.sub state.current 0 n, empty_md (), update_state state n)
     else (state.current, 
           error_md [gen_err_msg (Printf.sprintf "Not enough chars: %d needed" n) state] (), 
           update_state state (String.length state.current))
  | PTS str ->
     begin (*
       Printf.printf "Term is: %s\n" (String.escaped str);
             Printf.printf "String is: %s\n" (String.escaped state.current); *)
       let regexp = Str.quote str |> Str.regexp in
       try 
	 let n = Str.search_forward regexp state.current 0 in
         let (r,m,state) =
	   (Str.string_before state.current n,
	    empty_md (),
	    update_state state n)
         in
         let _,_,state = parse_string s state in
         (r,m,state)
       with Not_found ->
	 ("", 
	  error_md [gen_err_msg (Printf.sprintf "Can't find terminator %s" (String.escaped str)) state] (), 
	  state)
     end
  | PTRE reg ->
     (* TODO: Should we consume terminator? *)
     let s,d = 
       match reg with
       | RE s  -> s,""
       | REd (s, d) -> s,d
     in
     let regexp = Str.regexp s in
     begin
       try
         let n = Str.search_forward regexp state.current 0 in
         (Str.string_before state.current n,
	  empty_md (),
	  update_state state n)
       with Not_found -> 
	 ("", 
	  error_md [gen_err_msg (Printf.sprintf "Can't find terminator matching regex %s" (String.escaped s)) state] (), 
	  state)
     end
  | PTEOF -> 
     if check_eof state
     then ("", empty_md (), state)
     else ("", error_md [gen_err_msg (Printf.sprintf "EOF not found" ) state] (), state)


let int_regex = Str.regexp "-?[0-9]+"
let parse_int (state : pads_parse_state) =
  if Str.string_match int_regex state.current 0
  then let matched = Str.matched_string state.current in 
       (int_of_string matched, empty_md (), update_state state (String.length matched))
         
  else (0, error_md [gen_err_msg "Int not found" state] (), state)

let float_regex = Str.regexp "\\(-?[0-9]+\\(\\.[0-9]*\\)?\\|-?\\.[0-9]+\\)\\(E[\\+-][0-9]+\\)*"
let parse_float (state : pads_parse_state) =
  if Str.string_match float_regex state.current 0
  then let matched = Str.matched_string state.current in
        (float_of_string matched, empty_md (), update_state state (String.length matched))
  else (0., error_md [gen_err_msg "Float not found" state] (), state)

let parse_list (p : ('a, 'b) pads_parser) (sep : pads_constant) (term : pads_constant)  state =
  let last_state = ref {current="";rest=[];loc = start_loc} in
  let rec pl term state = 
    if !last_state = state
    then ([], error_md [(gen_err_msg "Made no progress last list iteration" state)] [], state)
    else
      let _ = last_state := state in
      match term with
      | PTI n ->
         if n = 0 then ([], empty_md [], state)
         else if check_eof state
         then ([], error_md [gen_err_msg "Reached end of file while parsing list" state] [], state)
         else
           let (rep, md, new_state) = p state in
           let (_, sep_md, sep_state) = parse_string sep new_state in
           if sep_md.pads_num_errors > 0
           then ([],
                 error_md ((gen_err_msg "No separator found" state) :: sep_md.pads_error_msg) [],
                 state)
           else
             let (rest_rep, rest_md, final_state) = pl (PTI (n-1)) sep_state in
             let final_md = 
               { pads_num_errors = md.pads_num_errors + rest_md.pads_num_errors;
                 pads_error_msg = md.pads_error_msg @ rest_md.pads_error_msg;
                 pads_data = md :: rest_md.pads_data ;
                 pads_extra = md.pads_extra @ rest_md.pads_extra
               } in
             (rep::rest_rep, final_md, final_state)
      | PTRE _ 
      | PTS _ ->
         let (_, term_md, term_state) = parse_string term state in
         if term_md.pads_num_errors = 0
         then ([], empty_md [], term_state)
         else if check_eof state
         then ([], error_md [gen_err_msg "Reached end of file while parsing list" state] [], state)
         else 
           let (rep, md, new_state) = p state in
           let (_, sep_md, sep_state) = parse_string sep new_state in
           if sep_md.pads_num_errors > 0
           then  let (_, term_md, term_state) = parse_string term new_state in
                 if term_md.pads_num_errors > 0 (* Neither a separator nor a terminator found *)
                 then ([],
                       {sep_md with
                         pads_error_msg =
                           (gen_err_msg (Printf.sprintf "No separator or terminator found.") state)
                         :: sep_md.pads_error_msg @ term_md.pads_error_msg;
                         pads_data = []}
                         , state)
                 else (* Termination condition *)
                   ([rep], {md with pads_data = [md]}, new_state)
           else (* Found separator, continuing to next *)
             let (rest_rep, rest_md, final_state) = pl term sep_state in
             let combined_md =
               { pads_num_errors = md.pads_num_errors + rest_md.pads_num_errors;
                 pads_error_msg = md.pads_error_msg @ rest_md.pads_error_msg;
                 pads_data = md :: rest_md.pads_data ;
                 pads_extra = md.pads_extra @ rest_md.pads_extra
               } in
             (rep::rest_rep, combined_md, final_state)
      | PTEOF ->
         if check_eof state
         then ([], empty_md [], state)
         else
           let (rep, md, new_state) = p state in
           let (_, sep_md, sep_state) = parse_string sep new_state in
           if sep_md.pads_num_errors > 0
           then  if not (check_eof new_state) (* Neither a separator nor a terminator found *)
             then ([],
                   {sep_md with
                     pads_error_msg =
                       (gen_err_msg "No separator or terminator (EOF) found." state)
                     :: sep_md.pads_error_msg;
                     pads_data = []}
                     , state)
             else (* Termination condition *)
               ([rep], {md with pads_data = [md]}, new_state)
           else (* Found separator, continuing to next *)
             let (rest_rep, rest_md, final_state) = pl term sep_state in
             let combined_md =
               { pads_num_errors = md.pads_num_errors + rest_md.pads_num_errors;
                 pads_error_msg = md.pads_error_msg @ rest_md.pads_error_msg;
                 pads_data = md :: rest_md.pads_data ;
                 pads_extra = md.pads_extra @ rest_md.pads_extra
               } in
             (rep::rest_rep, combined_md, final_state)
  in
  pl term state
      

let list_to_buf f sep buf (r,m) =
  let sep =
    match sep with
    | PTEOF -> failwith "Using EOF as a list separator is non-sensical"
    | PTRE reg ->
       begin
         match reg with
         | RE _  -> ""
         | REd (_, d) -> d
       end
    | PTS s -> s
    | PTI n -> string_of_int n
  in
  let rec ltb (r,m) =
    match (r,m) with
    | [],[] -> ()
    | (rep::[]),(md::[]) -> f buf (rep,md)
    | (rep::rt),(md::mt) ->
       f buf (rep,md);
      Buffer.add_string buf sep;
      ltb (rt,mt)
    | _ -> failwith "Md list size and rep list size are not the same"
  in
  ltb (r,m)
     
(*
let parse_char (state : pads_parse_state) =
  if String.length state.current <= state.loc.character - 1
  then (' ', error_md [gen_err_msg "String was empty" state] (), state)
  else
    (String.get state.current (state.loc.character - 1), empty_md (), update_state state 1)

let parse_EOF (state : pads_parse_state) =
  if String.length state.current = state.loc.character - 1 && state.rest = []
  then ((), empty_md (), state)
  else ((), error_md [gen_err_msg "Not end of file" state] (), state)

let parse_EOR (state : pads_parse_state) =
  if String.length state.current = state.loc.character - 1 then ((), empty_md (), inc_line state)
  else ((), error_md [gen_err_msg "Not end of line" state] (), inc_line state)



*)
