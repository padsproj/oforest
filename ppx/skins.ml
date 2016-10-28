open Forest_types
open Utility

(* Helper functions *)
let rec rec_replace (oldX : varname) (newX: varname) (e: forest_node ast) : forest_node ast =
  let rr = rec_replace oldX newX in
  {e with node =
      match e.node with 
      | Thunked(e) -> Thunked(rr e)
      | Predicate(e,p) -> Predicate(rr e,p)
      | Option(e)      -> Option(rr e)
      | PathExp(pe,e)  -> PathExp(pe,rr e)
      | Comprehension(l,e,m) -> Comprehension(l,rr e,m)
      | Url(e) -> Url(rr e)
      | Directory (es) ->
	 let newlist = List.map (fun (labeli,expi) -> (labeli,rr expi)) es in
	 Directory(newlist)  
      | Var(x) when x = oldX -> Var(newX)
      | _ -> e.node}

let rec rec_check (name : varname) (e:forest_node ast) : bool =
  let (e,loc) = get_NaL e in
  match e with 
  | Thunked(e) -> rec_check name e
  | Predicate(e,_)
  | Option(e)
  | PathExp(_,e)
  | Comprehension(_,e,_) -> rec_check name e
  | Directory (dlist) ->
    List.fold_right (fun (labeli,expi) b ->
      b || (rec_check name expi)) dlist false  
  | Var(x) when x = name -> true 
  | SkinApp(e,_) -> rec_check name e
  | _ -> false



let fget_name : forest_node -> string =
  (*
  let _ = Forest_parser_helper.forest_parse_string in
  *)                                    
(*Forest_parser_helper.show_forest_node *)
function
  | Url(e)               -> "URL"
  | Pads(x)              -> "Pads " ^ x
  | PathExp(_,_)         -> "path"
  | Var(x)               -> "Var " ^ x
  | Thunked(_)           -> "delay"
  | Option(_)            -> "opt"
  | File                 -> "file"
  | Link                 -> "link"
  | Directory (_)        -> "dir"
  | Predicate(_,_)       -> "pred"
  | Comprehension(_,_,_) -> "comp"
  | SkinApp _            -> "skinApp"

let tget_name : t_node -> string = function
  | TTop       -> "top"
  | TBot       -> "bottom"
  | TOr _      -> "or"
  | TTypeOf(_) -> "typeof"
  | TDirFun(x,_) -> x
  | TAnd _     -> "and"
  | TComp _    -> "comprehension"
  | TOpt _     -> "option"
  | TDir _     -> "dir"
  | TFile      -> "file"
  | TLink      -> "link"
  | TPred      -> "predicate"
  | TRec       -> "rec"
  | TPads(x)   -> x


let hget_name : skin_node -> string = function
  | HDelay            -> "delay"
  | HUndelay          -> "undelay"
  | HNegate           -> "negate"
  | HId               -> "id" 
  | HComp _            -> "comprehension"
  | HOpt _             -> "option"
  | HDir _             -> "dir"

  | HType _            -> "type"
  | HVar(x)            -> "var " ^ x
  | HSeq _             -> "seq"
  | HAlt _             -> "alt"
  | HMap _             -> "map"
  | HDirFun(x,_)       -> "dirEnt " ^ x


let get_err s t e =  Printf.sprintf s (tget_name t) (fget_name e)
let get_std_err =  get_err "Expected type %s, but was of type %s"

let rec comp_types f1 f2 =
  let e1,loc1 = get_NaL f1 in
  let e2,loc2 = get_NaL f2 in
  match e1,e2 with
  | File, File
  | Link, Link -> true,""
  | Pads(x1),Pads(x2) when x1 = x2 -> true,""
  | Var(x1),Var(x2) when x1 = x2 -> true,""
  | PathExp(_,f1),_
  | Thunked(f1),_   -> comp_types f1 f2
  | _,PathExp(_,f2)
  | _,Thunked(f2)   -> comp_types f1 f2  
  | Comprehension(_,f1,_),Comprehension(_,f2,_)
  | Predicate(f1,_),Predicate(f2,_)
  | Option f1, Option f2
  | SkinApp(f1,_),SkinApp(f2,_)
  | Url(f1),Url(f2) -> comp_types f1 f2
  | Directory dlist1, Directory dlist2 ->
     let rec dirChecker = function
       | ((l1,f1) :: tl1),((l2,f2) :: tl2) ->
	  if l1 = l2
	  then
	    let b,m = comp_types f1 f2 in
	    if b
	    then dirChecker (tl1,tl2)
	    else false,m
	  else false,Printf.sprintf "Tried to compare fields %s and %s" l1 l2
       | [],[] -> true,""
       | _ -> false, "Directories have different number of fields"
      in
     dirChecker (dlist1,dlist2)     
  | _ -> false,(Printf.sprintf "Expected construct %s, but was construct %s" (fget_name e1) (fget_name e2))

(* Main functions *)

let rec evalTypeGen (t : t_node ast) (f : forest_node ast) =
  let (e,loc) = get_NaL f in
  match e with
  | Thunked(e) -> evalTypeGen t e
  | e ->
    match t.node,e with
    | TBot,_ -> false,"Either expected a function in a function application or got a function without a function application"
    | TTop,_
    | TFile,File
    | TLink,Link
    | TPred,Predicate(_) -> true,""
    | TPads(x),Pads(y) when x = y -> true,""
    | TRec,Var(_) -> f.payload = PRec,"Expected recursive type, but type was not recursive"
    | _,Var(x) -> 
       if Hashtbl.mem forestTbl x
      then let e = Hashtbl.find forestTbl x in
	   let r = rec_check x e in
	   if not r
	   then comp_types e f
	   else raise_loc_err loc "TODO: Recursive internal skins are unimplemented"
      else raise_loc_err loc (Printf.sprintf "%s is not a named forest description" x)
    | TOr(t1,t2),e ->
      let (b1,m1) = evalTypeGen t1 f in
      let (b2,m2) = evalTypeGen t2 f in
      if b1 || b2
      then true,""
      else false,(Printf.sprintf "Expected %s to match %s or %s" (fget_name e) (tget_name t1.node) (tget_name t2.node))
    | TAnd(t1,t2),_ ->
      let (b1,m1) = evalTypeGen t1 f in
      let (b2,m2) = evalTypeGen t2 f in
      if b1 && b2
      then true,""
      else if not b1
      then false,m1
      else false,m2
    | _,Predicate(e,_) -> evalTypeGen t e
    | TOpt(t),Option(e)
    | TComp(t),Comprehension(_,e,_) -> evalTypeGen t e
    | TDir(tlist),Directory(dlist)  ->
      let rec dirChecker = function
	| (t :: ttl), ((_,e) :: etl) ->
	   let b,m = evalTypeGen t e in
	   if b
	   then dirChecker (ttl,etl)
	   else false,m
	| (t :: ttl), [] -> false,"More directory entries in skin than description"  
	| [], _ -> true,""
      in
      dirChecker (tlist,dlist)
    | TTypeOf(x),_ -> 
      if Hashtbl.mem forestTbl x
      then let e = Hashtbl.find forestTbl x in
           comp_types e f
      else raise_loc_err loc (Printf.sprintf "%s is not a named forest description" x)
    | TDirFun(x,t),Directory(dlist) ->
      if List.exists (fun (s,_) -> s = x) dlist
      then let (_,f) = List.find (fun (s,_) -> s = x) dlist in
	   evalTypeGen t f	     
      else false,"Directory didn't contain " ^ x
  | t,e -> false,get_std_err t e


let evalType t e = fst (evalTypeGen t e)

let evalTypeF loc t e = 
  let (b,msg) = (evalTypeGen t e) in
  if b then b else raise_loc_err loc (Printf.sprintf "%s" msg)


let rec typeofH (h : skin_node ast) : (t_node ast)=
  let loc = h.loc in
  mk_ast loc @@
  match h.node with
  | HDelay         -> TTop
  | HUndelay       -> TTop
  | HNegate        -> TTop
  | HId            -> TTop
  | HMap(_)        -> TTop

  | HDir(hlist)    -> TDir(List.map (typeofH) hlist)
  | HOpt(h)        -> TOpt(typeofH h)
  | HComp(h)       -> TComp(typeofH h)
  | HType(h,t)     -> TAnd(t,typeofH h)
  | HSeq(h1,h2)    -> TAnd(typeofH h1,typeofH h2)
  | HAlt(h1,h2)    -> TOr(typeofH h1,typeofH h2)
  | HDirFun(x,h)   -> TDirFun(x,typeofH h)

  | HVar(x)       -> 
    if Hashtbl.mem Utility.skinTbl x
    then let h = Hashtbl.find skinTbl x in
         (typeofH h).node
    else
      raise_loc_err loc (x ^ " is not a named skin")
  
let rec evalSkin loc (h : skin_node ast) (f : forest_node ast) : forest_node ast =
  let (e,floc) = get_NaL f in
  let evalSkin = evalSkin loc in
  let es = evalSkin in
  match h.node with
  | HVar(x)       -> 
    if Hashtbl.mem Utility.skinTbl x
    then let h = Hashtbl.find skinTbl x in
         es h f
    else
      raise_loc_err loc (x ^ " is not a named skin")
  | HType(h,t)      -> if evalType t f then es h f else raise_loc_err loc "Sanity check failure for h|t"
  | HSeq(h1,h2)     -> es h2 (es h1 f)
  | HAlt(h1,h2)     -> if (evalType (typeofH h1) f) then es h1 f else es h2 f
  | HDirFun(x,h) ->
     let rec rem_tp = function
       | Thunked(e) -> rem_tp e.node
       | Predicate(e,_) -> rem_tp e.node
       | e -> e
     in
     begin
       match (rem_tp e) with
       | Directory(dlist) ->
          let hlist = List.map (fun (lbl,exp) -> if lbl = x then h else mk_ast (get_loc exp) HId) dlist in
	  let h = mk_ast loc @@ HDir hlist in
            es h f
       | _ -> raise_loc_err loc "Function application is only implemented for directories and map. Didn't find a directory."
     end

  | hn                    ->
    let e,del =
      match e with
      | Thunked(e) -> e,true
      | e -> f,false
    in    
    let e,del =
      match (hn,e.node) with   
      | HOpt(h),Option(e)   -> Option(es h e),del
      | HComp(h),Comprehension(m,e,l) -> Comprehension(m,es h e,l),del
      | HDir(hlist),Directory(dlist) ->
	 let rec dirChanger = function
	   | (h :: htl), ((lbl,exp) :: etl) ->
	      (lbl,(es h exp)) :: (dirChanger (htl,etl))
	   | (h :: ttl), [] ->  raise_loc_err loc "More directory entries in skin than description"  
	   | [], _ -> []
	 in
	 let nlist = dirChanger (hlist,dlist) in
	 (Directory nlist),del
      | HDelay,e       -> e,true
      | HUndelay,e     -> e,false
      | HNegate,e      -> e,(not del)
      | HId,e          -> e,del
      | HMap(h),Directory(dlist)   ->
	 let rec dirChanger = function
	   | ((lbl,exp) :: tl) ->
	      (lbl,(es h exp)) :: (dirChanger tl)
	   | [] -> []
	 in
	 let nlist = dirChanger dlist in
	 (Directory nlist),del
      | HMap(happ),ef   ->
	 let h1 = mk_ast loc @@ HOpt happ in
	 let h2 = mk_ast loc @@ HComp happ in
	 let h3 = mk_ast loc HId in
	 let h = List.fold_right (fun h acc -> mk_ast loc @@ HAlt(h,acc) ) [h1;h2] h3 in
	 let newf = es h f in
	 begin 
	   match newf.node with
	   | Thunked(e) -> e.node,true
	   | e -> e,del
	 end
      | hn,Predicate(e,p)     -> Predicate(es h e,p),del
      | hn,PathExp(p,e)       -> PathExp(p,es h e),del
      | hn,SkinApp(e,x)       -> SkinApp(es h e,x),del
      | hn,Url(e)             -> Url(es h e),del
      | h,e                  -> raise_loc_err loc 
        ("Failure in evalSkin. Type checking is not sound w.r.t. function application, so that's probably the issue." ^ 
            (debug_out ("h=" ^ (hget_name h) ^ " e=" ^ (fget_name e))))
    in
    let f = mk_ast loc e in
    if del
    then mk_ast loc @@ Thunked(f)
    else f

let rec doSkinning ((name,e) : (varname * forest_node ast)) : forest_node ast =
  (* First, walk through until you find a skin application *)
  let rec walkThrough (b : bool) (e : forest_node ast) =
    let loc = get_loc e in
    let wt = walkThrough false in
    {e with node = 
	match e.node with 
	| Thunked(e) -> Thunked(wt e)
	| Predicate(e,p) -> Predicate(wt e,p)
	| Option(e) -> Option(wt e)
	| PathExp(p,e)  -> PathExp(p,wt e)
	| Comprehension(t,e,plist) -> Comprehension(t,wt e,plist) 
	| Url(e) -> Url(wt e)
	| Directory (dlist) ->
	   let newlist = List.map (fun (labeli,expi) -> (labeli,wt expi)) dlist in
	   Directory (newlist)
	| SkinApp(spec,skin) ->
	   let spec = walkThrough b spec in
	   let spec,b =
             match spec.node with
             | Var(x) when x = name -> {spec with payload=PRec},true
             | Var(x) -> 
		if b
		then if Hashtbl.mem forestTbl x
		  then let e = Hashtbl.find forestTbl x in
                       let newE = rec_replace x name e in
		       newE,b
		  else raise_loc_err loc (Printf.sprintf "%s is not a forest description" x)
		else {spec with payload = PNone},false
             | _ -> spec,b
	   in 
	   let _ = evalTypeF loc (typeofH skin) spec in
	   (evalSkin loc skin spec).node
	| _ -> e.node
    }
  in 
  walkThrough true e
