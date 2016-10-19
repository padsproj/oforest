open Pads
open PadsParser
open Forest

let alphaNumRE = REd ("[a-zA-Z0-9]*", "a")
let alphaRE = REd ("[a-zA-Z]*", "a")
let uc = REd ("[A-Z]+", "A")
let label = REd ("[A-Z_0-9]+", "A")
let filenameRE = REd ("[a-zA-Z.0-9]+", "a")
let fertnameRE = REd ("[a-zA-Z.0-9\\-]*", "a")
let ws = REd ("[ \t]*", "   ")
let nonws = REd ("[^ \t]*", "\"1\"")
let nonNewline = RE (".*");;
let nL = REd ("\r?\n","\n")

[%%pads {|

  ptype wsDouble =  $ws$ * Pfloat

  ptype stringLn =  {str : Pstring($nL$) ; $nL$ }

  ptype swatInt = {
    $ws$
    ; intValue : Pint
    ; $ws$; "|"; $ws$; intLabel : $label$; $ws$; ":"; $ws$
    ; intDescription : stringLn
}

  ptype swatFloat = {
      $ws$
    ; floatValue : Pfloat
    ; $ws$; "|"; $ws$; floatLabel : $label$; $ws$; ":"; $ws$
    ; floatDescription : stringLn
}

  ptype swatString = {
    stringValue : Pstring(13)
    ; $ws$; "|"; $ws$; stringLabel : $label$; $ws$; ":"; $ws$
    ; stringDescription : stringLn
}

  pdatatype swatLine = 
    | SwatInt of swatInt 
    | SwatFloat of swatFloat 
    | SwatString of swatString

  pdatatype valueOrLine = 
    | SwatValue of swatLine 
    | SwatLine of stringLn

  ptype variableLines = stringLn * $ws$ * Pint Plist($ws$,$nL$)

  ptype swatLines = valueOrLine Plist("",EOF)
  ptype cio = {
    cioPreamble : stringLn Plist("",6);
    figFile : stringLn;
    cioTimeInfo : valueOrLine Plist("",4);
    stringLn;
    cioClimateInfo : valueOrLine Plist("",21);
    stringLn;
    pcpFiles : stringLn Plist("",3);
    stringLn;
    tmpFiles : stringLn Plist("",3);
    weatherFiles : valueOrLine Plist("",4);
    stringLn;
    basinFile : valueOrLine;
    stringLn;
    databaseFiles : valueOrLine Plist("",5); 
    stringLn;
    specialInfo : valueOrLine Plist("",3);
    stringLn;
    outputInfo : valueOrLine Plist("",5);
    outputVariables : variableLines Plist("",4);
    stringLn;
    atmoFile : stringLn;
    outputFlags : valueOrLine Plist("",12)
}

  ptype figSub = { "subbasin"; subInfo : stringLn; Pstring(10); subFile : $filenameRE$; $nL$ }
  ptype figRoute = { "route"; routeInfo : stringLn; Pstring(10); rteFile : Pstring(13); swqFile : $filenameRE$; $nL$ }
  ptype figAdd = { "add"; addInfo : stringLn }
  ptype figSaveConc = { "saveconc"; saveConcInfo : stringLn; Pstring(10); concFile : $filenameRE$; $nL$ }
  ptype figReccnst = { "reccnst"; recCnstInfo : stringLn }
  ptype figPrint = { "Pnt.Source"; printInfo : stringLn }

  pdatatype figLine = 
    | Sub of figSub 
    | Route of figRoute 
    | Add of figAdd 
    | RecCnst of figReccnst 
    | Print of figPrint 
    | Save of figSaveConc

  ptype figFinish = { "finish"; finishInfo : stringLn }
  (* TODO: Terminator needs to be figFinish...*)
  ptype figLines = figLine Plist("","finish")

  ptype hruLine = {
    hruFile : Pstring(13);
    mgtFile : Pstring(13);
    solFile : Pstring(13);
    chmFile : Pstring(13);
    gwFile : Pstring(13);
    opsFile : Pstring(13);
    sepFile : Pstring(13);
    $nL$
}

  ptype lineInfo = {
    stringLn;
    nums : wsDouble Plist("",$nL$)
}

  ptype hruLines = hruLine Plist("",EOF)
  ptype subFile = {
    stringLn; 
    subArea : valueOrLine;
    stringLn Plist("",2);
    climate : valueOrLine Plist("",9);
    stringLn;
    elevationBands : lineInfo Plist("",3);
    subPrecipInfo : valueOrLine Plist("",3);
    stringLn;
    tributaryChannels : valueOrLine Plist("",5);
    stringLn;
    impoundmentFile : valueOrLine;
    stringLn;
    waterUseFile : valueOrLine;
    stringLn;
    carbonDioxide : valueOrLine;
    monthlyInfo : lineInfo Plist("",8);
    stringLn;
    numHrus : valueOrLine;
    stringLn Plist("",4);
    stringLn Plist("",4); (* May need to change this *)
    hru : hruLines
}

  ptype notUsed = "Not Used"
  
  pdatatype headerVal = 
    | NotUsed of notUsed 
    | HeadVal of Pfloat

  ptype headerLine = $alphaNumRE$ * $ws$ * headerVal * $nL$

  ptype pcpLine = {
    pcpYear : Pstring(4);
    pcpDate : Pstring(3);
    pcpPrecip : Pstring(5); $nL$
}
  ptype pcpLines = pcpLine Plist("",EOF)
  ptype pcpFile = { stringLn; headerLine Plist("",3); pcpLines : pcpLines }

  ptype tmpLine = {
    tmpYear : Pstring(4);
    tmpDate : Pstring(3);
    tmpMaxTemp : Pstring(5);
    tmpMinTemp : Pstring(5); $nL$
}
  ptype tmpLines = tmpLine Plist("",EOF)
  ptype tmpFile = { stringLn; headerLine Plist("",3); tmpLines : tmpLines }

  ptype mgtBeginCommand = {
    " "; beginMonth : Pstring(2);
    " "; beginDay : Pstring(2);
    " "; beginHUSC : Pstring(8);
    " "; " 1"; " "; beginID : Pstring(4);
    beginInfo : stringLn
}
  ptype mgtFertCommand = {
    " "; fertMonth : Pstring(2);
    " "; fertDay : Pstring(2);
    " "; fertHUSC : Pstring(8);
    " "; " 3"; " "; fertID : Pstring(4);
    fertInfo : stringLn
}
  ptype mgtAutoFertCommand = {
    " "; autoFertMonth : Pstring(2);
    " "; autoFertDay : Pstring(2);
    " "; autoFertHUSC : Pstring(8);
    " "; "11"; " "; autoFertID : Pstring(4);
    autoFertInfo : stringLn
}
  ptype mgtContFertCommand = {
    " "; contFertMonth : Pstring(2);
    " "; contFertDay : Pstring(2);
    " "; contFertHUSC : Pstring(8);
    " "; "14"; " "; contFertDays : Pstring(4);
    " "; contFertID : Pstring(4);
    contFertInfo : stringLn
}

  pdatatype mgtScheduleLine = 
    | MgtBegin of mgtBeginCommand
    | MgtFert of mgtFertCommand
    | MgtAuto of mgtAutoFertCommand
    | MgtCont of mgtContFertCommand
    | MgtOther of stringLn
  ptype mgtScheduleLines = mgtScheduleLine Plist("",EOF)

  ptype mgtFile = {
    mgtHeader : stringLn Plist("",3);
    mgtPlantParams : valueOrLine Plist("",5);
    stringLn;
    mgtGeneralParams : valueOrLine Plist("",5);
    stringLn;
    mgtUrbanParams : valueOrLine Plist("",2);
    stringLn;
    mgtIrrigationParams : valueOrLine Plist("",5);
    stringLn;
    mgtDrainParams : valueOrLine Plist("",3);
    stringLn;
    mgtYears : valueOrLine;
    stringLn;
    mgtSchedule : mgtScheduleLines
}

  ptype gwFile = {
    gwHeader : stringLn;
    gwParams : valueOrLine Plist("",15)
}

(* Some inconsistency between docs and actual data here *)
  ptype bsnFile = {
    bsnHeader : stringLn Plist("",3);
    bsnWaterParams : valueOrLine Plist("",13);
    stringLn;
    bsnRunoffParams : valueOrLine Plist("",7);
    stringLn;
    bsnNutrientParams : valueOrLine Plist("",9);
    stringLn;
    bsnPesticidePercolation : valueOrLine;
    stringLn;
    bsnWaterQuality : valueOrLine;
    stringLn;
    bsnBacteriaParams : valueOrLine Plist("",17);
    stringLn;
    bsnReachParams : valueOrLine Plist("",50); (* Two missing params? *)
    stringLn;
    bsnNoRunoff : stringLn;
    stringLn;
    bsnErosionParams : valueOrLine Plist("",17) (* Missing last parameter? *)
}

  ptype hruFile = {
    hruHeader : stringLn;
    hruParams : valueOrLine Plist("",16);
    stringLn;
    hruSpecialParams : valueOrLine Plist("",6);
    stringLn Plist("",3);
    hruConcentrationParams : valueOrLine Plist("",10)
}

  ptype plantLine1 = { lineData1 : wsDouble Plist("",10); $nL$ }
  ptype plantLine2 = { lineData2 : wsDouble Plist("",5); $nL$ }
  ptype plantEntry = { 
    $ws$; plantId : Pint; $ws$; plantLabel : $uc$; $ws$; Pint; $nL$;
    lineData : plantLine1 Plist("",3);
    line2Data : plantLine2
}
  ptype plantFile = plantEntry Plist("",EOF)

  ptype tillLine = { 
    $ws$; Pint; $ws$;
    tillName : Pstring(8); $ws$;
    tillEff : Pfloat; $ws$;
    tillDepth : Pfloat ; $ws$ ; Pfloat
    (* Last float isn't in manual, no idea what it is *)
}
  ptype tillFile = tillLine Plist($nL$,EOF)

  ptype urbanLine = {
    $ws$; Pint; $ws$;
    urbanLabel : $uc$;
    Pstring(56); wsDouble; wsDouble; $nL$;
    urbanData : wsDouble Plist("",8); $nL$
}
  ptype urbanLines = urbanLine Plist("",EOF)

  ptype wusLine = wsDouble Plist("",6)
  ptype wusLines = wusLine Plist($nL$,EOF)
  ptype wusFile = {
    stringLn Plist("",3);
  wusLines : wusLines
}

  ptype labelDouble = {
    label : Pstring(10);
    $ws$; "="; $ws$; Pfloat
}
  ptype wgnInfo = wsDouble Plist("",12)
  ptype wgnLine = { wgnInfo : wgnInfo; $nL$ }
  ptype wgnLatLong = labelDouble Plist("",2)
  ptype wgnLines = wgnLine Plist("",14)
  ptype wgnFile = {
    stringLn;
    wgnLatLong : wgnLatLong; $nL$;
    wgnElev : labelDouble; $nL$;
    wgnRain : labelDouble; $nL$;
    wgnLines : wgnLines
}

  ptype chmLine = {
    colonLabel : $RE "[^:]+"$; ":";
    chmInfo : wsDouble Plist("",10); $nL$
}
  ptype chmEndLine = wsDouble Plist("",$nL$)
  ptype chmEndLines = chmEndLine Plist("",EOF)
  ptype chmFile = {
    stringLn Plist("",2);
    chmData : chmLine Plist("",5);
    stringLn Plist("",4);
    chmPestData : chmEndLines
}

  ptype dateFloatLine = {
    year : Pstring(4);
    date : Pstring(3);
    dateInfo : Pstring(8)
}
  ptype dateFloatLines = dateFloatLine Plist($nL$,EOF)
  ptype dateFloatFile = {
    stringLn;
    dateFloatLines : dateFloatLines
}

  ptype pestLine = {
    pestNum : Pstring(3);
    pestName : Pstring(17);
    pestSKOC : Pstring(10);
    pestWOF : Pstring(5);
    pestHLIFE_F : Pstring(8);
    pestHLIFE_S : Pstring(8);
    pestAP_EF : Pstring(5);
    pestWSOL : Pstring(11)
}
  ptype pestFile = pestLine Plist($nL$,EOF)

  ptype fertLine = {
    fertNum : Pstring(4); " ";
    festName : Pstring(8);
    fertMineralN : Pstring(8);
    festMineralP : Pstring(8);
    fertOrgN : Pstring(8);
    fertOrgP : Pstring(8);
    fertAmmN : Pstring(8);
    bactPers : Pstring(8);
    bactLP : Pstring(10);
    bactPart : Pstring(6) (* Issue: should be length 10, but could be less *)
}
  ptype fertFile = fertLine Plist($nL$,EOF)

  ptype solLineTopStr = { 
    solDescLabel : $RE "[^:]+"$; ":"; $ws$; stringLn
}
  ptype solLineTopVal = {
    solValLabel : $RE "[^:]+"$; ":"; $ws$;
    solVal : Pfloat; $nL$
}
  ptype solLineBot = {
    solBotLabel : $RE "[^:]+"$; ":";
    solBotVal1 : wsDouble;
    solBotVal2 : wsDouble; $nL$
}
  ptype solFile = {
    stringLn;
    solDesc : solLineTopStr Plist("",2);
    solVals : solLineTopVal Plist("",3);
    stringLn;
    solBotVals : solLineBot Plist("",14)
}

  ptype reachLine = {
    "REACH"; $ws$;
    reachID : Pint; $ws$;
    reachGIS : Pint; $ws$;
    reachMon : Pint; $ws$;
    reachArea : Pfloat; $ws$;
    reachFlowIn : Pfloat; $ws$;
    reachFlowOut : Pfloat; $ws$;
    Pstring(480);
    reachTotalNitrogen : Pfloat; $ws$;
    reachTotalPhosphorus : Pfloat; $ws$; Pfloat; $nL$
}
  ptype reachLines = reachLine Plist("",EOF)
  ptype reachFile = {
    stringLn Plist("",9);
    reachInfo : reachLines
}

  ptype outputHruLine = {
    outputHruLULC : Pstring(4); " ";
    outputHruHRU : Pstring(4); " ";
    outputHruGIS : Pstring(9); " ";
    outputHruSUB : Pstring(4); " ";
    outputHruMGT : Pstring(4); " ";
    outputHruMON : Pstring(4);
    outputHruAREA : Pstring(10);
    outputHruPrecip : Pstring(10); $nL$
}
  ptype outputHruLines = outputHruLine Plist("",EOF)
  ptype outputHruFile = {
    stringLn Plist("",9);
    outputHruInfo : outputHruLines
}

  ptype outputSedLine = {
    "REACH "; outputSedRCH : Pstring(4); " ";
    outputSedGIS : Pstring(8); " ";
    outputSedArea : Pstring(5);
    outputSedIn : Pstring(12);
    outputSedOut : Pstring(12);
    outputSandIn : Pstring(12);
    outputSandOut : Pstring(12);
    outputSiltIn : Pstring(12);
    outputSiltOut : Pstring(12);
    Pstring(168); $nL$ }
  ptype outputSedLines = outputSedLine Plist("",EOF)
  ptype outputSedFile = {
    stringLn;
    outputSedInfo : outputSedLines
}

  ptype outputSubLine = {
    "BIGSUB"; outputSubSub : Pstring(4); " ";
    outputSubGIS : Pstring(8); " ";
    outputSubMon : Pstring(4);
    outputSubArea : Pstring(10);
    outputSubPrecip : Pstring(10);
    outputSubMelt : Pstring(10);
    outputSubPET : Pstring(10);
    outputSubET : Pstring(10);
    outputSubSW : Pstring(10);
    Pstring(171); $nL$
}
  ptype outputSubLines = outputSubLine Plist("",EOF) 
  ptype outputSubFile = {
    stringLn Plist("",9);
    outputSubInfo : outputSubLines
}


  ptype word = $nonws$ * $ws$
  ptype date = { year : Pstring(4); "-"; month : Pstring(2); "-"; day : Pstring(2) }
  ptype flowLine = {
    word Plist("",4);
    flowValue : Pfloat; $ws$;
    word;
    flowDate : date; $nL$
}
  ptype flowLines = flowLine Plist("",EOF)
  ptype flowFile = {
    stringLn;
    flowInfo : flowLines
}
|}]


(* Hacky *)
let rec charListToString l =
  match l with
  | [] -> ""
  | h::t -> if (List.mem h ['\r';'\n']) then (charListToString t) else (Char.escaped h) ^ (charListToString t)

let rec stringToCharList s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec get_nth l i =
  match l with
  | [] -> failwith "Index out of range"
  | h::t -> if i = 0 then h else get_nth t (i-1)

let rec drop n l =
  match l with
  | [] -> []
  | h::t -> if n = 0 then l else drop (n-1) t

let rec take n l =
  match l with
  | [] -> []
  | h::t -> if n = 0 then [] else h::(take (n-1) t)

let getSwatStringVal s =
  match s with
  | SwatValue(SwatString {stringValue = s}) -> String.trim s
  | _ -> ""

let words = Str.split (Str.regexp "[ \t]+")

let get_f_weath_opt n rep = 
  let s = getSwatStringVal (get_nth rep.weatherFiles n) in
  if s = ""
  then "NOFILEEXIST"
  else s
  
let cstFile = get_f_weath_opt 3  
let slrFile = get_f_weath_opt 0
let rhFile = get_f_weath_opt 1
let wndFile = get_f_weath_opt 2
    
let basinFile rep = getSwatStringVal rep.basinFile
let plantFile rep = getSwatStringVal (get_nth rep.databaseFiles 0)
let tillFile rep = getSwatStringVal (get_nth rep.databaseFiles 1)
let pestFile rep = getSwatStringVal (get_nth rep.databaseFiles 2)
let fertFile rep = getSwatStringVal (get_nth rep.databaseFiles 3)
let urbanFile rep = getSwatStringVal (get_nth rep.databaseFiles 4)

let pcpFiles s =
  List.fold_left (fun acc l -> words l.str @ acc) [] s.pcpFiles

let tmpFiles s =
  List.fold_left (fun acc l -> words l.str @ acc) [] s.tmpFiles

let subFiles f =
  List.fold_left (fun acc l ->
    match l with
    | Sub s -> s.subFile :: acc
    | _ -> acc) [] f

let rteFiles f =
  List.fold_left (fun acc l ->
    match l with
    | Route r -> r.rteFile :: acc
    | _ -> acc) [] f

let swqFiles f =
  List.fold_left (fun acc l ->
    match l with
    | Route r -> r.swqFile :: acc
    | _ -> acc) [] f

let wgnFile rep = getSwatStringVal (get_nth rep.climate 8)
let pndFile rep = getSwatStringVal rep.impoundmentFile
let wusFile rep = getSwatStringVal rep.waterUseFile

let allWgnFiles l = List.filter (fun s -> s <> "") @@ List.map wgnFile l
let allPndFiles l = List.filter (fun s -> s <> "") @@ List.map pndFile l
let allWusFiles l = List.filter (fun s -> s <> "") @@ List.map wusFile l

let hruFile x = x.hruFile
let mgtFile x = x.mgtFile
let solFile x = x.solFile
let chmFile x = x.chmFile
let gwFile x = x.gwFile
let opsFile x = x.opsFile
let septFile x = x.sepFile (*
let sdrFile x = x.sdrFile *)

let getHruFiles f l =
  List.fold_left (fun acc l ->
    let next = String.trim (f l) in
    if next = "" then acc else next::acc) [] l

let hruFiles = getHruFiles hruFile
let mgtFiles = getHruFiles mgtFile
let solFiles = getHruFiles solFile
let chmFiles = getHruFiles chmFile
let gwFiles = getHruFiles gwFile
let opsFiles = getHruFiles opsFile
let septFiles = getHruFiles septFile (*
let sdrFiles = getHruFiles sdrFile *)

let allHruFiles x = List.flatten (List.map (fun sub -> hruFiles (sub.hru)) x)
let allMgtFiles x = List.flatten (List.map (fun sub -> mgtFiles (sub.hru)) x)
let allSolFiles x = List.flatten (List.map (fun sub -> solFiles (sub.hru)) x)
let allChmFiles x = List.flatten (List.map (fun sub -> chmFiles (sub.hru)) x)
let allGwFiles x = List.flatten (List.map (fun sub -> gwFiles (sub.hru)) x)
let allOpsFiles x = List.flatten (List.map (fun sub -> opsFiles (sub.hru)) x)
let allSepFiles x = List.flatten (List.map (fun sub -> septFiles (sub.hru)) x)

let mapSize m = PathMap.fold (fun _ _ x -> x + 1) m 0
;;

[%%skin {|
  delayAll = <>;map(delayAll)

  skin2 = delayAll
  skin3 = skin2;cio(><)
  skin4 = skin3;fig(><)
  skin5 = skin4;subs(><;[><])

  predSkin = delayAll;fig(><);subs(><;[><]);cio(><)
  
  inCalib = predSkin;bsn(><);gws(><;[><]);hrus(><;[><])
  outCalib = delayAll;outRch(><)

  inLU = predSkin;mgts(><;[><])
  outLU = delayAll;outStd(><)
|}]

(* TODO: Change here if we want a different cost function *)
module CostMon = Forest.CostUnitMon

[%%forest {|

cioFile = pads cio

figFile = pads figLines

sub = pads subFile

pcp = pads pcpFile

tmp = pads tmpFile

crop = pads plantFile

till = pads tillFile

urban = pads urbanLines

bsn = pads bsnFile

wus = pads wusFile

wgn = pads wgnFile

chm = pads chmFile

pest = pads pestFile

fert = pads fertFile

sol = pads solFile

wnd = pads dateFloatFile
rh = pads dateFloatFile
slr = pads dateFloatFile

hru = pads hruFile
mgt = pads mgtFile
gw = pads gwFile
rte = pads swatLines
swq = pads swatLines
pnd = pads swatLines
sep = pads swatLines

swatIn = directory {
  cio is "file.cio" :: cioFile;
  fig is $cio.figFile.str$:: figFile;
  bsn is $basinFile cio$ :: bsn;
  subs is [f :: sub | f <- $subFiles fig$ ];
  hrus is [f :: hru | f <- $allHruFiles subs$ ];
  mgts is map [f :: mgt | f <- $allMgtFiles subs$ ];
  gws is [ f :: gw | f <- $allGwFiles subs$ ];
  cst is $cstFile cio$ :: file option;
  wnd is $wndFile cio$ :: wnd option;
  rh is $rhFile cio$ :: rh option;
  slr is $slrFile cio$ :: slr option;
  plant is $plantFile cio$ :: crop;
  till is $tillFile cio$ :: till;
  pest is $pestFile cio$ :: pest;
  fert is $fertFile cio$ :: fert;
  urban is $urbanFile cio$ :: urban;
  pcps is [ f :: pcp | f <- $pcpFiles cio$ ];
  tmps is [ f :: tmp | f <- $tmpFiles cio$ ];
  rtes is [f :: rte | f <- $rteFiles fig$ ];
  swqs is [f :: swq | f <- $swqFiles fig$ ];
  sols is [f :: sol | f <- $allSolFiles subs$ ];
  chms is [f :: chm | f <- $allChmFiles subs$ ];
  seps is [ f :: sep | f <- $allSepFiles subs$ ];
  wgns is [ f :: wgn | f <- $allWgnFiles subs$ ];
  pnds is [ f :: pnd | f <- $allPndFiles subs$ ];
  wuss is [ f :: wus | f <- $allWusFiles subs$ ]
} 

swatICB = swatIn @ inCalib
swatILU = swatIn @ inLU
swatIP = swatIn @ predSkin

swatOut = directory {
  outHru is "output.hru" :: pads outputHruFile;
  outPst is "output.pst" :: file;
  outRch is "output.rch" :: pads reachFile;
  outRsv is "output.rsv" :: file;
  outSed is "output.sed" :: pads outputSedFile;
  outStd is "output.std" :: file;
  outSub is "output.sub" :: pads outputSubFile
}
swatOCB = swatOut @ outCalib
swatOLU = swatOut @ outLU
swatOP = swatOut @ delayAll

reach = pads reachFile
flow = pads flowFile
|}]


let vl_to_s = function
  | SwatValue (SwatInt i) ->i.intLabel ^ ":" ^ i.intDescription.str
  | SwatValue (SwatFloat f) -> f.floatLabel ^ ":" ^ f.floatDescription.str
  | SwatValue (SwatString s) -> s.stringValue ^ "|" ^ s.stringLabel ^ ":" ^ s.stringDescription.str
  | SwatLine s -> s.str

let dir = Filename.concat (Sys.getcwd ())  "../../swatData/TxtInOut_currentnew"

let test_reach () =
  let (rep, md) = reach_load (Filename.concat dir "output.rch") in
  Printf.printf "Loading reach\n";
  Printf.printf "Errors: %d\n" (md.num_errors);
  List.iter print_endline md.error_msg

let test_flow () =
  let (rep, md) = flow_load (Filename.concat dir "FC_flowgage.csv") in
  Printf.printf "Errors: %d\n" (md.num_errors);
  List.iter print_endline md.error_msg

let test_swat () =
  let (rep, md) = swatIn_load dir in
  Printf.printf "Errors: %d\n" (md.num_errors);
  List.iter print_endline md.error_msg

let test_cio () =
  Printf.printf "Loading cio\n";
  let (rep, md) = cioFile_load (Filename.concat dir "file.cio") in
  Printf.printf "Fig file: %s\n" rep.figFile.str;
  Printf.printf "Errors: %d\n" (md.num_errors);
  Forest.print_md_errors md
                                 
let test_fig () =
  let (fig_rep, fig_md) = figFile_load (Filename.concat dir "fig.fig") in
  Printf.printf "Errors: %d\n" (fig_md.num_errors);
  List.iter print_endline fig_md.error_msg
  (*
  List.iter2 (fun r md ->
    match r with
    | Sub fs -> Printf.printf "%s:%s" fs.subInfo fs.subFile
    | _ -> ()
  ) fig_rep fig_md.data.pads_data
  *)

let test_sub () =
  Printf.printf "Checking sub file\n"; flush stdout;
  let (sub_rep, sub_md) = sub_load (Filename.concat dir "000010000.sub") in
  Printf.printf "Errors: %d\n" (sub_md.num_errors);
  List.iter print_endline sub_md.error_msg

let test_pcp () =
  let (pcp_rep, pcp_md) = pcp_load (Filename.concat dir "pcp1.pcp") in
  Printf.printf "Errors: %d\n" (pcp_md.num_errors);
  List.iter print_endline pcp_md.error_msg

let test_tmp () =
  let (tmp_rep, tmp_md) = tmp_load (Filename.concat dir "tmp1.tmp") in
  Printf.printf "Errors: %d\n" (tmp_md.num_errors);
  List.iter print_endline tmp_md.error_msg

let test_crop () =
  let (crop_rep, crop_md) = crop_load (Filename.concat dir "plant.dat") in
  Printf.printf "Errors: %d\n" (crop_md.num_errors);
  List.iter print_endline crop_md.error_msg

let test_till () =
  let (till_rep, till_md) = till_load (Filename.concat dir "till.dat") in
  Printf.printf "Errors: %d\n" (till_md.num_errors);
  List.iter print_endline till_md.error_msg

let test_urban () =
  let (urban_rep, urban_md) = urban_load (Filename.concat dir "urban.dat") in
  Printf.printf "Errors: %d\n" (urban_md.num_errors);
  List.iter print_endline urban_md.error_msg

let test_wus () =
  let (wus_rep, wus_md) = wus_load (Filename.concat dir "000010000.wus") in
  Printf.printf "Errors: %d\n" (wus_md.num_errors);
  List.iter print_endline wus_md.error_msg

let test_wgn () =
  let (wgn_rep, wgn_md) = wgn_load (Filename.concat dir "000010000.wgn") in
  Printf.printf "Errors: %d\n" (wgn_md.num_errors);
  List.iter print_endline wgn_md.error_msg

let test_chm () =
  let (chm_rep, chm_md) = chm_load (Filename.concat dir "000010001.chm") in
  Printf.printf "Errors: %d\n" (chm_md.num_errors); flush stdout;
  List.iter print_endline chm_md.error_msg

let test_pest () =
  let (pest_rep, pest_md) = pest_load (Filename.concat dir "pest.dat") in
  Printf.printf "Errors: %d\n" (pest_md.num_errors);
  List.iter print_endline pest_md.error_msg

let test_fert () =
  let (fert_rep, fert_md) = fert_load (Filename.concat dir "fert.dat") in
  Printf.printf "Errors: %d\n" (fert_md.num_errors);
  List.iter print_endline fert_md.error_msg

let test_sol () =
  let (sol_rep, sol_md) = sol_load (Filename.concat dir "000010001.sol") in
  Printf.printf "Errors: %d\n" (sol_md.num_errors);
  List.iter print_endline sol_md.error_msg
