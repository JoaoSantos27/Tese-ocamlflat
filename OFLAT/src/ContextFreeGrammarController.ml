open OCamlFlat
open BasicTypes
open HtmlPageClient
open JS
open Lang
open Controller
open ContextFreeGrammarView
open ContextFreeGrammarLL1View
open Listeners

(* PEDRO CARLOS VER! debug? *)
let grammar2Str (rep:ContextFreeGrammarView.t) = 
  let open ContextFreeGrammarBasic in
  let initialRules = Set.filter (fun {head = h; _} -> h = rep.initial) rep.rules in
  let nonInitialRules = Set.filter (fun {head = h; _} -> h <> rep.initial) rep.rules in
  let initialRulesStrLst = ContextFreeGrammarBasic.toStringList initialRules in
  let nonInitialRulesStrLst = ContextFreeGrammarBasic.toStringList nonInitialRules in
  let rulesList = initialRulesStrLst @ nonInitialRulesStrLst in
  let rec toString l =
    match l with
    | [] -> ""
    | x::xs -> x ^ "\n" ^ (toString xs)
  in
    toString rulesList

class virtual cfgBasicController (cfg: ContextFreeGrammarView.model) (s:bool) =
  object(self) inherit controller as super
    val mutable myCFG = cfg 
    val side = s
    val cy = if s then Some (Cytoscape.initLL1Cy "cy2") else Some (Cytoscape.initLL1Cy "cy");

    method operationCFG opName: unit =
      super#operation opName "CFG"
   
    method model: Model.model =
      (myCFG :> Model.model)
    
    method getCFG = myCFG
    
    method changeCFG res = myCFG <- res
    
    method getModel =
      myCFG#toDisplayString "solution"
 
    method setTitle =
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (ContextFreeGrammarBasic.kind)

    method returnType = ContextFreeGrammar.kind
    
    method defineExample = 
      self#operationCFG "create example";
      self#updateButtons;
      HtmlPageClient.putCyCFGButtons();
      HtmlPageClient.cfgBoxRegex();
      HtmlPageClient.cfgCyClose();
      HtmlPageClient.defineCFG();
      myCFG#createGrammarTableHtml ""; 
      self#defineInformationBox
end

class virtual cfgLL1Controller (cfg: ContextFreeGrammarView.model) (s:bool) =
  object(self) inherit cfgBasicController cfg s  as super

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      if side then HtmlPageClient.cfgCy2Close();
      let ll1 = myCFG#isLL1 in 
        HtmlPageClient.getIsLL1 ll1 infoBox;
      let lr = myCFG#isLeftRecursive in 
        HtmlPageClient.getIsLeftRecursive lr infoBox;
      let lf = myCFG#isLeftFactoring in 
        HtmlPageClient.getIsLeftFactoring lf infoBox;
      let pConf = myCFG#hasParsingTableConflict in
        HtmlPageClient.getHasParsingTableConflict pConf infoBox;
      let c = myCFG#isClean in
      let prod = myCFG#isFullyProductive in
      let access = myCFG#isFullyAccessible in
        HtmlPageClient.getIsCFGClean c prod access infoBox

    method box2CFGShow (f : ContextFreeGrammarLL1.transformation) =
      self#operationCFG "create example2";
      CtrlUtil.twoBoxes self#getCy_opt;
      HtmlPageClient.printCFG2Grammar f.tType (ContextFreeGrammarLL1View.productionsTableId2());
      (ContextFreeGrammarView.adjust f.grammar)#createGrammarTableHtml (ContextFreeGrammarLL1View.productionsTableId2());
      !Ctrl.ctrlR#defineInformationBox

    method checkWord word =
      self#operationCFG "checkWord";
      let w = str2word word in
      myCFG#staticAccept w;
      let (accepted, configs, exact, time) = myCFG#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time

    method autoAccept =
      if myCFG#isLL1
      then (self#acceptCFGLL1; Lwt.return_true)
      else (JS.alertStr (Lang.i18nIsNotLL1()); Lwt.return_false)      

    method private acceptCFGLL1 =
      self#cancelProm;
      let steps = myCFG#nSteps - 1 in
      let rec tic state n =
        match state with
        | true -> HtmlPageClient.changeButtonColor "autoAccept" "crimson";
                  Lwt.return()
        | false -> let prom = Lwt.bind 
                  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)
                  (fun () -> self#nextStep; tic false (n-1)) in
                  HtmlPageClient.changeButtonColor "autoAccept" "green";
                  self#changeProm prom;
                  prom
      in
      ignore (tic (self#promState = Lwt.Sleep) steps)

    method getWords v = 
      self#operationCFG "accepted words";
      let var = myCFG#staticGenerate v in
      let (_, visitedConfigs, exact, time) =
        myCFG#returnStats in
          HtmlPageClient.putWords var;
          HtmlPageClient.displayGenStats visitedConfigs exact time


    method startStep word =
      self#operationCFG "accept start";
      self#cancelProm;
      CtrlUtil.twoBoxes self#getCy_opt;
      HtmlPageClient.cfgCyOpen();
      HtmlPageClient.prepareCFG2Tables ();
      HtmlPageClient.enableButton "autoAccept";
      match cy with
        | None -> ();
        | Some cy -> Cytoscape.removeAllElements cy;
      myCFG#createFirstAndFollowTableHtml;
      myCFG#createParsingTableHtml;
      if myCFG#isLL1
        then myCFG#startAccept self#getCy (str2word word)
        else JS.alertStr (Lang.i18nIsNotLL1())
    
    method nextStep =
      self#operationCFG "accept next";
      self#cancelProm;
      myCFG#next self#getCy
          
    method backStep = 
      self#operationCFG "accept back";
      self#cancelProm;
      myCFG#back self#getCy

    method editModel =
      !ListenersCFG.editModelListener(); ()

    method updateButtons = 
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTM2TapesConvertButtons;

      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGConvertButtons
  
    method model2Str =
      let gr_representation = !Ctrl.ctrlL#getCFG#representation in
      grammar2Str gr_representation

    method convertToPDA =
      let open PushdownAutomatonView in
      self#operationCFG "convert to PDA";
      let pda = PolyModel.cfg2pda (myCFG :> ContextFreeGrammar.model) in
      new PushdownAutomatonView.model (Representation (pda#representation))
      
    method convertToGR =
      let open GrammarView in
      self#operationCFG "convert to GR";
      let gr = PolyModel.cfg2gr (myCFG :> ContextFreeGrammar.model) in
      new GrammarView.model (Representation (gr#representation)) 

    method convertToTM_SingleTape =
      let open TuringMachineView in
      self#operationCFG "convert to TM single tape";
      let tm = PolyModel.cfg2tm (myCFG :> ContextFreeGrammar.model) in
      new TuringMachineView.model (Representation (tm#representation))

    method convertToTM_DualTape =
      let open TuringMachineView in
      self#operationCFG "convert to TM dual tape";
      let tm = PolyModel.cfg2tm_2tapes (myCFG :> ContextFreeGrammar.model) in
      new TuringMachineView.model (Representation (tm#representation))

    (*Possivelmente mudar para um controlador basic separado*)
    method showTrace word =
      self#operationCFG "trace";
      let w = str2word word in
      myCFG#staticAcceptFull w;
      let (accepted, configs, exact, time) = myCFG#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;
      myCFG#displayTrace

    
end


class cfgLRController (cfg: ContextFreeGrammarView.model) (s: bool)=
  object(self) inherit cfgLL1Controller cfg s as super
      


    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (ContextFreeGrammarBasic.kind)
      
     method defineExample = (* abrir elementos de graficos na janela esquerda *)
		super#defineExample;
		
		let infoBox = HtmlPageClient.defineInformationBox side in

		HtmlPageClient.putLRButtons ();
	(*     !Listeners.defineInformationBoxListener(); *)
		(* myLR#buildCyLR0Diagram self#getCy; *)
		Cytoscape.fit self#getCy_opt;
		 
		 
		(* HtmlPageClient.cfgCyClose(); *)
		HtmlPageClient.defineCFG();
		 
		(* JS.log(self#getCy);
		 JS.log(self#getCy##getElementById (Js.string "14")) 
		 *)
		 
		 if(myCFG#isLR0) then HtmlPageClient.getIsLR0 true infoBox
		 else if(myCFG#isSLR1) then HtmlPageClient.getIsSLR1 true infoBox
		 else if(myCFG#isLALR1) then HtmlPageClient.getIsLALR1 true infoBox
		 else if(myCFG#isLR1) then HtmlPageClient.getIsLR1 true infoBox
		 else HtmlPageClient.getIsLR1 false infoBox
		 

  (*  method defineExample2 = () *) (* abrir codigo na janela direita *)
  (*    myLR#drawExample self#getCy *)
(*      !Listeners.defineInformationBoxListener()*)

    method defineExample2 =
      self#operationCFG "create example2";
      CtrlUtil.twoBoxes self#getCy_opt;
      HtmlPageClient.printCFG2GrammarComp (ContextFreeGrammarLL1View.productionsTableId2());
      myCFG#createGrammarTableHtml (ContextFreeGrammarLL1View.productionsTableId2());
      !Ctrl.ctrlR#defineInformationBox


    method replicateOnLeft =
      let c = new cfgLRController self#getCFG false in
      Ctrl.ctrlL := (c :> controller);

(* AMD check!!! *)
	 method updateButtons =
    List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGButtons;
    List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyPDAButtons;
    List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons;
    List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRConvertButtons;
    List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMConvertButtons;
    List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTM2TapesConvertButtons;

    List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
    List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
    List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGConvertButtons;
end


class cfgController (cfg: ContextFreeGrammarView.model) (s: bool)=
  object(self) inherit cfgLRController cfg s as super



end
