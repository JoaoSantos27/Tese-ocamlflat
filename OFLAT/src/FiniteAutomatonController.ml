open OCamlFlat
open BasicTypes
open HtmlPageClient
open Lang
open Js_of_ocaml
open JS
open Controller
open AutomatonController
open AutomatonView
open FiniteAutomatonView
open Listeners

class faController (fa: FiniteAutomatonView.model) (s: bool)=
  object(self) inherit automatonController(s) as super

    val mutable myFA = fa

    method operationAutomaton opName : unit =
        super#operation opName "FA"

    method model: Model.model = 
      (myFA :> Model.model) 

    method getAutomaton: AutomatonView.model =
      (myFA :> AutomatonView.model)

    method getFA =
      myFA

    method changeAutomata res =
      myFA <- res

    method getModel = 
      myFA#toDisplayString "solution"

(* ML
	  method addNode x y st : unit = 
      self#operationAutomaton "add Node";
      if (Set.belongs st myFA#representation.states) then 
        (JS.alertStr (Lang.i18nAlertExists ()))
      else 
        (myFA <- myFA#addNode st false;
        Cytoscape.addNode self#getCy st ~x:x ~y:y false false;
        self#defineInformationBox;)
*)

(* imitar TM *)
	  method addNode x y initial final: unit = 
      self#operationAutomaton "add Node";
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> let st = (Js.to_string v) in
                  JS.log myFA#representation.states;
                  if (Set.belongs st myFA#representation.states) then 
                    (JS.alertStr (Lang.i18nAlertExists ()))
                  else 
                    (myFA <- myFA#addNode st false;
                    Cytoscape.addNode self#getCy st ~x:x ~y:y initial final;
                    self#defineInformationBox;)


    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (FiniteAutomaton.kind)

    method returnType = FiniteAutomaton.kind

    method loadButtons = 
      HtmlPageClient.putCyAutomataButtons ()

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      let deter = myFA#isDeterministic in 
        HtmlPageClient.getDeterminim deter infoBox;
      let min = myFA#isMinimized in 
        HtmlPageClient.getMinimism min infoBox;
      let useful = myFA#areAllStatesUseful in
      let uStates = myFA#getUselessStates in 
        HtmlPageClient.getHasUselessStates useful uStates infoBox;
      let nStates = myFA#numberStates in 
        HtmlPageClient.getNumberStates nStates infoBox;
      let nTransitions = myFA#numberTransitions in
        HtmlPageClient.getNumberTransitions nTransitions infoBox;
      let _ = myFA#buildTable in () (*UPDATE TABLE*)

    method createTransition source target =
      self#operationAutomaton "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransition ()) "c") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
        let v = symb (Js.to_string v) in
        (if v = epsilon
        then myFA <- myFA#newEpsylonTransition (source, v, target)
        else myFA <- myFA#newTransition (source, v, target));
        Cytoscape.addEdge self#getCy (source, symb2str v, target);
        self#defineInformationBox;
      
(* imitar TM *)
    method addFinalNode x y node =
      self#operationAutomaton "add final node";
      if (Set.belongs node myFA#representation.states) then
        (JS.alertStr (Lang.i18nAlertExists ()))
      else (
        myFA <- myFA#addFinalNode node false false;
        Cytoscape.addNode self#getCy ~x:x ~y:y node false true;
        self#defineInformationBox;
      )

(* imitar TM *)
    method addInitialNode node =
      self#operationAutomaton "make node initial";
      let stateExists = Set.belongs node myFA#representation.states in 
          myFA <- (myFA#addInitialNode node false stateExists);
          let cy = self#getCy in
          Cytoscape.resetFaElems cy;
          myFA#drawExample cy;
          self#defineInformationBox;

    method eliminateTransition (v1, s, v2) =
      self#operationAutomaton "erase transition";
      let c3 = symb s in
      if (Set.belongs (v1, c3, v2) myFA#representation.transitions) then
        (myFA <- (myFA#eliminateTransition(v1, c3, v2));
      Cytoscape.removeEdge self#getCy v1 (symb2str c3) v2;
        self#defineInformationBox;)
      else 
        JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ "(" ^ v1 ^ ", " ^ symb2str c3 ^ ", " ^ v2 ^ ")" ^ (Lang.i18nAlertDoNotExists ()))
    
     method turnFinalNode node =
      self#operationAutomaton "make node final";
      if (Set.belongs node myFA#representation.acceptStates) then
          (JS.alertStr (Lang.i18nAlertAlreadyFinal ()))
      else
        (myFA <- (myFA#changeToFinal node);
        Cytoscape.turnFinal self#getCy node);
      self#defineInformationBox;
    
    method removeFinalNode node =
      self#operationAutomaton "make node not final";
      if (Set.belongs node myFA#representation.acceptStates) then
        (myFA <- (myFA#removeFinal node);
        Cytoscape.removeFinal self#getCy node)
      else
        (JS.alertStr (Lang.i18nAlertNonFinal ())); 
      self#defineInformationBox;
      
    method eliminateNode node =
      self#operationAutomaton "eliminate node";
      let eliminateNodeTransitions (a, b, c) node = 
        if (a = node || c = node) then
          (myFA <- (myFA#eliminateTransition (a, b, c));
      self#defineInformationBox;) in 
        if (node = myFA#representation.initialState )then 
          JS.alertStr (Lang.i18nAlertDelete ()) 
        else 
          if (Set.belongs node myFA#representation.states) then 
            (let isFinal = Set.belongs node myFA#representation.acceptStates in 
            myFA <- myFA#eliminateNode node false isFinal;
            Set.iter (fun el -> (eliminateNodeTransitions el node)) myFA#representation.transitions;
            Cytoscape.removeNode self#getCy node;
            self#defineInformationBox;)
          else 
            JS.alertStr (Lang.i18nAlertUnexistentState ())

    method renameState state =
      self#operationAutomaton "rename node";
      let newName = JS.prompt (Lang.i18nRenameStateQuestion()) state in
      match Js.Opt.to_option newName with
      | None -> ()
      | Some n -> myFA <- myFA#renameState state (Js.to_string n);
                  Cytoscape.resetFaElems self#getCy;
                  self#defineExample

	  method updateButtons =
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;

      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMConvertButtons;

      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons

      (* method getWords v = 
      self#operationAutomaton "accepted words";
        let var = self#getAutomaton#staticGenerate v in 
        let (_, visitedConfigs, exact, time) = self#getAutomaton#returnStats in
          HtmlPageClient.putWords var;
          HtmlPageClient.displayGenStats visitedConfigs exact time *)

    method defineMinimize listColors number =
      myFA#paintMinimization self#getCy listColors;
      myFA#drawMinimize self#getCy listColors number;
      Cytoscape.fit self#getCy_opt
    
    method editModel = 
      !ListenersFA.editModelListener(); ()
    
    method replicateOnLeft =
      let c = new faController self#getFA false in
      Ctrl.ctrlL := (c :> controller);

    method convertToRegExp =
      let open RegularExpressionView in
      self#operationAutomaton "convert to RE";
      let reg = PolyModel.fa2re (myFA :> FiniteAutomaton.model) in
      let r = reg#simplify in 
      let rep = r#representation in 
      new RegularExpressionView.model (Representation (rep))

    method convertToPDA =
      let open PushdownAutomatonView in
      self#operationAutomaton "convert to PDA";
      let pda = PolyModel.fa2pda (myFA :> FiniteAutomaton.model) in
      new PushdownAutomatonView.model (Representation (pda#representation))

    method convertToCFG =
      let open ContextFreeGrammarView in
      self#operationAutomaton "convert to CFG";
      let cfg = PolyModel.fa2cfg (myFA :> FiniteAutomaton.model) in
      new ContextFreeGrammarView.model (Representation (cfg#representation))

    method convertToGR =
      let open GrammarView in
      self#operationAutomaton "convert to GR";
      let gr = PolyModel.fa2gr (myFA :> FiniteAutomaton.model) in
      new GrammarView.model (Representation (gr#representation))

    method convertToTM_SingleTape =
      let open TuringMachineView in
      self#operationAutomaton "convert to TM single tape";
      let tm = PolyModel.fa2tm (myFA :> FiniteAutomaton.model) in
      new TuringMachineView.model (Representation (tm#representation))

    method printErrors =
          let errors = myFA#errors in
            if errors = [] then
              ()
            else
              JS.alertStr (String.concat "\n" errors)
end
