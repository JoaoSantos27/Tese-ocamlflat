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
open Settings
open StateVariables

class faController (fa: FiniteAutomatonView.model) (s: bool)=
  object(self) inherit automatonController(s) as super

    val mutable myFA: FiniteAutomatonView.model = fa

    method operationFA opName : unit =
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
      self#operationFA "add Node";
      if (Set.belongs st myFA#representation.states) then 
        (JS.alertStr (Lang.i18nAlertExists ()))
      else 
        (myFA <- myFA#addNode st false;
        Cytoscape.addNode self#getCy st ~x:x ~y:y false false;
        self#defineInformationBox;)
*)

(* imitar TM *)
	  method addNode x y initial final: unit = 
      self#operationFA "add Node";
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> let st = (Js.to_string v) in
                  JS.log myFA#representation.states;
                  if (Set.belongs st myFA#representation.states) then 
                    (JS.alertStr (Lang.i18nAlertExists ()))
                  else 
                    (myFA <- (if final then myFA#addFinalNode st false false else myFA#addNode st false);
                    super#resetStyle;
                    Cytoscape.addNode self#getCy st ~x:x ~y:y initial final;
                    self#defineInformationBox;)

    method returnType = FiniteAutomaton.kind

    method loadButtons = 
      HtmlPageClient.putCyAutomataButtons ()

    method defineInformationBox =
      let name = myFA#getName in
      let isDeter = myFA#isDeterministic in 
      let isMin = myFA#isMinimized in 
      let hasUseless = not myFA#areAllStatesUseful in
      let nUseless = myFA#getUselessStates in 
      let nStates = myFA#numberStates in 
      let nTrans = myFA#numberTransitions in
      let _ = myFA#buildTable in (*UPDATE TABLE*)
        HtmlPageClient.drawAutomatonStats (Lang.i18nFA ()) name isDeter hasUseless nUseless nStates nTrans isMin side

    method createTransition source target =
      let getText isEmpty trans =
        if isEmpty then StateVariables.returnEmpty () else trans
      in
      self#operationFA "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransition ()) "c") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
        let trans = Js.to_string v in
        let isEmpty = Settings.isEpsilon trans in
        (if isEmpty
        then myFA <- myFA#newEpsylonTransition (source, epsilon, target)
        else myFA <- myFA#newTransition (source, (symb trans), target));
        super#resetStyle;
        Cytoscape.addEdge self#getCy (source, (getText isEmpty trans), target);
        self#defineInformationBox;
      
    method addInitialNode: unit =
      self#operationFA "Add Initial Node";
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> let st = (Js.to_string v) in
                  if (Set.belongs st myFA#representation.states) then 
                    (JS.alertStr (Lang.i18nAlertExists ()))
                  else 
                    (
                    let cy = self#getCy in
                    let stateExists = Set.belongs st myFA#representation.states in
                      myFA <- (myFA#addInitialNode st false stateExists);
                      Cytoscape.resetFaElems cy;
                      myFA#drawExample cy (Settings.getSetting "layout");
                      self#defineInformationBox;)

    method addFinalNode x y node =
      self#operationFA "add final node";
      if (Set.belongs node myFA#representation.states) then
        (JS.alertStr (Lang.i18nAlertExists ()))
      else (
        myFA <- myFA#addFinalNode node false false;
        Cytoscape.addNode self#getCy ~x:x ~y:y node false true;
        self#defineInformationBox;
      )

    method turnInitialNode node =
      self#operationFA "turn node initial";
      let stateExists = Set.belongs node myFA#representation.states in 
          myFA <- (myFA#addInitialNode node false stateExists);
          let cy = self#getCy in
          Cytoscape.resetFaElems cy;
          myFA#drawExample cy (Settings.getSetting "layout");
          self#defineInformationBox

    method eliminateTransition (v1, s, v2) =
      let getSymb isEmpty trans =
        if isEmpty then epsilon else symb trans
      in
      self#operationFA "erase transition";
      let c3 = getSymb (Settings.isEpsilon s) s in
      if (Set.belongs (v1, c3, v2) myFA#representation.transitions) then
        (super#resetStyle;
        myFA <- (myFA#eliminateTransition(v1, c3, v2));
        Cytoscape.removeEdge self#getCy v1 s v2;
        self#defineInformationBox;)
      else 
        JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ "(" ^ v1 ^ ", " ^ symb2str c3 ^ ", " ^ v2 ^ ")" ^ (Lang.i18nAlertDoNotExists ()))

     method turnFinalNode node =
      self#operationFA "make node final";
      if (Set.belongs node myFA#representation.acceptStates) then
          (JS.alertStr (Lang.i18nAlertAlreadyFinal ()))
      else
        (super#resetStyle; 
        myFA <- (myFA#changeToFinal node);
        Cytoscape.turnFinal self#getCy node);
      self#defineInformationBox;
    
    method removeFinalNode node =
      self#operationFA "make node not final";
      if (Set.belongs node myFA#representation.acceptStates) then
        (super#resetStyle;
        myFA <- (myFA#removeFinal node);
        Cytoscape.removeFinal self#getCy node)
      else
        (JS.alertStr (Lang.i18nAlertNonFinal ())); 
      self#defineInformationBox;
      
    method eliminateNode node =
      self#operationFA "eliminate node";
      let eliminateNodeTransitions (a, b, c) node = 
        if (a = node || c = node) then
          (myFA <- (myFA#eliminateTransition (a, b, c));
          self#defineInformationBox;) 
        in 
          if (node = myFA#representation.initialState )then 
            JS.alertStr (Lang.i18nAlertDelete ()) 
          else
            if (Set.belongs node myFA#representation.states) then 
              (let isFinal = Set.belongs node myFA#representation.acceptStates in 
              super#resetStyle;
              Set.iter (fun el -> (eliminateNodeTransitions el node)) myFA#representation.transitions;
              myFA <- myFA#eliminateNode node false isFinal;
              Cytoscape.removeNode self#getCy node;
              self#defineInformationBox;)
            else 
              JS.alertStr (Lang.i18nAlertUnexistentState ())

    method renameState state =
      self#operationFA "rename node";
      let newName = JS.prompt (Lang.i18nRenameStateQuestion()) state in
      match Js.Opt.to_option newName with
      | None -> ()
      | Some n -> super#resetStyle;
                  myFA <- myFA#renameState state (Js.to_string n);
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
      self#operationFA "accepted words";
        let var = self#getAutomaton#staticGenerate v in 
        let (_, visitedConfigs, exact, time) = self#getAutomaton#returnStats in
          HtmlPageClient.putWords var;
          HtmlPageClient.displayGenStats visitedConfigs exact time *)

    method defineMinimize listColors number =
      self#operationFA "minimize";
      myFA#paintMinimization self#getCy listColors;
      myFA#drawMinimize self#getCy listColors number (Settings.getSetting "layout");
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt
    
    method editModel = 
      !ListenersFA.editModelListener(); ()
    
    method replicateOnLeft =
      let c = new faController self#getFA false in
        Ctrl.changeCtrl c false

    method convertToRegExp =
      let open RegularExpressionView in
      self#operationFA "convert to RE";
      let reg = PolyModel.fa2re (myFA :> FiniteAutomaton.model) in
      let r = reg#simplify in 
      let rep = r#representation in 
      new RegularExpressionView.model (Representation (rep))

    method convertToPDA =
      let open PushdownAutomatonView in
      self#operationFA "convert to PDA";
      let pda = PolyModel.fa2pda (myFA :> FiniteAutomaton.model) in
      new PushdownAutomatonView.model (Representation (pda#representation))

    method convertToCFG =
      let open ContextFreeGrammarView in
      self#operationFA "convert to CFG";
      let cfg = PolyModel.fa2cfg (myFA :> FiniteAutomaton.model) in
      new ContextFreeGrammarView.model (Representation (cfg#representation))

    method convertToGR =
      let open GrammarView in
      self#operationFA "convert to GR";
      let gr = PolyModel.fa2gr (myFA :> FiniteAutomaton.model) in
      new GrammarView.model (Representation (gr#representation))

    method convertToTM_SingleTape =
      let open TuringMachineView in
      self#operationFA "convert to TM single tape";
      let tm = PolyModel.fa2tm (myFA :> FiniteAutomaton.model) in
      new TuringMachineView.model (Representation (tm#representation))

    method getModelName =
      "fa"

    method printErrors =
          let errors = myFA#errors in
            if errors = [] then
              ()
            else
              JS.alertStr (String.concat "\n" errors)
end
