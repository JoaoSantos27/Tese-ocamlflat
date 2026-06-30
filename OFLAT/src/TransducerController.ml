(*
 * TransducerController.ml
 *
 * Description: Controller for Finite-State Transducers in the OFLAT GUI.
 *)
open OCamlFlat
open BasicTypes
open HtmlPageClient
open Lang
open Js_of_ocaml
open JS
open Controller
open AutomatonController
open AutomatonView
open TransducerView
open Listeners
open Settings
open StateVariables

class fstController (fst : TransducerView.model) (s: bool) =
  object (self) inherit automatonController(s) as super

    val mutable myFST: TransducerView.model = fst

    method operationAutomaton opName : unit =
        super#operation opName "FST"

    method getAutomaton: AutomatonView.model =
      (myFST :> AutomatonView.model)

    method getFST = myFST 

    method model: Model.model = 
      (myFST :> Model.model)

    method changeAutomata res =
      myFST <- res

    method getModel = 
      myFST#toDisplayString "solution"

    method returnType = Transducer.kind

    method loadButtons = 
      HtmlPageClient.putCyTransducerButtons ()

    method updateButtons =
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyFSTConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons;
      HtmlPageClient.disableButton "selectRegex"

    method defineInformationBox =
      let name = myFST#getName in
      let isDeter = myFST#isDeterministic in
      let isMin = if isDeter then myFST#isMinimized else false in
      let isMealy = myFST#isMealy in
      let isMoore = myFST#isMoore in
      let hasUseless = not myFST#areAllStatesUseful in
      let nUseless = myFST#getUselessStates in
      let nStates = myFST#numberStates in
      let nTrans = myFST#numberTransitions in
      let _ = myFST#buildTable in
        HtmlPageClient.drawFSTStats (Lang.i18nFST ()) name isDeter isMealy isMoore hasUseless nUseless nStates nTrans isMin side

    method addNode x y initial final : unit = 
      self#operationAutomaton "add Node";
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> 
          let st = (Js.to_string v) in
          if (Set.belongs st myFST#representation.states) then 
            (JS.alertStr (Lang.i18nAlertExists ()))
          else 
            (myFST <- (if final then myFST#addFinalNode st false false else myFST#addNode st false);
             super#resetStyle;
             Cytoscape.addNode self#getCy st ~x:x ~y:y initial final;
             self#defineInformationBox;)

    method addInitialNode : unit =
      self#operationAutomaton "make node initial";
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
          let node = (Js.to_string v) in
          if (Set.belongs node myFST#representation.states) then
            (JS.alertStr (Lang.i18nAlertExists ()))
          else
            (let stateExists = Set.belongs node myFST#representation.states in
             myFST <- (myFST#addInitialNode node false stateExists);
             let cy = self#getCy in
             Cytoscape.resetFaElems cy;
             myFST#drawExample cy (Settings.getSetting "layout");
             self#defineInformationBox;)

    method turnInitialNode node =
      self#operationAutomaton "turn node initial";
      let stateExists = Set.belongs node myFST#representation.states in 
      myFST <- (myFST#addInitialNode node false stateExists);
      let cy = self#getCy in
      Cytoscape.resetFaElems cy;
      myFST#drawExample cy (Settings.getSetting "layout");
      self#defineInformationBox

    method addFinalNode x y node =
      self#operationAutomaton "add final node";
      if (Set.belongs node myFST#representation.states) then
        (JS.alertStr (Lang.i18nAlertExists ()))
      else (
        myFST <- myFST#addFinalNode node false false;
        Cytoscape.addNode self#getCy ~x:x ~y:y node false true;
        self#defineInformationBox;
      )

    method turnFinalNode node =
      self#operationAutomaton "make node final";
      if (Set.belongs node myFST#representation.acceptStates) then
          (JS.alertStr (Lang.i18nAlertAlreadyFinal ()))
      else
        (super#resetStyle;
         myFST <- (myFST#changeToFinal node);
         Cytoscape.turnFinal self#getCy node);
      self#defineInformationBox;
    
    method removeFinalNode node =
      self#operationAutomaton "make node not final";
      if (Set.belongs node myFST#representation.acceptStates) then
        (super#resetStyle;
         myFST <- (myFST#removeFinal node);
         Cytoscape.removeFinal self#getCy node)
      else
        (JS.alertStr (Lang.i18nAlertNonFinal ()));
      self#defineInformationBox;

    method eliminateNode node =
      self#operationAutomaton "eliminate node";
      let eliminateNodeTransitions (a, b, c, d) node = 
        if (a = node || d = node) then
          (myFST <- (myFST#eliminateTransition (a, b, c, d));
           self#defineInformationBox;) 
      in 
      if (node = myFST#representation.initialState) then 
        JS.alertStr (Lang.i18nAlertDelete ()) 
      else 
        if (Set.belongs node myFST#representation.states) then 
          (let isFinal = Set.belongs node myFST#representation.acceptStates in 
           super#resetStyle;
           Set.iter (fun el -> (eliminateNodeTransitions el node)) myFST#representation.transitions;
           myFST <- myFST#eliminateNode node false isFinal;
           Cytoscape.removeNode self#getCy node;
           self#defineInformationBox;)
        else 
          JS.alertStr (Lang.i18nAlertUnexistentState ())

    method renameState state =
      self#operationAutomaton "rename node";
      let newName = JS.prompt (Lang.i18nRenameStateQuestion()) state in
      match Js.Opt.to_option newName with
      | None -> ()
      | Some n -> super#resetStyle;
                  myFST <- myFST#renameState state (Js.to_string n);
                  Cytoscape.resetFaElems self#getCy;
                  self#defineExample

    method createTransition source target =
      let getSymb trans =
        let trimmed = String.trim trans in
        if Settings.isEpsilon trimmed then epsilon else symb trimmed
      in
      let getText sy =
        if sy = epsilon then StateVariables.returnEmpty () else symb2str sy
      in
      self#operationAutomaton "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransition ()) "a,b") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
        let str = Js.to_string v in
        let parts = String.split_on_char ',' str in
        match parts with
        | [input; output] ->
            let iSym = getSymb input in
            let oSym = getSymb output in
            myFST <- myFST#newTransition (source, iSym, oSym, target);
            super#resetStyle;
            Cytoscape.addEdge self#getCy (source, (getText iSym) ^ ":" ^ (getText oSym), target);
            self#defineInformationBox;
        | _ -> 
            JS.alertStr "Invalid format. Use 'input,output' (e.g., 'a,b')"

    method eliminateTransition (v1, label, v2) =
      let getSymb trans =
        let trimmed = String.trim trans in
        if Settings.isEpsilon trimmed then epsilon else symb trimmed
      in
      self#operationAutomaton "erase transition";
      let parts = String.split_on_char ':' label in
      match parts with
      | [input; output] -> 
          let iSym = getSymb input in
          let oSym = getSymb output in
          if (Set.belongs (v1, iSym, oSym, v2) myFST#representation.transitions) then
            (super#resetStyle;
             myFST <- (myFST#eliminateTransition(v1, iSym, oSym, v2));
             Cytoscape.removeEdge self#getCy v1 label v2;
             self#defineInformationBox;)
          else 
            JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ " does not exist")
      | _ -> JS.alertStr "Could not parse transition label to delete."

    method editModel = 
      !ListenersAutomaton.editModelListener();
      ()
      
    method replicateOnLeft =
      let c = new fstController self#getFST false in
      Ctrl.changeCtrl c false

    method defineMinimize listColors number =
      self#operationAutomaton "minimize";
      !Ctrl.ctrlL#getFST#paintMinimization !Ctrl.ctrlL#getCy listColors;
      myFST#drawMinimize self#getCy listColors number (Settings.getSetting "layout");
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt

    method convertToFA =
      let open FiniteAutomatonView in
      self#operationAutomaton "convert to FA";
      let fa = myFST#asFiniteAutomaton in
      new FiniteAutomatonView.model (Representation fa)

    method convertToTM_SingleTape =
      let open TuringMachineView in
      self#operationAutomaton "convert to TM single tape";
      let tm = myFST#asTuringMachine in
      new TuringMachineView.model (Representation tm)

    method getModelName =
      "fst"

    method printErrors =
          let errors = myFST#errors in
            if errors = [] then
              ()
            else
              JS.alertStr (String.concat "\n" errors)

    method getWords v =
      self#operationAutomaton "accepted words";
      let pairs = myFST#staticGenerateWithOutput v in
      let (_, visitedConfigs, exact, time) = myFST#returnStats in
        HtmlPageClient.putWordsWithOutput pairs;
        HtmlPageClient.displayGenStats visitedConfigs exact time

end
