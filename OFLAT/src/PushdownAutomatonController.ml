open OCamlFlat
open BasicTypes
open HtmlPageClient
open Lang
open Js_of_ocaml
open JS
open Controller
open AutomatonController
open AutomatonView
open PushdownAutomatonView
open Listeners

class pdaController (pda: PushdownAutomatonView.model) (s: bool)=
  object(self) inherit automatonController(s) as super

    val mutable myPDA = pda

    method operationAutomaton opName : unit =
        super#operation opName "PDA"

    method model: Model.model = 
      (myPDA :> Model.model)

    method getAutomaton: AutomatonView.model =
      (myPDA :> AutomatonView.model)

    method getPDA =
      myPDA

    method changeAutomata res =
      myPDA <- res

    method getModel = 
      myPDA#toDisplayString "solution"

    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (PushdownAutomaton.kind)

    method returnType = PushdownAutomaton.kind

    method loadButtons = 
      HtmlPageClient.putCyAutomataPDAButtons ()

    method updateButtons =
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons

    method defineInformationBox = 
      let infoBox = HtmlPageClient.defineInformationBox side in
      let deter = myPDA#isDeterministic in 
        HtmlPageClient.getDeterminim deter infoBox;
      let useful = myPDA#areAllStatesUseful in
      let uStates = myPDA#getUselessStates in 
        HtmlPageClient.getHasUselessStates useful uStates infoBox;
      let isEquivalentFA = myPDA#isFiniteAutomaton in
        HtmlPageClient.getIsEquivalentFA isEquivalentFA infoBox;
      let nStates = myPDA#numberStates in 
        HtmlPageClient.getNumberStates nStates infoBox;
      let nTransitions = myPDA#numberTransitions in
        HtmlPageClient.getNumberTransitions nTransitions infoBox;
      let _ = myPDA#buildTable in () (*UPDATE TABLE*)

    method toggleAcceptanceCriteria =
      self#operationAutomaton "toggle acceptance criteria";
      self#changeToEditModelMode;
      myPDA <- myPDA#toggleAcceptanceCriteria;
      let toggleButton = Dom_html.getElementById "toggleAcceptanceCriteria" in
        toggleButton##.innerHTML := Js.string (
          if myPDA#representation.criteria then
            Lang.i18nTogleAcceptCriteriaState ()
          else
            Lang.i18nTogleAcceptCriteriaEmptyStack ()
        );
      self#defineInformationBox

    method changeInitialStackSymbol =
      self#operationAutomaton "change initial stack symbol";
      self#changeToEditModelMode;
      let promptResult = (JS.prompt (Lang.i18nChangeInitialStackSymbolPda ()) "z") in (*TODO need to verify if validInput, (no spaces)*)
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
          let initStackSymbolString = (Js.to_string v) in
          if initStackSymbolString <> "" then 
            begin
              let initStackSymbol = List.hd (String.split_on_char ' ' initStackSymbolString) in
              myPDA <- myPDA#changeInitialStackSymbol (symb initStackSymbol);
              let button = Dom_html.getElementById "buttonInitialStackSymbol" in
              button##.innerHTML := Js.string ((Lang.i18nInitialStackSymbol ())^(initStackSymbol))
            end else ();
      self#defineInformationBox

    method convertAcceptStates =
      if not myPDA#getCriteria then 
        let convertedPDA = myPDA#transformPdaToAcceptStates in
        Some (new PushdownAutomatonView.model (Representation convertedPDA#representation))
      else None
          
    method convertEmptyStackAccept =
      if myPDA#getCriteria then 
        let convertedPDA = myPDA#transformPdaToAcceptEmptyStack in
        Some (new PushdownAutomatonView.model (Representation convertedPDA#representation))
      else None
      
(* imitar TM *)
 (*   method addNode x y st : unit = 
      self#operationAutomaton "add Node";
      if (Set.belongs st myPDA#representation.states) then 
        JS.alertStr (Lang.i18nAlertExists ())
      else 
        begin
          myPDA <- myPDA#addState st;
          Cytoscape.addNode self#getCy st ~x:x ~y:y false false
        end;
      self#defineInformationBox *)

	  method addNode x y initial final: unit = 
      self#operationAutomaton "add Node";
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> let st = (Js.to_string v) in
                  JS.log myPDA#representation.states;
                  if (Set.belongs st myPDA#representation.states) then 
						JS.alertStr (Lang.i18nAlertExists ())
				  else 
					begin
					  myPDA <- myPDA#addState st;
					  Cytoscape.addNode self#getCy st ~x:x ~y:y false false
					end;
				  self#defineInformationBox

    method eliminateNode node =
      self#operationAutomaton "eliminate node";
      let removeNodeTransitions node (fromNode,st,is,toNode,ts) = 
        if (fromNode = node || toNode = node) then
          (myPDA <- myPDA#removeTransition (fromNode,st,is,toNode,ts)) 
      in 
        if (node = myPDA#representation.initialState) then 
          JS.alertStr (Lang.i18nAlertDelete ()) 
        else 
          (myPDA <- myPDA#removeState node;
          Set.iter (removeNodeTransitions node) myPDA#representation.transitions;
          Cytoscape.removeNode self#getCy node);
      self#defineInformationBox

(* imitar TM *)
    method addInitialNode node =
      self#operationAutomaton "make node initial";
      myPDA <- myPDA#addState node;
      myPDA <- myPDA#updateInitialState node;
      let cy = self#getCy in
      Cytoscape.resetFaElems cy;
      myPDA#drawExample cy;  
      self#defineInformationBox     
      
(* imitar TM *)
    method addFinalNode x y node =
      self#operationAutomaton "add final node";
      myPDA <- myPDA#addAcceptState node;
      Cytoscape.addNode self#getCy ~x:x ~y:y node false true;
      self#defineInformationBox
      
    method turnFinalNode node =
      self#operationAutomaton "make node final";
      myPDA <- myPDA#addAcceptState node;
      Cytoscape.turnFinal self#getCy node;
      self#defineInformationBox
    
    method removeFinalNode node =
      self#operationAutomaton "make node not final";
      myPDA <- myPDA#removeAcceptState node;
      Cytoscape.removeFinal self#getCy node;
      self#defineInformationBox

    method createTransition source target = (*need to check if transition it is already created*)
      self#operationAutomaton "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransitionPda ()) "a : z / ~") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
        if (PushdownAutomatonView.isInputValid (Js.to_string v)) then
          begin 
            let (topStack, inputSymb, toPutInStack) = PushdownAutomatonView.parseUserInput (Js.to_string v) in
            let transition = (source, topStack, inputSymb, target, toPutInStack) in
            myPDA <- myPDA#addTransition transition;
            Cytoscape.addEdge self#getCy (PushdownAutomatonView.transitionPda2CytoscapeEdge transition)
          end;
      self#defineInformationBox

    method eliminateTransition (source, label, target) =
      let open Re in
      self#operationAutomaton "erase transition";
      (if (PushdownAutomatonView.isInputValid label) then
        (let (topStack, inputSymb, toPutInStack) = PushdownAutomatonView.parseUserInput label in
        let transition = (source, topStack, inputSymb, target, toPutInStack) in
        if (Set.belongs transition myPDA#representation.transitions) then
          begin
            myPDA <- (myPDA#removeTransition transition);
            Cytoscape.removeEdge self#getCy source label target
          end
        else 
          JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ "(" ^ source ^ ", " ^ label ^ ", " ^ target ^ ")" ^ (Lang.i18nAlertDoNotExists ()))));
      self#defineInformationBox
      
    method renameState node =
      self#operationAutomaton "rename node";
      let newName = JS.prompt (Lang.i18nRenameStateQuestion()) node in
      match Js.Opt.to_option newName with
      | None -> ()
      | Some n -> 
        let stateName = String.trim (Js.to_string n) in
        if (PushdownAutomatonView.isStateNameValid stateName) then
          begin
            myPDA <- myPDA#renameState node (state stateName);
            Cytoscape.resetFaElems self#getCy;
            self#defineExample2
          end

    method replicateOnLeft =
      let c = new pdaController self#getPDA false in
      Ctrl.ctrlL := (c :> controller);
     
    method convertToFA = 
      let open FiniteAutomatonView in
      self#operationAutomaton "convert to FA";
      let fa = PolyModel.pda2fa (myPDA :> PushdownAutomaton.model) in
      new FiniteAutomatonView.model (Representation (fa#representation))

    method convertToCFG = 
      let open ContextFreeGrammarView in
      let cfg = PolyModel.pda2cfg (myPDA :> PushdownAutomaton.model) in
      new ContextFreeGrammarView.model (Representation (cfg#representation))


    method convertToTM_SingleTape = 
      let open TuringMachineView in
      self#operationAutomaton "convert to TM single tape";
      let tm = PolyModel.pda2tm (myPDA :> PushdownAutomaton.model) in
      new TuringMachineView.model (Representation (tm#representation))

    method convertToTM_DualTape = 
      let open TuringMachineView in
      self#operationAutomaton "convert to TM dual tape";
      let tm = PolyModel.pda2tm_2tapes (myPDA :> PushdownAutomaton.model) in
      new TuringMachineView.model (Representation (tm#representation))

    method convertToRegExp =
      let open RegularExpressionView in
      self#operationAutomaton "convert to RE";
      let reg = PolyModel.pda2re (myPDA :> PushdownAutomaton.model) in
      let r = reg#simplify in 
      let rep = r#representation in 
      new RegularExpressionView.model (Representation (rep))
end
