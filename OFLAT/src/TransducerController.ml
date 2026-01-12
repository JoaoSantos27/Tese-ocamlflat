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

class fstController (fst : TransducerView.model) (s: bool) =
  object (self) inherit automatonController(s) as super

    val mutable myFST = fst

    method operationAutomaton opName : unit =
        super#operation opName "FST"

    method getAutomaton: AutomatonView.model =
      (myFST :> AutomatonView.model)

    method getFST = myFST (* Helper if needed, though strictly it's an FST *)

    method model: Model.model = 
      (myFST :> Model.model)

    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (Transducer.kind)

    method returnType = Transducer.kind

    method loadButtons = 
      HtmlPageClient.putCyTransducerButtons ()

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      let deter = myFST#isDeterministic in 
        HtmlPageClient.getDeterminim deter infoBox;
      let min = myFST#isMinimized in 
        HtmlPageClient.getMinimism min infoBox;
      let useful = myFST#areAllStatesUseful in
      let uStates = myFST#getUselessStates in 
        HtmlPageClient.getHasUselessStates useful uStates infoBox;
      let nStates = myFST#numberStates in 
        HtmlPageClient.getNumberStates nStates infoBox;
      let nTransitions = myFST#numberTransitions in
        HtmlPageClient.getNumberTransitions nTransitions infoBox;
      let _ = myFST#buildTable in () 

    (* --- Node Editing Methods --- *)

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
            (myFST <- myFST#addNode st false;
             Cytoscape.addNode self#getCy st ~x:x ~y:y initial final;
             self#defineInformationBox;)

    method addInitialNode node =
      self#operationAutomaton "make node initial";
      let stateExists = Set.belongs node myFST#representation.states in 
      myFST <- (myFST#addInitialNode node false stateExists);
      let cy = self#getCy in
      Cytoscape.resetFaElems cy;
      myFST#drawExample cy;
      self#defineInformationBox;

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
        (myFST <- (myFST#changeToFinal node);
        Cytoscape.turnFinal self#getCy node);
        self#defineInformationBox;
    
    method removeFinalNode node =
      self#operationAutomaton "make node not final";
      if (Set.belongs node myFST#representation.acceptStates) then
        (myFST <- (myFST#removeFinal node);
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
           myFST <- myFST#eliminateNode node false isFinal;
           Set.iter (fun el -> (eliminateNodeTransitions el node)) myFST#representation.transitions;
           Cytoscape.removeNode self#getCy node;
           self#defineInformationBox;)
        else 
          JS.alertStr (Lang.i18nAlertUnexistentState ())

    method renameState state =
      self#operationAutomaton "rename node";
      let newName = JS.prompt (Lang.i18nRenameStateQuestion()) state in
      match Js.Opt.to_option newName with
      | None -> ()
      | Some n -> myFST <- myFST#renameState state (Js.to_string n);
                  Cytoscape.resetFaElems self#getCy;
                  self#defineExample

    (* --- Transition Editing Methods --- *)

    method createTransition source target =
      self#operationAutomaton "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransition ()) "a,b") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
        let str = Js.to_string v in
        let parts = String.split_on_char ',' str in
        match parts with
        | [input; output] ->
            let iSym = symb input in
            let oSym = symb output in
            myFST <- myFST#newTransition (source, iSym, oSym, target);
            Cytoscape.addEdge self#getCy (source, (symb2str iSym) ^ ":" ^ (symb2str oSym), target);
            self#defineInformationBox;
        | _ -> 
            JS.alertStr "Invalid format. Use 'input,output' (e.g., 'a,b')"

    method eliminateTransition (v1, label, v2) =
      self#operationAutomaton "erase transition";
      (* Expect label format "input:output" *)
      let parts = String.split_on_char ':' label in
      match parts with
      | [input; output] -> 
          let iSym = symb input in
          let oSym = symb output in
          if (Set.belongs (v1, iSym, oSym, v2) myFST#representation.transitions) then
            (myFST <- (myFST#eliminateTransition(v1, iSym, oSym, v2));
             Cytoscape.removeEdge self#getCy v1 label v2;
             self#defineInformationBox;)
          else 
            JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ " does not exist")
      | _ -> JS.alertStr "Could not parse transition label to delete."

    (* --- Other Methods --- *)

    method editModel = 
      (* !ListenersFA.editModelListener(); *) (* You might need a specific Listener for Transducers *)
      ()

    method replicateOnLeft =
      let c = new fstController self#getFST false in
      Ctrl.ctrlL := (c :> controller);

    method defineMinimize listColors number =
      myFST#paintMinimization self#getCy listColors;
      myFST#drawMinimize self#getCy listColors number;
      Cytoscape.fit self#getCy_opt

    method printErrors =
          let errors = myFST#errors in
            if errors = [] then
              ()
            else
              JS.alertStr (String.concat "\n" errors)

end

