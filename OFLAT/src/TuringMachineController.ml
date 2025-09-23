open OCamlFlat
open BasicTypes
open HtmlPageClient
open Lang
open Js_of_ocaml
open JS
open Controller
open AutomatonController
open AutomatonView
open TuringMachineView
open Listeners


  class tmController (tm: TuringMachineView.model) (s: bool)=
    object(self) inherit controller as super

    val mutable myTM = tm

    val side = s
    val cy = Some (if s then Cytoscape.initFaCy "cy2" else Cytoscape.initFaCy "cy")

    method operationTM opName : unit =
        super#operation opName "TM"

    method model: Model.model = 
      (myTM :> Model.model) 

    method resetStyle = 
      Cytoscape.resetStyle self#getCy Cytoscape.faStyle

    method getTM =
      myTM

    method changeAutomata res =
      myTM <- res

    method getModel = 
      myTM#toDisplayString "solution" 

    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (TuringMachine.kind)

    method returnType = TuringMachine.kind

    method startGraph = self#defineExample (*TODO Remove this call, call defineExample directly*)

    method defineExample =
      self#operationTM "create example";
      self#updateButtons;
      HtmlPageClient.putCyTMButtons ();
      HtmlPageClient.closeBoxRegex ();
      myTM#drawExample self#getCy;
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt;

    method defineExample2 = 
      myTM#drawExample self#getCy;
      self#defineInformationBox;

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      let deter = myTM#isDeterministic in 
        HtmlPageClient.getDeterminim deter infoBox;
      let useful = myTM#areAllStatesUseful in
      let uStates = myTM#getUselessStates in 
        HtmlPageClient.getHasUselessStates useful uStates infoBox;
      let nStates = myTM#numberStates in 
        HtmlPageClient.getNumberStates nStates infoBox;
      let nTransitions = myTM#numberTransitions in
        HtmlPageClient.getNumberTransitions nTransitions infoBox;
      let isLB = myTM#isLB in
        HtmlPageClient.getIsLinearBounded isLB infoBox;

    (* TODO *)
    (* - Para TM's, como e que posso pedir mais dados como simbolo escrever e a direcao,
       pedir tudo de uma vez ou pedir prompt a prompt?
       - necessario epsilon?
    *)

	  method updateButtons =
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons;
      if myTM#isDeterministic then begin
        List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRConvertButtons
      end



    method convertToGR =
      let open GrammarView in
      self#operationTM "convert to GR";
      let gr = PolyModel.tm2gr (myTM :> TuringMachine.model) in
      new GrammarView.model (Representation (gr#representation)) 

    method private checkForSimulation : bool = 
      if self#getTM#isSimulating then 
        (
          JS.confirm (Lang.i18nLeaveSimulationToEdit())
        )
      else 
        (
          true
        )
    
    method private resetAndRedraw =
      let cy = self#getCy in   
      Cytoscape.resetFaElems cy;
      myTM#drawExample cy;
      self#defineInformationBox
      
    method private drawNode x y node =
      self#defineInformationBox
    
    method editModel = 
      !ListenersTM.editModelListener();
    
    method replicateOnLeft =
      let c = new tmController self#getTM false in
        Ctrl.ctrlL := (c :> controller);

    method updateRight =
      if !Ctrl.ctrlR#getUpdateType = Some "specification"
      then !Listeners.showModelListener ()
    
    method printErrors =
      let errors = myTM#errors in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors);

(*     method accept =
      self#cancelProm;
      let rec tic n =
        match n with
        | true -> HtmlPageClient.changeButtonColor "autoAccept" "crimson";
                  Lwt.return()
        | false -> let prom = Lwt.bind 
                  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)
                  (fun () -> self#nextStep; tic myTM#isOver) in
                  HtmlPageClient.changeButtonColor "autoAccept" "green";
                  self#changeProm prom;
                  prom
      in
      ignore(tic (self#promState = Lwt.Sleep));
      Lwt.return_true *)

    method getNewSentence = 
      Js.string myTM#newSentence1

    method updateScreenSentence = 
      (Dom_html.getElementById "regExp")##.innerHTML := self#getNewSentence

    method changeToEditMode =
      self#getTM#changeToEditModelMode self#getCy;
      self#updateScreenSentence

    method startStep word =
      self#operationTM "accept start";
      self#cancelProm;
      HtmlPageClient.fitBoxRegex ();
(*       HtmlPageClient.enableButton "autoAccept";
 *)      
      myTM#changeTheTestingSentence word;
      myTM#startAccept self#getCy;
      self#updateScreenSentence

    method nextStep =
      self#operationTM "accept next";
      self#cancelProm;
      myTM#next self#getCy;
      self#updateScreenSentence

    method backStep = 
      self#operationTM "accept back";
      self#cancelProm;
      myTM#back self#getCy;
      self#updateScreenSentence

    method addNode x y initial final: unit = 
      self#operationTM "add Node";

      let result = self#checkForSimulation in
      if result then 
        (          
          let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
          match Js.Opt.to_option promptResult with
          | None -> ()
          | Some v -> 
              let node = (Js.to_string v) in
              if (myTM#hasState node) then 
                (
                  JS.alertStr (Lang.i18nAlertExists ())
                )
              else 
                ( 
                  self#changeToEditMode;

                  if (initial) then 
                    (
                      myTM <- myTM#addInitialNode node;

                      Cytoscape.resetFaElems self#getCy;
                      self#defineExample;
                      self#defineInformationBox
                    )
                  else 
                    (
                      if (final) then
                        (
                          myTM <- myTM#addFinalNode node
                        )
                      else 
                        (
                          myTM <- myTM#addNode node
                        );

                      Cytoscape.addNode self#getCy node ~x:x ~y:y false final;
                      self#defineInformationBox
                    )
                )
        )
      else ()

    method eliminateNode node =
      self#operationTM "eliminate node";

      let result = self#checkForSimulation in
      if result then 
        ( 
          if (not (myTM#hasState node)) then 
            (
              JS.alertStr (Lang.i18nAlertUnexistentState ())
            )
          else if (myTM#isInitial node) then 
            (
              JS.alertStr (Lang.i18nAlertDelete ()) 
            )
          else
            (
              self#changeToEditMode;

              let eliminateNodeTransitions (a, b, c) node = 
                if (a = node || c = node) then
                  ( 
                    self#eliminateTransition (a, b, c);
                  )
              in 

              myTM <- myTM#eliminateNode node;
              Set.iter (fun (a,b,c,d,e) -> 
				eliminateNodeTransitions (a, myTM#makeLabel b d e, c) node)
					myTM#representation.transitions;

              Cytoscape.removeNode self#getCy node;
              self#defineInformationBox
            )
        )
      else ()

    method turnNodeFinal node =
      self#operationTM "make node final";

      let result = self#checkForSimulation in
      if result then 
        ( 
          if (myTM#isFinal node) then
            (
              JS.alertStr (Lang.i18nAlertAlreadyFinal ())
            )
          else
            (
              self#changeToEditMode;

              myTM <- myTM#changeToFinal node;

              Cytoscape.turnFinal self#getCy node;
              self#defineInformationBox
            )
        )
      else()

    method turnNodeInitial node =
      self#operationTM "make node initial";

      let result = self#checkForSimulation in
      if result then 
        ( 
          if (myTM#isInitial node) then
            (
              JS.alertStr (Lang.i18nAlertAlreadyInitial ())
            )
          else
            (
              self#changeToEditMode;

              myTM <- myTM#addInitialNode node;

              Cytoscape.resetFaElems self#getCy;
              self#defineExample;
              self#defineInformationBox
            )
        )
      else()

    method removeFinalNode node =
      self#operationTM "make node not final";

      let result = self#checkForSimulation in
      if result then 
        ( 
          if (not (myTM#isFinal node)) then
            (
              JS.alertStr (Lang.i18nAlertNonFinal ())
            )
          else
            (
              self#changeToEditMode;

              myTM <- myTM#removeFinal node;

              Cytoscape.removeFinal self#getCy node;
              self#defineInformationBox
            )
        )
      else ()

    method renameState state =
      self#operationTM "rename node";
     
      let result = self#checkForSimulation in
      if result then 
        ( 
          let prompt = JS.prompt (Lang.i18nRenameStateQuestion()) state in
          match Js.Opt.to_option prompt with
          | None -> ()
          | Some n -> 
              let newName = Js.to_string n in
                if myTM#hasState newName then
                  (
                    JS.alertStr (Lang.i18nAlertExists ())
                  )
                else
                  (
                    self#changeToEditMode;

                    (* Make a function in cytoscape that reset a single node *)
                    myTM <- myTM#renameNode state newName;

                    Cytoscape.resetFaElems self#getCy;
                    self#defineExample;
                    self#defineInformationBox
                  )
        )
      else ()

    method createTransition source target =
      self#operationTM "add transition";

    let charIsDirection (dirC: char) : bool =
        List.mem dirC ['L'; 'S'; 'R'] in

    let char2direction (dirC: char) : direction =
        List.assoc dirC [('L',L);('S',S);('R',S)] in
        
     let result = self#checkForSimulation in
      if result then 
        (
          let promptResult = (JS.prompt (Lang.i18nTextEnterTransitionTM ()) "a/a/R") in
          match Js.Opt.to_option promptResult with
          | None -> ()
          | Some x ->
              let v = Js.to_string x in
                let (rdSymbol, wrtSymbol, dir) = myTM#dissectTransitionInput v in
                let newTrs = (source, [str2symb rdSymbol], target, [char2symb wrtSymbol], [char2direction dir]) in
                  if not (charIsDirection dir) then
                    (
                      (JS.alertStr (Lang.i18nAlertDirectionWrong ()))
                    )
                  else if (String.length v != 5 && (String.length v != 6 && (String.get v 0) != '~')) then
                    (
                      (JS.alertStr (Lang.i18nAlertExceededCharacters ()))
                    )
                  else if (myTM#hasTransition newTrs) then
                    (
                      (JS.alertStr (Lang.i18nAlertTransitionExists ()))
                    )
                  else
                    (
                      self#changeToEditMode;

                      myTM <- myTM#newTransition newTrs;

                      Cytoscape.addEdgeGeneral self#getCy (source, v, target);
                      self#defineInformationBox
                    )
        )
      else ()
                  

    method eliminateTransition (a, b, c) =
      self#operationTM "erase transition";

	  let string2direction = dirI in

      let result = self#checkForSimulation in
      if result then 
        (
          let (d,e,f) = match String.split_on_char '/' (b)  with
            | [d;e;f] -> (d,e,f)
            | _ -> Error.fatal "eliminate transition"
          in
          let trans = (a, [str2symb d], c, [str2symb e], [string2direction f]) in
          if (not (myTM#hasTransition trans)) then
            (
              JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ")" ^ (Lang.i18nAlertDoNotExists ()))
            )
          else 
            (
              self#changeToEditMode;

              myTM <- myTM#eliminateTransition trans;

              Cytoscape.removeEdge self#getCy a b c;
              self#defineInformationBox
            )
        )
      else ()

    method getWords v = 
      self#operationTM "accepted words";
      let var = myTM#generate v in 
        HtmlPageClient.putWords var

    (*JP*)
    method showTrace word =
      self#operationTM "trace";
      myTM#staticAcceptFull;
      let (accepted, configs, exact, time) = myTM#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;
      myTM#displayTrace;

    method checkWord word =
      self#operationTM "checkWord";
      myTM#changeTheTestingSentence word;
      myTM#staticAccept;
      let (accepted, configs, exact, time) = myTM#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;
      self#updateScreenSentence


  end

