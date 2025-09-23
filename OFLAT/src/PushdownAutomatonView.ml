(*
 * FiniteAutomatonView.ml
 *
 * This file is part of the OFLAT app
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Carlos Freitas
 *)

open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open ViewUtil
open Lang
open Cytoscape
open AutomatonView

module PushdownAutomatonView =
struct
	open PushdownAutomaton

  (** Auxiliar Methods **)

  let transitionPda2CytoscapeEdge (state1, symbStack, symbInput, state2, topStack) =
    let topStackString = if topStack = [] then "~" else String.concat "" (List.map (fun s -> symb2str s) topStack) in
      let edgeLabel = (symb2str symbInput) ^ " : " ^ (symb2str symbStack) ^ " / " ^ topStackString in
        (state1, edgeLabel, state2)

  let symbInputRegex = " *[A-Za-z0-9~]+ *"

  let htmlDelta = "ẟ"

  let isStateNameValid input =
    let open Re in
    Str.string_match (Str.regexp symbInputRegex) input 0

  let isInputValid input =
    let open Re in
    Str.string_match (Str.regexp (symbInputRegex^":"^symbInputRegex^"/"^symbInputRegex)) input 0

  let processToPutInStackInput toPutInStackString =
    let open Re in
    let processedString = 
      if toPutInStackString <> "~" then Str.global_replace (Str.regexp "~") "" toPutInStackString 
      else toPutInStackString 
    in
      if processedString = "~" then [] 
      else List.map char2symb (List.init (String.length processedString) (String.get processedString))

  let parseUserInput input = 
    let open Re in
    let open Lwt in
    let trimmedStr = Str.global_replace (Str.regexp " ") "" input in
    let splittedInput = Str.split (Str.regexp "[:/]")  trimmedStr in
    let topStack = symb (List.nth splittedInput 1) in
    let inputSymb = symb (List.nth splittedInput 0) in
    let toPutInStack = processToPutInStackInput (List.nth splittedInput 2) in
      (topStack, inputSymb, toPutInStack)

module FILTERI =
struct
	let rec genNX n =
		if n = 0 then [] else (n-1)::genNX (n-1)
	;;
	let rec genN n =
		List.rev (genNX n)
	;;
	let makeIndex l =
		let idx = genN (List.length l) in
			List.combine idx l
	;;
	let rec filteriX f l =
		match l with
		| [] ->
			[]
		| (i,v)::xs ->
			(if f i v then [v] else [])@filteriX f xs
	;;	
	let rec filteri f l =
		let idx = makeIndex l in
			filteriX f idx
	;;
end

  let buildStackDisplayString stack = 
    let maxSymbDisplayed = 15 in
    let (restrictedStack, wasRestricted) = 
      if List.length stack > maxSymbDisplayed then (FILTERI.filteri (fun i _ -> i < maxSymbDisplayed) stack, true) 
      else (stack, false)
    in
    let stringStack = List.fold_left (fun acc s -> acc^"\n"^(symb2str s)) "Stack content:\n" restrictedStack in
    if wasRestricted then stringStack^"\n..." else stringStack
  
  let buildConfigMenu ((menuID, stack)) =
    let stackDisplay = buildStackDisplayString stack in
    Js.def (object%js
      val id = Js.string menuID
      val content = Js.string stackDisplay
      val selector = Js.string "node"
      val show = Js.bool false
      val disabled = Js.bool true
      val onClickFunction = fun element -> ()
    end)

  let buildIdFromState state (suffix: int) =
    (state2str state)^"_"^(string_of_int suffix)

  let getConfigsWithState state configs =
    Set.filter (fun (st, _, _) -> st = state) configs

  let processNodeConfig (configs: PushdownAutomaton.configurations_) state =
    let configsOfState = getConfigsWithState state configs in
    Set.mapi (fun i (state, stack, _) -> buildConfigMenu (buildIdFromState state i, stack)) configsOfState

  let processConfigMenus (configs:PushdownAutomaton.configurations_) =
    let states = PushdownAutomaton.configuration_Get1 configs in
    Set.flatMap (processNodeConfig configs) states

  let menuConfigPDA (configs: PushdownAutomaton.configurations_) = 
    Js.Unsafe.coerce @@ object%js
      val evtType = Js.string "tapdragover"
      val menuItems = Js.array (Array.of_list (Set.toList (processConfigMenus configs)))
    end 

  let buildIdsStateAndApplyF f node configs: unit =
    let configsOfState = getConfigsWithState (state node) configs in
    Set.iteri (fun idSuffix (st, _, _) -> f (buildIdFromState st idSuffix)) configsOfState

  let hideMenu menu id =
    menu##hideMenuItem (Js.string id)
  
  let showMenu menu id =
    menu##showMenuItem (Js.string id)

  let hideMenus menu configs node =
    buildIdsStateAndApplyF (hideMenu menu) node configs
    
  let showMenus menu configs node =
    buildIdsStateAndApplyF (showMenu menu) node configs 

  let hideAllConfigMenus menu configs =
    let states = PushdownAutomaton.configuration_Get1 configs in
      Set.iter (hideMenus menu configs) states
  
  let __none__ = "__none__"

  let optionsPopper = 
    Js.def (object%js 
      val placement = Js.string "right-end"
    end)

  let _popperDiv_ = "popper-div"
  let _displayConfig_ = "display-config"
  let _displayConfigClass_ = "display-config-class"
  let _stackConfigDisplayID_ = "stackConfigDisplay"
  let _nodeConfigDisplayID_ = "nodeConfigDisplay"

  (* let renderedPos target = 
    JS.log(target##.renderedPosition##x);
    let (x, y) = Cytoscape.getTargetPos target in
    Js.def (object%js 
      val x = 100
      val y = 500
    end)

  let renderedDim target = 
    () *)

  let buildPopper nConfigs (node: Cytoscape.DataItem.t Js_of_ocaml.Js.t) =
    node##popper (
      Js.Unsafe.coerce @@ object%js
      (* val renderedPosition = renderedPos
      val renderedDimensions = renderedDim *)
        val content = fun () -> 
          let countConfigsDiv = Dom_html.document##createElement (Js.string "div") in
            (countConfigsDiv##.classList)##add(Js.string _popperDiv_);
            countConfigsDiv##.innerHTML := Js.string (string_of_int nConfigs);
            Dom.appendChild (Dom_html.getElementById "cy") countConfigsDiv;
              countConfigsDiv
        val popper = optionsPopper
      end
    )

  let getConfigCountForNode node configs: int =
    Set.fold_left (fun c (st, _, _) -> if (state2str st) = (data_fromName node "id") then c+1 else c) 0 configs

  let buildConfigsCount (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) configs = 
    let nodes = List.filter (fun node -> (data_fromName node "id") <> "transparent") (Cytoscape.getAllNodes cy) in
      List.map (fun node -> buildPopper (getConfigCountForNode node configs) node) nodes

  let updateAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##update ()) poppers
  
  let destroyAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##destroy ()) poppers

  let isFinal configs finalStates criteria state =
    let configsOfState = getConfigsWithState state configs in
    let stateStack = Set.map (fun (a, b, _) -> (a, b)) configsOfState in
      PushdownAutomaton.isInAcceptState stateStack finalStates criteria

  let buildStackForTable stack =
    let stackStr = List.map symb2str stack in
      String.concat " " stackStr

  let buildTableConfigDisplay = 
    let table = Dom_html.document##createElement (Js.string "table") in
    (table##.classList)##add(Js.string _displayConfigClass_);

    let tHead = Dom_html.document##createElement (Js.string "tHead") in
    let trHead = Dom_html.document##createElement (Js.string "tr") in
    let thNode = Dom_html.document##createElement (Js.string "th") in
    let thStack = Dom_html.document##createElement (Js.string "th") in
    thNode##.innerHTML := Js.string ("Node");
    thStack##.innerHTML := Js.string ("Stack");
    Dom.appendChild tHead trHead;
    Dom.appendChild trHead thNode;
    Dom.appendChild trHead thStack;

    let tBody = Dom_html.document##createElement (Js.string "tBody") in
    let trBody = Dom_html.document##createElement (Js.string "tr") in
    let tdNode = Dom_html.document##createElement (Js.string "td") in
    let tdStack = Dom_html.document##createElement (Js.string "td") in
    tdNode##.id := Js.string (_nodeConfigDisplayID_);
    tdStack##.id := Js.string (_stackConfigDisplayID_);
    Dom.appendChild tBody trBody;
    Dom.appendChild trBody tdNode;
    Dom.appendChild trBody tdStack;
    
    Dom.appendChild table tHead;
    Dom.appendChild table tBody;
      table

  let buildPopperConfigDisplay (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
    cy##popper (
      Js.Unsafe.coerce @@ object%js
        val content = fun () -> 
          let configDisplay = buildTableConfigDisplay in
          configDisplay##.id := Js.string (_displayConfig_);
          Dom.appendChild (Dom_html.getElementById "cy") configDisplay;
            configDisplay
        val renderedPosition = fun () ->
          Js.def (object%js
            val x = 50
            val y = 500
          end)
      end
    ) 

  let updateconfigDisplay (config: PushdownAutomaton.configuration_ option) =
    let nodeDisplay = (Dom_html.getElementById _nodeConfigDisplayID_) in
    let stackDisplay = (Dom_html.getElementById _stackConfigDisplayID_) in
    let (nodeString, stackString) = 
      match config with
      | None -> ("--", Lang.i18nNoPathAvailablePDA ())
      | Some (state, stack, _) -> (state2str state, buildStackForTable stack) 
    in
      nodeDisplay##.innerHTML := Js.string nodeString;
      stackDisplay##.innerHTML := Js.string stackString

  let buildModelSpecsMenuDom initialStackSymbol criteria =
    let specsMenu = Dom_html.document##createElement (Js.string "div") in
    
    let initialStackSymbDiv = Dom_html.document##createElement (Js.string "div") in
    let issTitle = Dom_html.document##createElement (Js.string "h3") in
    let issInput = Dom_html.document##createElement (Js.string "input") in
    issTitle##.innerHTML := Js.string ("Initial stack symbol:");
    issInput##.innerHTML := Js.string (symb2str initialStackSymbol);
    Dom.appendChild initialStackSymbDiv issTitle;
    Dom.appendChild initialStackSymbDiv issInput;

    let citeriaDiv = Dom_html.document##createElement (Js.string "div") in
    let cTitle = Dom_html.document##createElement (Js.string "h3") in
    let cButton = Dom_html.document##createElement (Js.string "button") in
    cTitle##.innerHTML := Js.string ("Acceptance mode:");
    cButton##.innerHTML := Js.string (if criteria then Lang.i18nTogleAcceptCriteriaState () else Lang.i18nTogleAcceptCriteriaEmptyStack ());
    Dom.appendChild citeriaDiv cTitle;
    Dom.appendChild citeriaDiv cButton;

    Dom.appendChild specsMenu initialStackSymbDiv;
    Dom.appendChild specsMenu citeriaDiv;
      specsMenu

  let bestStateColor = "DarkBlue"

  class model (arg: t Arg.alternatives) =
		object(self) 
    inherit AutomatonView.model arg as abstractAutomaton
    inherit PushdownAutomaton.model arg as super

    val mutable specsMenu: Cytoscape.popper Js_of_ocaml.Js.t option = None
    val mutable bestPath: PushdownAutomatonJoao.path_ = []
    val mutable configMenu: contextMenus Js.t option = None
    val mutable selectedNodeConfigMenu = __none__
    val mutable configsCounter: Cytoscape.popper Js_of_ocaml.Js.t list = []
    val mutable configDisplay: Cytoscape.popper Js_of_ocaml.Js.t option = None
    val mutable accepted : bool = false
    val mutable visitedConfigs : int = 0
    val mutable exactResult : bool = false
    val mutable acceptTime : float = 0.0


    method inputEdges cy = 
      let addEdgesCytoscape transition = Cytoscape.addEdge cy (transitionPda2CytoscapeEdge transition) in
        Set.iter addEdgesCytoscape self#representation.transitions

    method cleanUselessStates1 (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) : model =
      Cytoscape.resetStyle cy Cytoscape.faStyle;
      let uselessStates = self#getUselessStates in
        Set.iter (fun s -> Cytoscape.paintNode cy s AutomatonView.usefulColor) uselessStates;
      let clean = super#cleanUselessStates in
      let rep = clean#representation in 
      new model (Arg.Representation rep) 

    method reachableFromInitialState: states = self#reachable self#representation.initialState
    
    method toggleAcceptanceCriteria =
      let rep: t = self#representation in 
        new model (Arg.Representation  {
          inputAlphabet = rep.inputAlphabet;
          stackAlphabet = rep.stackAlphabet;
          states = rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = rep.transitions;
          acceptStates = rep.acceptStates;
          criteria = not rep.criteria
        })

  method changeInitialStackSymbol symb =
    let rep: t = self#representation in 
      new model (Arg.Representation  {
        inputAlphabet = rep.inputAlphabet;
        stackAlphabet = rep.stackAlphabet;
        states = rep.states;
        initialState = rep.initialState;
        initialStackSymbol = symb;
        transitions = rep.transitions;
        acceptStates = rep.acceptStates;
        criteria = rep.criteria
      })

    method addState state =
      let rep: t = self#representation in 
        new model (Arg.Representation {
          inputAlphabet = rep.inputAlphabet;
          stackAlphabet = rep.stackAlphabet;
          states = Set.add state rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = rep.transitions;
          acceptStates = rep.acceptStates;
          criteria = rep.criteria
        })

    method removeState state = 
      let rep: t = self#representation in 
        new model (Arg.Representation {
          inputAlphabet = rep.inputAlphabet;
          stackAlphabet = rep.stackAlphabet;
          states = Set.remove state rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = rep.transitions;
          acceptStates = Set.remove state rep.acceptStates;
          criteria = rep.criteria
        })

    method updateInitialState state =
      let rep: t = self#representation in 
      new model (Arg.Representation {
        inputAlphabet = rep.inputAlphabet;
        stackAlphabet = rep.stackAlphabet;
        states = rep.states;
        initialState = state;
        initialStackSymbol = rep.initialStackSymbol;
        transitions = rep.transitions;
        acceptStates = rep.acceptStates;
        criteria = rep.criteria
      })

     method addAcceptState state = 
      let rep: t = self#representation in 
        new model (Arg.Representation {
          inputAlphabet = rep.inputAlphabet;
          stackAlphabet = rep.stackAlphabet;
          states = Set.add state rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = rep.transitions;
          acceptStates = Set.add state rep.acceptStates;
          criteria = rep.criteria
      })

    method removeAcceptState state =
      let rep: t = self#representation in 
        new model (Arg.Representation {
          inputAlphabet = rep.inputAlphabet;
          stackAlphabet = rep.stackAlphabet;
          states = rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = rep.transitions;
          acceptStates = Set.remove state rep.acceptStates;
          criteria = rep.criteria
        })

    method addTransition (fromState, topStack, symbol, toState, putInStack) = 
      let rep: t = self#representation in       
        new model (Arg.Representation {
          inputAlphabet = if symbol <> epsilon then Set.add symbol rep.inputAlphabet else rep.inputAlphabet;
          stackAlphabet = Set.union (Set.make (topStack::putInStack)) rep.stackAlphabet;
          states = rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = Set.add (fromState, topStack, symbol, toState, putInStack) rep.transitions;
          acceptStates = rep.acceptStates;
          criteria = rep.criteria
        })

    method removeTransition (fromState, topStack, symbol, toState, putInStack) = 
      let rep: t = self#representation in 
      let newTransitions = Set.remove (fromState, topStack, symbol, toState, putInStack) rep.transitions in
      let newStackAlphabet = 
        let stackAlphabetTrns = Set.union (PushdownAutomaton.transitionGet2 newTransitions) (PushdownAutomaton.transitionGet5Flat newTransitions) in
          Set.add rep.initialStackSymbol stackAlphabetTrns
      in
      new model (Arg.Representation {
          inputAlphabet = Set.remove epsilon (PushdownAutomaton.transitionGet3 newTransitions);
          stackAlphabet = newStackAlphabet;
          states = rep.states;
          initialState = rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = newTransitions;
          acceptStates = rep.acceptStates;
          criteria = rep.criteria
    }) 

    method renameState oldState newState =
      let rep: t = self#representation in 
      let renameState (state: state) = if state = oldState then newState else state in
      let renameStates (states: states) = Set.map renameState states in
      let renameTransition ((fromState, topStack, symbol, toState, putInStack): transition) = 
        (renameState fromState, topStack, symbol, renameState toState, putInStack) in
      let renameTransitions (transitions: transitions) = Set.map renameTransition transitions in

      new model (Arg.Representation {
          inputAlphabet = rep.inputAlphabet;
          stackAlphabet = rep.stackAlphabet;
          states = renameStates rep.states;
          initialState = renameState rep.initialState;
          initialStackSymbol = rep.initialStackSymbol;
          transitions = renameTransitions rep.transitions;
          acceptStates = renameStates rep.acceptStates;
          criteria = rep.criteria
    })

    method getCriteria = self#representation.criteria

    method private paintBestCurrentStep cy =
      match self#getCurrConfigFromBestPath with
      | None -> ()
      | Some (currBestState, _, _) -> if position <> (List.length bestPath)-1 then Cytoscape.paintNode cy currBestState bestStateColor else ()
  
    method paintCurrentStates cy =
      let states = PushdownAutomaton.configuration_Get1 steps.(position) in
      self#paintStates cy states (isFinal steps.(position) self#getAcceptStates self#representation.criteria);
      self#paintBestCurrentStep cy

    method getAcceptStates = self#representation.acceptStates
    method getInitialState = self#representation.initialState
    method getStates = self#representation.states

    method staticGenerate len =
      let res = self#generate len in
      let (exact, configVisited, time) = Model.stats() in
      exactResult <- exact;
      acceptTime <- time;
      visitedConfigs <- configVisited;
      res
      

    method staticAccept =
      let acc = self#accept (List.map char2symb !sentence) in
      let (exact, configVisited, time) = Model.stats() in
      JS.log("set initial step");
      self#setConfigsAndBestPath2 acc exact time configVisited;

      method staticAcceptFull =
        let (acc, bestPath, trail) = self#acceptFull (List.map char2symb !sentence) in
        let (exact, configVisited, time) = Model.stats() in
        JS.log("set initial step");
        self#setConfigsAndBestPath trail bestPath acc exact time configVisited;

    method setInitialStep cy =
      self#staticAcceptFull;
      self#initAllMenusAndFeatures cy steps.(0)

    method setNextStep cy =
      self#updateAllMenusAndFeatures cy steps.(position)

    method setBackStep (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
      self#updateAllMenusAndFeatures cy steps.(position)
      
    method resetToEditModel = 
      self#destroyConfigDisplay;
      self#resetConfigMenu;
      self#destroyAllPoppers

    method numberStates: int =
      Set.size self#representation.states

    method numberTransitions: int =
      Set.size self#representation.transitions

    method getWordFromConfig config : word =
      let (state, stack, word) = config in
      word
      
    method private initAllMenusAndFeatures cy configs = 
      self#buildConfigDisplay cy configs;
      self#buildPoppersConfigsCounter cy configs;
      self#subscribeNodesPositionUpdate cy;
      self#updateConfigMenu cy configs;
      self#subscribeConfigEventMenu cy

    method private setConfigsAndBestPath trail bestPathAutomaton acc exact time configVisited=
      steps <- Array.of_list trail;
      JS.log("METI OS STEPS");
        bestPath <- bestPathAutomaton;
        accepted <- acc;
        exactResult <- exact;
        acceptTime <- time;
        visitedConfigs <- configVisited

        method private setConfigsAndBestPath2 acc exact time configVisited=
          accepted <- acc;
          exactResult <- exact;
          acceptTime <- time;
          visitedConfigs <- configVisited    

    method private updateAllMenusAndFeatures cy configs =
      self#updateConfigDisplay configs;
      self#buildPoppersConfigsCounter cy configs;
      self#updateConfigMenu cy configs

    method private buildConfigDisplay cy configs = (*latter is not configs but the best path*)
      (match configDisplay with
      | None -> configDisplay <- Some (buildPopperConfigDisplay cy)
      | Some _ -> ());
      self#updateConfigDisplay configs

    method private getCurrConfigFromBestPath = 
      if bestPath <> [] then Some (List.nth bestPath position) else None

    method private updateConfigDisplay configs = 
      let config = 
        if self#isDeterministic then (if Set.isEmpty configs then None else Some (Set.hd configs))
        else self#getCurrConfigFromBestPath
      in
        updateconfigDisplay config
    
    method private destroyConfigDisplay =
      match configDisplay with
      | None -> ()
      | Some s -> 
          s##destroy ();
          configDisplay <- None;
          let cy = Dom_html.getElementById "cy" in
          let configDisplayElem = Dom_html.getElementById _displayConfig_ in
          Dom.removeChild cy configDisplayElem

    method private updateConfigMenu cy configs =
      self#resetConfigMenu;
      let cm = cy##contextMenus(menuConfigPDA configs) in
		  configMenu <- Some cm;
		  hideAllConfigMenus cm configs

    method private resetConfigMenu =
      match configMenu with
      | None -> ()
      | Some menu -> 
          menu##destroy();
          configMenu <- None;
          selectedNodeConfigMenu <- __none__
    
    method private showConfigurationMenu node =
      if selectedNodeConfigMenu <> node then 
        match configMenu with
          | None -> ()
          | Some menu ->
            let configs = steps.(position) in
            let selectedNode = selectedNodeConfigMenu in
              if selectedNode <> __none__ then 
                begin
                  hideMenus menu configs selectedNode;
                  selectedNodeConfigMenu <- __none__
                end;
              if Set.belongs (state node) (PushdownAutomaton.configuration_Get1 configs) then
                begin 
                  showMenus menu configs node;
                  selectedNodeConfigMenu <- node;
                end
              
    method private subscribeConfigEventMenu (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t): unit =
      cy##on (Js.string "mouseover") (Js.string "node")
      (fun evt -> 
          match Js.Opt.to_option (evt##.target) with 
          | None -> JS.log("Error, none node selected")
          | Some t ->
            let target = Js.Unsafe.coerce t in
              self#showConfigurationMenu (Js.to_string target##data##.id)
      )

    method private destroyPopperDivs =
      let cyDiv = Dom_html.getElementById "cy" in
      let elems = Dom_html.document##getElementsByClassName (Js.string _popperDiv_) in
      for _ = 0 to elems##.length - 1 do
        let elem = elems##item 0 in
        match Js.Opt.to_option elem with
          | None -> ()
          | Some r ->
          		let r = Js.Unsafe.coerce r in
          			Dom.removeChild cyDiv r
      done

    method private destroyAllPoppers =
      destroyAllPoppers configsCounter;
      self#destroyPopperDivs;
      configsCounter <- []            

    method private buildPoppersConfigsCounter cy configs =
      self#destroyAllPoppers;
      configsCounter <- buildConfigsCount cy configs
      
    method private updateAllPoppers =
      updateAllPoppers configsCounter

    method private subscribeNodesPositionUpdate cy =
      cy##on (Js.string "position") (Js.string "node")
      (fun _ -> self#updateAllPoppers);

      cy##on_3 (Js.string "pan zoom resize")
      (fun _ -> self#updateAllPoppers);
    


    (*JP*)

    method buildTable =

      let makeStateContents st stckAlp inpAlp trans =
          let rec stateRows states stack =
          match states with
          | [] -> []
          | a :: b ->  
              let rec stateStackRow state stack =
              match stack with
              | [] -> []
              | sa :: [] -> HTMLTable.makePDAStateRow state sa inpAlp trans :: stateRows b stckAlp
              | sa :: sb -> HTMLTable.makePDAStateRow state sa inpAlp trans :: stateStackRow state sb
              in
              stateStackRow a stckAlp
          in
          let result = stateRows st stckAlp in
          result
      in
      let makePDATable () : string list list =
          let inputAlphabet = (Set.toList self#representation.inputAlphabet) in
          let states = Set.toList self#representation.states in
          let trans = self#representation.transitions in
          let stackAlphabet = Set.toList self#representation.stackAlphabet in
          let headers = List.map symb2str inputAlphabet in 
          let contents = makeStateContents states stackAlphabet inputAlphabet trans in
          (htmlDelta :: headers) :: contents
      in
      if not (HTMLTable.tableExists "automataTable") then (
      let contents = makePDATable () in
      HTMLTable.buildTable contents "automataTable" "tab"
      )
      else (
        let parent = Dom_html.getElementById "tab" in
        parent##.innerHTML := Js.string "";
        let contents = makePDATable () in
        HTMLTable.buildTable contents "automataTable" "tab"
      )


    method returnStats =
      (accepted, visitedConfigs, exactResult, acceptTime)

    method displayTrace =
      let makePath () : string list list =
        let headers = ["Trace"; "Current State"; "Stack"; "Word To Consume"] in
          headers :: HTMLTable.makePDAPath bestPath 0
      in
      if not (HTMLTable.tableExists "pathTable") then (
        let contents = makePath () in
      (*expected string list list*)
        HTMLTable.buildTable contents "pathTable" "cy2";
        let tab = HTMLTable.fetchTable "pathTable" in
        let lastRowIndex = List.length bestPath in
        let rec paintCells step =
          match step with
          | n when n < 5 -> 
            if accepted then HTMLTable.paint tab lastRowIndex step "mediumseagreen"
            else (HTMLTable.paint tab lastRowIndex step "crimson");
            paintCells (step+1);
          | n when n = 5 -> ()
        in
        paintCells 1; (*ignore step column*)
        HTMLTable.changeDisplay tab ""  
      )
      else () 
end
end
