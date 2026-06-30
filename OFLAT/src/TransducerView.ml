open OCamlFlat
open BasicTypes
open AutomatonView
open Js_of_ocaml
open JS
open HTMLTable
open Cytoscape
open StateVariables

module TransducerView = 
struct
	open Transducer

  let htmlDelta = "ẟ"

  let __none__ = "__none__"
  let bestStateColor = "DarkBlue"

  let optionsPopper = 
    Js.def (object%js 
      val placement = Js.string "right-end"
    end)

  let _popperDiv_ = "popper-div"

  let buildPopper nConfigs (node: Cytoscape.DataItem.t Js_of_ocaml.Js.t) =
    node##popper (
      Js.Unsafe.coerce @@ object%js
        val content = fun () -> 
          let countConfigsDiv = Dom_html.document##createElement (Js.string "div") in
            (countConfigsDiv##.classList)##add(Js.string _popperDiv_);
            countConfigsDiv##.innerHTML := Js.string (string_of_int nConfigs);
            Dom.appendChild (Dom_html.getElementById "cy") countConfigsDiv;
              countConfigsDiv
        val popper = optionsPopper
      end
    )

  let getConfigCountForNode node (configs: Transducer.configurations) : int =
    Set.fold_left (fun c (st,_,_) -> if (state2str st) = (Cytoscape.data_fromName node "id") then c+1 else c) 0 configs

  let buildConfigsCount (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) configs = 
    let nodes = List.filter (fun node -> (Cytoscape.data_fromName node "id") <> "transparent") (Cytoscape.getAllNodes cy) in
      List.map (fun node -> buildPopper (getConfigCountForNode node configs) node) nodes

  let updateAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##update ()) poppers

  let destroyAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##destroy ()) poppers

  let getConfigsWithState state (configs: Transducer.configurations) =
    Set.filter (fun (st, _, _) -> st = state) configs

  let buildInfoString state in_word out_word =
    let in_str = if in_word = [] then "~" else word2str in_word in
    let out_str = if out_word = [] then "~" else word2str out_word in
    "[" ^ (state2str state) ^ ", In: " ^ in_str ^ ", Out: " ^ out_str ^ "]"
    
  let buildConfigMenu ((menuID, state, in_w, out_w)) = 
    let infoDisplay = buildInfoString state in_w out_w in
      Js.def (object%js 
        val id = Js.string menuID
        val content = Js.string infoDisplay
        val selector = Js.string "node"
        val show = Js.bool false
        val disabled = Js.bool true
        val onClickFunction = fun _ -> ()
      end)
  
  let buildIdFromState state (suffix: int) =
    (state2str state)^"_"^(string_of_int suffix)
  
  let processNodeConfig (configs: Transducer.configurations) state =
    let configsOfState = getConfigsWithState state configs in
    Set.mapi (fun i (st, in_w, out_w) -> buildConfigMenu (buildIdFromState st i, st, in_w, out_w)) configsOfState

  let processConfigMenus (configs: Transducer.configurations) =
    let states = Set.map (fun (a, _, _) -> a) configs in
    Set.flatMap (processNodeConfig configs) states

  let menuConfigFST (configs: Transducer.configurations) =
    Js.Unsafe.coerce @@ object%js 
      val evtType = Js.string "tapdragover"
      val menuItems = Js.array (Array.of_list (Set.toList (processConfigMenus configs)))
    end 

  let buildIdsStateAndApplyF f node configs: unit =
    let configsOfState = getConfigsWithState node configs in
    Set.iteri (fun idSuffix (st, _, _) -> f (buildIdFromState st idSuffix)) configsOfState

  let hideMenu menu id = menu##hideMenuItem (Js.string id)
  let showMenu menu id = menu##showMenuItem (Js.string id)

  let hideMenus menu configs node = buildIdsStateAndApplyF (hideMenu menu) node configs
  let showMenus menu configs node = buildIdsStateAndApplyF (showMenu menu) node configs 

  let hideAllConfigMenus menu configs =
    let states = Set.map (fun (a, _, _) -> a) configs in
      Set.iter (hideMenus menu configs) states

	class model (arg: (t) Arg.alternatives) =
		object(self) 
      inherit AutomatonView.model arg as abstractAutomaton
      inherit Transducer.model arg as super

      val mutable bestPath : Transducer.path = []
      val mutable accepted : bool = false
      val mutable visitedConfigs : int = 0
      val mutable exactResult : bool = false
      val mutable acceptTime : float = 0.0

      val mutable outputSentence : string = ""

      val mutable configsCounter: Cytoscape.popper Js_of_ocaml.Js.t list = []
      val mutable selectedNodeConfigMenu = __none__
      val mutable configMenu: Cytoscape.contextMenus Js.t option = None

      method getInitialState = self#representation.initialState
      method getAcceptStates = self#representation.acceptStates
      method getStates = self#representation.states

      method reachableFromInitialState = 
        Transducer.reachable self#representation self#representation.initialState

      method productive = 
        Transducer.productive self#representation

      method areAllStatesUseful = 
        Transducer.isClean self#representation
      
      method isDeterministic = 
        Transducer.isDeterministic self#representation

      method numberStates = 
        Set.size self#representation.states

      method numberTransitions = 
        Set.size self#representation.transitions

      method getUselessStates =
        let useful = Set.inter self#productive self#reachableFromInitialState in
        Set.diff self#representation.states useful

      method isMealy = Transducer.isMealyMachine self#representation
      
      method isMoore = Transducer.isMooreMachine self#representation

      method asFiniteAutomaton = Transducer.asFiniteAutomaton self#representation

      method asTuringMachine = Transducer.asTuringMachine self#representation

      method addNode node firstNode =
        if firstNode then
          (new model (Arg.Representation {
            inAlphabet = Set.empty;
            outAlphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty
          }))  
        else
          (let rep = self#representation in 
            new model (Arg.Representation { rep with states = Set.add node rep.states }))

      method addInitialNode node firstNode exists =
        if firstNode then
          (new model (Arg.Representation {
            inAlphabet = Set.empty;
            outAlphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty
          }))  
        else
          let rep = self#representation in
          let new_states = if exists then rep.states else Set.add node rep.states in
          new model (Arg.Representation { rep with states = new_states; initialState = node })

      method addFinalNode node firstNode exists = 
        if firstNode then
          (new model (Arg.Representation {
            inAlphabet = Set.empty;
            outAlphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.make [node]
          })) 
        else 
          let rep = self#representation in
          let new_states = if exists then rep.states else Set.add node rep.states in
          let new_accepts = Set.add node rep.acceptStates in
          new model (Arg.Representation { rep with states = new_states; acceptStates = new_accepts })

      method changeToFinal node =
        let rep = self#representation in 
        new model (Arg.Representation { rep with acceptStates = Set.add node rep.acceptStates })

      method removeFinal node =
        let rep = self#representation in 
        new model (Arg.Representation { rep with acceptStates = Set.remove node rep.acceptStates })

      method eliminateNode node isStart isFinish = 
        let rep = self#representation in
        let new_states = Set.remove node rep.states in
        let new_initial = if isStart then "" else rep.initialState in
        let new_accepts = if isFinish then Set.remove node rep.acceptStates else rep.acceptStates in
        let new_transitions = Set.filter (fun (s, _, _, d) -> s <> node && d <> node) rep.transitions in
        new model (Arg.Representation { 
          rep with 
          states = new_states; 
          initialState = new_initial; 
          acceptStates = new_accepts;
          transitions = new_transitions
        })

      method newTransition (src, inSym, outSym, dst) = 
        let rep = self#representation in
        let new_inAlpha = if inSym <> epsilon then Set.add inSym rep.inAlphabet else rep.inAlphabet in
        let new_outAlpha = if outSym <> epsilon then Set.add outSym rep.outAlphabet else rep.outAlphabet in
        let new_trans = Set.add (src, inSym, outSym, dst) rep.transitions in
        new model (Arg.Representation { 
          rep with 
          inAlphabet = new_inAlpha;
          outAlphabet = new_outAlpha;
          transitions = new_trans 
        })
      
      method eliminateTransition (src, inSym, outSym, dst) = 
        let rep = self#representation in 
        let new_trans = Set.remove (src, inSym, outSym, dst) rep.transitions in
        new model (Arg.Representation { rep with transitions = new_trans })

      method renameState state name =
        let rep = self#representation in 
        let new_initial = if state = rep.initialState then name else rep.initialState in
        let new_states = Set.remove state (Set.add name rep.states) in
        let new_transitions = Set.map (fun (s, i, o, t) -> 
          let ns = if s = state then name else s in
          let nt = if t = state then name else t in
          (ns, i, o, nt)
        ) rep.transitions in
        let new_accepts = Set.map (fun s -> if s = state then name else s) rep.acceptStates in
        new model (Arg.Representation {
          rep with
          states = new_states;
          initialState = new_initial;
          transitions = new_transitions;
          acceptStates = new_accepts
        })

      method inputEdges (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        let mapToCytoscapeEdge transitions =
            Set.map (fun (src, iSym, oSym, dst) ->
                let symbText sy =
                  if sy = epsilon then StateVariables.returnEmpty () else symb2str sy
                in
                let label = (symbText iSym) ^ ":" ^ (symbText oSym) in
                (src, label, dst)
            ) self#representation.transitions
        in
        Set.iter (Cytoscape.addEdge cy) (mapToCytoscapeEdge self#representation.transitions)

      method drawExample (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) layout = 
        self#inputNodes cy;
        self#inputEdges cy;
        Cytoscape.runLayout cy layout

      method private getStatesFromConfigs (configs: Transducer.configurations) =
          Set.map (fun (st, _, _) -> st) configs

      method setConfigsAndBestPath trail bestPathAutomaton acc exact time configVisited =
          steps <- Array.of_list trail;
          bestPath <- bestPathAutomaton;
          accepted <- acc;
          exactResult <- exact;
          acceptTime <- time;
          visitedConfigs <- configVisited

      method setConfigsAndBestPath2 acc exact time configVisited =
          accepted <- acc;
          exactResult <- exact;
          acceptTime <- time;
          visitedConfigs <- configVisited

      method staticAcceptFull =
        let (acc, bPath, trail) = Transducer.acceptFull self#representation (List.map char2symb !sentence) in
        let (exact, configVisited, time) = Model.stats() in
        self#setConfigsAndBestPath trail bPath acc exact time configVisited;
        JS.log("Transducer execution initialized")

      method paintCurrentStates cy = 
        let currentConfigs = steps.(position) in
        let currentStates = self#getStatesFromConfigs currentConfigs in
        self#paintStates cy currentStates (fun st -> Set.belongs st self#getAcceptStates);
        self#paintBestCurrentStep cy

      method setInitialStep cy =
        self#staticAcceptFull;
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        if Array.length steps > 0 then self#initAllMenusAndFeatures cy steps.(0)

      method setNextStep cy = 
        if position < Array.length steps then
          self#updateAllMenusAndFeatures cy steps.(position)
          
      method setBackStep cy =
        if position < Array.length steps then
          self#updateAllMenusAndFeatures cy steps.(position)
      
      method private getWordFromConfig config =
        let (_, remainingWord, _) = config in
        remainingWord

      method private changeSentence config =
        newSentence := "";
        let wordToConsume = self#getWordFromConfig config in
        let (_, _, outputWord) = config in
        let bar = '|' in
        
        for i = 0 to (List.length !sentence) - (List.length wordToConsume) - 1 do
          newSentence := !newSentence ^ String.make 1 (List.nth !sentence i)
        done;
        
        newSentence := !newSentence ^ String.make 1 bar;
        
        for i = 0 to (List.length wordToConsume) - 1 do
          newSentence := !newSentence ^ symb2str (List.nth wordToConsume i)
        done;
        
        newSentence := !newSentence ^ " → ";
        newSentence := !newSentence ^ word2str outputWord

      method private getCurrConfigFromBestPath = 
        if bestPath <> [] && position < List.length bestPath then 
          Some (List.nth bestPath position) 
        else None

      method private paintBestCurrentStep cy =
        match self#getCurrConfigFromBestPath with
        | None -> ()
        | Some (currBestState, _, _) ->
            if position <> (List.length bestPath) - 1 then
              Cytoscape.paintNode cy currBestState bestStateColor
            else ()

      method buildTable =
        let makeFSTTable () : string list list =
            let inAlphabet = Set.toList self#representation.inAlphabet in
            
            let hasEpsilon = Set.exists (fun (_, i, _, _) -> i = epsilon) self#representation.transitions in
            let tableAlphabet = if hasEpsilon then inAlphabet @ [epsilon] else inAlphabet in
            
            let states = Set.toList self#representation.states in
            let headers = List.map symb2str tableAlphabet in
            
            let contents = List.map (fun st -> 
              let row = ref [state2str st] in
              List.iter (fun sy ->
                let matching = Set.filter (fun (s, i, o, d) -> s = st && i = sy) self#representation.transitions in
                if Set.isEmpty matching then row := !row @ ["-"]
                else
                  let cell = String.concat ", " (Set.toList (Set.map (fun (_, _, o, d) -> 
                    (state2str d) ^ "/" ^ (symb2str o)) matching)) in
                  row := !row @ [cell]
              ) tableAlphabet;
              !row
            ) states in
            (htmlDelta :: headers) :: contents
        in
      if not (HTMLTable.tableExists "automataTable") then (
        let contents = makeFSTTable () in
        HTMLTable.buildTable contents "automataTable" "tab";
      )
      else (
        let parent = Dom_html.getElementById "tab" in
        parent##.innerHTML := Js.string "";
        let contents = makeFSTTable () in
        HTMLTable.buildTable contents "automataTable" "tab"
      )

      method staticAccept =
        let word = List.map char2symb !sentence in
        let acc = Transducer.accept self#representation word in
        let (exact, configVisited, time) = Model.stats() in
        self#setConfigsAndBestPath2 acc exact time configVisited;

      method returnStats = (accepted, visitedConfigs, exactResult, acceptTime)

      method getColors = 
        Set.size self#equivalencePartition

      method paintMinimization (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) (colors: string array) = 
          let number = self#getColors in
          let listEquivalence = Set.toList self#equivalencePartition in
          for i=0 to number-1 do 
            let block = List.nth listEquivalence i in
            let color = Array.get colors i in
            Set.iter (fun st -> Cytoscape.paintNode cy st color) block
          done

      method private inputNodesPainting cy2 colors number = 
        let listStates = Set.toList self#representation.states in 
        for i=0 to number-1 do
          let newState = List.nth listStates i in 
          Cytoscape.addNode cy2 newState (newState = self#representation.initialState) (Set.belongs newState self#representation.acceptStates);
          let color = Array.get colors i in
          Cytoscape.paintNode cy2 newState color
        done

      method drawMinimize cy2 colors number layout =
        self#inputNodesPainting cy2 colors number;
        self#inputEdges cy2;
        Cytoscape.runLayout cy2 layout

      method displayTrace =
        let rec makeFSTPath path acc =
          match path with
          | [] -> []
          | (st, rem_word, out_word) :: xs ->
              let trace_step = string_of_int acc in
              let state_str = state2str st in
              let out_str = if out_word = [] then "~" else word2str out_word in
              let in_str = if rem_word = [] then "~" else word2str rem_word in
              [trace_step; state_str; out_str; in_str] :: makeFSTPath xs (acc + 1)
        in
        
        let makePath () : string list list =
          let headers = ["Trace"; "Current State"; "Output"; "Word To Consume"] in
            headers :: makeFSTPath bestPath 0
        in
        
        if not (HTMLTable.tableExists "pathTable") then (
          let contents = makePath () in
          HTMLTable.buildTable contents "pathTable" "cy2";
          
          let tab = HTMLTable.fetchTable "pathTable" in
          let lastRowIndex = List.length bestPath in
          
          let _ =
            for n = 1 to 4 do
              if accepted then HTMLTable.paint tab lastRowIndex n "mediumseagreen"
              else HTMLTable.paint tab lastRowIndex n "crimson"
            done
          in
          HTMLTable.changeDisplay tab ""   
        )
        else ()

      method staticGenerate n = super#generate n

      method staticGenerateWithOutput n =
        let words = super#generate n in
        let pairs = List.filter_map (fun w ->
          let (ok, out) = Transducer.acceptOut self#representation w in
          if ok then Some (w, out) else None
        ) (Set.toList words) in
        pairs

      method errors = 
        let rep = self#representation in
        let errList = ref [] in
        if rep.initialState <> "" && not (Set.belongs rep.initialState rep.states) then
          errList := "Initial state does not belong to the set of states." :: !errList;
        if not (Set.subset rep.acceptStates rep.states) then
          errList := "One or more accept states do not exist." :: !errList;
        let validTrns = 
          Set.for_all (fun (src, _, _, dst) -> 
            Set.belongs src rep.states && Set.belongs dst rep.states
          ) rep.transitions 
        in
        if not validTrns then
          errList := "There are transitions linking to non-existent states." :: !errList;
        !errList

      method private resetConfigMenu =
        match configMenu with
        | None -> ()
        | Some menu -> 
            menu##destroy();
            configMenu <- None;
            selectedNodeConfigMenu <- __none__

      method resetToEditModel = 
        self#resetConfigMenu;
        self#destroyAllPoppers

      method clearPoppers = 
        self#destroyAllPoppers;
        self#resetConfigMenu

      method private updateConfigMenu (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) configs =
        self#resetConfigMenu;
        let cm = cy##contextMenus(menuConfigFST configs) in
        configMenu <- Some cm;
        hideAllConfigMenus cm configs

      method private updateAllPoppers =
        updateAllPoppers configsCounter

      method private subscribeNodesPositionUpdate cy =
        cy##on (Js.string "position") (Js.string "node")
        (fun _ -> self#updateAllPoppers);
        cy##on_3 (Js.string "pan zoom resize")
        (fun _ -> self#updateAllPoppers)

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
                if Set.belongs node (Set.map (fun (a, _, _) -> a) configs) then
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

      method private initAllMenusAndFeatures cy configs =
        self#updateConfigMenu cy configs;
        self#buildPoppersConfigsCounter cy configs;
        self#subscribeNodesPositionUpdate cy;
        self#subscribeConfigEventMenu cy 
      
      method private updateAllMenusAndFeatures cy configs =
        self#buildPoppersConfigsCounter cy configs;
        self#updateConfigMenu cy configs
end
end
