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
 *  Written by Rita Macedo
 *)

open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open ViewUtil
open Lang
open Cytoscape
open AutomatonView
open HTMLTable

module FiniteAutomatonView = 
struct
	open FiniteAutomaton
	
  let htmlDelta = "ẟ"

  (** Auxiliar Methods **)
        
  let paintMinimization (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state color = 
        Cytoscape.paintNode cy state color
  
  let paintMinimized (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state color = 
        Cytoscape.paintNode cy state color

  let getMinStates cy list color = 
    Set.iter (fun el -> paintMinimization cy el color) list

  let delay n =
    Js_of_ocaml_lwt.Lwt_js.sleep (float_of_int n *. 0.01)

  let rec delay1 n = if n = 0 then Lwt.return ()
                                  else Lwt.bind (Lwt.pause()) (fun () -> delay1 (n-1))
  
  let transitionGet3 trns = Set.map ( fun (_,_,c) -> c ) trns
  
  let nextEpsilon2 st ts =
        let trns = Set.filter (fun (a,b,c) -> st = a && b = epsilon) ts in
        let nextStates = transitionGet3 trns in	
			Set.add st nextStates 

  let rec closeEmpty sts t = 
		let ns = Set.flatMap (fun st -> nextEpsilon2 st t) sts in
			if (Set.subset ns sts) then ns else closeEmpty (Set.union sts ns) t 

  let nextEpsilon1 st t =
          let n = Set.filter (fun (a,b,c) -> st = a && b = epsilon) t in
                  Set.map ( fun (_,_,d) -> d ) n		
				
	let rec nextEpsilons currsts t = 
		let ns = Set.flatMap (fun nst -> nextEpsilon1 nst t) currsts in
			if (Set.subset ns currsts) then ns else nextEpsilons (Set.union currsts ns) t
			
  let nextStates st sy t =
    let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
            Set.map ( fun (_,_,d) -> d) n

  let transition sts sy t = 
        let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
          Set.union nsts (nextEpsilons nsts t)

  let isFinal acceptStates el =
    Set.belongs el acceptStates

  (*JP*)

  let __none__ = "__none__"
  let bestStateColor = "DarkBlue"

  let optionsPopper = 
    Js.def (object%js 
      val placement = Js.string "right-end"
    end)

  let _popperDiv_ = "popper-div"
  let _displayConfig_ = "display-config"
  let _displayConfigClass_ = "display-config-class"
  let _infoConfigDisplayID_ = "infoConfigDisplay"
  let _nodeConfigDisplayID_ = "nodeConfigDisplay"

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

  let getConfigCountForNode node configs : int =
    Set.fold_left (fun c (st,_) -> if (state2str st) = (data_fromName node "id") then c+1 else c) 0 configs

  let buildConfigsCount (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) configs = 
    let nodes = List.filter (fun node -> (data_fromName node "id") <> "transparent") (Cytoscape.getAllNodes cy) in
      List.map (fun node -> buildPopper (getConfigCountForNode node configs) node) nodes

  let updateAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##update ()) poppers

  let destroyAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##destroy ()) poppers

  let getConfigsWithState state configs =
    Set.filter (fun (st, _) -> st = state) configs

  let buildInfoString state info =
    "["^(state2str state)^", "^(word2str info)^"]"
    
  
  (*que info queremos dar display??*)
  let buildConfigMenu ((menuID, state, info)) = 
    let infoDisplay = buildInfoString state info in
      Js.def (object%js 
        val id = Js.string menuID
        val content = Js.string infoDisplay
        val selector = Js.string "node"
        val show = Js.bool false
        val disabled = Js.bool true
        val onClickFunction = fun element -> ()
      end)
  
  let buildIdFromState state (suffix: int) =
    (state2str state)^"_"^(string_of_int suffix)
  
  (*MUDAR*)
  let processNodeConfig (configs: FiniteAutomaton.configurations) state =
    let configsOfState = getConfigsWithState state configs in
    Set.mapi (fun i (state, symb) -> buildConfigMenu (buildIdFromState state i, state, symb)) configsOfState

  let processConfigMenus (configs:FiniteAutomaton.configurations) =
    let states = Set.map (fun (a, _) -> a) configs in
    Set.flatMap (processNodeConfig configs) states

  let menuConfigFA (configs: FiniteAutomaton.configurations) =
    Js.Unsafe.coerce @@ object%js 
      val evtType = Js.string "tapdragover"
      val menuItems = Js.array (Array.of_list (Set.toList (processConfigMenus configs)))
    end 

  let buildIdsStateAndApplyF f node configs: unit =
    let configsOfState = getConfigsWithState (state node) configs in
    Set.iteri (fun idSuffix (st, _) -> f (buildIdFromState st idSuffix)) configsOfState

  let hideMenu menu id =
    menu##hideMenuItem (Js.string id)
  
  let showMenu menu id =
    menu##showMenuItem (Js.string id)

  let hideMenus menu configs node =
    buildIdsStateAndApplyF (hideMenu menu) node configs
    
  let showMenus menu configs node =
    buildIdsStateAndApplyF (showMenu menu) node configs 

  let hideAllConfigMenus menu configs =
    let states = Set.map (fun (a, _ ) -> a) configs in
      Set.iter (hideMenus menu configs) states

  let getStatesFromConfigs (configs: FiniteAutomaton.configurations) =
      Set.map (fun (st, _) -> st) configs


  class model (arg: (t) Arg.alternatives) =
		object(self) 
      inherit AutomatonView.model arg as abstractAutomaton
      inherit FiniteAutomaton.model arg as super

      val mutable bestPath: FiniteAutomaton.path = []
      val mutable configsCounter: Cytoscape.popper Js_of_ocaml.Js.t list = []
      val mutable selectedNodeConfigMenu = __none__
      val mutable configDisplay: Cytoscape.popper Js_of_ocaml.Js.t option = None
      val mutable configMenu: contextMenus Js.t option = None
      val mutable accepted : bool = false
      val mutable visitedConfigs : int = 0
      val mutable exactResult : bool = false
      val mutable acceptTime : float = 0.0


      method inputNodesPainting cy2 colors number = 
        let listStates = Set.toList self#representation.states in 
        for i=0 to number-1 do
          let newState = List.nth listStates i in 
          Cytoscape.addNode cy2 newState (newState = self#representation.initialState) (Set.belongs newState self#representation.acceptStates);
          let color = Array.get colors i in
          paintMinimized cy2 newState color
        done

      method inputEdges cy = 
        let mapToCytoscapeEdge transitions = 
          Set.map (fun (n1,s,n2) -> (n1, symb2str s, n2)) self#representation.transitions 
        in
          Set.iter (Cytoscape.addEdge cy) (mapToCytoscapeEdge self#representation.transitions)

      method accept3 (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) (w: word) =
        let transition sts sy t = 
          let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                    Set.union nsts (nextEpsilons nsts t) in
                  
        let rec accept2X sts w t exists =
          match w with
            [] -> Lwt.bind (delay 40) (fun () -> Lwt.bind (Lwt.return (self#paintStates cy sts (isFinal self#getAcceptStates))) (fun () -> Lwt.return ((Set.inter sts self#representation.acceptStates) <> Set.empty)))
            |x::xs -> Lwt.bind (delay 40) (fun () -> Lwt.bind (Lwt.return (self#paintStates cy sts (isFinal self#getAcceptStates))) 
              (fun () -> let nextTrans = transition sts x t in
                            if (Set.size nextTrans) = 0 then
                              accept2X sts [] t false
                            else 
                              accept2X (transition sts x t) xs t true
              )) in
          
        let i = closeEmpty (Set.make [self#representation.initialState]) self#representation.transitions in
          accept2X i w self#representation.transitions true

      method drawMinimize cy2 colors number =
        self#inputNodesPainting cy2 colors number;
        self#inputEdges cy2

      method addInitialNode node firstNode exists =
        if firstNode then
          (new model (Arg.Representation {
            alphabet = Set.empty;
	          states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty
          }))  
        else
          if exists then
            (let rep: t = self#representation in 
            new model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = node;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            }))
          else
            (let rep: t = self#representation in 
            new model (Representation{
              alphabet = rep.alphabet;
	            states = Set.add node rep.states;
              initialState = node;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            }))
      
      method addNode node firstNode =
      if firstNode then
        (new model (Representation {
          alphabet = Set.empty;
	        states = Set.make [node]; 
          initialState = node;
          transitions = Set.empty;
          acceptStates = Set.empty
        }))  
      else
        (let rep: t = self#representation in 
          new model (Representation{
            alphabet = rep.alphabet;
	          states = Set.add node rep.states;
            initialState = rep.initialState;
            transitions = rep.transitions;
            acceptStates = rep.acceptStates
          }))

      method addFinalNode node firstNode exists = 
        if firstNode then
          (new model (Representation {
          alphabet = Set.empty;
	        states = Set.make [node]; 
          initialState = node;
          transitions = Set.empty;
          acceptStates = Set.make [node]
        })) 
        else 
          if exists then
          (let rep: t = self#representation in 
            new model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates
            }))
          else 
            (let rep: t = self#representation in 
              new model (Representation{
              alphabet = rep.alphabet;
	            states = Set.add node rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates
            }))

      method changeToFinal node =
        let rep: t = self#representation in 
              new model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates
            })

        method removeFinal node =
        let rep: t = self#representation in 
              new model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.remove node rep.acceptStates
            })

      method eliminateNode node isStart isFinish = 
        let rep: t = self#representation in 
        if (isStart && isFinish) then
          new model (Representation{  
            alphabet = rep.alphabet;
	          states = Set.remove node rep.states;
            initialState = "";
            transitions = rep.transitions;
            acceptStates = Set.remove node rep.acceptStates
            })
        else
          if (isStart) then
            new model (Representation{  
              alphabet = rep.alphabet;
	            states = Set.remove node rep.states;
              initialState = "";
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            })
          else 
            if (isFinish) then
              new model (Representation{  
                alphabet = rep.alphabet;
	              states = Set.remove node rep.states;
                initialState = rep.initialState;
                transitions = rep.transitions;
                acceptStates = Set.remove node rep.acceptStates
            })
          else
            new model (Representation{  
              alphabet = rep.alphabet;
	            states = Set.remove node rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            })


      method newTransition (a, b, c) = 
      let rep: t = self#representation in 
        new model (Representation{
            alphabet = Set.add b rep.alphabet;
	          states = rep.states;
            initialState = rep.initialState;
            transitions = Set.add (a, b , c) rep.transitions;
            acceptStates = rep.acceptStates
      })

      method newEpsylonTransition (a, b, c) = 
      let rep: t = self#representation in 
        new model (Representation{
            alphabet = rep.alphabet;
	          states = rep.states;
            initialState = rep.initialState;
            transitions = Set.add (a, b , c) rep.transitions;
            acceptStates = rep.acceptStates
      })

      method eliminateTransition (a, b, c) = 
        let rep: t = self#representation in 
        let transitions = Set.remove (a, b, c) rep.transitions in
        new model (Representation{
          alphabet = rep.alphabet;
          states = rep.states;
          initialState = rep.initialState;
          transitions = transitions;
          acceptStates = rep.acceptStates
      })

      method getColors:int =
        Set.size self#equivalencePartition

      method paintMinimization cy colors: unit = 
          let number = self#getColors in
          let listEquivalence = Set.toList self#equivalencePartition in
          for i=0 to number-1 do 
            getMinStates cy (List.nth listEquivalence i) (Array.get colors i)
          done

      method minimize1: model = 			
        let min = super#minimize in
          let rep = min#representation in 
            new model (Representation rep) 
 
    method toDeterministic1: model = 
      let deter = super#toDeterministic in
        let rep = deter#representation in 
        new model (Representation rep) 

    method reachableFromInitialState: states = self#reachable self#representation.initialState

      method cleanUselessStates1 (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) : model =
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        let uss = self#getUselessStates in
        Set.iter (fun s -> Cytoscape.paintNode cy s AutomatonView.usefulColor) uss;
        let clean = super#cleanUselessStates in
        let rep = clean#representation in 
        new model (Representation rep) 

      method numberStates: int =
        Set.size self#representation.states

      method numberTransitions: int =
        Set.size self#representation.transitions
        
      method renameState state name =
        let rep: t = self#representation in 
        let initial = if state = rep.initialState then name else rep.initialState in
        let newStates = Set.remove state (Set.add name rep.states) in
        let newTransitions = Set.map (fun (s,n,t) -> 
          if s = state && t = state then (name,n,name) else
          if s = state then (name,n,t) else
          if t = state then (s,n,name) else (s,n,t)
        ) rep.transitions in
        let newAcceptStates = Set.map (fun s -> if s = state then name else s) rep.acceptStates in
        new model (Representation{
          alphabet = rep.alphabet;
          states = newStates;
          initialState = initial;
          transitions = newTransitions;
          acceptStates = newAcceptStates
      })

    method paintCurrentStates cy = 
      self#paintStates cy (getStatesFromConfigs steps.(position)) (isFinal self#getAcceptStates);
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
      self#setConfigsAndBestPath2 acc exact time configVisited;
      JS.log("set initial step");

      method staticAcceptFull =
        let (acc, bestPath, trail) = self#acceptFull (List.map char2symb !sentence) in
        let (exact, configVisited, time) = Model.stats() in
        self#setConfigsAndBestPath trail bestPath acc exact time configVisited;
        JS.log("set initial step");


    method setInitialStep cy =
      self#staticAcceptFull;
      self#initAllMenusAndFeatures cy steps.(0)

    method setNextStep cy =
      self#updateAllMenusAndFeatures cy steps.(position)

    method setBackStep (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
      self#updateAllMenusAndFeatures cy steps.(position)
      
(*      method renameTransition transition name =*)

    (* vvvvvvvvvv  Joao Pinto vvvvvvvvvvv*)

    method getWordFromConfig config : word =
      let (state, word) = config in
      word
    
    method returnStats =
      (accepted, visitedConfigs, exactResult, acceptTime)

    method returnPath = bestPath

    (*TABELAS*)

    method buildTable =
      let makeFATable () : string list list =
          let alphabet = Set.toList self#representation.alphabet in
          let states = Set.toList self#representation.states in
          let headers = List.map symb2str alphabet in
          let contents = List.map (fun st -> HTMLTable.makeFAStateRow st alphabet self#representation.transitions) states in
          (htmlDelta :: headers) :: contents
      in
      if not (HTMLTable.tableExists "automataTable") then (
        let contents = makeFATable () in
        HTMLTable.buildTable contents "automataTable" "tab";
      )
      else (
        let parent = Dom_html.getElementById "tab" in
        parent##.innerHTML := Js.string "";
        let contents = makeFATable () in
        HTMLTable.buildTable contents "automataTable" "tab"
      )

    method displayTrace =
      let makePath () : string list list =
        let headers = ["Trace"; "Current State"; "Word To Consume"] in
          headers :: HTMLTable.makeFAPath bestPath 0
      in
      if not (HTMLTable.tableExists "pathTable") then (
        let contents = makePath () in
      (*expected string list list*)
        HTMLTable.buildTable contents "pathTable" "cy2";
        let tab = HTMLTable.fetchTable "pathTable" in
        let lastRowIndex = List.length bestPath in
        let _ =
          for n = 1 to 4 (*List.length headers + 1*) do
            if accepted then HTMLTable.paint tab lastRowIndex n "mediumseagreen"
            else HTMLTable.paint tab lastRowIndex n "crimson"
          done
        in
        HTMLTable.changeDisplay tab ""   
      )
      else ()
    
      
    (*ANIMS*)

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


    method private updateConfigMenu (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) configs =
      self#resetConfigMenu;
      let cm = cy##contextMenus(menuConfigFA configs) in
      configMenu <- Some cm;
      hideAllConfigMenus cm configs

    method private updateAllPoppers =
      updateAllPoppers configsCounter

    method private subscribeNodesPositionUpdate cy =
      cy##on (Js.string "position") (Js.string "node")
      (fun _ -> self#updateAllPoppers);

      cy##on_3 (Js.string "pan zoom resize")
      (fun _ -> self#updateAllPoppers);
    
    method private paintBestCurrentStep cy =
      match self#getCurrConfigFromBestPath with
        | None -> ()
        | Some (currBestState, _ ) -> 
          if position <> (List.length bestPath)-1 
            then Cytoscape.paintNode cy currBestState bestStateColor 
          else ()
    
    
    method private setConfigsAndBestPath trail bestPathAutomaton acc exact time configVisited=
      steps <- Array.of_list trail;
      JS.log("METI OS STEPS");
      JS.log(steps);
      (*let setConfigs = List.fold_left (fun c config -> 
        steps.(c) <- config;
        c+1;
        ) 0 trail in*)
      (*let rec setConfigs configs position =
        match configs with
          | [] -> ()
          | configs :: cs ->
            steps.(position) <- configs;
            setConfigs cs (position + 1)
      in
      *)
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

    method private getCurrConfigFromBestPath = 
      if bestPath <> [] then Some (List.nth bestPath position) else None

      
      

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
              if Set.belongs (state node) (
                Set.map (fun (a, _) -> a) configs
              ) then
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
