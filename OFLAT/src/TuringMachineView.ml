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
open Lang
open Cytoscape
open TuringMachine
open JSon


module TuringMachineView
=
struct
	open TuringMachine


  (* ******************Auxiliar Functions****************** *)

  let productiveColor = "orange"
  let reachableColor = "yellow"
  let usefulColor = "purple"
  let stepState = "blue"
  let acceptState = "green"
  let wrongFinalState = "red"
  let uncertainState = "#a34900" (*"LightBrown"*)
  let bestStateColor = "DarkBlue"

  let sentence: char list ref = ref []
 
  let newSentence = ref ""

  let direction2string direction = if direction = L then "L" else if direction = R then "R" else "S"

  let iterateList meth (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) list =
    List.iter (fun el -> (meth cy el) ) list

  let nextStates st sy t =
    let n = Set.filter (fun (a,b,c,_,_) -> st = a && sy = b) t in
      Set.map (fun (_,_,d,_,_) -> d) n

  let getNextSteps sts sy t = 
    Set.flatMap (fun st -> nextStates st sy t) sts (*O que acontece quando se faz flat map de um array em que cada celula contem apenas uma string/simbolo*)

  let rec intersection l1 l2 =
    match l1 with
        [] -> []
      | x::xs -> (if List.mem x l2 then [x] else []) @ intersection xs l2

  let cut s = (String.get s 0, String.sub s 1 ((String.length s)-1))

  let getCurrConfigFromBestPath bestPath position: configuration option =
	if (bestPath <> []) && (not ((List.length bestPath) <= position)) then Some (List.nth bestPath position) else None


  (* ******************Function Method Functions****************** *)

  (*-------------makeLabel-------------*)
  let makeLabel rsymbs wsymbs directions =
    let r_str = "[" ^ (String.concat ";" (List.map symb2str rsymbs)) ^ "]" in
    let w_str = "[" ^ (String.concat ";" (List.map symb2str wsymbs)) ^ "]" in
    let d_str = "[" ^ (String.concat ";" (List.map direction2string directions)) ^ "]" in
      String.concat "/" [r_str; w_str; d_str]

  
  (*-------------inputEdges-------------*)
  let transformTransitions (trs: transitions) =
	Set.map (fun (rstate, rsymbs, nstate, wsymbs, directions) -> (rstate, (makeLabel rsymbs wsymbs directions), nstate)) trs
  let inputEdges cy (trs: transitions) =
	Set.iter (fun el -> (Cytoscape.addEdgeGeneral cy el) ) (transformTransitions trs)


  (*-------------inputNodes-------------*)
  let inputNodes cy rep = 
    Set.iter (fun el -> (
      JS.log (el); 
      Cytoscape.addNode cy el (el = rep.initialState) (Set.belongs el rep.acceptStates)
    )) rep.states


  (*-------------drawExample-------------*)
  let drawExample cy rep = 
    inputNodes cy rep;
    inputEdges cy rep.transitions


  (*-------------numberStates-------------*)
  let numberStates sts = Set.size sts


  (*-------------numberTransitions-------------*)
  let numberTransitions trs = Set.size trs


  (*-------------productivePainting-------------*)
  let productivePainting cy prod =
    let paintProductive cy state = Cytoscape.paintNode cy state productiveColor in
    let list1 = Set.toList prod in
      iterateList paintProductive cy list1


  (*-------------reachablePainting-------------*)
  let reachablePainting cy reach initSt =
    let paintReachable cy state = Cytoscape.paintNode cy state reachableColor in
    let list1 = Set.toList (reach (initSt)) in
      iterateList paintReachable cy list1 


  (*-------------usefulPainting-------------*)
  let usefulPainting cy prod reach initSt =
    let paintUseful cy state = Cytoscape.paintNode cy state usefulColor in
    let intre = intersection (Set.toList prod) (Set.toList (reach (initSt))) in
      iterateList paintUseful cy intre 

(*********** AMD
  (*-------------cleanUselessStatesCy-------------*)
  let cleanUselessStatesCy cy cleanUSt: TuringMachineView.model =
    (*Cytoscape.resetStyle cy Cytoscape.faStyle;
    Set.iter (fun el -> paintUseful cy el) self#getUselessStates;*)
    new TuringMachineView.model (Arg.Representation cleanUSt) 
*)

  (*-------------stringAsList-------------*)
  let rec stringAsList s =
    if s = "" then []
    else
        let (x,xs) = cut s in
            x::stringAsList xs


  (*-------------changeTheTestingSentence-------------*)
  let changeTheTestingSentence word = sentence := stringAsList word


  (*-------------changeSentence-------------*)
  let changeSentence config = 
    newSentence := "";
    let bar = '|' in 
    let (_,tapes) = config in
    let (revLeft,right) = List.hd tapes in
    let notEmptyRight = if List.length right = 0 then [empty] else right in
    let left = List.rev revLeft in
      for i = 0 to (List.length left) - 1 do 
        newSentence:= !newSentence ^ symb2str (List.nth left i);
      done;
      newSentence:= !newSentence ^ String.make 1 bar;
      for i = 0 to (List.length notEmptyRight) - 1 do 
        newSentence:= !newSentence ^ symb2str (List.nth notEmptyRight i);
      done


  (*-------------paint-------------*)
  let paint (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) state final best finished isOver accepted criteria = 
    if final || ((not criteria) && isOver && accepted) then Cytoscape.paintNode cy state acceptState
    else if isOver && not finished then Cytoscape.paintNode cy state uncertainState
    else if not accepted && isOver then Cytoscape.paintNode cy state wrongFinalState
    else if best then Cytoscape.paintNode cy state bestStateColor
    else Cytoscape.paintNode cy state stepState


  (*-------------paintCurrentStates-------------*)

  let isFinalState criteria finalStates state =
    if criteria then Set.exists (fun x -> x = state) finalStates else false

  let paintStates cy states criteria acceptSts finished isOver accepted = 
    Set.iter (fun el -> paint cy el (isFinalState criteria acceptSts el) false finished isOver accepted criteria) states

  let paintBestCurrentStep cy bestPath position criteria acceptSts finished isOver accepted =
    match getCurrConfigFromBestPath bestPath position with
    | None -> ()
    | Some (currBestState,_) -> paint cy currBestState (isFinalState criteria acceptSts currBestState) true finished isOver accepted criteria

	let configurationGet1 (configs: configurations) = Set.map ( fun (st, tapes) -> st ) configs

  let paintCurrentStates cy configs criteria acceptSts bestPath position finished isOver accepted =
    Cytoscape.resetStyle cy Cytoscape.faStyle;
    paintStates cy (configurationGet1 configs) criteria acceptSts finished isOver accepted;
    paintBestCurrentStep cy bestPath position criteria acceptSts finished isOver accepted 


  (*-------------startAccept-------------*)    



  (*-------------next-------------*)



  (*-------------back-------------*)



  (*-------------isOver-------------*)



  (*-------------newSentence1-------------*)



  (*-------------dissectTransitionInput-------------*)
  let filterTransitionInput input = 
    let rdSymb = String.sub input 0 1 in
    let wrtSymb = String.get input 2 in
    let direction = String.get input 4 in
      (*Check if everyting is in order*)
      (rdSymb, wrtSymb, direction)

  let filterTransitionInputWithInv input =
    let rdSymb = String.sub input 0 2 in
    let wrtSymb = String.get input 3 in
    let direction = String.get input 5 in
      (rdSymb, wrtSymb, direction)

  let dissectTransitionInput input =
    if String.length input = 5 then filterTransitionInput input 
    else filterTransitionInputWithInv input


  (*-------------addNode-------------*)



  (*-------------addInitialNode-------------*)



  (*-------------addFinalNode-------------*)



  (*-------------eliminateNode-------------*)



  (*-------------changeToFinal-------------*)



  (*-------------removeFinal-------------*)



  (*-------------renameNode-------------*)



  (*-------------newTransition-------------*)



  (*-------------eliminateTransition-------------*)



  (*-------------setInitialStep-------------*)



  (*-------------setStep-------------*)



  (*-------------isSimulating-------------*)



  (*-------------Poppers and menus-------------*)
  
  let buildConfigMenu ((menuID, tape)) =
    Js.def (object%js
      val id = Js.string menuID
      val content = Js.string tape
      val selector = Js.string "node"
      val show = Js.bool false
      val disabled = Js.bool true
      val onClickFunction = fun element -> ()
    end)

  let buildIdFromState state (suffix: int) =
    (state2str state)^"_"^(string_of_int suffix)

  let getConfigsWithState state (configs: configurations) =
    Set.filter (fun (st, _) -> st = state) configs

  let buildTape left right = 
(* ML   let rightSide = String.cat "|" (word2str right) in *)
    let rightSide = "|" ^ (word2str right) in
    (word2str (List.rev left)) ^ rightSide

  let processNodeConfig (configs: configurations) state =
    let configsOfState = getConfigsWithState state configs in
    Set.mapi (fun i (state, tapes) ->
		let (left, right) = List.hd tapes in
			buildConfigMenu (buildIdFromState state i, buildTape left right)) configsOfState

  let processConfigMenus (configs: configurations) =
    let states = configurationGet1 configs in
    Set.flatMap (processNodeConfig configs) states

  let menuConfigPDA (configs: TuringMachine.configurations) = 
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
    let states = configurationGet1 configs in
      Set.iter (hideMenus menu configs) states
  
  let __none__ = "__none__"

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

  let getConfigCountForNode node configs: int =
    Set.fold_left (fun c (st,_) -> if (state2str st) = (data_fromName node "id") then c+1 else c) 0 configs

  let buildConfigsCount (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) configs = 
    let nodes = List.filter (fun node -> (data_fromName node "id") <> "transparent") (Cytoscape.getAllNodes cy) in
      List.map (fun node -> buildPopper (getConfigCountForNode node configs) node) nodes

  let updateAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##update ()) poppers
  
  let destroyAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##destroy ()) poppers

  let isSimulating bpath =
    if Set.size (Set.make bpath) = 0 then false else true


  let getInitConfig rep sentence: configuration =
    let word = str2word (String.init (List.length !sentence) (fun i -> List.nth !sentence i)) in
      Set.hd (initialConfigs rep word)
       
  class model (arg: t Arg.alternatives) =
    object(self) inherit TuringMachine.model arg as super

      val mutable specsMenu: Cytoscape.popper Js_of_ocaml.Js.t option = None
      val mutable bestPath: TuringMachine.path = []
      val mutable configMenu: contextMenus Js.t option = None
      val mutable selectedNodeConfigMenu = __none__
      val mutable configsCounter: Cytoscape.popper Js_of_ocaml.Js.t list = []

      val mutable steps = [||]
      val mutable position = -1
      val mutable currentState: state = ""
      val mutable isOver = false
      val mutable accepted = false
      val mutable finished = true
      val mutable visitedConfigs : int = 0
      val mutable exactResult : bool = false
      val mutable acceptTime : float = 0.0

      method numberStates = 
        numberStates self#getStates
  
      method numberTransitions = 
        numberTransitions self#getTransitions

      method isOver = 
        isOver

      method isSimulating = 
        not (position = -1)

      (* Edition Methods *)

      method inputEdges cy = 
        inputEdges cy self#getTransitions
      
      method inputNodes cy = 
        inputNodes cy self#representation

      method drawExample cy = 
        drawExample cy self#representation

      method paint (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) state final best = 
        paint cy state final best finished isOver accepted self#getCriteria

      method productivePainting cy = 
        productivePainting cy self#productive 

      method reachablePainting cy = 
        reachablePainting cy self#reachable self#getInitialState

      method usefulPainting cy =  
        usefulPainting cy self#productive self#reachable self#getInitialState

      method addNode node: model = 
        new model (Arg.Representation (self#addState node))

      method addInitialNode node: model =
        new model (Arg.Representation (self#addInitialState node))

      method addFinalNode node: model =
        new model (Arg.Representation (self#addFinalState node))

      method eliminateNode node: model = 
        new model (Arg.Representation (self#removeState node))
      
      method changeToFinal node: model =
        new model (Arg.Representation (self#changeStateToFinal node))
      
      method removeFinal node: model =
        new model (Arg.Representation (self#changeStateFromFinal node))

      method renameNode node newNode: model =
        new model (Arg.Representation (self#renameState node (state newNode)))

      method newTransition trs: model = 
        new model (Arg.Representation (self#addTransition trs))

      method eliminateTransition trs: model = 
        new model (Arg.Representation (self#removeTransition trs))

      (* Simulation Methods *)

      method startAccept cy =
        steps <- Array.make 1000 Set.empty;
        position <- 0;
        isOver <- false;
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        self#setInitialStep cy;
        if (Set.size steps.(position + 1)) = 0 then(
          isOver <- true
        );
        self#paintCurrentStates cy; 
        match self#getCurrConfigFromBestPath with
          | None -> ()
          | Some config -> self#changeSentence config; 

      method next (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        if isOver then
          (JS.alertStr (Lang.i18nAlertEndTMSim ()))
        else 
          (
            position <- position + 1;
            let currConfig: TuringMachine.configuration = 
              match self#getCurrConfigFromBestPath with
              | None -> getInitConfig self#representation sentence
              | Some config -> config
            in
            self#setStep cy;
            self#changeSentence currConfig;
            if (Set.size steps.(position + 1)) = 0 then(
              isOver <- true
            );
            self#paintCurrentStates cy;
          )
              
      method back cy =
        if position = 0 then
          (JS.alertStr (Lang.i18nAlertArrivedInitial ()))
        else
          (
            position <- position - 1;
            isOver <- false;
            self#setStep cy;
            self#paintCurrentStates cy;
            match self#getCurrConfigFromBestPath with
              | None -> ()
              | Some config -> self#changeSentence config;
          )
  
      method setInitialStep cy =
        self#staticAcceptFull;
        self#initAllMenusAndFeatures cy steps.(0)
  
      method setStep (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        self#updateAllMenusAndFeatures cy steps.(position)

      method changeTheTestingSentence word = 
        changeTheTestingSentence word

      method changeSentence config = 
        changeSentence config

      method newSentence1 = 
        !newSentence

      method changeToEditModelMode (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        steps <- [||];
        position <- -1;
        isOver <- false;
        currentState <- "";
        accepted <- false;
        finished <- true;
        sentence := [];
        newSentence := "";
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        self#resetSimulationHelpers

      method cleanUselessStatesCy (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t): model =
		new model (Arg.Representation self#cleanUselessStates)
  
      method makeLabel rsymb wsymb direction = 
        makeLabel rsymb wsymb direction

      method dissectTransitionInput input = 
        dissectTransitionInput input

      (* Private Methods *)

      method private getStates = 
        self#representation.states

      method private getInitialState = 
        self#representation.initialState

      method private getTransitions = 
        self#representation.transitions

      method private getAcceptStates = 
        self#representation.acceptStates

      method private getCriteria = 
        self#representation.criteria 

      method private getCurrConfigFromBestPath = 
        getCurrConfigFromBestPath bestPath position

      method private paintCurrentStates cy  = 
        paintCurrentStates cy steps.(position) self#getCriteria self#getAcceptStates bestPath position finished isOver accepted

      method private resetSimulationHelpers = 
        self#resetConfigMenu;
        self#destroyAllPoppers
        
      method private initAllMenusAndFeatures cy configs = 
        self#resetSimulationHelpers;
        self#buildPoppersConfigsCounter cy configs;
        self#subscribeNodesPositionUpdate cy;
        self#updateConfigMenu cy configs;
        self#subscribeConfigEventMenu cy
 
      method private setConfigsAndBestPath trail bestPathAutomaton acc exact time configVisited=
        steps <- Array.of_list trail;
          bestPath <- bestPathAutomaton;
          accepted <- acc;
        finished <- exact;
        acceptTime <- time;
        visitedConfigs <- configVisited

        method private setConfigsAndBestPath2 acc exact time configVisited=
          accepted <- acc;
          finished <- exact;
          acceptTime <- time;
          visitedConfigs <- configVisited
  
      method private updateAllMenusAndFeatures cy configs =
        self#resetSimulationHelpers;
        self#buildPoppersConfigsCounter cy configs;
        self#updateConfigMenu cy configs

  (*  method private updateConfigMenu cy configs =
        configMenu <- Some (cy##contextMenus(menuConfigPDA configs));
        hideAllConfigMenus (Option.get configMenu) configs  ML *)
   
      method private updateConfigMenu cy configs =
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
                if Set.belongs (state node) (configurationGet1 configs) then
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
        configsCounter <- buildConfigsCount cy configs
        
      method private updateAllPoppers =
        updateAllPoppers configsCounter
  
      method private subscribeNodesPositionUpdate cy =
        cy##on (Js.string "position") (Js.string "node")
        (fun _ -> self#updateAllPoppers);
  
        cy##on_3 (Js.string "pan zoom resize")
        (fun _ -> self#updateAllPoppers);


      (*JP*)

      method staticAccept =
        let acc = self#accept (List.map char2symb !sentence) in
        let (exact, configs, time) = Model.stats() in
        self#setConfigsAndBestPath2 acc exact time configs;

        method staticAcceptFull =
          let (acc, bestPath, trail) = self#acceptFull (List.map char2symb !sentence) in
          let (exact, configs, time) = Model.stats() in
          self#setConfigsAndBestPath trail bestPath acc exact time configs;

      method returnStats =
        (accepted, visitedConfigs, exactResult, acceptTime)

      method displayTrace =
        let makePath () : string list list =
          let headers = ["Trace"; "State"; "Tape"] in
            headers :: HTMLTable.makeTMPath bestPath 0
        in
        if not (HTMLTable.tableExists "pathTable") then (
          let contents = makePath () in
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
      
  end
end
