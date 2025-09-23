(*
 * FiniteAutomatonGraphics.ml
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


module type TuringMachineGraphicsSig =
sig
  type transition = state * symbol * state * symbol * direction
  type transitions = transition set
  type t = TuringMachine.t
  type tx = TuringMachine.tx

  val modelDesignation: unit -> string (* a funtion required for module recursive call *)

  class model :
    (t, tx) Arg.alternatives ->
      object

    (* Functions inherited by TuringMachine*)

      method id: Entity.t
      method errors: string list
      method toJSon: JSon.t
      method toJSon2: JSon.t
      method show : unit
	  method show2 : unit

      method representation: t
      method representationx: tx

	(* Kind *)
	method isFiniteAutomaton : bool
	method isRegularExpression : bool
	method isContextGrammar : bool
	method isContextFreeGrammar : bool
	method isPushdownAutomaton : bool
	method isTuringMachine : bool

      method acceptOld: word -> bool
      method accept: word -> bool
      method acceptFull: word -> bool * configuration list * configuration set list
      method generate: int -> words	
      method downgradeModelToFiniteAutomaton : FiniteAutomaton.model
      method reachable: state -> states
      method productive: states
      method getUsefulStates: states
      method getUselessStates: states
      method isDeterministic: bool
      method areAllStatesUseful: bool
      method cleanUselessStates: TuringMachine.t
      method acceptLB: word -> bool
      method acceptFullLB: word -> bool * configuration list * configuration set list
      method isLB: bool
      method convertToStopCriteria: TuringMachine.model
      method hasState: state -> bool
      method hasTransition: transition -> bool
      method isFinal: state -> bool
      method isInitial: state -> bool
      method addState: state -> TuringMachine.t
      method addInitialState: state -> TuringMachine.t
      method addFinalState: state -> TuringMachine.t
      method removeState: state -> TuringMachine.t
      method changeStateToInitial: state -> TuringMachine.t
      method changeStateToFinal: state -> TuringMachine.t
      method changeStateFromFinal: state -> TuringMachine.t
      method addTransition: transition -> TuringMachine.t
      method removeTransition: transition -> TuringMachine.t
      method renameState: state -> state-> TuringMachine.t
    
    (* Exercices support *)
      method checkProperty: string -> bool
      method checkExercise: Exercise.exercise -> bool
      method checkExerciseFailures: Exercise.exercise -> words * words * properties

    (* Learn-OCaml support *)
      method moduleName: string
      method xTypeName: string
      method xTypeDeclString: string
      method toDisplayString: string -> string
      method example: JSon.t

    (* TuringMachineGraphics Methods*)

      method numberStates: int
      method numberTransitions: int
      method isSimulating: bool
      method isOver: bool

      method inputEdges: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method inputNodes: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method drawExample: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit 

      method paint: Cytoscape.cytoscape Js_of_ocaml.Js.t -> state -> bool -> bool -> unit
      method productivePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method reachablePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method usefulPainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit

      method addNode: state -> model
      method addInitialNode: state -> model
      method addFinalNode: state -> model
      method eliminateNode: state -> model
      method changeToFinal: state -> model
      method removeFinal: state -> model
      method renameNode: state -> string -> model
      method newTransition: transition -> model
      method eliminateTransition: transition-> model

      method startAccept: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method next: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method back: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method setInitialStep: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
			method setStep: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit

      method changeTheTestingSentence: string -> unit
      method changeSentence: configuration -> unit
      method newSentence1: string

			method changeToEditModelMode: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method cleanUselessStatesCy: Cytoscape.cytoscape Js_of_ocaml.Js.t -> model

      method makeLabel: symbol -> symbol -> direction -> string
      method dissectTransitionInput: string -> (string * char * char)

  end
end

module rec TuringMachineGraphics: TuringMachineGraphicsSig =
struct
  type transition =
    state	(* state *)
    * symbol	(* consumed input symbol *)
    * state	(* next state *)
    * symbol	(* write input symbol *)
    * direction	

  type transitions = transition set
  type t = TuringMachine.t
  type tx = TuringMachine.tx


  (* ******************Auxiliar Functions****************** *)

  let modelDesignation () = "turing machine"
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

  let direction2string direction = if direction = L then "L" else "R"

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

  let getCurrConfigFromBestPath bestPath position = if (bestPath <> []) && (not ((List.length bestPath) <= position)) then Some (List.nth bestPath position) else None


  (* ******************Function Method Functions****************** *)

  (*-------------makeLabel-------------*)
  let makeLabel rsymb wsymb direction =
    let symbs = String.concat "/" [(symb2str rsymb); (symb2str wsymb)] in
      String.concat "/" [symbs; (direction2string direction)]

  
  (*-------------inputEdges-------------*)
  let transformTransitions trs = Set.map (fun (rstate, rsymb, nstate, wsymb, direction) -> (rstate, (makeLabel rsymb wsymb direction), nstate)) trs
  let inputEdges cy trs = Set.iter (fun el -> (Cytoscape.addEdgeGeneral cy el) ) (transformTransitions trs)


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


  (*-------------cleanUselessStatesCy-------------*)
  let cleanUselessStatesCy cy cleanUSt =
    (*Cytoscape.resetStyle cy Cytoscape.faStyle;
    Set.iter (fun el -> paintUseful cy el) self#getUselessStates;*)
    new model (Arg.Representation cleanUSt) 


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
    let (_,revLeft,right) = config in
    let notEmptyRight = if List.length right = 0 then [empty] else right in
    let left = TuringMachine.reverse revLeft in
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
    | Some (currBestState,_,_) -> paint cy currBestState (isFinalState criteria acceptSts currBestState) true finished isOver accepted criteria

  let paintCurrentStates cy configs criteria acceptSts bestPath position finished isOver accepted =
    Cytoscape.resetStyle cy Cytoscape.faStyle;
    paintStates cy (TuringMachine.configurationGet1 configs) criteria acceptSts finished isOver accepted;
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

  let getConfigsWithState state configs =
    Set.filter (fun (st, _, _) -> st = state) configs

  let buildTape left right = 
(* ML   let rightSide = String.cat "|" (word2str right) in *)
    let rightSide = "|" ^ (word2str right) in
    (word2str (TuringMachine.reverse left)) ^ rightSide

  let processNodeConfig (configs: TuringMachine.configurations) state =
    let configsOfState = getConfigsWithState state configs in
    Set.mapi (fun i (state, left, right) -> buildConfigMenu (buildIdFromState state i, buildTape left right)) configsOfState

  let processConfigMenus (configs:TuringMachine.configurations) =
    let states = TuringMachine.configurationGet1 configs in
    Set.flatMap (processNodeConfig configs) states

  let menuConfigPDA (configs: TuringMachine.configurations) = 
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
    let states = TuringMachine.configurationGet1 configs in
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
    Set.fold_left (fun c (st,_,_) -> if (state2str st) = (data_fromName node "id") then c+1 else c) 0 configs

  let buildConfigsCount (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) configs = 
    let nodes = List.filter (fun node -> (data_fromName node "id") <> "transparent") (Cytoscape.getAllNodes cy) in
      List.map (fun node -> buildPopper (getConfigCountForNode node configs) node) nodes

  let updateAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##update ()) poppers
  
  let destroyAllPoppers (poppers: Cytoscape.popper Js_of_ocaml.Js.t list) =
    List.iter (fun popper -> popper##destroy ()) poppers

  let isSimulating bpath =
    if Set.size (Set.make bpath) = 0 then false else true

  let getInitConfig rep sentence =
    let word = str2word (String.init (List.length !sentence) (fun i -> List.nth !sentence i)) in
      (rep.initialState, [empty], word@[empty])

      
  class model (arg: (t, tx) Arg.alternatives) =
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

      method addNode node = 
        new model (Arg.Representation (self#addState node))

      method addInitialNode node =
        new model (Arg.Representation (self#addInitialState node))

      method addFinalNode node =
        new model (Arg.Representation (self#addFinalState node))

      method eliminateNode node = 
        new model (Arg.Representation (self#removeState node))
      
      method changeToFinal node =
        new model (Arg.Representation (self#changeStateToFinal node))
      
      method removeFinal node =
        new model (Arg.Representation (self#changeStateFromFinal node))

      method renameNode node newNode =
        new model (Arg.Representation (self#renameState node (state newNode)))

      method newTransition trs = 
        new model (Arg.Representation (self#addTransition trs))

      method eliminateTransition trs = 
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
        let (acc, bestPath, configsList) = self#acceptFull (List.map char2symb !sentence) in
        let (exact, configs, time) = Model.stats() in
        self#setConfigsAndBestPath configsList bestPath acc exact;
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

      method cleanUselessStatesCy (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) : model = 
        cleanUselessStatesCy cy self#cleanUselessStates

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
 
      method private setConfigsAndBestPath configsList bestPathAutomaton acc exact =
        let rec setConfigs configs position =
          match configs with
          | [] -> ()
          | configs::cs ->
            steps.(position) <- configs;
            setConfigs cs (position+1)
        in
          setConfigs configsList 0;
          bestPath <- bestPathAutomaton;
          accepted <- acc;
          finished <- exact
  
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
                if Set.belongs (state node) (TuringMachine.configurationGet1 configs) then
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
  end
end
