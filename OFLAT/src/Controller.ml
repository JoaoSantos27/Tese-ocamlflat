(*
 * Controller.ml
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

(* 
 * Description: Controller component of the application.
 *)


open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open ViewUtil
open AutomatonView
open FiniteAutomatonView
open TransducerView
open PushdownAutomatonView
open RegularExpressionView
open ContextFreeGrammarView
open GrammarView
open TuringMachineView
open CompositionView
open Lang
open Listeners
open HtmlPageClient
open StateVariables
open String
open Random
open Grammar

let createModelPrep titleTxt otherTxt textAreaString okAction =
  let modelContent = HtmlPageClient.editModelContent titleTxt otherTxt textAreaString okAction in
  HtmlPageClient.setModal (Js.Unsafe.coerce modelContent);
  HtmlPageClient.showModalWindow ()

let createModelPrepFA textAreaString okAction =
  createModelPrep (Lang.i18nMainTitle1()) (Lang.i18nInstructionsAutomaton()) textAreaString okAction

let createModelPrepPDA textAreaString okAction =
  createModelPrep (Lang.i18nMainTitlePDA()) (Lang.i18nInstructionsAutomaton()) textAreaString okAction

let createModelPrepRE textAreaString okAction =
  createModelPrep (Lang.i18nMainTitle2()) "" textAreaString okAction

let createModelPrepComp textAreaString okAction =
  createModelPrep (Lang.i18nMainTitleComp()) "" textAreaString okAction
  
let createModelPrepCFG textAreaString okAction =
  createModelPrep (Lang.i18nMainTitle4()) (Lang.i18nInstructionsCFG()) textAreaString okAction

let createModelPrepGR textAreaString okAction =
  createModelPrep (Lang.i18nMainTitle5()) (Lang.i18nInstructionsGR()) textAreaString okAction

module CtrlUtil = struct
  let changeToControllerCtrlRight = ref (fun () -> ())
  let changeToControllerCtrlLeft = ref (fun () -> ())

  let oneBox cy = 
    !changeToControllerCtrlRight();
    HtmlPageClient.oneBox();
    Cytoscape.fit cy
    
  let twoBoxes cy =
    HtmlPageClient.twoBoxes();
    Cytoscape.fit cy
end

class virtual controller =
  object(self)

    val mutable layoutDir = None
    val mutable updateType = None
    val virtual cy : Cytoscape.cytoscape Js_of_ocaml.Js.t option
    val mutable activeProm = Lwt.return();

    val listOnlyAutomataButtons = ["backwards"; "start"; "forward"; "selectRegex"]
    val listOnlyTMButtons = ["backwards"; "start"; "forward"]  (* ML *)
    val listOnlyExpressionButtons = ["selectFA"; "start"]
    val listOnlyPDAButtons = ["selectPDA"]
    val listOnlyCFGConvertButtons = ["selectCFG"]
    val listOnlyGRConvertButtons = ["selectGR"]
    val listOnlyTMConvertButtons = ["selectTM"]	(* carolina *)
    val listOnlyTM2TapesConvertButtons = ["selectTM2Tapes"]
    val listOnlyCFGButtons = ["testing"; "trace"; "generate"; "backwards"; "start"; "forward"]
    val listOnlyGRButtons = ["testing"; "trace"; "generate"]
    val listOtherButtons = ["testing"; "trace"; "generate"; "fitGraph"; "editModel"; "exportModel"]
    val listDisCompButtons = ["trace"; "backwards"; "start"; "forward"; "autoAccept"]

    method locked : bool = false
    (* TM *)
    (* method addNode (x : int) (y : int) (st: state) : unit = Error.fatal "addNode" *)
	method addNode (x : int) (y : int) (initial : bool) (final : bool): unit = Error.fatal "addNode"
    method eliminateNode (st: state) : unit = Error.fatal "eliminateNode"
    method changeTab : unit = Error.fatal "changeTab"
    method getTab : bool = Error.fatal "getTab"
    method startGraph : unit = Error.fatal "startGraph"
    method defineExample : unit = Error.fatal "defineExample"
    method defineExample2 : unit = Error.fatal "define Example cy 2"
    method defineInformationBox : unit = Error.fatal "defineInformationBox"
    method convertToRegExp : RegularExpressionView.model = Error.fatal "convert to regex"
    method convertToFA : FiniteAutomatonView.model = Error.fatal "convert to FA"
    method convertToFST : TransducerView.model = Error.fatal "convert to FST"
    method convertToCFG : ContextFreeGrammarView.model = Error.fatal "convert to CFG"
    method convertToGR : GrammarView.model = Error.fatal "convert to GR"
    method convertToTM_SingleTape : TuringMachineView.model = Error.fatal "convert to TM single tape"
    method convertToTM_DualTape : TuringMachineView.model = Error.fatal "convert to TM dual tape"
    method convertToPDA : PushdownAutomatonView.model = Error.fatal "convert to PDA"
    method createTransition (s1 : state) (s2 : state) : unit = Error.fatal "createTransition"
    method eliminateTransition ((c1: state), (c2: string), (c3: state)): unit = Error.fatal "eliminateTransition"
    method defineMinimize (listColors: string array) (number : int) : unit = Error.fatal "minimize"
    method addFinalNode (x : int) (y : int) (st: state): unit = Error.fatal "addFinalNode"
    method turnFinalNode (st: state): unit = Error.fatal "turnFinalNode"
    method removeFinalNode (st: state): unit = Error.fatal "removeFinalNode"
    method addInitialNode (st: state) : unit = Error.fatal "addInitialNode"
    method autoAccept : bool Lwt.t = Error.fatal "accept"
    method getModel : string = ""
    method getAutomaton: AutomatonView.model = Error.fatal "addInitialNode"
    method changeToEditModelMode: unit = Error.fatal "changeToEditMode"
    method getWords (number: int): unit = Error.fatal "getWords"
    method getNewSentence = Js.string ("")
    method startStep (word: string): unit = Error.fatal "startStep"
    method nextStep: unit  = Error.fatal "nextStep"  
    method backStep: unit = Error.fatal "backStep"
    method model: Model.model = Error.fatal "model"
    method replicateOnLeft: unit = Error.fatal "replicateOnLeft"
    method setUpdateType (s : string) : unit = Error.fatal "setUpdateType"
    method getUpdateType : string option = updateType
    method updateRight: unit = ()
    method toggleAcceptanceCriteria: unit = Error.fatal "toggleAcceptanceCriteria"
    method changeInitialStackSymbol: unit = Error.fatal "changeInitialStackSymbol"
    method convertAcceptStates: PushdownAutomatonView.model option = Error.fatal "convertAcceptStates"
    method convertEmptyStackAccept: PushdownAutomatonView.model option = Error.fatal "convertEmptyStackAccept"
    method showTrace (word: string) : unit = Error.fatal "trace"
    method checkWord (word: string) : unit = Error.fatal "checkword"
    method model2Str : string = Error.fatal "model2Str"


    method getCy: Cytoscape.cytoscape Js_of_ocaml.Js.t = 
      match cy with
        | None -> Error.fatal "getCy"
        | Some cy -> cy
    method getCy_opt = cy

    method feedback = ()
    method about = ()

    method printErrors =
      let errors = [] in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors)

    method clearExerciseAction = 
      HtmlPageClient.oneBox ();
      HtmlPageClient.clearBox2 ();
      let element = Dom_html.getElementById "cy2" in
        element##.innerHTML := Js.string ""

    method updateButtons = 
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTMConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyGRButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOtherButtons

    method setTitle: unit = HtmlPageClient.defineMainTitle ("")

    method checkHelper (result: bool) ((insideErrors: word set), (outsideErrors: word set), (properties: property set)) : unit = Error.fatal "checkHelper"

    method changeLayoutDir newLayout =
      layoutDir <- Some (newLayout)

    method getLayoutDir : string = 
      match layoutDir with
        | None -> ""
        | Some layoutDir -> layoutDir
      

    method closeRightAction = 
      HtmlPageClient.oneBox ();
      HtmlPageClient.clearBox2 ();
      !CtrlUtil.changeToControllerCtrlRight ()

    method resetStyle = ()

    method returnType = ""

    method operation opName modelKind: unit =
      Js.Unsafe.global##logEntry (Js.string opName) (Js.string modelKind)
    

      method changeProm prom = activeProm <- prom
      method returnProm = activeProm
      method cancelProm = Lwt.cancel activeProm
      method promState = Lwt.state activeProm
      
      method finish = self#cancelProm (*upgradeable*)  

    method getFA : FiniteAutomatonView.model = Error.fatal "get automata"
    method getFST : TransducerView.model = Error.fatal "get FST"
    method getPDA : PushdownAutomatonView.model = Error.fatal "get PDA"
    method getRE : RegularExpressionView.model = Error.fatal "get RE"
    method getCFG : ContextFreeGrammarView.model = Error.fatal "get CFG"

    method getGR : GrammarView.model = Error.fatal "get GR"
    method handleOp (operation: string) : unit = Error.fatal "GR operations"
    
    (*  ????? getTM  *)
    method getComp : CompositionView.model = Error.fatal "get Comp"  (* carolina *)
    method getExercise : Exercise.exercise = Error.fatal "get RE"
    method getResultTree : bool = Error.fatal "get Result Tree"
    method getWordAsList () : word = Error.fatal "get word as list"

    (* method editModel : unit = Error.fatal "edit model" *)
    
    method box2CFGShow (f : ContextFreeGrammarLL1.transformation) : unit = Error.fatal "show cfg transformation box 2"

    method box2GRShow (g : Grammar.model) : unit = Error.fatal "show gr transformation box 2"
    
    method renameState (s : state) : unit = Error.fatal "rename state"
end

class textController (s : bool) = 
  object(self) inherit controller as super
    
    val side = s
    
    val cy = 
      let cyString = if s then "cy2" else "cy" in
      let cyElement = Dom_html.getElementById_opt cyString in
      match cyElement with
      | None -> None
      | Some a -> Some (Cytoscape.initCy cyString)

    method returnType = ""
    
    method defineExample = ()
    
    method replicateOnLeft =
      !CtrlUtil.changeToControllerCtrlLeft ()

    method setUpdateType s =
      updateType <- Some s

    method feedback =
      super#operation "Feedback" "Feedback";
      HtmlPageClient.oneBox ();
      HtmlPageClient.disableButtons (self#returnType);
      HtmlPageClient.feedback()
  
    method about =  (* VER SE É PRECISO AMD *****)
    super#operation "About" "About";
      HtmlPageClient.oneBox ();
      StateVariables.changeCy1ToText();
      HtmlPageClient.disableButtons (self#returnType);
      HtmlPageClient.about()

end

module Ctrl = struct 
  let textCtrl s = new textController s

  let ctrlL = ref (textCtrl false)
  let ctrlR = ref (textCtrl true)

  let changeCtrlL (nc: controller) =
    ctrlL := nc;;

  let changeCtrlR (nc: controller) =
    ctrlR := nc;;

  let runOp (op: string) =
    !ctrlL#handleOp op;;

  let _ = changeCtrlL (textCtrl false);
          changeCtrlR (textCtrl true);
          Listeners.runOp := runOp;;

  CtrlUtil.changeToControllerCtrlRight := fun () -> (changeCtrlR (textCtrl true));;
  CtrlUtil.changeToControllerCtrlLeft := fun () -> (changeCtrlL (textCtrl false));;
end
