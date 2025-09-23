(*
 * Listeners.ml
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
 * Description: Part of the controller that is acessible from the
 * Visualizer (HtmlPageClient). Using this module, we avoid the need to use
 * mutually recursive modules.
 *)
open OCamlFlat.BasicTypes

module Listeners = 
struct
	let save = ref (fun () -> ()) (*carolina*)
  let saveModel = ref (fun () -> ())
	let closeLeftListener = ref (fun () -> ())
	let closeRightListener = ref (fun () -> ())
	let openEntityListener = ref (fun (txt : string) -> ())
	let defineInformationBoxListener = ref (fun () -> ())
	let showModelListener = ref (fun () -> ())
	let createModelListener = ref (fun () -> ())
	let updateRightListener = ref (fun () -> ())
	let editModelListener = ref (fun () -> ())
	let runOp = ref (fun (op : string) -> ())
end


module ListenersAutomaton = 
struct
	include Listeners
	let removeNode = ref (fun (node : state) -> ())
	let turnFinal = ref (fun (node : state) -> ())
	let addNode = ref (fun (x : int) (y : int) -> ())
	let removeTypeFinal = ref (fun (node : state) -> ())
	let turnNodeInitial = ref (fun (node : state) -> ())
	let addInitialNode = ref (fun (x : int) (y : int) -> ())
	let addFinalNode = ref (fun (x : int) (y : int) -> ())
	let addTransition = ref (fun (src : state) (trg : state) -> ())
	let removeTransition = ref (fun (srcId : state) (trgId : state) (symb : state) -> ())
	let renameNodeListener = ref (fun (state : state) -> ())
	let paintAllProductivesListener = ref (fun () -> ())
	let paintAllReachableListener = ref (fun () -> ())
	let paintAllUsefulListener = ref (fun () -> ())
	let clearAutoListener = ref (fun () -> ())
	let showTable = ref (fun () -> ())
	let editModelListener = ref (fun () -> ())
end

module ListenersFA = 
struct
	include ListenersAutomaton
	let createModelListener = ref (fun () -> ())
	let changeDirectionListener = ref (fun () -> ())
	let defineInformationBoxListener = ref (fun () -> ())
	let getDeterministicListener = ref (fun () -> ())
	let defineMinimizedListener = ref (fun () -> ())
	let cleanUselessListener = ref (fun () -> ())

end

module ListenersTM = 
    struct
      include ListenersAutomaton
      let createModelListener = ref (fun () -> ())
      (*let changeDirectionListener = ref (fun () -> ())*)
      let paintAllProductivesListener = ref (fun () -> ())
      let paintAllReachableListener = ref (fun () -> ())
      let paintAllUsefulListener = ref (fun () -> ())
      let cleanUselessListener = ref (fun () -> ())
      let getDeterministicListener = ref (fun () -> ())
      let clearAutoListener = ref (fun () -> ())
    end

module ListenersPDA = 
struct
	include ListenersAutomaton
	let createModelListener = ref (fun () -> ())
	let cleanUselessListener = ref (fun () -> ())
	let toggleAcceptanceCriteria = ref (fun () -> ())
	let changeInitialStackSymbol = ref (fun () -> ())
	let convertAcceptStates = ref (fun () -> ())
	let convertEmptyStackAccept = ref (fun () -> ())
end

module ListenersRE =
struct
	include Listeners
	let createModelListener = ref (fun () -> ())
	let resultCountListener = ref (fun () -> ())
	let previousTreeListener = ref (fun () -> ())
	let nextTreeListener = ref (fun () -> ())
	let defineNumberTreesListener = ref (fun () -> ())
	let changeDirectionListener = ref (fun () -> ())
	let editModelListener = ref (fun () -> ())
	
end

module ListenersCFG =
struct
	include Listeners
	let createModelListener = ref (fun () -> ())
	let cleanCFGListener = ref (fun () -> ())
	let removeLeftRecursionListener = ref (fun () -> ())
	let leftFactoringListener = ref (fun () -> ())
	let tablesListener = ref (fun () -> ())
	let parsingTableListener = ref (fun () -> ())
	let generateCFGListener = ref (fun () -> ())
	let recursiveDescedentParserListener = ref (fun () -> ())
	let simpleToggleListener = ref (fun () -> ())
	let removeEpsilonListener = ref (fun () -> ())
	let removeUnitListener = ref (fun () -> ())
	let previousNewCFGListener = ref (fun () -> ())
	let nextNewCFGListener = ref (fun () -> ())
	let transformLL1Listener = ref (fun () -> ())
	let editModelListener = ref (fun () -> ())
end 


module ListenersGR =
struct
    include Listeners
		let createModelListener = ref (fun () -> ())
    let tablesListener = ref (fun () -> ())
    let parsingTableListener = ref (fun () -> ())
    let generateGRListener = ref (fun () -> ())
    let editModelListener = ref (fun () -> ())
    let grOps = ref (fun (_: string) -> ())
end

let undefinedListener () = JS.JS.alertStr "undefined listener"

module ListenersLR =
struct
	let	buildLR0DiagramListener = ref undefinedListener
	let	buildSLR1DiagramListener = ref (fun () -> ())
	let	buildLR1DiagramListener = ref (fun () -> ()) 
	let	buildLALR1DiagramListener = ref (fun () -> ()) 
	let	buildLR0TableListener = ref (fun () -> ()) 
	let	buildSLR1TableListener = ref (fun () -> ())    
	let	buildLR1TableListener = ref (fun () -> ())   
	let	buildLALR1TableListener = ref (fun () -> ())
	let acceptLR0Listener = ref (fun () -> ())
	let	acceptSLR1Listener = ref (fun () -> ())
	let	acceptLR1Listener = ref (fun () -> ())
	let	acceptLALR1Listener = ref (fun () -> ())
end 

module ListenersComp =  (*carolina*)
struct
	include Listeners
	let createModelListener = ref (fun () -> ())
	let editModelListener = ref (fun () -> ())
	let showTreeNode = ref (fun (model : string) -> ())
	
end

module ListenersEXER = 
struct
	include Listeners
	let checkExerciseListener = ref (fun () -> ())
	let clearExerciseListener = ref (fun () -> ())
end
