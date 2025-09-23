(*
 * TransducerSupport.ml
 *
 * This file is part of the OCamlFLAT library
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
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jul/2025 (amd) - New file.
 *)

(*
 * Description: Supporting types and functions for Finite-state transducers.
 *)

open BasicTypes

module TransducerBasics =
struct
	type transition4 = state * symbol * symbol * state
	type transitions4 = transition4 set
	type t = {
		inAlphabet : symbols;
		outAlphabet : symbols;
		states : states;
		initialState : state;
		transitions : transitions4;
		acceptStates : states
	}

	type configuration = state * word * word
	type configurations = configuration set
	type path = configuration list
	type trail = configurations list

	let kind = "transducer"

	let fst_zero: t = {
		inAlphabet = Set.empty;
		outAlphabet = Set.empty;
		states = Set.make [draftState];
		initialState = draftState;
		transitions = Set.empty;
		acceptStates = Set.empty
	}
end

module TransducerConversions =
struct
	open TransducerBasics

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then
			fst_zero
		else {
			inAlphabet = JSon.fieldSymbolSet j "inAlphabet";
			outAlphabet = JSon.fieldSymbolSet j "outAlphabet";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			transitions = JSon.fieldQuadsSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates"
		}

	let toJSon0 (rep: t): JSon.t =
		JSon.makeAssoc [
			("inAlphabet", JSon.makeSymbolSet rep.inAlphabet);
			("outAlphabet", JSon.makeSymbolSet rep.outAlphabet);
			("states", JSon.makeStateSet rep.states);
			("initialState", JSon.makeState rep.initialState);
			("transitions", JSon.makeQuadsSet rep.transitions);
			("acceptStates", JSon.makeStateSet rep.acceptStates)
		]
	
	let toJSon2 (id: Entity.t) (rep: t): JSon.t =
		 JSon.append (Entity.toJSon id) (toJSon0 rep)
	
	let toJSon (rep: t): JSon.t =
		 toJSon2 (Entity.dummyId kind) rep
end

module TransducerBasicFunctions =
struct
	open TransducerBasics
	open TransducerConversions

	let make2 (arg: t Arg.alternatives) validate: Entity.t * t =
		Entity.make2 arg fromJSon kind validate

	let make (arg: t Arg.alternatives) validate: t =
		snd (make2 arg validate)

	let show (rep: t): unit =
		let j = toJSon rep in
			JSon.show j

	let show2 (id: Entity.t) (rep: t): unit =
		let j = toJSon2 id rep in
			JSon.show j
end

module TransducerX =
struct
	open TransducerBasics

	type transition4X = state * symbolX * symbolX * state
	type tx = {
		inAlphabet : symbolX list;
		outAlphabet : symbolX list;
		states : state list;
		initialState : state;
		transitions : transition4X list;
		acceptStates : state list
	}

	let transitions4I (l: transition4X list): transitions4 =
		let trans4I (a,b,c,d): transition4 = (a, symbI b, symbI c, d) in
			Set.make (List.map trans4I l)
			
	let transitions4X (s: transitions4): transition4X list =
		let trans4X (a,b,c,d): transition4X = (a, symbX b, symbX c, d) in
			List.map trans4X (Set.toList s)

	let internalize (fst: tx): t = {
		inAlphabet = symbolsI fst.inAlphabet;
		outAlphabet = symbolsI fst.outAlphabet;
		states = Set.make fst.states;
		initialState = fst.initialState;
		transitions = transitions4I fst.transitions;
		acceptStates = Set.make fst.acceptStates
	}
	
	let externalize (fst: t): tx = {
		inAlphabet = symbolsX fst.inAlphabet;
		outAlphabet = symbolsX fst.outAlphabet;
		states = Set.toList fst.states;
		initialState = fst.initialState;
		transitions = transitions4X fst.transitions;
		acceptStates = Set.toList fst.acceptStates
	}
end

module TransducerLearnOCaml =
struct
	open TransducerBasics
	open TransducerX

	let moduleName =
		"Transducer"

	let xTypeName =
		"finiteAutomaton"

	let transs4XD (l: transition4X list): string =
		let t2d (a,b,c,d) =
			Printf.sprintf "(%s, %s, %s, %s)"
			(stateXD a)
			(symbXD b)
			(symbXD c)
			(stateXD d)
		in listD t2d l

	let solution (name: string) (rep: t): string =
		let repx = externalize rep in
		Printf.sprintf {zzz|
		%s{
			inAlphabet = %s;
			outAlphabet = %s;
			states = %s;
			initialState = %s;
			transitions = %s;
			acceptStates = %s
		}
		|zzz}	(* please, do not change this line *)
			(FiniteEnumerationLearnOCaml.displayHeader name xTypeName)
			(symbolsXD repx.inAlphabet)
			(symbolsXD repx.outAlphabet)
			(statesXD repx.states)
			(stateXD repx.initialState)
			(transs4XD repx.transitions)
			(statesXD repx.acceptStates)


	let prelude : string =
		Printf.sprintf {zzz|
		type symbol = %s
		type state = string
		type finiteAutomaton = {
			alphabet : symbol list;
			states : state list;
			initialState : state;
			transitions : (state * symbol * state) list;
			acceptStates : state list
		}
		|zzz}	(* please, do not change this line *)
			symbolTypeName

	let example : JSon.t =
		JSon.parse {|
		{
			kind : "transducer",
			description : "this is an example",
			name : "fst example",
			alphabet: ["w", "z"],
			states : ["START", "X", "Z"],
			initialState : "START",
			transitions : [
				["START", "w", "w", "X"], ["X", "z", "z", "X"]
			],
			acceptStates : ["Z"]
		}
		|}	(* please, do not change this line *)
end

module TransducerSupport =
struct
	include TransducerBasics
	include TransducerConversions
	include TransducerBasicFunctions
	include TransducerLearnOCaml
end
