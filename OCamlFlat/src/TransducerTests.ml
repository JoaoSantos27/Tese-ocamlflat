(*
 * TransducerTests.ml
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
 *  Written by João Santos (js)
 *)

(*
 * ChangeLog:
 *
 * jul/2025 (amd) - New file.
 *)

(*
 * Description: Finite-state transducer testing.
 *)

open BasicTypes

module TransducerTests : sig end =
struct
  open FiniteAutomaton
	open Transducer
	
	let active = true
	
	let fstIdentity = {| {
		kind : "transducer",
		description : "Identity transducer",
		name : "fstId",
		inAlphabet : ["a", "b"],
		outAlphabet : ["a", "b"],
		states : ["S"],
		initialState : "S",
		transitions : [["S","a","a","S"], ["S","b","b","S"]],
		acceptStates : ["S"]
	} |}

	let fst1_Identity = {| {
		kind : "transducer",
		description : "1: Deterministic, Complete, Mealy",
		name : "fst1",
		inAlphabet : ["a", "b"],
		outAlphabet : ["c", "d"],
		states : ["S"],
		initialState : "S",
		transitions : [
			["S","a","c","S"],
			["S","b","d","S"]
		],
		acceptStates : ["S"]
	} |}

	let fst2_Moore = {| {
		kind : "transducer",
		description : "2 Moore",
		name : "fst2",
		inAlphabet : ["a", "b"],
		outAlphabet : ["x", "y"],
		states : ["S","A"],
		initialState : "S",
		transitions : [
			["S","a","x","A"],
			["S","b","x","A"],
			["A","a","y","S"],
			["A","b","y","S"]
		],
		acceptStates : ["S","A"]
	} |}

	let fst3_NonDeterministic = {| {
		kind : "transducer",
		description : "3: Nondeterministic  (2 transitions with same input)",
		name : "fst3",
		inAlphabet : ["a"],
		outAlphabet : ["x","y"],
		states : ["S"],
		initialState : "S",
		transitions : [
			["S","a","x","S"],
			["S","a","y","S"]
		],
		acceptStates : ["S"]
	} |}

	let fst4_Incomplete = {| {
		kind : "transducer",
		description : "4: Deterministic but incomplete (missing 'b' on S)",
		name : "fst4",
		inAlphabet : ["a", "b"],
		outAlphabet : ["x"],
		states : ["S"],
		initialState : "S",
		transitions : [
			["S","a","x","S"]
		],
		acceptStates : ["S"]
	} |}

	let fst5_EpsConflict = {| {
		kind : "transducer",
		description : "5: Nondeterministic due to epsilon with conflicting outputs",
		name : "fst5",
		inAlphabet : ["a"],
		outAlphabet : ["x","y"],
		states : ["S","A","B"],
		initialState : "S",
		transitions : [
			["S","~","x","A"],
			["S","~","y","B"],
			["A","a","x","A"],
			["B","a","y","B"]
		],
		acceptStates : ["A","B"]
	} |}

	let fst6_EpsDeterministic = {| {
		kind : "transducer",
		description : "6: ε-closure but deterministic (same epsilon output)",
		name : "fst6",
		inAlphabet : ["a"],
		outAlphabet : ["x","y"],
		states : ["S","A","B","C"],
		initialState : "S",
		transitions : [
			["S","~","x","A"],
			["A","~","x","B"],
			["A","~","x","C"],
			["B","a","x","B"]
		],
		acceptStates : ["B","C"]
	} |}

	let fst7_NotMoore = {| {
		kind : "transducer",
		description : "7: Deterministic & complete but not Moore (output depends on input)",
		name : "fst7",
		inAlphabet : ["a","b"],
		outAlphabet : ["x","y"],
		states : ["S"],
		initialState : "S",
		transitions : [
			["S","a","x","S"],
			["S","b","y","S"]
		],
		acceptStates : ["S"]
	} |}


	let test0 () =
    let fst: t = make (Arg.Text fstIdentity) in
    show fst

	let test1 () =
		let fst: t = make (Arg.Text fstIdentity) in
		let json: JSon.t = toJSon fst in
			JSon.show json

	let test2 () =
		let json: JSon.t = JSon.parse fstIdentity in
		let fst: t = make (Arg.JSon json) in
		let json: JSon.t = toJSon fst in
			JSon.show json

	let test3 () =
		let json: JSon.t = JSon.parse fstIdentity in
		let fst: t = fromJSon json in
		let json: JSon.t = toJSon fst in
			JSon.show json

	let test4 () =
    let fst: t = make (Arg.Text fst7_NotMoore) in
		let test = isDeterministic fst in
		Printf.printf "%s\n" (string_of_bool test)

	let test5 () =
    let fst: t = make (Arg.Text fst7_NotMoore) in
		let test = isComplete fst in
		Printf.printf "%s\n" (string_of_bool test)
	
	let test6 () =
    let fst: t = make (Arg.Text fst7_NotMoore) in
		let test = isMealyMachine fst in
		Printf.printf "%s\n" (string_of_bool test)

	let test7 () =
    let fst: t = make (Arg.Text fst1_Identity) in
		let input = BasicTypes.word "bab" in
		let ok = Transducer.accept fst input in
		Printf.printf "accept(\"bab\") = %b\n" ok;

		let outs = Transducer.generate fst 3 |> BasicTypes.wordsX in
		Printf.printf "generate(3) = [%s]\n" (String.concat "; " outs)

	let runAll =
		if Util.testing active "Transducer" then begin
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header "test2";
			test2 ();
			Util.header "test3";
			test3 ();
			Util.header "test4";
			test4 ();
			Util.header "test5";
			test5 ();
			Util.header "test6";
			test6 ();
			Util.header "test7";
			test7 ();
			Util.header ""
		end
end
