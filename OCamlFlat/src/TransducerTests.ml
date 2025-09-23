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
			Util.header ""
		end
end
