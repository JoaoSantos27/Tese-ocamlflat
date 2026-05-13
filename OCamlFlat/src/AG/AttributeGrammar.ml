(*
 * AttributeGrammar.ml
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
 *  Written by Pedro Bailão (pb)
 *)

(*
 * ChangeLog:
 *
 * ???/2025 (pb) - ....
 * feb/2025 (amd) - New file "AttributeGrammar.ml".
 *)

(*
 * Description: Attribute grammar functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes

module AttributeGrammarPrivate =
struct
	open AttributeGrammarSupport

	let ag2cfg (rep: t): ContextFreeGrammarBasic.t =
		ContextFreeGrammarBasic.cfg_zero
	
	let validateAG (name: string) (rep: t): unit =
		()
	
	let validate (name: string) (rep: t): unit =
		let cfg = ag2cfg rep in
			ContextFreeGrammarPrivate.validate "_" cfg;
			validateAG "_" rep

	let accept (rep: t) (w: word): bool =
		false
end

module AttributeGrammar =
struct
	include AttributeGrammarSupport
	open AttributeGrammarPrivate

	(* Make *)
	let make2 (arg: t Arg.alternatives): Entity.t * t = make2 arg validate
	let make (arg: t Arg.alternatives): t = make arg validate

	(* Exercices support *)
	let checkProperty (fa: t) (prop: string) =
		match prop with
			| _ -> Model.checkProperty prop
	let checkExercise ex fa = Model.checkExercise ex (accept fa) (checkProperty fa)	
	let checkExerciseFailures ex fa = Model.checkExerciseFailures ex (accept fa) (checkProperty fa)

	(* Ops *)
	let stats = Model.stats
	let accept = accept
end

module AttributeGrammarTop =
struct
	open AttributeGrammar
end

open AttributeGrammarTop

module AttributeGrammarSupportTests : sig end =
struct
	open AttributeGrammar
	
	let active = false
	
	let ag = {| {
		kind : "attribute grammar",
		description : "",
		name : "ag",
		alphabet : ["[", "]"],
		variables : ["S"],
		inherited : [],
		synthesized : [],
		initial : "S",
		rules : [ "S -> [S] {l(S) = 2; l(S) = 'ole'; l(S0) = l(S1)} [123 + 56; 56; 'ola']",
				  "S -> SS {l(S) = l(S1) + 3 + 'ola' + l(S12345)}",
				  "S -> ~ {l(S0) = 6}",
				  "S -> ~ {l(S0) = 1+2*3<T>F<=T>=5=T<>T+(1*2)}"
				]
	} |}

	let test0 () =
		let j = JSon.parse ag in
		let g = fromJSon j in
		let h = toJSon g in
			JSon.show h

	let test1 () =
		let g = make (Arg.Text ag) in
		let h = toJSon g in
			JSon.show h

	let runAll =
		if Util.testing active "AttributeGrammarSupport" then begin
			Util.header "test0";
			test1 ();
			Util.header ""
		end
end

