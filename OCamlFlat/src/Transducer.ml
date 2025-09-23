(*
 * Transducer.ml
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
 * jul/2025 (amd) - Initial skeleton.
 *)

(*
 * Description: Finite-state transducer functionality.
 *)

open BasicTypes

module TransducerAccept =
struct
	open TransducerSupport

	let accept (fst: t) (w: word): bool =
		false

	let acceptFull (fa: t) (w: word) : bool * path * trail =
		(false, [], [])
end

module TransducerGenerate =
struct
	open TransducerSupport
	open TransducerAccept

	let generate (fa: t) (len: int): words =
		Set.empty

	let generateDumb (fa: t) (len: int): words =
		Set.empty
end

module TransducerPrivate =
struct
	open TransducerSupport

	let validate (name: string) (fst: t): unit =
		()	
	
	let asFiniteAutomaton (fst:t): FiniteAutomaton.t =
		FiniteAutomaton.fa_zero

	let cleanUselessStates (fst: t): t =
		fst

	let isClean (fst: t): bool =
		false

	let isDeterministic (fst: t): bool =
		false

	let isComplete (fst: t): bool =
		false

	let isMooreMachine (fst: t): bool =
		false

	let isMeelyMachine (fst: t): bool =
		false
end

module Transducer =
struct
	include TransducerSupport
	open TransducerAccept
	open TransducerGenerate
	open TransducerPrivate

	(* Make *)
	let make2 (arg: t Arg.alternatives): Entity.t * t = make2 arg validate
	let make (arg: t Arg.alternatives): t = make arg validate

	(* Exercices support *)
	let checkProperty (fst: t) (prop: string) =
		match prop with
			| "deterministic" -> isDeterministic fst
			| "complete" -> isComplete fst
			| "moore" -> isMooreMachine fst
			| "meely" -> isMeelyMachine fst
			| "transducer" -> true
			| "finite-state transducer" -> true
			| _ -> Model.checkProperty prop
	let checkExercise ex fst = Model.checkExercise ex (accept fst) (checkProperty fst)	
	let checkExerciseFailures ex fst = Model.checkExerciseFailures ex (accept fst) (checkProperty fst)

	(* Ops *)
	let stats = Model.stats
	let accept = accept
	let acceptFull = acceptFull
	let generate = generate	

	(* Class *)
	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model (make2 arg) as super	
		(* Representation *)
			method representation = representation
		(* Kind *)
			method isTransducer : bool = true
		(* Show *)			
			method toJSon: JSon.t = toJSon representation
			method toJSon2: JSon.t = toJSon2 id representation
			method show: unit = show representation
			method show2: unit = show2 id representation
		(* Ops *)
			method accept (w: word): bool = accept representation w
			method acceptFull (w: word) : bool * path * trail = acceptFull representation w
			method generate (length: int): words = generate representation length
		(* Exercices support *)
			method checkProperty (prop: string) = Util.println["WWW"]; checkProperty representation prop	
		(* Learn-OCaml support *)
			method moduleName = moduleName
			method xTypeName = xTypeName
			method xTypeDeclString : string = prelude
			method toDisplayString (name: string): string = solution name self#representation
			method example : JSon.t = example
		end
end
