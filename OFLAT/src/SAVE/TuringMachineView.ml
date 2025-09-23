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
 *  Written by Carlos Freitas
 *)

open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open ViewUtil
open Lang
open Cytoscape
open AutomatonView

module TuringMachineView =
struct
	open TuringMachine

	(** Auxiliar Methods **)

	class virtual model (arg: (t, tx) Arg.alternatives) =
		object(self) 
		inherit AutomatonView.model arg as abstractAutomaton
		inherit TuringMachine.model arg as super
    
	end
end
