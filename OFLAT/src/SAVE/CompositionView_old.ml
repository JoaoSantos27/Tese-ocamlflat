(*
 * CompositionView.ml
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
 *  Written by Carolina Duarte
 *)


open OCamlFlat
open BasicTypes
open JS
open ViewUtil
open Lang
open Cytoscape
open AutomatonView

module CompositionView = 
struct
   open Composition

  class model (arg: t Arg.alternatives) =
        object(self) inherit Composition.model arg as super
  end
      

end

