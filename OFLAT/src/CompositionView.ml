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

      val mutable accepted : bool = false
      val mutable visitedConfigs : int = 0
      val mutable exactResult : bool = false
      val mutable acceptTime : float = 0.0
      val mutable bestPath = []

      method staticAccept word =
            let acc = self#accept word in
            let (exact, configVisited, time) = Model.stats() in
            JS.log("set initial step");
            self#setConfigsAndBestPath2 acc exact time configVisited;

      method private setConfigsAndBestPath2 acc exact time configVisited=
            accepted <- acc;
            exactResult <- exact;
            acceptTime <- time;
            visitedConfigs <- configVisited

      method returnStats =
            (accepted, visitedConfigs, exactResult, acceptTime)      

  end
      

end

