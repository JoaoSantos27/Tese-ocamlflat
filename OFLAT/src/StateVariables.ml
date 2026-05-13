(* 
 * StateVariables.ml
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
 * Description: Model component of the application.
 *)

open OCamlFlat

module StateVariables = 
    struct
      (** -------------------------------  State Variables --------------------------------------- **)
      (** Can be "finite automaton", "regular expression", "info", "feedback" or "clean" **)
      let cyType = ref "clean"
  
      (** Can be "finite automaton", "regular expression", "enumeration", "info", "verify" or "clean" **)
      let cy2Type = ref "clean"
    
      let empty = ref "ε"
    
      let enum = ref (new Exercise.exercise (Representation {
        problem = "No exercise yet";
        inside = Set.empty;
        outside = Set.empty;
        properties = Set.empty;
      }))
    
      (** -------------------------------  Functions --------------------------------------- **)
  
      let changeEmpty symb =
        empty := symb
  
      let returnEmpty () = !empty
  
      let returnEnum () =
        !enum
  
      let changeCy1ToText () =
        cyType := "info"
  
      let getCy1Type() =
        !cyType
  
      let getAutomatonType() = "finite automaton"
  
      let getRegexType() = "regular expression"
  
      let getEnumerationType() = "enumeration"
  
      let getInfoType() = "info"
  
       let getFeedbackType() = "feedback"
  
      let getVerifyType() = "verify"
  
      let getClean() = "clean"
  
      let getCy2Type() =
        !cy2Type
  
    end