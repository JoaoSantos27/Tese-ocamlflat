(*
 * ContextFreeGrammar.ml
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
????
 *)

(*
 * Description: Context-free grammar functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes

module ContextFreeGrammar =
struct
	include ContextFreeGrammarSupport

	class model (arg: t Arg.alternatives) =
		object(self) inherit ContextFreeGrammarLR.model arg as super
     end
end


module ContextFreeGrammarTop =
struct
	open ContextFreeGrammarBasic
	open ContextFreeGrammarBasicsX

	let cfgI cfg = internalize cfg
	let cfgX cfg = externalize cfg
	let ptX pt = externalizeParseTree pt
	
	let cfg_load file = cfgX (make (Arg.File file))
	let cfg_text text = cfgX (make (Arg.Text text))
	let cfg_json json = cfgX (make (Arg.JSon json))
	let cfg_predef name = cfg_text (Examples.example name)

	let confX (sf, w) = (word2str sf, word2str w)
	let pathX (p: path) = pathX confX p
	let trailX (t: trail) = trailX confX t
	
	let stats () = RuntimeControl.stats ()

	let cfg_accept cfg w = accept (cfgI cfg) (wordI w)

	let cfg_path cfg w =
		let (r,p,t) = acceptFull (cfgI cfg) (wordI w) in
			pathX p

	let cfg_trail cfg w =
		let (r,p,t) = acceptFull (cfgI cfg) (wordI w) in
			trailX t

	let cfg_parse_tree cfg w =
		let pt = parseTree (cfgI cfg) (wordI w) in
			ptX pt

	let cfg_generate cfg len = wordsX (generate (cfgI cfg) len)
	
	let cfg_info cfg  =
		if ContextFreeGrammarLL1.isLL1 true (cfgI cfg) then
			print_string "LL1\n"
	;;

end



(*
    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      if side then HtmlPageClient.cfgCy2Close();
      let ll1 = myCFG#isLL1 in
        HtmlPageClient.getIsLL1 ll1 infoBox;
      let lr = myCFG#isLeftRecursive in
        HtmlPageClient.getIsLeftRecursive lr infoBox;
      let lf = myCFG#isLeftFactoring in
        HtmlPageClient.getIsLeftFactoring lf infoBox;
      let pConf = myCFG#hasParsingTableConflict in
        HtmlPageClient.getHasParsingTableConflict pConf infoBox;
      let c = myCFG#isClean in
      let prod = myCFG#isFullyProductive in
      let access = myCFG#isFullyAccessible in
        HtmlPageClient.getIsCFGClean c prod access infoBox
*)


(*

examples;;
open ContextFsreeGrammarSupport;;
open ContextFreeGrammarBasicsX;;
let g = cfg_predef "cfg_balanced";;
let w = "[[][]]";;
cfg_accept g w;;
cfg_path g w;;
cfg_parse_tree g w;;

let g = cfg_predef "cfg_balanced";;
cfg_info g;;

*)

open ContextFreeGrammarTop

     (* Adds a sufix to a variable name name *)
     let addSufixCFG (v: symbol)(sufix: string): symbol =
       str2symb((symb2str v)^"_"^sufix)


(* addSufix a que? *)
    let addSufixList  body sufix =
        List.map(fun s -> addSufixCFG  s sufix) body

       (* Renames all the variables in one gramatic adding a sufix *)	
     let renameVariablesCFG (cfg: ContextFreeGrammarBasic.t) (sufix: string): ContextFreeGrammarBasic.t =
       let open ContextFreeGrammarBasic in 
       {alphabet = cfg.alphabet;
       variables =	Set.map (fun v -> addSufixCFG v sufix) cfg.variables;
       initial = addSufixCFG cfg.initial sufix;
       rules = Set.map (fun {head= h;body = b} -> {head=(addSufixCFG h sufix);body= addSufixList b sufix}) cfg.rules
       }


(*

--------------------

let cfg_balanced = {| {
		kind : "context free grammar",
		description : "CFG: Language of balanced square bracket parentheses",
		name : "cfg_balanced",
		alphabet : ["[", "]"],
		variables : ["<Start>"],
		initial : "<Start>",
		rules : [ "<Start> -> [<Start>] | <Start><Start> | ~"]
	} |};;

let cfg = cfg_text cfg_balanced;;

let cfg2 = cfgX cfg;;





















let cfg2 = renameVariablesCFG (cfgI cfg) "ola";;





let cfg = cfg_predef "cfg_simple";;

let cfg2 = renameVariablesCFG (cfgI cfg) "ola";;



let cfg2 = cfgX (renameVariablesCFG (cfgI cfg) "ola");;

fa_generate fa 8;;

fa_accept fa "aaaa";;
fa_accept fa "aaaca";;

fa_path fa "aaaa";;
fa_path fa "aaaca";;

fa_trail fa "aaaa";;
--------------------

#print_depth 10000;;
#print_length 10000;;




--------------------
let fa = fa_predef "dfa_astar";;

fa_generate fa 8;;

fa_accept fa "aaaa";;
fa_accept fa "aaaca";;

fa_path fa "aaaa";;
fa_path fa "aaaca";;

fa_trail fa "aaaa";;
--------------------

#print_depth 10000;;
#print_length 10000;;



let fa_astar = {| {
		kind : "finite automaton2",
		description : "this is an example",
		name : "dfa_astar",
		alphabet: ["a"],
		states : ["START", "Z1"],
		initialState : "START",
		transitions : [
			["START", "a", "START"],
			["START", "~", "START"],			
			["START", "~", "Z"],			
			["Z", "a", "Z"],
			["START", "a", "Z"]
		],
		acceptStates : ["START", "Z"]
		} |}
;;
let fa = fa_text fa_astar;;

let fa_astar = {| {
		kind : "finite automaton2",
		description : "this is an example",
		name : "dfa_astar",
		alphabet: ["a"],
		states : ["START", "Z1"],
		initialState : "START",
		transitions : [
			["START", "a", "START"],
			["START", "~", "START"],			
			["START", "~", "Z"],			
			["Z", "a", "Z"],
			["START", "a", "Z"]
		],
		acceptStates : ["START", "Z"]
		} |}
;;
let fa = fa_text fa_astar;;

*)
