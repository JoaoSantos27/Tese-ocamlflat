(*
 * RegularExpressionTests.ml
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
 * apr/2023 (amd) - New file.
 *)

(*
 * Description: Regular expressions testing.
 *)

open BasicTypes

module RegularExpressionTests: sig end =
struct
	open RegularExpression
	let active = false

	let test0 () =
		let m = make (Arg.Predef "re_abc") in
		let j = toJSon m in
				JSon.show j

	let test1 () =
		let re = make (Arg.Predef "re_abc") in
		let j = toJSon re in
				JSon.show j

	let testAlphabet () =
		let re = make (Arg.Predef "re_abc") in
			Util.println ["alphabet: "];
			Util.printAlphabet (alphabet re);
			Util.println []

	let testAlphabet2 () =
		let re = make (Arg.Predef "re_simple") in
			Util.println ["alphabet: "];
			Util.printAlphabet (alphabet re);
			Util.println []

	let testAlphabet3 () =
		let re = make (Arg.Predef "re_complex") in
			Util.println ["alphabet: "];
			Util.printAlphabet (alphabet re);
			Util.println []

	let testAlphabet4 () =
		let re = make (Arg.Predef "re_convoluted") in
			Util.println ["alphabet: "];
			Util.printAlphabet (alphabet re);
			Util.println []

	let testQuasiLang () =
		let re = make (Arg.Predef "re_abc") in
		let ws = quasiLanguage re in
			Util.printWords ws

	let testQuasiLang2 () =
		let re = make (Arg.Predef "re_simple") in
		let ws = quasiLanguage re in
			Util.printWords ws

	let testQuasiLang3 () =
		let re = make (Arg.Predef "re_complex") in
		let ws = quasiLanguage re in
			Util.printWords ws

	let testQuasiLang4 () =
		let re = make (Arg.Predef "re_convoluted") in
		let ws = quasiLanguage re in
			Util.printWords ws

	let check f w =
		let msg = 
			if f w then "word was accepted"
			else "word was not accepted"
		in Util.println [msg]

	let testAccept () =
		let m = make (Arg.Predef "re_abc") in
			check (accept m) (word "aa")

	let testAccept2 () =
		let m = make (Arg.Predef "re_simple") in
			check (accept m) (word "aa")

	let testAccept3 () =
		let m = make (Arg.Predef "re_complex") in
			check (accept m) (word "aa")

	let testAccept4 () =
		let m = make (Arg.Predef "re_convoluted") in
			check (accept m) (word "aa")

	let testGenerate () =
		let re = make (Arg.Predef "re_abc") in
			Util.println ["generated words size 0:"]; Util.printWords (generate re 0);
			Util.println ["generated words size 1:"]; Util.printWords (generate re 1);
			Util.println ["generated words size 2:"]; Util.printWords (generate re 2);
			Util.println ["generated words size 3:"]; Util.printWords (generate re 3);
			Util.println ["generated words size 4:"]; Util.printWords (generate re 4);
			Util.println []

	let testGenerate2 () =
		let re = make (Arg.Predef "re_simple") in
			Util.println ["generated words size 0:"]; Util.printWords (generate re 0);
			Util.println ["generated words size 1:"]; Util.printWords (generate re 1);
			Util.println ["generated words size 2:"]; Util.printWords (generate re 2);
			Util.println ["generated words size 3:"]; Util.printWords (generate re 3);
			Util.println ["generated words size 4:"]; Util.printWords (generate re 4);
			Util.println []

	let testGenerate3 () =
		let re = make (Arg.Predef "re_complex") in
			Util.println ["generated words size 0:"]; Util.printWords (generate re 0);
			Util.println ["generated words size 1:"]; Util.printWords (generate re 1);
			Util.println ["generated words size 2:"]; Util.printWords (generate re 2);
			Util.println ["generated words size 3:"]; Util.printWords (generate re 3);
			Util.println ["generated words size 4:"]; Util.printWords (generate re 4);
			Util.println []

	let testGenerate4 () =
		let re = make (Arg.Predef "re_convoluted") in
			Util.println ["generated words size 0:"]; Util.printWords (generate re 0);
			Util.println ["generated words size 1:"]; Util.printWords (generate re 1);
			Util.println ["generated words size 2:"]; Util.printWords (generate re 2);
			Util.println ["generated words size 3:"]; Util.printWords (generate re 3);
			Util.println ["generated words size 4:"]; Util.printWords (generate re 4);
			Util.println []

	let re = {| {
			kind : "regular expression",
			description : "this is a simple example",
			name : "re_simple",
			re : "a+a*+bc*"
		} |}

	let testSimplify () =
		let re = make (Arg.Text re) in
			show re;
			show (simplify re)

	let testEnum () =
		let e = new Exercise.exercise (Arg.Predef "exer_re2fa") in
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let result = re#checkExercise e in
			if result then Util.print ["it works"]
			else Util.print ["it does not work"]

	let testTrace () =
		let re = make (Arg.Predef "re_simple") in
			allTrees re (word "acbacb")
	
	let re_more = {| {
			kind : "regular expression",
			description : "this is an example",
			name : "re_more",
			re : "a*"
	} |}
				
	let testMore () =
		let re = make (Arg.Text re_more) in
			allTrees re (word "aa")

	let runAll =
		if Util.testing active "RegularExpression" then begin
			testSimplify ()
		end
end
