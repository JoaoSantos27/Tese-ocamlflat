(*
 * GrammarUnrestrictedTests.ml
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
 *  Written by Pedro Carlos (p.carlos)
 *)


module GrammarTests : sig end =
struct


	let active = false

	open Grammar
	open BasicTypes

	(* Sample grammars *)

	let unGrammar = {| {
    kind: "grammar",
    description: "Unrestricted grammar example",
    name: "g_unrestricted",
    alphabet: ["a", "b", "c"],
    variables: ["S", "A", "B"],
    initial: "S",
    rules: [
        "S -> aSBc",
				"S -> ~",
        "cB -> Bc",
        "bB -> bb"
    ]
} |}

let replaced = {|{
        kind : "grammar",
        description : "_",
        name : "_",
        alphabet : ["a", "b"],
        variables : ["A", "B", "C", "D", "E", "F", "S", "G", "H"],
        initial : "S",
        rules : ["A -> AA", "A -> BD", "A -> CS",
				"B -> AB", "B -> BE", "B -> CG",
				 "C -> AC", "C -> BF", "C -> CH",
				  "D -> DA", "D -> ED", "D -> FS",
					 "E -> DB", "E -> EE", "E -> FG",
					  "F -> DC", "F -> EF", "F -> FH",
						 "S -> SA", "S -> GD", "S -> HS",
						  "G -> SB", "G -> GE", "G -> HG",
							 "H -> SC", "H -> GF", "H -> HH",
							 "G -> aGb", "G -> aHb", "H -> ~", "E -> ~", "A -> ~"]
}|}

let unGrammar2 = {| {
	kind: "grammar",
	description: "Unrestricted grammar example",
	name: "g_unrestricted",
	alphabet: ["1", "2", "c", "3", "+", "d"],
	variables: ["S", "A"],
	initial: "S",
	rules: [
			"S -> +AAAA+",
			"AA -> 1",
			"AA -> 2",
			"1AA -> 3",
			"A1A -> c",
			"A2A -> d"	]
} |}
 

let testAB = {| {
	kind: "grammar",
	description: "Unrestricted grammar example",
	name: "g_unrestricted",
	alphabet: ["a", "b"],
	variables: ["S", "A", "B"],
	initial: "S",
	rules: [
			"S -> SAB | ~",
			"A -> a",
			"B -> b"	]
} |}


let cfg = {| 		{
	kind : "grammar",
	description : "this is an example",
	name : "cfg_simple",
	alphabet : ["0", "1"],
	variables : ["S", "P"],
	initial : "S",
	rules : [
	"S -> 1S0",
	"S -> P",
	"P -> 0P1", "P -> ~" ]
}|}

let cfg_bounded = {| {
	kind : "grammar",
	description : "CFG: Language of balanced square bracket parentheses",
	name : "cfg_balanced",
	alphabet : ["[", "]", "a"],
	variables : ["S", "A"],
	initial : "S",
	rules : [ "S -> [S] | A", "A -> a" ]
} |}

let csg = {| {
	kind: "grammar",
	description: "a^nb^nc^n",
	name: "custom_csg",
	alphabet: ["a", "b", "c"],
	variables: ["S", "B", "C", "Z", "W"],
	initial: "S",
	rules: [
	"S -> aBC",
	"S -> aSBC",
	"CB -> CZ",
	"CZ -> WZ",
	"WZ -> WC",
	"WC -> BC",
	"aB -> ab",
	"bB -> bb",
	"bC -> bc",
	"cC -> cc"]
} |}

let non_contracting = {| {
	kind: "grammar",
	description: "a^nb^nc^n",
	name: "custom_non_contracting",
	alphabet: ["a", "b", "c"],
	variables: ["S", "B"],
	initial: "S",
	rules: [
	"S -> abc",
	"S -> aSBc",
	"cB -> Bc",
	"bB -> bb"]
} |}

let gram_example = {| {
	kind: "grammar",
	description: "a^nb^nc^n",
	name: "custom_non_contracting",
	alphabet: ["[", "]", "a"],
	variables: ["S", "A"],
	initial: "S",
	rules: [
	"S -> [ S ] | A",
	"A -> a"]
	} |}


	let gram_example2 = {| {
		kind: "grammar",
		description: "a^nb^nc^n",
		name: "custom_non_contracting",
		alphabet: [ "b", "a"],
		variables: ["S", "A", "B"],
		initial: "S",
		rules: [
			"S	-> aAB",
			"aA	-> aB",
			"B	-> b | aB"
			]
		} |}

	let gram_example3 = {| {
		kind: "grammar",
		description: "a^nb^nc^n",
		name: "custom_non_contracting",
		alphabet: [ "b", "a"],
		variables: ["S", "A", "B"],
		initial: "S",
		rules: [
			"S	-> AB",
			"A	-> a",
			"B	-> b"
			]
		} |}

	let lg = {| 		{
		kind : "grammar",
		description : "this is an example",
		name : "linear_grammar",
		alphabet : ["a", "b"],
		variables : ["S"],
		initial : "S",
		rules : [	"S -> aSb | ~" ]
	}|}

	let rlgrammar = {| {
		kind: "grammar",
		description: "Right-linear grammar example",
		name: "g_right_linear",
		alphabet: ["0", "1"],
		variables: ["S", "B"],
		initial: "S",
		rules: [
				"S -> 00B | 11S",
				"B -> 0B | 1B | 0 | 1 | ~"
		]
	}|}

	let llgrammar = {| {
		kind: "grammar",
		description: "Left-linear grammar example",
		name: "g_left_linear",
		alphabet: ["0", "1"],
		variables: ["S", "B"],
		initial: "S",
		rules: [
				"S -> B00 | S11",
				"B -> B0 | B1 | 0 | 1"
		]
	}|}

	let cfg_balanced = {| {
		kind : "grammar",
		description : "",
		name : "remove epsilon example",
		alphabet: ["a", "b", "c"],
		variables: ["S", "B", "A", "C"],
		initial: "S",
		rules: [
			"S -> AbB | C",
			"B -> AA | AC",
			"C -> b | c",
			"A -> a | ~"
		]
	} |}

	let g_unproductive = {| {
		kind: "grammar",
		description: "Unproductive grammar",
		name: "g_unproductive",
		alphabet: ["a", "b"],
		variables: ["S", "A", "B", "C"],
		initial: "S",
		rules: [
			"S -> aA",
			"A -> B",
			"B -> BB",
			"C -> a"
		]
	}|}

	let g_inaccessible = {| {
		kind: "grammar",
		description: "Inaccessible grammar",
		name: "g_inaccessible",
		alphabet: ["t", "b", "u"],
		variables: ["S", "B", "A", "Z", "C"],
		initial: "S",
		rules: [
			"S -> AB",
			"BAu -> BCZ",
			"A -> t",
			"B -> b",
			"Z -> u"

		]
	}|}

	let cleang = {| {
		kind : "grammar",
		description : "Clean example from https://www.tutorialspoint.com/automata_theory/removal_of_useless_symbols_in_cfg.htm",
		name : "Clean1",
		alphabet : ["a", "b"],
		variables : ["S", "A", "B", "C", "D"],
		initial : "S",
		rules : ["S -> aA | BC", "A -> b | bB", "B -> aB | ~", "C -> aC | D", "D -> bD"]
	} |}

	(* Result of clean should be: *)
	(*  S → aA
			A → b|bB
			B → aB|ε *)


		(* Rule with body length > 2 *)
		(* "S -> ABC", *)

		(* Rule with head length > 2 AND body length > 2 *)
		(* "ABC -> DEF", *)

		(* Rule already in AB -> CD form (after terminal replacement) *)
		(* "DE -> FG", *)

		(* Simple terminal rules *)
		(* "F -> b",
		"A -> a",
		"E -> b",
*)
		(* Rule with terminal in body *)
		(* "G -> cG", *)

		(* Rule already in A -> BC form *)
		(* "B -> AC", *)

		(* Unit rule A -> B *)
		(* "C -> S" *)
		let knf_test_grammar = {| {
			kind : "grammar",
			description : "Test grammar for Kuroda Normal Form conversion (long heads/bodies, terminals)",
			name : "knf_test",
			alphabet : ["a", "b", "c"],
			variables : ["S", "A", "B", "C", "D", "E", "F", "G"],
			initial : "S",
			rules : [
				"S -> ABC",
				"ABC -> DEF",
				"DE -> FG",
				"F -> b",
				"A -> a",
				"E -> b",
				"G -> cG",
				"B -> AC",
				"C -> S"
			]
	} |}

	let make_grammar json = Grammar.make (Arg.Text json)
	let word = str2word
	let assert_accepts grammar words = List.iter (fun w -> assert (Grammar.accept grammar (word w))) words
	let assert_rejects grammar words = List.iter (fun w -> assert (not (Grammar.accept grammar (word w)))) words

	module ClassificationTests = struct
    let test_grammar_types () =
      print_endline "Testing grammar classification";
      
      let cfg = make_grammar cfg in
      let csg = make_grammar csg in
			let unrestricted = make_grammar unGrammar in
      let unrestricted2 = make_grammar unGrammar2 in

			(* print_endline "Testing isUnrestrictedGrammar"; *)
			assert (Grammar.isUnrestrictedGrammar unrestricted);
			assert (Grammar.isUnrestrictedGrammar unrestricted2);
			assert (Grammar.isUnrestrictedGrammar cfg);
			assert (Grammar.isUnrestrictedGrammar csg);
			print_endline "Passed isUnrestrictedGrammar";

			(* print_endline "Testing isContextSensitiveGrammar"; *)
			assert (Grammar.isContextSensitiveGrammar csg);
			assert (Grammar.isContextSensitiveGrammar cfg);
			assert (not (Grammar.isContextSensitiveGrammar unrestricted));
			print_endline "Passed isContextSensitiveGrammar";

			(* print_endline "Testing isContextFreeGrammar"; *)
			assert (Grammar.isContextFreeGrammar cfg);
			assert (not (Grammar.isContextFreeGrammar csg));
			assert (not (Grammar.isContextFreeGrammar unrestricted));
			print_endline "Passed isContextFreeGrammar";

			(* print_endline "Testing isLinearGrammar"; *)
			let linear = make_grammar lg in
			assert (Grammar.isLinearGrammar linear);
			assert (not (Grammar.isLinearGrammar csg));
			print_endline "Passed isLinearGrammar";

			(* print_endline "Testing isRightLinearGrammar"; *)
			let rlgrammar = make_grammar rlgrammar in
			assert (Grammar.isRightLinearGrammar rlgrammar);
			assert (not (Grammar.isRightLinearGrammar csg));
			print_endline "Passed isRightLinearGrammar";

			(* print_endline "Testing isLeftLinearGrammar"; *)
			let llgrammar = make_grammar llgrammar in
			assert (Grammar.isLeftLinearGrammar llgrammar);
			assert (not (Grammar.isLeftLinearGrammar csg));
			print_endline "Passed isLeftLinearGrammar";

			(* print_endline "Testing isMonotonicGrammar"; *)
			let non_contracting = make_grammar non_contracting in
			assert (Grammar.isMonotonicGrammar non_contracting);
			assert (Grammar.isMonotonicGrammar csg);
			assert (not (Grammar.isMonotonicGrammar unrestricted2));
			print_endline "Passed isMonotonicGrammar"
  end

	module ProductionTests = struct
    let test_acceptance () =
      print_endline "Testing acceptance";
      
      let csg = make_grammar csg in
			let non_contracting = make_grammar non_contracting in
      assert_accepts csg ["abc"; "aabbcc"; "aaabbbccc"];
      assert_rejects csg ["ab"; "aabcc"];
			assert_accepts non_contracting ["abc"; "aabbcc"; "aaabbbccc"];
			assert_rejects non_contracting ["ab"; "aabcc"];

      let cfg = make_grammar cfg in
      assert_accepts cfg ["10"; "110100"];
      assert_rejects cfg ["001"; "101"];

      let unrestricted = make_grammar unGrammar2 in
      assert_accepts unrestricted ["+12+"; "+3+"; "+c+"; "+21+"; "+d+"; "+11+"; "+22+"];

			print_endline "Passed acceptance"
      
    let test_generation () =
      print_endline "Testing generation";
      let csg = make_grammar csg in
			let len = 9 in
			let expected_words = Set.make (List.map word ["abc"; "aabbcc"; "aaabbbccc"]) in
			let actual_words = Grammar.generate csg len in
			assert (Set.equals expected_words actual_words);

			let testNew = make_grammar unGrammar2 in
			let len = 4 in
			let expected_words = Set.make (List.map word ["+12+"; "+3+"; "+c+";"+21+";"+d+";"+11+";"+22+" ]) in
			let actual_words = Grammar.generate testNew len in
			assert (Set.equals expected_words actual_words);

			let g_cfg = make_grammar cfg in
			let len = 6 in
			let expected_words = Set.make (List.map word [""; "01"; "10";"0011"; "1010"; "1100";"000111";"100110"; "110100"; "111000" ]) in
			let actual_words = Grammar.generate g_cfg len in
			assert (Set.equals expected_words actual_words);
			print_endline "Passed generation"


  end

	module ConversionTests = struct
		let test_kuroda_normalization () =
			print_endline "Testing Kuroda normalization";
			let csg = make_grammar csg in
			let kuroda = Grammar.kurodaNormalForm csg in
			assert (Grammar.isContextSensitiveGrammar kuroda);
			(* assert_accepts kuroda ["abc"; "aabbcc"]; *)
			print_endline "Passed Kuroda normalization"
			(* Grammar.show kuroda; *)

		let test_kuroda_full () =
			print_endline "Testing Kuroda full";
			let csg = make_grammar knf_test_grammar in
			let kuroda = Grammar.kurodaNormalForm csg in
			(* assert (Grammar.isContextSensitiveGrammar kuroda); *)
			(* assert_accepts kuroda ["abc"; "aabbcc"]; *)
			Grammar.show kuroda;
			print_endline "Passed Kuroda full"
			(* Grammar.show kuroda; *)

		let test_penttonen_normalization () =
			print_endline "Testing Penttonen normalization";
			let example_grammar = make_grammar gram_example in
			let penttonen = Grammar.penttonenNormalForm example_grammar in
			assert (Grammar.isContextFreeGrammar penttonen);
			assert_accepts penttonen ["[a]"; "[[a]]"];
			print_endline "Passed Penttonen normalization"
			(* Grammar.show penttonen; *)

		let test_nonContrating_to_CSG () =
			print_endline "Testing nonContrating to CSG";
			let g_non_contracting = make_grammar non_contracting in
			let csg = Grammar.nonContractingToCSG g_non_contracting in
			(* Grammar.show csg; *)
			assert (Grammar.isContextSensitiveGrammar csg);
			assert_accepts csg ["abc"; "aabbcc"];
			print_endline "Passed nonContrating to CSG"

	end

	module PerformanceTests = struct
		let testAcceptImplementations () =
			print_endline "Running testAcceptImplementations";
			(* let g_nc = make_grammar non_contracting in *)
			let g_csg = make_grammar csg in
			(* let g_cfg = make_grammar cfg in *)
	
			let input_strings = ref [] in
			let accept_times = ref [] in
			let accept2_times = ref [] in
	
			let test_word g w =
				input_strings := w :: !input_strings;
				let (_, duration1) = Util.benchmark (fun () -> Grammar.accept g (word w)) in
				accept_times := duration1 :: !accept_times;
				let (_, duration2) = Util.benchmark (fun () -> Grammar.accept2 g (word w)) in
				accept2_times := duration2 :: !accept2_times
			in 
			(* test_word g_cfg "111000";
			test_word g_cfg "110100";
			test_word g_cfg "100110";
			test_word g_cfg "000111"; *)
	
			(* test_word g_nc "aaaaaaaaaabbbbbbbbbbccccccccccc";  *)
			(* let (accept, duration1) = Util.benchmark (fun () -> Grammar.accept g_nc (word "aabbcc")) in
			let (accept2, duration2) = Util.benchmark (fun () -> Grammar.accept2 g_nc (word "aabbcc")) in
			print_endline (string_of_float duration1);
			print_endline (string_of_bool accept); *)
			test_word g_csg "aabbcc"; 
			(* test_word g_csg "aabbcc"; *)
	
			(* test_word g_nc "aaaabbbbcccc";
			test_word g_csg "aaaabbbbcccc";
	
			test_word g_nc "aaaaabbbbbccccc";
			test_word g_csg "aaaaabbbbbccccc";  *)
			(* test_word g_nc "aaaaaabbbbbbcccccc";*)
			(* test_word g_csg "aaaaaabbbbbbcccccc";  NOT ENOUGH MEMORY IF TIME IS UNLIMITED*)
	
			let channel = open_out "acceptTest.txt" in
			Printf.fprintf channel "input_strings = [%s]\n" (String.concat ", " (List.rev (List.map (fun s -> "\"" ^ s ^ "\"") !input_strings)));
			Printf.fprintf channel "accept_times = [%s]\n" (String.concat ", " (List.rev (List.map (Printf.sprintf "%.6f") !accept_times)));
			Printf.fprintf channel "accept2_times = [%s]\n" (String.concat ", " (List.rev (List.map (Printf.sprintf "%.6f") !accept2_times)));
			close_out channel;
	
			print_endline "Ended testAcceptImplementations"
	end


		module CleanupTests = struct
			let test_cleaning () =
				print_endline "Testing grammar cleaning";
				let g_inaccessible = make_grammar g_inaccessible in
				let g_unproductive = make_grammar g_unproductive in
				let g_cleang = make_grammar cleang in
				let csg = make_grammar csg in
				assert (not (Grammar.isClean g_inaccessible));
				assert (not (Grammar.isClean g_unproductive));
				(* Grammar.show g_cleang; *)
				
				assert (not (Grammar.isClean g_cleang));
				assert ((Grammar.isClean csg));

				let cleaned_inaccessible = Grammar.clean g_inaccessible in
				assert (Grammar.isClean cleaned_inaccessible);

				let cleaned_unproductive = Grammar.clean g_unproductive in
				assert (Grammar.isClean cleaned_unproductive);

				let cleaned_cleang = Grammar.clean g_cleang in
				assert (Grammar.isClean cleaned_cleang)
				(* Grammar.show cleaned_cleang; *)
				(* ;
				Grammar.show cleaned_cleang; *)

		end


		module UtilityTests = struct
			let testRemoveEpislon () =
				print_endline "Testing remove epsilon";
				
				let g = make_grammar cfg_balanced in
				assert (Grammar.isContextFreeGrammar g);
				assert (Grammar.hasEpsilonRules g);
				let g = Grammar.removeEpsilonRules g in
				(*Result should be:
					S → AbB | Ab | bB | b | C
					B → AA | A | AC | C
					C → b | c
					A → a
				webgraphy: https://en.wikipedia.org/wiki/Chomsky_normal_form
				*)
				Grammar.show g;

				print_endline "Passed testRemoveEpislon"

			let	test_find_applied_rules () =
				print_endline "Running test_find_applied_rules";
				let g_non_contracting = make_grammar gram_example3 in
				let word = "ab" in
				let (accepted, path, trail) = Grammar.acceptFull g_non_contracting (str2word word) in

				print_endline ("Path: " ^
						(String.concat " -> " (
								List.map (fun (syms, _) ->
										String.concat "" (List.map symb2str syms)
								) path
						))
				);

				print_endline ("Trail: " ^
						(String.concat ", " (
								List.map (fun config_set ->
										"{" ^
												(String.concat "; " (
														Set.toList config_set
														|> List.map (fun (syms, _) -> String.concat "" (List.map symb2str syms))
												)) ^
										"}"
								) trail
						))
				);
				let rule_map = find_applied_rules g_non_contracting path in
				List.iter (fun ((key, int), (rule_opt, index)) ->  
						match rule_opt with
						| Some rule ->
								Printf.printf "%s %d: (" (word2str key) int;
								Printf.printf "%s, %d " (rule2str rule) index;
								Printf.printf ")\n"
						| None -> ()
				) rule_map;

				print_endline "Passed test_find_applied_rules"
		end


	let runAll =
		if Util.testing active "Grammar" then begin
      ClassificationTests.test_grammar_types ();

			Util.sep ();
      ProductionTests.test_acceptance ();
      ProductionTests.test_generation ();

			Util.sep ();
      PerformanceTests.testAcceptImplementations ();

			Util.sep ();
      ConversionTests.test_kuroda_normalization ();
			ConversionTests.test_kuroda_full ();
      ConversionTests.test_penttonen_normalization ();
			ConversionTests.test_nonContrating_to_CSG ();

			Util.sep ();
			CleanupTests.test_cleaning ();

			Util.sep ();
			UtilityTests.testRemoveEpislon ();
			UtilityTests.test_find_applied_rules ();

		end
end

