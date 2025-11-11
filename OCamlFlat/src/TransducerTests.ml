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
 * Written by João Santos (js)
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
  open FiniteAutomaton
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

  let fst1_Identity = {| {
    kind : "transducer",
    description : "1: Deterministic, Complete, Mealy",
    name : "fst1",
    inAlphabet : ["a", "b"],
    outAlphabet : ["c", "d"],
    states : ["S"],
    initialState : "S",
    transitions : [
      ["S","a","c","S"],
      ["S","b","d","S"]
    ],
    acceptStates : ["S"]
  } |}

  let fst2_Moore = {| {
    kind : "transducer",
    description : "2 Moore",
    name : "fst2",
    inAlphabet : ["a", "b"],
    outAlphabet : ["x", "y"],
    states : ["S","A"],
    initialState : "S",
    transitions : [
      ["S","a","x","A"],
      ["S","b","x","A"],
      ["A","a","y","S"],
      ["A","b","y","S"]
    ],
    acceptStates : ["S","A"]
  } |}

  let fst3_NonDeterministic = {| {
    kind : "transducer",
    description : "3: Nondeterministic  (2 transitions with same input)",
    name : "fst3",
    inAlphabet : ["a"],
    outAlphabet : ["x","y"],
    states : ["S"],
    initialState : "S",
    transitions : [
      ["S","a","x","S"],
      ["S","a","y","S"]
    ],
    acceptStates : ["S"]
  } |}

  let fst4_Incomplete = {| {
    kind : "transducer",
    description : "4: Deterministic but incomplete (missing 'b' on S)",
    name : "fst4",
    inAlphabet : ["a", "b"],
    outAlphabet : ["x"],
    states : ["S"],
    initialState : "S",
    transitions : [
      ["S","a","x","S"]
    ],
    acceptStates : ["S"]
  } |}

  let fst5_EpsConflict = {| {
    kind : "transducer",
    description : "5: Nondeterministic due to epsilon with conflicting outputs",
    name : "fst5",
    inAlphabet : ["a"],
    outAlphabet : ["x","y"],
    states : ["S","A","B"],
    initialState : "S",
    transitions : [
      ["S","~","x","A"],
      ["S","~","y","B"],
      ["A","a","x","A"],
      ["B","a","y","B"]
    ],
    acceptStates : ["A","B"]
  } |}

  let fst6_EpsDeterministic = {| {
    kind : "transducer",
    description : "6: ε-closure but deterministic (same epsilon output)",
    name : "fst6",
    inAlphabet : ["a"],
    outAlphabet : ["x","y"],
    states : ["S","A","B","C"],
    initialState : "S",
    transitions : [
      ["S","~","x","A"],
      ["A","~","x","B"],
      ["A","~","x","C"],
      ["B","a","x","B"]
    ],
    acceptStates : ["B","C"]
  } |}

  let fst7_NotMoore = {| {
    kind : "transducer",
    description : "7: Deterministic & complete but not Moore (output depends on input)",
    name : "fst7",
    inAlphabet : ["a","b"],
    outAlphabet : ["x","y"],
    states : ["S"],
    initialState : "S",
    transitions : [
      ["S","a","x","S"],
      ["S","b","y","S"]
    ],
    acceptStates : ["S"]
  } |}

  let fstD = {| {
    kind : "transducer",
    description : "Not deterministic",
    name : "fstND",
    inAlphabet : ["a"],
    outAlphabet : ["x","y"],
    states : ["S","A","B"],
    initialState : "S",
    transitions : [
        ["S","a","x","A"],
        ["S","a","x","B"]
    ],
    acceptStates : ["A","B"]
  } |}

  let fstMin = {| {
    kind : "transducer",
    description : "Minimizable: A and B are equivalent)",
    name : "fst_min_merge_AB",
    inAlphabet : ["a","b"],
    outAlphabet : ["x","y"],
    states : ["S","A","B"],
    initialState : "S",
    transitions : [
      ["S","a","x","A"],
      ["S","b","y","B"],

      ["A","a","x","A"],
      ["A","b","y","B"],

      ["B","a","x","A"],
      ["B","b","y","B"]
    ],
    acceptStates : ["A","B"]
  } |}

  (* Transducers for composition tests *)

  let fst_A_to_B = {| {
    kind : "transducer", 
		description : "A to B",
		name : "A_to_B",
    inAlphabet : ["a"], 
		outAlphabet : ["b"], 
		states : ["S"],
    initialState : "S", 
		transitions : [["S","a","b","S"]], 
		acceptStates : ["S"]
  } |}

  let fst_B_to_C = {| {
    kind : "transducer", 
		description : "B to C",
		name : "B_to_C",
    inAlphabet : ["b"], 
		outAlphabet : ["c"], 
		states : ["S"],
    initialState : "S", 
		transitions : [["S","b","c","S"]], 
		acceptStates : ["S"]
  } |}
  
  let fst_A_to_X = {| {
    kind : "transducer",
		description : "A to X", 
		name : "A_to_X",
    inAlphabet : ["a"], 
		outAlphabet : ["x"], 
		states : ["S"],
    initialState : "S", 
		transitions : [["S","a","x","S"]], 
		acceptStates : ["S"]
  } |}
  
  let fst_B_to_Y = {| {
    kind : "transducer", 
		description : "B to Y",
		name : "B_to_Y",
    inAlphabet : ["b"], 
		outAlphabet : ["y"], 
		states : ["S"],
    initialState : "S", 
		transitions : [["S","b","y","S"]], 
		acceptStates : ["S"]
  } |}

  let fst_ax_by = {| {
    kind : "transducer", 
		description : "a->x, b->y",
		name : "ax_by",
    inAlphabet : ["a","b"], 
		outAlphabet : ["x","y"], 
		states : ["S"],
    initialState : "S", 
		transitions : [
			["S","a","x","S"], 
			["S","b","y","S"]
		], 
		acceptStates : ["S"]
  } |}

  let fst_ax_bz = {| {
    kind : "transducer", 
		description : "a->x, b->z",
		name : "ax_bz",
    inAlphabet : ["a","b"], 
		outAlphabet : ["x","z"], 
		states : ["S"],
    initialState : "S", 
		transitions : [
			["S","a","x","S"], 
			["S","b","z","S"]
		], 
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

  let test4 () =
    let fst: t = make (Arg.Text fst6_EpsDeterministic) in
    let test = isDeterministic fst in
    Printf.printf "%s\n" (string_of_bool test)

  let test5 () =
    let fst: t = make (Arg.Text fst7_NotMoore) in
    let test = isComplete fst in
    Printf.printf "%s\n" (string_of_bool test)
  
  let test6 () =
    let fst: t = make (Arg.Text fst7_NotMoore) in
    let test = isMealyMachine fst in
    Printf.printf "%s\n" (string_of_bool test)

  let test7 () =
    let fst: t = make (Arg.Text fst1_Identity) in
    let input = BasicTypes.word "bab" in
    let (ok, path, _) = Transducer.acceptFull fst input in
    Printf.printf "accept(\"bab\") = %b\n" ok;
    let (a, b, c) = List.hd (List.rev path) in
    word2str c |> Printf.printf "output = %s\n";

    let outs = Transducer.generate fst (-3) |> BasicTypes.wordsX in
    Printf.printf "generate(3) = [%s]\n" (String.concat "; " outs)

  let test8 () =
    let fst: t = make (Arg.Text fstD) in
    let fstD = Transducer.toDeterministic fst in
    show fstD

  (* Test for compose(T1(a->b), T2(b->c)) = T(a->c) *)
  let test9 () =
    let t1 = make (Arg.Text fst_A_to_B) in
    let t2 = make (Arg.Text fst_B_to_C) in
    let t_comp = Transducer.compose t1 t2 in
    Printf.printf "--- Composition (a->b o b->c) ---\n";
    show t_comp;
    let (ok, path, _) = Transducer.acceptFull t_comp (BasicTypes.word "a") in
    Printf.printf "accept(\"a\") = %b\n" ok;
    if ok then
      let (_, _, output_word) = List.hd (List.rev path) in
      word2str output_word |> Printf.printf "output = %s (expected c)\n"

  (* Test for union(T1(a->x), T2(b->y)) *)
  let test10 () =
    let t1 = make (Arg.Text fst_A_to_X) in
    let t2 = make (Arg.Text fst_B_to_Y) in
    let t_union = Transducer.union t1 t2 in
    Printf.printf "--- Union (a->x U b->y) ---\n";
    show t_union;
    (* Test first machine's word *)
    let (ok1, path1, _) = Transducer.acceptFull t_union (BasicTypes.word "a") in
    Printf.printf "accept(\"a\") = %b\n" ok1;
    if ok1 then
      let (_, _, out1) = List.hd (List.rev path1) in
      word2str out1 |> Printf.printf "output(a) = %s (expected x)\n";
    (* Test second machine's word *)
    let (ok2, path2, _) = Transducer.acceptFull t_union (BasicTypes.word "b") in
    Printf.printf "accept(\"b\") = %b\n" ok2;
    if ok2 then
      let (_, _, out2) = List.hd (List.rev path2) in
      word2str out2 |> Printf.printf "output(b) = %s (expected y)\n"

  (* Test for intersection(T1(a->x, b->y), T2(a->x, b->z)) = T(a->x) *)
  let test11 () =
    let t1 = make (Arg.Text fst_ax_by) in
    let t2 = make (Arg.Text fst_ax_bz) in
    let t_inter = Transducer.intersection t1 t2 in
    Printf.printf "--- Intersection (a->x, b->y) n (a->x, b->z) ---\n";
    show t_inter;
    (* Test matching word *)
    let (ok1, path1, _) = Transducer.acceptFull t_inter (BasicTypes.word "a") in
    Printf.printf "accept(\"a\") = %b (expected true)\n" ok1;
    if ok1 then
      let (_, _, out1) = List.hd (List.rev path1) in
      word2str out1 |> Printf.printf "output(a) = %s (expected x)\n";
    (* Test non-matching word *)
    let (ok2, _, _) = Transducer.acceptFull t_inter (BasicTypes.word "b") in
    Printf.printf "accept(\"b\") = %b (expected false)\n" ok2
  
  (* Test for inverse(T(a->x)) = T(x->a) *)
  let test12 () =
    let t1 = make (Arg.Text fst_A_to_X) in
    let t_inv = Transducer.inverse t1 in
    Printf.printf "--- Inverse (a->x) => (x->a) ---\n";
    show t_inv;
    let (ok, path, _) = Transducer.acceptFull t_inv (BasicTypes.word "x") in
    Printf.printf "accept(\"x\") = %b\n" ok;
    if ok then
      let (_, _, output_word) = List.hd (List.rev path) in
      word2str output_word |> Printf.printf "output = %s (expected a)\n"

  (* Test for concatenate(T1(a->x), T2(b->y)) = T(ab->xy) *)
  let test13 () =
    let t1 = make (Arg.Text fst_A_to_X) in
    let t2 = make (Arg.Text fst_B_to_Y) in
    let t_cat = Transducer.concatenate t1 t2 in
    Printf.printf "--- Concatenate (a->x) . (b->y) ---\n";
    show t_cat;
    let (ok, path, _) = Transducer.acceptFull t_cat (BasicTypes.word "ab") in
    Printf.printf "accept(\"ab\") = %b\n" ok;
    if ok then
      let (_, _, output_word) = List.hd (List.rev path) in
      word2str output_word |> Printf.printf "output = %s (expected xy)\n"

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
      Util.header "test4";
      test4 ();
      Util.header "test5";
      test5 ();
      Util.header "test6";
      test6 ();
      Util.header "test7";
      test7 ();
      Util.header "test8";
      test8 ();
      Util.header "test9 (compose)";
      test9 ();
      Util.header "test10 (union)";
      test10 ();
      Util.header "test11 (intersection)";
      test11 ();
      Util.header "test12 (inverse)";
      test12 ();
      Util.header "test13 (concatenate)";
      test13 ();
      Util.header ""
    end
end