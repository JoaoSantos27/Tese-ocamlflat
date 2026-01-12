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

  let initialConfigs (fst: t) (w: word) : configurations =
    Set.make [(fst.initialState, w, [])]

  let isAcceptingConfig (fst: t) (st, w, _out) : bool =
    Set.belongs st fst.acceptStates && w = []

  let nextConfigs (fst: t) (st, w, out) : configurations =
    let build_next_config (new_w) (_, _, outSym, st2) =
      let new_out = if outSym = epsilon then out else out @ [outSym] in
      (st2, new_w, new_out)
    in

    match w with
    | [] ->
        let epsTr =
          Set.filter (fun (st1, sy, _, _) -> st1 = st && sy = epsilon) fst.transitions
        in
        Set.map (build_next_config []) epsTr

    | x::xs ->
        let consume =
          Set.filter (fun (st1, sy, _, _) -> st1 = st && sy = x) fst.transitions
        in
        let viaConsume =
          Set.map (build_next_config xs) consume
        in
        let epsTr =
          Set.filter (fun (st1, sy, _, _) -> st1 = st && sy = epsilon) fst.transitions
        in
        let viaEps =
          Set.map (build_next_config w) epsTr
        in

        Set.union viaConsume viaEps

  let accept (fst: t) (w: word) : bool =
    ignore (Model.checkWord fst.inAlphabet w);
    Model.accept fst w initialConfigs nextConfigs isAcceptingConfig

  let acceptFull (fst: t) (w: word) : bool * path * trail =
    ignore (Model.checkWord fst.inAlphabet w);
    Model.acceptFull fst w initialConfigs nextConfigs isAcceptingConfig

  let acceptOut (fst: t) (w: word) : bool*word =
	let (ok, path, _) = acceptFull fst w in
	if ok then
		let (_, _, c) = List.hd (List.rev path) in
		(ok, c)
	else
		(ok, [])
	(*trail lista de outputs*)

end

module TransducerGenerate =
struct
  open TransducerSupport
  open TransducerAccept

  let nextConfigs2 (fst: t) _ (st, w, out) =
    let trs = Set.filter (fun (st1, _, _, _) -> st1 = st) fst.transitions in
    Set.map
      (fun (_, inSym, outSym, st2) ->
         if inSym = epsilon then
           (st2, w, out @ [outSym])       
         else
           (st2, inSym::w, out @ [outSym])  
      )
      trs

  let isAcceptingConfig2 (fst: t) (st, _, _) =
    Set.belongs st fst.acceptStates

  let getWord (_, w, _) = w

  let generate (fst: t) (len: int) : words =
    Model.generate fst len initialConfigs nextConfigs2 isAcceptingConfig2 getWord

  let generateDumb (fst: t) (len: int) : words =
    Model.generateDumb fst fst.inAlphabet len initialConfigs nextConfigs isAcceptingConfig
end


module TransducerPrivate =
struct
	open FiniteAutomaton
	open TuringMachine
	open TransducerSupport

	(* get start state, start symbol, end symbol, or end state of all transitions in set *)
	let transitionsGetS trns = Set.map ( fun (a,_,_,_) -> a ) trns
	let transitionsGetSS trns = Set.map ( fun (_,b,_,_) -> b ) trns
	let transitionsGetES trns = Set.map ( fun (_,_,c,_) -> c ) trns
	let transitionsGetE trns = Set.map (fun (_,_,_,d) -> d) trns

	let asFiniteAutomaton (fst: t): FiniteAutomaton.t =
	{
		alphabet = fst.inAlphabet;
		states = fst.states;
		initialState = fst.initialState;
		acceptStates = fst.acceptStates;
		transitions =
		Set.map
			(fun (src, input, _output, dst) -> (src, input, dst))
			fst.transitions
	}

	let validate (name: string) (fst: t): unit =
		(* input alphabet must not contain " " *)
		let validInAlphabet = not (Set.belongs epsilon fst.inAlphabet) in

		(* output alphabet must not contain " " *)
		let validOutAlphabet = not (Set.belongs epsilon fst.outAlphabet) in

		(* initial state must belong to the set of all states *)
		let validInitSt = Set.belongs fst.initialState fst.states in

		(* all accepted states must belong to the set of all states *)
		let validAccSts = Set.subset fst.acceptStates fst.states in

		let fromSt = transitionsGetS fst.transitions in
		let ssy = transitionsGetSS fst.transitions in
		let esy = transitionsGetES fst.transitions in
		let toSt = transitionsGetE fst.transitions in
		let salpha = Set.add epsilon fst.inAlphabet in
		let ealpha = Set.add epsilon fst.outAlphabet in
		(* all transitions states must belong to all states and symbols must belong corresponding to the alphabet *)
		let validTrns = (Set.subset fromSt fst.states)
					&& (Set.subset ssy salpha) && (Set.subset esy ealpha) 
					&& (Set.subset toSt fst.states) in

			if not validInAlphabet then
				Error.error name "The input alphabet contains epsilon '~', and it should not" ();
			if not validOutAlphabet then
				Error.error name "The output alphabet contains epsilon '~', and it should not" ();
			if not validInitSt then
				Error.error name "The initial state does not belong to the set of all states" ();
			if not validAccSts then
				Error.error name "Some accept states do not belong to the set of all states" ();
			if not validTrns then
				Error.error name "Some transitions are invalid" ()
	
	(**
	* This function generates all states that are reachable from the given state. A state is reachable from s if there
	* exists a word that starting on s will lead to that state
	*)
	let reachable (fst: t) (s:state): states =
		let neighbourSts st t = transitionsGetE (Set.filter (fun (a,_,_,_) -> a = st) t) in
		let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
		let remain s t = Set.filter (fun (a,_,_,_) -> not (Set.belongs a s)) t in
		let rec reach visited s t = if visited = s then Set.empty else Set.union s ( reach s (nextStates s t) (remain s t) ) in
			reach Set.empty (Set.make [s]) fst.transitions

	(**
	* This function generates all productive states. A state is productive if there exists a word that will lead said state
	* to an acceptance state
	*)
	let productive (fst: t): states =
		let reachsAccSt st = Set.exists (fun s -> Set.belongs s fst.acceptStates ) (reachable fst st) in
			Set.filter (fun st -> reachsAccSt st) fst.states

	(**
	* This function generates the set of all useful states
	*)
	let getUsefulStates (fst: t): states =
		Set.inter (productive fst) (reachable fst fst.initialState)

	(**
	* This function generates the set of all non useful states
	*)
	let getUselessStates (fst: t): states =
		Set.diff fst.states (getUsefulStates fst)

	(**
	* This function creates the equivalent fst where all states are useful
	*)
	let cleanUselessStates (fst: t): t =
		let usfSts = getUsefulStates fst in
		let usfTrs = Set.filter
						(fun (a,_,_,d) -> Set.belongs a usfSts && Set.belongs d usfSts)
						fst.transitions in
		let inAlf = transitionsGetSS usfTrs in
		let usfInAlf = Set.diff inAlf (Set.make [epsilon]) in
		let outAlf = transitionsGetES usfTrs in
		let usfOutAlf = Set.diff outAlf (Set.make [epsilon]) in
		let accSts = Set.inter fst.acceptStates usfSts in
		let usfSts = Set.add fst.initialState usfSts in
			{
				inAlphabet = usfInAlf;
				outAlphabet = usfOutAlf;
				states = usfSts;
				initialState = fst.initialState;
				transitions = usfTrs;
				acceptStates = accSts
			}
		
	let isClean (fst: t): bool =
		let fa = asFiniteAutomaton fst in
		FiniteAutomaton.areAllStatesUseful fa
		(*s
		let usfSts = getUsefulStates fst in
			Set.size fst.states = Set.size usfSts
		*)

	(**
	* Performs a fast check for infinitely ambiguous epsilon-loops.
	* A transducer with an epsilon self-loop that produces output
	* is infinitely ambiguous and so, not determinizable.
	*)
	let isSelfLoop (fst: t) : bool =
		let has_output_eps_loop =
		  Set.exists
			(fun (a, b, c, d) ->
			   b = epsilon &&  (* Is it an epsilon transition? *)
			   a = d &&        (* Is it a self-loop? *)
			   c <> epsilon   (* Does it produce output? *)
			)
			fst.transitions
		in
		
		if has_output_eps_loop then
			true (* Immediately fail, FST is ambiguous *)
		else
			false (* Does not have this specific ambiguity *)
	
	(**
	* Computes the ε-closure of a state, keeping track of output words
	* produced along ε-transitions.
	*)
	let closeEmptyOut (st: state) (ts: transitions4) : (state * word) Set.t =
		let rec explore (frontier: (state * word) Set.t) (visited: (state * word) Set.t) =
			if Set.subset frontier visited then visited
			else
				let next =
					Set.flatMap
						(fun (s, out) ->
							let epsTr = Set.filter (fun (a, b, _, _) -> a = s && b = epsilon) ts in
							Set.map
								(fun (_, _, epsOut, s2) -> (s2, out @ [epsOut]))
								epsTr
						)
						frontier
				in
				let newSet = Set.union frontier visited in
				explore next newSet
		in
		explore (Set.make [(st, [])]) Set.empty

	(** 
	* Take the first n elements of a list.
	*)
	let rec prefix n xs =
		match (n, xs) with
		| 0, _ -> []
		| _, [] -> []
		| n, x::xs' -> x :: prefix (n-1) xs'

	(**
	* Check if all words in a set are prefix-compatible.
	* or any two words w1 and w2 in the set,
	* either w1 is a prefix of w2 or w2 is a prefix of w1.
	*)
	let prefix_consistent (outputs: word Set.t) : bool =
		let outs = Set.toList outputs in
		List.for_all (fun w1 ->
			List.for_all (fun w2 ->
				let l1 = List.length w1 in
				let l2 = List.length w2 in
				if l1 <= l2 then
					prefix l1 w2 = w1
				else
					prefix l2 w1 = w2
			) outs
		) outs

	(**
	* Checks if the transducer fst is deterministic.
	*
	* Deterministic means:
	*  - there are no self looping ε-transitions that produce output
	*  - ε-closure must not yield multiple output words without consuming input
	*  - For each (state, input), at most one resulting (nextState, outputWord) is possible
	*)
	let isDeterministicEpsilon (fst: t) : bool =
		if (isSelfLoop fst) then
			false (* Fails the fast ambiguity check *)
		else
		Set.for_all
			(fun st ->
				let epsClosure = closeEmptyOut st fst.transitions in
				(* ε-output ambiguity check *)
				let epsOutputs = Set.map snd epsClosure in
					if not (prefix_consistent epsOutputs) then
						false
					else
				(* Determinism for each input symbol *)
				Set.for_all
					(fun input ->
						if input = epsilon then 
							true  (* skip ε input symbol explicitly *)
						else
							let results =
								Set.flatMap
									(fun (s, outPrefix) ->
										(* transitions consuming this input *)
										let trs =
											Set.filter (fun (a, b, _, _) -> a = s && b = input) fst.transitions
										in
										(* follow each transition, accumulate output, then close with ε *)
										Set.flatMap
											(fun (_, _, out, s2) ->
													let eps2 = closeEmptyOut s2 fst.transitions in
													Set.map
														(fun (sFinal, outSuffix) ->
															(sFinal, outPrefix @ (out :: outSuffix))
														)
														eps2
											)
											trs
									)
								epsClosure
							in
						(* Deterministic if ≤ 1 possible (state, output) result *)
						Set.size results <= 1
					)
				fst.inAlphabet
			)
		fst.states
	
	(**
	* Checks if the transducer fst is deterministic.
	*
	* Deterministic means:
	*  - The transducer has no ε-transitions.
	*  - For every (state, input symbol), there is at most one transition.
	*  - For each  transition, there is a single output.
	*)
	let isDeterministic (fst: t) : bool =
	let has_epsilon =
		Set.exists (fun (_, b, _, _) -> b = epsilon) fst.transitions
	in
	if has_epsilon then
		false
	else
		Set.for_all
		(fun st ->
			Set.for_all
			(fun input ->
				let trs =
					Set.filter (fun (a, b, _, _) -> a = st && b = input) fst.transitions
				in
				let outs = Set.map (fun (_,_,out,_) -> out) trs in
				let dests = Set.map (fun (_,_,_,d) -> d) trs in
				Set.size trs <= 1 && Set.size outs <= 1 && Set.size dests <= 1
			)
			fst.inAlphabet
		)
		fst.states

	(* Get all states reachable from 'sts' via epsilon-input transitions *)
	let rec closeEmpty (sts: states) (ts: transitions4) : states =
		let nextEps = Set.flatMap (fun st ->
		Set.map (fun (_,_,_,d) -> d)
			(Set.filter (fun (a,b,_,_) -> a = st && b = epsilon) ts)
		) sts in
		let newSts = Set.union sts nextEps in
		if Set.equals sts newSts then sts
		else closeEmpty newSts ts

	(* Get states reachable from 'sts' on one symbol 'sy' *)
	let move (sts: states) (sy: symbol) (ts: transitions4) : states =
		Set.flatMap (fun st ->
		Set.map (fun (_,_,_,d) -> d)
			(Set.filter (fun (a,b,_,_) -> a = st && b = sy) ts)
		) sts

	(* generates the set of states reachable from the given state set though the given symbol *)
	let newR (oneR: states) (sy: symbol) (ts: transitions4) : states =
		let nxtSts = move oneR sy ts in
		Set.union nxtSts (closeEmpty nxtSts ts)

	(* creates all transitions (given state set, a given symbol, output, states reachable) *)
	let rToTs (r: states) (all_transitions: transitions4) in_alphabet =
		let nxtTrans = Set.map (fun sy ->
			(* Find all 'move' transitions from set 'r' on 'sy' *)
			let moveTransitions = Set.filter (fun (a,b,_,_) -> Set.belongs a r && b = sy) all_transitions in

			(* Get all outputs from this 'move' *)
			let outputs = Set.map (fun (_,_,c,_) -> c) moveTransitions in

			(* Calculate the destination DFST state (move + closeEmpty) *)
			let destSet = newR r sy all_transitions in

			if Set.isEmpty outputs then
				(r, sy, epsilon, Set.empty) (* Placeholder, will be filtered *)
			else
				let out = List.hd (Set.toList outputs) in (* Pick the single output *)
        		(r, sy, out, destSet)

		) in_alphabet in
		Set.filter (fun (_,_,_,z) -> not (Set.isEmpty z)) nxtTrans
	
	(* applies previous function to all state sets until no new set is generated *)
	let rec rsToTs (stsD: states Set.t) (rD: states Set.t) (trnsD: (states * symbol * symbol * states) Set.t) (all_transitions: transitions4) in_alphabet =
		let nxtTs = Set.flatMap (fun stSet -> rToTs stSet all_transitions in_alphabet) rD in
		let nxtRs = Set.map (fun (_,_,_,z) -> z) nxtTs in 
		let newRs = Set.filter (fun r -> not (Set.belongs r stsD)) nxtRs in
		if Set.isEmpty newRs then (Set.union trnsD nxtTs) else
		rsToTs (Set.union newRs stsD) newRs (Set.union trnsD nxtTs) all_transitions in_alphabet

	(* Gets the longest common prefix of two words *)
	let rec lcp w1 w2 =
		match (w1, w2) with
		| (x::xs, y::ys) when x = y -> x :: (lcp xs ys)
		| _ -> []

	(* Gets the suffix of a word after removing a prefix *)
	let rec suffix prefix w =
		match (prefix, w) with
		| ([], _) -> w
		| (p::ps, x::xs) when p = x -> suffix ps xs
		| _ -> w (* Prefix doesn't match, return original word *)

	(*
	* This is the new "DFST state". It's a set of
	* (NFST state * pending output word) pairs.
	*)
	type dfstState = (state * word) Set.t

	(*
	* This function "fuses" the dfstState into a single string
	* name.
	*)
	let fusedfstState (st: dfstState) : state =
		let l = Set.toList st in
		let sorted_l = List.sort (fun (s1, w1) (s2, w2) ->
			if s1 <> s2 then compare s1 s2 else compare w1 w2
		) l in
		let s = String.concat "," (List.map (fun (s, w) ->
			s ^ "" ^ (String.concat "" (List.map Symbol.symbD w))
		) sorted_l) in
	"{" ^ s ^ "}"

	(**
	* Check for
	* No ε-transitions from a state that produce two different outputs.
	* No two transitions from the same (state, input) producing different outputs.
	*)
	let hasConflictOut (fst: t): bool =
		(* ε-transition ambiguity: same source state, multiple ε outputs *)
		let eps_conflict =
			Set.exists (fun st ->
			let outs =
				Set.map (fun (_,_,out,_) -> out)
				(Set.filter (fun (a,b,_,_) -> a = st && b = epsilon) fst.transitions)
			in
			Set.size outs > 1
			) fst.states
		in

		(* input transition ambiguity: same (state, input), multiple outputs *)
		let input_conflict =
			Set.exists (fun st ->
			Set.exists (fun input ->
				if input = epsilon then false else
				let outs =
				Set.map (fun (_,_,out,_) -> out)
					(Set.filter (fun (a,b,_,_) -> a = st && b = input) fst.transitions)
				in
				Set.size outs > 1
			) fst.inAlphabet
			) fst.states
		in

	not eps_conflict && not input_conflict


	(**
	* This function converts the non-deterministic fst into its deterministic equivalent if it exists
	*)
	let toDeterministicEpsilon (fst: t): t =
	
	if (isSelfLoop fst) then
		(Error.error "toDeterministicEpsilon"
		"The FST is infinitely ambiguous and cannot be determinized." ();
		fst)
	else if not (hasConflictOut fst) then
		(Error.error "toDeterministicEpsilon"
		"The FST has conflicting outputs and cannot be determinized." ();
		fst)
	else

		let dfstStates: dfstState Set.t ref = ref Set.empty in
		let newDfaTransitions: (state * symbol * symbol * state) Set.t ref = ref Set.empty in

		let getNext (q: dfstState) (sy: symbol) (all_transitions: transitions4) =
			let allResults =
				Set.flatMap
				(fun (st, outPrefix) -> 
					let moveTransitions =
					Set.filter (fun (a, b, _, _) -> a = st && b = sy) all_transitions
					in
					Set.flatMap
					(fun (_, _, moveOut, destSt) ->
						let newPrefix = outPrefix @ [moveOut] in
						let epsClosure = closeEmptyOut destSt all_transitions in
						Set.map
						(fun (s, epsOut) -> (s, newPrefix @ epsOut))
						epsClosure
					)
					moveTransitions
				)
				q
			in
			if Set.isEmpty allResults then
				None
			else
				begin
				let allWords = Set.map (fun (_, w) -> w) allResults in
				let firstWord = (Set.toList allWords) |> List.hd in
				let lcpWord = Set.fold_left lcp firstWord allWords in
				
				let newdfstState =
					Set.map (fun (s, w) -> (s, suffix lcpWord w)) allResults
				in

				let outSymbol =
					if List.length lcpWord > 0 then List.hd lcpWord else epsilon
				in
				
				Some (outSymbol, newdfstState)
				end
		in

		(* Initial state is the epsilon-closure of the original start state *)
		let r1 = closeEmptyOut fst.initialState fst.transitions in

		(* Use a worklist to find all reachable DFST states and transitions *)
		let worklist = ref [r1] in
		dfstStates := Set.add r1 !dfstStates;

		while !worklist <> [] do
		let currentdfstState = List.hd !worklist in
		worklist := List.tl !worklist;
		let srcFused = fusedfstState currentdfstState in

		Set.iter (fun sy ->
			match getNext currentdfstState sy fst.transitions with
			| Some (outSymbol, nextdfstState) ->
				let destFused = fusedfstState nextdfstState in
				
				if outSymbol <> epsilon then
				newDfaTransitions := Set.add (srcFused, sy, outSymbol, destFused) !newDfaTransitions;
				
				if not (Set.belongs nextdfstState !dfstStates) then (
				dfstStates := Set.add nextdfstState !dfstStates;
				worklist := nextdfstState :: !worklist
				)
			| None -> ()
		) fst.inAlphabet
		done;

		(* Determine new accepting states *)
		let newAllSts = Set.map fusedfstState !dfstStates in
		let newAccSts =
		Set.map fusedfstState (
			Set.filter (fun dfstState ->
			Set.exists (fun (st, _) -> Set.belongs st fst.acceptStates) dfstState
			) !dfstStates
		)
		in
		let newOutAlf = Set.map (fun (_,_,c,_) -> c) !newDfaTransitions in

		(* Build the new FST *)
		{
		inAlphabet = fst.inAlphabet;
		outAlphabet = Set.diff newOutAlf (Set.make [epsilon]);
		states = newAllSts;
		initialState = fusedfstState r1;
		transitions = !newDfaTransitions;
		acceptStates = newAccSts
		}
	
	(**
	* Converts a possibly ε-nondeterministic FST into an equivalent deterministic one,
	* as long as ε-transitions do not produce any output symbols.
	*
	* Determinization fails (and returns the original fst) if:
	*   - there are ε self-loops with output (infinite ambiguity)
	*   - there are conflicting outputs for the same (state, input)
	*
	*)
	let toDeterministic (fst: t): t =
		let has_bad_eps =
			Set.exists (fun (_, b, c, _) -> b = epsilon && c <> epsilon) fst.transitions
		in
		if has_bad_eps then (
			Error.error "toDeterministic"
			"The FST has ε-transitions that emit output, cannot determinize." ();
			fst
		) else
		let dfaStates : states Set.t ref = ref Set.empty in
		let newTransitions : (state * symbol * symbol * state) Set.t ref = ref Set.empty in

		let move (sts: states) (sy: symbol) (ts: transitions4) : states * symbols =
			let relevant =
			Set.filter (fun (a,b,_,_) -> Set.belongs a sts && b = sy) ts
			in
			let next = Set.map (fun (_,_,_,d) -> d) relevant in
			let outs = Set.map (fun (_,_,c,_) -> c) relevant in
			(next, outs)
		in

		let startSet = closeEmpty (Set.make [fst.initialState]) fst.transitions in
		let worklist = ref [startSet] in
		dfaStates := Set.add startSet !dfaStates;

		while !worklist <> [] do
			let current = List.hd !worklist in
			worklist := List.tl !worklist;
			let srcName = fusedfstState (Set.map (fun s -> (s, [])) current) in

			Set.iter (fun sy ->
			if sy <> epsilon then (
				let nextSts, outs = move current sy fst.transitions in
				if not (Set.isEmpty nextSts) then
				let closedNext = closeEmpty nextSts fst.transitions in
				let destName = fusedfstState (Set.map (fun s -> (s, [])) closedNext) in

				if Set.size outs > 1 then (
					Error.error "toDeterministic"
					"Multiple distinct outputs found for same (state,input), cannot determinize." ();
					()
				) else (
					let outSymbol =
					if Set.isEmpty outs then epsilon
					else List.hd (Set.toList outs)
					in

					newTransitions := Set.add (srcName, sy, outSymbol, destName) !newTransitions;

					if not (Set.belongs closedNext !dfaStates) then (
					dfaStates := Set.add closedNext !dfaStates;
					worklist := closedNext :: !worklist
					)
				)
			)
			) fst.inAlphabet
		done;

		let newStates = Set.map (fun sset -> fusedfstState (Set.map (fun s -> (s, [])) sset)) !dfaStates in
		let newAccepts =
			Set.filter (fun sname ->
			let compSet = Set.find (fun sset -> fusedfstState (Set.map (fun s -> (s, [])) sset) = sname) !dfaStates in
			Set.exists (fun st -> Set.belongs st fst.acceptStates) compSet
			) newStates
		in
		let newOutAlf = Set.map (fun (_,_,c,_) -> c) !newTransitions in

		{
			inAlphabet = fst.inAlphabet;
			outAlphabet = Set.diff newOutAlf (Set.make [epsilon]);
			states = newStates;
			initialState = fusedfstState (Set.map (fun s -> (s, [])) startSet);
			transitions = !newTransitions;
			acceptStates = newAccepts;
		}

	let isComplete (fst: t): bool =
		Set.for_all
			(fun st ->
			Set.for_all
				(fun input ->
				if input = epsilon then true
				else
					Set.exists
					(fun (a, b, _, _) -> a = st && b = input)
					fst.transitions
				)
				fst.inAlphabet
		)
		fst.states

	(** 
	* Checks whether a given finite-state transducer (fst) is a Moore machine.
	* 
	* A Moore machine must satisfy three conditions:
	*  1. It is deterministic — for each (state, input) pair there is at most one transition.
	*  2. It is complete — for each state and input, there is at least one transition.
	*  3. Every state has a single output symbol (the output depends only on the state, not on the input).
	*)
	let isMooreMachine (fst: t): bool =
		let deterministic = isDeterministic fst in
		let complete = isComplete fst in
		let state_has_single_output =
			Set.for_all
				(fun st ->
					(* Get all transitions that start from this state *)
					let outgoing = Set.filter (fun (a, _, _, _) -> a = st) fst.transitions in
					(* Extract all output symbols from those transitions *)
					let outputs = Set.map (fun (_, _, c, _) -> c) outgoing in
					(* For Moore: all outgoing transitions from this state must share the same output *)
					Set.size outputs <= 1
				)
				fst.states
		in
	deterministic && complete && state_has_single_output


	(**
	* Checks whether a given finite-state transducer (fst) is a Mealy machine.
	*
	* A Mealy machine must satisfy three conditions:
	*  1. It is deterministic — no two transitions share the same (state, input) pair.
	*  2. It is complete — for every state and input symbol, there exists a transition.
	*  3. For each (state, input) pair, there is at most one output symbol.
	*)
	let isMealyMachine (fst: t): bool =
		let deterministic = isDeterministic fst in
		let complete = isComplete fst in
		let transition_output_ok =
			Set.for_all
				(fun st ->
					Set.for_all
						(fun input ->
							if input = epsilon then true (* ignore epsilon transitions *)
							else
								(* Filter all transitions with this source state and input symbol *)
								let transitions_for_input =
									Set.filter
										(fun (a, b, _, _) -> a = st && b = input)
										fst.transitions
								in
								(* Extract all output symbols from those transitions *)
								let outputs =
									Set.map (fun (_, _, c, _) -> c)
										transitions_for_input
								in
								(* Must have at most one unique output per (state, input) *)
								Set.size outputs <= 1
						)
						fst.inAlphabet
				)
				fst.states
		in
		deterministic && complete && transition_output_ok

	(**
	* Minimizes a deterministic transducer.
	* It first determinizes, cleans useless states,
	* then partitions states by (final/nonfinal) and refines using
	* signatures of outgoing transitions: (input, output, destination block).
	*)
	let minimize (transducer: t): t =
		(* Ensure DFST and remove dead/unreachable *)
		let fst_det = toDeterministic transducer in
		let fst_clean = cleanUselessStates fst_det in

		(* Initial partition: finals vs nonfinals *)
		let finals, nonfinals =
			Set.partition (fun st -> Set.belongs st fst_clean.acceptStates) fst_clean.states
		in
		let init_partition =
			Set.filter (fun b -> Set.size b > 0) (Set.make [finals; nonfinals])
		in

		(* Find the block in a partition that contains a given state *)
		let block_of (partition: states Set.t) (st: state) : states =
			Set.find (fun b -> Set.belongs st b) partition
		in

		(* Compute a state's signature relative to a partition:
		the set of (input, output, destBlock) for all outgoing transitions *)
		let signature (partition: states Set.t) (st: state) =
			let outgoing = Set.filter (fun (a,_,_,_) -> a = st) fst_clean.transitions in
			Set.map
			(fun (_, inp, out, dst) -> (inp, out, block_of partition dst))
			outgoing
		in

		(* Refine one block into groups of states with identical signatures *)
		let refine_block (partition: states Set.t) (block: states) : states list =
			let pairs =
			Set.map (fun st -> (st, signature partition st)) block |> Set.toList
			in
			let rec group acc = function
			| [] -> acc
			| (s, sigs) :: rest ->
				let same, diff =
					List.partition (fun (_, sigs2) -> sigs2 = sigs) rest
				in
				let new_block = Set.make (s :: List.map (fun (x, _) -> x) same) in
				group (new_block :: acc) diff

			in
			group [] pairs
		in

		(* Refine all blocks until fixpoint *)
		let rec refine (partition: states Set.t) : states Set.t =
			let refined =
			Set.flatMap
				(fun block -> Set.make (refine_block partition block))
				partition
			in
			if Set.equals refined partition then partition else refine refined
		in

		let final_partition = refine init_partition in

		(* Choose a representative per block and a translator *)
		let representative (block: states) : state = List.hd (Set.toList block) in
		let translate (st: state) : state =
			representative (block_of final_partition st)
		in

		(* Rebuild minimized machine *)
		let new_states   = Set.map representative final_partition in
		let new_init     = translate fst_clean.initialState in
		let new_accepts  =
			Set.map representative
			(Set.filter
				(fun blk -> Set.exists (fun st -> Set.belongs st fst_clean.acceptStates) blk)
				final_partition)
		in
		let new_trans =
			Set.map (fun (a,b,c,d) -> (translate a, b, c, translate d)) fst_clean.transitions
		in

		{
			inAlphabet   = fst_clean.inAlphabet;
			outAlphabet  = fst_clean.outAlphabet;
			states       = new_states;
			initialState = new_init;
			transitions  = new_trans;
			acceptStates = new_accepts;
		}
		
	(**
	* This function verifies if the fst is minimal
	*)
	let isMinimized (fst: t): bool =
		let min = minimize fst in
			Set.size fst.states = Set.size min.states
   
   (**
   * Converts a Finite-State Transducer (FST) into an equivalent
   * 2-tape Turing Machine (TM).
   *
   * Tape 1: Read-only input tape
   * Tape 2: Write-only output tape
   *)
  let asTuringMachine (fst: t): TuringMachine.t =
    let tm_accept_state = "q_accept_tm" in
    let tm_states = Set.add tm_accept_state fst.states in
    let tm_in_alphabet = fst.inAlphabet in
    
    let tm_empty = empty in
    
    let tm_tape_alphabet =
      Set.union (Set.union fst.inAlphabet fst.outAlphabet) (Set.make [tm_empty])
    in
    
    let tm_initial_state = fst.initialState in
    let tm_accept_states = Set.make [tm_accept_state] in
    
    (* Handle transitions that consume input *)
    let input_consuming_trs = Set.filter (fun (_, inp, _, _) -> inp <> epsilon) fst.transitions in
    let tm_trs_1 = Set.map (fun (q1, a, b_out, q2) ->
      (*
       * FST: (q1, a, b, q2)
       * TM:  In state q1, read 'a' (tape 1) and 'empty' (tape 2)
       * Go to q2, write 'a' (tape 1), write 'b' (tape 2)
       * Move Tape 1 Right, Tape 2 Right/Stay
       *)
      let (write_b, move_b) =
        if b_out = epsilon then (tm_empty, S)
        else (b_out, R) 
      in
      (q1, [a; tm_empty], q2, [a; write_b], [R; move_b])
    ) input_consuming_trs in

    (* Handle transitions that do NOT consume input (epsilon-input) *)
    let epsilon_consuming_trs = Set.filter (fun (_, inp, _, _) -> inp = epsilon) fst.transitions in
    
    let all_input_symbols_and_empty = Set.add tm_empty fst.inAlphabet in

    let tm_trs_2 = Set.flatMap (fun (q1, _, b_out, q2) ->
      (*
       * FST: (q1, ~, b, q2)
       * TM:  For each symbol 's' on Tape 1:
       * In state q1, read 's' (tape 1) and 'empty' (tape 2)
       * Go to q2, write 's' (tape 1), write 'b' (tape 2)
       * Move Tape 1 Stay, Tape 2 Right/Stay
       *)
      let (write_b, move_b) =
        if b_out = epsilon then (tm_empty, S) 
        else (b_out, R) 
      in
      Set.map (fun s ->
        (q1, [s; tm_empty], q2, [s; write_b], [S; move_b])
      ) all_input_symbols_and_empty
    ) epsilon_consuming_trs in

    (* Handle transitions to the final accept state *)
    let tm_trs_3 = Set.map (fun q_f ->
      (*
       * FST: state q_f is accepting
       * TM:  If in state q_f and input tape is empty, move to tm_accept_state
       *)
      (q_f, [tm_empty; tm_empty], tm_accept_state, [tm_empty; tm_empty], [S; S])
    ) fst.acceptStates in

    let all_tm_trs = Set.union (Set.union tm_trs_1 tm_trs_2) tm_trs_3 in

    let tm_data = {
      states = tm_states;
      entryAlphabet = tm_in_alphabet;
      tapeAlphabet = tm_tape_alphabet;
      initialState = tm_initial_state;
      empty = tm_empty;
      transitions = all_tm_trs;
      acceptStates = tm_accept_states;
      criteria = true;
      lbMarkers = [];
	  _nTapes = 2 
    } in
    
    (TuringMachine.make (Arg.Representation tm_data) : TuringMachine.t)
	
end

(* * 
 * This module is added to provide composition functionality.
 *)
module TransducerComposition =
struct
  open TransducerSupport
  open TransducerPrivate

  let fuse_states (s1: state) (s2: state) : state =
	s1 ^ "_" ^ s2

  (**
   * Composes two finite-state transducers.
   * T1: A -> B
   * T2: B -> C
   * T = T1 o T2: A -> C
   *)
  let compose (fst1: t) (fst2: t): t =
	
		let new_in_alphabet = fst1.inAlphabet in
		let tr2_eps_outputs =
			Set.map (fun (_, _, c, _) -> c)
			(Set.filter (fun (_, a, _, _) -> a = epsilon) fst2.transitions)
		in
		let new_out_alphabet = Set.union fst2.outAlphabet (Set.diff tr2_eps_outputs (Set.make[epsilon])) in
		
		let initial_pair = (fst1.initialState, fst2.initialState) in
		let initial_fused_state = fuse_states fst1.initialState fst2.initialState in
		
		let new_states = ref (Set.make [initial_fused_state]) in
		let new_transitions = ref Set.empty in
		let new_accept_states = ref Set.empty in
		
		let worklist = ref (Set.make [initial_pair]) in
		let visited = ref (Set.make [initial_pair]) in

		if Set.belongs fst1.initialState fst1.acceptStates && Set.belongs fst2.initialState fst2.acceptStates then
			new_accept_states := Set.add initial_fused_state !new_accept_states;

		while not (Set.isEmpty !worklist) do
			let (q1, q2) = List.hd (Set.toList !worklist) in
			worklist := Set.remove (q1, q2) !worklist;
			
			let fused_src = fuse_states q1 q2 in
			(*
			* Case 1: Lock-step (T1 consumes input 'a' != eps, T2 consumes T1's output 'b')
			* T1: q1 --a/b--> q1' (a != epsilon)
			* T2: q2 --b/c--> q2'
			* T: (q1,q2) --a/c--> (q1',q2')
			*)
			let tr1_moves = Set.filter (fun (s, a, _, _) -> s = q1 && a != epsilon) fst1.transitions in
			Set.iter (fun (q1, a, b, q1') ->
			let tr2_matches = Set.filter (fun (s, b', _, _) -> s = q2 && b' = b) fst2.transitions in
			Set.iter (fun (q2, b, c, q2') ->
				let new_pair = (q1', q2') in
				let fused_dst = fuse_states q1' q2' in
				
				new_transitions := Set.add (fused_src, a, c, fused_dst) !new_transitions;
				new_states := Set.add fused_dst !new_states;
				
				if Set.belongs q1' fst1.acceptStates && Set.belongs q2' fst2.acceptStates then
				new_accept_states := Set.add fused_dst !new_accept_states;

				if not (Set.belongs new_pair !visited) then (
				visited := Set.add new_pair !visited;
				worklist := Set.add new_pair !worklist
				)
			) tr2_matches
			) tr1_moves;

			(*
			* Case 2: T1 epsilon-step (T1 consumes epsilon, T2 consumes T1's output 'b')
			* T1: q1 --eps/b--> q1'
			* T2: q2 --b/c--> q2'
			* T: (q1,q2) --eps/c--> (q1',q2')
			*)
			let tr1_eps = Set.filter (fun (s, a, _, _) -> s = q1 && a = epsilon) fst1.transitions in
			Set.iter (fun (q1, _, b, q1') ->
			let tr2_matches = Set.filter (fun (s, b', _, _) -> s = q2 && b' = b) fst2.transitions in
			Set.iter (fun (q2, b, c, q2') ->
				let new_pair = (q1', q2') in
				let fused_dst = fuse_states q1' q2' in

				new_transitions := Set.add (fused_src, epsilon, c, fused_dst) !new_transitions;
				new_states := Set.add fused_dst !new_states;

				if Set.belongs q1' fst1.acceptStates && Set.belongs q2' fst2.acceptStates then
				new_accept_states := Set.add fused_dst !new_accept_states;

				if not (Set.belongs new_pair !visited) then (
				visited := Set.add new_pair !visited;
				worklist := Set.add new_pair !worklist
				)
			) tr2_matches
			) tr1_eps;

			(*
			* Case 3: T2 epsilon-step (T1 does nothing, T2 consumes epsilon)
			* T1: (stuck at q1)
			* T2: q2 --eps/c--> q2'
			* T: (q1,q2) --eps/c--> (q1,q2')
			*)
			let tr2_eps = Set.filter (fun (s, a, _, _) -> s = q2 && a = epsilon) fst2.transitions in
			Set.iter (fun (q2, _, c, q2') ->
			let new_pair = (q1, q2') in 
			let fused_dst = fuse_states q1 q2' in

			new_transitions := Set.add (fused_src, epsilon, c, fused_dst) !new_transitions;
			new_states := Set.add fused_dst !new_states;

			if Set.belongs q1 fst1.acceptStates && Set.belongs q2' fst2.acceptStates then
				new_accept_states := Set.add fused_dst !new_accept_states;

			if not (Set.belongs new_pair !visited) then (
				visited := Set.add new_pair !visited;
				worklist := Set.add new_pair !worklist
			)
			) tr2_eps;

		done;
		
		{
			inAlphabet = new_in_alphabet;
			outAlphabet = new_out_alphabet;
			states = !new_states;
			initialState = initial_fused_state;
			transitions = !new_transitions;
			acceptStates = !new_accept_states
		}

   (*
   * Union
   *)
  let union (fst1: t) (fst2: t): t =
		let rename_state (suffix: string) (st: state) : state =
			st ^ suffix
		in
		let s1_suffix = "_1" in
		let s2_suffix = "_2" in
		
		let new_init = "S_init_union" in
		
		let rename_trans suffix (a,b,c,d) =
			(rename_state suffix a, b, c, rename_state suffix d)
		in
		
		let sts1 = Set.map (rename_state s1_suffix) fst1.states in
		let sts2 = Set.map (rename_state s2_suffix) fst2.states in
		let new_states = Set.union (Set.union sts1 sts2) (Set.make [new_init]) in
		
		let trs1 = Set.map (rename_trans s1_suffix) fst1.transitions in
		let trs2 = Set.map (rename_trans s2_suffix) fst2.transitions in
		
		let init_trs = Set.make [
			(new_init, epsilon, epsilon, rename_state s1_suffix fst1.initialState);
			(new_init, epsilon, epsilon, rename_state s2_suffix fst2.initialState)
		] in
		let new_transitions = Set.union (Set.union trs1 trs2) init_trs in
		
		let acc1 = Set.map (rename_state s1_suffix) fst1.acceptStates in
		let acc2 = Set.map (rename_state s2_suffix) fst2.acceptStates in
		let new_accepts = Set.union acc1 acc2 in
		
		{
			inAlphabet = Set.union fst1.inAlphabet fst2.inAlphabet;
			outAlphabet = Set.union fst1.outAlphabet fst2.outAlphabet;
			states = new_states;
			initialState = new_init;
			transitions = new_transitions;
			acceptStates = new_accepts
		}

  (*
   * Intersection
   * Implements T1 n T2.
   * This is a product construction where a transition exists
   * only if both machines take the *same input* and produce the
   * *same output*.
   *)
  let intersection (fst1: t) (fst2: t): t =
		let new_in_alphabet = Set.inter fst1.inAlphabet fst2.inAlphabet in
		let new_out_alphabet = Set.inter fst1.outAlphabet fst2.outAlphabet in
		
		let initial_pair = (fst1.initialState, fst2.initialState) in
		let initial_fused_state = fuse_states fst1.initialState fst2.initialState in
		
		let new_states = ref (Set.make [initial_fused_state]) in
		let new_transitions = ref Set.empty in
		let new_accept_states = ref Set.empty in
		
		let worklist = ref (Set.make [initial_pair]) in
		let visited = ref (Set.make [initial_pair]) in

		if Set.belongs fst1.initialState fst1.acceptStates && Set.belongs fst2.initialState fst2.acceptStates then
			new_accept_states := Set.add initial_fused_state !new_accept_states;

		while not (Set.isEmpty !worklist) do
			let (q1, q2) = List.hd (Set.toList !worklist) in
			worklist := Set.remove (q1, q2) !worklist;
			
			let fused_src = fuse_states q1 q2 in

			let trs1 = Set.filter (fun (s, _, _, _) -> s = q1) fst1.transitions in
			let trs2 = Set.filter (fun (s, _, _, _) -> s = q2) fst2.transitions in

			(*
			* Case 1: Lock-step on (input, output)
			* Find all pairs of transitions (t1, t2) where
			* t1.input = t2.input AND t1.output = t2.output.
			* This includes (eps, eps) transitions.
			*)
			Set.iter (fun (q1, a1, c1, q1') ->
			let matching_trs2 =
				Set.filter (fun (_, a2, c2, _) -> a1 = a2 && c1 = c2) trs2
			in
			Set.iter (fun (q2, a, c, q2') ->
				let new_pair = (q1', q2') in
				let fused_dst = fuse_states q1' q2' in

				new_transitions := Set.add (fused_src, a, c, fused_dst) !new_transitions;
				new_states := Set.add fused_dst !new_states;

				if Set.belongs q1' fst1.acceptStates && Set.belongs q2' fst2.acceptStates then
				new_accept_states := Set.add fused_dst !new_accept_states;

				if not (Set.belongs new_pair !visited) then (
				visited := Set.add new_pair !visited;
				worklist := Set.add new_pair !worklist
				)
			) matching_trs2
			) trs1;
			
			(*
			* Case 2: T1 moves on (eps, eps), T2 stutters
			* This is for (eps, eps) only. (eps, output) is handled above.
			*)
			let tr1_eps_eps = Set.filter (fun (s, a, c, _) -> s = q1 && a = epsilon && c = epsilon) fst1.transitions in
			Set.iter (fun (q1, a, c, q1') ->
			let new_pair = (q1', q2) in
			let fused_dst = fuse_states q1' q2 in
			
			new_transitions := Set.add (fused_src, epsilon, epsilon, fused_dst) !new_transitions;
			new_states := Set.add fused_dst !new_states;

			if Set.belongs q1' fst1.acceptStates && Set.belongs q2 fst2.acceptStates then
				new_accept_states := Set.add fused_dst !new_accept_states;

			if not (Set.belongs new_pair !visited) then (
				visited := Set.add new_pair !visited;
				worklist := Set.add new_pair !worklist
			)
			) tr1_eps_eps;
			
			(*
			* Case 3: T2 moves on (eps, eps), T1 stutters
			*)
			let tr2_eps_eps = Set.filter (fun (s, a, c, _) -> s = q2 && a = epsilon && c = epsilon) fst2.transitions in
			Set.iter (fun (q2, a, c, q2') ->
			let new_pair = (q1, q2') in
			let fused_dst = fuse_states q1 q2' in
			
			new_transitions := Set.add (fused_src, epsilon, epsilon, fused_dst) !new_transitions;
			new_states := Set.add fused_dst !new_states;

			if Set.belongs q1 fst1.acceptStates && Set.belongs q2' fst2.acceptStates then
				new_accept_states := Set.add fused_dst !new_accept_states;

			if not (Set.belongs new_pair !visited) then (
				visited := Set.add new_pair !visited;
				worklist := Set.add new_pair !worklist
			)
			) tr2_eps_eps;

		done;
		
		{
			inAlphabet = new_in_alphabet;
			outAlphabet = new_out_alphabet;
			states = !new_states;
			initialState = initial_fused_state;
			transitions = !new_transitions;
			acceptStates = !new_accept_states
		}

   (*
   * Inverse
   * Creates a new transducer T' that inverts the input/output
   * relationship of T.
   * If T maps (w, v), then T' maps (v, w).
   *)
  let inverse (fst: t): t =
		let new_transitions =
			Set.map (fun (src, inp, outp, dst) ->
			(src, outp, inp, dst)
			) fst.transitions
		in
		{
			inAlphabet = fst.outAlphabet;
			outAlphabet = fst.inAlphabet;
			states = fst.states;
			initialState = fst.initialState;
			transitions = new_transitions;
			acceptStates = fst.acceptStates
		}
	
  (*
   * Concatenate
   * Implements T1 . T2.
   *)
  let concatenate (fst1: t) (fst2: t): t =
		let rename_state (suffix: string) (st: state) : state =
			st ^ suffix
		in
		let s1_suffix = "_1" in
		let s2_suffix = "_2" in
		
		let rename_trans suffix (a,b,c,d) =
			(rename_state suffix a, b, c, rename_state suffix d)
		in
		
		let sts1 = Set.map (rename_state s1_suffix) fst1.states in
		let sts2 = Set.map (rename_state s2_suffix) fst2.states in
		let new_states = Set.union sts1 sts2 in
		
		let trs1 = Set.map (rename_trans s1_suffix) fst1.transitions in
		let trs2 = Set.map (rename_trans s2_suffix) fst2.transitions in
		
		let connect_trs =
			Set.map (fun acc_st1 ->
			(rename_state s1_suffix acc_st1, epsilon, epsilon, rename_state s2_suffix fst2.initialState)
			) fst1.acceptStates
		in
		
		let new_transitions = Set.union (Set.union trs1 trs2) connect_trs in
		
		let new_init = rename_state s1_suffix fst1.initialState in
		
		let new_accepts = Set.map (rename_state s2_suffix) fst2.acceptStates in
		
		{
			inAlphabet = Set.union fst1.inAlphabet fst2.inAlphabet;
			outAlphabet = Set.union fst1.outAlphabet fst2.outAlphabet;
			states = new_states;
			initialState = new_init;
			transitions = new_transitions;
			acceptStates = new_accepts
		}

end

module Transducer =
struct
	include TransducerSupport
	open TransducerAccept
	open TransducerGenerate
	open TransducerPrivate
	open TransducerComposition

	(* Make *)
	let make2 (arg: t Arg.alternatives): Entity.t * t = make2 arg validate
	let make (arg: t Arg.alternatives): t = make arg validate

	(* Exercices support *)
	let checkProperty (fst: t) (prop: string) =
		match prop with
			| "deterministic" -> isDeterministic fst
			| "complete" -> isComplete fst
			| "moore" -> isMooreMachine fst
			| "mealy" -> isMealyMachine fst
			| "minimal" -> isMinimized fst
			| "clean" -> isClean fst
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
	let asFiniteAutomaton = asFiniteAutomaton
	let reachable = reachable
	let productive = productive
	let isDeterministic = isDeterministic
	let toDeterministic = toDeterministic
	let minimize = minimize
	let isMinimized = isMinimized
	let asTuringMachine = asTuringMachine
	let isComplete = isComplete
	let isMooreMachine = isMooreMachine
	let isMealyMachine = isMealyMachine
	let isClean = isClean
	let compose = compose 
	let union = union 
	let intersection = intersection 
	let inverse = inverse
	let concatenate = concatenate 

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
			method isClean: bool = isClean representation
			method minimize: t = minimize representation
			method isMinimized: bool = isMinimized representation
			method toDeterministic: t = toDeterministic representation
			method compose (fst: t): t = compose representation fst
			method union (fst: t): t = union representation fst
			method intersection (fst: t): t = intersection representation fst
			method inverse: t = inverse representation
			method concatenate (fst: t): t = concatenate representation fst
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
