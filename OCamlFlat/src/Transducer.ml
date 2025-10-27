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

  (* configuration: current state, remaining input word, and accumulated output word *)
  type config = state * word * word
  type configurations = config Set.t

  let initialConfigs (fst: t) (w: word) : configurations =
    Set.make [(fst.initialState, w, [])]

  let isAcceptingConfig (fst: t) (st, w, _out) : bool =
    Set.belongs st fst.acceptStates && w = []

  let nextConfigs (fst: t) (st, w, out) : configurations =
    match w with
    | [] ->
        (* epsilon transitions: consume no input, but may emit output *)
        let epsTr =
          Set.filter (fun (st1, sy, _, _) -> st1 = st && sy = epsilon) fst.transitions
        in
        Set.map (fun (_, _, outSym, st2) -> (st2, [], out @ [outSym])) epsTr

    | x::xs ->
        (* normal input-consuming transitions *)
        let consume =
          Set.filter (fun (st1, sy, _, _) -> st1 = st && sy = x) fst.transitions
        in
        let viaConsume =
          Set.map (fun (_, _, outSym, st2) -> (st2, xs, out @ [outSym])) consume
        in

        (* epsilon transitions possible without consuming input *)
        let epsTr =
          Set.filter (fun (st1, sy, _, _) -> st1 = st && sy = epsilon) fst.transitions
        in
        let viaEps =
          Set.map (fun (_, _, outSym, st2) -> (st2, w, out @ [outSym])) epsTr
        in

        Set.union viaConsume viaEps

  let accept (fst: t) (w: word) : bool =
    ignore (Model.checkWord fst.inAlphabet w);
    Model.accept fst w initialConfigs nextConfigs isAcceptingConfig

  let acceptFull (fst: t) (w: word) : bool * path * trail =
    ignore (Model.checkWord fst.inAlphabet w);
    Model.acceptFull fst w initialConfigs nextConfigs isAcceptingConfig
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
           (st2, w, out @ [outSym])       (* epsilon transition → no input consumed *)
         else
           (st2, inSym::w, out @ [outSym])  (* normal transition → prepend input to word *)
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
	

	(** CLEANING FUNCTIONS
	
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
 	CLEANING FUNCTIONS **)
		
	let isClean (fst: t): bool =
		let fa = asFiniteAutomaton fst in
		FiniteAutomaton.areAllStatesUseful fa
		(*s
		let usfSts = getUsefulStates fst in
			Set.size fst.states = Set.size usfSts
		*)
	
	(**
	* Computes the ε-closure of a state, keeping track of output words
	* (lists of symbols) produced along ε-transitions.
	* Returns a set of (state, word) pairs.
	*)
	let epsilonClosureWithOutput (st: state) (ts: transitions4) : (state * word) Set.t =
		let rec explore (frontier: (state * word) Set.t) (visited: (state * word) Set.t) =
			if Set.subset frontier visited then visited
			else
				let next =
					Set.flatMap
						(fun (s, out) ->
							(* ε-transitions from s *)
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
	*  - ε-closure must not yield multiple output words without consuming input
	*  - For each (state, input), at most one resulting (nextState, outputWord) is possible
	*)
	let isDeterministic (fst: t) : bool =
		Set.for_all
			(fun st ->
				let epsClosure = epsilonClosureWithOutput st fst.transitions in

				(* --- 1. ε-output ambiguity check --- *)
				let epsOutputs = Set.map snd epsClosure in
					if not (prefix_consistent epsOutputs) then
						false
					else
					(* --- 2. Determinism for each input symbol --- *)
					Set.for_all
						(fun input ->
								if input = epsilon then true  (* skip ε input symbol explicitly *)
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
															let eps2 = epsilonClosureWithOutput s2 fst.transitions in
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
			| "mealy" -> isMealyMachine fst
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
	let isDeterministic = isDeterministic
	let isComplete = isComplete
	let isMooreMachine = isMooreMachine
	let isMealyMachine = isMealyMachine
	let isClean = isClean

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
