(*
 * PolyModel.ml
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
 *  Written by Various
 *)

(*
 * ChangeLog:
 *
 * apr/2021 (amd) - Several new build functions.
 * jan/2021 (amd) - Created this module, collecting all the operation.
                    involving two or more kinds of models.
                    This allows to got rid of the mutual recursion between
                    modules, allowing storing each module in a different file.
 * dec/2019 (jg) - Initial code, across several modules in file "OCamlFlat.ml".
 *)

(*
 * Description: Poly-model operations.
 *
 * TODO: Cleanup.
 *)
 
open BasicTypes

(********************************************************************)
module RE2FA =
struct
	open RegularExpression
	open FiniteAutomaton

	(*auxiliary var for genName function*)
	let k = ref 0

	(*for each new state, generates a name that will distinguish it from all the other generated states *)
	let genName () =
		let n = !k in
		let () = k:= n + 1 in
			(*easy way of having all single digit state names have a zero before their actual number*)
			let name = if n > 9 then "new_St" ^ (string_of_int n)
						else "new_St0" ^ (string_of_int n) in
				str2state name

	let rec compile (re: RegularExpression.t) : FiniteAutomaton.t =
		match re with
			| Plus(l, r) ->
					let fa1 = compile l in
					let fa2 = compile r in
					let newStart = genName () in
					let newSts = Set.add newStart (Set.union fa1.states fa2.states) in
					let newAccSts = Set.union fa1.acceptStates fa2.acceptStates in
					let newTran1 = (newStart, epsilon, fa1.initialState) in
					let newTran2 = (newStart, epsilon, fa2.initialState) in
					let newTrans = Set.add newTran1 (Set.add newTran2
						(Set.union fa1.transitions fa2.transitions)) in
					let newAlf = Set.union fa1.alphabet fa2.alphabet in
						{alphabet = newAlf; states = newSts; initialState = newStart;
							transitions = newTrans; acceptStates = newAccSts}
			| Seq(l, r) ->
					let fa1 = compile l in
					let fa2 = compile r in
					let ist = fa1.initialState in
					let sts = Set.union fa1.states fa2.states in
					let asts = fa2.acceptStates in
					let newTrns = Set.map (fun x -> (x, epsilon, fa2.initialState) ) fa1.acceptStates in
					let trns = Set.union newTrns (Set.union fa1.transitions fa2.transitions) in
					let alf = Set.union fa1.alphabet fa2.alphabet in
						{alphabet = alf; states = sts; initialState = ist;
							transitions = trns; acceptStates = asts}
			| Star(r) ->
					let fa = compile r in
					let newStart = genName () in
					let newSts = Set.add newStart fa.states in
					let newTrns = Set.map (fun st -> (st, epsilon, newStart)) fa.acceptStates in
					let allNewTrns = Set.add (newStart, epsilon, fa.initialState) (Set.union newTrns fa.transitions) in
						{alphabet = fa.alphabet; states = newSts; initialState = newStart;
							transitions = allNewTrns; acceptStates = Set.make [newStart]}
			| Symb(c) ->
					let newStart = genName () in
					let newAcc = genName () in
					let newSts = Set.make [newStart; newAcc] in
					let newTrn = Set.make [(newStart, c, newAcc)] in
						{alphabet = Set.make [c]; states = newSts; initialState = newStart;
							transitions = newTrn; acceptStates = Set.make [newAcc]}
			| Empty ->
					let newStart = genName () in
							{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
								transitions = Set.empty; acceptStates = Set.make [newStart]}
			| Zero ->
					let newStart = genName () in
						{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
								transitions = Set.empty; acceptStates = Set.empty}
	
	let re2fa (re: RegularExpression.t): FiniteAutomaton.t =
		compile re
end

(********************************************************************)
module FA2RE =
struct
	open FiniteAutomaton
	open RegularExpression

	(* transforms the set of expressions into the regex: plus of all expressions of the set *)
	let plusSet reSet =
		let rec pls l =
			match l with
				[] -> Zero
				| x::xs -> if xs = [] then x else Plus (x, pls xs)
		in
			pls (Set.toList reSet)

	(* For the given i and j, returns the value of R when k is zero.
		Note that k will always be 0 when called inside this method *)
	let calczerok k i j trns =
		let ts = Set.filter (fun (a,_,b) -> i = a && j = b) trns in
		if ts <> Set.empty then
			if i <> j then
				let res = Set.map (fun (_,c,_) -> Symb c) ts in
					(k,i,j,plusSet res)
			else
				let res = Set.map (fun (_,c,_) -> Symb c) ts in
				let re = Plus(Empty, (plusSet res)) in
					(k,i,j,re)

		else (k,i,j,Zero)
		
	let getRij i j prvK =
		let r = Set.nth (Set.filter (fun (_,x,y,_) -> x = i && y = j) prvK) 0 in
			(fun (_,_,_,re) -> re) r

	let assembleRe st i j prvK =
		let rik = getRij i st prvK in
		let rkk = Star (getRij st st prvK) in
		let rkj = getRij st j prvK in
			Seq(rik, Seq(rkk,rkj))
				
	(* For the given i and j, returns the value of R when k is not zero. *)
	let calck k i j prvK sts =
		let rij = getRij i j prvK in
		let rikjs = Set.map (fun st -> assembleRe st i j prvK) sts in
		let rikj = plusSet rikjs in
			(k,i,j,Plus(rij,rikj))

	(* Main function that applies previous 2 functions to all possible i and j pairs *)
	let rec rkij k trns sts =
		if k < 1 then
			Set.map (fun (i,j) -> calczerok k i j trns) (Set.product sts sts)
		else
			let prvK = rkij (k-1) trns sts in
				Set.map (fun(i,j) -> calck k i j prvK sts) (Set.product sts sts)

	let fa2re (fa: FiniteAutomaton.t): RegularExpression.t =
		(* Since the algorithm only works for deterministic automaton, we first convert it
			to its deterministic equivalent *)
		let fa = FiniteAutomaton.toDeterministic fa in
		let sts = fa.states in
		let trns = fa.transitions in
		let allRks = rkij (Set.size sts) trns sts in
		let result = Set.filter (fun (_,i,j,_) -> i = fa.initialState && Set.belongs j fa.acceptStates ) allRks in
		let res = Set.map (fun (_,_,_,re) -> re) result in		
			plusSet res
		(*	simplify (plusSet res) *)
end

(********************************************************************)
module RE2CFG =
struct
	open RegularExpression
	open ContextFreeGrammarBasic
	
	(*auxiliary var for genVar function*)
	let k = ref 0

	(* generates new unused variable name for the cfg *)
	let genVar () =
		let n = !k in
		let () = k:= n + 1 in
		let ascii = 65 + n in
		if ascii < 65 || ascii > 90
		then char2symb 'A'
		else char2symb (Char.chr ascii)

	(*
	let convertPlsRules rl i1 i2 newInit =
		(* swaps the initial variables of both old cfgs for the new initial var *)
		let swapInits c = if c = i1 || c = i2 then newInit else c in

		let newBody b = List.map (fun c -> swapInits c) b in
		let newRule r = {head = swapInits r.head; body = newBody r.body} in

			Set.map (fun r -> newRule r) rl

	in
	*)
	(* create gcf rules for plus expression *)
	let convertPlsRules rl i1 i2 newInit =
		let newRule1 = {head = newInit; body = [i1]} in
		let newRule2 = {head = newInit; body = [i2]} in
			Set.add newRule1 (Set.add newRule2 rl)

	(* create gcf rules for seq expression *)
	let convertSeqRules lcfg rcfg =
		let rl1 = lcfg.rules in
		let rl2 = rcfg.rules in
		let alp1 = lcfg.alphabet in
		let rl = Set.union rl1 rl2 in
		let newBody r =
			let b = r.body in
				match b with
					| [c] when Set.belongs r rl1 && not (Set.belongs c alp1) && c <> epsilon -> b
					| [c] when Set.belongs r rl1 && Set.belongs c alp1 -> [c; rcfg.initial]
					| [epsilon] when Set.belongs r rl1 -> [epsilon; rcfg.initial]
					| b when Set.belongs r rl2 -> b
					| _ -> b
		in
		let newRule r = {head = r.head; body = newBody r} in
			Set.map (fun r -> newRule r) rl

	(* create gcf rules for star expression *)
	let convertStrRules cfg =
		let newBody b =
			match b with
				| [c] when Set.belongs c cfg.alphabet -> [c; cfg.initial]
				| _ -> b
		in
		let r0 = {head = cfg.initial; body = [epsilon]} in

		let newRule r = {head = r.head; body = newBody r.body} in
		let newRules = Set.map (fun r -> newRule r) cfg.rules in
			Set.add r0 newRules

	let rec compile re =
		match re with
			| Plus(l, r) ->
					let cl = compile l in
					let cr = compile r in
					let alp = Set.union cl.alphabet cr.alphabet in
					let init = genVar () in
					let vs = Set.add init (Set.union cl.variables cr.variables) in
					let rl = Set.union cl.rules cr.rules in
					let rl = convertPlsRules rl cl.initial cr.initial init in
						{alphabet = alp; variables = vs;
							initial = init; rules = rl}
			| Seq(l, r) ->
					let cl = compile l in
					let cr = compile r in
					let alp = Set.union cl.alphabet cr.alphabet in
					let init = cl.initial in
					let vs = Set.union cl.variables cr.variables in
					let rl = convertSeqRules cl cr in
						{alphabet = alp; variables = vs;
							initial = init; rules = rl}
			| Star(re) ->
					let cre = compile re in
					let alp = cre.alphabet in
					let init = cre.initial in
					let vs = cre.variables in
					let rl = convertStrRules cre in
						{alphabet = alp; variables = vs;
							initial = init; rules = rl}
			| Symb(c) ->
					let alp = Set.make [c] in
					let init = genVar () in
					let vars = Set.make [init] in
					let rules = Set.make [{head = init; body = [c]}] in
						{alphabet = alp; variables = vars;
							initial = init; rules = rules}
			| Empty ->
					let alp = Set.empty in
					let init = genVar () in
					let vars = Set.make [init] in
					let rules = Set.make [{head = init; body = [epsilon]}] in
						{alphabet = alp; variables = vars;
							initial = init; rules = rules}
			| Zero ->
					let alp = Set.empty in
					let init = genVar () in
					let var2 = genVar () in
					let vars = Set.make [init; var2] in
					let r1 = {head = init; body = [var2]} in
					let r2 = {head = var2; body = [init]} in
					let rules = Set.make [r1; r2] in
						{alphabet = alp; variables = vars;
								initial = init; rules = rules}

	let re2cfg (re: RegularExpression.t): ContextFreeGrammarBasic.t =
		compile re
end

(********************************************************************)
module FA2CFG =
struct
	let fa2cfg fa =
		let re = FA2RE.fa2re fa in
			RE2CFG.re2cfg re
end

(********************************************************************)
module CFG2FA = (* right-linear CFG *)
struct
	open ContextFreeGrammarBasic
	open FiniteAutomaton
	
	let toState sy = state (symb2str sy)

	let toStates ssy = Set.map toState ssy

	(* This name will always be unique in the generated automaton *)
	let accSt = state "AccSt"

	let ruleToTrans (cfg: ContextFreeGrammarBasic.t) rh rb =
		let alp = cfg.alphabet in
		let vrs = cfg.variables in
		match rb with
			| [s;v] when Set.belongs s alp && Set.belongs v vrs	-> Set.make [(toState rh, s, toState v)]
			| [v] when Set.belongs v vrs -> Set.make [(toState rh, epsilon, toState v)]
			| [s] when Set.belongs s alp -> Set.make [(toState rh, s, accSt)]
			| [e] when e = epsilon -> Set.make [(toState rh, epsilon, accSt)]
			| _ -> Set.empty

	let cfg2fa (cfg: ContextFreeGrammarBasic.t): FiniteAutomaton.t =
	{	alphabet = cfg.alphabet;
		states = Set.add accSt (toStates cfg.variables);
		initialState = toState cfg.initial;
		transitions = Set.flatMap (fun r -> ruleToTrans cfg r.head r.body) cfg.rules;
		acceptStates = Set.make [accSt]
	}
end

(********************************************************************)
module CFG2RE = (* right-linear CFG *)
struct
	let cfg2re (cfg: ContextFreeGrammarBasic.t): RegularExpression.t =
		let fa = CFG2FA.cfg2fa cfg in
			FA2RE.fa2re fa
end

(********************************************************************)
module PDA2FA =
struct
	let transitionsFa trns = Set.map ( fun (s1,_,a,s2,_) -> (s1,a,s2) ) trns
	
	let pda2fa (pda: PushdownAutomaton.t): FiniteAutomaton.t  =
	{	alphabet = pda.inputAlphabet;
		states = pda.states;
		initialState = pda.initialState;
		transitions = transitionsFa pda.transitions;
		acceptStates = pda.acceptStates
	}
end

(********************************************************************)
module FA2PDA =
struct
	let upgradeTransition (s1,symb,s2) =
			(s1,PushdownAutomaton.stackSpecialSymb,symb,s2,[PushdownAutomaton.stackSpecialSymb])
			
	let upgradeTransitions trns =
		Set.map upgradeTransition trns
		
	let fa2pda (fa: FiniteAutomaton.t): PushdownAutomaton.t =
	{	inputAlphabet = fa.alphabet;
		stackAlphabet = Set.make [PushdownAutomaton.stackSpecialSymb];
		states = fa.states;
		initialState = fa.initialState;
		initialStackSymbol = PushdownAutomaton.stackSpecialSymb;
		transitions = upgradeTransitions fa.transitions;
		acceptStates = fa.acceptStates;
		criteria = true
	}
end

(********************************************************************)
module RE2PDA =
struct
	let re2pda (re: RegularExpression.t): PushdownAutomaton.t =
		let fa = RE2FA.re2fa re in
			FA2PDA.fa2pda fa
end

(********************************************************************)
module PDA2RE =
struct
	let pda2re (pda: PushdownAutomaton.t): RegularExpression.t =
		let fa = PDA2FA.pda2fa pda in
			FA2RE.fa2re fa
end

(********************************************************************)
(*FIX BY PEDRO CARLOS 11/11 -> handle r.body = [epsilon] differently VER!*)
module CFG2PDA =
struct
	open ContextFreeGrammarBasic
	open PushdownAutomaton

	let computeState = state "q"
	
	let makeNewTransition symbToConsume topStackSymbol toPutInStack: transition =
		(computeState, topStackSymbol, symbToConsume, computeState, toPutInStack)

	let make_new_transition_for_rule r = (*PEDRO CARLOS*)
		if r.body = [epsilon] then
			makeNewTransition epsilon r.head []  (*NOVO PC*)
		else
			makeNewTransition epsilon r.head r.body

	let buildTransitions(cfg: ContextFreeGrammarBasic.t): transitions =
	  let transitionsRules = Set.map make_new_transition_for_rule cfg.rules in
	  let transitionsFinalSymb = Set.map (fun alph -> makeNewTransition alph alph []) cfg.alphabet in
	  Set.union transitionsRules transitionsFinalSymb

	let cfg2pda (cfg: ContextFreeGrammarBasic.t): PushdownAutomaton.t = 
	{	inputAlphabet = cfg.alphabet;
		stackAlphabet = Set.union cfg.alphabet cfg.variables;
		states = Set.make [computeState];
		initialState = computeState;
		initialStackSymbol = cfg.initial;
		transitions = buildTransitions cfg;
		acceptStates = Set.empty;
		criteria = false
	}
end

(********************************************************************)
module PDA2CFG = (*PEDRO CARLOS!!!*)
struct
  open PushdownAutomaton
  open ContextFreeGrammarBasic
  	
  	(* 2. Empties stack before accepting *)
	let checkEmptyStackOnAccept (pda: PushdownAutomaton.t): bool =
		Set.exists (fun (curr_state, curr_symb, input, n_state, top_stack) -> 
			Set.belongs n_state pda.acceptStates && (top_stack = [pda.initialStackSymbol] || top_stack = [])
		) pda.transitions

	(* 3. Each transition either pushes one symbol to the stack, or pops one symbol off the stack, but
	not both or none.  *)
	let check_pda_transitions (pda: PushdownAutomaton.t) : bool =
		Set.for_all (fun (_, _, _, _, stack_to_push) ->
			match stack_to_push with
			| [] -> true  (* Pop operation *)
			| [a; b] -> true  (* Push one symbol, symbol to push and previous top are the new top *)
			| _ -> false    (* Invalid: pushes multiple symbols *)
		) pda.transitions

	let newEpsilonRules states = 
		let epsilonRules = Set.map (fun st -> {head = str2symb ("<" ^ st ^ st ^ ">"); body = [epsilon]}) states in
		epsilonRules

	let newRulesCombinatory q_states =
		Set.fold_right (fun p acc ->
			Set.fold_right (fun q acc_q ->
				Set.fold_right (fun r acc_r ->
					Set.add { head = str2symb ("<"  ^ p ^ q ^ ">"); body = [str2symb ("<" ^ p ^ r ^ ">"); str2symb  ("<" ^ r ^ q ^ ">")] } acc_r
				) q_states acc_q
			) q_states acc
		) q_states Set.empty	
		
		(* 
		type transition =
			state			(* state *)
		  * symbol		(* current symbol on top of the stack *)
		  * symbol		(* consumed input symbol *)
		  * state			(* next state *)
		  * symbol list	(* new top of stack*)
		*)

	let matchingTransitions stateP stateQ stateR stateS pda =
		let transitions = pda.transitions in

		let filtered_transitions = Set.filter (fun (p, e, a, r, u) -> 
			p = stateP && r = stateR &&
			List.mem e u && List.mem a u
		) transitions in

		let filtered_transitions2 = Set.filter (fun (p, e, a, r, u) -> 
			List.length u = 0 (*&& e <> pda.initialStackSymbol check if condition e <> pda.initialStackSymbol applyes to other grammars *)
		) transitions in


		let result =
		  Set.fold_right (fun (p, e, a, r, u) acc ->
			let transitions = Set.filter (fun (s, u', b, q, v) ->
			  s = stateS && q = stateQ && e = u'
			) filtered_transitions2 in
			if not (Set.isEmpty transitions) then
			  Set.union acc (Set.add (p, e, a, r, u) transitions)
			else
			  acc
		  ) filtered_transitions Set.empty in

		let limited =
			result
			|> Set.toList
			|> (function
				| a :: b :: _ -> [a; b]
				| [a] -> [a]
				| [] -> [])
		in
		limited
		  
	let newRules3 pda q_states =
		Set.fold_right (fun p acc ->
			Set.fold_right (fun q acc_q ->
				Set.fold_right (fun r acc_r ->
					Set.fold_right (fun s acc_s ->
					let t =  matchingTransitions p q r s pda in
					if List.length t <> 2 then
						acc_s
					else
						let (r', _, a, s', u') = List.hd t in
						let (r'', _, b, s'', v') = List.hd (List.tl t) in
						Set.add { 	
							head = str2symb ("<" ^ p ^ q ^ ">"); 
							body = 
								if a <> epsilon && b <> epsilon (*must check this condition in other pdas *)
									then [b; str2symb ("<" ^ r ^ s ^ ">"); a] 
								else [str2symb ("<" ^ r ^ s ^ ">")] 
						} acc_s
					) q_states acc_r
				) q_states acc_q
			) q_states acc
		) q_states Set.empty

	let pda2cfg (pda: PushdownAutomaton.t): ContextFreeGrammarBasic.t =
		(*For the conversion to be possible pda must respect prerequisites:
			1. Single accept state
			2. Empties stack before accepting
			3. Each transition either pushes one symbol to the stack, or pops one symbol off the stack, but
			not both or none. 
		*)
		if Set.size pda.acceptStates <> 1 then
			failwith "PDA to CFG conversion: PDA must have exactly one accept state"
		else if not (checkEmptyStackOnAccept pda) then
			failwith "PDA to CFG conversion: Stack must be empty before accepting" 
		else if not (check_pda_transitions pda)  then
			failwith "PDA to CFG conversion: Each transition either pushes one symbol to the stack, or pops one symbol off the stack, but not both or none" 
		else
		let rules = Set.union (newRulesCombinatory pda.states) (Set.union (newRules3 pda pda.states)(newEpsilonRules pda.states)) in
		(* Set.iter (fun rule -> print_endline (rule2str rule)) rules; *)
		let initial = str2symb ("<" ^ state2str pda.initialState ^ state2str (Set.hd pda.acceptStates) ^ ">") in
		{
			alphabet = pda.inputAlphabet;
			variables = Set.union (Set.map (fun rule -> rule.head) rules) (Set.make [initial]);
			initial = initial;
			rules = rules
		}
end

(********************************************************************)
(* PDA2TM module implementation PEDRO CARLOS UPDATES *)
module PDA2TM =
struct
  open PushdownAutomaton
  open TuringMachineBasics

	let counter = ref 0
	let last_state = ref (str2state "q_temp_init")  
	
	let fresh_state =
		fun () ->
			let state = str2state ("q_temp_" ^ string_of_int !counter) in
			incr counter;
			last_state := state; 
			state
	
	let get_current_state () = !last_state

  let pda2tm_2tapes (pda: PushdownAutomaton.t) : TuringMachine.t =
	let new_initial_state = fresh_state () in

	let initial_stack_check = 
		if pda.initialStackSymbol = epsilon || String.length (symb2str pda.initialStackSymbol) = 0 then
			false
		else
			true
	in

	let transiton_on_push (q, stack_top, input, q', symbol_to_push, input_dir, stack_dir) =
		if input = epsilon then
			Set.fold_right (fun symbol acc ->
				let transition = (q, [symbol; stack_top], q', [symbol; symbol_to_push], [S; stack_dir]) in
				transition :: acc
			) pda.inputAlphabet []
		else
			[ (q, [input; stack_top], q', [input; symbol_to_push], [input_dir; stack_dir]) ]
	in


	(*TODO REWRITE FUNCTIONAL *)
  let convert_transition (q, stack_top, input, q', alpha) =
		let reversed_alpha = List.rev alpha in
		let n = List.length reversed_alpha in
		if n = 0 then
			let input = if input = epsilon then empty else input in
			let stack_top = if Set.belongs stack_top pda.inputAlphabet then stack_top else empty in
			[ (q, [input; stack_top], q', [input; empty], [R; L]) ]
		else
			let rec build_transitions i current_state transitions =
				if i >= n then transitions
				else
					let s = List.nth reversed_alpha i in (* symbol to push *)
					let new_top = if i = 0 then stack_top else empty in
					let is_last = i = n - 1 in		 (* is this the last symbol to push? *)
					let next_state = if is_last then q' else fresh_state () in
					let input_dir = if is_last then R else S in
					let stack_dir = if is_last then S else R in
					let transitions_on_push = transiton_on_push (current_state, new_top, input, next_state, s, input_dir, stack_dir) in
					build_transitions (i + 1) next_state (transitions_on_push @ transitions)
			in
			build_transitions 0 q []
	in

	let make_initial_state =
		if not initial_stack_check then
			[]
		else
			Set.fold_right (fun symbol acc ->
				let transition = (new_initial_state, [symbol; empty], pda.initialState, [symbol; pda.initialStackSymbol], [S; S]) in
				transition :: acc
			) pda.inputAlphabet []
	in

	let transitions =
		Set.fold_right (fun trans acc ->
			let converted = convert_transition trans in
			List.fold_left (fun acc' t -> Set.add t acc') acc converted
		) pda.transitions (Set.make make_initial_state)
    in

    (* Add accept state and transitions *)
    let accept_state = fresh_state () in
    let accept_transitions =
		if Set.isEmpty pda.acceptStates then
			Set.fold_right (fun q acc ->
				let transition = (q, [empty; empty], accept_state, [empty; empty], [S; S]) in
				Set.add transition acc
			) pda.states Set.empty
		else
			Set.fold_right (fun q acc ->
				let transition = (q, [empty; pda.initialStackSymbol	], accept_state, [empty; empty], [S; S]) in
				Set.add transition acc
			) pda.states Set.empty
    in



	let all_transitions = Set.union transitions accept_transitions in

    (* Collect all states involved *)
    let states =
      Set.fold_right (fun (q,_,q',_,_) acc ->
        Set.add q (Set.add q' acc)
      ) all_transitions (Set.add accept_state pda.states)
    in

	let init_state = 
		if initial_stack_check then
			new_initial_state
		else
			pda.initialState
	in
	let accept_states = 
		if Set.belongs pda.initialState pda.acceptStates then
			[accept_state; init_state]
		else
			[accept_state]
	in

	
	{ 
	  entryAlphabet = pda.inputAlphabet;
	  tapeAlphabet = Set.union pda.stackAlphabet (Set.add empty pda.inputAlphabet);
	  empty = empty;
	  states = states;
	  initialState = init_state;
	  transitions = all_transitions;
	  acceptStates = Set.make accept_states;
	  criteria = true;
	  lbMarkers = [];
	  _nTapes = 2;
	}

 
  let pda2tm (pda: PushdownAutomaton.t) : TuringMachine.t =
		let new_initial_state = fresh_state () in


		(*limiter between stack and input*)
		let stack_limiter = symb "$" in 
		(*marks the current input being read when checking stack (changes on epsilon input to "<" ^ (symb2str symbol) ^ "|" ^ ">" to track current symbol)*)
		let head_pointer = symb "|" in 
		(*stack end, marks the top of the stack*)
		let stack_end = symb "#" in 

		let pda_alpha = Set.union pda.stackAlphabet pda.inputAlphabet in
		let alphabet_movement = Set.union (Set.make [stack_limiter; empty]) pda_alpha in

		(* Collect all states involved in transitions*)
		let state_collector transitions =
			Set.fold_right (fun (q,_,q',_,_) acc ->
				Set.add q (Set.add q' acc)
			) transitions Set.empty
		in

		(*input to stack*)
		let go_to_stack alphabet state nextState =
			Set.fold_right (fun symbol acc ->
				let transition = (state, [symbol], nextState, [symbol], [L]) in
				transition :: acc
			) alphabet [] 
		in

		(*stack to input*)
		let return_from_stack alphabet state nextState =
			Set.fold_right (fun symbol acc ->
				let transition = (state, [symbol], nextState, [symbol], [R]) in
				transition :: acc
			) alphabet []
		in

		(*add transitions to handle initialStackSymbol*)
		let make_initial_transitions =
			let new_state = fresh_state () in
			let to_stack = go_to_stack pda.inputAlphabet new_initial_state new_state in

			let new_state2 = fresh_state () in
			let put_delimiter = (new_state, [empty], new_state2, [stack_limiter], [L]) in

			let new_state3 = fresh_state () in
			let put_initial = (new_state2, [empty], new_state3, [pda.initialStackSymbol], [L]) in

			let new_state4 = fresh_state () in
			let put_stack_end = (new_state3, [empty], new_state4, [stack_end], [R]) in

			let new_state5 = fresh_state () in
			let put_initial_2 = (new_state4, [pda.initialStackSymbol], new_state5, [pda.initialStackSymbol], [R]) in

			let start = (new_state5, [stack_limiter], pda.initialState, [stack_limiter], [R]) in

			Set.make (to_stack @ [put_delimiter; put_initial; start; put_initial_2; put_stack_end])
		in

		let extract_original_symbol modified_symbol =
			let str = symb2str modified_symbol in
			let bar_pos = String.index str '|' in
			let original_str = String.sub str 1 (bar_pos - 1) in
			str2symb original_str
		in

		let convert_transition (q, stack_top, input, q', alpha) =
			let n = List.length alpha in
			let input_empty_alphabet = Set.toList (Set.add empty pda.inputAlphabet) in
			(* let input_empty_alphabet = Set.toList pda.inputAlphabet in *)
			if n = 0 then
				let new_state = fresh_state () in

				let read_input = 
					if input = epsilon then
						List.fold_right (fun symbol acc ->
							let special_symb = str2symb ("<" ^ (symb2str symbol) ^ "|" ^ ">") in
							let transition = (q, [symbol], new_state, [special_symb], [L]) in
							transition :: acc
						) input_empty_alphabet []
					else
						[(q, [input], new_state, [head_pointer], [L])]
				in

				let to_stack = go_to_stack alphabet_movement new_state new_state in

				let new_state2 = fresh_state () in
				let read_stack_end = (new_state, [stack_end], new_state2, [stack_end], [R]) in

				let new_state3 = fresh_state () in
				let new_end = (new_state2, [stack_top], new_state3, [stack_end], [R]) in

				let return_stack = return_from_stack alphabet_movement new_state3 new_state3 in
				let move_head = 
					if input = epsilon then
						let specials = List.map (fun symbol -> str2symb ("<" ^ (symb2str symbol) ^ "|" ^ ">")) input_empty_alphabet in
						List.fold_right (fun special acc ->
							let original = extract_original_symbol special in
							let transition = (new_state3, [special], q', [original], [S]) in
							transition :: acc
						) specials []
					else
						[(new_state3, [head_pointer], q', [input], [R])]
				in

				[read_stack_end; new_end] @ to_stack @ return_stack @ read_input @ move_head
			else
				let new_state = fresh_state () in

				let read_input = 
					if input = epsilon then
						List.fold_right (fun symbol acc ->
							let special_symb = str2symb ("<" ^ (symb2str symbol) ^ "|" ^ ">") in
							let transition = (q, [symbol], new_state, [special_symb], [L]) in
							transition :: acc
						) input_empty_alphabet []
					else
						[(q, [input], new_state, [head_pointer], [L])]
				in

				let to_stack = go_to_stack alphabet_movement new_state new_state in

				let new_state2 = fresh_state () in
				let read_stack_end = (new_state, [stack_end], new_state2, [empty], [R]) in

				let new_state3 = fresh_state () in
				let empty_top = (new_state2, [stack_top], new_state3, [empty], [S]) in

				
				let reversed = stack_end :: alpha in
				let new_stack_top = 
					List.fold_right (fun symbol acc ->
						let current_state = get_current_state () in
						let write_state = fresh_state () in
						let transitions = [(current_state, [stack_end], write_state, [symbol], [L]); (current_state, [empty], write_state, [symbol], [L])] in
						transitions @ acc
					) reversed []
				in

				let current_state = get_current_state () in
				let return_stack = return_from_stack (Set.add stack_end alphabet_movement) current_state current_state in
				let move_head = 
					if input = epsilon then
						let specials = List.map (fun symbol -> str2symb ("<" ^ (symb2str symbol) ^ "|" ^ ">")) input_empty_alphabet in
						List.fold_right (fun special acc ->
							let original = extract_original_symbol special in
							let transition = (current_state, [special], q', [original], [S]) in
							transition :: acc
						) specials []
					else
						[(current_state, [head_pointer], q', [input], [R])]
				in
				
				[read_stack_end; empty_top] @ to_stack @ new_stack_top @ return_stack @ read_input @ move_head
		in 

		let transitions =
			Set.fold_right (fun trans acc ->
				let converted = convert_transition trans in
				List.fold_left (fun acc' t -> Set.add t acc') acc converted
			) pda.transitions make_initial_transitions
		in


		let accept_state = fresh_state () in

		let accept_transitions =
			if not (Set.isEmpty pda.acceptStates) then
				Set.fold_right (fun q acc ->
					let new_state = fresh_state () in

					let read_input = (q, [empty], new_state, [head_pointer], [L]) in

					let to_stack = go_to_stack alphabet_movement new_state new_state in

					let new_state2 = fresh_state () in
					let read_stack_end = (new_state, [stack_end], new_state2, [stack_end], [R]) in

					(* let new_state3 = fresh_state () in *)
					let new_end = (new_state2, [pda.initialStackSymbol], accept_state, [empty], [R]) in 
					(* let move_head = (new_state3, [head_pointer], accept_state, [empty], [R]) in *)

					let transSet = Set.make ([read_stack_end; read_input; new_end] @ to_stack) in
					Set.union transSet acc
				) pda.states transitions
			else
				let current_states = state_collector transitions in
				Set.fold_right (fun q acc ->
					let new_state = fresh_state () in

					let read_input = (q, [empty], new_state, [head_pointer], [L]) in

					let to_stack = go_to_stack (Set.add stack_limiter pda_alpha) new_state new_state in

					let new_state2 = fresh_state () in
					let read_stack_end = (new_state, [stack_end], new_state2, [stack_end], [R]) in

					(* let new_state3 = fresh_state () in *)
					let new_end = (new_state2, [stack_limiter], accept_state, [empty], [R]) in 
					(* let move_head = (new_state3, [head_pointer], accept_state, [empty], [R]) in *)

					let transSet = Set.make ([read_stack_end; read_input; new_end] @ to_stack) in

					Set.union transSet acc
				) current_states transitions
		in
			


		let all_states = state_collector accept_transitions in

		let accept_states = 
			if Set.belongs pda.initialState pda.acceptStates then
				[accept_state; new_initial_state]
			else
				[accept_state]
		in

		let getTransSymbolMs transitions =
			let trns2 = Set.map (fun (_,b,_,_,_) -> b) transitions in
			let trns4 = Set.map (fun (_,_,_,d,_) -> d) transitions in
				Set.union trns2 trns4
		in
	
		let getTransSymbols transitions =
			Set.flatten (Set.map Set.make (getTransSymbolMs transitions))
		in

		{	entryAlphabet = pda.inputAlphabet;
			tapeAlphabet = (getTransSymbols accept_transitions);
			empty = empty;
			states = Set.union all_states (Set.make accept_states);
			initialState = new_initial_state;
			transitions = accept_transitions;
			acceptStates = Set.make accept_states;
			criteria = true;
			lbMarkers = [];
			_nTapes = 1
		} 
end

(********************************************************************)
module FA2TM = (* Carolina *)
struct
	let fa2tm (fa : FiniteAutomaton.t): TuringMachine.t = 
	{	entryAlphabet = Set.remove empty fa.alphabet;  (*PEDRO CARLOS *)
		tapeAlphabet = Set.add empty fa.alphabet ;  (*PEDRO CARLOS *)
	(* {	entryAlphabet = fa.alphabet;
		tapeAlphabet = fa.alphabet; VER!!!   MUDOU PORQUE? empty não pertence a nenhum alfabeto!*)
		empty = empty;
		states = fa.states;
		initialState = fa.initialState;
		transitions = Set.map (fun (a,b,c) -> (a,[b],c,[b],[R])) fa.transitions;
		acceptStates = fa.acceptStates;
		criteria = true;
		lbMarkers = [];
		_nTapes = 1
	}
end

(********************************************************************)
module RE2TM =
struct
	let re2tm (re: RegularExpression.t): TuringMachine.t =
		let re = RE2FA.re2fa re in
			FA2TM.fa2tm re
end

(********************************************************************)
module CFG2TM = (* PEDRO CARLOS VER!!! *)
struct
	let cfg2tm (cfg: ContextFreeGrammarBasic.t): TuringMachine.t =
		let pda = CFG2PDA.cfg2pda cfg in
			PDA2TM.pda2tm pda (* uma fita *)

	let cfg2tm_2tapes (cfg: ContextFreeGrammarBasic.t): TuringMachine.t =
		let pda = CFG2PDA.cfg2pda cfg in
			PDA2TM.pda2tm_2tapes pda 
(* 	
	let cfg2tm (cfg: ContextFreeGrammarBasic.t): TuringMachine.t =
		let pda = CFG2PDA.cfg2pda cfg in
			PDA2TM.pda2tm_2tapes pda  *)
	(* let cfg2tm (cfg: ContextFreeGrammarBasic.t): TurMachMultiTypes.t =
		let pda = CFG2PDA.cfg2pda cfg in
			PDA2TM.pda2tm pda *)
end

(********************************************************************)
(*PEDRO CARLOS VER!!! *)
(********************************************************************)
  (* GRAMMAR *)
	module CFG2GR =
	struct
		open ContextFreeGrammarBasic
		open Grammar

    let cfg2gr (cfg: ContextFreeGrammarBasic.t): Grammar.t =
			let rules = 
					Set.map (fun (rule: ContextFreeGrammarBasic.rule) : Grammar.rule ->
							{head = [rule.head]; body = rule.body}) cfg.rules
			in
			{
					alphabet = cfg.alphabet;
					variables = cfg.variables;
					initial = cfg.initial;
					rules = rules;
			}
	end


	(********************************************************************)
	module RE2GR =
	struct
		let re2gr re =
			let cfg = RE2CFG.re2cfg re in
				CFG2GR.cfg2gr cfg
	end

(********************************************************************)

	(********************************************************************)
	module FA2GR =
	struct
		let fa2gr (fa: FiniteAutomaton.t): Grammar.t =
			let cfg = FA2CFG.fa2cfg fa in
				CFG2GR.cfg2gr cfg
	end

(********************************************************************)

	module GR2CFG = 
	struct
		open ContextFreeGrammarBasic
		open Grammar

	(* pre: gram is context free *)
	let gr2cfg (gram: Grammar.t): ContextFreeGrammarBasic.t =
		{
			alphabet = gram.alphabet;
			variables = gram.variables;
			initial = gram.initial;
			rules = Set.map (fun (rule: Grammar.rule) : ContextFreeGrammarBasic.rule -> 
				{head = List.hd rule.head; body = rule.body}) gram.rules;
		}
	end

	module GR2PDA = (* ??? *)
		struct
			let gr2pda (gr: Grammar.t): PushdownAutomaton.t =
				let cfg = GR2CFG.gr2cfg gr in
					CFG2PDA.cfg2pda cfg 

			(* let cfg2tm (cfg: ContextFreeGrammarBasic.t): TurMachMultiTypes.t =
				let pda = CFG2PDA.cfg2pda cfg in
					PDA2TM.pda2tm pda *)
end
	
	module GR2TM =
	struct
		 open Grammar
		 open TuringMachine

	let gr2tm (gram: Grammar.t): TuringMachine.t =
		(* Not implemented *)
				{
					entryAlphabet = Set.make [];
					tapeAlphabet = Set.make [];
					empty = empty;
					states = Set.make [];
					initialState = "q0";
					transitions = Set.make [];
					acceptStates = Set.make ["q_accept"];
					criteria = true;
					lbMarkers = [];
					_nTapes = 1;
				}

	end

	module TM2GR = (* Linz *)
	struct
		 open Grammar
		 open TuringMachine

		 	let tm2gr (tm: TuringMachine.t): Grammar.t =
				(* Check if the Turing Machine is deterministic *)
				if tm._nTapes <> 1 then
					failwith "The Turing Machine must have exactly one tape"
				else if not (TuringMachinePrivate.isDeterministic tm) then
					failwith "The Turing Machine is not deterministic"
				else
					let ini_symb = str2symb "S" in
					let t_symb = str2symb "T" in
					let aSet = Set.add tm.empty tm.entryAlphabet in
					let bSet = tm.tapeAlphabet in
					let newAB_Vars =
						Set.fold_right (fun a acc ->
							Set.fold_right (fun b acc_inner ->
								let new_var = Printf.sprintf "<V%s%s>" (symb2str a) (symb2str b) in
								Set.add (str2symb new_var) acc_inner
							) bSet acc
						) aSet Set.empty
					in
					let states = tm.states in
					let newAIB_Vars =
						Set.fold_right (fun a acc ->
							Set.fold_right (fun i acc_inner ->
								Set.fold_right (fun b acc_innermost ->
									let new_var = Printf.sprintf "<V%s%s%s>" (symb2str a) (state2str i) (symb2str b) in
									Set.add (str2symb new_var) acc_innermost
								) bSet acc_inner
							) states acc
						) aSet Set.empty
					in
			
					let firstRules = 
						let empty = tm.empty in
						let v_empty_empty = str2symb (Printf.sprintf "<V%s%s>" (symb2str empty) (symb2str empty)) in
						Set.make [
							{head = [ini_symb]; body = [v_empty_empty; ini_symb]};
							{head = [ini_symb]; body = [ini_symb; v_empty_empty]};
							{head = [ini_symb]; body = [t_symb]};
						]
					in
			
					let secondRules =
						Set.fold_right (fun a acc ->
							let new_var = str2symb (Printf.sprintf "<V%s%s>" (symb2str a) (symb2str a)) in
							let new_var_2 = str2symb (Printf.sprintf "<V%s%s%s>" (symb2str a) (state2str tm.initialState) (symb2str a)) in
							let new_rule = {head = [t_symb]; body = [t_symb; new_var]} in
							let new_rule_2 = {head = [t_symb]; body = [new_var_2]} in
							let new_rules = Set.make [new_rule; new_rule_2] in
							Set.union new_rules acc
						) tm.entryAlphabet Set.empty
					in
			
					let transition_rules =
						Set.flatMap (fun (curr_state, input, next_state, tape_symb, dir) ->
							match dir with
							| [L] -> 
								Set.fold_right (fun a acc ->
									Set.fold_right (fun p acc_inner ->
										Set.fold_right (fun q acc_innermost ->
											let v_pq = str2symb (Printf.sprintf "<V%s%s>" (symb2str p) (symb2str q)) in
											let v_aic = str2symb (Printf.sprintf "<V%s%s%s>" (symb2str a) (state2str curr_state) (symb2str (List.hd input))) in
											let v_pjq = str2symb (Printf.sprintf "<V%s%s%s>" (symb2str p) (state2str next_state) (symb2str q)) in
											let v_ad = str2symb (Printf.sprintf "<V%s%s>" (symb2str a) (symb2str (List.hd tape_symb))) in
											let new_rule = {head = [v_pq; v_aic]; body = [v_pjq; v_ad]} in
											Set.add new_rule acc_innermost
										) bSet acc_inner
									) aSet acc
								) aSet Set.empty
							| [R] -> 
								Set.fold_right (fun a acc ->
									Set.fold_right (fun p acc_inner ->
										Set.fold_right (fun q acc_innermost ->
											let v_pq = str2symb (Printf.sprintf "<V%s%s>" (symb2str p) (symb2str q)) in
											let v_aic = str2symb (Printf.sprintf "<V%s%s%s>" (symb2str a) (state2str curr_state) (symb2str (List.hd input))) in
											let v_pjq = str2symb (Printf.sprintf "<V%s%s%s>" (symb2str p) (state2str next_state) (symb2str q)) in
											let v_ad = str2symb (Printf.sprintf "<V%s%s>" (symb2str a) (symb2str (List.hd tape_symb))) in
											let new_rule = {head = [v_aic; v_pq]; body = [v_ad; v_pjq]} in
											Set.add new_rule acc_innermost
										) bSet acc_inner
									) aSet acc
								) aSet Set.empty
							| _ -> Set.empty
						) tm.transitions
					in
			
					let accept_state_rules = 
						Set.fold_right (fun a acc ->
							Set.fold_right (fun i acc_inner ->
								Set.fold_right (fun b acc_innermost ->
									let new_var = str2symb (Printf.sprintf "<V%s%s%s>" (symb2str a) (state2str i) (symb2str b)) in
									let new_rule = {head = [new_var]; body = [a]} in
									Set.add new_rule acc_innermost
								) bSet acc_inner
							) tm.acceptStates acc
						) aSet Set.empty
					in
			
					let handle_terminals_rules = 
						Set.fold_right (fun a acc ->
							Set.fold_right (fun c acc_inner ->
								Set.fold_right (fun b acc_innermost ->
									let new_var = str2symb (Printf.sprintf "<V%s%s>" (symb2str a) (symb2str b)) in
									let new_rule = {head = [c; new_var]; body = [c; a]} in
									let new_rule2 = {head = [new_var; c]; body = [a; c]} in
									let new_rules = Set.make [new_rule; new_rule2] in
									Set.union new_rules acc_innermost
								) bSet acc_inner
							) aSet acc
						) aSet Set.empty
					in
			
					let empty_epsilon_rule =
						let new_rule = {head = [tm.empty]; body = [epsilon]} in
						Set.make [new_rule]
					in
			
					let rules = 
						let first = Set.union firstRules secondRules in
						let second = Set.union transition_rules accept_state_rules in
						let third = Set.union handle_terminals_rules empty_epsilon_rule in
						Set.union first (Set.union second third)
					in
			
					let vars = 
						let symbs = Set.make [ini_symb; t_symb; tm.empty] in
						Set.union (Set.union newAB_Vars newAIB_Vars) symbs
					in
			
					{
						alphabet = tm.entryAlphabet;
						variables = vars;
						initial = ini_symb;
						rules = rules
					}


	end
	
(********************************************************************)
(*PEDRO CARLOS *)
(********************************************************************)

module PolyBasic =
struct
	let re2fa = RE2FA.re2fa
	let pda2fa = PDA2FA.pda2fa
	let cfg2fa = CFG2FA.cfg2fa
	
	let fa2re = FA2RE.fa2re
	let pda2re = PDA2RE.pda2re
	let cfg2re = CFG2RE.cfg2re

	let fa2pda = FA2PDA.fa2pda
	let re2pda = RE2PDA.re2pda
	let cfg2pda = CFG2PDA.cfg2pda

	let gr2pda = GR2PDA.gr2pda (* PEDRO CARLOS VER!!! impossivel  está expplicado....*)
	
	let fa2cfg = FA2CFG.fa2cfg
	let re2cfg = RE2CFG.re2cfg
	let re2gr = RE2GR.re2gr (* PEDRO CARLOS *)
	let pda2cfg = PDA2CFG.pda2cfg
	
	let fa2tm = FA2TM.fa2tm
	let fa2gr = FA2GR.fa2gr (* PEDRO CARLOS *)
	let re2tm = RE2TM.re2tm
	let pda2tm = PDA2TM.pda2tm
	let pda2tm_2tapes = PDA2TM.pda2tm_2tapes (* PEDRO CARLOS *)
	let cfg2tm = CFG2TM.cfg2tm
	let cfg2tm_2tapes = CFG2TM.cfg2tm_2tapes (* PEDRO CARLOS *)

	(* GRAMMAR *)
	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)
	let gr2cfg = GR2CFG.gr2cfg
	let gr2tm = GR2TM.gr2tm 
	let cfg2gr = CFG2GR.cfg2gr
	let tm2gr = TM2GR.tm2gr


	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)

end

(********************************************************************)
module PolyModel =
struct
	open PolyBasic

	let json2model (j: JSon.t): Model.model =
		let kind = JSon.fieldString j "kind" in
			(********************************************************************)
			(*PEDRO CARLOS *)
			(********************************************************************)
			if Grammar.kind = kind then
				(new Grammar.model (Arg.JSon j) :> Model.model)
			(********************************************************************)
			(*PEDRO CARLOS *)
			(********************************************************************)
			else if FiniteAutomaton.kind = kind then
				(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)
			else if RegularExpression.kind = kind then
				(new RegularExpression.model (Arg.JSon j) :> Model.model)
			else if PushdownAutomaton.kind = kind then
				(new PushdownAutomaton.model (Arg.JSon j) :> Model.model)
			else if ContextFreeGrammarBasic.kind = kind then
				(new ContextFreeGrammarBasic.model (Arg.JSon j) :> Model.model)
			else if TuringMachine.kind = kind then
				(new TuringMachine.model (Arg.JSon j) :> Model.model)
			else if FiniteEnumeration.kind = kind then
				(new FiniteEnumeration.model (Arg.JSon j) :> Model.model)
			else if Exercise.kind = kind then (
				ignore (new Exercise.exercise (Arg.JSon j));			
				(new FiniteAutomaton.model (Arg.JSon FiniteAutomaton.example) :> Model.model)
			)
			else (* need to ignore Composition.kind *)
				(new FiniteAutomaton.model (Arg.JSon FiniteAutomaton.example) :> Model.model)
				
	let text2model (text: string): Model.model = json2model (JSon.parse text)
	
	let file2model (filename: string): Model.model = json2model (JSon.fromFile filename)
	
	let example2model (name: string): Model.model = text2model (Examples.example name)

	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)
	let gr2model (g: Grammar.t): Grammar.model =
		new Grammar.model (Arg.Representation g)
	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)

	let fa2model (fa: FiniteAutomaton.t): FiniteAutomaton.model =
		new FiniteAutomaton.model (Arg.Representation fa)

	let re2model (re: RegularExpression.t): RegularExpression.model =
		new RegularExpression.model (Arg.Representation re)

	let pda2model (pda: PushdownAutomaton.t): PushdownAutomaton.model =
		new PushdownAutomaton.model (Arg.Representation pda)

	let cfg2model (cfg: ContextFreeGrammarBasic.t): ContextFreeGrammarBasic.model =
		new ContextFreeGrammarBasic.model (Arg.Representation cfg)

	let tm2model (tm: TuringMachine.t): TuringMachine.model =
		new TuringMachine.model (Arg.Representation tm)

	(* let tmMulti2model (tm: TurMachMultiTypes.t): TurMachMultiTypes.model =
		new TurMachMultiTypes.model (Arg.Representation tm) *)

	(* GRAMMAR *)
	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)
	let model2gr (model: Model.model): Grammar.t =
		if model#isGrammar then Grammar.make (Arg.JSon (model#toJSon))
		else Error.fatal "model2gr"
	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)

	let model2fa (model: Model.model): FiniteAutomaton.t =
		if model#isFiniteAutomaton then FiniteAutomaton.make (Arg.JSon (model#toJSon))
		else Error.fatal "model2fa"
		
	let model2re (model: Model.model): RegularExpression.t =
		if model#isRegularExpression then RegularExpression.make (Arg.JSon (model#toJSon))
		else Error.fatal "model2re"
		
	let model2cfg (model: Model.model): ContextFreeGrammarBasic.t =
		if model#isContextFreeGrammar then ContextFreeGrammarBasic.make (Arg.JSon (model#toJSon))
		else Error.fatal "model2cfg"
		
	let model2pda (model: Model.model): PushdownAutomaton.t =
		if model#isPushdownAutomaton then PushdownAutomaton.make (Arg.JSon (model#toJSon))
		else Error.fatal "model2pda"
		
	let model2tm (model: Model.model): TuringMachine.t =
		if model#isTuringMachine then TuringMachine.make (Arg.JSon (model#toJSon))
		else Error.fatal "model2tm"

	(* Carolina *)
	let model2comp (model: Model.model): CompositionSupport.t =
		if model#isFiniteAutomaton then FA (model2fa model)
		else if model#isRegularExpression then RE (model2re model)
		else if model#isPushdownAutomaton then PDA (model2pda model)
		else if model#isContextFreeGrammar then CFG (model2cfg model)
		else if model#isTuringMachine then TM (model2tm model)
		(********************************************************************)
		(*PEDRO CARLOS *)
		(********************************************************************)
		else if model#isGrammar then GR (model2gr model)
		(********************************************************************)
		(*PEDRO CARLOS *)
		(********************************************************************)
		else if model#isComposition then 
			!CompositionSupport.makeCompositionRef (Arg.JSon (model#toJSon))
		else Error.fatal "model2comp"

	let re2fa m = fa2model (re2fa m#representation)
	let pda2fa m = fa2model (pda2fa m#representation)
	let cfg2fa m = fa2model (cfg2fa m#representation)

	let fa2re m = re2model (fa2re m#representation)
	let pda2re m = re2model (pda2re m#representation)
	let cfg2re m = re2model (cfg2re m#representation)

	let fa2pda m = pda2model (fa2pda m#representation)
	let re2pda m = pda2model (re2pda m#representation)
	let cfg2pda m = pda2model (cfg2pda m#representation)

	let fa2cfg m = cfg2model (fa2cfg m#representation)
	let re2cfg m = cfg2model (re2cfg m#representation)
	let re2gr m = gr2model (re2gr m#representation) (*PEDRO CARLOS *)
	let pda2cfg m = cfg2model (pda2cfg m#representation)
	
	let fa2tm m = tm2model (fa2tm m#representation)
	let re2tm m = tm2model (re2tm m#representation)

	let cfg2tm m = tm2model (cfg2tm m#representation)
	let cfg2tm_2tapes m = tm2model (cfg2tm_2tapes m#representation) (*PEDRO CARLOS *)

	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)
	let pda2tm m = tm2model (pda2tm m#representation)
	let pda2tm_2tapes m = tm2model (pda2tm_2tapes m#representation)

	let fa2gr m = gr2model (fa2gr m#representation)
	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)	

	let gr2pda m = pda2model (gr2pda m#representation)
	(* let pda2tmMulti m = tmMulti2model (pda2tm m#representation)	 *)

	let gr2cfg m = cfg2model (gr2cfg m#representation)
	let gr2tm m = tm2model (gr2tm m#representation) 
	let tm2gr m = gr2model (tm2gr m#representation)
	let cfg2gr m = gr2model (cfg2gr m#representation)

	(********************************************************************)
	(*PEDRO CARLOS *)
	(********************************************************************)
end

module PolyCheckExamples: sig end =
struct
	open Examples
	open PolyModel

	let checkExamples = (* run immediately *)
		List.iter
			(fun name -> ignore (text2model (example name))) examples
end

(********************************************************************)
module PolyBasicTests: sig end = (*PEDRO CARLOS VER!!! *)
struct
	open PolyBasic

	let active = false

	let cfg_g = {| 		{
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


	let cfg = {| 		{
		kind : "context free grammar",
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

	let cfg2 = {| 		{
		kind : "context free grammar",
		description : "this is an example",
		name : "cfg_simple",
		alphabet : ["a", "b"],
		variables : ["S"],
		initial : "S",
		rules : [	
		"S -> aSb",
		"S -> ab" ]
	}|}
 (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
	let pda_AABB = {| {
		kind: "pushdown automaton",
		description: "this is an example",
		name: "pda_AABB",
		inputAlphabet: ["a", "b"],
		stackAlphabet: ["a", "z"],
		states: ["S1", "S2", "S3"],
		initialState: "S1",
		initialStackSymbol: "z",
		transitions: [
			["S1", "z", "a", "S1", "az"],   
			["S1", "a", "a", "S1", "aa"],   
			["S1", "a", "b", "S2", ""],           
			["S2", "a", "b", "S2", ""],          
			["S2", "z", "~", "S3", ""]
		],
		acceptStates: ["S3"],
		criteria: "true"
	} |}


	let pda_AABB2 = {| {
		kind: "pushdown automaton",
		description: "this is an example",
		name: "pda_AABB",
		inputAlphabet: ["a", "b"],
		stackAlphabet: ["a", "S", "b"],
		states: ["q"],
		initialState: "q",
		initialStackSymbol: "z",
		transitions: [
			["q", "S", "~", "q", "bSa"],   
			["q", "S", "~", "q", "ab"],   
			["q", "a", "a", "q", ""],           
			["q", "b", "b", "q", ""],          
		],
		acceptStates: ["S3"],
		criteria: "true"
	} |}




let tm_astar3 = {| {
	kind: "turing machine",
	description: "Accepts words of the form a^nb^n (equal number of a's followed by b's)",
	name: "tm_astar3",
	entryAlphabet: ["a", "b"],
	tapeAlphabet: ["a", "b", "z", "B"],
	empty: "B",
	states: ["S1","S1_push1","S1_push2", "S2", "S3"],
	initialState: "S1",
	transitions: [
    ["S1", "z", "S1_push1", "z", "R"],
    ["S1_push1", "B", "S1", "a", "L"],
    ["S1", "a", "S1_push2", "a", "R"],
    ["S1_push2", "B", "S1", "a", "L"],
    ["S1", "b", "S2", "B", "L"],
    ["S2", "b", "S2", "B", "L"],
    ["S2", "z", "S3", "B", "L"],
		["S1", "a", "S1", "z", "R"],
		["S1", "a", "S1", "a", "R"]
],
	acceptStates: ["S3"],
	criteria: "true",
	markers: []
} |} 
 (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
	let fa_toRe = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "fa_toRe",
		alphabet : ["a","b"],
		states : ["1", "2"],
		initialState : "1",
		transitions : [
				["1","a","2"],["2","b","2"]
			],
		acceptStates : ["2"]
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

	 (*
	 Terceiro exemplo dos slides do stor - Aceita a palavra (a + b)*aa(a + b)*
	 Este exemplo e:
		 - determinista
		 - nao entra em loop de configuracao
		 - nao corre infinitamente sem repetir configuracao
		 - nao tem estados useless
		 - termina por estados de aceitacao
 *)
 let tm_astar3 = {| {
		 kind: "turing machine",
		 description: "this is an example changed",
		 name: "tm_astar3",
		 entryAlphabet: ["a", "b"],
		 tapeAlphabet: ["a", "b","B"],
		 empty: "B",
		 states: ["q1", "q2", "q3"],
		 initialState: "q1",
		 transitions: [
			 ["q1", "a", "q2", "a", "R"],
			 ["q1", "b", "q1", "b", "R"],
			 ["q2", "a", "q3", "a", "R"],
			 ["q2", "b", "q1", "b", "R"]
		 ],
		 acceptStates: ["q3"],
		 criteria: "true",
		 markers: []
		 } |}


		 let tm_astar4 = {| {
			kind: "turing machine",
			description: "Turing machine for language a^n b^n c^n",
			name: "tm_astar4",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q0","q1", "q2", "q3", "q4", "qf"],
			initialState: "q0",
			transitions: [
				["q0", "a", "q1", "X", "R"],
				["q0", "Y", "q4", "Y", "R"],
		
				["q1", "a", "q1", "a", "R"],
				["q1", "Y", "q1", "Y", "R"],
				["q1", "b", "q2", "Y", "R"],
		
				["q2", "b", "q2", "b", "R"],
				["q2", "Z", "q2", "Z", "R"],
				["q2", "c", "q3", "Z", "L"],
		
				["q3", "a", "q3", "a", "L"],
				["q3", "Y", "q3", "Y", "L"],
				["q3", "b", "q3", "b", "L"],
				["q3", "Z", "q3", "Z", "L"],
				["q3", "X", "q0", "X", "R"],
		
				["q4", "Y", "q4", "Y", "R"],
				["q4", "Z", "q4", "Z", "R"],
				["q4", "B", "qf", "B", "L"]
			],
			acceptStates: ["qf"],
			criteria: "true",
			markers: []
		} |}

	let clean = {| {
		kind : "context free grammar", 
		description : "Clean example from https://www.cs.scranton.edu/~mccloske/courses/cmps260/cfg_remove_useless.html",
		name : "Clean1",
		alphabet : ["a", "b", "c", "d"],
		variables : ["S", "A", "B", "C", "D"],
		initial : "S",
		rules : ["S -> aSa | bB | bAA", "A -> a | SbA | aB", "B -> AB | CaB", "C -> cC | Sa | bD", "D -> dD | ~"]
	} |}

	let cleang = {| {
		kind : "grammar", 
		description : "Clean example from https://www.cs.scranton.edu/~mccloske/courses/cmps260/cfg_remove_useless.html",
		name : "Clean1",
		alphabet : ["a", "b", "c", "d"],
		variables : ["S", "A", "B", "C", "D"],
		initial : "S",
		rules : ["S -> aSa | bB | bAA", "A -> a | SbA | aB", "B -> AB | CaB", "C -> cC | Sa | bD", "D -> dD | ~"]
	} |}


	let pda_WW_1 = {| {
		kind: "pushdown automaton",
		description : "this is an example",
		name : "pda_WW-1",
		inputAlphabet : ["a","b"],
		stackAlphabet: ["z","a","b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		initialStackSymbol: "z",
		transitions : [
				["S1","z","a","S2","az"], 
				["S1","z","b","S2","bz"],
				["S2","a","a","S2","aa"],
				["S2","a","a","S3",""],
				["S2","a","b","S2","ba"],
				["S2","b","a","S2","ab"],
				["S2","b","b","S2","bb"],
				["S2","b","b","S3",""],
				["S3","a","a","S3",""],
				["S3","b","b","S3",""],
				["S3","z","~","S4","z"]
			],
		acceptStates : ["S1","S4"],
		criteria: "true"
	} |}

	(* == Finite Automaton (FA) Tests == *)
	
	let testSimplify () = (* FA -> RE simplify *)
		let fa = FiniteAutomaton.make (Arg.Text fa_toRe) in
		let re = fa2re fa in
			RegularExpression.show re;
			let rs =  RegularExpression.simplify re in
				RegularExpression.show rs

	let testFAToRe () = (* FA -> RE *)
		let fa = FiniteAutomaton.make (Arg.Text fa_toRe) in
		let re = fa2re fa in
			RegularExpression.show re

	(* == Regular Expression (RE) Tests == *)
	
	let testREToFA () = (* RE -> FA *)
		let re = RegularExpression.make (Arg.Predef "re_abc") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testREToFA2 () = (* RE -> FA *)
		let re = RegularExpression.make (Arg.Predef "re_simple") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testREToFA3 () = (* RE -> FA *)
		let re = RegularExpression.make (Arg.Predef "re_complex") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testREToFA4 () = (* RE -> FA *)
		let re = RegularExpression.make (Arg.Predef "re_convoluted") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testREToCFG () = (* RE -> CFG *)
		let re = RegularExpression.make (Arg.Predef "re_simple") in
		let cfg = re2cfg re in
			ContextFreeGrammarBasic.show cfg

	(* == Context-Free Grammar (CFG) Tests == *)
	
	let testCFGToFA () = (* CFG -> FA *)
		let cfg = ContextFreeGrammarBasic.make (Arg.Predef "cfg_abc") in
		let fa = cfg2fa cfg in
			FiniteAutomaton.show fa

	let testCFGToRe () = (* CFG -> RE *)
		let cfg = ContextFreeGrammarBasic.make (Arg.Predef "cfg_abc") in
		let re = cfg2re cfg in
			RegularExpression.show re

	(* == Grammar (GR) / CFG Inter-conversion Tests == *)
	
	let testGrToCfg () = (* GR -> CFG *)
		let g_cfg = Grammar.make (Arg.Text cfg) in
		let cfg_res = gr2cfg g_cfg in (*two must be equal*)
			ContextFreeGrammarBasic.show cfg_res;
			Grammar.show g_cfg;
		print_endline "-------------------";
		let g_csg = Grammar.make (Arg.Text csg) in
		let cfgFromCsg = gr2cfg g_csg in (*FAIL show returns two different grammars, supposed to fail*)
			ContextFreeGrammarBasic.show cfgFromCsg;
			Grammar.show g_csg

	let testCFGToGR () = (* CFG -> GR *)
		let g_cfg = ContextFreeGrammarBasic.make (Arg.Text clean) in
		let gr_res = cfg2gr g_cfg in (*two must be equal*)
		Grammar.show gr_res;
		ContextFreeGrammarBasic.show g_cfg

	let testCleanCfgToGr () = (* CFG -> GR (with cleaning) *)
		let cfg = ContextFreeGrammarBasic.make (Arg.Text clean) in
		let g_cfg = Grammar.make (Arg.Text cleang) in
		let cleanedCFG = ContextFreeGrammarLL1.clean cfg in
		List.iter (fun t -> ignore (ContextFreeGrammarLL1.transformationToString t)) cleanedCFG;
		let cleanedGCFG = Grammar.clean g_cfg in
		Grammar.show cleanedGCFG

	(* == Pushdown Automaton (PDA) / CFG/GR Inter-conversion Tests == *)
	
	let testPDA2CFG () = (* PDA -> CFG -> GR -> Clean GR *)
		print_endline "testPDA2CFG";
		let pda = PushdownAutomaton.make (Arg.Text pda_AABB) in
		print_endline "Original PDA";
		PushdownAutomaton.show pda;
		print_endline "";
		let cfg = pda2cfg pda in
		print_endline "Converted CFG";
		ContextFreeGrammarBasic.show cfg;
		print_endline ""; 
		let g_cfg = cfg2gr cfg in (*Convert to grammar to test with clean*)
		print_endline "Converted CFG Grammar";
		Grammar.show g_cfg;
		print_endline ""; 
		let cleaned_g_cfg = Grammar.clean g_cfg in
		print_endline "Cleaned gr converted";
		Grammar.show cleaned_g_cfg;
		assert (Grammar.accept cleaned_g_cfg (word "ab"));
		assert (Grammar.accept cleaned_g_cfg (word "aabb"));
		assert (Grammar.accept cleaned_g_cfg (word "aaabbb"));
		print_endline "testPDA2CFG done"

	let testGRToPDA () = (* GR -> CFG -> PDA *)
		let gr_cfg = Grammar.make (Arg.Text cfg_g) in
		let cfg_cfg = ContextFreeGrammarBasic.make (Arg.Text cfg) in
		(*Grammars*)
		let cfg_converted = gr2cfg gr_cfg in
		print_endline "Converted CFG";
		ContextFreeGrammarBasic.show cfg_converted;
		print_endline "Original CFG";
		ContextFreeGrammarBasic.show cfg_cfg;
		(*PDAs*)
		let pda_gr = cfg2pda cfg_converted in
		let pda_cfg = cfg2pda cfg_cfg in
		print_endline "PDA_CFG";
		PushdownAutomaton.show pda_cfg;
		print_endline "PDA";
		PushdownAutomaton.show pda_gr;
		let wordsToAccept = [""; "01"; "10"; "0011"; "1010"; "1100"; "000111"; "100110"; "110100"; "111000"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda_gr (word w) in
				Printf.printf "Word %s accepted: %b\\n" w result
		) wordsToAccept

	(* == Pushdown Automaton (PDA) / Turing Machine (TM) Tests == *)
	
	let testPDA2TM () = (* PDA -> TM (1 tape) *)
		let pda = PushdownAutomaton.make (Arg.Text pda_AABB) in  
		let pda2 = PushdownAutomaton.make (Arg.Text pda_WW_1) in 

		print_endline "Original PDA 1 (AABB)";
		(* PushdownAutomaton.show pda; *)
		let wordsToAccept1 = ["";"a"; "b"; "aab"; "abb"; "aaaaaaabbbb"; "bbbbbaaaaa"  ; "aa" ;"aaaa"; "ab"; "aaaaabbbbb"; "aabb"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda (word w) in
				Printf.printf "Word '%s' accepted by PDA1: %b\\n" w result
		) wordsToAccept1; 
		
		let tm = pda2tm pda in
		TuringMachinePrivate.validate "pda2tm" tm;
		print_endline "Converted TM 1";
(* 		TuringMachine.show tm;
 *)		List.iter (fun w ->
				let result = TuringMachine.accept tm (word w) in
				Printf.printf "Word '%s' accepted by TM1: %b\\n" w result
		) wordsToAccept1;  
		print_endline "";
		
		print_endline "Original PDA 2 (WW_1)";
		(* PushdownAutomaton.show pda2; *)
		let wordsToAccept2 = [""; "a"; "b"; "aa"; "bb"; "aba"; "bab"; "abba"; "baab"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda2 (word w) in
				Printf.printf "Word '%s' accepted by PDA2: %b\\n" w result
		) wordsToAccept2; 
		print_endline "";
		
		let tm2 = pda2tm pda2 in
		print_endline "Converted TM 2";
		(* TuringMachine.show tm2;  *)
		List.iter (fun w ->
				let result = TuringMachine.accept tm2 (word w) in
				Printf.printf "Word '%s' accepted by TM2: %b\\n" w result
		) wordsToAccept2; 
		print_endline ""

	let testPDA2TM_MULTI () = (* PDA -> TM (2 tapes) *)
		let pda = PushdownAutomaton.make (Arg.Text pda_AABB) in  
		let pda2 = PushdownAutomaton.make (Arg.Text pda_WW_1) in 
		print_endline "Original PDA 1 (AABB)";
		PushdownAutomaton.show pda;
		print_endline "";
		let wordsToAccept1 = ["aabb"; "ab"; "a"; "b"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda (word w) in
				Printf.printf "Word '%s' accepted by PDA1: %b\\n" w result
		) wordsToAccept1; 
		
		let tm = pda2tm_2tapes pda in
		print_endline "Converted TM 1 (2 tapes)";
		TuringMachine.show tm;
		print_endline "";
		let wordsToAccept1_tm = ["aabb"; "ab"; "b"; "aaabbb"; "abbb"] in
		List.iter (fun w ->
				let result = TuringMachine.accept tm (word w) in
				Printf.printf "Word '%s' accepted by TM1: %b\\n" w result
		) wordsToAccept1_tm; 
		print_endline "";
		
		print_endline "Original PDA 2 (WW_1)";
		PushdownAutomaton.show pda2;
		print_endline "";
		let wordsToAccept2 = [""; "aa"; "bb"; "abba"; "aabb"; "aba"; "b"; "aaabbb"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda2 (word w) in
				Printf.printf "Word '%s' accepted by PDA2: %b\\n" w result
		) wordsToAccept2; 
		print_endline "";
		
		let tm2 = pda2tm_2tapes pda2 in
		print_endline "Converted TM 2 (2 tapes)";
		TuringMachine.show tm2;
		List.iter (fun w ->
				let result = TuringMachine.accept tm2 (word w) in
				Printf.printf "Word '%s' accepted by TM2: %b\\n" w result
		) wordsToAccept2; 
		print_endline ""

	let testCFGToTM () = (* CFG -> PDA -> TM (1 tape) *)
		let g_cfg = ContextFreeGrammarBasic.make(Arg.Text cfg2) in
		print_endline "CFG (ab)";
		ContextFreeGrammarBasic.show g_cfg;
		let pda = cfg2pda g_cfg in 
		print_endline "Intermediate PDA";
		PushdownAutomaton.show pda;
		let wordsToAccept = ["a"; "b"; "aab"; "abb"; "aaaaaaabbbb"; "bbbbbaaaaa"  ; "aa" ;"aaaa"; "ab"; "aaaaabbbbb"; "aabb"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda (word w) in
				Printf.printf "Word '%s' accepted by PDA: %b\\n" w result
		) wordsToAccept;
		
		let tm = pda2tm pda in
		print_endline "Converted TM (1 tape)";
		TuringMachine.show tm;
		List.iter (fun w ->
			let result = TuringMachine.accept tm (word w) in
			Printf.printf "Word '%s' accepted by TM: %b\\n" w result
		) wordsToAccept

	let testCFGToTM_MULTI () = (* CFG -> PDA -> TM (2 tapes) *)
		let g_cfg = ContextFreeGrammarBasic.make(Arg.Text cfg2) in
		print_endline "CFG (ab)";
		ContextFreeGrammarBasic.show g_cfg;
		let pda = cfg2pda g_cfg in 
		print_endline "Intermediate PDA";
		PushdownAutomaton.show pda; 
		let wordsToAccept = ["a"; "b"; "aab"; "abb"; "aaaaaaabbbb"; "bbbbbaaaaa"  ; "aa" ;"aaaa"; "ab"; "aaaaabbbbb"; "aabb"] in
		List.iter (fun w ->
				let result = PushdownAutomaton.accept pda (word w) in
				Printf.printf "Word '%s' accepted by PDA: %b\\n" w result
		) wordsToAccept;
		
		let tm = pda2tm_2tapes pda in
		print_endline "Converted TM (2 tapes)";
		TuringMachine.show tm;
		List.iter (fun w ->
				let result = TuringMachine.accept tm (word w) in
				Printf.printf "Word '%s' accepted by TM: %b\\n" w result
		) wordsToAccept

	(* == Turing Machine (TM) / Grammar (GR) Tests == *)
	
	(* 
	let testGrToTM () =
		let g_cfg = Grammar.make (Arg.Text cfg_g) in
		let tm = gr2tm g_cfg in
		TuringMachine.show tm;
		let wordsToAccept = [""; "01"; "10"; "0011"; "1010"; "1100"; "000111"; "100110"; "110100"; "111000"] in
		List.iter (fun w ->
				let result = TuringMachine.accept tm (word w) in
				Printf.printf "Word %s accepted: %b\\n" w result
		) wordsToAccept
	*)
	
	(* let testTmToGr () =
		let tm = TuringMachine.make (Arg.Text tm_astar4) in
		let wordsToAccept = ["abc"; "aabbcc"] in
		List.iter (fun w ->
				let result = TuringMachine.accept tm (word w) in
				Printf.printf "Word %s accepted by tm: %b\\n" w result
		) wordsToAccept;
		let gr = tm2gr tm in
		Grammar.show gr; 
		print_endline "";
		let cleaned_gr = Grammar.clean gr in
		print_endline "Cleaned Grammar";
		Grammar.show cleaned_gr; 
		let wordsToAccept = ["abc"] in
		List.iter (fun w ->
				let result = Grammar.accept gr (str2word w) in
				Printf.printf "Word %s accepted: %b\\n" w result
		) wordsToAccept  *)


	(* == Test Runner == *)
	
	let runAll =
		if Util.testing active "PolyModel" then begin
			(* FA Tests *)
(* 			Util.sep(); testSimplify ();
			Util.sep(); testFAToRe ();
			(* RE Tests *)
			Util.sep(); testREToFA ();
			Util.sep(); testREToFA2 ();
			Util.sep(); testREToFA3 ();
			Util.sep(); testREToFA4 ();
			Util.sep(); testREToCFG ();
			(* CFG Tests *)
			Util.sep(); testCFGToFA ();
			Util.sep(); testCFGToRe ();
			(* GR/CFG Tests *)
			Util.sep (); testGrToCfg (); 
			Util.sep (); testCFGToGR ();
			Util.sep (); testCleanCfgToGr (); *)
			(* PDA/CFG/GR Tests *)
(* 			Util.sep (); testPDA2CFG ();  *)
(* 			Util.sep (); testGRToPDA ();   *)
			(* PDA/TM Tests *)
		Util.sep (); testPDA2TM (); 
(* 			Util.sep (); testPDA2TM_MULTI (); 
			Util.sep (); testCFGToTM (); 
			Util.sep (); testCFGToTM_MULTI ();  *)
			(* TM/GR Tests (Commented out) *)
			(* Util.sep (); testGrToTM (); *)    
			(* Util.sep (); testTmToGr (); *)    
		end
end

(* OLD original, mudado pelo Pedro Carlos VER!!!
(********************************************************************)
module PolyBasicTests: sig end =
struct
	open PolyBasic

	let active = false

	let testToFA () =
		let re = RegularExpression.make (Arg.Predef "re_abc") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testToFA2 () =
		let re = RegularExpression.make (Arg.Predef "re_simple") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testToFA3 () =
		let re = RegularExpression.make (Arg.Predef "re_complex") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let testToFA4 () =
		let re = RegularExpression.make (Arg.Predef "re_convoluted") in
		let fa = re2fa re in
			JSon.show (FiniteAutomaton.toJSon fa)

	let fa_toRe = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "fa_toRe",
		alphabet : ["a","b"],
		states : ["1", "2"],
		initialState : "1",
		transitions : [
				["1","a","2"],["2","b","2"]
			],
		acceptStates : ["2"]
	} |}

	let testSimplify () =
		let fa = FiniteAutomaton.make (Arg.Text fa_toRe) in
		let re = fa2re fa in
			RegularExpression.show re;
			let rs =  RegularExpression.simplify re in
				RegularExpression.show rs

	let testToRe () =
		let fa = FiniteAutomaton.make (Arg.Text fa_toRe) in
		let re = fa2re fa in
			RegularExpression.show re

	let testToGrammar () =
		let re = RegularExpression.make (Arg.Predef "re_simple") in
		let cfg = re2cfg re in
			ContextFreeGrammarBasic.show cfg

	let testToAutomaton () =
		let cfg = ContextFreeGrammarBasic.make (Arg.Predef "cfg_abc") in
		let fa = cfg2fa cfg in
			FiniteAutomaton.show fa

	let testToRe () =
		let cfg = ContextFreeGrammarBasic.make (Arg.Predef "cfg_abc") in
		let re = cfg2re cfg in
			RegularExpression.show re

	let runAll =
		if Util.testing active "PolyModel" then begin
			testSimplify ()
		end
end
*)




(* OLD original, mudado pelo Pedro Carlos VER!!!
(********************************************************************)
module PDA2TM =
struct
	let generateTransitionsToPD st alphEntr alphPD: TuringMachine.transitions =
		let allAlph = Set.add dollar (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,[symb],st,[symb],[R])) allAlph 

	let generateTransitionsFromPD st alphEntr alphPD: TuringMachine.transitions =
		let allAlph = Set.add dollar (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,[symb],st,[symb],[L])) allAlph 

	let insertSymbolsPD alphEntr (pda: PushdownAutomaton.t): states * TuringMachine.transitions =
		let alphPD = pda.stackAlphabet in
		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		let newSts = Set.add st1 ( Set.add st2 ( Set.add st3 Set.empty)) in
		let newTrs1 = Set.union (generateTransitionsToPD st1 alphEntr alphPD) (generateTransitionsFromPD st3 alphEntr alphPD) in
		let newTrs2 = Set.add (st1,[empty],st2,[symb "$"],[R]) (Set.add (st2,[empty],st3,[pda.initialStackSymbol],[R]) ( Set.add (st3,[empty],pda.initialState,[empty],[R])  newTrs1 )) in
			(Set.union pda.states newSts) , newTrs2

	let rec fillStackTransition lastSt prevSt (trs: TuringMachine.transitions) wordL: TuringMachine.transitions =
		match wordL with
		| [] -> trs
		| x::y ->	let newState = if (Set.isEmpty (Set.make y)) then lastSt else IdGenerator.gen("q") in
							let dir = if (Set.isEmpty (Set.make y)) then L else R in
								fillStackTransition lastSt newState (Set.add (prevSt,[empty], newState, [x], [dir]) trs) y 

	let convertNormalTransition (tr: PushdownAutomaton.transition) alphEntr alphPD: states * TuringMachine.transitions =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = tr in
		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		let ftrs = (startState,[readSymbol],st1,[empty],[R]) in
		let trsTPD = Set.add ftrs (generateTransitionsToPD st1 alphEntr alphPD) in
		let trsRTOP = Set.add (st1,[empty],st2,[empty],[L]) trsTPD in
		let firstDirection = if ((List.length writeSymbolL) = 1) then L else R in
		let lastSt = if ((List.length writeSymbolL) = 1) then st3 else state (IdGenerator.gen("q")) in
		let replaceTop = Set.add (st2,[unstackedSymbol],st3, [List.hd writeSymbolL], [firstDirection]) trsRTOP in
		let additionalSymbolTrs = Set.union replaceTop (fillStackTransition lastSt st3 Set.empty (List.tl writeSymbolL)) in
		let trsFPD = Set.union additionalSymbolTrs (generateTransitionsFromPD lastSt alphEntr alphPD) in
		let trsLast = Set.add (lastSt,[empty],nextState,[empty],[R]) trsFPD in
			Set.add lastSt (Set.add st3 (Set.add st2 (Set.add st1 Set.empty))), trsLast

	let convertAcceptTransition (tr: PushdownAutomaton.transition) alphEntr alphPD initialStackSymb: states * TuringMachine.transitions =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = tr in
		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		let ftrs = Set.add (startState,[dollar],st1,[dollar],[R]) Set.empty in
		let checkInitSS = Set.add (st1,[initialStackSymb],st2,[empty],[R]) ftrs in
		let lastCheck = Set.add (st2,[empty],st3,[empty],[R]) checkInitSS in
			Set.add st3 (Set.add st2 (Set.add st1 Set.empty)), lastCheck

	let convertTransitionX (tr: PushdownAutomaton.transition) alphEntr alphPD initialStackSymb: states * TuringMachine.transitions = 
		let (_,_,readSymbol,_,_) = tr in
			if readSymbol == draftVar then convertAcceptTransition tr alphEntr alphPD initialStackSymb
			else convertNormalTransition tr alphEntr alphPD

	let rec convertTransitions newSts newTrs alphEntr (pda: PushdownAutomaton.t) (trs: PushdownAutomaton.transitions): states * TuringMachine.transitions = 
		let alphPD = pda.stackAlphabet in
		let initialStackSymb = pda.initialStackSymbol in
		if (Set.size trs) = 0 then newSts, newTrs
		else 
			let (nSts,nTrs) = convertTransitionX (Set.hd trs) alphEntr alphPD initialStackSymb in
				convertTransitions (Set.union nSts newSts) (Set.union nTrs newTrs) alphEntr pda (Set.tl trs)

	(*Se parar por pilha vazia 'e ncess'ario criar um estado final*)
	let getFinalStates trs: states =
		Set.map (fun (_,_,_,d,_) -> d) (Set.filter (fun (_,_,c,_,_) -> c = dollar) trs)

	let pda2tm (pda: PushdownAutomaton.t): TuringMachine.t =
		let pdaAlphabet = Set.remove draftVar pda.inputAlphabet in
		let (initialStates, initialTransitions) = insertSymbolsPD pdaAlphabet pda in
		let (convertedTransitionStates,convertedTransitions) =
			convertTransitions Set.empty Set.empty pdaAlphabet pda pda.transitions in
		let allAlphabet = Set.add dollar ( Set.union pdaAlphabet pda.stackAlphabet) in
		let allStates = Set.union initialStates convertedTransitionStates in
		let allTransitions = Set.union initialTransitions convertedTransitions in
		let allAcceptStates = Set.union pda.acceptStates (getFinalStates pda.transitions) in
			{ TuringMachine.tm_zero with
				entryAlphabet = pda.inputAlphabet;
				tapeAlphabet = allAlphabet;
				states = allStates;
				initialState = state "q00";
				transitions = allTransitions;
				acceptStates = allAcceptStates;
				criteria = true }
end
*)
