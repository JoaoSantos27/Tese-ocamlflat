(*
 * Grammar.ml
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

(*
 * ChangeLog:
 *
 * jul/2024 (amd) - New file.
 *)

(*
 * Description: General grammar functionality.
 *)

open BasicTypes



module GrammarPrivate =
struct
	open GrammarSupport

	let validate (name: string) (gram: t): unit =
		(* Util.show (symb2str gram.initial);
		Set.iter (fun v -> Util.show (symb2str v)) gram.variables;
		if Set.size gram.variables = 0 then
			Util.show "No variables"
		else
			Util.show "Variables"; *)
		(* The alphabet must not contain epsilon ('~') *)
		let isValidAlphabet = not (Set.belongs epsilon gram.alphabet) in
		(* The variables must not contain epsilon ('~') *)
		let isValidVariables = not (Set.belongs epsilon gram.variables) in
		(* The alphabet and the variables must not intersect *)
		let isIntersectionValid = (Set.inter gram.variables gram.alphabet) = Set.empty in
		(* The initial symbol must be a variable *)
		let isInitialValid = Set.belongs gram.initial gram.variables in

		(* heads should not be empty, and must be a subset of valid symbols (alphabet + variables) *)
		let areRuleHeadsValid =
			let allHeadSymbols = Set.flatMap (fun r -> Set.make r.head) gram.rules in (* set of all heads in rules, heads *)
			let allValidSymbs = Set.union gram.alphabet gram.variables in
			Set.subset allHeadSymbols allValidSymbs && 		(* heads must be a subset of valid symbols (alphabet + variables) *)
			Set.for_all (fun r -> r.head <> []) gram.rules (* heads should not be empty*)
		in

		(* bodies must be a subset of valid symbols (alphabet + variables + epsilon) *)
		let areRuleBodiesValid =
			let allBodySymbols = Set.flatMap (fun r -> Set.make r.body) gram.rules in (* set of all bodies in rules, bodies *)
			(* Set.iter (fun s -> Printf.printf "%s\n" (symb2str s)) allBodySymbols; *)
			let allValidSymbs = Set.add epsilon (Set.union gram.alphabet gram.variables) in
			Set.subset allBodySymbols allValidSymbs (* bodies must be a subset of valid symbols (alphabet + variables + epsilon) *)
		in


		if not isValidAlphabet then
			Error.error name
				"The alphabet contains epsilon '~', and it should not" ();
		if not isValidVariables then
			Error.error name
				"The variables contain epsilon '~', and it should not" ();
		if not isIntersectionValid then
			Error.error name
				"The intersection between the alphabet and the variables is not empty" ();
		if not isInitialValid then
			Error.error (symb2str gram.initial)
				"The initial symbol is not a declared variable" ();
		if not areRuleHeadsValid then
			Error.error name
				"Some rule heads contain invalid symbols or are empty" ();
		if not areRuleBodiesValid then
			Error.error name
				"Some rule bodies contain invalid symbols" ()


	let isUnrestrictedGrammar (gram: t): bool =
		true 
		(* Unrestricted grammars have no restrictions on the form of their production rules. *)
		(* The restrictions are unsured by validate *)

	let hasEpsilonRules (gram: t)=
		let isEpsilonRule r =
			if r.body = [epsilon] then (
				if r.head = [gram.initial] then ( (* Special case: S → ε *)
					(* Ensure S doesn't appear on the body of any rule *)
					List.for_all (fun r' -> not (List.mem gram.initial r'.body)) (Set.toList gram.rules)
				)
				else false
			)
			else true (* Not an epsilon rule, so it's valid *)
		in
		List.for_all isEpsilonRule (Set.toList gram.rules)

	let lengthMonotonicCheck (gram: t): bool =
		List.for_all (fun r ->
			(* Check if the rule's head is the initial symbol and the body is epsilon *)
				(* if r.head = [gram.initial] && r.body = [epsilon] then
					false
				else   /// this applys for essentially noncontracting grammars *)
				List.length r.body >= List.length r.head
		) (Set.toList gram.rules)
		
	(* A grammar is monotonic if the length of the body of each rule is greater or equal to the length of the head. *)
	let isMonotonicGrammar (gram: t): bool =
		hasEpsilonRules gram &&
		lengthMonotonicCheck gram


	let isNoncontractingGrammar = isMonotonicGrammar

	(* A grammar is monotonic strict if the length of the body of each rule is strictly greater than the length of the head. *)
	let isMonotonicStrictGrammar (gram: t): bool =
		lengthMonotonicCheck gram


	(* A grammar is context-sensitive if it is monotonic and if each rule in P is either of the form S → ε where ε is the empty string, or of the form αAβ → αγβ *)
	let isContextSensitiveGrammar (gram: t): bool =
		(* Check if epsilon rule is valid: S → ε is allowed only if S is the start symbol and S does not appear in any rule's body *)
		let isContextRule (rule: rule) = (*check if rule are of the from αAβ → αγβ*)
			let rec findContextsHelper head body match_found =
				match head, body with
				| [], _ -> match_found  (* Return true if a match was found *)
				| _, [] -> false  (* No match found *)
				| h::ht, b::bt ->
				if h = b then
					findContextsHelper ht bt true  (* Continue matching and set match_found to true *)
				else
					Set.belongs h gram.variables &&  (* Check if h belongs to variables *)
					List.length body >= List.length head &&  (* Ensure rule is monotonic *)
					findContextsHelper ht bt match_found  (* Continue matching with the same match_found status *)
		  	in
		  	let findContexts head body =
				findContextsHelper head body false  (* Start with match_found set to false *)
		  	in
		  findContexts rule.head rule.body
		in
		(* Helper function to check if the head of the rule is a single nonterminal *)
		let isSingleNonterminal (rule: rule) =
			match rule.head with
			| [h] -> Set.belongs h gram.variables || h = epsilon
			| _ -> false
		in
		List.for_all (fun rule -> isSingleNonterminal rule || isContextRule rule) (Set.toList gram.rules)



	(*In a context-free grammar, each production rule is of the form A → α with A a single nonterminal symbol, and α a string of terminals and/or nonterminals  *)
	(*i.e the head of each rule is a single variable. *)
	let isContextFreeGrammar (gram: t): bool = (*verify if the head has exactly one symbol (length 1) and that this symbol belongs to the set of variables *)
		List.for_all (fun r -> List.length r.head = 1 && Set.belongs (List.hd r.head) gram.variables) (Set.toList gram.rules)

	(*in a left linear grammar all rules are of the form A → αw where α is either empty or a single nonterminal and w is a string of terminals *)
	let isLeftLinearGrammar (gram: t): bool =
		isContextFreeGrammar gram &&
		List.for_all (fun r ->
			match r.body with
			| [] ->
					true
			| [var] ->
					let belongs_to_vars = Set.belongs var gram.variables in
					let belongs_to_alphabet = Set.belongs var gram.alphabet in
					belongs_to_vars || belongs_to_alphabet || var = epsilon
			| var :: rest ->
					let var_belongs = Set.belongs var gram.variables in
					let rest_belongs = List.for_all (fun s -> Set.belongs s gram.alphabet) rest in
					var_belongs && rest_belongs
		) (Set.toList gram.rules)

	let isRightLinearGrammar (gram: t): bool =
		isContextFreeGrammar gram &&
		List.for_all (fun r ->
			match List.rev r.body with
			| [] ->
					true
			| [var] ->
					let belongs_to_vars = Set.belongs var gram.variables in
					let belongs_to_alphabet = Set.belongs var gram.alphabet in
					belongs_to_vars || belongs_to_alphabet || var = epsilon (*Allow epsilon ?? *)
			| var :: rest ->
					let var_belongs = Set.belongs var gram.variables in
					let rest_belongs = List.for_all (fun s -> Set.belongs s gram.alphabet) rest in
					var_belongs && rest_belongs
		) (Set.toList gram.rules)

	(*linear grammar is a context-free grammar that has at most one nonterminal in the right-hand side of each of its productions. *)
	let isLinearGrammar (gram: t): bool =
		isContextFreeGrammar gram &&
		(List.for_all (fun r ->
			let var_count = List.fold_left (fun count symb ->
				if Set.belongs symb gram.variables then count + 1 else count)
				0
				r.body
				in var_count <= 1 (* Only one variable allowed on the right-hand side *)
		) (Set.toList gram.rules))



	let rechableSymbs (gram: t) set =
		Set.fold_right (fun r acc ->
			if List.exists (fun symb -> Set.belongs symb set) r.head then
				Set.union (Set.make r.body) acc
			else
				acc
		) gram.rules set


	let rechableSymbsCFG (gram: t) set =
		Set.fold_right (fun r acc ->
			if List.for_all (fun symb -> Set.belongs symb set) r.head then
				Set.union (Set.make r.body) acc
			else
				acc
		) gram.rules set

	let rec reachSymbs (gram :t) symbSet =
		let reach = rechableSymbs gram symbSet in
		if Set.subset reach symbSet then
			reach
		else
			reachSymbs gram reach

	let allRulesAccessible (gram: t): bool =
		(* let accessSymbols = reachSymbs gram (Set.make [gram.initial]) in *)
		let accessSymbols = 
			if isContextFreeGrammar gram then
				Set.acumFixedPoint (rechableSymbsCFG gram) (Set.make [gram.initial])
			else
				Set.acumFixedPoint (rechableSymbs gram) (Set.make [gram.initial])
		in
		(* let accessSymbols = Set.acumFixedPoint (rechableSymbs gram) (Set.make [gram.initial]) in *)
		let allVars = gram.variables in
		Set.equals (Set.inter accessSymbols allVars) allVars

	let terminalSymbs (gram: t) set =
		Set.fold_right (fun r acc ->
			if Set.exists (fun elem -> Set.belongs elem set) (Set.make r.body) then
					Set.union (Set.make r.head) acc
			else
					acc
		) gram.rules set

	let terminalSymbsCFG (gram: t) set =
		Set.fold_right (fun r acc ->
			if List.for_all (fun symb -> Set.belongs symb set) r.body then
				Set.union (Set.make r.head) acc
			else
				acc
		) gram.rules set

	let rec productiveSymbs (gram :t) symbSet =
		let prod = terminalSymbs gram symbSet in
		if Set.subset prod symbSet then
			prod
		else
			productiveSymbs gram prod






	let allRulesProductive (gram: t): bool =
		(* let prodVars = productiveSymbs gram (Set.add epsilon gram.alphabet) in  *)
		let prodVars =
			if isContextFreeGrammar gram then
				Set.acumFixedPoint (terminalSymbsCFG gram) (Set.add epsilon gram.alphabet)
			else
				Set.acumFixedPoint (terminalSymbs gram) (Set.add epsilon gram.alphabet)
		in
		let allVars = Set.add epsilon (Set.union gram.variables gram.alphabet) in

		Set.equals prodVars allVars

	(* Checks if the grammar is clean (all symbols are productive and accessible) *)
	let isClean (gram: t): bool =
		allRulesAccessible gram && allRulesProductive gram


	(* Cleans the grammar by removing unproductive symbols and rules *)
	let cleanUnproductive (gram: t) =
		let allVars = gram.variables in
		let prodSymbols =
			if isContextFreeGrammar gram then
				Set.acumFixedPoint (terminalSymbsCFG gram) (Set.add epsilon gram.alphabet)
			else
				Set.acumFixedPoint (terminalSymbs gram) (Set.add epsilon gram.alphabet)
		in
		let prodVars = Set.inter prodSymbols gram.variables in

		if Set.equals prodVars allVars then
			gram
		else
			let filteredRules = Set.filter (fun rule ->
				List.for_all (fun var -> (Set.belongs var prodSymbols || var = epsilon)) rule.body
			) gram.rules in

			(*In the context of unproductive rules we can i have a rule like A -> AA and A -> ~ , in this case rule A -> AA should be removed*)
			(*First get the heads of the rules that derive epsilon*)
			let epsilonRuleHeads = Set.flat_map (fun r -> Set.make r.head) (Set.filter (fun r -> r.body = [epsilon]) filteredRules) in
			(*get the rules that have as head and body the head of epsilonRules*)
			let unprodictiveEpsilonRules =
					Set.filter
							(fun r ->
									List.for_all (fun s ->
										Set.belongs s epsilonRuleHeads
									) r.head
								&&
									List.for_all (fun s ->
											Set.belongs s epsilonRuleHeads
									) r.body
							)
							filteredRules
			in
			let newFilteredRules = Set.diff filteredRules unprodictiveEpsilonRules in
			{ gram with
			variables = Set.inter allVars prodVars;
			rules = newFilteredRules
			}

	(* Cleans the grammar by removing inaccessible symbols and rules *)
	let cleanInaccessible (gram: t) =
		let allVars = gram.variables in
		let accessSymbols = 
			if isContextFreeGrammar gram then
				Set.acumFixedPoint (rechableSymbsCFG gram) (Set.make [gram.initial])
			else
				Set.acumFixedPoint (rechableSymbs gram) (Set.make [gram.initial])
		in
		(* let accessSymbols = Set.acumFixedPoint (rechableSymbs gram) (Set.make [gram.initial]) in *)
		let accessibleVars = Set.inter accessSymbols allVars in
		if Set.equals accessibleVars allVars then
			gram
		else
			let filteredRules = Set.filter (fun rule ->
				List.for_all (fun var -> Set.belongs var accessSymbols) rule.head
				(* &&
				List.for_all (fun var -> Set.belongs var accessSymbols) rule.body *)
			) gram.rules in
			let filteredAlphabet = Set.inter gram.alphabet accessSymbols in
			{ gram with
			alphabet = filteredAlphabet;
			variables = accessibleVars;
			rules = filteredRules
			}

	(* Cleans the grammar by removing unproductive and then inaccessible symbols and rules *)
	let clean (gram: t) =
		let cleanUnproductiveGram = cleanUnproductive gram in
		(* print_endline "show cleanUnproductiveGram:";
		show cleanUnproductiveGram;   *)
		(* print_endline "show cleanInacessibleGram:"; *)
		(* let cleanInaccessibleGram = cleanInaccessible gram in *)
		(* show cleanInaccessibleGram;   *)
		cleanInaccessible cleanUnproductiveGram


	

	let rec starts_with sub lst =
		match sub, lst with
		| [], _ -> true
		| _, [] -> false
		| shd::stail, lhd::ltail -> shd = lhd && starts_with stail ltail

	let rec removeN n lst =
		match n, lst with
		| 0, _ -> lst
		| _, [] -> []
		| n, _::tail -> removeN (n - 1) tail


	let rec replace_subsequence original subseq replacement subseq_lenght = (*passing lenght to improve performance since List.lenght is O(n) *)
		match original with
		| [] -> []
		| _ when starts_with subseq original -> replacement @ (removeN subseq_lenght original) (*Returns on first occurence replacement*)
		| x::xs -> x :: (replace_subsequence xs subseq replacement subseq_lenght)

	let rec replace_subsequence_all original subseq replacement subseq_length =
		match original with
		| [] -> []
		| _ when starts_with subseq original ->
			replacement @ (replace_subsequence_all (removeN subseq_length original) subseq replacement subseq_length)
		| x::xs -> x :: (replace_subsequence_all xs subseq replacement subseq_length)


	let rechableSymbsCFG (gram: t) set =
		Set.fold_right (fun r acc ->
			if List.for_all (fun symb -> Set.belongs symb set) r.head then
				Set.union (Set.make r.body) acc
			else
				acc
		) gram.rules set

	let hasEpsilonRules (gram: t)=
		let isEpsilonRule r =
			if r.body = [epsilon] then
				true
			else false
		in
		List.exists isEpsilonRule (Set.toList gram.rules)

	let generatesEmpty (gram: t)=
		(* Collect all that are direct derivations from start symbol *)
		let accessSymbols = Set.acumFixedPoint (rechableSymbsCFG gram) (Set.make [gram.initial]) in
		let accessVars = Set.inter accessSymbols gram.variables in
		let filteredRules = Set.filter (fun rule ->
			List.for_all (fun var -> Set.belongs var accessVars) rule.head
		) gram.rules in
	
		List.exists (fun rule -> rule.body = [epsilon]) (Set.toList filteredRules)

	let removeEpsilonRules (gram: t) : t =
		(* Step 1: Identify Nullable Variables *)
		let rec nullableVars nullable =
			let newNullable = Set.fold_right (fun rule acc ->
				if List.for_all (fun symb -> Set.belongs symb nullable) rule.body then
					Set.union (Set.make rule.head) acc
				else
					acc
			) gram.rules nullable
			in
			if Set.equals newNullable nullable then
				nullable
			else
				nullableVars newNullable
		in
		let nullable = nullableVars (Set.filter (fun r -> r.body = [epsilon]) gram.rules |> Set.flatMap (fun r -> Set.make r.head)) in

		(* Step 2: Adjust Productions *)
		let newRules = Set.flatMap (fun rule ->
			let rec generateCombinations body =
				match body with
				| [] -> [[]]
				| hd :: tl ->
					let tlCombinations = generateCombinations tl in
					if Set.belongs hd nullable then
						List.map (fun comb -> hd :: comb) tlCombinations @ tlCombinations
					else
						List.map (fun comb -> hd :: comb) tlCombinations
			in
			let newBodies = generateCombinations rule.body in
			Set.make (List.map (fun body -> { head = rule.head; body }) newBodies)
		) gram.rules
		|> Set.filter (fun r -> r.body <> [] && r.body <> [epsilon] && r.head <> r.body) (* Step 3: Remove Epsilon Rules *)
		in

		{ gram with rules = newRules }

end

module GrammarAccept = (* Pedro + Artur *)
struct
	open GrammarSupport
	open GrammarPrivate
	(* open ContextFreeGrammarBasic *)


	let initialConfig (gram: t) (w: word) : configurations =
		Set.make [([gram.initial], w)]

	let rec containsSubSequence subSequence list =
		match subSequence, list with
		| [], _ -> true
		| _, [] -> false
		| sShd::sStail, lhd::ltail ->
			if sShd = lhd then containsSubSequence sStail ltail
			else containsSubSequence subSequence ltail


	let showInFile (msg: string) : unit =
		let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "file.txt" in
		output_string oc (msg ^ "\n");
		close_out oc



	let expand2 (gram: t) (sf,w) : configurations =
		let rules = Set.filter (fun r -> containsSubSequence r.head sf) gram.rules in
		(* Generate one configuration for each matching rule *)
		Set.flatMap (fun rule ->
				(* For each rule, generate just one new configuration by replacing the first and only the first occurence of its head with its body *)
				let rec combinations sf rule =
						match sf with
						| [] -> []
						| x::xs ->
								if rule.body = [epsilon] then
										(replace_subsequence sf rule.head [] (List.length rule.head)) :: (List.map (fun c -> x :: c) (combinations xs rule))
								else
										(replace_subsequence sf rule.head rule.body (List.length rule.head)) :: (List.map (fun c -> x :: c) (combinations xs rule))
				in
				let newSfs = combinations sf rule in
				Set.make (List.map (fun newSf -> (newSf, w)) newSfs)
		) rules

	let nextConfigs2 (gram: t) (sf, w) : configurations =
		(* showInFile("Starting nextConfigs for: " ^ word2str sf); *)
		let configs = expand2 gram (sf, w) in
		if isMonotonicGrammar gram then
			let filtered = Set.filter (fun (sf', _) -> List.length sf' <= List.length w) configs in
			(* Set.iter(fun (sf', w) -> showInFile("[" ^ word2str sf' ^ "," ^ word2str w ^ "]")) filtered; *)
			filtered
		else
			configs

	let nextConfigs2Full (gram: t) (sf, w) : configurations =
		(* showInFile("Starting nextConfigs for: " ^ word2str sf); *)
		let configs = expand2 gram (sf, w) in
		configs

	let rec partOneX fs h =
		match fs, h with
		| [], [] -> (true, [])
		| [], _ -> (false, [])
		| x::xs, [] -> (true, fs)
		| x::xs, y::ys ->
			if x = y then
				partOneX xs ys
			else
				(false, [])

	let partOne fs h =
		let (a,b) = partOneX fs h in
			(a,h,b)

	let part fs heads =
		let tryMatch = List.map (partOne fs) heads in
		let reallyMatch = List.filter (fun (a, b, c) -> a) tryMatch in
			List.map (fun (a,b,c) -> (b,c)) reallyMatch

	let rec processHRest heads rules w (h, rest) =
		let xRules = Set.filter (fun r -> r.head = h) rules in
		let xBodies = Set.map (fun r -> if r.body = [epsilon] then [] else r.body) xRules in (*special case foe epsilon rule*)
		let ySet = expand heads rules rest w in
		Set.flatMap (fun (fs1, w) -> Set.map (fun fs2 -> (fs2@fs1, w)) xBodies) ySet
	and expand heads rules fs w =
		match fs with
		| [] -> Set.make [([],w)]
		| x :: xs ->
			let alternativesX = Set.make (part fs heads) in
			let useX = Set.flatMap (processHRest heads rules w) alternativesX in
			let ignoreX = expand heads rules xs w in
			let restoreX = Set.map (fun (fs, w) -> (x::fs, w)) ignoreX in
			Set.union useX restoreX


	let nextConfigs (gram: t) (sf, w) : configurations =
		(* showInFile("Starting nextConfigs for: " ^ word2str sf); *)
		let rules = gram.rules in
		let heads = Set.toList (Set.map (fun r -> r.head) rules) in
		let configs = expand heads rules sf w in
		(* Set.iter(fun (sf', w) -> showInFile("[" ^ word2str sf' ^ "," ^ word2str w ^ "]")) configs;  *)
		if isMonotonicGrammar gram then
			let filtered = Set.filter (fun (sf', _) -> List.length sf' <= List.length w) configs in
			(* Set.iter(fun (sf', w) -> showInFile("[" ^ word2str sf' ^ "," ^ word2str w ^ "]")) filtered; *)
			filtered
		else
			(* Set.iter(fun (sf', w) -> showInFile("[" ^ word2str sf' ^ "," ^ word2str w ^ "]")) configs; *)
			configs
			(* configs *)



	let isAcceptingConfig (gram: t) (genW, w) : bool =
		(* Printf.printf "genW: %s , w: %s\n" (word2str genW) (word2str w);  *)
		genW = w

	let simplifyGrammar (gram: t) : t =
		gram
		|> (fun g -> if not (isClean g) then clean g else g)
		|> (fun g ->
			if isContextFreeGrammar g && hasEpsilonRules g
			then removeEpsilonRules g
			else g)

	let accept (gram: t) (w: word) : bool =
		let processed_gram = simplifyGrammar gram in
		if (generatesEmpty gram) && (w = [] || w = [epsilon]) then
			true
		else
			Model.accept processed_gram w initialConfig nextConfigs isAcceptingConfig

	let accept2 (gram: t) (w: word) : bool =
		let processed_gram = simplifyGrammar gram in
		if (generatesEmpty gram) && (w = [] || w = [epsilon]) then
			true
		else
			Model.accept processed_gram w initialConfig nextConfigs2 isAcceptingConfig

	let acceptFull (gram: t) (w: word) : bool * path * trail =
		if (generatesEmpty gram) && (w = [] || w = [epsilon]) then
			(true, [], [])
		else
			Model.acceptFull gram w initialConfig nextConfigs2 isAcceptingConfig

	let split_at n lst =
		let rec aux i acc = function
			| [] -> (List.rev acc, [])
			| h :: t as l -> if i = 0 then (List.rev acc, l) else aux (i - 1) (h :: acc) t
		in
		aux n [] lst


	let string_of_symbol_list symbols =
	  "[" ^ (String.concat "; " (List.map symb2str symbols)) ^ "]"

	let count_subsequence_occurrences subseq lst =
		let rec aux count lst =
			match lst with
			| [] -> count
			| _ when starts_with subseq lst -> aux (count + 1) (List.tl lst)
			| _ :: tl -> aux count tl
		in
	aux 0 lst

	let replace_subsequence_with_index original subseq replacement subseq_length =
			let rec aux original subseq replacement subseq_length index =
					match original with
					| [] -> ([], None)
					| _ when starts_with subseq original ->
									(replacement @ (removeN subseq_length original), Some index)
					| x::xs ->
									let (replaced, found_index) = aux xs subseq replacement subseq_length (index + 1) in
									(x :: replaced, match found_index with Some _ -> found_index | None -> Some index)
			in
			aux original subseq replacement subseq_length 0


	let find_applied_rules (gram: t) (path: path) =
		let rec aux i acc =
			if i < List.length path - 1 then
				let current = fst (List.nth path i) in
				let next = fst (List.nth path (i + 1)) in
				let rules = Set.filter (fun r -> containsSubSequence r.head current) gram.rules in
				let index_rule = ref 0 in
				let applicable_rule =
					try
						Some (Set.find (fun rule ->
							let rec combinations current rule curr_i =
								match current with
								| [] -> []
								| x::xs ->
									let (replaced, index) =
										if rule.body = [epsilon] then
											replace_subsequence_with_index current rule.head [] (List.length rule.head)
										else
											replace_subsequence_with_index current rule.head rule.body (List.length rule.head)
									in
									match index with
									| Some idx ->
										if replaced <> current then
											(replaced, Some (idx + curr_i)) :: (List.map (fun (c, i) -> (x :: c, i)) (combinations xs rule (curr_i + 1)))
										else
											List.map (fun (c, i) -> (x :: c, i)) (combinations xs rule (curr_i + 1))
									| None -> combinations xs rule (curr_i + 1)
							in
							let replaced = combinations current rule 0 in
							(* List.iter (fun (w, i) -> Printf.printf "Current %s, Replaced: %s, Index: %d\n" (word2str current) (word2str w) (match i with Some idx -> idx | None -> -1)) replaced;
							print_endline "----------------"; *)
							try
								let found = List.find (fun r -> (fst r) = next) replaced in
								match snd found with
								| Some idx -> index_rule := idx; true
								| _ -> false
							with Not_found -> false
						) rules)
					with Not_found -> None
				in
				aux (i + 1) (((current, i), (applicable_rule, !index_rule)) :: acc)
			else
				acc
		in
		let result = aux 0 [((fst (List.nth path (List.length path - 1)), List.length path - 1), (Some {head = []; body = []}, -1))] in
		List.sort (fun ((_, level1), _) ((_, level2), _) -> compare level1 level2) result

end

module GrammarGenerate =
struct
	open GrammarSupport
	open GrammarAccept
	open GrammarPrivate


	let expandGenerate (gram: t) (len: int) (sf,w) : configurations =
		let rules = gram.rules in
		let heads = Set.toList (Set.map (fun r -> r.head) rules) in
		let configs = expand heads rules sf w in
		if isMonotonicGrammar gram then
			let filtered = Set.filter (fun (sf', _) -> List.length sf' <= len) configs in
			filtered
		else
			configs

	let nextConfigsGenerate (gram: t) (len: int) (sf, w) : configurations =
		let res = expandGenerate gram len (sf, w) in
			res


	let isAcceptingConfigGenerate (gram: t) (sf, w) : bool =
		List.for_all(fun sym -> Set.belongs sym gram.alphabet) sf

	let isTerminalSymbol (symbol: symbol) : bool =
		let str = symb2str symbol in
		not ("A" <= str && str <= "Z") && not (String.get str 0 = '<' && String.get str (String.length str - 1) = '>')

	let getWord (sf, _) = List.filter (fun symb -> isTerminalSymbol symb) sf

	let generate (gram: t) (len: int) : words =
		let processed_gram =
			gram
			|> (fun g -> if not (isClean g) then clean g else g)
			|> (fun g ->
				if isContextFreeGrammar g && hasEpsilonRules g
				then removeEpsilonRules g
				else g)
		in
		(* show processed_gram; *)
		if (generatesEmpty gram) then
			Set.add (str2word "") (Model.generate processed_gram len initialConfig nextConfigsGenerate isAcceptingConfigGenerate getWord)
		else
			Model.generate processed_gram len initialConfig nextConfigsGenerate isAcceptingConfigGenerate getWord

end

module GrammarConversion = (* Pedro + Artur *)
struct
	open GrammarSupport
	open GrammarPrivate
	open GrammarGenerate

	let generateNewVariable variables=
		let start = 65 in (* ASCII value for 'A' *)
		let rec auxGenerateNewVariable i =
			if i > 90 then (* ASCII value for 'Z' No more capital letters available for new nonterminals*)
				Symbol.str2symb (IdGenerator.genVar "A")
			else
				let newVar = Char.uppercase_ascii (char_of_int i) in
				if Set.belongs (Symbol.str2symb (Char.escaped newVar)) variables then
					auxGenerateNewVariable (i + 1)
				else
					Symbol.str2symb (Char.escaped newVar)
		in
		auxGenerateNewVariable start





	(*HOW to Kuroda normal form:
	Ensure the grammar is context-sensitive
	Remove useless symbols:
		Ensure all nonterminals can derive a terminal string and can be reached from the start symbol. Use the clean.
	Convert long productions to binary form:
		Break down rules with more than two nonterminals or terminals (e.g.,  A -> X_1 X_2 X_3 ) into binary forms, using intermediate nonterminals:
		A -> X_1 A_1, A_1 -> X_2 X_3
	Handle terminal symbols:
		If a terminal appears with a nonterminal (e.g.,  A -> aB ), introduce a new nonterminal for the terminal:
		A -> T_a B, T_a -> a
	*)
		(* Helper function to count variables in a list of symbols *)
		let countVariables symbols variables =
			List.fold_left (fun count symb -> if Set.belongs symb variables then count + 1 else count) 0 symbols

		(* Helper function to find consecutive non-terminals in a list of symbols *)
		let rec findConsecutiveNonTerminals variables body =
			match body with
			| [] | [_] -> None
			| x :: y :: xs ->
				if Set.belongs x variables && Set.belongs y variables then
					Some [x; y]
				else
					findConsecutiveNonTerminals variables (y :: xs)

		(* Helper function to find a sequence of a variable and an alphabet symbol *)
		let rec findSequence variables alphabet body =
			match body with
			| [] | [_] -> None
			| x :: y :: xs ->
				if (Set.belongs x variables && Set.belongs y alphabet) || (Set.belongs x alphabet && Set.belongs y variables) then
					Some [x; y]
				else
					findSequence variables alphabet (y :: xs)

		(* Helper function to find an alphabet symbol in a sequence *)
		let rec findAlphabetSymbol alphabet sequence =
			match sequence with
			| [] -> str2symb "a" (* not reached *)
			| x :: xs ->
				if Set.belongs x alphabet then
					x
				else
					findAlphabetSymbol alphabet xs

		(* Step 1: Introduce variables for terminals (T_a -> a) using Hashtbl *)
		let introduceTerminalVariables (gram: t) : t =
			(* Create a mutable hash table to map terminal symbols to new variables *)
			(* Initial size can be estimated, e.g., size of alphabet *)
			let term_map : (symbol, symbol) Hashtbl.t = Hashtbl.create (Set.size gram.alphabet) in
			let current_vars = ref gram.variables in
			let new_terminal_rules = ref Set.empty in (* Stores T_a -> a rules *)
			let original_rules = gram.rules in (* Keep a copy of original rules *)

			(* First pass: Populate the hash map and create new variable symbols/rules *)
			Set.iter (fun terminal ->
				if terminal <> epsilon then (* Epsilon is not in the alphabet *)
					(* Generate a new variable for this terminal *)
					let new_var_symb = generateNewVariable !current_vars in
					current_vars := Set.add new_var_symb !current_vars;
					(* Add mapping to hash table: terminal -> new_var_symb *)
					Hashtbl.replace term_map terminal new_var_symb;
					(* Create the rule: new_var_symb -> terminal *)
					new_terminal_rules := Set.add { head = [new_var_symb]; body = [terminal] } !new_terminal_rules
			) gram.alphabet;

			(* Second pass: Iterate through original rules and replace terminals *)
			let final_other_rules = ref Set.empty in (* Stores modified original rules *)
			Set.iter (fun rule ->
				let replace_terminal symb =
					match Hashtbl.find_opt term_map symb with (* Lookup in hash table *)
					| Some new_var -> new_var (* Found mapping, replace *)
					| None -> symb (* Not a terminal needing replacement, keep as is *)
				in
				let new_head = List.map replace_terminal rule.head in
				let new_body = List.map replace_terminal rule.body in
				final_other_rules := Set.add { head = new_head; body = new_body } !final_other_rules
			) original_rules;

			(* Combine the new (T_a -> a) rules and the modified original rules *)
			let final_rules = Set.union !new_terminal_rules !final_other_rules in

			(* Return the updated grammar *)
			{ gram with variables = !current_vars; rules = final_rules }
			(* We don't need to return the hash table itself if it's not used later *)

		(* Main transformation function *)
		let rec transformGrammar gram =
			(* Step 1: Prioritize checking for heads longer than 2 symbols *)
			match Set.find_opt (fun r -> List.length r.head > 2) gram.rules with
			| Some rule_with_long_head ->
					(* Found a rule with head length > 2. Fix this first. *)
					(* Example: H1 H2 H3... -> Body  becomes  NewVar -> H1 H2,  NewVar H3... -> Body *)
					let newVariable = generateNewVariable gram.variables in
					let updated_vars = Set.add newVariable gram.variables in
					let remaining_rules = Set.remove rule_with_long_head gram.rules in
		
					(match rule_with_long_head.head with
					 | h1 :: h2 :: rest_head -> (* Must have at least 3 elements due to length check *)
							 let newRuleHead = { head = [newVariable]; body = [h1; h2] } in
							 let modifiedRule = { head = newVariable :: rest_head; body = rule_with_long_head.body } in
							 let updated_rules = Set.add newRuleHead (Set.add modifiedRule remaining_rules) in
							 let updated_gram = { gram with variables = updated_vars; rules = updated_rules } in
							 transformGrammar updated_gram (* Recurse: continue transforming *)
					 | _ ->
							 (* This case should technically not be reached if List.length > 2 *)
							 Error.error "transformGrammar" "Internal inconsistency checking head length" ();
							 gram (* Return original gram on unexpected error *)
					)
		
			| None ->
					(* No heads longer than 2 found. Now check for bodies longer than 2 symbols. *)
					(* This part adapts the KNF requirement: Head -> B1 B2 B3... becomes Head -> B1 NewVar, NewVar -> B2 B3... *)
					match Set.find_opt (fun r -> List.length r.body > 2) gram.rules with
					| Some rule_with_long_body ->
							(* Found a rule with body length > 2. *)
							let newVariable = generateNewVariable gram.variables in
							let updated_vars = Set.add newVariable gram.variables in
							let remaining_rules = Set.remove rule_with_long_body gram.rules in
		
							(match rule_with_long_body.body with
							 | b1 :: rest_body_all when List.length rest_body_all >= 2 -> (* Body needs at least 3 elements *)
									 let newRuleBody = { head = [newVariable]; body = rest_body_all } in
									 let modifiedRule = { head = rule_with_long_body.head; body = [b1; newVariable] } in
									 let updated_rules = Set.add newRuleBody (Set.add modifiedRule remaining_rules) in
									 let updated_gram = { gram with variables = updated_vars; rules = updated_rules } in
									 transformGrammar updated_gram (* Recurse: continue transforming *)
							 | _ ->
									 (* This case should not be reached if List.length > 2 *)
									 Error.error "transformGrammar" "Internal inconsistency checking body length" ();
									 gram (* Return original gram on unexpected error *)
							)
					| None ->
							(* No heads > 2 and no bodies > 2. Transformation is complete. *)
							gram
		(* let rec transformGrammar gram =

			let longProductionsBody = Set.filter (fun r -> countVariables r.body gram.variables > 2) gram.rules in
			
			if Set.isEmpty longProductionsBody then
				gram
			else
				let gram =
					if not (Set.isEmpty longProductionsBody) then
						let longProduction = Set.hd longProductionsBody in
						let newVariable = generateNewVariable gram.variables in
						let consecutiveNonTerminals = findConsecutiveNonTerminals gram.variables longProduction.body in
						let newRules =
							List.map (fun rule ->
								match consecutiveNonTerminals with
								| Some cn ->
									let new_body = replace_subsequence rule.body cn [newVariable] 2 in
									{ rule with body = new_body }
								| None -> rule
							) (Set.toList gram.rules)
						in
						let newRules = Set.add {head = [newVariable]; body = Option.get consecutiveNonTerminals} (Set.make newRules) in
						{gram with rules = newRules; variables = Set.add newVariable gram.variables}
					else
						gram
				in

		
				transformGrammar gram *)



		 
	let kurodaNormalForm (gram: t) : t =
		(* show gram;
		show (replaceTerminalSymbols gram); *)
		gram
			|> clean
			|> introduceTerminalVariables
			|> transformGrammar
		

		(* gram
		|> clean 
		|> transformGrammar *)


	(*
	NOTE:
	Conversely, every noncontracting grammar that does not generate the empty string
	can be converted to Kuroda normal form.

	A straightforward technique attributed to György Révész
	transforms a grammar in Kuroda normal form to a
	context-sensitive grammar: AB → CD is replaced
	by four context-sensitive rules AB → AZ, AZ → WZ, WZ → WD and WD → CD. *)

	let makeContextRules (gram: t) : t =
		let rec processRules rulesToCheck gramAux =
			match rulesToCheck with
			| [] -> gramAux
			| rule :: rest ->
					if List.length rule.head = 2 && List.length rule.body = 2 then
							match rule.head, rule.body with
							| [a; b], [c; d] ->
								if a <> c && b <> d then
									let z = generateNewVariable gramAux.variables in
									let w = generateNewVariable (Set.union gramAux.variables (Set.make [z])) in
									let new_variables = Set.add z (Set.add w gramAux.variables) in

									let new_rules = Set.add {head = [a; b]; body = [a; z]} gramAux.rules in
									let new_rules = Set.add {head = [a; z]; body = [w; z]} new_rules in
									let new_rules = Set.add {head = [w; z]; body = [w; d]} new_rules in
									let new_rules = Set.add {head = [w; d]; body = [c; d]} new_rules in
									processRules rest {gramAux with rules = (Set.remove rule new_rules); variables = new_variables}
								else
									processRules rest gramAux
							| _ -> failwith  "No supposed to happen, matching error"
					else processRules rest gramAux

		in
		processRules (Set.toList gram.rules) gram

	let nonContractingToCSG (gram: t) : t =
			if isContextSensitiveGrammar gram then
					gram
			else
					if isNoncontractingGrammar gram then
						let kuroda_gram = kurodaNormalForm gram in
						(* print_endline "Kuroda Grammar:";
						show kuroda_gram; *)
						(* let new_gram = makeContextRules kuroda_gram in *)
						(* print_endline "New Grammar:"; *)
						makeContextRules kuroda_gram
					else
							failwith "Grammar is not noncontracting"

	(*HOW TO DO Grammar to Penttonen normal form:
	Perform the Kuroda Normal Form Conversion
	Remove unit productions:
		Eliminate any unit productions of the form  A -> B , where both  A  and  B  are nonterminals.*)
	let rec removeUnitProductions (gram: t) : t =
		(* Identify unit productions *)
		let unitProductions = Set.filter (fun r -> List.length r.head = 1
				&& List.length r.body = 1
				&& Set.belongs (List.hd r.body) gram.variables
				&& Set.belongs (List.hd r.head) gram.variables)
		gram.rules in

		if Set.isEmpty unitProductions then
				gram
		else
		(* Select a unit production *)
		let unitProduction = Set.hd unitProductions in
		let symbolToReplace = List.hd unitProduction.head in
		let replacementSymbol = List.hd unitProduction.body in
		let replace_symbol lst =
				List.map (fun x -> if x = symbolToReplace then replacementSymbol else x) lst
		in
		(* Replace occurrences of unitProduction.head with unitProduction.body in other rules *)
		let newRules =
			List.filter (fun rule -> rule.head <> rule.body) (
				List.map (fun rule ->
					let new_head = replace_symbol rule.head in
					let new_body = replace_symbol rule.body in
					{ head = new_head; body = new_body }
				) (Set.toList gram.rules)
			)
		in
		let newVariables = Set.remove symbolToReplace gram.variables in
		let newInitial = if gram.initial = symbolToReplace then replacementSymbol else gram.initial in
		removeUnitProductions {gram with rules = (Set.make newRules); variables = newVariables; initial = newInitial}

	let penttonenNormalForm (gram: t) : t =
		gram
		|> kurodaNormalForm
		|> makeContextRules
		|> removeUnitProductions



end




module Grammar =
struct
    include GrammarSupport
    open GrammarPrivate

    (* Make *)
    let make2 (arg: t Arg.alternatives): Entity.t * t = make2 arg validate
    let make (arg: t Arg.alternatives): t = make arg validate

    let isUnrestrictedGrammar = isUnrestrictedGrammar
    let isContextFreeGrammar = isContextFreeGrammar
    let isLinearGrammar = isLinearGrammar
    let isRightLinearGrammar = isRightLinearGrammar
    let isLeftLinearGrammar = isLeftLinearGrammar
    let isContextSensitiveGrammar = isContextSensitiveGrammar
    let isMonotonicGrammar = isMonotonicGrammar
		let isMonotonicStrictGrammar = isMonotonicStrictGrammar
    let isNoncontractingGrammar = isNoncontractingGrammar
		let removeEpsilonRules = removeEpsilonRules
		let hasEpsilonRules = hasEpsilonRules


		(*Conversions*)
		let kurodaNormalForm = GrammarConversion.kurodaNormalForm
		let penttonenNormalForm = GrammarConversion.penttonenNormalForm
		let nonContractingToCSG = GrammarConversion.nonContractingToCSG
		(* Clean *)
		let allRulesAccessible = allRulesAccessible
		let allRulesProductive = allRulesProductive
		let isClean = isClean
		let clean = clean

		(* Acceptance *)
		let accept = GrammarAccept.accept
		let accept2 = GrammarAccept.accept2
		let acceptFull = GrammarAccept.acceptFull
		let find_applied_rules = GrammarAccept.find_applied_rules

		(* Generate *)
		let generate = GrammarGenerate.generate

    (* Exercices support *)
    let checkProperty (gram: t) (prop: string) =
        match prop with
        | "grammar" -> true
				| "unrestricted grammar" -> true
				| "monotonic grammar" -> isMonotonicGrammar gram
				| "monotonic grammar strict" -> isMonotonicStrictGrammar gram
				| "context free grammar" -> isContextFreeGrammar gram
				| "context sensitive grammar" -> isContextSensitiveGrammar gram
				| "context sensitive grammar strict" -> isContextSensitiveGrammar gram && not (isContextFreeGrammar gram)
				| "noncontracting grammar" -> isNoncontractingGrammar gram
				| "linear grammar" -> isLinearGrammar gram
				| "right linear grammar" -> isRightLinearGrammar gram
				| "left linear grammar" -> isLeftLinearGrammar gram
        | _ -> Model.checkProperty prop
    let checkExercise ex gram = Model.checkExercise ex (accept gram) (checkProperty gram)
    let checkExerciseFailures ex gram = Model.checkExerciseFailures ex (accept gram) (checkProperty gram)




    class model (arg: t Arg.alternatives) =
        object(self) inherit Model.model (make2 arg) as super
            val mutable simplified = false
        (* Representation *)
            method representation = representation
        (* Kind *)
						method isGrammar : bool = true

        (* Show *)
            method toJSon: JSon.t = toJSon representation
            method toJSon2: JSon.t = toJSon2 id representation
            method show: unit = show representation
            method show2: unit = show2 id representation

            method accept (testWord:word) : bool = GrammarAccept.accept representation testWord
            method generate (length:int) : words = GrammarGenerate.generate representation length

        (* Exercices support *)
            method checkProperty (prop: string) = checkProperty representation prop

        (* Learn-OCaml support *)
            method moduleName = moduleName
            method xTypeName = xTypeName
            method xTypeDeclString : string = prelude
            method toDisplayString (name: string): string = solution name self#representation
            method example : JSon.t = example
        end

end


module GrammarTop =
struct
	open Grammar
	open GrammarBasicsX

	type configurationX = string * string
	type configurationsX = configurationX list

	let confX ((s,w): configuration): configurationX =
		(wordX s, wordX w)
	let confsX (c: configurations): configurationsX =
		List.map confX (Set.toList c)

	let pathX (p: path) = pathX confX p
	let trailX (t: trail) = trailX confX t

	let gI = internalize
	let gX = externalize

	let g_load file = gX (make (Arg.File file))
	let g_text text = gX (make (Arg.Text text))
	let g_json json = gX (make (Arg.JSon json))
	let g_predef name = g_text (Examples.example name)

	let g_init gx w =
		let is = GrammarAccept.initialConfig (gI gx) (wordI w) in
			confsX is

	let stats () = RuntimeControl.stats ()

	let g_accept gx w = accept (gI gx) (wordI w)

	let g_path gx w =
		let (r,p,t) = acceptFull (gI gx) (wordI w) in
			pathX p

	let g_trail gx w =
		let (r,p,t) = acceptFull (gI gx) (wordI w) in
			trailX t

	let g_generate gx len = wordsX (generate (gI gx) len)
end

open GrammarTop


(*

--------------------

#print_depth 10000;;
#print_length 10000;;

let ab = {| {
		kind : "grammar",
		description : "this is an example",
		name : "ab",
		alphabet : ["a", "b"],
		variables : ["S", "A", "B"],
		initial : "S",
		rules : [	"S -> AB",
					"A -> aA | ~",
					"B -> b" ]
		} |};;

let g = g_text ab;;
let w = "ab";;
g_init g w;;
g_accept g "ab";;
g_path g "ab";;
g_trail g "ab";;
g_generate g 4;;
--------------------

*)
