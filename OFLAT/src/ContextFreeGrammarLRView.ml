open OCamlFlat
open BasicTypes
open JS
open Js_of_ocaml
open Js.Opt
open Lang
open HTMLTable
open StateVariables

open ContextFreeGrammarLL1View

module ContextFreeGrammarLRView		
=
struct
	open ContextFreeGrammarBasic
	open ContextFreeGrammarLR

	type t = ContextFreeGrammarBasic.t
	type cfgTree2 = Leaf of string * symbol | Root of string * symbol * cfgTree2 list
	type syntaxTable = ContextFreeGrammarLL1.syntaxTable
	type acceptTable = ContextFreeGrammarLL1.acceptTable
	type recognized = ContextFreeGrammarLL1.recognized
	type acceptStep = {
		syntaxTable : syntaxTable;
		acceptedString: string;
		acceptTable : acceptTable;
		recognized : recognized;
		accepted: bool option;
		nodes: cfgTree2 list;
		cyId: string option
	}

	let firstFollowTableId = ContextFreeGrammarLL1View.firstFollowTableId
	let parsingTableId = ContextFreeGrammarLL1View.parsingTableId
	let parsingGuideTableId = ContextFreeGrammarLL1View.parsingGuideTableId
	let productionsTableId = ContextFreeGrammarLL1View.productionsTableId
	let productionsTableId2 = ContextFreeGrammarLL1View.productionsTableId2

	type lr0Item = LR0Grammar.lr0Item	
	type lr0State = LR0Grammar.lr0State	
	
	type stateName = LR0Grammar.stateName
	
	type lr0StateId = LR0Grammar.lr0StateId 
	type lr0DiagramId = LR0Grammar.lr0DiagramId
	
	type lr0Action = LR0Grammar.lr0Action
	type lr0TableEntry = LR0Grammar.lr0TableEntry
	type lr0Table = LR0Grammar.lr0Table
	
	type lr0TableEntryExt = stateName * (symbol * stateName) set * lr0Action set
	type lr0TableExt = lr0TableEntryExt set
	
	type slr1Action = SLR1Grammar.slr1Action
	type slr1TableEntry = SLR1Grammar.slr1TableEntry
	type slr1Table = SLR1Grammar.slr1Table
	
	
	type lr1Item = LR1Grammar.lr1Item
	type lr1State = LR1Grammar.lr1State
	type lr1Diagram = LR1Grammar.lr1Diagram
	
	type lr1StateId = LR1Grammar.lr1StateId
	type lr1DiagramId = LR1Grammar.lr1DiagramId
	
	type lr1Action = LR1Grammar.lr1Action
	type lr1TableEntry = LR1Grammar.lr1TableEntry
	type lr1Table = LR1Grammar.lr1Table
	
	
	let writeLR0StateItems (lr0state:lr0State)  = 
		let open LR0Grammar in
		List.map (fun it -> (symb2str it.head)  ^ " →" ^ " "  
					^ (word2str it.body1)  ^ "•"  
					^ (word2str it.body2)) (Set.toList lr0state)
					
	let writeLR1StateItems (lr1state:lr1State)  = (* Depois alterar para LR1 *)
		let open LR1Grammar in
		List.map (fun it -> (symb2str it.head)  ^ " →" ^ " "  
					^ (word2str it.body1)  ^ "•"  
					^ (word2str it.body2)  ^ " ,{"
					^ (word2str (Set.toList it.lookahead)) ^ "}") (Set.toList lr1state)
		
	
	let inputLR0Nodes cy lr0Diagram = 
		let (statesId,transitions) = lr0Diagram in
		let ids = Set.map fst statesId in
			Set.iter (fun id -> Cytoscape.addNode2 cy id false false) ids
			
	let inputLR0NodesV2 cy lr0Diagram = 
		let (statesId,transitions) = lr0Diagram in
			Set.iter (fun (id,state) -> Cytoscape.addNode2 cy id false false; 
										Cytoscape.buildLR0NodeStyle cy id (writeLR0StateItems state)) statesId
		
        
       
    let inputLR0Edges cy lr0Diagram =
		let (statesId,transitions) = lr0Diagram in
			Set.iter (fun ((id1,_),symb,(id2,_)) -> (Cytoscape.addEdge cy (id1,symb2str symb,id2)) ) transitions
			
	
	let inputLR1NodesV2 cy lr1Diagram = 
		let (statesId,transitions) = lr1Diagram in
		Set.iter (fun (id,state) -> Cytoscape.addNode2 cy id false false; 
									Cytoscape.buildLR0NodeStyle cy id (writeLR1StateItems state)) statesId
		
        
       
    let inputLR1Edges cy lr1Diagram =
		let (statesId,transitions) = lr1Diagram in
			Set.iter (fun ((id1,_),symb,(id2,_)) -> (Cytoscape.addEdge cy (id1,symb2str symb,id2)) ) transitions



	let rec rev l =
		match l with
		| [] -> []
		| x::xs -> (rev xs) @ [x]


	let actionToString action =
		let open LR0Grammar in
		match action with
		| Accept -> "Accept"
		| Shift -> "Shift"
		| Reduce({head = h;body = b}) -> 
				if(List.length b = 0) then 
					(symb2str h) ^ "→" ^ StateVariables.returnEmpty()
				else
					(symb2str h) ^ "→" ^ (word2str b)
		
	let slr1actionToString action =
		let open SLR1Grammar in
		match action with
		| Accept -> "Accept"
		| Shift -> "Shift"
		| Reduce({head = h;body = b}) -> 
				if(List.length b = 0) then 
					(symb2str h) ^ "→" ^ StateVariables.returnEmpty()
				else
					(symb2str h) ^ "→" ^ (word2str b)
		
	let lr1actionToString action =
		let open LR1Grammar in
		match action with
		| Accept -> "Accept"
		| Shift -> "Shift"
		| Reduce({head = h;body = b}) -> 
				if(List.length b = 0) then 
					(symb2str h) ^ "→" ^ StateVariables.returnEmpty()
				else
					(symb2str h) ^ "→" ^ (word2str b)


	let fillLR0Row row entry symbSet = (*symbSet is a set containg all variable and alphabet symbols *)
		let (stateName,shifts,action) = entry in
		let _ = HTMLTable.insertCell row stateName stateName in
		let s = Set.iter ( fun symb -> (* Para cada simbolo, se existir uma transição possivel, preencher com o estado resultante *)
			let targetShifts = Set.filter (fun (a,b) -> a = symb) shifts in
			if(Set.size targetShifts > 0) then 
				let (nextSymbol,nextState) = Set.nth targetShifts 0 in
				let _ = HTMLTable.insertCell row nextState nextState in ()
		) symbSet in
		(*Inserir ação LR0 no final da linha*)
		let _ = HTMLTable.insertCell row (actionToString action) (actionToString action) in
			s


	let createLR0ParsingTableHtml (cfg:t) = (* Não é usado atualmente, versão anterior que so retorna uma ação LR0 por coluna. *)
		let table = HTMLTable.fetchTable("cfgParsingTable") in
		let lr0Table = LR0Grammar.getLR0Table cfg in
		let row = HTMLTable.insertRow table in
		(*Inserir celula vazia *)
		let _ = HTMLTable.insertCell row "LR0" "LR0" in
		let symbSet = Set.union cfg.alphabet cfg.variables in
		(* criar header da tabela *)
		    Set.iter ( fun symb ->
			let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) symbSet;
		(*Inserir celula de ação LR0 *)	
		let _ = HTMLTable.insertCell row "Action" "Action" in ();
			Set.iter ( fun entry -> (* preencher linha correspondente a um estado, repetindo para todos os estados/linhas da tabela *)
				let row = HTMLTable.insertRow table in
					fillLR0Row row entry symbSet
			) lr0Table;
			
		let numberOfCells = (Set.size symbSet) + 2 in
		let cellWidth = 50 in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth) in
			HTMLTable.setWidth table width

		
(* -------extended LR0 for clarity----------- Allows LR0 Tables to display multiple actions in each state line.*)

	let rec concActionsLR0 actions =
		match actions with
		| [] -> ""
		| x::xs -> (actionToString x) ^ "\n" ^ concActionsLR0 xs


	let fillLR0RowExt row entry symbSet = (*symbSet is a set containg all variable and alphabet symbols *)
		let (stateName,shifts,actionSet) = entry in
		let _ = HTMLTable.insertCell row stateName stateName in
		let s = Set.iter ( fun symb -> (* Para cada simbolo, se existir uma transição possivel, preencher com o estado resultante *)
			let targetShifts = Set.filter (fun (a,b) -> a = symb) shifts in
			let newCell1 = HTMLTable.insertCell row "" "" in
			if(Set.size targetShifts > 0) then 
				let (nextSymbol,nextState) = Set.nth targetShifts 0 in
				let _ = HTMLTable.modifyCell newCell1 nextState in ()
		) symbSet in
		(*Inserir ação LR0 no final da linha*)
		let actionList = Set.toList actionSet in
			(*newCell1##.innerHTML := Js.string (actionToString action); *)
			let newCell3 = HTMLTable.insertCell row (concActionsLR0 (Set.toList actionSet)) (concActionsLR0 (Set.toList actionSet)) in (* metodo para devolver o conjunto de ações em conflito em vez de CONF*)
			if(List.length actionList > 1) then
				let newCell4 = HTMLTable.cToCell newCell3 in
				newCell4##.style##.backgroundColor := (Js.string "red");
			s
		
		
	let createLR0ParsingTableHtmlExt (cfg:t) =
		let table = HTMLTable.fetchTable("cfgParsingTable") in
		let lr0Table = LR0Grammar.getLR0TableExt cfg in
		let row = HTMLTable.insertRow table in
		(*Inserir celula vazia *)
		let _ = HTMLTable.insertCell row "LR0" "LR0" in
		let symbSet = Set.union cfg.alphabet cfg.variables in
		(* criar header da tabela *)
		    Set.iter ( fun symb ->
					let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) symbSet;
		(*Inserir celula de ação LR0 *)	
		let _ = HTMLTable.insertCell row "Action" "Action" in ();
			Set.iter ( fun entry -> (* preencher linha correspondente a um estado, repetindo para todos os estados/linhas da tabela *)
				let row = HTMLTable.insertRow table in
					fillLR0RowExt row entry symbSet
			) lr0Table;	
		let numberOfCells = (Set.size symbSet) + 3 in
		let cellWidth = 50 in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth) in
			HTMLTable.setWidth table width

	(* ------- End of extended LR0 -----------*)

	let rec concActions actions =
		match actions with
		| [] -> ""
		| x::xs -> (slr1actionToString x) ^ "\n" ^ concActions xs
		
	
	let rec concActionsLR1 actions =
		match actions with
		| [] -> ""
		| x::xs -> (lr1actionToString x) ^ "\n" ^ concActionsLR1 xs
		
		
	let fillSLR1Row row (entry:SLR1Grammar.slr1TableEntry) symbSet completeAlphabet = (*symbSet is a set containg all variable and alphabet symbols *)
		let (stateName,shifts,actionSet) = entry in
		let _ = HTMLTable.insertCell row stateName stateName in
		Set.iter ( fun symb -> (* Para cada simbolo, se existir uma transição possivel, preencher com o estado resultante *)
		let newCell1 = HTMLTable.insertCell row "" "" in
			let targetShifts = Set.filter (fun (a,b) -> a = symb) shifts in
			if(Set.size targetShifts > 0) then 
				let (nextSymbol,nextState) = Set.nth targetShifts 0 in
				let _ = HTMLTable.modifyCell newCell1 nextState in ()
		) symbSet;
		(*Inserir ação SLR1 no final da linha*)
		
			Set.iter ( fun symb -> 
				let symbActions = Set.filter( fun (s,a) -> s = symb && Set.size a > 0 ) actionSet in
				let newCell1 = HTMLTable.insertCell row "" "" in
				if(Set.size symbActions > 0) then 
					let (symbol,actions) = Set.hd symbActions in (* atualmente está a falhar aqui, com hd failure *)
					let action = Set.hd actions in
					let newCell2 = HTMLTable.modifyCell newCell1 (slr1actionToString action) in
					if(Set.size actions > 1) then
					let actionList = Set.toList actions in
					let newCell2 = HTMLTable.modifyCell newCell2 (concActions (actionList)) in
						if(List.length actionList > 1) then
							let newCell4 = HTMLTable.cToCell newCell2 in
						newCell4##.style##.backgroundColor := (Js.string "red");
						(* newCell1##.style := Js.string "red"; *) (* maybe later *)
						(* newCell1##.innerHTML := Js.string ("Conf"); *)
			) completeAlphabet
				
	
	let createSLR1ParsingTableHtml (cfg:t) =
		let table = HTMLTable.fetchTable("cfgParsingTable") in
		let slr1Table = SLR1Grammar.getSLR1Table cfg in
		let row = HTMLTable.insertRow table in
		(*Inserir celula vazia *)	
		let _ = HTMLTable.insertCell row "SLR1" "SLR1" in
		let symbSet = Set.union cfg.alphabet cfg.variables in
		(* criar header da tabela *)
		    Set.iter ( fun symb ->
					let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) symbSet;
		(*Inserir celulas de ações para cada simbolo do alfabeto *)	
		
		let completeAlphabet = (Set.add dollar cfg.alphabet) in
		
			Set.iter (fun symb ->
				let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) completeAlphabet;
		
			Set.iter ( fun entry -> (* preencher linha correspondente a um estado, repetindo para todos os estados/linhas da tabela *)
			let row = HTMLTable.insertRow table in
					fillSLR1Row row entry symbSet completeAlphabet
			) slr1Table;
			
		let numberOfCells = (Set.size symbSet) + (Set.size completeAlphabet) + 1 in
		let cellWidth = 50 in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth) in
			HTMLTable.setWidth table width
			
	
	
	let fillLR1Row row (entry:LR1Grammar.lr1TableEntry) symbSet completeAlphabet = (*symbSet is a set containg all variable and alphabet symbols *)
		let (stateName,shifts,actionSet) = entry in
		let _ = HTMLTable.insertCell row stateName stateName in
		Set.iter ( fun symb -> (* Para cada simbolo, se existir uma transição possivel, preencher com o estado resultante *)
			let newCell1 = HTMLTable.insertCell row "" "" in
			let targetShifts = Set.filter (fun (a,b) -> a = symb) shifts in
			if(Set.size targetShifts > 0) then 
				let (nextSymbol,nextState) = Set.nth targetShifts 0 in
			let _ = HTMLTable.modifyCell newCell1 nextState in ()
		) symbSet;
		(*Inserir ação SLR1 no final da linha*)
		
			Set.iter ( fun symb -> 
				let symbActions = Set.filter( fun (s,a) -> s = symb && Set.size a > 0 ) actionSet in
				let newCell1 = HTMLTable.insertCell row "" "" in
				if(Set.size symbActions > 0) then 
					let (symbol,actions) = Set.hd symbActions in 
					let action = Set.hd actions in
					let newCell2 = HTMLTable.modifyCell newCell1 (lr1actionToString action) in
					if(Set.size actions > 1) then
					let actionList = Set.toList actions in
						let newCell3 = HTMLTable.modifyCell newCell2 (concActionsLR1 (actionList)) in (* metodo para devolver o conjunto de ações em conflito em vez de CONF, provavelmente esta será a opção futura correta *)
						if(List.length actionList > 1) then
							let newCell4 = HTMLTable.cToCell newCell3 in
							newCell4##.style##.backgroundColor := (Js.string "red");
						(* newCell1##.innerHTML := Js.string ("Conf"); *)
			) completeAlphabet
			
	let createLR1ParsingTableHtml (cfg:t) =
		let table = HTMLTable.fetchTable("cfgParsingTable") in
		let lr1Table = LR1Grammar.getLR1Table cfg in
		let row = HTMLTable.insertRow table in
		(*Inserir celula vazia *)	
		let _ = HTMLTable.insertCell row "LR1" "LR1" in
		let symbSet = Set.union cfg.alphabet cfg.variables in
		(* criar header da tabela *)
		    Set.iter ( fun symb ->
					let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) symbSet;
		(*Inserir celulas de ações para cada simbolo do alfabeto *)	
		
		let completeAlphabet = (Set.add dollar cfg.alphabet) in
		
			Set.iter (fun symb ->
				let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) completeAlphabet;
		
			Set.iter ( fun entry -> (* preencher linha correspondente a um estado, repetindo para todos os estados/linhas da tabela *)
				let row = HTMLTable.insertRow table in
					fillLR1Row row entry symbSet completeAlphabet
			) lr1Table;
			
		let numberOfCells = (Set.size symbSet) + (Set.size completeAlphabet) + 1 in
		let cellWidth = 50 in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth) in
			HTMLTable.setWidth table width
			
			
			
	let createLALR1ParsingTableHtml (cfg:t) =
		let table = HTMLTable.fetchTable("cfgParsingTable") in
		let lalr1Table = LALR1Grammar.getLALR1Table cfg in
		let row = HTMLTable.insertRow table in
		(*Inserir celula vazia *)	
		let symbSet = Set.union cfg.alphabet cfg.variables in
		(* criar header da tabela *)
		    Set.iter ( fun symb ->
					let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) symbSet;
		(*Inserir celulas de ações para cada simbolo do alfabeto *)	
		
		let completeAlphabet = (Set.add dollar cfg.alphabet) in
		
			Set.iter (fun symb ->
				let _ = HTMLTable.insertCell row (symb2str symb) (symb2str symb) in ();
			) completeAlphabet;
		
			Set.iter ( fun entry -> (* preencher linha correspondente a um estado, repetindo para todos os estados/linhas da tabela *)
				let row = HTMLTable.insertRow table in
					fillLR1Row row entry symbSet completeAlphabet
			) lalr1Table;
		
		let numberOfCells = (Set.size symbSet) + (Set.size completeAlphabet) + 1 in
		let cellWidth = 50 in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth) in
			HTMLTable.setWidth table width



	let stringFromBool value =
		if value then
			"True"
		else
			"False"
			
	let boolFromString value =
	match value with
	| "Ongoing" -> "True"
	| "Aceite" -> "True"
	| v -> "False"


	let rec stringListToString l =
		match l with
		| [] -> ""
		| x::xs -> x ^ stringListToString xs
		
	(* type predictAction = | Error | Conflict | Accept | Shift | Reduce of rule (*Ou Normal of lr0Action*) *)
	
	
		
	let parserPredictLR0Action word lr0Table revStack (cfg:t) :  string = 
		let currentState = int_of_string (List.hd revStack) in 
		let (_,shifts,(action:lr0Action)) = Set.nth lr0Table currentState in (* get corresponding table entry *)		
		match action with
		| Shift -> 
			begin
				match word with
				| [] -> "Error"
				| s::_ -> 
					if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
						let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
							if(Set.size targetShifts = 0) then "Error"
							else
								"Shift"
					else				
						"Error"
			end
		| Accept -> 
			if (word = [dollar]) then
					"Ac"
				else 
					"Error"
		| Reduce({head = h;body = b}) -> 
			(symb2str h) ^ "->" ^ (word2str b)
				
	let rec parserPredictSLR1Action word slr1Table revStack (cfg:t) :  string = 
		let currentState = int_of_string(List.hd revStack) in 
		let (id,shifts,actionSet) = Set.nth slr1Table currentState in (* get corresponding table entry *)
		let topSymbol = List.nth word 0 in
			if(Set.belongs topSymbol cfg.variables) then (*Fazemos um Shift com uma variavel -> Goto*)
				let targetShifts = Set.filter (fun (a,b) -> a = topSymbol) shifts in
					if(Set.size targetShifts = 0) then "Error"
					else
						"Shift"
			else 
				let peekedSymbol = List.nth word 0 in
				let peekedsymbolAndActions = Set.filter( fun (s,a) -> s = peekedSymbol && Set.size a > 0 ) actionSet in
				let nEntries = Set.size peekedsymbolAndActions in
					if nEntries = 0 then 
						"Error"
					else if nEntries > 1 then
						"Conflict"
					else
						let (symbol,actions) = Set.hd peekedsymbolAndActions in (* atualmente está a falhar aqui, com hd failure *)
						let (action:slr1Action) = Set.hd actions in
						match action with
						| Shift -> 
							begin
								match word with
								| [] -> "Error"
								| s::_ -> 
									if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
										let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
											if(Set.size targetShifts = 0) then "Error"
											else
												"Shift"
									else
										"Error"
							end
						| Accept -> 
							if (word = [dollar]) then
								"Ac"
							else 
								"Error"
						| Reduce({head = h;body = b}) -> 
							(symb2str h) ^ "->" ^ (word2str b)		
			
	let rec parserPredictLR1Action word lr1Table revStack (cfg:t) :  string = 
		let currentState = int_of_string(List.hd revStack) in 
		let (id,shifts,actionSet) = Set.nth lr1Table currentState in (* get corresponding table entry *)
		let topSymbol = List.nth word 0 in
			if(Set.belongs topSymbol cfg.variables) then (*Fazemos um Shift com uma variavel -> Goto*)
				let targetShifts = Set.filter (fun (a,b) -> a = topSymbol) shifts in
					if(Set.size targetShifts = 0) then "Error"
					else
						"Shift"
			else 
				let peekedSymbol = List.nth word 0 in
				let peekedsymbolAndActions = Set.filter( fun (s,a) -> s = peekedSymbol && Set.size a > 0 ) actionSet in
				let nEntries = Set.size peekedsymbolAndActions in
					if nEntries = 0 then 
						"Error"
					else if nEntries > 1 then
						"Conflict"
					else
						let (symbol,actions) = Set.hd peekedsymbolAndActions in (* atualmente está a falhar aqui, com hd failure *)
						let (action:lr1Action) = Set.hd actions in
						match action with
						| Shift -> 
							begin
								match word with
								| [] -> "Error"
								| s::_ -> 
									if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
										let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
											if(Set.size targetShifts = 0) then "Error"
											else
												"Shift"
									else
										"Error"
							end
						| Accept -> 
							if (word = [dollar]) then
								"Ac"
							else 
								"Error"
						| Reduce({head = h;body = b}) -> 
							(symb2str h) ^ "->" ^ (word2str b)
		
	
	let buildLR0Steps word2 cfg =
		let word = str2word word2 in
		let open LR0Grammar in
		let firstStep = acceptWordLR0Init word cfg in
		let stepList : lr0TableStep list = [firstStep] in
		let nextStep = firstStep in
		let (word,stateRevStack,symbolRevStack,lr0Table,valid) = nextStep in
		let valid = ref valid in
		let word = ref word in
		let stepList = ref stepList in
		let nextStep = ref nextStep in
			while(!valid && !word != [dollar]) do
				nextStep := acceptWordLR0Step !nextStep cfg;
				let (word2,stateRevStack,symbolRevStack,lr0Table,valid2) = !nextStep in
					stepList := !stepList @ [!nextStep];
					valid := valid2;
					word := word2;
				()
			done;
			!stepList
	
	
	let createLR0AcceptTableHtml word (cfg:t) =
		let table = HTMLTable.fetchTable("acceptTable") in
		let row = HTMLTable.insertRow table in
		let stepList = buildLR0Steps word cfg in
		
		(*Headers *)
		let _ = HTMLTable.insertCell row "Palavra Restante" "Palavra Restante" in
		let _ = HTMLTable.insertCell row "Pilha_Estados" "Pilha_Estados" in
		let _ = HTMLTable.insertCell row "Pilha_Simbolos" "Pilha_Simbolos" in
		let _ = HTMLTable.insertCell row "Valida" "Valida" in
			
		(* Passos do accept *)	
		List.iter ( fun (word2,stateRevStack,symbolRevStack,_,valid) -> 
				let row = HTMLTable.insertRow table in
				let _ = HTMLTable.insertCell row (word2str word2) (word2str word2) in
				let _ = HTMLTable.insertCell row (stringListToString stateRevStack) (stringListToString stateRevStack) in
				let _ = HTMLTable.insertCell row (word2str symbolRevStack) (word2str symbolRevStack) in
				let _ = HTMLTable.insertCell row (stringFromBool valid) (stringFromBool valid) in ();
			) stepList;
			let numberOfCells = 4 in
			let cellWidth = 100 in
			let width = Printf.sprintf "%d" (numberOfCells*cellWidth) in
			HTMLTable.setWidth table width
	
	(*updated accept, joining states and symbols *)		
	
	let buildLR0StepsV2 word2 cfg =
		let word = str2word word2 in
		let open LR0Grammar in
		let firstStep = acceptWordLR0InitV2 word cfg in
		let stepList : truelr0TableStep list = [firstStep] in
		let nextStep = firstStep in
		let (word,revStack,lr0Table,valid) = nextStep in
		let valid = ref valid in
		let word = ref word in
		let stepList = ref stepList in
		let nextStep = ref nextStep in
			while(!valid = "Ongoing") do
				nextStep := acceptWordLR0StepV2 !nextStep cfg;
				let (word2,revStack,lr0Table,valid2) = !nextStep in
					stepList := !stepList @ [!nextStep];
					valid := valid2;
					word := word2;
				()
			done;
			!stepList
			
	let rec listEndWith l a n =
	match l with
	| [] -> []
	| x::xs -> if(n=0) then [a] else x::(listEndWith xs a (n-1))
				
	let createLR0AcceptTableHtmlV2 word (cfg:t) =
		let table = HTMLTable.fetchTable("acceptTable") in
		let row = HTMLTable.insertRow table in
		let stepList = buildLR0StepsV2 word cfg in
		
		(* injecting result, this should be deleted and fixed
		let open LR0Grammar in
		let lr0Table = LR0Grammar.getLR0Table cfg in 
		
		
		let finalStep : truelr0TableStep = ([],[symb2str cfg.initial],lr0Table,true) in
		
		let finalStepList = listEndWith stepList finalStep ((List.length stepList) -2) in
		end of injection *)
		
		(*Headers *)
		let _ = HTMLTable.insertCell row "Palavra" "Palavra" in
		let _ = HTMLTable.insertCell row "Pilha" "Pilha" in
		let _ = HTMLTable.insertCell row "Ação" "Ação" in
		(* Passos do accept *)	
		List.iter ( fun (word2,revStack,lr0Table,valid) -> 
				let row = HTMLTable.insertRow table in
				let _ = HTMLTable.insertCell row (word2str word2) (word2str word2) in
				let _ = HTMLTable.insertCell row (stringListToString (rev revStack)) (stringListToString (rev revStack)) in
				(*	
				let newCell = row##insertCell (-1) in
					newCell##.innerHTML := Js.string (word2str symbolRevStack);
					ignore (newCell##.classList##add (Js.string "monospaceClass"));
				*)	
					(*
					newCell##.innerHTML := Js.string (boolFromString valid);
					*)
				let actionString = parserPredictLR0Action word2 lr0Table revStack cfg in
				let _ = HTMLTable.insertCell row actionString actionString in ();
			) stepList;
			
		let numberOfCells = 3 in
		let cellWidth = 100 in
		let numberOfSymbols = String.length word in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth*numberOfSymbols) in
			HTMLTable.setWidth table width
			
		
			
	let buildSLR1StepsV2 word2 cfg =
		let word = str2word word2 in
		let open SLR1Grammar in
		let firstStep = acceptWordSLR1InitV2 word cfg in
		let stepList : trueslr1TableStep list = [firstStep] in
		let nextStep = firstStep in
		let (word,revStack,slr1Table,valid) = nextStep in
		let revStack = ref revStack in
		let valid = ref valid in
		let word = ref word in
		let stepList = ref stepList in
		let nextStep = ref nextStep in
			while(!valid = "Ongoing") do (*!word != [dollar] *)
				nextStep := acceptWordSLR1StepV2 !nextStep cfg;
				let (word2,revStack2,slr1Table,valid2) = !nextStep in
					stepList := !stepList @ [!nextStep];
					valid := valid2;
					word := word2;
					revStack := revStack2;
				()
			done;
			!stepList
	
	
	let createSLR1AcceptTableHtmlV2 word (cfg:t) = 
		let table = HTMLTable.fetchTable("acceptTable") in
		let row = HTMLTable.insertRow table in
		
		let stepList = buildSLR1StepsV2 word cfg in
		
		(* injecting result, this should be deleted and fixed *)
		
		(* end of injection *)
		
		(*Headers *)
		let _ = HTMLTable.insertCell row "Word" "Word" in
		let _ = HTMLTable.insertCell row "Stack" "Stack" in
		let _ = HTMLTable.insertCell row "Action" "Action" in
			
		(* Passos do accept *)	
		List.iter ( fun (word2,revStack,slr1Table,valid) -> 
			let row = HTMLTable.insertRow table in
			let _ = HTMLTable.insertCell row (word2str word2) (word2str word2) in
			let _ = HTMLTable.insertCell row "id" (stringListToString (rev revStack)) in
				(*	
				let newCell = row##insertCell (-1) in
					newCell##.innerHTML := Js.string (word2str symbolRevStack);
					ignore (newCell##.classList##add (Js.string "monospaceClass"));
				*)	
				
				let actionString = parserPredictSLR1Action word2 slr1Table revStack cfg in
					let _ = HTMLTable.insertCell row actionString actionString in ();
					(*
					newCell##.innerHTML := Js.string (boolFromString valid);
					*)
			) stepList;
			
			
		let numberOfCells = 3 in
		let cellWidth = 100 in
		let numberOfSymbols = String.length word in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth*numberOfSymbols) in
			HTMLTable.setWidth table width
				
				
	let buildLR1StepsV2 word2 cfg =
		let word = str2word word2 in
		let open LR1Grammar in
		let firstStep = acceptWordLR1InitV2 word cfg in
		let stepList : truelr1TableStep list = [firstStep] in
		let nextStep = firstStep in
		let (word,revStack,lr1Table,valid) = nextStep in
		let valid = ref valid in
		let word = ref word in
		let stepList = ref stepList in
		let nextStep = ref nextStep in
			while(!valid = "Ongoing") do
				nextStep := acceptWordLR1StepV2 !nextStep cfg;
				let (word2,revStack,lr1Table,valid2) = !nextStep in
					stepList := !stepList @ [!nextStep];
					valid := valid2;
					word := word2;
				()
			done;
			!stepList
	
	let createLR1AcceptTableHtmlV2 word (cfg:t) = 
	
		let table = HTMLTable.fetchTable("acceptTable") in
		let row = HTMLTable.insertRow table in
		let stepList = buildLR1StepsV2 word cfg in
		
		(* injecting result, this should be deleted and fixed *)
		
		(* end of injection *)
		
		(*Headers *)
		let _ = HTMLTable.insertCell row "Word" "Word" in
		let _ = HTMLTable.insertCell row "Stack" "Stack" in
		let _ = HTMLTable.insertCell row "Action" "Action" in
			
			
		(* Passos do accept *)	
		List.iter ( fun (word2,revStack,lr1Table,valid) -> 
			let row = HTMLTable.insertRow table in
			let _ = HTMLTable.insertCell row (word2str word2) (word2str word2) in
			let _ = HTMLTable.insertCell row (stringListToString (rev revStack)) (stringListToString (rev revStack)) in
				
				(*	
				let newCell = row##insertCell (-1) in
					newCell##.innerHTML := Js.string (word2str symbolRevStack);
					ignore (newCell##.classList##add (Js.string "monospaceClass"));
				*)	
				
				let actionString = parserPredictLR1Action word2 lr1Table revStack cfg in
					let _ = HTMLTable.insertCell row actionString actionString in ();
					(*
					newCell##.innerHTML := Js.string (boolFromString valid);
					*)
			) stepList;
			
			
		let numberOfCells = 3 in
		let cellWidth = 100 in
		let numberOfSymbols = String.length word in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth*numberOfSymbols) in
			HTMLTable.setWidth table width
	
	
	
	let buildLALR1StepsV2 word2 cfg =
		let word = str2word word2 in
		let open LALR1Grammar in
		let firstStep = acceptWordLALR1InitV2 word cfg in
		let stepList = [firstStep] in
		let nextStep = firstStep in
		let (word,revStack,lr1Table,valid) = nextStep in
		let valid = ref valid in
		let word = ref word in
		let stepList = ref stepList in
		let nextStep = ref nextStep in
			while(!valid = "Ongoing") do
				nextStep := acceptWordLALR1StepV2 !nextStep cfg;
				let (word2,revStack,lr1Table,valid2) = !nextStep in
					stepList := !stepList @ [!nextStep];
					valid := valid2;
					word := word2;
				()
			done;
			!stepList
	
	let createLALR1AcceptTableHtmlV2 word (cfg:t) = 
	
		let table = HTMLTable.fetchTable("acceptTable") in
		let row = HTMLTable.insertRow table in
		let stepList = buildLALR1StepsV2 word cfg in
		
		(* injecting result, this should be deleted and fixed *)
		
		(* end of injection *)
		
		(*Headers *)
		let _ = HTMLTable.insertCell row "Word" "Word" in
		let _ = HTMLTable.insertCell row "Stack" "Stack" in
		let _ = HTMLTable.insertCell row "Action" "Action" in
			
		(* Passos do accept *)	
		List.iter ( fun (word2,revStack,lr1Table,valid) -> 
			let row = HTMLTable.insertRow table in
			let _ = HTMLTable.insertCell row (word2str word2) (word2str word2) in
			let _ = HTMLTable.insertCell row (stringListToString (rev revStack)) (stringListToString (rev revStack)) in
				
				(*	
				let newCell = row##insertCell (-1) in
					newCell##.innerHTML := Js.string (word2str symbolRevStack);
					ignore (newCell##.classList##add (Js.string "monospaceClass"));
				*)	
					let actionString = parserPredictLR1Action word2 lr1Table revStack cfg in
					let _ = HTMLTable.insertCell row actionString actionString in ();

					(*
					newCell##.innerHTML := Js.string (boolFromString valid);
					*)
		
			) stepList;
			
			
		let numberOfCells = 3 in
		let cellWidth = 100 in
		let numberOfSymbols = String.length word in
		let width = Printf.sprintf "%d" (numberOfCells*cellWidth*numberOfSymbols) in
			HTMLTable.setWidth table width
	
							
	(* Para futuras implementações remover a função boolFromString *)
			
	class model (arg: t Arg.alternatives) =
		object(self) inherit ContextFreeGrammarLL1View.model arg as super
		

		method buildCyLR0Diagram cy = 
			let lr0Diagram = LR0Grammar.getLR0DiagramId super#representation in
			
				inputLR0NodesV2 cy lr0Diagram; 
				inputLR0Edges cy lr0Diagram
				
		method buildCySLR1Diagram cy = 
			let slr1Diagram = LR0Grammar.getLR0DiagramId super#representation in
			
				inputLR0NodesV2 cy slr1Diagram; 
				inputLR0Edges cy slr1Diagram
				
		method buildCyLR1Diagram cy = 
			let lr1Diagram = LR1Grammar.getLR1DiagramId super#representation in
			
				inputLR1NodesV2 cy lr1Diagram; 
				inputLR1Edges cy lr1Diagram
				
		method buildCyLALR1Diagram cy = 
				let lalr1Diagram = LALR1Grammar.getLALR1DiagramId super#representation  in
				inputLR1NodesV2 cy lalr1Diagram; 
				inputLR1Edges cy lalr1Diagram
				
		method createLR0ParsingTableHtml =
				(*createLR0ParsingTableHtml super#representation old version*)
				createLR0ParsingTableHtmlExt super#representation
				
		method createSLR1ParsingTableHtml =
				createSLR1ParsingTableHtml super#representation
		
		method createLR1ParsingTableHtml =
				createLR1ParsingTableHtml super#representation
			
		method createLALR1ParsingTableHtml =
				createLALR1ParsingTableHtml super#representation
				
		method createLR0AcceptTableHtml word =
				createLR0AcceptTableHtml word super#representation
				
		method createLR0AcceptTableHtmlV2 word =
				createLR0AcceptTableHtmlV2 word super#representation
				
		method createSLR1AcceptTableHtmlV2 word =
				createSLR1AcceptTableHtmlV2 word super#representation
		
		method createLR1AcceptTableHtmlV2 word =
				createLR1AcceptTableHtmlV2 word super#representation
			
		method createLALR1AcceptTableHtmlV2 word =
				createLALR1AcceptTableHtmlV2 word super#representation
			
	end
end	
