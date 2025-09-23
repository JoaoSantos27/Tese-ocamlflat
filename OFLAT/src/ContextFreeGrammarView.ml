open OCamlFlat
open BasicTypes
open JS
open Js_of_ocaml
open Js.Opt
open Lang
open ContextFreeGrammarBasicView
open ContextFreeGrammarLL1View
open ContextFreeGrammarLRView

module ContextFreeGrammarView =
struct
	open ContextFreeGrammarBasic

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

  let doc = Dom_html.document
	
	class model (arg: t Arg.alternatives) =
		object(self) inherit ContextFreeGrammarLRView.model arg as super

		val mutable accepted : bool = false
    val mutable visitedConfigs : int = 0
    val mutable exactResult : bool = false
    val mutable acceptTime : float = 0.0
    val mutable bestPath: ContextFreeGrammarBasic.path = []
    val mutable tree_rules: (word * rule list * int list) list = [] (*PEDRO CARLOS *)

		(*JP*)
      method staticGenerate len =
        let res = self#generate len in
        let (exact, configVisited, time) = Model.stats() in
        exactResult <- exact;
        acceptTime <- time;
        visitedConfigs <- configVisited;
        res
        
      method staticAccept word =
        let acc = self#accept word in
        let (exact, configVisited, time) = Model.stats() in
          JS.log("set initial step");
        self#setConfigsAndBestPath2 acc exact time configVisited;


      method staticAcceptFull word =
        let (acc, bestPath, trail) = self#acceptFull word in
        let (exact, configVisited, time) = Model.stats() in
          JS.log("set initial step");
        self#setTreeStruct bestPath;
        self#setConfigsAndBestPath trail bestPath acc exact time configVisited;

      method returnStats =
        (accepted, visitedConfigs, exactResult, acceptTime)

      (* method displayTrace =
        let makePath () : string list list =
          let headers = ["Trace"; "Current Symbols"; "Word To Match"] in
            headers :: HTMLTable.makeCFGPath bestPath 0
          in
          if not (HTMLTable.tableExists "pathTable") then (
            let contents = makePath () in
              HTMLTable.buildTable contents "pathTable" "cy2";
              let tab = HTMLTable.fetchTable "pathTable" in
              let lastRowIndex = List.length bestPath in
              let rec paintCells step =
                match step with
                | n when n < 4 -> 
                  if accepted then HTMLTable.paint tab lastRowIndex step "mediumseagreen"
                  else (HTMLTable.paint tab lastRowIndex step "crimson");
                  paintCells (step+1);
                | n when n = 4 -> ()
              in
              paintCells 1; (*ignore step column*)
              HTMLTable.changeDisplay tab ""
              )
          else () *)

			(* PEDRO CARLOS VER! tem representacao abstrata? falta parseTree? o que faz isto *)
          method displayTrace =
          let makePath () : string list list =
            let headers = ["Trace"; "Current Symbols"; "Word To Match"] in
            headers :: HTMLTable.makeGRPath bestPath 0
          in
          let button = Dom_html.createButton doc in
          button##.textContent := Js.some (Js.string "Toggle View");
          button##.onclick := Dom_html.handler (fun _ ->
            let table = Dom_html.getElementById "pathTable" in
            let cy3 = Dom_html.getElementById "cy3" in
            let treeDiv = Dom_html.getElementById "treeDiv" in
            let tableStyle = Dom_html.window##getComputedStyle table in
            if (Js.string (Js.to_string tableStyle##.display) = Js.string "none") then (
                table##.style##.display := Js.string "";
                cy3##.style##.display := Js.string "none";
                cy3##.style##.height := Js.string "0";
                treeDiv##.style##.display := Js.string "none"
              ) else (
                table##.style##.display := Js.string "none";
                cy3##.style##.display := Js.string "";
                cy3##.style##.height := Js.string "60vh";
                treeDiv##.style##.display := Js.string ""
              );
            Js._true
          );
          let cy2 = Dom_html.getElementById "cy2" in
          Dom.appendChild cy2 button;

          (* Create tree div and append to cy2 *)
          let treeDiv = Dom_html.createDiv doc in
          treeDiv##.id := Js.string "treeDiv";
          treeDiv##.style##.display := Js.string "none";
          Dom.appendChild cy2 treeDiv;

          let cy3 = Dom_html.createDiv doc in
          cy3##.id := Js.string "cy3";
          cy3##.style##.display := Js.string "";
          Dom.appendChild cy2 cy3;
    
          (* Create fitToBox button and append to treeDiv and cy3 *)
          let cy3_cy = Cytoscape.initGRCy3 "cy3" in
          let fitToBoxButton = Dom_html.createButton doc in
          fitToBoxButton##.textContent := Js.some (Js.string "Fit To Box");
          fitToBoxButton##.onclick := Dom_html.handler (fun _ ->
            (* Assuming centerGraph is a function that centers the graph in cy3 *)
            Cytoscape.centerGraph cy3_cy;
            Js._true
          );
          Dom.appendChild treeDiv fitToBoxButton;
    
          if not (HTMLTable.tableExists "pathTable") then (
            let contents = makePath () in
            HTMLTable.buildTable contents "pathTable" "cy2";
            let tab = HTMLTable.fetchTable "pathTable" in
            let lastRowIndex = List.length bestPath in
            let rec paintCells step =
              match step with
              | n when n < 4 -> 
                if accepted then HTMLTable.paint tab lastRowIndex step "mediumseagreen"
                else HTMLTable.paint tab lastRowIndex step "crimson";
                paintCells (step + 1)
              | n when n = 4 -> ()
            in
            paintCells 1;
            HTMLTable.changeDisplay tab "";
            self#buildTree cy3_cy;
            ()
          ) else ()

      method private setTreeStruct bestPath = 
        tree_rules <- self#find_applied_rules bestPath

        method buildTree cy3 =

        (* --- Helper Functions --- *)
        let node_counter = ref 0 in
        let generate_node_id () = incr node_counter; Printf.sprintf "N%d" !node_counter in
        let add_cy_node node_label =
          let node_id = generate_node_id () in
          Cytoscape.add_node cy3 node_id "root" "" node_label;
          node_id
        in
        let add_cy_edge parent_id child_id =
          let edge_id = Printf.sprintf "E_%s_to_%s" parent_id child_id in
          Cytoscape.addEdge cy3 (parent_id, edge_id, child_id);
          edge_id
        in
        let isTerminalSymbol (symbol: symbol) : bool =
          let str = symb2str symbol in
          (* Consider refining this check based on your exact grammar conventions *)
          not (str >= "A" && str <= "Z") (* Basic check for uppercase non-terminals *)
          && not (String.length str > 1 && String.get str 0 = '<' && String.get str (String.length str - 1) = '>') (* Basic check for <...> non-terminals *)
        in
        (* Ensure 'epsilon' symbol is defined in the class/module scope *)
        let epsilon_str = symb2str epsilon in (* Get epsilon string representation *)


        (* --- Tree Building Logic --- *)

        (* 1. Initialization *)
        (* Get root symbol string from the first step's sentential form *)
        let (root_sf, _, _) = List.nth tree_rules 0 in
        let root_label_str = word2str root_sf in 
        let root_id = add_cy_node root_label_str in
        (* Frontier: List of (cytoscape_node_id, non_terminal_label) *)
        let frontier : (string * string) list ref = ref [(root_id, root_label_str)] in

        (* 2. Process Each Derivation Step *)
        tree_rules |> List.iter (fun (_sf, rules, _positions) ->
          let num_rules = List.length rules in
          if num_rules >= 1 then (* Handle steps with one or more rules *)
            begin
              (* --- Identify Nodes to Expand (Sequential Matching) --- *)
              (* Stores tuples of: (parent_node_id, parent_node_label, rule_to_apply) *)
              let nodes_to_expand_this_step = ref [] in
              let remaining_rules = ref rules in (* Rules for this step to be matched *)
              let current_frontier_copy = !frontier in (* Iterate over a snapshot of the frontier *)

              (* Iterate through the frontier to find nodes matching the heads of remaining rules *)
              current_frontier_copy |> List.iter (fun (node_id, node_label) ->
                match !remaining_rules with
                | current_rule :: rest_rules ->
                    (* Check if the current frontier node matches the head of the next rule *)
                    if symb2str current_rule.head = node_label then
                      begin
                        (* Match found! Record it and consume the rule *)
                        nodes_to_expand_this_step := !nodes_to_expand_this_step @ [(node_id, node_label, current_rule)];
                        remaining_rules := rest_rules;
                      end
                    (* else: This frontier node doesn't match the head of the current rule in sequence. *)
                    (* It might be matched by a later rule if rules/frontier get out of sync, *)
                    (* or it might not be expanded in this step. Simple sequential match proceeds. *)
                | [] -> () (* No more rules left in this step to match *)
              );

              (* Optional: Warning if not all rules were matched (indicates potential issue) *)
              if !remaining_rules <> [] then
                Printf.eprintf "Warning: Not all rules in the step were matched sequentially to frontier nodes. Step rules: %d, Unmatched: %d\n" num_rules (List.length !remaining_rules);


              (* --- Expand All Identified Nodes & Collect New Frontier Nodes --- *)
              (* Use a Hashtbl to map expanded parent_id -> list of its new non-terminal children (id, label) *)
              let expansion_results : (string, (string * string) list) Hashtbl.t = Hashtbl.create (List.length !nodes_to_expand_this_step) in

              !nodes_to_expand_this_step |> List.iter (fun (parent_id, _parent_label, rule) ->
                  (* This list collects new non-terminal children ONLY for this specific parent_id *)
                  let new_children_for_this_parent = ref [] in
                  (* Expand this parent according to its matched rule *)
                  rule.body |> List.iter (fun symbol ->
                    let symbol_str = symb2str symbol in
                    let is_epsilon = (symbol_str = epsilon_str) in
                    let child_label = if is_epsilon then "ε" else symbol_str in

                    let child_id = add_cy_node child_label in
                    ignore (add_cy_edge parent_id child_id);

                    (* Check if the child is a non-terminal and not epsilon *)
                    let is_non_terminal = not is_epsilon && not (isTerminalSymbol symbol) in
                    if is_non_terminal then
                      (* Add new non-terminal child to the list for *this parent* *)
                      new_children_for_this_parent := !new_children_for_this_parent @ [(child_id, child_label)]
                  );
                  (* Store the collected children for this parent_id in the Hashtbl *)
                  Hashtbl.add expansion_results parent_id !new_children_for_this_parent;
              );

              (* --- Update the Main Frontier --- *)
              (* Build the next frontier list by replacing expanded nodes with their children *)
              let next_frontier = ref [] in
              !frontier |> List.iter (fun (id, label) ->
                  match Hashtbl.find_opt expansion_results id with
                  | Some new_children_list ->
                      (* This node was expanded, replace it with its new non-terminal children *)
                      next_frontier := !next_frontier @ new_children_list
                  | None ->
                      (* This node was not expanded in this step, keep it *)
                      next_frontier := !next_frontier @ [(id, label)]
              );
              frontier := !next_frontier; (* Update the main frontier reference *)

            end (* End of block for num_rules >= 1 *)
          (* else num_rules = 0: Implicitly do nothing for this step *)

        ); (* End of iterating through tree_rules *)

        (* 6. Apply Tree Layout *)
        let treeLayout = object%js
          val name = Js.string "dagre"
          val rankDir = Js.string "TB"
          (* Add other dagre options if needed *)
        end in
        try
           (* Adapt layout call based on your Cytoscape binding *)
           Cytoscape.run_layout (cy3##layout treeLayout);
        with ex -> Printf.eprintf "Error applying layout: %s\n" (Printexc.to_string ex)

      
              
      method private setConfigsAndBestPath trail bestPathCFG acc exact time configVisited=
        JS.log("METI OS STEPS");
        JS.log(bestPathCFG);
        bestPath <- bestPathCFG;
        accepted <- acc;
        exactResult <- exact;
        acceptTime <- time;
        visitedConfigs <- configVisited

        method private setConfigsAndBestPath2 acc exact time configVisited=
          accepted <- acc;
          exactResult <- exact;
          acceptTime <- time;
          visitedConfigs <- configVisited    
      

	end

	let adjust (cfg: ContextFreeGrammarBasic.model): model =
		let r = cfg#representation in
			new model (Representation r)

	

end	
