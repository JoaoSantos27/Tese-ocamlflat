open OCamlFlat
open BasicTypes
open JS
open Js_of_ocaml
open Js.Opt
open Lang
open StateVariables

module GrammarView =
struct
  open Grammar

  type t = Grammar.t
  type grTree = Grammar.grTree
  type acceptStep = {
    acceptedString: string;
    accepted: bool option;
    tree: grTree;
    cyId: string option
  }

  (**HTML entities**)
  let htmlEpsilon = "ε" (*"&epsilon;"*) (** Define a representação HTML para épsilon. *)
  let htmlArrow = " → " (*" &rarr; "*) (** Define a representação HTML para a seta de produção. *)
  let htmlDollar = "$" (** Define a representação HTML para o símbolo de fim de entrada (geralmente usado em análise). *)

  let productionsTableId() = "grProductionsTable" (** Retorna o ID para a tabela principal de produções da gramática. *)
  let productionsTableId2() = "grProductionsTable2" (** Retorna o ID para uma segunda tabela de produções da gramática. *)
  let monospaceClass = "monospaceClass" (** Define o nome da classe CSS para texto monoespaçado. *)
  let doc = Dom_html.document (** Atalho para o objeto 'document' do DOM HTML. *)
  

  (** Cria um elemento de tabela HTML com um ID específico. *)
  let createHtmlTable id =
    let t = Dom_html.createTable doc in
    t##.id := Js.string id;
    t

  (**
   * Retorna um conjunto de corpos de regras para uma dada cabeça 'h' numa representação de gramática 'rep'.
   * Se a cabeça for o símbolo inicial e não tiver regras, retorna um conjunto contendo épsilon.
   *)
  let bodiesOfHead h rep =
    let rls = Set.filter (fun r -> r.head = h) rep.rules in
    if Set.isEmpty rls && h = [rep.initial] then
      Set.singleton [epsilon]
    else
      Set.map (fun r -> r.body) rls


  (**
   * Cria e preenche uma tabela HTML com as produções da gramática.
   * @param rep A representação da gramática.
   * @param id O ID a ser atribuído à tabela HTML; se vazio, usa o ID padrão.
   *)
  let createGrammarTableHtml (rep: t) id =
    (** Preenche uma linha da tabela com os corpos das regras, separados por " | ". *)
    let rec fillRule row bodies =
      if bodies = Set.empty then ()
      else let (x, xs) = Set.cut bodies in
        let span = Dom_html.createSpan doc in
          span##.textContent := Js.some (Js.string (if x = [] then StateVariables.returnEmpty() else word2str x));
          span##.classList##add (Js.string monospaceClass);
          HTMLTable.appendChildtoRow row span;
        if xs = Set.empty then fillRule row xs
        else (
          let orSpan = Dom_html.createSpan doc in
            orSpan##.innerHTML := Js.string " | ";
            orSpan##.classList##add (Js.string monospaceClass);
            HTMLTable.appendChildtoRow row orSpan;
            fillRule row xs
        )
    in

    (** Preenche uma linha da tabela para uma dada cabeça e os seus corpos de regra. *)
    let fillRow table head bodies =
      let open ViewUtil in
      let headStr = word2str head in (* Converte a cabeça (lista de símbolos) para uma string *)
      let escapedHeadStr =
        headStr
        |> ViewUtil.replace_str '&' "&amp;"
        |> ViewUtil.replace_str '<' "&lt;"
        |> ViewUtil.replace_str '>' "&gt;"
      in
      let row = HTMLTable.insertRow table in
      let _ = HTMLTable.insertCell row (escapedHeadStr ^ "Prods") (escapedHeadStr) in
      let _ = HTMLTable.insertCell row htmlArrow htmlArrow in
        fillRule row bodies
    in
    let nonInitialRules = Set.map (fun r -> r.head) rep.rules in
    let nonInitialRules = Set.filter (fun h -> List.hd h <> rep.initial) nonInitialRules in
    let id = if id = "" then productionsTableId() else id in
      let table = HTMLTable.fetchTable id in
      fillRow table [rep.initial] (bodiesOfHead [rep.initial] rep);
      Set.iter (fun head -> fillRow table head (bodiesOfHead head rep)) nonInitialRules

    (**
     * Divide uma lista de símbolos 'symbols' em duas partes: antes e depois da ocorrência da 'head' (cabeça da regra),
     * começando a procurar a partir de 'start_index'.
     * Retorna (símbolos_antes, símbolos_depois). Se a 'head' não for encontrada ou se 'head' for igual a 'symbols',
     * pode retornar (symbols, []) ou ([], []) dependendo das condições.
     *)
    let split_around_rule_head symbols head start_index =
      let head_len = List.length head in
      let symbols_len = List.length symbols in
    
      if head_len = symbols_len then (* Se a cabeça tem o mesmo tamanho que os símbolos, não há o que dividir em torno dela. *)
        ([], [])
      else
        (* Verifica se 'sub' é uma subsequência de 'lst' começando do início de ambas. *)
        let rec find_subsequence lst sub =
          match lst, sub with
          | _, [] -> true (* Subsequência vazia é sempre encontrada. *)
          | [], _ -> false (* Lista principal vazia, mas subsequência não. *)
          | h::t, s::sub_t -> h = s && find_subsequence t sub_t (* Compara cabeças e continua recursivamente. *)
        in
    
        (* Divide 'lst' na posição 'pos'. Retorna (elementos_antes_de_pos, elementos_a_partir_de_pos). *)
        let rec split_at pos lst =
          if pos <= 0 then ([], lst) (* Posição de divisão atingida ou ultrapassada. *)
          else match lst with
            | [] -> ([], []) (* Lista vazia. *)
            | h::t -> 
                let (before, after) = split_at (pos - 1) t in (* Chamada recursiva. *)
                (h::before, after) (* Adiciona elemento atual à parte 'antes'. *)
        in
    
        (* Tenta encontrar 'head' em 'symbols' a partir de 'pos'. *)
        let rec try_positions pos =
          if pos > symbols_len - head_len then (* Se não há espaço suficiente para 'head'. *)
            (symbols, []) (* Retorna todos os símbolos como 'antes' e nada como 'depois'. Considerar se este é o comportamento ideal. *)
          else
            let (_, rest) = split_at pos symbols in (* Pega a parte dos símbolos a partir de 'pos'. *)
            if find_subsequence rest head then (* Se 'head' é encontrada no 'rest'. *)
              let (before_match, after_head_temp) = split_at pos symbols in (* Divide os símbolos originais em 'pos'. *)
              let (_, after_match) = split_at head_len after_head_temp in (* Remove 'head' de 'after_head_temp' para obter 'after_match'. *)
              (before_match, after_match)
            else
              try_positions (pos + 1) (* Tenta a próxima posição. *)
        in
        try_positions start_index (* Inicia a busca a partir de 'start_index'. *)
      
  (** Classe que modela uma gramática, estendendo Grammar.model com funcionalidades de visualização e análise. *)
  class model (arg: t Arg.alternatives) =
    object(self) inherit Grammar.model arg as super

    val mutable accepted : bool = false (** Indica se a última palavra testada foi aceite. *)
    val mutable visitedConfigs : int = 0 (** Número de configurações visitadas durante a última análise. *)
    val mutable exactResult : bool = false (** Indica se o resultado da última análise é exato (não limitado por tempo/passos). *)
    val mutable acceptTime : float = 0.0 (** Tempo gasto na última operação de aceitação/geração. *)
    val mutable bestPath: Grammar.path = [] (** O melhor caminho de derivação encontrado para a última palavra aceite. *)
    val mutable tree_rules: ((symbol list * int) * (rule option * int)) list = [] (** Regras aplicadas e os seus índices, usadas para construir a árvore de derivação. *)



    method createGrammarTableHtml id =
      createGrammarTableHtml self#representation id

    method isContextFreeGrammar : bool =
      Grammar.isContextFreeGrammar self#representation

    method isContextSensitiveGrammar : bool =
      Grammar.isContextSensitiveGrammar self#representation

    method isMonotonicGrammar : bool =
      Grammar.isMonotonicGrammar self#representation

    method isLinearGrammar : bool =
      Grammar.isLinearGrammar self#representation

    method isLeftLinearGrammar : bool =
      Grammar.isLeftLinearGrammar self#representation

    method isRightLinearGrammar : bool =
      Grammar.isRightLinearGrammar self#representation

    method isUnrestrictedGrammar : bool =
      Grammar.isUnrestrictedGrammar self#representation

    (** Encontra as regras aplicadas num dado caminho de derivação. *)
    method find_applied_rules bestPath =
      Grammar.find_applied_rules self#representation bestPath

    method isClean : bool =
      Grammar.isClean self#representation

    method clean : model =
      let cleanedGrammar = Grammar.clean self#representation in
      new model (Arg.Representation cleanedGrammar)

    method kuroda : model =
      let kurodaGrammar = Grammar.kurodaNormalForm self#representation in
      new model (Arg.Representation kurodaGrammar)

    method penttonen : model =
      let penttonenGrammar = Grammar.penttonenNormalForm self#representation in
      new model (Arg.Representation penttonenGrammar)
      
    method nonContractingToCSG : model =
      let csgGrammar = Grammar.nonContractingToCSG self#representation in
      new model (Arg.Representation csgGrammar)

    method allRulesProductive : bool =
      Grammar.allRulesProductive self#representation

    method allRulesAccessible : bool =
      Grammar.allRulesAccessible self#representation


    method staticGenerate len =
      let res = Grammar.generate self#representation len in
      let (exact, configVisited, time) = Model.stats() in
      exactResult <- exact;
      acceptTime <- time;
      visitedConfigs <- configVisited;
      res
      

    method staticAccept word =
      let acc = Grammar.accept self#representation word in
      let (exact, configVisited, time) = Model.stats() in
      self#setConfigsAndBestPath2 acc exact time configVisited;


    method staticAcceptFull word =
      let (acc, bestPath, trail) = Grammar.acceptFull self#representation word in
      let (exact, configVisited, time) = Model.stats() in
      self#setTreeStruct bestPath;
      self#setConfigsAndBestPath trail bestPath acc exact time configVisited; 

    method returnStats =
      (accepted, visitedConfigs, exactResult, acceptTime)

    (**
     * Configura e mostra a visualização do trace e da árvore de derivação.
     * Cria uma tabela para o trace e um grafo Cytoscape para a árvore.
     * Inclui um botão para alternar entre a visualização da tabela e da árvore.
     *)
    method displayTrace =
          let makePath () : string list list = (* Cria os dados para a tabela de traço. *)
            let headers = ["Trace"; "Current Symbols"; "Word To Match"] in
            headers :: HTMLTable.makeGRPath bestPath 0
          in
          let button = Dom_html.createButton doc in (* Botão para alternar visualização. *)
          button##.textContent := Js.some (Js.string (Lang.i18nToggleView()));
          button##.onclick := Dom_html.handler (fun _ -> (* Lógica para alternar a visibilidade. *)
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
                treeDiv##.style##.display := Js.string "";
              );
            Js._true
          );
          let cy2 = Dom_html.getElementById "cy2" in (* Container para a visualização. *)
          Dom.appendChild cy2 button;

          let treeDiv = Dom_html.createDiv doc in (* Div para a árvore Cytoscape. *)
          treeDiv##.id := Js.string "treeDiv";
          treeDiv##.style##.display := Js.string "none";
          Dom.appendChild cy2 treeDiv;

          let cy3 = Dom_html.createDiv doc in (* Div que conterá o grafo Cytoscape (visível por padrão). *)
          cy3##.id := Js.string "cy3";
          cy3##.style##.display := Js.string ""; (* Inicialmente visível *)
          Dom.appendChild cy2 cy3;
    
          let cy3_cy = Cytoscape.initGRCy3 "cy3" in (* Inicializa o grafo Cytoscape. *)
          let fitToBoxButton = Dom_html.createButton doc in (* Botão para ajustar o grafo. *)

          fitToBoxButton##.textContent := Js.some (Js.string (Lang.i18nFitGraph()));
          fitToBoxButton##.onclick := Dom_html.handler (fun _ ->
            Cytoscape.centerGraph cy3_cy;
            Js._true
          );
          Dom.appendChild treeDiv fitToBoxButton; (* Adiciona botão de ajuste ao div da árvore. *)
          if self#isContextFreeGrammar then ( (* Adiciona nota sobre representação geral se for GLC. *)
            let p = Dom_html.createP doc in
            p##.textContent := Js.some (Js.string (Lang.i18nGeneralRepresentation()));
            p##.style##.margin := Js.string "0px";
            p##.style##.padding := Js.string "0px 20px";
            Dom.appendChild treeDiv p
          );
    
          if not (HTMLTable.tableExists "pathTable") then ( (* Se a tabela de traço não existe, cria-a. *)
            let contents = makePath () in
            HTMLTable.buildTable contents "pathTable" "cy2";
            let tab = HTMLTable.fetchTable "pathTable" in
            let lastRowIndex = List.length bestPath in
            let rec paintCells step = (* Pinta a última linha da tabela para indicar aceitação/rejeição. *)
              match step with
              | n when n < 4 -> 
                if accepted then HTMLTable.paint tab lastRowIndex step "mediumseagreen"
                else HTMLTable.paint tab lastRowIndex step "crimson";
                paintCells (step + 1)
              | n when n = 4 -> ()
            in
            paintCells 1;
            HTMLTable.changeDisplay tab ""; (* Torna a tabela visível. *)
            self#buildTree cy3_cy bestPath; (* Constrói a árvore de derivação. *)
            ()
          ) else () (* Se a tabela já existe, não faz nada. *)
    
      (**
       * Constrói a árvore de derivação no grafo Cytoscape 'cy3' com base no 'bestPath'.
       * Utiliza 'tree_rules' (preenchido por setTreeStruct) para identificar as regras aplicadas.
       * Adiciona nós para símbolos e nós compostos para regras.
       * Tenta simplificar a árvore removendo nós e arestas redundantes.
       *)
      method buildTree cy3 (bestPath: Grammar.path) =
        
        let edges = ref [] in (* Lista para armazenar as arestas do grafo. *)

        (** Adiciona um nó épsilon ao grafo Cytoscape. *)
        let fill_epsilon current_level index offset=
          let nodeLabel = "ε" in
          let nodeId = Printf.sprintf "L%d_N%d_W%s_TNORMAL" current_level index nodeLabel in
          let x = ((index) * 50) - offset - 50 in 
          let y = current_level * 100 in 
          Cytoscape.add_node cy3 nodeId ~pos:(x, y) "root" "" nodeLabel;
        in
        
        (** Adiciona um nó auxiliar (normal) para um símbolo ao grafo Cytoscape. *)
        let addNodeAux level index word path_len =
          let nodeLabel = word in
          let nodeId = Printf.sprintf "L%d_N%d_W%s_TNORMAL" level index nodeLabel in
          let offset = (path_len * 100) / 2 in (* Usado para centralizar o grafo. *)
          let x = (index * 100) - offset in  
          let y = level * 100 in 
          Cytoscape.add_node cy3 nodeId ~pos:(x, y) "root" "" nodeLabel;
        in
        
        (** Adiciona um nó composto para uma regra (cabeça como pai, corpo como filhos) ao grafo Cytoscape. 
            Retorna o próximo índice disponível após adicionar os nós do corpo da regra. *)
        let addCompoundNodeForRule level index rule path_len =
          let labelParent = word2str rule.head in
          let parentId = Printf.sprintf "L%d_N%d_W%s_TSUPER" level index labelParent in (* Nó "SUPER" para a cabeça. *)
          let offset = (path_len * 100) / 2 in
          let x = (index * 100) - offset in 
          let y = level * 100 in 
          let pos = object%js val x = x val y = y end in
          Cytoscape.addCompoundNode cy3 parentId ~pos (Some "");
        
          (* Adiciona nós filhos para cada símbolo na cabeça da regra (dentro do nó composto). *)
          List.iteri (fun sym_idx symbol ->
            let childLabel = symb2str symbol in
            let childId = Printf.sprintf "L%d_N%d_W%s_TNORMAL" level (index + sym_idx) childLabel in
            let x = ((index + sym_idx) * 100) - offset in 
            let y = level * 100 in 
            let pos = object%js val x = x val y = y end in
            Cytoscape.addChildNode cy3 parentId childId (Some childLabel) ~pos;
          ) rule.head;
        
          (* Adiciona arestas da cabeça para os símbolos do corpo (se não for épsilon). *)
          if rule.body <> [epsilon] then begin
            List.iteri (fun sym_idx symbol ->
              let childLabel = symb2str symbol in
              edges := ((level, index, labelParent, "SUPER"), ((level + 1), (index + sym_idx), childLabel, "NORMAL")) :: !edges;
            ) rule.body;
            index + List.length rule.body (* Retorna o novo índice inicial para o próximo elemento no mesmo nível. *)
          end else (* Se o corpo é épsilon. *)
            let nodeLabel = "ε" in
            edges := ((level, index, labelParent, "SUPER"), ((level + 1), (index), nodeLabel, "NORMAL") ):: !edges;
            fill_epsilon (level + 1) index offset; (* Adiciona nó épsilon. *)
            index (* Índice não muda significativamente para épsilon em termos de largura. *)
        in

        let levels = List.length bestPath in
        (* Itera sobre as regras aplicadas (obtidas de find_applied_rules) para construir os nós e arestas. *)
        tree_rules |> List.iteri (fun i ((path_elem, level), (rule_opt, index_in_path)) ->
            let path_len = List.length path_elem in (* Comprimento da forma sentencial atual. *)
            if i < levels then ( (* Processa apenas até ao número de níveis do caminho. *)
              match rule_opt with
              | None -> 
                failwith "Rule not found for tree construction" 
                
              | Some rule ->
                  if index_in_path = -1 then (* Se index_in_path é -1, trata como estado inicial/final ou sem aplicação de regra explícita. *)
                      List.iteri (fun idx symb ->
                      addNodeAux level idx (symb2str symb) path_len (* Adiciona cada símbolo como um nó normal. *)
                    ) path_elem
                  else (* Uma regra foi aplicada. *)
                    (* Divide a forma sentencial em volta da cabeça da regra aplicada. *)
                    let (before_match, after_match) = split_around_rule_head path_elem rule.head index_in_path in
                    if ((List.length before_match) + (List.length after_match) + (List.length rule.head)) > List.length path_elem then (
                      JS.log("ERROR: Rule head does not match path_elem, or split is incorrect");
                    );
                    let len_before = List.length before_match in
                    (* Adiciona nós para os símbolos antes da aplicação da regra. *)
                    List.iteri (fun idx symb ->
                      addNodeAux level idx (symb2str symb) path_len;
                      edges := ((level, idx, (symb2str symb), "NORMAL"), ((level + 1), (idx), (symb2str symb), "NORMAL")) :: !edges;
                    ) before_match;
                    (* Adiciona o nó composto para a regra aplicada. *)
                    let index_next_after_rule = addCompoundNodeForRule level len_before rule path_len in
                    let len_head = List.length rule.head in
                    (* Adiciona nós para os símbolos pós aplicação da regra. *)
                    List.iteri (fun idx symb ->
                      addNodeAux level (len_before + len_head + idx) (symb2str symb) path_len;
                      edges := ((level, (len_before + len_head + idx), (symb2str symb), "NORMAL"), ((level + 1), (index_next_after_rule + idx), (symb2str symb), "NORMAL")) :: !edges;
                    ) after_match
            )
        );
        
        (* Simplifica a árvore: remove nós que não mudam e não são usados em expansões futuras. *)
        let max_index_in_path = (* Maior número de símbolos numa forma sentencial. *)
          let lengths = List.map (fun step -> List.length (fst (fst step))) tree_rules @ [List.length (fst (List.hd bestPath))] in
          List.fold_left max 0 lengths
        in
        let getEdges = !edges in               
        let edges_to_delete = ref [] in
        
        (* Itera de baixo para cima, por coluna (índice de símbolo), para encontrar arestas redundantes. *)
        for i = 0 to max_index_in_path do 
          try 
            for j = levels downto 1 do (* Itera pelos níveis da árvore. *)
              List.iter (fun ((l_from, i_from, w_from, t_from), (l_to, i_to, w_to, t_to) as edge) ->
                if l_to = j && i = i_to then begin (* Se a aresta aponta para o nó atual (nível j, índice i). *)
                  if w_from <> w_to || t_from = "SUPER" then begin (* Se o símbolo muda ou é uma expansão de regra (SUPER). *)
                    raise Exit (* Para esta coluna, não há mais simplificações a fazer para cima. *)
                  end else (* Símbolo é o mesmo, não é SUPER, então esta aresta é candidata à remoção. *)
                    edges_to_delete := edge :: !edges_to_delete
                end
              ) getEdges
            done
          with Exit -> (); (* Passa para a próxima coluna (índice i). *)
        done;

        let getEdgesToDelete = !edges_to_delete in
        (* Filtra as arestas, removendo as que foram marcadas para eliminar. *)
        let filtered_edges = List.filter (fun edge -> not (List.mem edge getEdgesToDelete)) getEdges in

        (* Adiciona as arestas filtradas ao grafo Cytoscape. *)
        List.iter (fun ((l_from, i_from, w_from, t_from), (l_to, i_to, w_to, t_to)) -> 
          Cytoscape.addEdge cy3 
            ((Printf.sprintf "L%d_N%d_W%s_T%s" l_from i_from w_from t_from), (* ID do nó de origem. *)
             "", (* ID da aresta, opcional. *)
             (Printf.sprintf "L%d_N%d_W%s_T%s" l_to i_to w_to t_to)) (* ID do nó de destino. *)
        ) filtered_edges;

        (* Remove os nós de destino das arestas que foram eliminadas*)
        List.iter (fun (_, (l_to, i_to, w_to, t_to)) ->
          let to_id = Printf.sprintf "L%d_N%d_W%s_T%s" l_to i_to w_to t_to in
          Cytoscape.removeNode cy3 to_id;
        ) getEdgesToDelete;


    method private setConfigsAndBestPath trail bestPathGR acc exact time configVisited =
      bestPath <- bestPathGR;
      accepted <- acc;
      exactResult <- exact;
      acceptTime <- time;
      visitedConfigs <- configVisited


    method private setConfigsAndBestPath2 acc exact time configVisited =
      accepted <- acc;
      exactResult <- exact;
      acceptTime <- time;
      visitedConfigs <- configVisited    


    method private setTreeStruct bestPath = 
      tree_rules <- self#find_applied_rules bestPath
  end


  let adjust (gr: Grammar.model): model =
    let r = gr#representation in
      new model (Representation r)
end