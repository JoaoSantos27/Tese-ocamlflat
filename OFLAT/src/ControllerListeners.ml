open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open ViewUtil
open FiniteAutomatonView
open PushdownAutomatonView
open RegularExpressionView
open ContextFreeGrammarBasicView
open ContextFreeGrammarView
open ContextFreeGrammarLL1View
open GrammarView
open TuringMachineView
open CompositionView
open Lang
open Listeners
open HtmlPageClient
open StateVariables
open String
open Random
open AutomatonView
open Controller
open FiniteAutomatonController
open PushdownAutomatonController
open RegularExpressionController
open ContextFreeGrammarController
open TuringMachineController
open CompositionController
open ExerciseController
open GrammarController

(* PEDRO CARLOS VER! 80 linhas. Fazem o que?  o put abaixo?... *)
let cfgStr2Model str = (*TODO put in controller*)
  let open ContextFreeGrammarBasic in
  let splitStr = String.split_on_char '\n' str in
  let initialRule = Set.make [List.hd splitStr] in
  let otherRules = Set.make (List.tl splitStr) in
  let initialParsedRule = ContextFreeGrammarBasic.parse initialRule in
  let initial = (Set.nth initialParsedRule 0).head in
  let parsedRules = ContextFreeGrammarBasic.parse (Set.union initialRule otherRules) in
  let variables = Set.add initial (Set.map (fun {head = h; _} -> h) parsedRules) in
  let alphabet = Set.flatMap (fun {head = h; body = b} -> Set.make (List.filter (fun s -> not (Set.belongs s variables)) b) ) parsedRules in
  new ContextFreeGrammarBasic.model (Arg.Representation {
    alphabet = alphabet;
    variables = variables;
    initial = initial;
    rules = parsedRules
  } )


let grStr2Model str = (*TODO put in controller*)
  let isTerminalSymbol (symbol: symbol) : bool =
    let str = symb2str symbol in
    not (("A" <= str && str <= "Z") || String.get str 0 = '<' && String.get str (String.length str - 1) = '>')
  in
  (* Extract all symbols between < and > *)
  let extractInitVars str =
    let rec extract acc i =
      try
        let start = String.index_from str i '<' in
        let stop = String.index_from str (start + 1) '>' in
        let symbol = String.sub str start (stop - start + 1) in
        extract (Set.add (str2symb symbol) acc) (stop + 1)
      with Not_found -> acc
    in
    extract Set.empty 0
  in
  let open Grammar in
  let splitStr = String.split_on_char '\n' str in
  let initialRule = Set.make [List.hd splitStr] in
  let otherRules = Set.make (List.tl splitStr) in
  let initialParsedRule = Grammar.parse initialRule in
  let initial = List.hd (Set.nth initialParsedRule 0).head in
  let parsedRules = Grammar.parse (Set.union initialRule otherRules) in
  let initiVars = extractInitVars str in
  let variables =
    Set.fold_left 
      (fun acc rule -> 
        let headVariables = Set.make (List.filter (fun s -> not (isTerminalSymbol s)) rule.head) in
        let bodyVariables = Set.make (List.filter (fun s -> not (isTerminalSymbol s)) rule.body) in
        Set.union acc (Set.union headVariables bodyVariables)
      )
      initiVars
      parsedRules
  in

  (* Extract alphabet using map *)
  let alphabet =
    Set.fold_left
      (fun acc rule ->
        let headTerminals = Set.make (List.filter (fun s -> isTerminalSymbol s && s <> epsilon) rule.head) in
        let bodyTerminals = Set.make (List.filter (fun s -> isTerminalSymbol s && s <> epsilon) rule.body) in
        Set.union acc (Set.union headTerminals bodyTerminals)
      )
      Set.empty
      parsedRules
  in

  let model = new Grammar.model (Arg.Representation {
    alphabet = alphabet;
    variables = variables;
    initial = initial;
    rules = parsedRules
  }) in

  (* Convert model to JSON string and alert *)
  (* let model_json = model#toJSon in
  let model_str = Js_of_ocaml.Json.output model_json in
  Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string (Js_of_ocaml.Js.to_string model_str));
    *)
  model

module ControllerListeners = struct
  let listColors = [|"Red"; "Yellow"; "Cyan"; "Green"; "Indigo"; "Blue"; "Magenta"; "Sienna"; "Violet"; "Orange"; "Lime"; "Teal"; "SteelBlue"; "Silver"; "Olive"; "Salmon"; "Crimson"; "Purple"; "DarkKhaki"; "PowderBlue"|]
  let listColorsBig: string array ref = ref [||];;

  let setTitle () =
    if !Ctrl.ctrlR#locked then
      !Ctrl.ctrlR#setTitle
    else 
      !Ctrl.ctrlL#setTitle
  
  let createController c lr = 
    if lr then begin 
      Ctrl.ctrlR := (c :> controller);
    end
    else begin 
      Ctrl.ctrlL := (c :> controller);
    end

  let createFAController fa lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new faController fa lr in
      createController c lr
  
  let createPDAController pda lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new pdaController pda lr in
      createController c lr
  
  let createREController re lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new reController re lr in
      createController c lr

  let createCFGController (cfg: ContextFreeGrammarView.model) lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new cfgController cfg lr in
      createController c lr
      
  let createCFGController2 (cfg: ContextFreeGrammarBasic.model) lr =
      if not lr then !Ctrl.ctrlL#finish else ();
    let cfg = ContextFreeGrammarView.adjust cfg in
    createCFGController cfg lr


  let createGRController (gr: GrammarView.model) lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new grController gr lr in
      createController c lr
      
  let createGRController2 (gr: Grammar.model) lr =
      if not lr then !Ctrl.ctrlL#finish else ();
    let gr = GrammarView.adjust gr in
    createGRController gr lr 
  

  let createTMController (tm: TuringMachineView.model) lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new tmController tm lr in
       createController c lr   
      
  let createCompController (comp: CompositionView.model) lr =
    if not lr then !Ctrl.ctrlL#finish else ();
    let c = new compController comp lr in
      createController c lr
 
  let createExerController ex lr title =
    if not lr then !Ctrl.ctrlL#finish else (); (*pode se apagar eventualmente?*)
    let c = new exerController ex lr title in
      createController c lr 

  let closeLeftAction () =
    (match !Ctrl.ctrlR#locked with
    | false -> 
      HtmlPageClient.clearBox1 ();
      !Ctrl.ctrlR#replicateOnLeft;
      !Ctrl.ctrlL#defineExample;
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt
    | true -> (*apagar apenas esquerda*)
      HtmlPageClient.clearBox1 ();
      !CtrlUtil.changeToControllerCtrlLeft ());
    !Ctrl.ctrlL#updateButtons;
    setTitle()
    
  let conversionTo n =
    CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
    try
      match n with
      | 1 ->  
              let re = !Ctrl.ctrlL#convertToRegExp in 
              CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
              createREController re true;
              !Ctrl.ctrlR#defineExample2
      | 2 -> 
              let fa = !Ctrl.ctrlL#convertToFA in
              createFAController fa true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
      | 3 -> 
              let cfg = !Ctrl.ctrlL#convertToCFG in
              createCFGController cfg true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
      | 4 -> 
              let gr = !Ctrl.ctrlL#convertToGR in
              createGRController gr true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
      | 5 -> 
              let pda = !Ctrl.ctrlL#convertToPDA in
              createPDAController pda true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
      | 6 -> 
              let tm = !Ctrl.ctrlL#convertToTM_SingleTape in
              createTMController tm true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
      | 7 -> 
              let tm = !Ctrl.ctrlL#convertToTM_DualTape in
              createTMController tm true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
    with
    | Failure _ -> JS.alertStr (Lang.i18nErrorConversion ())
    ;;

  Listeners.openEntityListener :=
    fun (txt) -> (
      let j = JSon.parse txt in
        let kind = JSon.fieldString j "kind" in
        (match kind with
        | k when k = FiniteAutomaton.kind -> 
              HtmlPageClient.clearBox1 ();
              let fa = new FiniteAutomatonView.model (JSon j) in 
              createFAController fa false;
              !Ctrl.ctrlL#defineExample
        | k when k = RegularExpression.kind -> 
              HtmlPageClient.clearBox1 ();
              let re = new RegularExpressionView.model (JSon j) in 
              createREController re false;
              !Ctrl.ctrlL#defineExample
        | k when k = PushdownAutomaton.kind -> 
              HtmlPageClient.clearBox1 ();
              let pda = new PushdownAutomatonView.model (JSon j) in 
              createPDAController pda false;
              !Ctrl.ctrlL#defineExample
        | k when k = ContextFreeGrammar.kind ->
              HtmlPageClient.clearBox1 ();
              let cfg = new ContextFreeGrammarView.model (JSon j) in
              createCFGController cfg false;
              !Ctrl.ctrlL#defineExample

        | k when k = Grammar.kind ->
              HtmlPageClient.clearBox1 ();
              let gr = new GrammarView.model (JSon j) in
              createGRController gr false;
              !Ctrl.ctrlL#defineExample

        | k when k = TuringMachine.kind ->
              HtmlPageClient.clearBox1 ();
              let tm = new TuringMachineView.model (JSon j) in 
              createTMController tm false;
              !Ctrl.ctrlL#defineExample
        | k when k = Composition.kind ->
              HtmlPageClient.clearBox1 ();
              let tm = new CompositionView.model (JSon j) in 
              createCompController tm false;
              !Ctrl.ctrlL#defineExample
        | _ -> HtmlPageClient.clearBox2 ();
              let enu = new Exercise.exercise (JSon j) in 
              createExerController enu true "exercise";
              !Ctrl.ctrlR#defineExample2);
        setTitle();
        !Ctrl.ctrlL#printErrors);;

  Listeners.closeRightListener := 
    fun () -> !Ctrl.ctrlR#closeRightAction;
              !Ctrl.ctrlL#resetStyle;
              CtrlUtil.oneBox (!Ctrl.ctrlL#getCy_opt);
              setTitle();;

  Listeners.defineInformationBoxListener :=
    fun () -> !Ctrl.ctrlL#defineInformationBox;;

  ListenersAutomaton.paintAllUsefulListener :=
    fun () -> !Ctrl.ctrlL#resetStyle;
    (!Ctrl.ctrlL#getAutomaton)#usefulPainting !Ctrl.ctrlL#getCy;;

  ListenersAutomaton.paintAllProductivesListener :=
    fun () -> (!Ctrl.ctrlL#resetStyle;
    (!Ctrl.ctrlL#getAutomaton)#productivePainting !Ctrl.ctrlL#getCy);;

  ListenersAutomaton.paintAllReachableListener := 
    fun () -> !Ctrl.ctrlL#resetStyle;
    (!Ctrl.ctrlL#getAutomaton)#reachablePainting !Ctrl.ctrlL#getCy;;
(* ML *)

  ListenersAutomaton.addNode := fun x y -> !Ctrl.ctrlL#addNode x y false false;;

  ListenersAutomaton.addInitialNode := fun x y -> !Ctrl.ctrlL#addNode x y true false;;

  ListenersAutomaton.addFinalNode := fun x y -> !Ctrl.ctrlL#addNode x y false true;;

  ListenersAutomaton.removeNode := fun node -> !Ctrl.ctrlL#eliminateNode node;;

  ListenersAutomaton.turnFinal := fun node -> !Ctrl.ctrlL#turnFinalNode node;;

(*
  ListenersAutomaton.addNode := fun x y -> 
    let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
    match Js.Opt.to_option promptResult with
    | None -> ()
    | Some v -> !Ctrl.ctrlL#addNode x y (Js.to_string v)
    ;;
    
  ListenersAutomaton.addInitialNode := fun x y ->
    let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
    match Js.Opt.to_option promptResult with
    | None -> ()
    | Some v -> !Ctrl.ctrlL#addInitialNode (Js.to_string v)
    ;;

  ListenersAutomaton.addFinalNode := fun x y ->
    let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
    match Js.Opt.to_option promptResult with
    | None -> ()
    | Some v -> !Ctrl.ctrlL#addFinalNode x y (Js.to_string v)
    ;;
*)

  ListenersAutomaton.removeTypeFinal := fun node -> !Ctrl.ctrlL#removeFinalNode node;;

  ListenersAutomaton.turnNodeInitial := fun node -> !Ctrl.ctrlL#addInitialNode node;;

    
  ListenersAutomaton.addTransition := fun src trg -> !Ctrl.ctrlL#createTransition src trg ;;

  ListenersAutomaton.removeTransition := fun srcId trgId symbol -> 
    let open Re in
    (if List.length (Str.split (Str.regexp ",") symbol) > 1 then
      (let text = JS.prompt (Lang.i18nWhichTransition ()) symbol in 
      match Js.Opt.to_option text with
      | None -> ()
      | Some v -> !Ctrl.ctrlL#eliminateTransition (srcId, (Js.to_string v), trgId)
      )
    else 
        !Ctrl.ctrlL#eliminateTransition (srcId, symbol, trgId));;

  ListenersAutomaton.renameNodeListener :=
    fun (state) -> !Ctrl.ctrlL#renameState state;;

  ListenersAutomaton.showTable :=
    fun () -> 
              !Ctrl.ctrlL#changeTab;
              let flag = !Ctrl.ctrlL#getTab in
              HtmlPageClient.toggleTab flag;
              Cytoscape.fit !Ctrl.ctrlL#getCy_opt;;
              


  ListenersFA.cleanUselessListener :=
    fun () -> if ((!Ctrl.ctrlL#getFA)#areAllStatesUseful) then 
                JS.alertStr (Lang.i18nAlertClean ())
              else 
                (let auto = (!Ctrl.ctrlL#getFA)#cleanUselessStates1 !Ctrl.ctrlL#getCy in 
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                createFAController auto true;
                !Ctrl.ctrlR#defineExample2;
                Cytoscape.fit !Ctrl.ctrlR#getCy_opt;);;

  ListenersFA.getDeterministicListener :=
    fun () -> if ((!Ctrl.ctrlL#getFA)#isDeterministic) then 
                JS.alertStr (Lang.i18nAlertDeterministic ())
              else 
                (let auto = (!Ctrl.ctrlL#getFA)#toDeterministic1 in 
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                createFAController auto true;
                !Ctrl.ctrlR#defineExample2;
                Cytoscape.fit !Ctrl.ctrlR#getCy_opt);;
  
  let getRandom() = 
    let test = Random.int 16777215 in
    Printf.sprintf "#%06x" test
    
  let setColor number =
    if (number <= 20) then 
      listColorsBig := listColors
    else 
      (for i=0 to 19 do 
        Array.set !listColorsBig i (Array.get listColors i)
        done;
      for i=20 to number-1 do
        let newColor = getRandom () in 
          Array.set !listColorsBig i newColor
      done);;

  ListenersFA.defineMinimizedListener :=
  fun () -> if ((!Ctrl.ctrlL#getFA)#isDeterministic) then
              if ((!Ctrl.ctrlL#getFA)#isMinimized) then 
                JS.alertStr (Lang.i18nAlertMinimum ())
              else 
                (let auto = (!Ctrl.ctrlL#getFA)#minimize1 in 
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                createFAController auto true;
                let number = (!Ctrl.ctrlL#getFA)#getColors in
                setColor number;
                !Ctrl.ctrlR#defineMinimize !listColorsBig number;)
            else 
              JS.alertStr (Lang.i18nAlertNeedsDeterministic ());;

  ListenersPDA.cleanUselessListener :=
  fun () -> if ((!Ctrl.ctrlL#getPDA)#areAllStatesUseful) then 
              JS.alertStr (Lang.i18nAlertClean ())
            else 
              (let automaton = (!Ctrl.ctrlL#getPDA)#cleanUselessStates1 !Ctrl.ctrlL#getCy in 
              CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
              createPDAController automaton true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt;);;

  Listeners.closeLeftListener := 
  fun () -> 
    (closeLeftAction ();
    setTitle () );;
          
  Listeners.showModelListener := 
    fun () -> CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
              !CtrlUtil.changeToControllerCtrlRight();
              !Ctrl.ctrlR#setUpdateType "specification";
              let getInfo = JSon.toString ((!Ctrl.ctrlL#model)#toJSon) in
              HtmlPageClient.showModelInfo getInfo;;

  ListenersRE.resultCountListener := 
    fun () -> 
      if (!Ctrl.ctrlL#getResultTree)  then 
        HtmlPageClient.putTreeResult (Lang.i18nWordAccepted ())
      else 
        (HtmlPageClient.putTreeResult (Lang.i18nWordNotAccepted ()));
        let blah = !Ctrl.ctrlL#getRE in 
        let blah2 = !Ctrl.ctrlL#getWordAsList() in 
        let (right, wrong) = blah#countRightTrees blah2 in 
          let textt = (Lang.i18nExists ()) ^ (string_of_int (right)) ^ (Lang.i18nGoodDerivations ()) in
            HtmlPageClient.putTreeGoodDerivations textt;
          let textt1 = (Lang.i18nExists ()) ^ (string_of_int (wrong)) ^ (Lang.i18nBadDerivations ()) in
            HtmlPageClient.putTreeBadDerivations textt1;;
      
  ListenersRE.defineNumberTreesListener :=
    fun () -> 
      let pos = (!Ctrl.ctrlL#getRE)#position in 
      let leng = (!Ctrl.ctrlL#getRE)#length in 
      let textt = (string_of_int (pos)) ^ (Lang.i18nBy ()) ^ (string_of_int (leng)) in 
        HtmlPageClient.putTreeNumbers textt;;
  
  ListenersRE.previousTreeListener :=
    fun () -> 
      let back = (!Ctrl.ctrlL#getRE)#back in
      let cy = !Ctrl.ctrlR#getCy in
      Cytoscape.removeAllElements cy;
      ignore ((!Ctrl.ctrlL#getRE)#printTree back cy);
        !ListenersRE.defineNumberTreesListener ();;
  
  ListenersRE.nextTreeListener := 
    fun () ->
      let next = (!Ctrl.ctrlL#getRE)#next in
      let cy = !Ctrl.ctrlR#getCy in
      Cytoscape.removeAllElements cy;
      ignore ((!Ctrl.ctrlL#getRE)#printTree next cy); 
        !ListenersRE.defineNumberTreesListener ();;
              
  ListenersEXER.checkExerciseListener :=
    fun () -> (
        let model = !Ctrl.ctrlL#model in 
        let result = model#checkExercise (!Ctrl.ctrlR#getExercise) in
        let (insideErrors, outsideErrors, properties) = model#checkExerciseFailures (!Ctrl.ctrlR#getExercise) in 
          !Ctrl.ctrlR#checkHelper result (insideErrors, outsideErrors, properties);
    );;
      
  ListenersEXER.clearExerciseListener :=
    fun () -> (
      HtmlPageClient.oneBox ();
      let element = Dom_html.getElementById "cy2" in
        element##.innerHTML := Js.string "";
      !CtrlUtil.changeToControllerCtrlRight();
      setTitle ();
      );;

  ListenersRE.changeDirectionListener :=  fun () -> 
    let newDir = (Cytoscape.changeDirection !Ctrl.ctrlL#getCy !Ctrl.ctrlL#getLayoutDir) in
		!Ctrl.ctrlL#changeLayoutDir newDir;;

  ListenersCFG.cleanCFGListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if not cfg#isClean then
				let newCfg = cfg#clean1 in
					createCFGController2 newCfg.grammar true;
					!Ctrl.ctrlL#box2CFGShow newCfg
              else
				JS.alertStr (Lang.i18nAlertIsClean());;
 
  ListenersCFG.previousNewCFGListener :=
    fun () -> let newCFG = (!Ctrl.ctrlL#getCFG)#getPreviousTransformed in
              createCFGController2 newCFG.grammar true;
              !Ctrl.ctrlL#box2CFGShow newCFG;;

  ListenersCFG.nextNewCFGListener :=
    fun () -> let newCFG = (!Ctrl.ctrlL#getCFG)#getNextTransformed in
              createCFGController2 newCFG.grammar true;
              !Ctrl.ctrlL#box2CFGShow newCFG;;

  ListenersCFG.removeLeftRecursionListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if cfg#isLeftRecursive
              then ( let newCfg = cfg#removeLeftRecursion1 in
                createCFGController2 newCfg.grammar true;
                !Ctrl.ctrlL#box2CFGShow newCfg)
              else JS.alertStr (Lang.i18nAlertNotLeftRecursive());;
    
  ListenersCFG.leftFactoringListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if cfg#isLeftFactoring
              then ( let newCfg = cfg#leftFactoring1 in
                createCFGController2 newCfg.grammar true;
                !Ctrl.ctrlL#box2CFGShow newCfg)
              else JS.alertStr (Lang.i18nAlertNotLeftFactoring());;
  
  ListenersCFG.removeEpsilonListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if cfg#hasEmptyProductions
              then ( let newCfg = cfg#removeEmptyProductions1 in
                createCFGController2 newCfg.grammar true;
                !Ctrl.ctrlL#box2CFGShow newCfg)
              else JS.alertStr (Lang.i18nAlertNoEmptyProductions());;

  ListenersCFG.removeUnitListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if cfg#hasUnitProductions
              then ( let newCfg = cfg#removeUnitProductions1 in
                createCFGController2 newCfg.grammar true;
                !Ctrl.ctrlL#box2CFGShow newCfg)
              else JS.alertStr (Lang.i18nAlertNoUnitProductions());;

  ListenersCFG.transformLL1Listener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if not cfg#isLL1
              then ( let newCfg = cfg#transformToLL1X in
                createCFGController2 newCfg.grammar true;
                !Ctrl.ctrlL#box2CFGShow newCfg)
              else JS.alertStr (Lang.i18nAlertIsLL1());;

  ListenersCFG.tablesListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
              HtmlPageClient.prepareCFG2Tables ();
              cfg#createFirstAndFollowTableHtml;
              cfg#createParsingTableHtml;;
  
  ListenersCFG.recursiveDescedentParserListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              if not cfg#isLL1
              then JS.alertStr (Lang.i18nIsNotLL1())
              else  (
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                let optsList = cfg#rdparserOpts in
                HtmlPageClient.prepareCFG2RecursiveDescedentParser optsList (!Ctrl.ctrlL#getCFG#generateRecursiveDescendentParser));;


  (*GR *)
  ListenersGR.grOps :=
    fun (op: string) -> !Ctrl.ctrlL#handleOp op; 
  ;;


    

  ListenersGR.createModelListener := fun () -> 
    try 
      let str = ViewUtil.extractStringFromTextArea() in
      let grModel = grStr2Model str in
      HtmlPageClient.hideModalWindow();
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createGRController2 grModel false;
      !Ctrl.ctrlL#defineExample;
      setTitle()
    with
      _ -> JS.alert (Lang.i18nErrorParsing());;

                
open ContextFreeGrammarLL1View;;
  
  ListenersCFG.simpleToggleListener :=
    fun () -> let cfg = !Ctrl.ctrlL#getCFG in
              cfg#toggleSimplified;
              let elem = Dom_html.getElementById_opt (ContextFreeGrammarLL1View.firstFollowTableId()) in
              match elem with
              | None -> ()
              | Some e -> let otherE = Dom_html.getElementById (ContextFreeGrammarLL1View.parsingTableId()) in
                          e##.innerHTML := Js.string "";
                          otherE##.innerHTML := Js.string "";
                          cfg#createFirstAndFollowTableHtml;
                          cfg#createParsingTableHtml;;

    
    let createModelPrep titleTxt otherTxt textAreaString okAction =
      let modelContent = HtmlPageClient.editModelContent titleTxt otherTxt textAreaString okAction in
      HtmlPageClient.setModal (Js.Unsafe.coerce modelContent);
      HtmlPageClient.showModalWindow ()
  
      
    let createModel modelType ?(customString="") () =
      let getDefault default = if customString = "" then default else customString in
      match modelType with
      | "FA" -> !ListenersFA.createModelListener()
      | "PDA" -> !ListenersPDA.createModelListener()
      | "TM" -> !ListenersTM.createModelListener()
      | "RE" -> createModelPrep (Lang.i18nMainTitle2()) "" (getDefault "ab") (fun () -> !ListenersRE.createModelListener())
      | "Comp" -> createModelPrep (Lang.i18nMainTitleComp()) "" (getDefault "[dfa_1]+[dfa_1]") (fun () -> !ListenersComp.createModelListener())
      | "CFG" -> createModelPrep (Lang.i18nMainTitle4()) (Lang.i18nInstructionsCFG()) (getDefault "S -> [ S ] | A\nA -> a") (fun () -> !ListenersCFG.createModelListener())
      | "GR" -> createModelPrep (Lang.i18nMainTitle5()) (Lang.i18nInstructionsGR()) (getDefault "S -> aBC | aSBC\nCB -> CZ\nCZ -> WZ\nWZ -> WC\nWC -> BC\naB -> ab\nbB -> bb\nbC -> bc\ncC -> cc") (fun () -> !ListenersGR.createModelListener())
      | _ -> ();;
  
    
    Listeners.createModelListener :=
      fun () -> 
        HtmlPageClient.putInnerHtml "selectConv" (Lang.i18nSelectConv ());
        match Dom_html.getElementById_coerce "selectNewModel" Dom_html.CoerceTo.select with
        | None -> ()
        | Some select -> 
            let value = Js.to_string select##.value in
            if value = "optionNewAutomatonFA" then createModel "FA" () else
            if value = "optionNewAutomatonPDA" then createModel "PDA" () else
            if value = "optionNewRegularExpression" then createModel "RE" () else
            if value = "optionNewContextFreeGrammar" then createModel "CFG" () else
            if value = "optionNewGrammar" then createModel "GR" () else
            if value = "optionNewTuringMachine" then createModel "TM" () else
            if value = "optionNewComposition" then createModel "Comp" ();
            select##.selectedIndex := 0;;

    ListenersAutomaton.editModelListener :=
    fun () -> 
      JS.alert (Lang.i18nModelEditFA());
      !Ctrl.ctrlL#changeToEditModelMode;;
  
    ListenersRE.editModelListener :=
      fun () -> createModel "RE" ~customString:(!Ctrl.ctrlL#model2Str) ();;  
  
    ListenersCFG.editModelListener :=
      fun () -> createModel "CFG" ~customString:(!Ctrl.ctrlL#model2Str) ();;
  
    ListenersGR.editModelListener :=
      fun () -> createModel "GR" ~customString:(!Ctrl.ctrlL#model2Str) ();;

    Listeners.editModelListener :=
      fun () -> 
        HtmlPageClient.putInnerHtml "selectConv" (Lang.i18nSelectConv ());
        let model = !Ctrl.ctrlL#returnType in
        if model = RegularExpression.kind then
            createModel "RE" ~customString:(!Ctrl.ctrlL#model2Str) ()
        else if model = ContextFreeGrammar.kind then
            createModel "CFG" ~customString:(!Ctrl.ctrlL#model2Str) ()
        else if model = Grammar.kind then
            createModel "GR" ~customString:(!Ctrl.ctrlL#model2Str) ()
        else if model = FiniteAutomaton.kind then
            JS.alert (Lang.i18nModelEditFA())
        else if model = PushdownAutomaton.kind then
            JS.alert (Lang.i18nModelEditFA())
        else if model = TuringMachine.kind then
            JS.alert (Lang.i18nModelEditFA())
        else ();
        match Dom_html.getElementById_coerce "selectNewModel" Dom_html.CoerceTo.select with
        | None -> ()
        | Some select -> select##.selectedIndex := 0;;

(*
  let createComp () =
    createModelPrepComp "[dfa_1]+[dfa_1]" (fun () -> !ListenersComp.createModelListener())

  let createCFG () =
    createModelPrepCFG "S -> [ S ] | A\nA -> a" (fun () -> !ListenersCFG.createModelListener())

  let createTM () =
    !ListenersTM.createModelListener()
  
  let extractStringFromTextArea () =
    match Dom_html.getElementById_coerce "modelStringContainer" Dom_html.CoerceTo.textarea with
              | None -> ""
              | Some textarea -> Js.to_string textarea##.value;;

  Listeners.createModelListener :=
    fun () -> match Dom_html.getElementById_coerce "selectNewModel" Dom_html.CoerceTo.select with
              | None -> ()
              | Some select -> let value = Js.to_string select##.value in
                                if value = "optionNewAutomatonFA" then createFA () else
                                if value = "optionNewAutomatonPDA" then createPDA () else
                                if value = "optionNewRegularExpression" then createRE () else
                                if value = "optionNewContextFreeGrammar" then createCFG ();
                                if value = "optionNewTuringMachine" then createTM ();
                                if value = "optionNewComposition" then createComp ();
                               select##.selectedIndex := 0;;

  let cfgStr2Model str = (*TODO Where to put this?*)
    let open ContextFreeGrammarBasic in
    let splitStr = String.split_on_char '\n' str in
    let initialRule = Set.make [List.hd splitStr] in
    let otherRules = Set.make (List.tl splitStr) in
    let initialParsedRule = ContextFreeGrammarBasic.parse initialRule in
    let initial = (Set.nth initialParsedRule 0).head in
    let parsedRules = ContextFreeGrammarBasic.parse (Set.union initialRule otherRules) in
    let variables = Set.add initial (Set.map (fun {head = h; _} -> h) parsedRules) in
    let alphabet = Set.flatMap (fun {head = h; body = b} -> Set.make (List.filter (fun s -> not (Set.belongs s variables)) b) ) parsedRules in
    new ContextFreeGrammarBasic.model (Arg.Representation {
      alphabet = alphabet;
      variables = variables;
      initial = initial;
      rules = parsedRules
    } );;

  let grammar2Str (rep:ContextFreeGrammarView.t) = (*TODO Where to put this?*)
    let open ContextFreeGrammarBasic in
    let initialRules = Set.filter (fun {head = h; _} -> h = rep.initial) rep.rules in
    let nonInitialRules = Set.filter (fun {head = h; _} -> h <> rep.initial) rep.rules in
    let initialRulesStrLst = ContextFreeGrammarBasic.toStringList initialRules in
    let nonInitialRulesStrLst = ContextFreeGrammarBasic.toStringList nonInitialRules in
    let rulesList = initialRulesStrLst @ nonInitialRulesStrLst in
    let rec toString l =
      match l with
      | [] -> ""
      | x::xs -> x ^ "\n" ^ (toString xs)
    in
      toString rulesList;;
*)
  ListenersFA.createModelListener := fun () -> 
    let defaultFA = new FiniteAutomatonView.model (Representation {
      alphabet = Set.empty;
      states = Set.make ["START"]; 
      initialState = "START";
      transitions = Set.empty;
      acceptStates = Set.empty
    }) in
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createFAController defaultFA false;
      !Ctrl.ctrlL#defineExample;
      setTitle();;

  ListenersPDA.createModelListener := fun () -> 
    let defaultPDA = new PushdownAutomatonView.model (Representation {
        inputAlphabet = Set.empty;
        stackAlphabet = Set.make [PushdownAutomaton.stackSpecialSymb];
        states = Set.make ["START"];
        initialState = "START";
        initialStackSymbol = PushdownAutomaton.stackSpecialSymb;
        transitions = Set.empty;
        acceptStates = Set.empty;
        criteria = true
    }) in
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createPDAController defaultPDA false;
      !Ctrl.ctrlL#defineExample;
      setTitle();;

    ListenersTM.createModelListener := fun () -> 
       let defaultTM =
		new TuringMachineView.model (
			Representation
				{ TuringMachine.tm_zero with
					states = Set.make ["START"];
					initialState = "START"; }
		) in
  
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createTMController defaultTM false;
      !Ctrl.ctrlL#defineExample;
      setTitle();;
  
  ListenersRE.createModelListener := fun () ->
    try
      let str = ViewUtil.extractStringFromTextArea() in
      let reStr = new RegularExpressionView.model (Representation (RegularExpression.parse str)) in
      HtmlPageClient.hideModalWindow();
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createREController reStr false;
      !Ctrl.ctrlL#defineExample;
      setTitle()
    with
      _ -> JS.alert (Lang.i18nErrorParsing());;

(* carolina *)
  ListenersComp.createModelListener := fun () ->
    try
      let str = ViewUtil.extractStringFromTextArea() in
      let compStr = new CompositionView.model (Representation (Composition.parse str)) in
     HtmlPageClient.hideModalWindow();
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createCompController compStr false;
     !Ctrl.ctrlL#defineExample;
     setTitle()
    with
      _ -> JS.alert (Lang.i18nErrorParsing());;
  

  ListenersCFG.createModelListener := fun () -> 
    try 
      let str = ViewUtil.extractStringFromTextArea() in
      let cfgModel = cfgStr2Model str in
      HtmlPageClient.hideModalWindow();
      CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.clearBox1();
      createCFGController2 cfgModel false;
      !Ctrl.ctrlL#defineExample;
      setTitle()
    with
      _ -> JS.alert (Lang.i18nErrorParsing());;
(*
  Listeners.editModelListener :=
    fun () -> !Ctrl.ctrlL#editModel;;

  ListenersAutomaton.editModelListener :=
    fun () -> 
      JS.alert (Lang.i18nModelEditFA());
      !Ctrl.ctrlL#changeToEditModelMode;;
*)
(* carolina *)
  Listeners.save :=
  fun () ->     
  let open Js.Unsafe in
  let open Repository in
  let createModelPrep titleTxt otherTxt textAreaString okAction =
    let modelContent = HtmlPageClient.editModelContent titleTxt otherTxt textAreaString okAction in
    HtmlPageClient.setModal (Js.Unsafe.coerce modelContent);
    HtmlPageClient.showModalWindow () in

(* carolina *)
  let createModelPrepSave textAreaString okAction =
    createModelPrep (Lang.i18nSaveText()) "" textAreaString okAction in

  
(* carolina *)
  createModelPrepSave ("Example") (fun () -> !Listeners.saveModel() );; (*carolina*)
    (*Ctrl.ctrlL#getModel*)  
    
(* carolina *)
  Listeners.saveModel :=
    fun () ->
      let str = ViewUtil.extractStringFromTextArea() in
      (let m = !Ctrl.ctrlL#model in
      (*JS.alertStr str;*)
      JS.log (m));
      Repository.updateModel str !Ctrl.ctrlL#model;
      HtmlPageClient.hideModalWindow();
      HtmlPageClient.putButton2 str;;




  ListenersPDA.toggleAcceptanceCriteria :=
    fun () ->
      !Ctrl.ctrlL#toggleAcceptanceCriteria;;

  ListenersPDA.changeInitialStackSymbol :=
    fun () ->
      !Ctrl.ctrlL#changeInitialStackSymbol;;

  let showPdaCtrlR pda =
    CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
    createPDAController pda true;
    !Ctrl.ctrlR#defineExample2;
    Cytoscape.fit !Ctrl.ctrlR#getCy_opt;;

  ListenersPDA.convertAcceptStates :=
    fun () ->
      (match !Ctrl.ctrlL#convertAcceptStates with
      | None -> JS.alertStr (Lang.i18nAlreadyInAcceptanceModeByStates ())
      | Some pdaConverted -> showPdaCtrlR pdaConverted);;
          
  ListenersPDA.convertEmptyStackAccept :=
    fun () ->
      (match !Ctrl.ctrlL#convertEmptyStackAccept with
      | None -> JS.alertStr (Lang.i18nAlreadyInAcceptanceModeByEmptyStack ())
      | Some pdaConverted -> showPdaCtrlR pdaConverted);;
(*
  ListenersRE.editModelListener :=
    fun () -> createModelPrepRE (RegularExpression.toString !Ctrl.ctrlL#getRE#representation) (fun () -> !ListenersRE.createModelListener());;
  
  ListenersCFG.editModelListener :=
    fun () -> createModelPrepCFG (grammar2Str !Ctrl.ctrlL#getCFG#representation) (fun () -> !ListenersCFG.createModelListener());;
    *)
  ListenersAutomaton.clearAutoListener :=
    fun () -> Cytoscape.resetStyle !Ctrl.ctrlL#getCy Cytoscape.faStyle;;
    
  Listeners.updateRightListener :=
    fun () -> !Ctrl.ctrlL#updateRight
;;


    ListenersLR.buildLR0DiagramListener :=
		fun () -> try
					CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
					let cfg = !Ctrl.ctrlL#getCFG in
							
					!CtrlUtil.changeToControllerCtrlRight();
					let cy = !Ctrl.ctrlR#getCy in

					
					HtmlPageClient.defineCFG();
					HtmlPageClient.cfgBoxRegex();
					(* HtmlPageClient.cfgCyClose(); *)
					cfg#buildCyLR0Diagram cy;	 
					
					
					cy##resize; 
					cy##fit;
					
				with
                  _ -> JS.alert (Lang.i18nErrorParsing());;
		
	ListenersLR.buildSLR1DiagramListener :=
		fun () -> try
					CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
					let cfg = !Ctrl.ctrlL#getCFG in
					
					!CtrlUtil.changeToControllerCtrlRight();
					let cy = !Ctrl.ctrlR#getCy in

					
					HtmlPageClient.defineCFG();
					HtmlPageClient.cfgBoxRegex();
					(* HtmlPageClient.cfgCyClose(); *)
					cfg#buildCySLR1Diagram cy;	 
					
					
					cy##resize; 
					cy##fit;

				with
                  _ -> JS.alert (Lang.i18nErrorParsing());;
		
	ListenersLR.buildLR1DiagramListener :=
		fun () -> try
					CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
					let cfg = !Ctrl.ctrlL#getCFG in
			
					!CtrlUtil.changeToControllerCtrlRight();
					let cy = !Ctrl.ctrlR#getCy in
					
					HtmlPageClient.defineCFG();
					HtmlPageClient.cfgBoxRegex();
					(* HtmlPageClient.cfgCyClose(); *)
					cfg#buildCyLR1Diagram cy;	 
					
					
					cy##resize; 
					cy##fit;
					
				with
                  _ -> JS.alert (Lang.i18nErrorParsing());;
		
	ListenersLR.buildLALR1DiagramListener :=
		fun () -> try
					CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
					let cfg = !Ctrl.ctrlL#getCFG in			
					
					!CtrlUtil.changeToControllerCtrlRight();
					let cy = !Ctrl.ctrlR#getCy in
					
					HtmlPageClient.defineCFG();
					HtmlPageClient.cfgBoxRegex();
					(* HtmlPageClient.cfgCyClose(); *)
					cfg#buildCyLALR1Diagram cy;	 
					
					
					cy##resize; 
					cy##fit;

				with
                  _ -> JS.alert (Lang.i18nErrorParsing());;
		
	 ListenersLR.buildLR0TableListener :=
		fun () ->   
				let cfg = !Ctrl.ctrlL#getCFG in
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                HtmlPageClient.prepareCFG2Tables ();
                cfg#createFirstAndFollowTableHtml;
                cfg#createLR0ParsingTableHtml;; 
		
	ListenersLR.buildSLR1TableListener :=
		fun () -> let cfg = !Ctrl.ctrlL#getCFG in
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                HtmlPageClient.prepareCFG2Tables ();
                cfg#createFirstAndFollowTableHtml;
                cfg#createSLR1ParsingTableHtml;;    
		
	ListenersLR.buildLR1TableListener :=
		fun () -> let cfg = !Ctrl.ctrlL#getCFG in
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                HtmlPageClient.prepareCFG2Tables ();
                cfg#createFirstAndFollowTableHtml;
                cfg#createLR1ParsingTableHtml;;    
		
	ListenersLR.buildLALR1TableListener :=
		fun () -> let cfg = !Ctrl.ctrlL#getCFG in
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                HtmlPageClient.prepareCFG2Tables ();
                cfg#createFirstAndFollowTableHtml;
                cfg#createLALR1ParsingTableHtml;;  
                
    ListenersLR.acceptLR0Listener :=
		fun () -> let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "ababab$" in 
                
                 match Js.Opt.to_option text with
                | None -> ()
				| Some v -> let cfg = !Ctrl.ctrlL#getCFG in
							CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
							HtmlPageClient.prepareAcceptTable ();
							cfg#createLR0AcceptTableHtmlV2 (Js.to_string v);;
				
				
	ListenersLR.acceptSLR1Listener :=
		fun () -> let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "azc$" in 
                match Js.Opt.to_option text with
                | None -> ()
				| Some v -> let cfg = !Ctrl.ctrlL#getCFG in
							CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
							HtmlPageClient.prepareAcceptTable ();
							cfg#createSLR1AcceptTableHtmlV2 (Js.to_string v);;
				
	ListenersLR.acceptLR1Listener :=
		fun () -> let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "azc$" in 
                 match Js.Opt.to_option text with
                | None -> ()
				| Some v -> let cfg = !Ctrl.ctrlL#getCFG in
							CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
							HtmlPageClient.prepareAcceptTable ();
							cfg#createLR1AcceptTableHtmlV2 (Js.to_string v);;
				
	ListenersLR.acceptLALR1Listener :=
		fun () -> let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "cccccccdcccccccccd$" in 
                 match Js.Opt.to_option text with
                | None -> ()
				| Some v -> let cfg = !Ctrl.ctrlL#getCFG in
							CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
							HtmlPageClient.prepareAcceptTable ();
							cfg#createLALR1AcceptTableHtmlV2 (Js.to_string v);;
				

  ListenersComp.showTreeNode := (*carolina*)
      fun (label) -> (
      let open Examples in
      let open Error in
      let open Repository in
      let j = getJSon label in
        let kind = JSon.fieldString j "kind" in
        CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
        (*HtmlPageClient.twoBoxes();
        JS.alert j;*)
        (match kind with
        | k when k = FiniteAutomaton.kind -> 
              HtmlPageClient.clearBox2 ();
              let fa = new FiniteAutomatonView.model (JSon j) in
              createFAController fa true;
(*JS.alertStr "XX";  FAZ DUAS VEZES AMD *)
              !Ctrl.ctrlR#defineExample2;
(*JS.alertStr "YY"; *)
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
        | k when k = RegularExpression.kind -> 
              HtmlPageClient.clearBox2 ();
              let re = new RegularExpressionView.model (JSon j) in 
              createREController re true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
        | k when k = PushdownAutomaton.kind -> 
              HtmlPageClient.clearBox2 ();
              let pda = new PushdownAutomatonView.model (JSon j) in 
              createPDAController pda true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
        | k when k = ContextFreeGrammar.kind ->
              HtmlPageClient.clearBox2 ();
              let cfg = new ContextFreeGrammarView.model (JSon j) in
              createCFGController cfg true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt

        | k when k = Grammar.kind ->
              HtmlPageClient.clearBox2 ();
              let gr = new GrammarView.model (JSon j) in
              createGRController gr true;
              !Ctrl.ctrlR#defineExample;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt

        | k when k = TuringMachine.kind ->
              HtmlPageClient.clearBox2 ();
              let tm = new TuringMachineView.model (JSon j) in
              createTMController tm true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt
        | k when k = Composition.kind ->
              HtmlPageClient.clearBox2 ();
              let comp = new CompositionView.model (JSon j) in
              createCompController comp true;
              !Ctrl.ctrlR#defineExample2;
              Cytoscape.fit !Ctrl.ctrlR#getCy_opt        
        | _ -> Error.fatal "showTreeNode");
      )
        (*setTitle();
        !Ctrl.ctrlL#printErrors);;*)
  
   (*carolina*)
    
		





end
