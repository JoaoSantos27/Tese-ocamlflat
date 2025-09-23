open OCamlFlat
open BasicTypes
open HtmlPageClient
open Js_of_ocaml
open JS
open Lang
open Controller
open GrammarView
open Listeners
open ViewUtil


let grammar2Str (rep:GrammarView.t) =
  let open Grammar in
  let initialRules = Set.filter (fun {head = h; _} -> List.hd h = rep.initial && List.length h = 1) rep.rules in
  let nonInitialRules = Set.filter (fun {head = h; _} -> List.hd h <> rep.initial || List.length h <> 1) rep.rules in
  let initialRulesStrLst = Grammar.toStringList initialRules in
  let nonInitialRulesStrLst = Grammar.toStringList nonInitialRules in
  let rulesList = initialRulesStrLst @ nonInitialRulesStrLst in
  let rec toString l =
    match l with
    | [] -> ""
    | x::xs -> x ^ "\n" ^ (toString xs)
  in
    toString rulesList


  

class grController (gr: GrammarView.model) (s:bool) =
  object(self) inherit controller as super
    val mutable myGR = gr
    val side = s
    val cy = if s then Some (Cytoscape.initGRCy "cy2") else Some (Cytoscape.initGRCy "cy");

    method operationGR opName: unit =
      super#operation opName "GR"
   
    method model: Model.model =
      (myGR :> Model.model)
    
    method getGR = myGR
    
    method changeGR res = myGR <- res
    
    method getModel =
      myGR#toDisplayString "solution"
 
    method setTitle =
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (Grammar.kind)

    method returnType = Grammar.kind


    
    method defineExample = 
      self#operationGR "create example";
      self#updateButtons;
      HtmlPageClient.putCyGRButtons();
      HtmlPageClient.grBoxRegex(); 
      HtmlPageClient.grCyClose();  
      HtmlPageClient.defineGR();
      myGR#createGrammarTableHtml ""; 
      self#defineInformationBox

    method defineExample2 =
      self#operationGR "create example2";
      CtrlUtil.twoBoxes self#getCy_opt;
      HtmlPageClient.printGR2GrammarComp (GrammarView.productionsTableId2());
      myGR#createGrammarTableHtml (GrammarView.productionsTableId2());
      !Ctrl.ctrlR#defineInformationBox

    method replicateOnLeft =
      let c = new grController self#getGR false in
      Ctrl.ctrlL := (c :> controller);

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      if side then HtmlPageClient.grCy2Close();
      let cfg = myGR#isContextFreeGrammar in
      let csg = myGR#isContextSensitiveGrammar in 
      let mo = myGR#isMonotonicGrammar in 
      let ug = myGR#isUnrestrictedGrammar in
      if cfg then begin
        let lg = myGR#isLinearGrammar in
        let llg = myGR#isLeftLinearGrammar in
        let lrg = myGR#isRightLinearGrammar in
        if lg then begin
          HtmlPageClient.getIsLG lg infoBox;
          if llg then
            HtmlPageClient.getIsLLG llg infoBox
          else
            HtmlPageClient.getIsRLG lrg infoBox
        end else
          HtmlPageClient.getIsCFG cfg infoBox
      end else if csg then begin
        HtmlPageClient.getIsCSG csg infoBox
      end else 
        HtmlPageClient.getIsUG ug infoBox;
    
      HtmlPageClient.getIsMO mo infoBox;
      let c = myGR#isClean in
      let prod = myGR#allRulesProductive in
      let access = myGR#allRulesAccessible in
      HtmlPageClient.getIsGRClean c prod access infoBox;
      ()

    method box2GRShow (g : Grammar.model) =
      self#operationGR "create example2";
      CtrlUtil.twoBoxes self#getCy_opt;
      HtmlPageClient.printGR2Grammar (GrammarView.productionsTableId2());
      (GrammarView.adjust g)#createGrammarTableHtml (GrammarView.productionsTableId2());
      !Ctrl.ctrlR#defineInformationBox

    method checkWord word =
      self#operationGR "checkWord";
      let w = str2word word in
      myGR#staticAccept w;
      let (accepted, configs, exact, time) = myGR#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time


    method getWords v = 
      self#operationGR "accepted words";
      let var = myGR#staticGenerate v in
      let (_, visitedConfigs, exact, time) =
        myGR#returnStats in
          HtmlPageClient.putWords var;
          HtmlPageClient.displayGenStats visitedConfigs exact time
    

    method editModel =  
      !ListenersGR.editModelListener(); () 

    method updateButtons = 
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTMConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyPDAButtons;

      if myGR#isContextFreeGrammar then begin
        List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGConvertButtons;
        List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyPDAButtons;
      end;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons
  
    method convertToPDA =
      let open PushdownAutomatonView in
      self#operationGR "convert to PDA";
      let pda = PolyModel.gr2pda (myGR :> Grammar.model) in
      new PushdownAutomatonView.model (Representation (pda#representation)) 

    method model2Str =
      let gr_representation = !Ctrl.ctrlL#getGR#representation in
      grammar2Str gr_representation


    method convertToCFG =
      let open ContextFreeGrammarView in
      self#operationGR "convert to CFG";
      let cfg = PolyModel.gr2cfg (myGR :> Grammar.model) in
      new ContextFreeGrammarView.model (Representation (cfg#representation))

    method showTrace word =
      self#operationGR "trace";
      let w = str2word word in
      myGR#staticAcceptFull w;
      let (accepted, configs, exact, time) = myGR#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;
      myGR#displayTrace



    method handleOp (operation: string) =
      match operation with
      (* | "create" ->
          Js_of_ocaml.Dom_html.window##alert (Js.string "creating");
          let str = ViewUtil.extractStringFromTextArea() in
          Js_of_ocaml.Dom_html.window##alert (Js.string "extracted");
          let grModel = grStr2Model str in
          Js_of_ocaml.Dom_html.window##alert (Js.string "model");
          HtmlPageClient.hideModalWindow();
          CtrlUtil.oneBox self#getCy_opt;
          HtmlPageClient.clearBox1();
          createController grModel false;
          self#defineExample;
          self#setTitle *)
      | "clean" ->
          if not myGR#isClean then
            let newGr = myGR#clean in
            let newGrModel = (newGr :> Grammar.model) in
            let gr = GrammarView.adjust newGrModel in
            let c = new grController gr true in
              Ctrl.changeCtrlR (c :> Controller.controller);
            self#box2GRShow newGrModel
          else
            JS.alertStr (Lang.i18nAlertIsClean())
       | "kuroda" ->
          if myGR#isMonotonicGrammar then
            let newGr = myGR#kuroda in
            let newGrModel = (newGr :> Grammar.model) in
            let gr = GrammarView.adjust newGrModel in
            let c = new grController gr true in
              Ctrl.changeCtrlR (c :> Controller.controller);
            self#box2GRShow newGrModel
          else
            JS.alertStr (Lang.i18nAlertIsNotMonotonic())
      | "penttonen" ->
          if myGR#isMonotonicGrammar then
            let newGr = myGR#penttonen in
            let newGrModel = (newGr :> Grammar.model) in
            let gr = GrammarView.adjust newGrModel in
            let c = new grController gr true in
              Ctrl.changeCtrlR (c :> Controller.controller);
            self#box2GRShow newGrModel
          else
            JS.alertStr (Lang.i18nAlertIsNotMonotonic())
      | "nonContractingToCSG" ->
          if myGR#isContextSensitiveGrammar then
            JS.alertStr (Lang.i18nIsCSG())
          else if myGR#isMonotonicGrammar then
            let newGr = myGR#nonContractingToCSG in
            let newGrModel = (newGr :> Grammar.model) in
            let gr = GrammarView.adjust newGrModel in
            let c = new grController gr true in
              Ctrl.changeCtrlR (c :> Controller.controller);
            self#box2GRShow newGrModel
          else
            JS.alertStr (Lang.i18nAlertIsNotMonotonic())
      | _ -> JS.alertStr "Invalid operation"

end

