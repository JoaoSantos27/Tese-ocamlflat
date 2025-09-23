open OCamlFlat
open BasicTypes
open HtmlPageClient
open Lang
open JS
open RegularExpressionView
open Controller
open Listeners

class reController (re: RegularExpressionView.model) (s: bool) =
  object(self) inherit controller as super

    val re1 = re

    val side = s
    val cy = Some (if s then Cytoscape.startTree "cy2" else Cytoscape.startTree "cy")

    val mutable step = 0

    val mutable re = new RegularExpressionView.model (Representation Empty)
    val mutable resultTree = false
    val mutable wordAsList : word = []

    method model: Model.model = 
      (re1 :> Model.model) 

    method getWordAsList () =
      wordAsList

    method getRE =
      re1

    method getResultTree =
      resultTree

    method setRe newRe = 
      re <- newRe

    method getModel = 
      re1#toDisplayString "solution"

    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (RegularExpression.kind)

    method operationRE opName : unit =
      super#operation opName "RE"

    method returnType = RegularExpression.kind

    method private makeTree cy (re : RegularExpression.t) =
      let nGetName = ref 0 in
      let genName () = 
        let prefix = "N" in 
        nGetName := !nGetName + 1;
        prefix ^ (string_of_int !nGetName)
      in
      let rec makeTree2 cy (re: RegularExpression.t) =
      match re with
        | Plus (l, r) ->  let rootName = genName () in 
                          let rootL = makeTree2 cy l in 
                          let rootR = makeTree2 cy r in
                            Cytoscape.makeTreeNode cy rootName "+"; 
                            Cytoscape.makeTreeEdge cy rootName rootL;
                            Cytoscape.makeTreeEdge cy rootName rootR;
                            rootName
        | Seq (l, r) -> let rootName = genName () in 
                        let rootL = makeTree2 cy l in 
                        let rootR = makeTree2 cy r in
                          Cytoscape.makeTreeNode cy rootName "."; 
                          Cytoscape.makeTreeEdge cy rootName rootL;
                          Cytoscape.makeTreeEdge cy rootName rootR;
                          rootName
        | Star re -> let rootName = genName () in 
                        let root = makeTree2 cy re  in 
                          Cytoscape.makeTreeNode cy rootName "*"; 
                          Cytoscape.makeTreeEdge cy rootName root;
                          rootName
        | Symb b -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName (symb2str b);
                        rootName
        | Empty  -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName "~";
                        rootName
        | Zero   -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName "!";
                        rootName
      in
      makeTree2 cy re

    method private drawTree cy re text =
      if String.length text >= 120 
      then Cytoscape.makeTreeNode cy "nope" (Lang.i18nAlertRETooBig ())
      else ignore (self#makeTree cy re1#representation)

    method defineExample =
      self#operationRE "create";
      self#updateButtons;
      HtmlPageClient.putCyREButtons();
      HtmlPageClient.fitBoxRegex ();
      Cytoscape.fit self#getCy_opt;
      let test = RegularExpression.toString re1#representation in
       self#drawTree self#getCy re1#representation test;
        HtmlPageClient.defineRE test side

    method defineExample2 =
      self#operationRE "create 2";
      let text = RegularExpression.toString re1#representation in
        self#drawTree self#getCy re1#representation text;
        HtmlPageClient.defineRE text side

    method startStep word = 
      self#operationRE "accept";
      let w = str2word word in
        wordAsList <- w;
        CtrlUtil.twoBoxes self#getCy_opt;
        re1#startAllTrees w;
        resultTree <- re1#accept w;
        Ctrl.changeCtrlR ((new textController true) :> controller );
        Cytoscape.resetStyle !Ctrl.ctrlR#getCy Cytoscape.reStyle;
        if (resultTree) then
          (!ListenersRE.resultCountListener ();
          let right = re1#getRightTrees in 
          ignore (re1#printTree right (!Ctrl.ctrlR#getCy));
            !ListenersRE.defineNumberTreesListener ();
            HtmlPageClient.defineTreeButtons ();
          )
        else 
          (!ListenersRE.resultCountListener();
          let wrong = re1#getWrongTrees in 
          ignore(re1#printTree wrong (!Ctrl.ctrlR#getCy));
            !ListenersRE.defineNumberTreesListener ();
            HtmlPageClient.defineTreeButtons ();
           )

    method convertToFA =
      let open FiniteAutomatonView in
      self#operationRE "convert to FA";
      let auto = PolyModel.re2fa (re1 :> RegularExpression.model) in 
      let maton = auto#representation in 
        new FiniteAutomatonView.model (Representation (maton))

    method convertToPDA =
      let open PushdownAutomatonView in
      self#operationRE "convert to PDA";
      let pda = PolyModel.re2pda (re1 :> RegularExpression.model) in
      new PushdownAutomatonView.model (Representation (pda#representation))

	(* PEDRO CARLOS VER! porqu este e muitos outros nao existiam? *)
    method convertToCFG =
      let open ContextFreeGrammarView in
      self#operationRE "convert to CFG";
      let cfg = PolyModel.re2cfg (re1 :> RegularExpression.model) in
      new ContextFreeGrammarView.model (Representation (cfg#representation))

    method convertToGR =
      let open GrammarView in
      self#operationRE "convert to GR";
      let gr = PolyModel.re2gr (re1 :> RegularExpression.model) in
      new GrammarView.model (Representation (gr#representation))


    method convertToTM_SingleTape =
      let open TuringMachineView in
      self#operationRE "convert to TM single tape";
      let tm = PolyModel.re2tm (re1 :> RegularExpression.model) in
      new TuringMachineView.model (Representation (tm#representation))

    method model2Str =
      RegularExpression.toString !Ctrl.ctrlL#getRE#representation

    method updateButtons = 
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyExpressionButtons;

      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMConvertButtons;

      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons
    
    method getWords v = 
      self#operationRE "accepted words";
      let var = re1#staticGenerate v in
      let (_, visitedConfigs, exact, time) =
        re1#returnStats in
          HtmlPageClient.putWords var;
          HtmlPageClient.displayGenStats visitedConfigs exact time

    method editModel =
      !ListenersRE.editModelListener(); ()

    method replicateOnLeft =
      let c = new reController self#getRE false in
        Ctrl.ctrlL := (c :> controller)

    method printErrors =
      let errors = re1#errors in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors)


    (*JP*)
    
    method showTrace word =
      self#operationRE "trace";
      let w = str2word word in
      re1#staticAcceptFull w;
      let (accepted, configs, exact, time) = re1#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;
      re1#displayTrace

    method checkWord word =
      self#operationRE "checkWord";
      let w = str2word word in
      re1#staticAccept w;
      let (accepted, configs, exact, time) = re1#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;
end
