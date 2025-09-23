open OCamlFlat
open BasicTypes
open HtmlPageClient
open Lang
open JS
open CompositionView
open Controller
open Listeners
open Js_of_ocaml


class compController (comp: CompositionView.model) (s: bool) =
  object(self) inherit controller as super

    val comp1 = comp

    val side = s
    val cy = Some (if s then Cytoscape.startTree "cy2" else Cytoscape.startTree "cy")

    val mutable step = 0

    val mutable comp = comp
    val mutable resultTree = false
    val mutable wordAsList : word = []

    method model: Model.model = 
      (comp1 :> Model.model) 

    method getWordAsList () =
      wordAsList

    method getComp =
      comp1

    method getResultTree =
      resultTree

    method setComp newComp = 
      comp <- newComp

    method getModel = 
      comp1#toDisplayString "solution"

    method setTitle = 
      CtrlUtil.oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (Composition.kind)

    method operationComp opName : unit =
      super#operation opName "COMP"

    method returnType = Composition.kind

    method private makeTree cy (comp : Composition.t) =
      let nGetName = ref 0 in
      let genName () = 
        let prefix = "N" in 
        nGetName := !nGetName + 1;
        prefix ^ (string_of_int !nGetName)
      in
      let rec makeTree2 cy (comp: Composition.t) =
      match comp with
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
        | Intersect (l, r) -> let rootName = genName () in 
                        let rootL = makeTree2 cy l in 
                        let rootR = makeTree2 cy r in
                          Cytoscape.makeTreeNode cy rootName "^"; 
                          Cytoscape.makeTreeEdge cy rootName rootL;
                          Cytoscape.makeTreeEdge cy rootName rootR;
                          rootName
        | Star re -> let rootName = genName () in 
                        let root = makeTree2 cy re  in 
                          Cytoscape.makeTreeNode cy rootName "*"; 
                          Cytoscape.makeTreeEdge cy rootName root;
                          rootName
        | Rep rep -> let rootName = genName () in 
                    Cytoscape.makeTreeNode2 cy rootName (rep);
                    rootName
        | _ -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName "error";
                        rootName

       (* | Symb b -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName (symb2str b);
                        rootName
        | Empty  -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName "~";
                        rootName
        | Zero   -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName "!";
                        rootName*)
      in
      makeTree2 cy comp

    method private drawTree cy comp text =
      if String.length text >= 120 
      then Cytoscape.makeTreeNode cy "nope" (Lang.i18nAlertRETooBig ())
      else ignore (self#makeTree cy comp1#representation)

    method defineExample =
      self#operationComp "create";
      self#updateButtons;
      HtmlPageClient.putCyREButtons();
      HtmlPageClient.fitBoxRegex ();
      Cytoscape.fit self#getCy_opt;
      let test = Composition.toString comp1#representation in
       self#drawTree self#getCy comp1#representation test;
       HtmlPageClient.defineRE test side

    method defineExample2 =
      self#operationComp "create 2";
      let text = Composition.toString comp1#representation in
        self#drawTree self#getCy comp1#representation text;
        HtmlPageClient.defineRE text side

    method checkWord2 word = 
      self#operationComp "accept";
      let res = comp1#accept (str2word word) in
      (if res then JS.alertStr "true" else JS.alertStr "false");

    method checkWord word =
      self#operationComp "checkWord";
      let w = str2word word in
      comp1#staticAccept w;
      let (accepted, configs, exact, time) = comp1#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time;


      (* Lwt.return_false *)
      (*let w = str2word word in
        wordAsList <- w;
        CtrlUtil.twoBoxes self#getCy_opt;
        comp1#startAllTrees w;
        resultTree <- comp1#accept w;
        Ctrl.changeCtrlR ((new textController true) :> controller );
        Cytoscape.resetStyle !Ctrl.ctrlR#getCy Cytoscape.reStyle;
        if (resultTree) then
          (!ListenersRE.resultCountListener ();
          let right = comp1#getRightTrees in 
          ignore (comp1#printTree right (!Ctrl.ctrlR#getCy));
            !ListenersRE.defineNumberTreesListener ();
            HtmlPageClient.defineTreeButtons ();
            Lwt.return_true)
        else 
          (!ListenersRE.resultCountListener();
          let wrong = comp1#getWrongTrees in 
          ignore(comp1#printTree wrong (!Ctrl.ctrlR#getCy));
            !ListenersRE.defineNumberTreesListener ();
            HtmlPageClient.defineTreeButtons ();
            Lwt.return_false)*)


    method convertToRegExp = 
      let open RegularExpressionView in
      let re = comp1#evalRE in
        new RegularExpressionView.model (Representation re)

    method convertToFA =
      let open FiniteAutomatonView in
      let fa = comp1#evalFA in
        new FiniteAutomatonView.model (Representation fa)

    method convertToPDA =
      let open PushdownAutomatonView in
      let pda = comp1#evalPDA in
          new PushdownAutomatonView.model (Representation pda)

    method convertToCFG = 
      let open ContextFreeGrammarView in
      let cfg = comp1#evalCFG in
          new ContextFreeGrammarView.model (Representation cfg)

    method convertToTM = 
      let open TuringMachineView in
      let tm = comp1#evalTM in
          new TuringMachineView.model (Representation tm)

    method convertToGR = 
      let open GrammarView in
      let gr = comp1#evalGR in
          new GrammarView.model (Representation gr)

    method updateButtons =
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTM2TapesConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyPDAButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyGRConvertButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMConvertButtons; (* carolina *)
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGButtons; (*necessário ???*)
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listDisCompButtons
      
    
    method getWords v = 
      self#operationComp "accepted words";
        let var = comp1#generate v in 
          HtmlPageClient.putWords var

    method editModel =
      let open Js.Unsafe in
      let createModelPrep titleTxt otherTxt textAreaString okAction =
        let modelContent = HtmlPageClient.editModelContent titleTxt otherTxt textAreaString okAction in
        HtmlPageClient.setModal (Js.Unsafe.coerce modelContent);
        HtmlPageClient.showModalWindow () in

      let createModelPrepComp textAreaString okAction =
        createModelPrep (Lang.i18nMainTitleComp()) "" textAreaString okAction in
      
      createModelPrepComp (Composition.toString !Ctrl.ctrlL#getComp#representation) (fun () -> !ListenersComp.createModelListener())

     (* !ListenersRE.editModelListener(); ()*)

    method replicateOnLeft =
      let c = new compController self#getComp false in
        Ctrl.ctrlL := (c :> controller)

    method printErrors =
      let errors = comp1#errors in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors)
end
