open OCamlFlat
open HtmlPageClient
open Controller

class exerController (exer: Exercise.exercise) (on: bool) (title: string) =
  object(self) inherit controller as super

    val exer1 = exer

    val title1 = title
    
    val cy = Some (Cytoscape.initCy "cy2") (*TODO Check if needed*)
    
    method locked = true (*Exercises always located on the right*)

    method getExercise =
      exer1

    method setTitle = 
      HtmlPageClient.defineMainTitle (title1)

    method operationEXER opName : unit =
      super#operation opName "Exercise"

    method returnType = RegularExpression.kind

    method defineExample2 =
      self#operationEXER "create";
      CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
      HtmlPageClient.addEnumTitle();
      let prob = exer1#representation.problem in
        HtmlPageClient.defineEnumProblem prob;
      HtmlPageClient.addPropertiesBox ();
      Set.iter (fun el -> HtmlPageClient.createPropertiesList el "nothing" "properties") exer1#representation.properties;
      HtmlPageClient.addAcceptedTitle ();
      Set.iter (fun el -> HtmlPageClient.createSpanList el "nothing" "inside") exer1#representation.inside;
      HtmlPageClient.addNonAcceptTitle ();
      Set.iter (fun el -> HtmlPageClient.createSpanList el "nothing" "outside") exer1#representation.outside;
      HtmlPageClient.addEnumCheckButton ()

    method checkHelper result (insideErrors, outsideErrors, properties) = 
    self#operationEXER "check exercise";
          HtmlPageClient.defineResult result;
            Set.iter (fun el -> 
              if Set.belongs el properties then HtmlPageClient.createPropertiesList el "error" "properties" 
              else HtmlPageClient.createPropertiesList el "right" "properties") exer1#representation.properties;
            Set.iter (fun el -> 
              if Set.belongs el insideErrors then HtmlPageClient.createSpanList el "error" "inside" 
              else HtmlPageClient.createSpanList el "right" "inside") exer1#representation.inside;
            Set.iter (fun el -> 
              if Set.belongs el outsideErrors then HtmlPageClient.createSpanList el "error" "outside" 
              else HtmlPageClient.createSpanList el "right" "outside") exer1#representation.outside
end
