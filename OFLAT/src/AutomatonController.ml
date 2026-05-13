open Controller
open AutomatonView
open HtmlPageClient
open Js_of_ocaml
open JS
open Listeners
open FiniteAutomatonView
open OCamlFlat
open Settings
open State

class virtual automatonController (s: bool)=
  object(self) inherit controller as super

    val side = s
    val cy = Some (if s then Cytoscape.initFaCy "cy2" else Cytoscape.initFaCy "cy")
    val mutable tab = false;

    method virtual defineInformationBox: unit
    method virtual loadButtons: unit
    method virtual getAutomaton: AutomatonView.model
    method virtual clearPoppers: unit

    method operationA opName : unit =
        super#operation opName "Automaton"

    method ctrlType: string = "automaton"

    method hasAcceptWord =
      not self#getAutomaton#isSentenceEmpty

    method clearAcceptWord =
      self#getAutomaton#changeTheTestingSentence "";

    method clearPoppers =
      self#getAutomaton#clearPoppers

    method resetStyle = Cytoscape.resetStyle self#getCy Cytoscape.faStyle

    (*TableView*)
    method getTab = tab
    method changeTab =
      tab <- not tab;
      JS.log("Tab changed to" ^ Bool.to_string tab)

    method defineExample =
      self#operationA "create example";
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      self#updateButtons;
      self#loadButtons;
      self#getAutomaton#drawExample self#getCy (Settings.getSetting "layout");
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt

    method defineExample2 = 
      self#getAutomaton#drawExample self#getCy (Settings.getSetting "layout");
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt

    method redrawLayout =
      self#operationA "redraw";
      HtmlPageClient.disableButton "autoAccept";
      HtmlPageClient.changeButtonColor "autoAccept" "";
      self#updateButtons;
      self#loadButtons;
      self#getAutomaton#redrawLayout self#getCy (Dom_html.getElementById "cy");
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt

    method getWords v = 
      self#operationA "accepted words";
      let var = self#getAutomaton#staticGenerate v in 
      let (_, visitedConfigs, exact, time) = self#getAutomaton#returnStats in
        HtmlPageClient.putWords var;
        HtmlPageClient.displayGenStats visitedConfigs exact time

    method showTrace word =
      self#operationA "trace";
      self#getAutomaton#changeTheTestingSentence word;
      self#getAutomaton#staticAcceptFull;
      let (accepted, configs, exact, time) = self#getAutomaton#returnStats in
        HtmlPageClient.displayAcceptStats accepted configs exact time;
        self#getAutomaton#displayTrace 
        
    method getNewSentence = 
      Js.string self#getAutomaton#newSentence

    method startStep word =
      self#operationA "accept start";
      self#cancelProm;
      HtmlPageClient.fitBoxRegex ();
      HtmlPageClient.enableButton "autoAccept";
      self#getAutomaton#changeTheTestingSentence word;
      self#getAutomaton#startAccept self#getCy;
      (Dom_html.getElementById "regExp")##.innerHTML := self#getNewSentence;
      let (accepted, configs, exact, time) = self#getAutomaton#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time
    
    method nextStep =
      self#operationA "accept next";
      self#cancelProm;
      HtmlPageClient.changeButtonColor "autoAccept" "crimson";
      self#getAutomaton#next self#getCy;
      (Dom_html.getElementById "regExp")##.innerHTML := self#getNewSentence

    method backStep = 
      self#operationA "accept back";
      self#cancelProm;
      HtmlPageClient.changeButtonColor "autoAccept" "crimson";
      self#getAutomaton#back self#getCy;
      (Dom_html.getElementById "regExp")##.innerHTML := self#getNewSentence

    method checkWord word =
      self#operationA "checkWord";
      HtmlPageClient.fitBoxRegex ();
      self#getAutomaton#changeTheTestingSentence word;
      (Dom_html.getElementById "regExp")##.innerHTML := Js.string word;
      self#getAutomaton#staticAccept;
      let (accepted, configs, exact, time) = self#getAutomaton#returnStats in
      HtmlPageClient.displayAcceptStats accepted configs exact time


    method autoAccept = 
      self#cancelProm;
      let rec tic n =
        match n with
        | true -> HtmlPageClient.changeButtonColor "autoAccept" "crimson";
                  Lwt.return()
        | false -> let prom = Lwt.bind 
                  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)
                  (fun () -> self#nextStep; tic self#getAutomaton#isOver)
      in
                  HtmlPageClient.changeButtonColor "autoAccept" "green";
                  self#changeProm prom;
                  prom
      in
      ignore(tic (self#promState = Lwt.Sleep));  
      Lwt.return_true

    method changeToEditModelMode =
      self#operationA "editModel";
      (Dom_html.getElementById "regExp")##.innerHTML := Js.string "";
      self#getAutomaton#changeToEditModelMode self#getCy

(*    method printErrors =
      let errors = self#getAutomaton#errors in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors)
*)
    method updateRight =
      if !Ctrl.ctrlR#getUpdateType = Some "specification"
      then !Listeners.showModelListener ()
end
