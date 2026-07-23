open LocalStorage
open HtmlPageClient
open Lang
open StateVariables
open ViewUtil
open Listeners
open Js_of_ocaml
open JS
open OCamlFlat
open AutomatonView
open FiniteAutomatonView
open TransducerView
open PushdownAutomatonView
open RegularExpressionView
open ContextFreeGrammarBasicView
open ContextFreeGrammarView
open ContextFreeGrammarLL1View
open GrammarView
open TuringMachineView
open CompositionView
open BasicTypes

module Settings = struct

	let changeLang () =
	    ViewUtil.changeLang !Lang.lang;
	    HtmlPageClient.putInnerHtml "title" (Lang.i18nTitle ());
	    HtmlPageClient.putInnerHtml "version" (Lang.i18nVersion ());

	    HtmlPageClient.putInnerHtml "optionNewDefault" (Lang.i18nNewModel ());
      HtmlPageClient.putInnerHtml "optionNewAutomatonFA" (Lang.i18nMainTitle1());
      HtmlPageClient.putInnerHtml "optionNewAutomatonFST" (Lang.i18nMainTitleFST()); 
      HtmlPageClient.putInnerHtml "optionNewAutomatonPDA" (Lang.i18nMainTitlePDA());
	    HtmlPageClient.putInnerHtml "optionNewRegularExpression" (Lang.i18nMainTitle2());
	    HtmlPageClient.putInnerHtml "optionNewContextFreeGrammar" (Lang.i18nMainTitle4());
	    HtmlPageClient.putInnerHtml "optionNewComposition" (Lang.i18nMainTitleComp());
	    HtmlPageClient.putInnerHtml "optionNewGrammar" (Lang.i18nMainTitle5());
	    HtmlPageClient.putInnerHtml "optionNewTuringMachine" (Lang.i18nselectTM());

	    HtmlPageClient.putInnerHtml "editModel" (Lang.i18nEditModel ());
	    HtmlPageClient.putInnerHtml "fitGraph" (Lang.i18nFitGraph ());
	    HtmlPageClient.putInnerHtml "generate" (Lang.i18nGenerate ());
	    HtmlPageClient.putInnerHtml "testing" (Lang.i18nTesting ());
	    HtmlPageClient.putInnerHtml "trace" "Trace";
	    HtmlPageClient.putInnerHtml "step" (Lang.i18nStep ());
	    HtmlPageClient.putInnerHtml "start" (Lang.i18nStart ());
	        
	    HtmlPageClient.putInnerHtml "selectRegex" (Lang.i18nSelectRegex ());
      HtmlPageClient.putInnerHtml "selectFA" (Lang.i18nselectFA ());
      HtmlPageClient.putInnerHtml "selectFST" (Lang.i18nselectFST ()); 
      HtmlPageClient.putInnerHtml "selectPDA" (Lang.i18nselectPDA ());
	    HtmlPageClient.putInnerHtml "selectCFG" (Lang.i18nselectCFG ());
      HtmlPageClient.putInnerHtml "selectGR" (Lang.i18nselectGR ());
	    HtmlPageClient.putInnerHtml "selectTM" (Lang.i18nselectTM ()); (* carolina *)
      HtmlPageClient.putInnerHtml "selectTM2Tapes" (Lang.i18nselectTM2Tapes ());
	    HtmlPageClient.putInnerHtml "selectConv" (Lang.i18nSelectConv ());

	    HtmlPageClient.putInnerHtml "importModel" (Lang.i18nImportModel ());
	    HtmlPageClient.putInnerHtml "exportModel" (Lang.i18nExportModel ());
	    HtmlPageClient.putInnerHtml "saveModel" (Lang.i18nSaveModel ());
	    HtmlPageClient.putInnerHtml "deleteModels" (Lang.i18nDeleteModels ());
	    HtmlPageClient.putInnerHtml "server" (Lang.i18nServer ());     

	    (* HtmlPageClient.putInnerHtml "selectedL" (Lang.i18nSelectedL ());   
	    HtmlPageClient.putInnerHtml "selectPT" (Lang.i18nSelectPT ());
	    HtmlPageClient.putInnerHtml "selectEN" (Lang.i18nSelectEN ());
	    HtmlPageClient.putInnerHtml "selectFR" (Lang.i18nSelectFR ()); *)

	    HtmlPageClient.putInnerHtml "about" (Lang.i18nAbout ());
	    HtmlPageClient.putInnerHtml "feedback" (Lang.i18nFeedback ());
	    HtmlPageClient.putInnerHtml "settings" "Settings" (*TODO: LANG*);
	        
	    HtmlPageClient.putInnerHtml "developed" (Lang.i18nDeveloped ());
	    HtmlPageClient.putInnerHtml "footerButton0" (Lang.i18nNovaLincs () );
	    HtmlPageClient.putInnerHtml "project" (Lang.i18nProject ());
	    HtmlPageClient.putInnerHtml "footerButton3" (Lang.i18nFactor ());
	    HtmlPageClient.putInnerHtml "and" (Lang.i18nAnd ());
	    (* HtmlPageClient.putInnerHtml "leaf" (Lang.i18nLeafs ()); *)
	    HtmlPageClient.putInnerHtml "financing" (Lang.i18nFinancing ());
	    HtmlPageClient.putInnerHtml "footerButton1" (Lang.i18nFooter ());
	    HtmlPageClient.putInnerHtml "and1" (Lang.i18nAnd ());
	    HtmlPageClient.putInnerHtml "footerButton2" (Lang.i18nFooter1 ());

	    if (StateVariables.getCy1Type() = StateVariables.getAutomatonType() || StateVariables.getCy1Type() = StateVariables.getTransducerType()) then
	      (HtmlPageClient.putInnerHtml "tooltipCloseLeft" (Lang.i18nTooltipCloseLeft ());
	      HtmlPageClient.putInnerHtmlButtons "save" (Lang.i18nSave ()) "tooltipSpecification" "tooltiptext1" (Lang.i18nTooltipSpecification ());
	      HtmlPageClient.putInnerHtmlButtons "formatting" (Lang.i18nFormatting ()) "tooltipSpecification" "tooltiptext2" (Lang.i18nTooltipSpecification ());
	      HtmlPageClient.putInnerHtmlButtons "showHelpModel" (Lang.i18nShowHelpModel ()) "tooltipShowHelp" "tooltiptext2" (Lang.i18nTooltipShowHelpModel ());
	      HtmlPageClient.putInnerHtmlButtons "clean" (Lang.i18nClean ()) "tooltipClean" "tooltiptext3" (Lang.i18nTooltipClean ());
	      HtmlPageClient.putInnerHtmlButtons "deterministic" (Lang.i18nDeterministic ()) "tooltipDeterministic" "tooltiptext3" (Lang.i18nTooltipDeterministic ());
	      HtmlPageClient.putInnerHtmlButtons "minimize" (Lang.i18nMinimize ()) "tooltipMinimize" "tooltiptext3" (Lang.i18nTooltipMinimize ());
	      HtmlPageClient.putInnerHtmlButtons "productive" (Lang.i18nProductive ()) "tooltipProductive" "tooltiptext3" (Lang.i18nTooltipProductive ());
	      HtmlPageClient.putInnerHtmlButtons "accessible" (Lang.i18nAccessible ()) "tooltipAccessible" "tooltiptext3" (Lang.i18nTooltipAccessible ());
	      HtmlPageClient.putInnerHtmlButtons "useful" (Lang.i18nUseful ()) "tooltipUseful" "tooltiptext3" (Lang.i18nTooltipUseful ());
	      HtmlPageClient.putInnerHtml "infoBox" "";
	      !Listeners.defineInformationBoxListener());

	    if (StateVariables.getCy1Type() = StateVariables.getRegexType()) then
	      (HtmlPageClient.putInnerHtml "tooltipCloseLeft" (Lang.i18nTooltipCloseLeft ());
	      HtmlPageClient.putInnerHtmlButtons "changeDirection" (Lang.i18nDirection ()) "tooltipDirection" "tooltiptext2" (Lang.i18nTooltipDirection ()));

	    if (StateVariables.getCy2Type() = StateVariables.getEnumerationType()) then
	      (HtmlPageClient.putInnerHtml "enumVerify" (Lang.i18nVerify ());

	      let prob = (StateVariables.returnEnum())#representation.problem in
	        let prob1 = (Lang.i18nProblem ()) ^ prob in
	        HtmlPageClient.putInnerHtml "prob" prob1;
	      HtmlPageClient.putInnerHtml "enum" (Lang.i18nEnumTitle ());
	      HtmlPageClient.putInnerHtml "accept" (Lang.i18nAcceptedWords ());
	      HtmlPageClient.putInnerHtml "notAccept" (Lang.i18nNonAccepted ());
	      if Dom_html.getElementById_opt "correct" <> None then
	        HtmlPageClient.putInnerHtml "correct" (Lang.i18nRight ());
	      if Dom_html.getElementById_opt "wrong" <> None then
	        HtmlPageClient.putInnerHtml "wrong" (Lang.i18nWrong ());
	      );

	    if (StateVariables.getCy2Type() = StateVariables.getInfoType()) then
	      (HtmlPageClient.putInnerHtml "generateWords" (Lang.i18nGenerateWords ());
	      HtmlPageClient.putInnerHtml "tooltipCloseRight" (Lang.i18nTooltipCloseRight ());
	      );

	    if (StateVariables.getCy2Type() = StateVariables.getVerifyType()) then
	      (HtmlPageClient.putInnerHtml "textBox" "";
	      !ListenersRE.resultCountListener ();
	      !ListenersRE.defineNumberTreesListener ();
	      HtmlPageClient.defineTreeButtons ();
	      HtmlPageClient.putInnerHtml "tooltipCloseRight" (Lang.i18nTooltipCloseRight ());
	      );

	      if (StateVariables.getCy1Type() = StateVariables.getFeedbackType()) then
	      (HtmlPageClient.putInnerHtml "feedbackText" (Lang.i18nFeedbackText ());
	       HtmlPageClient.putInnerHtml "feedbackText2" (Lang.i18nFeedbackText2 ());
	       HtmlPageClient.putInnerHtml "feedbackThankYou" (Lang.i18nFeedbackThankYou ());
	      );

	   if (StateVariables.getCy1Type() = StateVariables.getInfoType ()) then
	       (HtmlPageClient.putInnerHtml "aboutSubtitle" (Lang.i18nAboutSubtitle ());
	        HtmlPageClient.putInnerHtml "aboutSubtitle2" (Lang.i18nAboutSubtitle2 ());
	        HtmlPageClient.putInnerHtml "aboutText1" (Lang.i18nAboutText1 ());
	        HtmlPageClient.putInnerHtml "aboutText2" (Lang.i18nAboutText2 ());
	        HtmlPageClient.putInnerHtml "aaa" (Lang.i18nAboutText16 ());
	        HtmlPageClient.putInnerHtml "bbb" (Lang.i18nAboutText3 ());
	        HtmlPageClient.putInnerHtml "aboutText4" (Lang.i18nAboutText4 ());
	        HtmlPageClient.putInnerHtml "aboutText5" (Lang.i18nAboutText5 ());
	        HtmlPageClient.putInnerHtml "aboutText6" (Lang.i18nAboutText6 ());
	        HtmlPageClient.putInnerHtml "aboutText7" (Lang.i18nAboutText7 ());
	        HtmlPageClient.putInnerHtml "aboutText8" (Lang.i18nAboutText8 ());
	        HtmlPageClient.putInnerHtml "aboutText9" (Lang.i18nAboutText9 ());
	        HtmlPageClient.putInnerHtml "aboutText10" (Lang.i18nAboutText10 ());
	        HtmlPageClient.putInnerHtml "aboutText11" (Lang.i18nAboutText11 ());
	        HtmlPageClient.putInnerHtml "aboutText12" (Lang.i18nAboutText12 ());
	        HtmlPageClient.putInnerHtml "aboutText13" (Lang.i18nAboutText13 ());
	        HtmlPageClient.putInnerHtml "aboutText14" (Lang.i18nAboutText14 ());
	        HtmlPageClient.putInnerHtml "aboutText15" (Lang.i18nAboutText15 ());
	        HtmlPageClient.putInnerHtml "aboutText16" (Lang.i18nAboutText16 ());
	        HtmlPageClient.putInnerHtml "tezos" (Lang.i18nFooter ());
			HtmlPageClient.putInnerHtml "inria" (Lang.i18nFooter1 ()); 
	       )

	let rec find_position key l =
		match l with
		  | [] -> 0
		  | x::xs -> if x = key then 0 else 1 + find_position key xs

	(* Language settings *)
	let languageOptions = ["EN"; "PT"; "FR"]

	let setLanguageSettings settingsDiv =
		let spanL = HtmlPageClient.span "settingsText" (Lang.i18nChangeLang ()) in
			Dom.appendChild settingsDiv spanL;
		let selectLanguage = HtmlPageClient.select "selectLanguage" languageOptions in
		let langVal = String.uppercase_ascii (LocalStorage.getItem "lang") in
		if langVal = "" then selectLanguage##.selectedIndex := 0 
						else selectLanguage##.selectedIndex := find_position langVal languageOptions;
		selectLanguage##.onchange := Dom.handler (fun _ ->
		  let langString = String.lowercase_ascii (Js.to_string selectLanguage##.value) in
		    Lang.set_language (Js.string langString); 
		    LocalStorage.setItem "lang" langString;
		    changeLang();
		    Js._true       
		  );
		Dom.appendChild settingsDiv selectLanguage

	(* Empty settings *)
	let emptyOptions = ["~"; "ε"; "λ"]

	let setEmptySettings settingsDiv =
		let span1 = HtmlPageClient.span "settingsText" (Lang.i18nChangeEmpty ()) in
			Dom.appendChild settingsDiv span1;
		let selectEmpty = HtmlPageClient.select "selectEmpty" emptyOptions in 
		let emptyVal = LocalStorage.getItem "empty" in
		if emptyVal = "" then selectEmpty##.selectedIndex := 0 
						 else selectEmpty##.selectedIndex := find_position emptyVal emptyOptions;
		selectEmpty##.onchange := Dom.handler (fun _ -> 
			StateVariables.changeEmpty (Js.to_string selectEmpty##.value);
			LocalStorage.setItem "empty" (Js.to_string selectEmpty##.value);
			Js._true);
		Dom.appendChild settingsDiv selectEmpty

	(* Layout settings *)
	let layoutOptions = ["random"; "circle"; "grid"]

	let setLayoutSettings settingsDiv =
		let span1 = HtmlPageClient.span "settingsText" (Lang.i18nChangeLayout ()) in
			Dom.appendChild settingsDiv span1;
		let selectLayout = HtmlPageClient.select "selectLayout" layoutOptions in
		let layoutVal = LocalStorage.getItem "layout" in
		if layoutVal = "" then selectLayout##.selectedIndex := 0 
						  else selectLayout##.selectedIndex := find_position layoutVal layoutOptions;
		selectLayout##.onchange := Dom.handler (fun _ -> 
			LocalStorage.setItem "layout" (Js.to_string selectLayout##.value);
			Js._true);
		Dom.appendChild settingsDiv selectLayout

	let openSettingsPage () =
		let buttonBox = Dom_html.getElementById "buttonBox" in
		let settingsDiv = HtmlPageClient.div "settings" in
			Dom.appendChild buttonBox settingsDiv;
			Dom.appendChild settingsDiv (HtmlPageClient.closeButtonSide());
		let h1 = HtmlPageClient.h2 "settingsIntro" "Customizable Settings" in
			Dom.appendChild settingsDiv h1;
			setLanguageSettings settingsDiv;
			setEmptySettings settingsDiv;
			setLayoutSettings settingsDiv

	let getSetting key =
		LocalStorage.getItem key

	(* Saved models *)

	let createModel model: Model.model =
	    let kind = JSon.fieldString model "kind" in
			(match kind with
			| k when k = FiniteAutomaton.kind -> 
			    (new FiniteAutomatonView.model (JSon model) :> Model.model)
			| k when k = RegularExpression.kind -> 
			    (new RegularExpressionView.model (JSon model) :> Model.model)
			| k when k = PushdownAutomaton.kind -> 
          (new PushdownAutomatonView.model (JSon model) :> Model.model)
      | k when k = Transducer.kind -> 
          (new TransducerView.model (JSon model) :> Model.model)
      | k when k = Grammar.kind ->
					(new GrammarView.model (JSon model) :> Model.model)
			| k when k = ContextFreeGrammar.kind ->
			    (new ContextFreeGrammarView.model (JSon model) :> Model.model)
			| k when k = TuringMachine.kind ->
			    (new TuringMachineView.model (JSon model) :> Model.model)
			| k when k = Composition.kind ->
			    (new CompositionView.model (JSon model) :> Model.model)
			)

	let getRepository () =
		let repository = LocalStorage.getItem "repository" in
  			if repository = "" then "{}" else repository

  	let isEpsilon word =
      let word = String.trim word in
      let epsilonAliases = [
        "~";
        "\206\181"; (* epsilon *)
        "\206\187"; (* lambda *)
        "\195\142\194\181"; (* mojibake epsilon kept for existing data *)
        "\195\142\194\187"  (* mojibake lambda kept for existing data *)
      ] in
      List.mem word emptyOptions || List.mem word epsilonAliases

	let saveModel name model =
  		let repoJson = match JSon.parse (getRepository()) with
			  			| JList modelList -> modelList
			  			| _ -> [] 
  		in
	  		let jsonModel = JSon.JList [JString name; model] in
	  		let updRep = JSon.toString (JSon.JList (repoJson @ [jsonModel])) in
	  			LocalStorage.setItem "repository" updRep

  	let extractNameModel jsonObj =
		match JSon.parse jsonObj with
			| JList [JString name; modelObj] -> (name, modelObj)
			| _ -> ("", JString "")

	let loadSavedModels () =
	  	match JSon.parse (getRepository()) with
		    | JList modelList ->
		    	List.iter (fun (jsonObj) -> 
					let (name, model) = extractNameModel (JSon.toString jsonObj) in
						Repository.updateModel name (createModel model);
						HtmlPageClient.putExample name
		        ) modelList
		    | _ -> ()

	let clearAllModels () =
		LocalStorage.removeItem "repository";
		HtmlPageClient.removeExamples ((Repository.getSize ())-1);
		Repository.clear ()

	(* Settings and other methods that need to run when starting *)

	let setSettings lang empty =
		Lang.set_language (Js.string lang);
		changeLang();
		StateVariables.changeEmpty empty

	let startup () =
		let langVal = LocalStorage.getItem "lang" in
		let lang = if langVal = "" then "en" else langVal in
		let emptyVal = LocalStorage.getItem "empty" in
		let empty = if emptyVal = "" then "~" else emptyVal in
			setSettings lang empty;
      		loadSavedModels ()
end
