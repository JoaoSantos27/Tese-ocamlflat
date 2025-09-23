(*
 * Calling.ml
 *
 * This file is part of the OFLAT app
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Rita Macedo
 *)

(* Description: This a component of the controller that is used for the
 * JavaScript to call OCaml functions in the Controller.
 *)

open OCamlFlat
open BasicTypes
open Js_of_ocaml
open ViewUtil
open Controller
open HtmlPageClient
open Lang
open Listeners
open RegularExpressionView
open JS
open FiniteAutomatonView
open ContextFreeGrammarView
open Lang
open ControllerListeners

module ExamplesView =
struct
	let examplesDiv () =
		Dom_html.getElementById "examplesServer"
	
	let createDivForTitle i title = 
		let titleDiv = HtmlPageClient.div ("title" ^ string_of_int i) in
		titleDiv##.innerHTML := Js.string title;
		titleDiv##.style##.cssText := Js.string "color: white; font-size: 11px; font-weight: bold; padding-left: 20px";
		Dom.appendChild (examplesDiv ()) titleDiv |> ignore

	let createDivForButtons i examples = 
		let buttonsDiv = HtmlPageClient.div ("button" ^ string_of_int i) in
		buttonsDiv##.style##.cssText := Js.string "padding-top: 2px; padding-bottom: 20px;";
		List.iter (fun (name, model) -> 
			let button = HtmlPageClient.createServerExampleButton name in
			Dom.appendChild buttonsDiv button |> ignore; 
		) examples;
		Dom.appendChild (examplesDiv ()) buttonsDiv |> ignore
	
	let handleKind i (title, examples) =
		createDivForTitle i title;
		createDivForButtons i examples

	let examplesView () =
		List.iteri handleKind Examples.examplesTable
end

module JSCallingOCaml =
struct

	let errorViewer mesg =
		JS.alertStr mesg

	let start () =
		Error.setViewer errorViewer;
		if (!Lang.lang <> "en" && !Lang.lang <> "pt" && !Lang.lang <> "fr") then
			Lang.lang := "en";
		HtmlPageClient.changeLang();
		ExamplesView.examplesView ()
	;;
(*
		let start _ =
			Error.setViewer errorViewer;
			if (!Lang.lang <> "en" && !Lang.lang <> "pt" && !Lang.lang <> "fr") then
				Lang.lang := "en";
			HtmlPageClient.changeLang();
			
			let examples = Dom_html.getElementById "examplesServer" in
			(* examples##.innerHTML := Js.string ""; *)
			let lis = Examples.examplesTable in
			let titles = [| "Finite Automaton"; "Regular Expressions";
											"Context Free Grammars"; "Pushdown Automaton";
											"Turing Machine"; "Composition";
		(* PEDRO CARLOS  VER! *)								"Grammars"; "Exercises" |] in
			List.iteri (fun i el ->
				(* Create a div for the title *)
				let titleDiv = HtmlPageClient.div ("title" ^ string_of_int i) in
				titleDiv##.innerHTML := Js.string titles.(i);
				titleDiv##.style##.cssText := Js.string "color: white; font-size: 11px; font-weight: bold; padding-left: 20px";
				Dom.appendChild examples titleDiv |> ignore; 
		
				(* Create a div for the buttons in this category *)
				let buttonsDiv = HtmlPageClient.div ("button" ^ string_of_int i) in
				buttonsDiv##.style##.cssText := Js.string "padding-top: 2px; padding-bottom: 20px;";
				List.iter (fun ex -> 
					let button = HtmlPageClient.createServerExampleButton (fst ex) in
					Dom.appendChild buttonsDiv button |> ignore; 
				) el;
				Dom.appendChild examples buttonsDiv |> ignore; 
			) lis;
			()
		;;
*)

	Js.Unsafe.global##.jscode :=
		object%js
		
		  method newModel =
			!Listeners.createModelListener()
		  
		  method editModel =
			!Listeners.editModelListener()
					  	(* !Ctrl.ctrlL#editModel *)

			
		  method getModel =
			let automaton = !Ctrl.ctrlL#getModel in
			ViewUtil.bcSend automaton

		  method fitGraph =
			Cytoscape.fit !Ctrl.ctrlL#getCy_opt
		  
		  method generateWords =
			let text = JS.prompt (i18nTextMaximumSize ()) "4" in 
			CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
			!CtrlUtil.changeToControllerCtrlRight();
			match Js.Opt.to_option text with
			| None -> ()
			| Some v -> 
				let size = int_of_string (Js.to_string v) in 
				  !Ctrl.ctrlL#getWords size

              method auto =
                !Ctrl.ctrlL#autoAccept
		  
		  method test =
			CtrlUtil.oneBox !Ctrl.ctrlL#getCy_opt;
			let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "ab" in 
			match Js.Opt.to_option text with
                | None -> ()
                | Some v -> !Ctrl.ctrlL#checkWord (Js.to_string v)

              method trace =
                CtrlUtil.twoBoxes !Ctrl.ctrlL#getCy_opt;
                let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "ab" in 
                match Js.Opt.to_option text with
                | None -> ()
                | Some v -> !Ctrl.ctrlL#showTrace (Js.to_string v)
			
		  method stepbystep =
			let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "ab" in 
			match Js.Opt.to_option text with
			| None -> JS.log "No input provided"
			| Some v -> 
					JS.log ("Input provided: " ^ (Js.to_string v));
					!Ctrl.ctrlL#startStep (Js.to_string v)

		  method backwards =
			JS.log "backwards";
			!Ctrl.ctrlL#backStep
		  
		  method forward =
			JS.log "forward";
			!Ctrl.ctrlL#nextStep

		  method selectConversions n =
			ControllerListeners.conversionTo n

		  method readFromFile n =
			let str = Js.to_string n in 
			!Listeners.openEntityListener str

		  method exportToFile =
			let json = JSon.toString ((!Ctrl.ctrlL#model)#toJSon) in
			let json = Js.to_string ((Js.encodeURIComponent (Js.string json))) in
			let element = Dom_html.document##createElement (Js.string "a") in
			let modelName = 
			  begin match !Ctrl.ctrlL#model#id.name with
			  | "_" | "" -> "oflatModel"
			  | a -> a
			  end ^ ".json"
			in
			element##setAttribute (Js.string "href") (Js.string ("data:application/json," ^ json));
			element##setAttribute (Js.string "download") (Js.string modelName);
			element##.style##.display := Js.string "none";
			let node = Dom_html.document##.body##appendChild (Js.Unsafe.coerce element) in
			element##click;
			Dom_html.document##.body##removeChild node

		  method feedback =
			HtmlPageClient.clearBox1 ();
			!CtrlUtil.changeToControllerCtrlLeft();
			!Ctrl.ctrlL#feedback
		  
		  method about  =
			HtmlPageClient.clearBox1 ();
			!CtrlUtil.changeToControllerCtrlLeft();
			!Ctrl.ctrlL#about

              method settings =
              HtmlPageClient.settings()

              method tooltipSettings =
                let textBox = Dom_html.getElementById "tooltipSettings" in
                textBox##.innerHTML := Js.string "Change Settings" (*TODO: LANG*)

		  method tooltipNewModel =
			let textBox = Dom_html.getElementById "tooltipNewModel" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipNewModel ())

		  method tooltipEditModel =
			let textBox = Dom_html.getElementById "tooltipEditModel" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipEditModel ())
		  
		  method tooltipFitGraph =
			let textBox = Dom_html.getElementById "tooltipFitGraph" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipFitGraph ())

		  method tooltipGenerate =
			let textBox = Dom_html.getElementById "tooltipGenerate" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipGenerate ())
		  
		  method tooltipTest =
			let textBox = Dom_html.getElementById "tooltipTest" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipTest ())

              method tooltipTrace =
                let textBox = Dom_html.getElementById "tooltipTrace" in
                textBox##.innerHTML := Js.string ("Show Acceptance Path")
		  
		  method tooltipStep =
			let textBox = Dom_html.getElementById "tooltipStep" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipStep ())

		  method tooltipClear =
			let textBox = Dom_html.getElementById "tooltipClear" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipClear ())

		  method tooltipConvert = 
			let textBox = Dom_html.getElementById "tooltipConvert" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipConvert ())

		  method tooltipFile = 
			let textBox = Dom_html.getElementById "tooltipFile" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipFile ())
			
		  method tooltipExportModel =
			let textBox = Dom_html.getElementById "tooltipExportModel" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipExportModel ())

		  method tooltipAbout =
			let textBox = Dom_html.getElementById "tooltipAbout" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipAbout ())

		  method tooltipFeedback =
			let textBox = Dom_html.getElementById "tooltipFeedback" in
			textBox##.innerHTML := Js.string (Lang.i18nTooltipFeedback ())

		  method start =
			start ()
	end
end
