(*
 * JS.ml
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

open Js_of_ocaml
open OCamlFlat
open BasicTypes

module JS = 
struct 
    let global =
      Js.Unsafe.global
  
    let eval s =
      Js.Unsafe.eval_string s
      
    let exec s =
      ignore (eval s)

    let console =
      Console.console

    let string s =
      Js.string s

    let log j =
      ignore (console##log j)

    let alert j =
      ignore (global##alert j)

    let alertStr s =
      alert (Js.string s)

    let prompt text default =
      Dom_html.window##prompt (Js.string text) (Js.string default)

    let confirm text =
      Js.to_bool (Dom_html.window##confirm (Js.string text))

(*    let prompt text default =*)
(*        Js.Opt.get*)
(*            (Dom_html.window##prompt (Js.string text) (Js.string default))*)
(*                  (fun () -> Error.fatal "teste")*)
(*                  |> Js.to_string*)
  
end

module type HTMLTableSig =
sig
	type t
  type r
  type c
  type tableType
	val create: string -> string -> t
  val insertRow: t -> r
  val insertCell: r -> string -> string -> c
  val modifyCell: c -> string -> c
  val modifyRowId: r -> string -> unit
  val tableExists: string -> bool
  val tableFilled: t -> bool
  val cToCell : c -> Dom_html.tableCellElement Js.t
  val rToRow : r -> Dom_html.tableRowElement Js.t
  val tToTable: t -> Dom_html.tableElement Js.t
  val toTable: Dom_html.element Js.t -> t
  val fetchTable : string -> t
  val fetchRow: t -> int -> r Js.opt
  val fetchCell: r -> int -> c Js.opt
  val fetchLastRow: t -> r Js.opt
  val setWidth: t -> string -> unit
  val addEventListener: r -> string -> 'a -> unit
  val appendChildtoCell: c -> Dom_html.element Js.t -> unit
  val appendChildtoRow: r -> Dom_html.element Js.t -> unit
  val appendChildtoTable: t -> Dom_html.element Js.t -> unit
  val lowLevelApply: t -> (_ Js.t -> unit) -> unit
  val changeDisplay: t -> string -> unit
  val buildTable : string list list -> string -> string -> unit
  val makeFAStateRow: OCamlFlat.BasicTypes.state ->
    'a list ->
    (OCamlFlat.BasicTypes.state * 'a * OCamlFlat.BasicTypes.state)
    OCamlFlat.Set.t -> string list
  val makePDAStateRow: OCamlFlat.BasicTypes.state ->
    OCamlFlat.BasicTypes.symbol ->
    'a list ->
    (OCamlFlat.BasicTypes.state * OCamlFlat.BasicTypes.symbol * 'a *
     OCamlFlat.BasicTypes.state * OCamlFlat.BasicTypes.symbol list)
    OCamlFlat.Set.t -> string list
  val makeFAPath: (OCamlFlat.BasicTypes.state * OCamlFlat.BasicTypes.word) list 
      -> int -> string list list
  val makePDAPath: (OCamlFlat.BasicTypes.state * OCamlFlat.BasicTypes.symbol list *
      OCamlFlat.BasicTypes.word) list 
      -> int -> string list list
  val makeCFGPath: (OCamlFlat.BasicTypes.word * OCamlFlat.BasicTypes.word) list 
      -> int -> string list list
  val makeGRPath: (OCamlFlat.BasicTypes.word * OCamlFlat.BasicTypes.word) list 
      -> int -> string list list
  val makeREPath: (OCamlFlat.RegularExpression.t * OCamlFlat.BasicTypes.word) list 
      -> int -> string list list
  val makeTMPath: (OCamlFlat.TuringMachine.path) -> int -> string list list
  val paint : t -> int -> int -> string -> unit
end

module HTMLTable : HTMLTableSig = 
struct
	(* https://ocsigen.org/js_of_ocaml/latest/api/js_of_ocaml/Js_of_ocaml/Dom_html/ *)
	type t = Dom_html.tableElement Js.t
	type r = Dom_html.tableRowElement Js.t
	type c = Dom_html.tableCellElement Js.t 
  type tableType = Table of t | Row of r | Cell of c

	let create (id: string) (parent: string) : t =
		let doc = Dom_html.document in
    let parentElement = Dom_html.getElementById parent in
		let table = Dom_html.createTable doc in
			table##.id := Js.string id;
      Dom.appendChild parentElement table;
			table

  let insertRow (table: t) : r =
    table##insertRow(-1)

  let insertCell (row: r) (id: string) (contents: string) : c =
    let newCell = row##insertCell (-1) in
      newCell##.id := Js.string id;
      newCell##.innerHTML := Js.string contents;
      newCell##.classList##add(Js.string "monospaceClass");
      newCell

  (* let insertCellHTML (cell: c) (contents: string) =
    let newCell = modifyCell cell "" in
      let firstChar = String.get contents 0 in
      let charSpan = Dom_html.createSpan doc in
        charSpan##.id := Js.string "firstChar";
        charSpan##.innerHTML := Js.string (String.make 1 firstChar);
        Dom.appendChild newCell charSpan;
      let subString = String.sub contents 1 ((String.length contents) - 1) in
      let stringSpan = Dom_html.createSpan doc in
        stringSpan##.id := Js.string "restOfString"; subString in
        Dom.appendChild newCell stringSpan; *)

  let modifyCell (cell: c) (contents: string) : c =
      cell##.innerHTML := Js.string contents;
      cell

  let tableFilled (table: t) : bool =
    if table##.rows##.length = 0 then true else false

  let tableExists (table: string) : bool =
    let _ = Dom_html.document in 
    let currTab = Dom_html.getElementById_opt table in
    (match currTab with
    | None -> false
    | Some a -> true)

    let toTable (element: Dom_html.element Js.t) : t =
      Js.Unsafe.coerce (element)

    let fetchTable (id: string) : t =
        toTable(Dom_html.getElementById(id))

    let fetchRow (table: t) (index: int) : r Js.opt=
        table##.rows##item(index)
      
    let fetchCell (row :r) (index: int) : c Js.opt =
        row##.cells##item(index)

    let fetchLastRow (table: t) : r Js.opt =
        let length = table##.rows##.length in
        fetchRow table (length-1)
    
    let cToCell (cell: c) : Dom_html.tableCellElement Js.t =
      Js.Unsafe.coerce (cell)

    let rToRow (row: r) : Dom_html.tableRowElement Js.t =
      Js.Unsafe.coerce (row)

    let tToTable (table: t) : Dom_html.tableElement Js.t =
      Js.Unsafe.coerce (table)

    let setWidth (table: t) (width: string) =
      table##.width := (Js.string width)
    
    let modifyRowId (row: r) (id: string) =
      row##.id := (Js.string id)

    let addEventListener (row: r) (s: string) f =
      let newRow = Js.Unsafe.coerce (row) in
      newRow##addEventListener (Js.string s) (f)

    let appendChildtoCell (parent: c) (child: Dom_html.element Js.t) =
      let u = Js.Unsafe.coerce parent in
        u##appendChild child
    let appendChildtoRow (parent: r) (child: Dom_html.element Js.t) =
      let u = Js.Unsafe.coerce parent in
        u##appendChild child
    let appendChildtoTable (parent: t) (child: Dom_html.element Js.t) =
      let u = Js.Unsafe.coerce parent in
        u##appendChild child
        
    let lowLevelApply (table: t) process =
      let lowTab = Js.Unsafe.coerce (tToTable table) in 
        process lowTab
      
    let changeDisplay (table: t) (disp: string) =
      table##.style##.display := Js.string (disp)

  (*CODE WITH NEW LOGIC*)

  let buildTable contents id parent =
    let rec fillTable table contents nRow =
      match contents with
      | [] -> []
      | (a::b) ->
          let rec fillRow cells row nIter =
          match cells with
            | [] -> []
            | (a :: c) ->
              let _ = if (nIter = 0 || nRow = 0) then (insertCell row "edgeCell" a) else (insertCell row "contentCell" a) in
                fillRow c row (nIter+1)
          in
          let newRow = insertRow table in
          let _ = fillRow a newRow 0 in
            fillTable table b (nRow+1)
    in
    let table = create id parent in
      let _ = fillTable table contents 0 in
        table##.style##.display := Js.string "none"


  (*makeCell para FAs*)
  let makeFACell st sy transitions =
    let trans1 = Set.filter (fun (st1,sy1,st2) -> st1 = st && sy1 = sy) transitions in
    let targets = Set.map ( fun (st1,sy1,st2) -> state2str st2) trans1 in
    String.concat ", " (Set.toList targets)

  (*makeCell para PDAs*)
  let makePDACell st sa sy transitions =
    let trans1 = Set.filter( 
      fun (st1, sa1, sy1, st2, sa2) -> 
        st1 = st && sa1 = sa && sy1 = sy
        ) 
      transitions 
      in
    let targets = Set.map (
      fun (st1, sa1, sy1, st2, sa2) ->
        (
          let stack : string list = List.map(fun symb -> symb2str symb) sa2 in
            match stack with
            | [] -> state2str st2
            | a :: b -> state2str st2 ^ ", " ^ String.concat "" stack
          )
      ) trans1
    in
    String.concat " | " (Set.toList targets)

    
  (*makeStateRow para FAs*)
  let makeFAStateRow st alphabet transitions : string list =
    state2str st :: (List.map (fun sy -> makeFACell st sy transitions) alphabet)

  (*makeStateRow para PDAs*)
  let makePDAStateRow st sa ia transitions : string list =
    (state2str st ^ ", " ^ symb2str sa) :: 
    (List.map (fun input -> makePDACell st sa input transitions) ia)


  (*TRACE*)

  let rec makeFAPath path step : string list list =
    match path with 
      | [] -> []
      | (state, word) :: b ->
        ["Step " ^ (Int.to_string step); (state2str state); (word2str word)] :: makeFAPath b (step+1)

  let rec makePDAPath path step : string list list =
    match path with 
      | [] -> []
      | (state, stack, word) :: b ->
        ["Step " ^ (Int.to_string step); (state2str state); (word2str stack); (word2str word)] :: makePDAPath b (step+1)

  let rec makeCFGPath path step : string list list =
    match path with
      | [] -> []
      | (symbs, word) :: b ->
        ["Step " ^ (Int.to_string step); (word2str symbs); (word2str word)] :: makeCFGPath b (step+1)

  let rec makeGRPath path step : string list list =
    match path with
      | [] -> []
      | (symbs, word) :: b ->
        ["Step " ^ (Int.to_string step); (word2str symbs); (word2str word)] :: makeGRPath b (step+1)

  let rec makeREPath path step : string list list =
    match path with
      | [] -> []
      | (t, word) :: b ->
        ["Step " ^ (Int.to_string step); RegularExpression.toString t; (word2str word)] :: makeREPath b (step+1)

  let rec makeTMPath (path: OCamlFlat.TuringMachine.path) step : string list list =
    match path with
      | [] -> []
      | (state, tapes) :: b ->
        let (left, right) = List.hd tapes in
        let currChar = List.hd right in
        let tmSpan = "<span id='currChar'>" ^ (symb2str currChar) ^ "</span>" in
        ["Step " ^ (Int.to_string step); state2str state; word2str (List.rev left) ^ tmSpan ^ word2str (List.tl right)] :: makeTMPath b (step+1)

  (*PAINTING METHODS*)

  let paint (tab: t) (rowIndex: int) (cellIndex: int) (color: string) =
    let row = fetchRow tab rowIndex in
    match Js.Opt.to_option row with
    | None -> ()
    | Some r -> 
      let cell = fetchCell r cellIndex in
      match Js.Opt.to_option cell with
      | None -> ()
      | Some c -> c##.style##.backgroundColor := Js.string color


      



      
end



