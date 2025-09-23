open Js_of_ocaml
open Js
open JS
open Lang
open Listeners
open Random
open OCamlFlat.BasicTypes

class type position =
  object
    method x : int readonly_prop
    method y : int readonly_prop
  end

class type ['a] style =
  object
    method selector : js_string t prop
    method style : 'a prop
    method resetToDefault : unit meth
    method update : unit meth
  end

class type popper =
  object
    method destroy : unit -> unit meth
    method update : unit -> unit meth
  end

class type contextMenus =
  object
    method destroy : unit -> unit meth
    method showMenuItem: js_string t -> unit meth
    method hideMenuItem: js_string t -> unit meth
  end

module DataItem =
struct

  class type data =
    object
      method id : js_string t prop
      method parent : js_string t prop (* PEDROCARLOS VER! para que serve    deve ser invisivel para o cytoscape *)
      method source : js_string t prop
      method target : js_string t prop
      method label : js_string t prop
      method nodeType : js_string t prop
    end

  class type t =
    object
      method data : data Js.t prop
      method data_fromName: js_string Js.t -> js_string Js.t meth (*ele.data(name) Get a particular data field for the element.*)
      method data_update: js_string Js.t -> js_string Js.t -> unit meth (*ele.data(name,value)Set a particular data field for the element.*)
      method group : js_string Js.t prop
      method position : position Js.t prop
      method renderedPosition : position Js.t prop
      method classes : js_string Js.t prop
      method length : number Js.t prop
      method remove : unit -> 'res Js_of_ocaml.Js.meth 
      method addClass : js_string Js.t -> unit meth
      method removeClass : js_string Js.t -> unit meth
      method lock : unit Js_of_ocaml.Js.meth
      method unlock : unit Js_of_ocaml.Js.meth 
      method incomers : js_string Js.t -> t js_array Js.t meth
      method outgoers : js_string Js.t -> t js_array Js.t meth
      method popper : 'z -> popper Js.t meth
    end
end

class type layout_options =
  object
    method name : js_string t readonly_prop
    method rankDir : js_string t readonly_prop
  end

class type layout =
  object
    method run : unit meth
  end

class type props =
  object
    method container : Dom_html.element t prop
    method elements : DataItem.t t js_array t prop
    method style : Unsafe.any style t js_array t prop
    method layout : layout_options t prop
    method zoom : int prop
    method pan : position t prop
    method minZoom : float prop
    method maxZoom : float prop
    method zoomingEnabled : bool t prop
    method userZoomingEnabled : bool t prop
    method panningEnabled : bool t prop
    method userPanningEnabled :bool t prop
    method boxSelectionEnabled : bool t prop
    method selectionType : js_string t prop
    method touchTapThreshold : int prop
    method desktopTapThreshold : int prop
    method autolock : bool t prop
    method autoungrabify : bool t prop
    method autounselectify : bool t prop
    method headless : bool t prop
    method styleEnabled : bool t prop
    method hideEdgesOnViewport : bool t prop
    method textureOnViewport : bool t prop
    method motionBlur : bool t prop
    method motionBlurOpacity : float prop
    method wheelSensitivity : float prop
    method pixelRatio : js_string t prop
  end

class type cytoscape =
  object
    method add : DataItem.t t -> unit meth
    method remove : DataItem.t t -> unit meth
    method remove_fromSelector : js_string t -> unit meth
    method mount : Dom_html.element t -> unit meth
    method layout : layout_options t -> layout t meth
    method resize : unit meth
    method on_3 : js_string t -> (Dom_html.event Js.t -> unit) -> unit meth
    method on_4 : js_string t -> js_string t opt -> ((< > t) Js.t -> (DataItem.t) Js.t -> (DataItem.t) Js.t -> (DataItem.t) Js.t -> unit) -> unit meth
    method on : js_string t -> js_string t -> (Dom_html.event Js.t -> unit) -> unit meth
    method edgehandles: 'a Js.t -> 'res meth
    method getElementById: js_string t -> DataItem.t t meth
    method cxtmenu : 'b t  -> unit meth
    method style : Unsafe.any style Js.t Js.meth
    method style_n : Unsafe.any style t js_array t -> unit meth
    method destroy: unit -> unit meth
    method autolock: bool -> unit Js.meth
    method fit : unit meth
    method edges : js_string t -> DataItem.t Js.t js_array Js.t meth
    method nodes : js_string t -> DataItem.t Js.t js_array Js.t meth
    method tapdragover: DataItem.t t -> unit meth
    method contextMenus : 'c t  -> contextMenus Js.t meth
    method popper : 'z -> popper Js.t meth
  end

type cytoscape_cs = (props Js.t -> cytoscape Js.t) constr

let cytoscape_cs : cytoscape_cs = Js.Unsafe.pure_js_expr "cytoscape"

let default_style : Unsafe.any style t js_array t =
  let node_style = Unsafe.coerce @@ object%js
      val selector = string "node"
      val style = def (object%js
          val label = string "data(id)"
        end)
    end in
  array [| node_style |]

let default_layout : layout_options t =
  object%js val name = string "preset" 
    val rankDir = Js.string "LR"
end

let position x y : position t =
  object%js val x = x val y = y end

let node id pos nodeType classes label : DataItem.t t =
  let data : DataItem.data t = Unsafe.obj [||] in
  let node : DataItem.t t = Unsafe.obj [||] in
  data##.id := string id;
  (match pos with None -> () | Some (x, y) -> node##.position := position x y);
  data##.nodeType := string nodeType;
  node##.classes := string classes;
  data##.label := string label;
  node##.data := data;
  node##.group := Js.string "nodes";
  node


let edge id source target label : DataItem.t t =
  let data : DataItem.data t = Unsafe.obj [||] in
  (match id with None -> () | Some id -> data##.id := Js.string id);
  data##.source := string source;
  data##.target := string target;
  data##.label := string label;
  let edge : DataItem.t t = Unsafe.obj [||] in
  edge##.data := data;
  edge##.group := string "edges";
  edge

let mk_graph ?(style=default_style) ?(layout=default_layout) ?(props=[]) container_id =
  let container = Dom_html.getElementById container_id in
  let props = array @@ Array.of_list props in
  let g : props t = Unsafe.obj [||] in
  g##.container := container;
  g##.elements := props;
  g##.style := style;
  g##.layout := layout;
  g

let display props = new%js cytoscape_cs props

let add_node cy id ?pos nodeType classes label =
(*  Firebug.console##log ("Adding node with id: " ^ id);*)
  cy##add (node id pos nodeType classes label)

(*PEDROCARLOS VER!  nos compostos das arvores para gramaticas irreestritas *)
let addCompoundNode cy id ?pos label =
  let data : DataItem.data t = Unsafe.obj [||] in
  data##.id := string id;
  (match label with None -> () | Some label -> data##.label := string label);
  let node : DataItem.t t = Unsafe.obj [||] in
  node##.data := data;
  node##.group := string "nodes";
  (match pos with None -> () | Some pos -> node##.position := pos);
  cy##add node

let addChildNode cy parentId childId ?pos childLabel=
  let data : DataItem.data t = Unsafe.obj [||] in
  data##.id := string childId;
  data##.parent := string parentId; 
  (match childLabel with None -> () | Some label -> data##.label := string label);
  let node : DataItem.t t = Unsafe.obj [||] in
  node##.data := data;
  node##.group := string "nodes";
  (match pos with None -> () | Some pos -> node##.position := pos);
  cy##add node

let add_edge cy ?id source target label =
(*  Firebug.console##log ("Adding edge with id: '" ^ (match id with |None -> "" | Some a -> a) ^ "' from: '" ^ source ^ "' to: '" ^ target ^ "' with symbol: '" ^ label ^ "'.");*)
  cy##add (edge id source target label)

let random_layout g : layout t =
  let layout_opt = object%js
    val name = string "random"
  end in
  g##layout layout_opt

let run_layout (l : layout t) =
  l##run

let on cy event selector cb =
  match selector with
    | "" -> cy##on_4 (string event) Js.null cb
    | _ -> cy##on_4 (string event) (Js.some (string selector)) cb

let elementId (cy: cytoscape Js_of_ocaml.Js.t) (node : string) =
  cy##getElementById (Js.string node)

let getIncomers cy nodeId selector =
  let node = elementId cy nodeId in
  let elements = node##(incomers (Js.string selector)) in
    Array.to_list (Js.to_array elements)
    
let getOutgoers cy nodeId selector =
  let node = elementId cy nodeId in
  let elements = node##(outgoers (Js.string selector)) in
    Array.to_list (Js.to_array elements)
    
let getEdges cy selector =
  Array.to_list (Js.to_array (cy##edges (Js.string selector)))

let getAllNodes (cy: cytoscape Js_of_ocaml.Js.t): DataItem.t Js_of_ocaml.Js.t list =
  Array.to_list (Js.to_array (cy##nodes (Js.string "*")))

let data_fromName element name =
  Js.to_string (element##data_fromName (Js.string name))
  
let data_update element name value =
  element##data_update (Js.string name) (Js.string value)

let faLayout : layout_options Js.t = (**Layout for finite automata**)
  object%js val name = Js.string "grid"
            val rankDir = Js.string "LR"
          end

let reLayout : layout_options Js.t = (**Layout for regular expression trees**)
  object%js val name = Js.string "dagre"
            val rankDir = Js.string "TB"
          end

let cfglayout : layout_options Js.t = (**Layout for context free grammars**)
  object%js val name = Js.string "dagre" 
            val rankDir = Js.string ""
          end

let grlayout : layout_options Js.t = (**Layout for grammars**)
object%js val name = Js.string "dagre" 
          val rankDir = Js.string ""
        end

(* let treeGrlayout : layout_options Js.t = (**Layout for grammars**)
object%js val name = Js.string "dagre" 
          val rankDir = Js.string ""
        end *)
let treeGrlayout : layout_options Js.t = (**Layout for grammars**)
object%js val name = Js.string "dagre" 
          val rankDir = Js.string ""
end

let edgehandlesOptions =
  object%js
    val canConnect = fun (sourceNode : DataItem.t) (targetNode : DataItem.t) -> Js.bool true
    val edgeParams = fun (sourceNode : DataItem.t) (targetNode : DataItem.t) -> object%js end
    val hoverDelay = 150
    val snap = Js.bool true
    val snapThreshold = 50
    val snapFrequency = 15
    val noEdgeEventsInDraw = Js.bool true
    val disableBrowserGestures = Js.bool true
  end

let menu cy eh= 
  Js.Unsafe.coerce @@ object%js
(*    val menuRadius = fun element -> (70)*)
    val selector = Js.string "node"
    val commands = 
      let menu1 = Js.def (object%js
        val content = Js.string (Lang.i18nTextRemove())
        val select = fun element -> 
          !ListenersAutomaton.removeNode (data_fromName element "id");
          !Listeners.updateRightListener ()
      end) in
      let menu2 = Js.def (object%js
        val content = Js.string (Lang.i18nTextTurnFinal())
        val select = fun element -> 
          !ListenersAutomaton.turnFinal (data_fromName element "id");
          !Listeners.updateRightListener ()
      end) in
      let menu3 = Js.def (object%js
        val content = Js.string (Lang.i18nTextRemoveFinal())
        val select = fun element -> 
          !ListenersAutomaton.removeTypeFinal (data_fromName element "id");
          !Listeners.updateRightListener ()
      end) in 
      let menu4 = Js.def (object%js
        val content = Js.string (Lang.i18nTextTurnInitial())
        val select = fun element -> 
          !ListenersAutomaton.turnNodeInitial (data_fromName element "id");
          !Listeners.updateRightListener ()
      end) in
      let menu5 = Js.def (object%js
        val content = Js.string (Lang.i18nTextRenameState())
        val select = fun element -> 
          !ListenersAutomaton.renameNodeListener (data_fromName element "id");
          !Listeners.updateRightListener ()
      end) in
      let menu6 = Js.def (object%js
        val content = Js.string (Lang.i18nTextAddTransition())
        val select = fun element -> 
          eh##start element
      end) in
      Js.array [|menu1; menu2; menu3; menu4; menu5; menu6|]
  end 

let menu2 = 
  Js.Unsafe.coerce @@ object%js
    val selector = Js.string "core"
    val commands = 
      let menu1 = Js.def (object%js
        val content = Js.string (Lang.i18nTextAdd ())
        val select = fun element evt -> 
          !ListenersAutomaton.addNode evt##.position##.x evt##.position##.y;
          !Listeners.updateRightListener ()
      end) in
      let menu2 = Js.def (object%js
        val content = Js.string (Lang.i18nTextAddInitial ())
        val select = fun element evt -> 
          !ListenersAutomaton.addInitialNode evt##.position##.x evt##.position##.y;
          !Listeners.updateRightListener ()
      end) in 
      let menu3 = Js.def (object%js
        val content = Js.string (Lang.i18nTextAddFinal ())
        val select = fun element evt -> 
          !ListenersAutomaton.addFinalNode evt##.position##.x evt##.position##.y;
          !Listeners.updateRightListener ()
      end) in 
      Js.array [|menu1; menu2; menu3|]
  end

  let menu3 = 
    Js.Unsafe.coerce @@ object%js
      val selector = Js.string "edge"
      val commands = 
        let menu1 = Js.def (object%js
          val content = Js.string (Lang.i18nTextRemove ())
          val select = fun element -> 
            (let source = element##source in 
              let srcId = data_fromName source "id" in
              let target = element##target in 
              let trgId = data_fromName target "id" in
              let symb = data_fromName element "label" in
                !ListenersAutomaton.removeTransition srcId trgId symb;
                !Listeners.updateRightListener ())
        end) in
        Js.array [|menu1|]
    end

let paintNodeStyle node color: Js.Unsafe.any style Js.t Js.js_array Js.t =
  let node_name_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ("#" ^ node)
      val style = Js.def (object%js
        val backgroundColor = Js.string color
      end)
  end in
  Js.array [|node_name_style|]

let encodeURI svg =
    Js_of_ocaml.Js.encodeURI (Js.string svg)

let insertLoopStyle cy nId =
    ignore(
        (Js.Unsafe.coerce cy)##
        style##
        (selector (Js.string ("#" ^ nId)) )##
        style(Js.Unsafe.coerce @@ object%js
            val curveStyle = Js.string "bezier"
            val controlPointStepSize = Js.string "120px" (* metade da largura, no minimo 60 *)
            val targetArrowShape = Js.string "triangle"

            val loopDirection = Js.string "-90deg"
            val loopSweep = Js.string "-90deg"

            val sourceEndpoint = Js.string "-90deg"
            val targetEndpoint = Js.string "-90deg"
        end)
  )##update


let insertNodeImage cy node itemString itemListSize maxSymbolCount =
  ignore(
    (Js.Unsafe.coerce cy)##
    style##
    (selector (Js.string ("#" ^ node)) )##
    style(Js.Unsafe.coerce @@ object%js
      val opacity = Js.string "0.50"
      val backgroundColor = Js.string "white"
      val backgroundImage = itemString
      val backgroundClip = Js.string "none"
      val borderWidth = Js.string "3px"
      val borderColor = Js.string "black"
      val width = Js.string (string_of_int ((maxSymbolCount*9) -20) ^"px")
      val height = Js.string (string_of_int ((itemListSize*20) + 10) ^"px")
      val shape = Js.string "roundrectangle"
    end)
  )##update

let rec tspan stringItems increment =
        match stringItems with
        | [] -> ""
        | x::xs -> Printf.sprintf {|<tspan x="20" y="%s"> %s </tspan>|} (string_of_int (increment)) x ^ tspan xs (increment + 20)

let rec maxLR0SymbolCount itemList max =
            match itemList with
            | [] -> max
            | x::xs -> if(String.length x > max) then maxLR0SymbolCount xs (String.length x)
                       else maxLR0SymbolCount xs max


let buildStyleImage stringItems=

    let prefix = "data:image/svg+xml;utf8," in

    let itemListSize = List.length stringItems in

    let maxSymbolCount = maxLR0SymbolCount stringItems 0 in

    let svg = Printf.sprintf {| <svg xmlns="http://www.w3.org/2000/svg" height="%s" width="%s"> |} (string_of_int ((itemListSize*20) + 30)) (string_of_int ((maxSymbolCount*9) + 10)) in
    let text = {| <text x="0" y="0" style="fill:black;"> |} in


    let textSpan = tspan stringItems 30 in


    let finish =    {|</text>
                </svg> |} in

    let style = prefix ^ svg ^ text ^ textSpan ^ finish in
        style


    (*
    let suffix = 
    "<svg xmlns="http://www.w3.org/2000/svg" height="100" width="100">
        <text x="10" y="20" style="fill:red;">Several lines:
                <tspan x="10" y="45">First line.</tspan>
                <tspan x="10" y="70">Second line.</tspan>
        </text>
    </svg>" in
    *)

let buildLR0NodeStyle cy node stringItemList =
    let styleImage = buildStyleImage stringItemList in
    let encodeStyle = encodeURI styleImage in
    let itemListSize = List.length stringItemList in
    let maxSymbolCount = maxLR0SymbolCount stringItemList 0 in
        insertNodeImage cy node encodeStyle itemListSize maxSymbolCount


let faStyle : Js.Unsafe.any style Js.t Js.js_array Js.t = (**Left FA style**)
  let node_name_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[label]"
      val style = Js.def (object%js
        val content = Js.string "data(label)"
        val textHalign = Js.string "center"
        val textValign = Js.string "bottom"
        val width = Js.string "40px"
        val height = Js.string "40px"
        val textMarginY = Js.string "2"
      end)
  end in
  let edge_symbol_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "edge[label]"
      val style = Js.def (object%js
        val content = Js.string "data(label)"
      end)
    end in
  let edge_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "edge"
    val style = Js.def (object%js
      val curveStyle = Js.string "bezier"
      val targetArrowShape = Js.string "triangle"
    end)
  end in
  (*
  let loop_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "loop"
        val style = Js.def (object%js
        val curveStyle = Js.string "bezier"
        val targetArrowShape = Js.string "triangle"
        val loopDirection = Js.string "-45deg"
        val loopSweep = Js.string "-90deg"
        val sourceEndpoint = Js.string "outside-to-node-or-label"
        val targetEndpoint = Js.string "outside-to-node-or-label"
        val controlPointStepSize = Js.string "1"
    end)
  end in
  *)
  let node_transparent_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "#transparent"
    val style = Js.def (object%js
      val visibility = Js.string "hidden"
    end)
  end in
  let node_style_success = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".SUCCESS"
    val style = Js.def (object%js
      val borderWidth = Js.string "7px"
      val borderColor = Js.string "black"
      val borderStyle = Js.string "double"
      val textMarginY = Js.string "5"
    end)
  end in
  let eh_handle_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-handle"
      val style = Js.def (object%js
        val backgroundColor = Js.string "red"
        val width = Js.string "12"
        val height = Js.string "12"
        val shape = Js.string "ellipse"
        val overlayOpacity = Js.string "0"
        val borderWidth = Js.string "12"
        val borderOpacity = Js.string "0"
      end)
  end in
  let eh_hover_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-hover"
      val style = Js.def (object%js
        val backgroundColor = Js.string "red"
      end)
  end in
  let eh_source_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-source"
      val style = Js.def (object%js
        val borderWidth = Js.string "2"
        val borderColor = Js.string "red"
      end)
  end in
  let eh_target_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-target"
      val style = Js.def (object%js
        val borderWidth = Js.string "2"
        val borderColor = Js.string "red"
      end)
  end in
  let eh_preview_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-preview"
      val style = Js.def (object%js
        val backgroundColor = Js.string "red"
        val lineColor = Js.string "red"
        val targetArrowColor = Js.string "red"
        val sourceArrowColor = Js.string "red"
      end)
  end in
  let eh_ghost_edge_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-ghost-edge"
      val style = Js.def (object%js
        val backgroundColor = Js.string "red"
        val lineColor = Js.string "red"
        val targetArrowColor = Js.string "red"
        val sourceArrowColor = Js.string "red"
      end)
  end in
  let eh_ghost_edge_preview_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".eh-ghost-edge.eh-preview-active"
      val style = Js.def (object%js
        val opacity = Js.string "0"
      end)
  end in
  Js.array [| node_name_style; edge_symbol_style; edge_style;
              node_transparent_style; node_style_success;
              eh_handle_style; eh_hover_style; eh_source_style;
              eh_target_style; eh_preview_style; eh_ghost_edge_style;
              eh_ghost_edge_preview_style |]

let faStyle2 : Js.Unsafe.any style Js.t Js.js_array Js.t = (**Right FA style**)
  let node_name_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[label]"
    val style = Js.def (object%js
      val content = Js.string "data(label)"
      val textHalign = Js.string "center"
      val textValign = Js.string "bottom"
      val width = Js.string "40px"
      val height = Js.string "40px"
    end)
  end in
  let edge_symbol_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "edge[label]"
    val style = Js.def (object%js
      val content = Js.string "data(label)"
    end)
  end in
  let edge_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "edge"
    val style = Js.def (object%js
      val curveStyle = Js.string "bezier"
      val targetArrowShape = Js.string "triangle"
    end)
  end in
  let node_transparent_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "#transparent1"
    val style = Js.def (object%js
      val visibility = Js.string "hidden"
    end)
  end in
  let node_style_success = Js.Unsafe.coerce @@ object%js
    val selector = Js.string ".SUCCESS"
    val style = Js.def (object%js
      val borderWidth = Js.string "7px"
      val borderColor = Js.string "black"
      val borderStyle = Js.string "double"
    end)
  end in
  Js.array [| node_name_style; edge_symbol_style; edge_style;
              node_transparent_style; node_style_success |]


let reStyle : Js.Unsafe.any style Js.t Js.js_array Js.t = (**RE style**)
  let node_name_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[label]"
    val style = Js.def (object%js
      val content = Js.string "data(label)"
      val textHalign = Js.string "center"
      val textValign = Js.string "center"
      val width = Js.string "40px"
      val height = Js.string "40px"
    end)
  end in
  let node_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node"
    val style = Js.def (object%js
      val backgroundColor = Js.string "white"
    end)
  end in
  let edge_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "edge"
    val style = Js.def (object%js
      val curveStyle = Js.string "bezier"
      val targetArrowShape = Js.string "triangle"
      val width = Js.string "4"
      val lineColor = Js.string "#9dbaea"
      val targetArrowColor = Js.string "#9dbaea"
    end)
  end in
  let node_fail_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[label = \'Fail\']"
    val style = Js.def (object%js
      val color = Js.string "red"
    end)
  end in
  Js.array [| node_name_style; node_style; edge_style; node_fail_style |]

let cfgStyle : Js.Unsafe.any style Js.t Js.js_array Js.t = (**CFG style**)
  let node_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node"
    val style = Js.def (object%js
      val label = Js.string "data(label)"
      val textHalign = Js.string "center"
      val textValign = Js.string "center"
      val borderWidth = Js.string "2"
      val fontFamily = Js.string "monospace"
    end)
  end in
  let node_selected_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node.selected"
    val style = Js.def (object%js
      val backgroundColor = Js.string "lightblue"
    end)
  end in
  let node_child_selected_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node.childrenSelected"
    val style = Js.def (object%js
      val backgroundColor = Js.string "lightgreen"
    end)
  end in
  let node_child_epsilon_syle = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node.epsilon"
    val style = Js.def (object%js
      val backgroundColor = Js.string "lightyellow"
    end)
  end in
  let node_style_root = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[nodeType = \'root\']"
    val style = Js.def (object%js
      val backgroundColor = Js.string "white"
    end)
  end in
  let node_style_leaf = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[nodeType = \'leaf\']"
    val style = Js.def (object%js
      val backgroundColor = Js.string "white"
    end)
  end in
  Js.array [| node_style; node_style_root; node_style_leaf;
              node_selected_style;
              node_child_selected_style;
              node_child_epsilon_syle |]


let grStyle : Js.Unsafe.any style Js.t Js.js_array Js.t = (**GR style**)
let node_style = Js.Unsafe.coerce @@ object%js
  val selector = Js.string "node"
  val style = Js.def (object%js
    val label = Js.string "data(label)"
    val textHalign = Js.string "center"
    val textValign = Js.string "center"
    val borderWidth = Js.string "2"
    val fontFamily = Js.string "monospace"
  end)
end in
let node_selected_style = Js.Unsafe.coerce @@ object%js
  val selector = Js.string "node.selected"
  val style = Js.def (object%js
    val backgroundColor = Js.string "lightblue"
  end)
end in
let node_child_selected_style = Js.Unsafe.coerce @@ object%js
  val selector = Js.string "node.childrenSelected"
  val style = Js.def (object%js
    val backgroundColor = Js.string "lightgreen"
  end)
end in
let node_child_epsilon_syle = Js.Unsafe.coerce @@ object%js
  val selector = Js.string "node.epsilon"
  val style = Js.def (object%js
    val backgroundColor = Js.string "lightyellow"
  end)
end in
let node_style_root = Js.Unsafe.coerce @@ object%js
  val selector = Js.string "node[nodeType = \'root\']"
  val style = Js.def (object%js
    val backgroundColor = Js.string "white"
  end)
end in
let node_style_leaf = Js.Unsafe.coerce @@ object%js
  val selector = Js.string "node[nodeType = \'leaf\']"
  val style = Js.def (object%js
    val backgroundColor = Js.string "white"
  end)
end in
let node_style_compound = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node[nodeType = 'compound']"
    val style = Js.def (object%js
      val backgroundColor = Js.string "#eee"
      val textValign = Js.string "top"
      val label = Js.string "data(label)" (* Add this line *)
    end)
  end in
  let child_node_style = Js.Unsafe.coerce @@ object%js
    val selector = Js.string "node:child"  (* Style for child nodes*)
    val style = Js.def (object%js
      val backgroundColor = Js.string "white"
  end)
  end in
Js.array [| child_node_style; node_style_compound; node_style; node_style_root; node_style_leaf;
            node_selected_style;
            node_child_selected_style;
            node_child_epsilon_syle |]

let applyStyle cy style =
  cy##style##resetToDefault;
  Array.iter (fun s -> let s = Js.Unsafe.coerce s in
                     cy##style##(selector(s##.selector))##style(s##.style)
           ) style;
  cy##style##update
  
let resetStyle cy style = (*TODO Port from original resetStyle and especific for FA. Make for any model?*)
  let styles = Js.to_array style in
  cy##style##resetToDefault;
  Array.iter (fun s -> let s = Js.Unsafe.coerce s in
                       (Js.Unsafe.coerce cy)##style##(selector(s##.selector))##style(s##.style)
             ) styles;
  cy##style##update

let removeAllElements cy = 
  cy##remove_fromSelector (Js.string "node")

let resetFaElems cy = 
  removeAllElements cy;
  add_node cy "transparent" ~pos:(0,200) "" "transparent" "transparent"
  

let paintNode cy node color = 
  ignore(
    (Js.Unsafe.coerce cy)##
    style##
    (selector (Js.string ("#" ^ node)) )##
    style(Js.Unsafe.coerce @@ object%js
      val backgroundColor = Js.string color
    end)
  )##update

let initCy cy =
  let props = mk_graph cy in
  let cy = display props in
  cy

let initFaCy cyContainer =
  let props = mk_graph ~style:faStyle ~layout: faLayout cyContainer in
  let cy = display props in
  let eh = (Js.Unsafe.coerce cy)##edgehandles edgehandlesOptions in
    on cy "ehcomplete" "" (fun event sourceNode targetNode addedEles -> 
      let source = data_fromName sourceNode "id" in 
      let target = data_fromName targetNode "id" in
      let ele = data_fromName addedEles "id" in 
      let getElement = elementId cy ele in 
       cy##remove (getElement);
        !ListenersAutomaton.addTransition source target;
        !Listeners.updateRightListener ()
    );
  if cyContainer <> "cy2" then begin
    cy##cxtmenu(menu cy eh);
    cy##cxtmenu(menu2);
    cy##cxtmenu(menu3) end;
  add_node cy ~pos:(0,200) "transparent" "" "transparent" "transparent";
  cy##autolock( false );
  cy

let startTree cyContainer =
  let props = mk_graph ~style:reStyle cyContainer in
  let cy = display props in
  run_layout (cy##layout reLayout);
  cy

let initLL1Cy cyContainer =
  let props = mk_graph ~style:cfgStyle cyContainer in
  let cy = display props in
  run_layout (cy##layout cfglayout);
  cy

let initGRCy cyContainer =
  let props = mk_graph ~style:grStyle cyContainer in
  let cy = display props in
  run_layout (cy##layout grlayout);
  cy

let initGRCy3 cyContainer =
  let props = mk_graph ~style:grStyle cyContainer in
  let cy = display props in
  run_layout (cy##layout treeGrlayout);
  cy

let generateEdgeID source target =
	source ^ "-" ^ target (* "-" avoid ambiguity: "A-AA" "AA-A" *)

let addEdge cy (first, edgeLabel, second) =
  let nId = generateEdgeID first second in
    let getEdge = elementId cy nId in
      if (Js.float_of_number getEdge##.length) = 0. then(
        add_edge cy ~id:nId first second edgeLabel)
      else 
        (let  test1 = Js.Unsafe.coerce (getEdge) in
          let k = Js.to_string (test1##data##.label) in 
        cy##remove (getEdge);
        let newSymbol = k ^ ", " ^ edgeLabel in
        add_edge cy ~id:nId first second newSymbol)

(* ML *)
let addEdgeGeneral cy (first, edgeLabel, second) =
  let nId = first ^ second in
    let getEdge = elementId cy nId in
      if (Js.float_of_number getEdge##.length) = 0. then(
        add_edge cy ~id:nId first second edgeLabel)
      else 
        (let  test1 = Js.Unsafe.coerce (getEdge) in
          let k = Js.to_string (test1##data##.label) in 
        cy##remove (getEdge);
        let newSymbol = k ^ ",  " ^ edgeLabel in
        add_edge cy ~id:nId first second newSymbol)

let addNode cy node ?(x = Random.int 1399) ?(y = Random.int 299) isStart isFinal =
  let verify = elementId cy node in
    if ((Js.float_of_number verify##.length) < 1.) then
      if (isFinal = true) then
        if (isStart = true) then
          (add_node ~pos:(100, 200) cy node "" "SUCCESS" node;
          (elementId cy node)##lock;
          addEdge cy ("transparent", " ", node) )
        else
          add_node cy ~pos:(x, y) node "" "SUCCESS" node
      else 
        if (isStart = true) then
          (add_node cy ~pos:(100, 200) node "" "NOT" node;
          (elementId cy node)##lock;
          addEdge cy ("transparent", " ", node) )
        else 
          add_node cy ~pos:(x, y) node "" "NOT" node

let addNode2 cy node isStart isFinal =
  let verify = elementId cy node in
  let y = 100 + (100 * (int_of_string node)) in
  (*let y = 200 + (50 * ((int_of_string node) -1)) in *)
  (* let y = Random.int 399 in **)
  let x = 200 + (50 * ((int_of_string node mod 2)-2) * ((int_of_string node))) * -1 in
    if ((Js.float_of_number verify##.length) < 1.) then
        if (node = "0") then
          add_node cy ~pos:(100, 200) node "" "" node
        else
          add_node cy ~pos:(x, y) node "" "" node

let removeNode cy node =
  let element = elementId cy node in
    cy##remove(element)

let turnFinal cy name =
  let nn = elementId cy name in
    nn##addClass(Js.string "SUCCESS")

let removeFinal cy name =
  let nn = elementId cy name in
    nn##removeClass (Js.string "SUCCESS")

let removeEdge cy source label target =
  let open Re in
  let nId = generateEdgeID source target in
  let getEdge = cy##getElementById (Js.string nId) in
  let labelEdge = Js.to_string (Js.Unsafe.coerce getEdge)##data##.label in
  let labelEscSpecialChars = Str.global_replace (Str.regexp {|\([\*\+\.\^\]\)|}) {|\\\1|} label in
  let newLabel = Str.replace_first (Str.regexp ("^"^labelEscSpecialChars^", \\|\\(, \\)?"^labelEscSpecialChars)) "" labelEdge in
  cy##remove(getEdge);
  if (newLabel <> "") then add_edge cy ~id:nId source target newLabel 

let destroyGraph cy =
  cy##destroy()

let fit cy = 
  match cy with
  | None -> ()
  | Some c -> c##resize; c##fit

let centerGraph cy =
  cy##resize;
  cy##fit
  
let makeTreeNode cy id node =
  add_node cy id "" "" node;
  run_layout (cy##layout reLayout)

(* Carolina *)
let makeTreeNode2 cy id name =
    add_node cy id "" "" name;
    (*let elem = elementId cy id in
    elem##on_3 "tap" (fun event a b c -> JS.alertStr ("aaa"));*)
    run_layout (cy##layout reLayout);
    cy##on (Js.string "tap") (Js.string "node") 
    (fun evt -> 
      match Js.Opt.to_option (evt##.target) with 
      | None -> JS.log("Error, none node selected")
      | Some t ->
        let target = Js.Unsafe.coerce t in
        let label = Js.to_string target##data##.label in
          !ListenersComp.showTreeNode label
    )


let makeTreeEdge cy idNode1 idNode2 =
  let nId = idNode1 ^ "_" ^ idNode2 in
    add_edge cy ~id:nId idNode1 idNode2 "";
    run_layout (cy##layout reLayout)

let changeDirection cy2 layoutDir =
  if (layoutDir = "LR") then
    (run_layout (cy2##layout (object%js 
                            val name = Js.string "dagre"
                            val rankDir = Js.string "TB"
                            end));
    "TB")
  else 
    (run_layout (cy2##layout (object%js 
                            val name = Js.string "dagre"
                            val rankDir = Js.string "LR"
                            end));
    "LR")


