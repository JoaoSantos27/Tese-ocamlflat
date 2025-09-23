open OCamlFlat
open BasicTypes
open JS
open Lang

module AutomatonView =
struct
	
  let productiveColor = "orange"
  let reachableColor = "yellow"
  let usefulColor = "purple"
  let stepState = "blue"
  let acceptState = "green"
  let wrongFinalState = "red"

  let rec intersection l1 l2 =
    match l1 with
       [] -> []
     | x::xs -> (if List.mem x l2 then [x] else []) @ intersection xs l2

    let cut s = (String.get s 0, String.sub s 1 ((String.length s)-1))

    let rec stringAsList s =
      if s = "" then []
      else
          let (x,xs) = cut s in
              x::stringAsList xs

  let iterateList meth (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) list =
    List.iter (meth cy) list

  let paintProductive (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state =
    Cytoscape.paintNode cy state productiveColor
      
  let paintReachable (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state =
        Cytoscape.paintNode cy state reachableColor
        
  let paintUseful (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state =
        Cytoscape.paintNode cy state usefulColor

  class virtual model (arg: ('r) Arg.alternatives) =
		object(self)

      val mutable steps = [||]
      val mutable position = 0
      val mutable isOver = false

      val sentence: char list ref = ref []
      val newSentence = ref ""

      method virtual inputEdges: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit     
      method virtual reachableFromInitialState: states
      method virtual usefulPainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method virtual productivePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method virtual areAllStatesUseful: bool
      method virtual productive: states
      method virtual paintCurrentStates: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method virtual staticGenerate : int -> words
      method virtual getInitialState: state
      method virtual getAcceptStates: states
      method virtual getStates: states
      method virtual staticAccept: unit
      method virtual staticAcceptFull: unit
      method virtual setInitialStep: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method virtual setNextStep: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method virtual buildTable: unit
      method virtual displayTrace: unit
      method virtual returnStats: bool * int * bool * float
      
      method private virtual getCurrConfigFromBestPath: 'configuration option
      method private virtual getWordFromConfig: 'configuration -> word

      method resetToEditModel = ()

      method setBackStep (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) = ()

      method inputNodes cy = 
        Set.iter (fun el -> 
          Cytoscape.addNode cy el (el = self#getInitialState) (Set.belongs el self#getAcceptStates)
        ) self#getStates

      method drawExample cy = 
        self#inputNodes cy;
        self#inputEdges cy

      method reachablePainting (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        let list1 = Set.toList self#reachableFromInitialState in
          iterateList paintReachable cy list1 

      method usefulPainting cy =
        let intre = intersection (Set.toList self#productive) (Set.toList self#reachableFromInitialState) in
        iterateList paintUseful cy intre 

      method productivePainting (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  =
        let list1 = Set.toList self#productive in
        iterateList paintProductive cy list1 

      method changeTheTestingSentence word =
        sentence := stringAsList word

      method newSentence = !newSentence

      method isOver = isOver

      method staticAccept =
        self#staticAccept

      method startAccept (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        steps <- Array.make 1 Set.empty;
        position <- 0;
        isOver <- false;
        self#setInitialStep cy;
        self#paintCurrentStates cy; 
        if (position = (List.length !sentence)) then
          (isOver <- true);
        match self#getCurrConfigFromBestPath with
          | None -> ()
          | Some config -> self#changeSentence config; 

      method next (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        if isOver then
          (JS.alertStr (Lang.i18nAlertNoMoreStates ()))
        else 
          (position <- position + 1;
          JS.log(steps);
          self#setNextStep cy;
          match self#getCurrConfigFromBestPath with
              | None -> ()
              | Some config -> 
                  self#changeSentence config;
                  if (Array.length steps) - 1 = position then (isOver <- true);
                  JS.log(isOver);
                  self#paintCurrentStates cy;
          )

      method back (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        position <- position - 1;
        isOver <- false;
        if position < 0 then
          (position <- 0; JS.alertStr (Lang.i18nAlertArrivedInitial ()))
        else
          (self#setBackStep cy;
          self#paintCurrentStates cy;
          match self#getCurrConfigFromBestPath with
              | None -> ()
              | Some config -> self#changeSentence config;
          )

      method changeToEditModelMode (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        steps <- [||];
        position <- 0;
        isOver <- false;
        sentence := [];
        newSentence :="";
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        self#resetToEditModel
          
      method private changeSentence config = 
        newSentence := "";
        let wordToConsume = self#getWordFromConfig config in
        let bar = '|' in 
        for i = 0 to (List.length !sentence) - (List.length wordToConsume) - 1 do 
              newSentence:= !newSentence ^ String.make 1 (List.nth !sentence i);
            done;
            newSentence:= !newSentence ^ String.make 1 bar;
        for i = 0 to (List.length wordToConsume) - 1 do
          newSentence:= !newSentence ^ symb2str (List.nth wordToConsume i);
        done
        

      method private paint (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state length final = 
          if (length != 0) then 
            Cytoscape.paintNode cy state stepState
          else 
              (if (final) then
                Cytoscape.paintNode cy state acceptState
              else 
                Cytoscape.paintNode cy state wrongFinalState)


      method private paintStates (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) states (isFinal: BasicTypes.state -> bool) = 
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        Set.iter (fun el -> self#paint cy el self#getLenghtLeftAccept (isFinal el)) states

      method private getLenghtLeftAccept = (Array.length steps) - position - 1

  end
end
