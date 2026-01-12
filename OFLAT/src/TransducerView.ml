open OCamlFlat
open BasicTypes
open AutomatonView

module TransducerView = 
struct
	open Transducer
	class model (arg: (t) Arg.alternatives) =
		object(self) 
      inherit AutomatonView.model arg as abstractAutomaton
      inherit Transducer.model arg as super

      method getInitialState = self#representation.initialState
      method getAcceptStates = self#representation.acceptStates
      method getStates = self#representation.states

      method reachableFromInitialState = 
        Transducer.reachable self#representation self#representation.initialState

      method productive = 
        Transducer.productive self#representation

      method areAllStatesUseful = 
        Transducer.isClean self#representation
      
      method isDeterministic = 
        Transducer.isDeterministic self#representation

      method numberStates = 
        Set.size self#representation.states

      method numberTransitions = 
        Set.size self#representation.transitions

      method getUselessStates =
        let useful = Set.inter self#productive self#reachableFromInitialState in
        Set.diff self#representation.states useful

      method staticGenerate n = 
        super#generate n

      method addNode node firstNode =
        if firstNode then
          (new model (Arg.Representation {
            inAlphabet = Set.empty;
            outAlphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty
          }))  
        else
          (let rep = self#representation in 
            new model (Arg.Representation { rep with states = Set.add node rep.states }))

      method addInitialNode node firstNode exists =
        if firstNode then
          (new model (Arg.Representation {
            inAlphabet = Set.empty;
            outAlphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty
          }))  
        else
          let rep = self#representation in
          let new_states = if exists then rep.states else Set.add node rep.states in
          new model (Arg.Representation { rep with states = new_states; initialState = node })

      method addFinalNode node firstNode exists = 
        if firstNode then
          (new model (Arg.Representation {
            inAlphabet = Set.empty;
            outAlphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.make [node]
          })) 
        else 
          let rep = self#representation in
          let new_states = if exists then rep.states else Set.add node rep.states in
          let new_accepts = Set.add node rep.acceptStates in
          new model (Arg.Representation { rep with states = new_states; acceptStates = new_accepts })

      method changeToFinal node =
        let rep = self#representation in 
        new model (Arg.Representation { rep with acceptStates = Set.add node rep.acceptStates })

      method removeFinal node =
        let rep = self#representation in 
        new model (Arg.Representation { rep with acceptStates = Set.remove node rep.acceptStates })

      method eliminateNode node isStart isFinish = 
        let rep = self#representation in
        let new_states = Set.remove node rep.states in
        let new_initial = if isStart then "" else rep.initialState in
        let new_accepts = if isFinish then Set.remove node rep.acceptStates else rep.acceptStates in
        let new_transitions = Set.filter (fun (s, _, _, d) -> s <> node && d <> node) rep.transitions in
        new model (Arg.Representation { 
          rep with 
          states = new_states; 
          initialState = new_initial; 
          acceptStates = new_accepts;
          transitions = new_transitions
        })

      method newTransition (src, inSym, outSym, dst) = 
        let rep = self#representation in
        let new_inAlpha = if inSym <> epsilon then Set.add inSym rep.inAlphabet else rep.inAlphabet in
        let new_outAlpha = if outSym <> epsilon then Set.add outSym rep.outAlphabet else rep.outAlphabet in
        let new_trans = Set.add (src, inSym, outSym, dst) rep.transitions in
        new model (Arg.Representation { 
          rep with 
          inAlphabet = new_inAlpha;
          outAlphabet = new_outAlpha;
          transitions = new_trans 
        })
      
      method eliminateTransition (src, inSym, outSym, dst) = 
        let rep = self#representation in 
        let new_trans = Set.remove (src, inSym, outSym, dst) rep.transitions in
        new model (Arg.Representation { rep with transitions = new_trans })

      method renameState state name =
        let rep = self#representation in 
        let new_initial = if state = rep.initialState then name else rep.initialState in
        let new_states = Set.remove state (Set.add name rep.states) in
        let new_transitions = Set.map (fun (s, i, o, t) -> 
          let ns = if s = state then name else s in
          let nt = if t = state then name else t in
          (ns, i, o, nt)
        ) rep.transitions in
        let new_accepts = Set.map (fun s -> if s = state then name else s) rep.acceptStates in
        new model (Arg.Representation {
          rep with
          states = new_states;
          initialState = new_initial;
          transitions = new_transitions;
          acceptStates = new_accepts
        })

      (* Draws edges with "input:output" labels *)
      method inputEdges (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        let mapToCytoscapeEdge transitions =
            Set.map (fun (src, iSym, oSym, dst) ->
                let label = (symb2str iSym) ^ ":" ^ (symb2str oSym) in
                (src, label, dst)
            ) self#representation.transitions
        in
        Set.iter (Cytoscape.addEdge cy) (mapToCytoscapeEdge self#representation.transitions)

      (* Overrides drawExample to ensure edges are drawn correctly *)
      method drawExample (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) = 
        self#inputNodes cy;
        self#inputEdges cy

      method paintCurrentStates (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) = ()
      method setInitialStep (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) = ()
      method setNextStep (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) = ()
      method staticAcceptFull = ()
      method displayTrace = ()
      method buildTable = ()
      
      method returnStats = (false, 0, false, 0.0)
      
      method private getCurrConfigFromBestPath = None
      method private getWordFromConfig _ = []

      method staticAccept = ()

      method getColors = 0
      method paintMinimization (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) (colors: string array) = ()
      method drawMinimize (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) (colors: string array) (n: int) = ()
      
      method errors = []
      method changeToEditModelMode (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) = ()
end
end