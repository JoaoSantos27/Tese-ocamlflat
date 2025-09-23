open OCamlFlat
open BasicTypes
open JS
open Js_of_ocaml
open Js.Opt
open Lang

module ContextFreeGrammarBasicView =
struct
	open ContextFreeGrammarBasic

	class model (arg: t Arg.alternatives) =
		object(self) inherit ContextFreeGrammar.model arg as super
	end
end
