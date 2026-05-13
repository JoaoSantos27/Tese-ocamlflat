(*
 * RegularExpression.ml
 *
 * This file is part of the OCamlFLAT library
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
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * set/2022 (amd) - Full restructuration.
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * may/2021 (amd) - Added support for an extern representation.
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Regular expressions functionality.
 *
 * TODO: More cleanup. Improve the regular expression simplifier.
 *)

open BasicTypes

module RegularExpressionPartialDerivative =
struct
	open RegularExpressionSupport

	let rec hasEmpty (rep: t): bool =
		match rep with
		| Plus(l, r) -> hasEmpty l && hasEmpty r
		| Seq(l, r) -> hasEmpty l || hasEmpty r
		| Star _ | Empty -> true
		| _ -> false

	let rightConcat (s: t set) (rep: t): t set =
		match rep with
		| Empty -> s
		| Zero -> Set.empty
		| _ -> Set.map (fun e -> Seq(e,rep)) s

	let rec partialDerivative (rep: t) (a: symbol): t set =
		let pd = partialDerivative in
		match rep with
		| Plus(l, r) -> Set.union (pd l a) (pd r a)
		| Seq(l, r) when hasEmpty l -> Set.union (rightConcat (pd l a) r) (pd r a)
		| Seq(l, r) -> rightConcat (pd l a) r
		| Star r -> rightConcat (pd r a) (Star r)
		| Symb c when c=a -> Set.make [Empty]
		| Symb c -> Set.empty
		| Empty -> Set.empty
		| Zero -> Set.empty

	let partialDerivativeUno (rep: t) (a: symbol): t =
		let s = partialDerivative rep a in
			Set.fold_right ((fun r s -> Plus(r,s))) s Zero
end

module RegularExpressionPrivate =
struct
	open RegularExpressionSupport

	(** aux *)
	let seqConcat aset bset = Set.flatMap (fun s1 -> Set.map (fun s2 -> s1@s2) bset) aset

	let validate (name: string) (rep: t): unit =
		(

						(*
				let representation = RegExpSyntax.parse "(((xx+ut)+(aaa+dss+ghf)+(xx+uu))ee)+bgc*+(jgg+bgcd)" in

				let rec lang rep =
					match rep with
						| RegExpSyntax.Plus(l, r) -> Util.print "pls: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Seq(l, r) -> Util.print "seq: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Star(r) -> Util.print "str: "; Util.print (RegExpSyntax.toString r); Util.println ""; (lang r)
						| RegExpSyntax.Symb(c) -> Set.make [c]
						| RegExpSyntax.Empty -> Set.empty
						| RegExpSyntax.Zero -> Set.empty
				in
				let a = lang representation in
				()
				*)
		)


	(**
	* This method generates the alphabet of all symbols in the expression
	* @returns symbols -> the set of all symbols in the expression's alphabet
	*)
	let rec alphabet rep: symbols =
	match rep with
		| Plus(l, r) -> Set.union (alphabet l) (alphabet r)
		| Seq(l, r) -> Set.union (alphabet l) (alphabet r)
		| Star(r) -> alphabet r
		| Symb(c) -> Set.make [c]
		| Empty -> Set.empty
		| Zero -> Set.empty

	(**
	* This method generates the language of the regular expression for when klenne is always zero
	* @returns words -> set of generated words
	*)
	let rec quasiLanguage rep: words =
		match rep with
		| Plus(l, r) -> Set.union (quasiLanguage l) (quasiLanguage r)
		| Seq(l, r) -> seqConcat (quasiLanguage l) (quasiLanguage r)
		| Star(r) -> Set.make [[]]
		| Symb(c) -> Set.make [[c]]
		| Empty -> Set.empty
		| Zero -> Set.empty

	(** aux *)
	let partition w =
		let rec partX w pword =
			match w with
				[] -> Set.empty
				| x::xs -> let fwp = pword@[x] in
								Set.add (fwp, xs) (partX xs fwp) in
		Set.add ([],w) (partX w [])

	(**
	* This method tests if a given word is accepted by the regular expression
	* @param w:word -> word to be tested for acceptance
	* @returns bool -> true if w is accepted and false otherwise
	*)
	let rec accept rep (w: word): bool =
		match rep with
		| Plus(l, r) -> (accept l w) || (accept r w)
		| Seq(l, r) ->
			let wpl = partition w in
				Set.exists (fun (wp1,wp2) -> (accept l wp1) && (accept r wp2)) wpl
		| Star(re) ->
			w = []
			|| (let wpl = Set.remove ([],w) (partition w) in
					Set.exists (fun (wp1,wp2) -> (accept re wp1) && (accept (Star re) wp2)) wpl)
		| Symb(c) -> w = [c]
		| Empty -> w = []
		| Zero -> false

	(**
	* This method returns the derivation tree for the word acceptance
	* @param w:word -> word to be tested for acceptance
	* @returns reTree list -> list of derivation trees
	*)
	let allTrees rep w : unit =

		let rec acc rep w =
			match rep with
			| Plus(l, r) ->
				let l1 = acc l w in
				let r1 = acc r w in
					List.map (fun t -> Tree (w, rep, [t])) (l1 @ r1)

			| Seq(l, r) ->
				let wps = partition w in
				let wpl = Set.toList wps in
				List.flatten ( List.map (fun (wp1, wp2) ->
					let tl = acc l wp1 in
					let tr = acc r wp2 in
						List.flatten (List.map (fun x -> List.map
							(fun y -> Tree (w, rep, [x; y])) tr)tl)
				) wpl)

			| Star(re) ->
				if w = [] then
					[Tree ([epsilon], rep, [])]
				else
					(let wps = Set.remove ([],w) (partition w) in
					let wpl = Set.toList wps in
					List.flatten (List.map (fun (wp1, wp2) ->
						let tl = acc re wp1 in
						let tr = acc (Star re) wp2 in
						List.flatten (List.map (fun x -> List.map
							(fun y -> Tree (w, rep, [x; y])) tr) tl)) wpl))

			| Symb(c) ->
				if w = [c] then
					[Tree (w, rep, [])]
				else
					[Tree (w, rep, [Fail])]

			| Empty ->
				if w = [] then
					[Tree ([epsilon], rep, [])]
				else
					[Tree (w, rep, [Fail])]

			| Zero -> [Tree (w, rep, [Fail])]

		in

		let ac = acc rep w in



		let rec isNotFail t =
			match t with
				Fail -> false
				| Tree ([], re, []) -> true
				| Tree (w, re, []) -> true
				| Tree ([], re, x::xs) -> (isNotFail x) && (isNotFail (Tree ([], re, xs)))
				| Tree (w, re, x::xs) -> (isNotFail x) && (isNotFail (Tree (w, re, xs)))
		in

		let ts = List.filter (fun t -> isNotFail t) ac in


		let printTreeX w re n =
			let s = String.make (3*n) ' ' in
			Util.println [s; word2str w; " -> "; RegularExpressionSyntax.toString re]
		in

		let rec printTree t n =
			match t with
				Fail -> Util.println ["Fail"]
				| Tree ([], re, []) -> Util.println ["TREH "]
				| Tree (w, re, []) -> printTreeX w re n
				| Tree ([], re, x::xs) -> printTreeX [] re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
				| Tree (w, re, x::xs) -> printTreeX w re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
		in

			List.iter (fun t -> printTree t 0) ts


	(**
	* This method generates all words up to the given length that are generated by the regular expression
	* @param length:int -> maximum length of all generated words
	* @returns words -> set of generated words
	*)
	let rec generate rep (len: int): words =
		match rep with
		| Plus(l, r) ->
				Set.union (generate l len) (generate r len)
		| Seq(l, r) ->
				let left = generate l len in
				let rigth w = generate r (len - (List.length w)) in
				let conc w = Util.concatAll w (Set.toList (rigth w)) in
					Set.flatMap (fun lw -> Set.make (conc lw)) left
		| Star r ->
				let exp = generate r len in
					Set.star exp len

			(* alternate version of star, leave 4 now

			let rec starX ws sz =
				if sz <= 0 then Set.make [[]]
				else
					let ws = Set.filter (fun x -> sz >= (List.length x)) ws in
					let newLn w = sz - (List.length w) in
					let tail w ws = Set.toList (starX ws (newLn w)) in
					let conc w ws = Util.concatAll w (tail w ws) in
					let track w ws = Set.add w (Set.make (conc w ws)) in
						Set.flatMap (fun w -> if w = [] then Set.make [[]] else track w ws) ws in
			let exp = generate r len in
				Set.add [] (starX exp len)*)

		| Symb(c) -> if len > 0 then Set.make [[c]] else Set.empty
		| Empty -> Set.make [[]]
		| Zero -> Set.empty


	(**
	* This method simplifies the regular expression
	*
	* @returns RegularExpression.model -> the new simplified, equivalent expression
	*
	* https://www.ict.griffith.edu.au/teaching/3515ICT/lectures/regular.pdf
	* https://youtu.be/u1eR88wxAWI
	* https://www.youtube.com/watch?v=xLp36MhQ_D8
	* Rich 106
	* https://people.csail.mit.edu/meyer/rsq.pdf
	* !! https://www.researchgate.net/publication/221570367_Optimal_Lower_Bounds_on_Regular_Expression_Size_Using_Communication_Complexity
	*
	
	converting deterministic ﬁnite automata accepting ﬁnite languages into reg-ular expression leads to an inevitable blow-up in size of nΘ(log n), 
	
	https://cs.uwaterloo.ca/research/tr/1994/01/intro.pdf
	
Simplifying Regular Expressions. A Quantitative Perspective
	https://www.uni-trier.de/fileadmin/fb4/prof/INF/TIN/Veroeffentlichungen/gruber_gulan_lata2010.pdf
	
	
Enumerating Regular Expressions and Their Languages
	https://cs.uwaterloo.ca/~shallit/Papers/ciaa-04.pdf
	
	Regular expressions have been studied for almost fifty years, yet many interesting
and challenging problems about them remain unsolved.

The equivalence problem for regular expressions with squaring requires exponential space
https://people.csail.mit.edu/meyer/rsq.pdf


Simplification of regular expression
https://cs.stackexchange.com/questions/93027/simplification-of-regular-expression
 to stare at that regular expression, understand very well what language it recognizes, and write down a shorter freah regexp

How can one ACTUALLY minimize a regular expression?
https://cstheory.stackexchange.com/questions/31630/how-can-one-actually-minimize-a-regular-expression

minimizing size of regular expression
https://cstheory.stackexchange.com/questions/12361/minimizing-size-of-regular-expression


minimizing size of regular expression for finite sets
https://cstheory.stackexchange.com/questions/16860/minimizing-size-of-regular-expression-for-finite-sets

On Learning Regular Languages
https://cstheory.blogoverflow.com/2011/08/on-learning-regular-languages/


Simplifying regular expressions further
https://www.sciencedirect.com/science/article/abs/pii/S0747717121000572

However, although various algebraic laws about expressions (Salomaa, 1966) are routinely used as simplification rules, there is no efficient equivalent to FSA minimisation for expressions.

Regular expression simplification
https://www.sciencedirect.com/science/article/abs/pii/S0378475497000864


A Program That Simplifies Regular Expressions (Tool paper)
https://arxiv.org/abs/2307.06436

Optimizing regular expressions
https://blog.gistre.epita.fr/posts/thomas.corbiere-2024-09-22-optimizing-regular-expressions/

Efficient Enumeration of Regular Expressions for Faster Regular Expression Synthesis
https://www.academia.edu/66935755/Efficient_Enumeration_of_Regular_Expressions_for_Faster_Regular_Expression_Synthesis

Maple
https://www.maplesoft.com/msw/program/MSW04FinalProgram.pdf




A tool for optimizing regular expressions
https://www.codeproject.com/Articles/5388733/A-tool-for-optimizing-regular-expressions
NAIVE

Simplification of regular expression
https://cs.stackexchange.com/questions/93027/simplification-of-regular-expression

Optimizing Regular Expressions via Rewrite-Guided Synthesis
https://arxiv.org/abs/2104.12039

Analysis of an efficient reduction algorithm for
random regular expressions based on universality
detection
https://hal.science/hal-04484361/document

https://regex101.com/


----
Regular expressions into finite automata
https://www.sciencedirect.com/science/article/pii/0304397593902874

Gruber, H., Gulan, S.: Simplifying regular expressions.
https://www.informatik.uni-giessen.de/reports/Report0904.pdf

Follow automata
https://www.sciencedirect.com/science/article/pii/S0890540103000907

Enumerating regular expressions and their languages.
https://cs.uwaterloo.ca/~shallit/Papers/ciaa-04.pdf

Short Regular Expressions from Finite Automata: Empirical Results
http://www.hermann-gruber.com/data/ciaa09-final.pdf

Approximation to the Smallest Regular Expression for a Given Regular Language
https://link.springer.com/chapter/10.1007/978-3-540-30500-2_31

https://www.gap-system.org/

Obtaining shorter regular expressions from finite-state automata
https://toc.yonsei.ac.kr/~emmous/papers/tcs_se.pdf

How to convert finite automata to regular expressions?
https://cs.stackexchange.com/questions/2016/how-to-convert-finite-automata-to-regular-expressions
Brzozowski algebraic method
https://web.archive.org/web/20140922220310/http://codepad.org/dbFztCCM

Here are some useful rules for simplifying RE's:

Shifting Rule: E(FE)* = (EF)*E
Denesting Rule: (E*F)*E* = (E+F)*

Conversion of Deterministic and Non-Deterministic Finite Automata to Regular Expression using Brzozowski Algebraic Method
https://www.researchgate.net/publication/346016950_Conversion_of_Deterministic_and_Non-_Deterministic_Finite_Automata_to_Regular_Expression_using_Brzozowski_Algebraic_Method

New Heuristics for Obtaining Smaller Regular Expressions from Deterministic Finite Automata
https://www.ripublication.com/ijaer17/ijaerv12n22_26.pdf

Regular-expression derivatives reexamined
https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf

Comparisons Amongst Different Techniques for Conversion of Deterministic Finite
Automata to Regular Expression
https://www.ijarcs.info/index.php/Ijarcs/article/download/491/479/957

Manipulation of Regular Expressions Using
Derivatives: an Overview
https://www.dcc.fc.up.pt/~nam/resources/publica/flan14.pdf

Converting Deterministic Finite Automata to
Regular Expressions
http://www.sci.brooklyn.cuny.edu/~zhou/teaching/cis3160/ConvertDFSMtoRegExp.pdf

RE -> NFA 



RE simplify = PSPACE-complete (Quantitative Perspective)




	
	*)

(* +++ simplify original 

	let simplify (rep: t): t =

	  (* various base case simplification rules to apply to the given expressions *)
	  let simpX re =
		match re with
			(* plus *)
			(* a* + empty -> a*  *)
			| Plus(Star(l), Empty) -> Star(l)
			| Plus(Empty, Star(r)) -> Star(r)
			(* a + zero = a  *)
			| Plus(Zero, r) -> r
			| Plus(l, Zero) -> l
			(* ~ + aa* = a*  *)
			| Plus(Empty, Seq(l, Star(r))) when l = r -> Star(r)
			| Plus(Empty, Seq(Star(l), r)) when l = r -> Star(l)
			| Plus(Seq(l, Star(r)), Empty) when l = r -> Star(r)
			| Plus(Seq(Star(l), r), Empty) when l = r -> Star(l)
			(* a* + a + empty = a* ????? *)
			| Plus(Star(l), Plus(Empty, r)) when l = r -> Star(l)
			| Plus(Star(l), Plus(r, Empty)) when l = r -> Star(l)
			| Plus(Plus(Empty, l), Star(r)) when l = r -> Star(r)
			| Plus(Plus(l, Empty), Star(r)) when l = r -> Star(r)
			(* a* + a = a* *)
			| Plus(Star(l), r) when l = r -> Star(l)
			| Plus(l, Star(r)) when l = r -> Star(r)
			(* a + a = a  *)
			| Plus(l, r) when l = r -> l
			(* seq *)
			| Seq(Empty, Empty) -> Empty
			| Seq(Zero, Zero) -> Zero
			| Seq(Empty, r) -> r
			| Seq(l, Empty) -> l
			| Seq(Zero, r) -> Zero
			| Seq(l, Zero) -> Zero
			(* (~+a)a* = a* *)
			| Seq(Plus(Empty, l),Star(r)) when l = r -> Star(r)
			| Seq(Plus(l, Empty),Star(r)) when l = r -> Star(r)
			| Seq(Star(l),Plus(Empty, r)) when l = r -> Star(l)
			| Seq(Star(l),Plus(r, Empty)) when l = r -> Star(l)
			| Seq(Star(l),Star(r)) when l = r -> Star(l)
			(* star a** = a* *)
			| Star(Star(r)) -> Star(r)
			(* star (~+a)* = a* *)
			| Star(Plus(Empty, r)) -> Star(r)
			| Star(Plus(r, Empty)) -> Star(r)
			(* star (~)* = ~ *)
			| Star(Empty) -> Empty
			(* star (0)* = ~ *)
			| Star(Zero) -> Empty
			(* other *)
			| _ -> re
		in

		(* applies various base case simplifications to the various sub-expressions of regular expression re *)
		let rec simplify re =

			match re with
				| Plus(l,r) -> simpX (Plus(simplify l, simplify r))
				| Seq(l,r) -> simpX (Seq(simplify l, simplify r))
				| Star(re) -> simpX (Star(simplify re))
				| Symb(c) -> Symb c
				| Empty -> Empty
				| Zero -> Zero
		in

			simplify rep

+++ simplify original *)

(* +++ simplify do Alexandre *)

	(**
	* This method simplifies the regular expression
	*
	* @returns RegularExpression.model -> the new simplified, equivalent expression
	*)
	let simplify (rep: t): t =
	  (* if an expression already exists inside a sum then we remove it *)
	  let rec removeDuplicates (re: t): t = 
	  	let rec existsIn (first: t) (second: t): bool =
	  		if first = second then true
	  		else match second with
		  		| Plus(l, r) -> if first = l then true else existsIn first r
			  	| _ -> false
	  	in
	  	match re with
		  	| Plus(l, r) -> let left = (removeDuplicates l) and right = (removeDuplicates r) in
		  		if (existsIn left right) then right else Plus(left, right)
		  	| Seq(l, r) -> Seq(removeDuplicates l, removeDuplicates r)
		  	| Star(l) -> Star(removeDuplicates l)
		  	| _ -> re
	  in

	  (* Shifts the expression to be left alligned *)
	  let rec shift (re: t): t =
	  	match re with
	  	| Plus(Plus(l1, r1), Plus(l2, r2)) -> Plus( shift l1, shift (Plus(r1, Plus(l2, r2))) )
	  	| Plus(Plus(l1, r1), r2) -> Plus(shift l1, Plus(shift r1, shift r2)) (* r2 is not a plus *)
	  	| Seq(Star(l1), Seq(l2, r2)) when l1 = l2 -> Seq( shift l1, shift (Seq(Star(l2), r2)) )
	  	| Seq(l, r) -> Seq(shift l, shift r)
	  	| Star(l) -> Star(shift l)
	  	| _ -> re
	  in

	  (* Applies the distributive property *)
	  let distribute (re: t): t =
	  	match re with
	  	| Plus(l1, Seq(l2, r2)) when l1 = l2 -> Seq(l1, Plus(Empty, r2)) (* doesn't work for nested expressions *)
	  	| Plus(l1, Seq(l2, r2)) when l1 = r2 -> Seq(l1, Plus(Empty, l2))
	  	| Plus(Seq(l1, r1), r2) when l1 = r2 -> Seq(r2, Plus(Empty, r1))
	  	| Plus(Seq(l1, r1), r2) when r1 = r2 -> Seq(r2, Plus(Empty, l1))
	  	| _ -> re
	  in

	  (* Checks for equality of 2 sides of a sum *)
	  let rec allExpressionsEqual (first: t) (second: t): bool =
	  	if first = second then true
	  	else match second with
	  	    | Plus(l, r) -> allExpressionsEqual first l && allExpressionsEqual first r
	  		| Seq(l, r) -> allExpressionsEqual first l && allExpressionsEqual first r
	  		| Star(l) -> allExpressionsEqual first l
	  		| _ -> false
	  in

	  (* Checks for non consecutive equal expressions in sums. a+b+a -> a + b *)
	  let rec existsInPlus (first: t) (second: t): bool =
	  	if first = second then true
	  	else match second with
	  	     | Plus(l, r) -> existsInPlus first l || existsInPlus first r
	  	     | _ -> false
	  in
	  (* various base case simplification rules to apply to the given expressions *)
	  let simpX re =
		match re with
			(* (a*a)* -> a* *)
			| Star(Seq(l, Star(r))) when l = r -> Star(l)
			| Star(Seq(Star(l), r)) when l = r -> Star(l)
			(* a*+a... -> a* *)
			| Plus(Star(l), r) when (allExpressionsEqual l r) -> Star(l)
			| Plus(l, Star(r)) when (allExpressionsEqual r l) -> Star(l)
			(* (a...+a...)* -> a* *)
			| Star(Plus(l, r)) when (allExpressionsEqual l r) -> Star(l)
			(* a*a*b -> a*b *)
			| Seq(Star(l1), Seq(Star(l2), r2)) when l1 = l2 -> Seq(Star(l1), r2)
			(* (a+b)*b* -> (a+b)* *)
			| Seq(Star(Plus(l1, r1)), Star(r2)) when (existsInPlus r2 (Plus(l1, r1))) -> Star(Plus(l1, r1))
			| Seq(Star(l1), Star(Plus(l2, r2))) when (existsInPlus l1 (Plus(l2, r2))) -> Star(Plus(l2, r2))
			(* plus *)
			(* a* + empty -> a*  *)
			| Plus(Star(l), Empty) -> Star(l)
			| Plus(Empty, Star(r)) -> Star(r)
			(* a + zero = a  *)
			| Plus(Zero, r) -> r
			| Plus(l, Zero) -> l
			(* ~ + aa* = a*  *)
			| Plus(Empty, Seq(l, Star(r))) when l = r -> Star(r)
			| Plus(Empty, Seq(Star(l), r)) when l = r -> Star(l)
			| Plus(Seq(l, Star(r)), Empty) when l = r -> Star(r)
			| Plus(Seq(Star(l), r), Empty) when l = r -> Star(l)
			(* a* + a + empty = a* ????? *)
			| Plus(Star(l), Plus(Empty, r)) when l = r -> Star(l)
			| Plus(Star(l), Plus(r, Empty)) when l = r -> Star(l)
			| Plus(Plus(Empty, l), Star(r)) when l = r -> Star(r)
			| Plus(Plus(l, Empty), Star(r)) when l = r -> Star(r)
			(* a* + a = a* *)
			(*| Plus(Star(l), r) when l = r -> Star(l) not needed anymore *)
			(*| Plus(l, Star(r)) when l = r -> Star(l) not needed anymore *)
			(* a + a = a  *)
			(*| Plus(l, r) when l = r -> l not needed anymore *)
			(* seq *)
			| Seq(Empty, Empty) -> Empty
			| Seq(Zero, Zero) -> Zero
			| Seq(Empty, r) -> r
			| Seq(l, Empty) -> l
			| Seq(Zero, r) -> Zero
			| Seq(l, Zero) -> Zero
			(* (~+a)a* = a* *)
			| Seq(Plus(Empty, l),Star(r)) when l = r -> Star(r)
			| Seq(Plus(l, Empty),Star(r)) when l = r -> Star(r)
			| Seq(Star(l),Plus(Empty, r)) when l = r -> Star(l)
			| Seq(Star(l),Plus(r, Empty)) when l = r -> Star(l)
			| Seq(Star(l),Star(r)) when l = r -> Star(l)
			(* star a** = a* *)
			| Star(Star(r)) -> Star(r)
			(* star (~+a)* = a* *)
			| Star(Plus(Empty, r)) -> Star(r)
			| Star(Plus(r, Empty)) -> Star(r)
			(* star (~)* = ~ *)
			| Star(Empty) -> Empty
			(* star (0)* = ~ *)
			| Star(Zero) -> Empty
			(* other *)
			| _ -> re
		in

		(* applies various base case simplifications to the various sub-expressions of regular expression re *)
		let rec simplify re =

			match re with
				| Plus(l,r) -> simpX (Plus(simplify l, simplify r))
				| Seq(l,r) -> simpX (Seq(simplify l, simplify r))
				| Star(re) -> simpX (Star(simplify re))
				| Symb(c) -> Symb c
				| Empty -> Empty
				| Zero -> Zero
		in

			removeDuplicates (simplify (shift (distribute rep)))





			(*CODIGO JP*)

	type configuration =
		t * word
	type configurations =
		configuration set
	type path =
		configuration list
	type trail =
		configurations list

	let initialConfigs (rep: t) (w: word): configurations =
		Set.make [(rep, w)]

	let nextConfigs (rep: t) (re, w) =
		match w with
		| [] -> Set.empty
		| x :: xs ->
			let der = RegularExpressionPartialDerivative.partialDerivativeUno re x in
				if der = Zero then Set.empty
				else Set.make [(der, xs)]

	let nextConfigs2 (rep: t) (re, w) : configurations =
		match w with
			| [] -> Set.empty
			| x :: xs ->
				let der = RegularExpressionPartialDerivative.partialDerivative re x in
					Set.map (fun re -> (re,xs)) der 


	let isAcceptingConfig (rep: t) (re,w) : bool =
		RegularExpressionPartialDerivative.hasEmpty re && w = []

	let accept (res: t) (w: word) : bool =
		Model.accept res w initialConfigs nextConfigs2 isAcceptingConfig
	
	let acceptFull (res: t) (w: word) : bool * path * trail =
		Model.acceptFull res w initialConfigs nextConfigs2 isAcceptingConfig

		let nextConfigs3 (_: t) (len: int) (re, w) : configurations = 
			show re;
			Util.show (word2str w);
				Set.flatMap (fun sym ->
					Util.show (symb2str sym);
					let der = RegularExpressionPartialDerivative.partialDerivative re sym in
						Set.map (fun re -> (re, sym::w)) der 
					) (alphabet re)

		let isAcceptingConfig2 (rep: t) (re,w) =
			RegularExpressionPartialDerivative.hasEmpty re

		let getWord (_, w) = List.rev w;;

		let generate (rep: t) (len: int): words =
			Model.generate rep len initialConfigs nextConfigs3 isAcceptingConfig2 getWord
					
end

module RegularExpression =
struct
	include RegularExpressionSupport
	open RegularExpressionPartialDerivative
	open RegularExpressionPrivate
	
	(* Make *)
	let make2 (arg: t Arg.alternatives): Entity.t * t = make2 arg validate
	let make (arg: t Arg.alternatives): t = make arg validate
	let show = show

	(* Exercices support *)
	let checkProperty (re: t) (prop: string) =
		match prop with
		| "regular expression" -> true
		| _ -> Model.checkProperty prop
	let checkExercise ex re = Model.checkExercise ex (accept re) (checkProperty re)	
	let checkExerciseFailures ex re = Model.checkExerciseFailures ex (accept re) (checkProperty re)	

	(* Ops *)
	let stats = Model.stats
	let accept = accept
	let generate = generate
	let alphabet = alphabet
	let quasiLanguage = quasiLanguage
	let allTrees = allTrees
	let simplify = simplify
	let partialDerivative = partialDerivative
	let partialDerivativeUno = partialDerivativeUno
	
	(* Class *)
	class model (arg: t Arg.alternatives) =
		object(self) inherit Model.model (make2 arg) as super
		(* Representation *)
			method representation = representation
		(* Kind *)
			method isRegularExpression : bool = true
		(* Show *)			
			method toJSon: JSon.t = toJSon representation
			method toJSon2: JSon.t = toJSon2 id representation
			method show: unit = show representation
			method show2: unit = show2 id representation
		(* Ops *)				
			method accept w = accept representation w
			method acceptFull (w: word) : bool * path * trail = acceptFull representation w
			method allTrees w = allTrees representation w
			method generate n = generate representation n
			method alphabet = alphabet representation
			method quasiLanguage = quasiLanguage representation
			method simplify = new model (Arg.Representation (simplify representation))

			method partialDerivative sy =
				let s = partialDerivative representation sy in
					Set.map (fun rep -> new model (Arg.Representation rep)) s

		(* Exercices support *)
			method checkProperty (prop: string) = checkProperty representation prop

		(* Learn-OCaml support *)
			method moduleName = moduleName
			method xTypeName = xTypeName
			method xTypeDeclString : string = prelude
			method toDisplayString (name: string): string = solution name self#representation
			method example : JSon.t = example
		end
end

module RegularExpressionTop =
struct
	open RegularExpression
(*
	let reF file = make (Arg.File file)
	let reT text = make (Arg.Text text)
	let reI x = internalize x
	let reX re = externalize (simplify re)

	let re_load file = externalize (reF file)
	let re_predef name = externalize (reT (Examples.example name))
	let re_text text = externalize (reI text)

	let re_accept re w = accept (reI re) (wordI w)
	let re_pd re sy = reX (partialDerivativeUno (reI re) (symbI sy))
*)
end

open RegularExpressionTop

(*
	let re_build (text: string): RegularExpressionBasics.tx =
		let re = new RegularExpression.model (Arg.Text text) in
			re#representationx

	let re_predef (name: string): RegularExpressionBasics.tx =
		re_build (Examples.example name)

	let re_convertTo (re: RegularExpressionBasics.t) : RegularExpressionBasics.tx =
		RegularExpression.toString re

	let re_convertFrom (re: RegularExpressionBasics.tx): RegularExpressionBasics.t = RegularExpression.parse re

	let re_accept (re: RegularExpressionBasics.tx) (w: string): bool =
		let re = re_convertFrom re in
		let a = new RegularExpression.model (Arg.Representation re) in
		let w = str2word w in
			a#accept w
*)
