(*
 * LocalStorage.ml
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
 *  Written by Alexandre Godinho
 *)

open Js_of_ocaml
open Lang
open JS

module LocalStorage = 
struct 
    let localStorage = Js.Optdef.get Dom_html.window##.localStorage (fun () -> failwith "localStorage not available")

    let setItem key value =
        localStorage##setItem (JS.string key) (JS.string value)
  
    let getItem key =
      Js.to_string (Js.Opt.get (localStorage##getItem (JS.string key)) (fun () -> Js.string ""))

    let removeItem key =
      localStorage##removeItem (JS.string key)

end