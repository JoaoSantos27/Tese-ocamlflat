/*
 * OFLAT/BridgeHelper.js
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
 * Written by Artur Miguel Dias and Rita Macedo
 */

/*

function selectConversion() {
	let x = document.getElementById("selectConversion");
	let y = x.selectedIndex;
	jscode.selectConversions(y);
	x.selectedIndex = 0;
}

function selectLang() {
	let x = document.getElementById("selectLang");
	let y = x.selectedIndex;
	jscode.selectLang(y);
	x.selectedIndex = 0;
}

function openFile(event) {
	let input = event.target;
	let reader = new FileReader();
	reader.onload =
		function() {
			let text = reader.result;
			jscode.readFromFile(text);
		};
	reader.readAsText(input.files[0]);
}

function closeFunction() {
	let modal = document.getElementById("myModal");
	modal.style.display = "none";
}

*/


/******************/

function createLearnOcamlButton() {
	let element = document.createElement("button");
	element.setAttribute("id", "dinamic");
	element.setAttribute("onclick", "jscode.getModel()");
	element.innerHTML = "Learn-OCaml";
	document.getElementById("oflat").appendChild(element);
}

function show(text) {
	let debug = false;
	if (debug)
		console.log(text);
}

let bc = null;

function bcSend(message) {
	show("SEND: " + message);
	bc.postMessage(message);
}

function bcReceive(message) {
	let text = message.data;
	show("RECEIVE: " + text);
	if (text === "rendezvous")
		bcSend("rendezvous");
	else
		jscode.createAutomaton(text);
}

function createBroadcastChannel() {
	bc = new window.BroadcastChannel("oflat_channel");
	bc.onmessage = bcReceive;
}

function startup() {
	let option = window.location.search;
	if (option === "?learn-ocaml") {
		createLearnOcamlButton();
		createBroadcastChannel();
	}
	jscode.start();
}
