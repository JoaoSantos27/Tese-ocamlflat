(*
 * Lang.ml
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

(* 
 * Description: support for internationalization.
 *)

open OCamlFlat
open Js_of_ocaml
open CSS

module Lang =
  struct

    let default_language () =
	  (Js.Opt.get
        Dom_html.window##.navigator##.language
          (fun () ->
            Js.Optdef.get Dom_html.window##.navigator##.userLanguage (fun () -> Js.string "en")))
        ##substring
        0
        2
  (* NOVO!
    let default_language () =
      (Js.Optdef.get
        Dom_html.window##.navigator##.language
          (fun () ->
            Js.Optdef.get Dom_html.window##.navigator##.userLanguage (fun () -> Js.string "en")))
        ##substring
        0
        2
     *)
      
    let lang =
      ref
      (Js.to_string (Js.Optdef.case Dom_html.window##.localStorage default_language (fun st ->
          Js.Opt.get (st##getItem (Js.string "hyp_lang")) default_language)))

    let set_language language =
        lang := Js.to_string (language)
    
    let i18nKey = 
      ["en"; "pt"; "fr"]
    
    let rec i18n sl =
      let rec i18nX l sl =
        match l, sl with
          | [], _ -> Error.fatal "i18nX"
          | _, [] -> Error.fatal "i18nX"
          | x::_, y::_ when x = !lang -> y
          | _::xs, _::ys -> i18nX xs ys
      in
        i18nX i18nKey sl
  
  (** ---------------- Barra lateral primeiro bloco ----------------- **)
  let i18nTitle () = 
    i18n ["OFLAT"; "OFLAT"; "OFLAT"]

  let i18nVersion () = 
    i18n ["Version 4.1"; "Versão 4.1"; "Version 4.1"]
    
  let i18nNewModel () =
    i18n ["New model"; "Novo modelo"; "Nouveau modèle"]
    
  let i18nEditModel () =
    i18n ["Edit model"; "Editar modelo"; "Modifier le modèle"] 

  let i18nFitGraph () = 
    i18n ["Fit to box"; "Ajustar à caixa"; "Ajustement à la boîte"]

  let i18nToggleView () = 
    i18n ["Toggle view"; "Alternar visualização"; "Basculer la vue"]

  let i18nGeneralRepresentation () = 
    i18n ["General purpose representation of the derivation tree,\n for context free grammars use context free model";
          "Representação geral da árvore de derivação,\n para gramáticas livres de contexto use o modelo de gramática livre de contexto";
          "Représentation générale de l'arbre de dérivation,\n pour les grammaires libres de contexte, utilisez le modèle de grammaire libre de contexte"]

(** ---------------- Barra lateral segundo bloco ----------------- **)
  let i18nGenerate () = 
    i18n ["Generate words with size X"; "Gerar palavras de tamanho X"; "Générer des mots de taille X"]
  
  let i18nTesting () = 
    i18n ["Test word"; "Testar palavra"; "Mot de test"]

  let i18nStep () = 
    i18n ["Step-by-step word acceptance"; "Aceitação passo-a-passo da palavra"; "Acceptation des mots étape par étape"]

  let i18nStart () = 
    i18n ["Start"; "Início"; "Début"]

  let i18nClearAuto () = 
    i18n ["Clean Automaton"; "Limpar Autómato"; "Nettoyer l'automate"]
  
  let i18nConvertToAcceptFinalState () = 
    i18n ["Convert to acceptance by final states"; "Converter aceitação por estados finais"; "Convertisseur accepté par les états finaux"]

  let i18nConvertToAcceptEmptyStack () = 
    i18n ["Convert to acceptance by empty stack"; "Converter aceitação por pilha vazia"; "Convertisseur pour pile vide accepter"]

(** ---------------- Barra lateral terceiro bloco ----------------- **)
  let i18nSelectConv () = 
    i18n ["Convert to"; "Converter em"; "Convertir en"]

  let i18nSelectRegex () = 
    i18n ["Regular Expression"; "Expressão Regular"; "Expression régulière"]

  let i18nselectFA () = 
    i18n ["Finite Automaton"; "Autómato Finito"; "Automate Fini"]

  let i18nselectFST () = 
    i18n ["Finite State Transducer"; "Transdutor de Estado Finito"; "Transducteur d'État Fini"]

  let i18nselectPDA () = 
    i18n ["Pushdown Automaton"; "Autómato de Pilha"; "Automate à Pile"]

  let i18nselectCFG () = 
    i18n ["Context Free Grammar"; "Linguagem Livre de Contexto"; "Angages Hors-Contexte"]

  let i18nselectGR () = 
      i18n ["Grammar"; "Gramática"; "Grammaire"]

  let i18nselectTM () = (* carolina *)
    i18n ["Turing Machine"; "Máquina de Turing"; "Machine de Turing"]

  let i18nselectTM2Tapes () =
    i18n ["Turing Machine 2 Tapes"; "Máquina de Turing 2 Fitas"; "Machine de Turing 2 Bandes"]

(** ---------------- Barra lateral quarto bloco ----------------- **)
  
let i18nImportModel () = 
  i18n ["Import model"; "Importar modelo"; "Modèle d'importation"]
  
let i18nExportModel () =
  i18n ["Export model"; "Exportar modelo"; "Modèle d'exportation"]

let i18nServer () = 
  i18n ["Predefined examples"; "Exemplos predefinidos"; "Exemples prédéfinis"] 

(** ---------------- Barra lateral quinto bloco ----------------- **)

let i18nSelectedL () = 
  i18n ["Language"; "Língua"; "Langue"]

let i18nSelectPT () = 
  i18n ["Portuguese"; "Português"; "Portugais"]

let i18nSelectEN () = 
  i18n ["English"; "Inglês"; "Anglaise"]

let i18nSelectFR () = 
  i18n ["French"; "Francês"; "Français"]

let i18nAbout () = 
  i18n ["About"; "Sobre"; "À propos"]

let i18nFeedback () = 
  i18n ["Feedback"; "Comentários"; "Commentaires"]

(** ---------------- Barra de topo ----------------- **)

let i18nMainTitle1 () = 
  i18n ["Finite Automata"; "Autómatos Finitos"; "Automates Finis"]

let i18nMainTitle2 () = 
  i18n ["Regular Expressions"; "Expressões Regulares"; "Expressions Régulières"]

let i18nMainTitle3 () = 
  i18n ["Exercises"; "Exercícios"; "Des exercices"]

let i18nMainTitle4 () =
  i18n ["Context Free Grammars"; "Gramáticas Independentes de Contexto"; "Grammaires Sans Contexte"]

let i18nMainTitle5 () =
  i18n ["Grammars"; "Gramáticas Gerais"; "Grammaires Générales"]

let i18nMainTitleTM () = (* carolina *)
  i18n ["Turing Machine"; "Máquina de Turing"; "Machine de Turing"]

let i18nMainTitlePDA () = 
  i18n ["Pushdown Automata"; "Autómatos de Pilha"; "Automate à Pile"]

let i18nMainTitleFST () = 
  i18n ["Finite State Transducer"; "Transdutor de Estado Finito"; "Transducteur d'État Fini"]

let i18nMainTitleComp () = (* carolina *)
  i18n ["Composition"; "Composição"; "Composition"]

(** ---------------- Footer ----------------- **)

let i18nDeveloped () = 
  i18n ["Developed in "; "Desenvolvido em "; "Développé en"]

let i18nNovaLincs () = 
    i18n ["NOVA-LINCS"; "NOVA-LINCS"; "NOVA-LINCS"]

let i18nProject () = 
  i18n [" by the projects "; " pelos projectos "; "par les projets"]

let i18nFactor () = 
  i18n ["Factor"; "Factor"; "Factor"] 

let i18nAnd () = 
  i18n [" and "; " e "; " et "]
  
let i18nLeafs () = 
  i18n [" LEAFs"; " LEAFs"; " LEAFs"]

let i18nFinancing () = 
  i18n [" / Co-financed by "; " / Cofinanciado por "; "/ Cofinancé par"]

let i18nFooter () = 
  i18n ["Tezos Foundation"; "Fundação Tezos"; "Fondation Tezos"]
  
let i18nFooter1 () = 
  i18n ["INRIA Foundation"; "Fundação INRIA"; "INRIA Foundation"]

(** ------------------ Modal ------------------- **)

let i18nNewModelSelectType() =
  i18n ["Select model type:"; "Selecionar o tipo de modelo:"; "Sélectionnez le type de modèle"]

let i18nInstructionsAutomaton () =
  i18n ["Set the name of the initial state.";
        "Defina o nome do estado inicial.";
        "Définissez le nom de l'état initial."]

let i18nInstructionsCFG() =
  i18n ["The head of the rule of the first line is considered the initial variable of the grammar.
The empty symbol can be indicated with a blank space or with the character ~."; 
        "A cabeça da regra da primeira linha é considerada a variável inicial da gramática.
O símbolo vazio pode ser indicado com um espaço em branco ou com o caracter ~.";
        "L'en-tête de la règle de la première ligne est considérée comme la variable initiale de la grammaire.
Le symbole vide peut être indiqué par un espace blanc ou par le caractère ~."]


let i18nInstructionsGR() =
  i18n ["The head of the rule of the first line is considered the initial variable of the grammar.
The empty symbol can be indicated with a blank space or with the character ~."; 
        "A cabeça da regra da primeira linha é considerada a variável inicial da gramática.
O símbolo vazio pode ser indicado com um espaço em branco ou com o caracter ~.";
        "L'en-tête de la règle de la première ligne est considérée comme la variable initiale de la grammaire.
Le symbole vide peut être indiqué par un espace blanc ou par le caractère ~."]

(** ---------------- Automatos ----------------- **)

let i18nFormatting () = 
  i18n ["See model specification"; "Ver especificação do modelo"; "Voir les spécifications du modèle"]

let i18nClean () = 
  i18n ["Clean"; "Limpar"; "Nettoyer"]

let i18nKuroda () = 
  i18n ["Kuroda Normal Form"; "Forma normal de Kuroda"; "Forme normale de Kuroda"]

let i18nPenttonen () = 
  i18n ["Penttonen Normal Form"; "Forma normal de Penttonen"; "Forme normale de Penttonen"]

let i18monoCSG () = 
  i18n ["Monotonic to CSG"; "Monótona para GSC"; "Monotone vers GSC"]

let i18nDeterministic () = 
  i18n ["Make deterministic"; "Tornar determinista"; "Rendre déterministe"]

let i18nMinimize () = 
  i18n ["Minimize automaton"; "Minimizar autómato"; "Minimiser l'automate"]

let i18nProductive () = 
  i18n ["Productive states"; "Estados produtivos"; "États productifs"]

let i18nAccessible () = 
  i18n ["Accessible states"; "Estados acessiveis"; "États accessibles"]

let i18nUseful () = 
  i18n ["Useful states"; "Estados úteis"; "États utiles"]

let i18nTogleAcceptCriteriaState () = 
  i18n ["Acceptance Mode: Final states"; "Modo de Aceitação: Estados Finais"; "Mode d'acceptation : États finaux"]

let i18nTogleAcceptCriteriaEmptyStack () = 
  i18n ["Acceptance Mode: Empty Stack"; "Modo de Aceitação: Pilha Vazia"; "Mode d'acceptation : Pile Vide"]

let i18nInitialStackSymbol () = 
  i18n ["Initial stack symbol: "; "Símbolo inicial da pilha: "; "Symbole de pile initial: "]

(* let i18nInfoBox () = 
  i18n ["Data about the Automaton"; "Dados sobre o Autómato"] *) 

let i18nIsDeterministic () = 
  i18n ["The automaton is deterministic. "; "O autómato é determinista. "; "L'automate est déterministe. "]

let i18nNotDeterministic () = 
  i18n ["The Automaton is not deterministic. "; "O Autómato não é determinista. "; "L'automate n'est pas déterministe. "]

let i18nIsMinimal () = 
  i18n ["The Automaton is minimal. "; "O Autómato é mínimo. "; "L'automate est minimal. "]

let i18nNotMinimal () = 
  i18n ["The automaton is not minimal. "; "O autómato não é mínimo. "; "L'automate n'est pas minimal. "]

let i18nNotUseless () = 
  i18n ["The automaton has no useless states. "; "O autómato não tem estados inúteis. "; "L'automate n'a pas d'états inutiles. "]

let i18nisEquivalentFA () =
  i18n ["The automaton can be transformed to a equivalent FA. "; "O autómato pode ser transformado em um AF equivalente. "; "L'automate peut être transformé en un AF équivalent. "]

let i18nisNotEquivalentFA () =
  i18n ["The automaton can not be transformed to a equivalent FA. "; "O autómato não pode ser transformado em um AF equivalente. "; "L'automate ne peut être transformé en un AF équivalent. "]

let i18nHas () = 
  i18n ["The automaton has "; "O autómato tem "; "L'automate a "]

let i18nUselessStates () = 
  i18n [" useless states. "; " estados inúteis. "; " etats inutiles. "] 

let i18nNumberStates () = 
    i18n ["Number of states: "; "Número de Estados: "; "Nombre d'états: "] 

let i18nNumberTransitions () = 
  i18n ["Number of transitions: "; "Número de Transições: "; "Nombre de transitions: "]

(* ML *)
let i18nIsLinearBounded() = 
  i18n ["The automaton is linear bounded "; "O autómato é linearmente limitado "; "L'automate est linéairement borné "]

let i18nIsNotLinearBounded() = 
  i18n ["The automaton is not linear bounded "; "O autómato não é linearmente limitado "; "L'automate n'est pas borné linéairement "]

let i18nNoPathAvailablePDA () = 
  i18n [
    "(Word not accepted, there is no acceptance path available)"; 
    "(Palavra não aceite pelo Autómato, não existe nenhum caminho aceite pelo autómato)"; 
    "(Mot non accepté, il n'y a pas de chemin d'acceptation disponible)"
  ]

let i18nAlreadyInAcceptanceModeByStates () = 
  i18n ["Automaton already in acceptance mode by final states"; 
  "O Autómato já está em modo de aceitação por estados finais"; 
  "Automate déjà en mode acceptation par états finaux"]

let i18nAlreadyInAcceptanceModeByEmptyStack () = 
  i18n ["Automaton already in acceptance mode by empty stack"; 
  "O Autómato já está em modo de aceitação por pilha vazia"; 
  "Automate déjà en mode acceptation par pile vide"]

(** ---------------- Regular Expressions ----------------- **)

let i18nDirection () = 
  i18n ["Change tree direction"; "Mudar direção da árvore"; "Changer la direction de l'arbre"]

(** ---------------- Context Free Grammars ---------------- **)

let i18nCFGFirst () =
  i18n ["FIRST";
        "PRIMEIROS";
        "PREMIER"]

let i18nCFGFollow () =
  i18n ["FOLLOW";
        "SEGUINTES";
        "SUIVANT"]

let i18nRemoveLeftRecursion () =
  i18n ["Remove left recursion"; "Remover recursividade esquerda"; "Supprimer la récursivité gauche"]
  
let i18nRemoveUnproductive () =
  i18n ["Remove unproductive symbols"; "Remover símbolos não produtivos"; "Supprimer les symboles improductifs"]

let i18nRemoveInaccessible () =
  i18n ["Remove inaccessible symbols"; "Remover símbolos inacessíveis"; "Supprimer les symboles inaccessibles"]

let i18nLeftFactoring () =
  i18n ["Left factoring"; "Fatorizar à esquerda"; "Factorisation à gauche"]

let i18nRemoveEpsilonProductions () =
  i18n ["Remove ε productions"; "Remover produções ε"; "Supprimer ε productions"]

let i18nRemoveUnitProductions () =
  i18n ["Remove unit productions"; "Remover produções unitárias"; "Supprimer les productions unitaires"]

let i18nTransformToLL1 () =
  i18n ["Transform to a LL1 Grammar"; "Transformar numa gramática LL1"; "Transformer en une grammaire LL1"]

let i18nFirstAndFollow () =
  i18n ["First and follow set"; "Conjuntos dos primeiros e seguintes"; "Ensemble premier et suivi"]
  
let i18nParsingTable () =
  i18n ["Parsing table";
        "Tabela de análise sintática";
        "Table d'analyse"]
  
let i18nSetTables () =
  i18n ["First, Follow and Parsing tables";
        "Tabelas dos primeiros, seguintes e análise sintática";
        "Tout d'abord, suivre et analyser les tables"]  

let i18nRDParser () =
  i18n ["Recursive descendent parser";
        "Analisador descendente recursivo";
        "Analyseur descendant récursif"]  

let i18nToggle () =
  i18n ["Toggle mode";
        "Alternar modo";
        "Bascule entre les modes simple et traditionnel"]

let i18nRDParserCopyButtonDefault () =
  i18n ["Copy to clipboard";
        "Copiar";
        "Copie"]

let i18nRDParserCopyButtonClick () =
  i18n ["Copied";
        "Copiado";
        "Copié"]

let i18nCFGLL1Accept () =
  i18n ["Test word acceptance"; "Testar aceitação de uma palavra"; "Tester l'acceptation des mots"]

let i18nIsLL1 () =
  i18n ["The grammar is LL1. "; "A gramática é LL1. "; "La grammaire est LL1. "]

let i18nIsNotLL1 () =
  i18n ["The grammar is not LL1. "; "A gramática não é LL1. "; "La grammaire n'est pas LL1. "]
  
let i18nIsLeftRecursive () =
  i18n ["The grammar is left recursive. "; "A gramática é recursiva à esquerda. "; "La grammaire est laissée récursive. "]
  
let i18nIsNotLeftRecursive () =
  i18n ["The grammar is not left recursive. "; "A gramática não é recursiva à esquerda. "; "La grammaire n'est pas laissée récursive. "]
  
let i18nIsLeftFactoring () =
  i18n ["The grammar can be left factored. "; "A gramática pode ser fatorizada à esquerda. "; "La grammaire peut être factorisée. "]
  
let i18nIsNotLeftFactoring () =
  i18n ["The grammar is already left factored. "; "A gramática é fatorizada à esquerda. "; "La grammaire est déjà factorisée à gauche. "]

let i18nHasParsingTableConflict () =
  i18n ["There are conflicts on the parsing table. ";
        "Existem conflitos na tabela de análise sintática. ";
        "Il y a des conflits sur la table d'analyse. "]

let i18nHasNotParsingTableConflict () =
  i18n ["There are no conflicts on the parsing table. ";
        "Não existem conflitos na tabela de análise sintática. ";
        "Il n'y a pas de conflits sur la table d'analyse. "]

let i18nIsCFGClean () =
  i18n ["The grammar is clean. "; "A gramática é limpa. "; "La grammaire est propre. "]

  let i18nIsCFG () =
      i18n ["The grammar is context free. "; "A gramática é independente de contexto. "; "La grammaire est indépendante du contexte. "]

  let i18nIsCSG () =
    i18n ["The grammar is context sensitive. "; "A gramática é dependente de contexto. "; "La grammaire est dépendante du contexte. "]
  
   let i18nIsMO () =
    i18n ["The grammar is monotonic. "; "A gramática é monotónica. "; "La grammaire est monotonique. "]

    let i18nIsUG () =
      i18n ["The grammar is unrestricted. "; "A gramática é irrestrita. "; "La grammaire est non restreinte. "]

      let i18nIsLG () =
        i18n ["The grammar is linear. "; "A gramática é linear. "; "La grammaire est linéaire. "]

        let i18nIsRLG () =
            i18n ["The grammar is right linear. "; "A gramática é linear direita. "; "La grammaire est linéaire droite. "]
        
        let i18nIsLLG () =
            i18n ["The grammar is left linear. "; "A gramática é linear esquerda. "; "La grammaire est linéaire gauche. "]

let i18nIsNotCFGClean () =
  i18n ["The grammar is not clean. "; "A gramática não é limpa. "; "La grammaire n'est pas propre. "]
  
let i18nNotProd () =
  i18n ["The grammar has unproductive symbols. "; "A gramática tem símbolos não produtivos. "; "La grammaire a des symboles improductifs. "]
  
let i18nNotAccess () =
  i18n ["The grammar has inaccessible symbols. "; "A gramática tem símbolos não acessíveis. "; "La grammaire a des symboles inaccessibles. "]
    
let i18nNewGrammar () =
  i18n ["New grammar"; "Nova gramática"; "Nouvelle grammaire"]
  
let i18nAcceptInput () =
  i18n ["Input"; "Entrada"; "Entrée"]

let i18nAcceptRecognized () =
  i18n ["Recognized"; "Reconhecido"; "Reconnu"]
  
let i18nAcceptStack () =
  i18n ["Stack"; "Pilha"; "Empiler"]
  
let i18nAcceptProduction () =
  i18n ["Production"; "Produção"; "Production"]

(** ---------------- Exercises ----------------- **)

let i18nVerify () = 
  i18n ["Verify"; "Verificar"; "Vérifier"]

let i18nNonAccepted () = 
  i18n ["Words to reject: "; "Palavras para rejeitar: "; "Mots à accepter: "]

let i18nAcceptedWords () = 
  i18n ["Words to accept: "; "Palavras para aceitar: "; "Des mots qui devraient être acceptés: "]

let i18nProblem () = 
  i18n ["Problem: "; "Problema: "; "Problème: "] 

let i18nEnumTitle () = 
  i18n ["Exercise"; "Exercício"; "Exercice"] 

let i18nRight () = 
  i18n ["Correct answer"; "Resposta correcta"; "Bonne réponse"]

let i18nWrong () = 
  i18n ["Wrong answer"; "Resposta errada"; "Mauvaise réponse"]

(** ---------------- Palavras Geradas ----------------- **)

let i18nGenerateWords () = 
  i18n ["Generated Words:"; "Palavras Geradas:"; "Mots générés:"]

(** ---------------- Accept das Expressões Regulares ----------------- **)

let i18nWordAccepted () = 
  i18n ["The word is accepted"; "A palavra é aceite"; "Le mot est accepté"]

let i18nWordNotAccepted () = 
  i18n ["The word is not accepted"; "A palavra não é aceite"; "Le mot n'est pas accept"] 

let i18nExists () = 
  i18n ["There are "; "Existem "; "Il y a "] 

let i18nGoodDerivations () = 
  i18n [" successful derivations: "; " derivações bem sucedidas: "; " dérivations réussies: "]

let i18nBadDerivations () = 
  i18n [" failed derivations: "; " derivações falhadas: "; " dérivations qui ont échoué: "]

let i18nBy () = 
  i18n [" by "; " por "; " par "]

let i18nNext () = 
  i18n ["Next"; "Próxima"; "Suivante"]

let i18nPrevious () = 
  i18n ["Previous"; "Anterior"; "Précédente"]

(** ---------------- Alertas ----------------- **)

let i18nAlertMinimum () = 
  i18n ["The automaton is already minimal"; "O Autómato já é mínimo"; "L'automate est déjà minimal"]

let i18nAlertNeedsDeterministic () = 
  i18n ["The Automaton must be deterministic in order to be minimized"; 
  "O Autómato tem de ser determinista para poder ser minimizado";
  "L'automate doit être déterministe pour être minimisé"]

let i18nAlertDelete () = 
  i18n ["It is not possible to delete the initial state, change the initial state to another one and then delete the desired one!"; 
  "Não é possível eliminar estado inicial, troque o estado inicial para outro e depois elimine o desejado!";
  "Il n'est pas possible d'éliminer l'état initial, changer l'état initial en un autre et puis supprimez l'état souhaité"] 

let i18nAlertUnexistentState () = 
  i18n ["The indicated state does not exist!"; "O estado indicado não existe!"; "L'état indiqué n'existe pas!"]

(* let i18nAlertInitialFirst () = 
  i18n ["It is necessary to create an initial state first"; 
  "É necessário criar primeiro um estado inicial"] *)

let i18nAlertTheTransition () = 
  i18n ["The transition "; "A transição "; "La transition "]

let i18nAlertAlreadyExists () = 
  i18n [" already exists!"; " já existe!"; " existe déjà!"] 

let i18nAlertDoNotExists () = 
  i18n [" doesn't exists!"; " não existe!"; " n'existe pas!"] 

let i18nAlertStartState () = 
  i18n ["The starting state does not exist!"; "O estado de partida não existe!"; "L'état de départ n'existe pas!"]

let i18nAlertArrivalState () = 
  i18n ["The state of arrival does not exist!"; "O estado de chegada não existe!"; "L'état d'arrivée n'existe pas!"]

let i18nAlertAlreadyFinal () = 
  i18n ["The state is already final"; "O estado já é final"; "L'état est déjà définitif"]

(* TM *)
let i18nAlertAlreadyInitial () = 
  i18n ["The state is already initial"; "O estado já é inicial"; "Le statut est déjà initial"]

let i18nAlertNonFinal () = 
    i18n ["The state is not final"; "O estado não é final"; "L'état n'est pas définitif"]

let i18nAlertWorkingWithAutomata () = 
  i18n ["You can't add state because you are not working with an automaton"; 
  "Não pode adicionar estado porque não está a trabalhar com um autómato";
  "Vous ne pouvez pas ajouter d'état parce que vous ne travaillez pas avec un automate"]

let i18nAlertDeterministic () = 
  i18n ["The automaton is already deterministic"; 
  "O Autómato já é determinista";
  "L'automate est déjà déterministe"]

let i18nAlertClean () = 
  i18n ["The automaton has no states to clean, there are no useless states!"; 
  "O Autómato não tem estados para limpar, não existem estados inúteis!";
  "L'automate n'a pas d'états à nettoyer, il n'y a pas d'états inutiles!"]

let i18nAlertRegex () = 
  i18n ["You are already working with a regular expression"; 
  "Já está a trabalhar com uma expressão regular";
  "Vous travaillez déjà avec une expression régulière"]

let i18nAlertAutomaton () = 
  i18n ["You are already working with a finite automaton"; 
  "Já está a trabalhar com um autómato finito";
  "Vous travaillez déjà avec un automate fini"]

let i18nAlertExists () = 
  i18n ["The state already exists"; "O estado já existe"; "L'état existe déjà"]

(* ML *)
let i18nLeaveSimulationToEdit () = 
  i18n [
        "To edit the model, you need to exit stop the simulation. Are you sure you want to continue?"; 
        "Para editar o modelo, precisas de sair da simulação. Tens a certeze de que queres continuar"; 
        "Pour modifier le modèle, vous devez quitter et arrêter la simulation. Es-tu sur de vouloir continuer?"
      ]

(* let i18nAlertAddStateTransitions () = 
  i18n ["Add states and transitions in the text box"; 
  "Adicione os estados e transições na caixa de texto"] *)

(*  let defineAlertFormat lang =
    match lang with 
    "" -> ""
      | "en" -> "The format of a transition is: State Transition State"
      | "pt" -> "O formato de uma transição é: Estado Transição Estado" *)

 (* let defineAlertName lang =
    match lang with 
    "" -> ""
      | "en" -> "You must name the state in the text box"
      | "pt" -> "Tem de dar um nome ao estado na caixa de texto" *)

let i18nAlertNoTransitions () = 
  i18n ["There is no transition with the symbol given!"; 
  "Não há transições com o símbolo indicado!";
  "Il n'y a pas de transition avec le symbole donné!"]

let i18nAlertNoMoreStates () = 
  i18n ["The word is over. There are no more states to follow."; 
  "A palavra terminou. Não existem mais estados seguintes.";
  "Nous sommes en fin de mot. Il n'y a plus d'état à suivre."]

let i18nAlertEndTMSim () = (* ML *)
  i18n [
    "The simulation is over. There are either no more transitions or the machine is in an accept state.";
    "A simulação acabou. Não há mais transições ou a máquina está num estado de aceitação";
    "La simulation est terminée. Il n'y a plus de transitions ou la machine est dans un état d'acceptation"
  ]
  
let i18nAlertArrivedInitial () = 
  i18n ["You are in the initial state. It is not possible to go backwards from this point"; 
  "Não é possível andar para trás do estado inicial";
  "Vous êtes dans l'état initial. Il n'est pas possible de revenir en arrière à partir de ce point"]

let i18nAlertNotLeftRecursive () =
  i18n ["Grammar is not left recursive";
  "A gramática não é recursiva à esquerda";
  "La grammaire n'est pas récursive"]

let i18nAlertIsLL1 () = i18nIsLL1()

let i18nAlertNotLeftFactoring () =
  i18n ["Grammar does not need to be left factored";
  "A gramática não precisa de ser fatorizada à esquerda";
  "La grammaire n'a pas besoin d'être factorisée"]

let i18nAlertIsClean () =
  i18n ["Grammar is already clean";
  "A gramática já é limpa";
  "La grammaire est déjà propre"]

let i18nAlertIsNotMonotonic () =
  i18n ["Grammar is not monotonic";
  "A gramática já não é monotónica";
  "La grammaire n'a pas monotonique"]
  

let i18nAlertIsContextSensitive () =
  i18n ["Grammar is not context sensitive";
  "A gramática não é dependente de contexto";
  "La grammaire n'est pas sensible au contexte"]

let i18nAlertNoEmptyProductions () =
  i18n ["Grammar does not have any epsilon productions";
  "A gramática não tem produções epsilon";
  "La grammaire n'a pas de productions epsilon"]
  
let i18nAlertNoUnitProductions () =
  i18n ["Grammar does not have any unit productions";
  "A gramática não tem produções unitárias";
  "La grammaire n'a pas de productions unitaires"]

let i18nAlertCFGAcceptNoInput () =
  i18n ["Word not accepted because there are no more input symbols to analyze and the remaining stack symbols do not derive an empty word.";
  "A palavra não é aceite porque não existem mais símbolos de entrada a analisar e os símbolos que sobram na pilha não derivam a palavra vazia.";
  "Mot non accepté car il n'y a plus de symboles d'entrée à analyser et les symboles de pile restants ne dérivent pas un mot vide."]
  
let i18nAlertCFGAcceptNoStack () =
  i18n ["Word not accepted because there are no more symbols on the stack but there are still symbols on the input to analyze.";
  "A palavra não é aceite porque não existem mais símbolos na pilha mas existem ainda símbolos de entrada por analisar.";
  "Mot non accepté car il n'y a plus de symboles sur la pile mais il y a encore des symboles sur l'entrée à analyser."]
  
let i18nAlertCFGAcceptNoProduction () =
  i18n ["Word not accepted because there is no production found with the given variable and terminal symbols.";
  "A palavra não é aceite porque não existe produção com os símbolos variável e terminal fornecidos.";
  "Mot non accepté car aucune production n'a été trouvée avec la variable et les symboles terminaux donnés."]

let i18nAlertRETooBig () =
  i18n ["RE tree too big to display";
  "Árvore ER muito grande para mostrar";
  "Arborescence ER trop grande pour être affichée"]

let i18nAlertDirectionWrong() =
  i18n ["The direction inserted is not correct, it must be either L or R";
    "A direção inserida não está correta, deve ser L ou R";
    "La direction insérée n'est pas correcte, elle doit être L ou R"]

let i18nAlertExceededCharacters() =
  i18n [
    "Expecting only 5 characters";
    "São esperados apenas 5 caractéres";
    "Attendre seulement 5 caractères"
  ]

let i18nAlertTransitionExists() =
  i18n [
    "This transition already exists";
    "Esta transição ja existe";
    "Cette transition existe déjà"
  ]


(** ---------------- Feedback ----------------- **)

let i18nFeedbackText () = 
  i18n ["In order to create an increasingly better application, we appreciate any kind of feedback."; 
  "De forma a criarmos uma aplicacão cada vez melhor, agradecemos qualquer tipo de comentários.";
  "Afin d'améliorer constamment la application, nous apprécierons grandement vos commentaires."]

let i18nFeedbackText2 () = 
  i18n ["If you have any questions or want to give an opinion, please send an email to "; 
  "Se tiver alguma dúvida ou queira dar opinião, envie, por favor, um email para ";
  "Si vous avez des questions ou souhaitez donner votre opinion, veuillez nous envoyer un e-mail à "]

let i18nFeedbackThankYou () = 
  i18n ["Thank You!"; "Obrigado!"; "Merci!"]

(** ---------------- Sobre ----------------- **)

let i18nAboutTitle () = 
  i18n ["About"; "Sobre"; "À propos de"]

let i18nAboutSubtitle () = 
  i18n ["About OFLAT"; "Sobre OFLAT"; "À propos de OFLAT"]

let i18nAboutSubtitle2 () = 
  i18n ["Instructions"; "Instruções"; "Instructions"]

let i18nAboutText1 () = 
  i18n ["This tool is being developed at "; "Esta ferramenta está a ser desenvolvida na "; "Cet outil est en cours de développement au "]

let i18nAboutText2 () = 
  i18n [" (at the Computer Science Department of FCT-UNL) by the projects "; 
  " (no Departamento de Informática da FCT- UNL) pelo projecto ";
  " (au Département d'informatique du FCT-UNL) par les projets "]

let i18nAboutText16 () = 
  i18n [" and "; " e "; " et "]

let i18nAboutText17 () = 
  i18n [" LEAFs"; " LEAFs"; " LEAFs"]

let i18nAboutText3 () = 
  i18n [" and co-financed by "; " e co-financiado pela "; " et cofinancé par "]

let i18nAboutText4 () = 
  i18n ["It is being developed in OCaml with the help of the libraries js_of_ocaml em Cytoscape.js, the source code being available on "; 
  "Está a ser desenvolvido em OCaml com a ajuda das bibliotecas js_of_ocaml e Cytoscape.js, estando o código fonte disponível no ";
  "Il est développé en OCaml à l'aide des bibliothèques js_of_ocaml et Cytoscape.js, le code source étant disponible à "]

let i18nAboutText5 () =  
  i18n ["At this moment it is possible to work with finite automata, regular expressions and perform exercises."; 
  "Neste momento é possível trabalhar com autómatos finitos, expressões regulares e realizar exercícios.";
  "En ce moment, il est possible de travailler avec des automates finis, des expressions régulières et de résoudre des exercices."]

let i18nAboutText6 () = 
  i18n ["To import a file it must be in txt or JSON. The format of an automaton is:"; 
  "Para se importar um ficheiro este deve estar em txt ou JSON. O formato de um autómato é:";
  "Pour importer un fichier il doit être au format txt ou JSON. Le format d'un automate est:"]

let i18nAboutText7 () = 
  i18n ["
  {
    kind: finite automaton,
    description: description of the automaton,
    name: name of the automaton,
    alphabet: [list of elements],
    states: [list of states],
    initialState: an initial state,
    transitions: [List of transitions in the format [departure state, alphabet element, arrival state]],
    acceptStates: [list of states]
  } " ; 
  "
              {
                kind: finite automaton,
                description: descrição do autómato,
                name: nome do autómato,
                alphabet: [lista de elementos],
                states: [lista de estados],
                initialState: um estado inicial,
                transitions: [Lista de transições com formato [estado de partida, elemento do alfabeto, estado de chegada]],
                acceptStates: [lista de estados]
              } ";
              "
              {
                kind: finite automaton,
                description: description de l'automate,
                name: nom de l'automate,
                alphabet: [liste des éléments],
                states: [liste des états],
                initialState: un état initial,
                transitions: [Liste des transitions au format [état de départ, élément de l'alphabet, état d'arrivée]],
                acceptStates: [liste des états]
              } "]

let i18nAboutText8 () = 
  i18n ["The format of a regular expression is:"; 
  "O formato de uma expressão regular é:";
  "Le format d'une expression régulière est:"]

let i18nAboutText9 () = 
  i18n ["
  {
    kind: regular expression,
    description: description of the regular expression,
    name: name of the regular expression,
    re: regular expression
  } "   ; 
  "
            {
              kind: regular expression,
              description: descrição da expressão regular,
              name: nome da expressão regular,
              re: expressão regular
            } ";
            "
  {
    kind: regular expression,
    description: description de l'expression régulière,
    name: nom de l'expression régulière,
    re: expression régulière
  } " ]

let i18nAboutText10 () = 
  i18n ["The format of an exercise is:"; 
  "O formato de um exercício é:";
  "Le format d'un exercice est:"]

let i18nAboutText11 () = 
  i18n ["
  {
    kind: enumeration,
    description: description of the automaton,
    name: exercise name,
    problem: exercise presented to the student,
    inside: [list of words to be accepted],
    outside: [list of words that should not be accepted]
  } "; 
  "
            {
              kind: enumeration,
              description: descrição do autómato,
              name: nome do exercício,
              problem: exercício apresentado ao aluno,
              inside: [lista de palavras que devem ser aceites],
              outside: [lista de palavras que não devem ser aceites]
            } ";
            "
  {
    kind: enumeration,
    description: description de l'automate,
    name: nom de l'exercice,
    problem: exercice présenté à l'élève,
    inside: [liste de mots à accepter],
    outside: [liste de mots qui ne devraient pas être acceptés]
  } "]

let i18nAboutText12 () = 
  i18n ["All elements on the right side of the colon must be enclosed in quotation marks."; 
  "Todos os elementos do lado direito dos dois pontos devem ser colocados entre aspas.";
  "Tous les éléments du côté droit des deux points doivent être placés entre guillemets."]

let i18nAboutText13 () = 
  i18n ["It is also possible to load predefined examples from the server."; 
  "É possivel também carregar exemplos predefinidos do servidor.";
  "Il est également possible de charger des exemples prédéfinis à partir du serveur."]

let i18nAboutText14 () = 
  i18n ["The regular expression can also be created by typing it in the text box and the automata can be created step by step using the State and Transition select boxes. To create or edit a state it is necessary to indicate its name in the text box and to create or edit a transition put in the text box start status, alphabet element, arrival status (example: A a B). "; 
  "A expressão regular pode também ser criada escrevendo a mesma na caixa de texto e os autómatos pode ser criados passo a passo usando as caixas de seleção Estado e Transição. Para se criar ou editar um estado é necessário indicar o nome do mesmo na caixa de texto e para se criar ou editar uma transição coloca-se na caixa de texto estado de partida, elemento do alfabeto, estado de chegada (exemplo: A a B).";
  "L'expression régulière peut également être créée en tapant dans la zone de texte et les automates peuvent être créés étape par étape à l'aide des boîtes de sélection État et Transition. Pour créer ou modifier un état, il est nécessaire d'indiquer son nom dans la zone de texte et pour créer ou modifier une transition mise dans la zone de texte état de départ, élément alphabet, état d'arrivée (exemple: A a B). "]

let i18nAboutText15 () = 
  i18n ["In the automata the epsilon transition is represented by ~."; 
  "Nos autómatos a transição épsilon é representada por ~.";
  "Dans les automates, la transition epsilon est représentée par ~."]

(** ---------------- ToolTips ----------------- **)
let i18nTooltipSave () =
  i18n ["Save model in repository";
  "Guardar modelo em repositório";
  "Enregistrer le modèle dans le référentiel";] (*Carolina*)

let i18nTooltipNewModel () =
  i18n ["Allows the creation of a new model."; "Permite a criação de um novo modelo."; "Permet la création d'un nouveau modèle."]
  
let i18nTooltipEditModel () =
  i18n ["Edit the visible model."; "Editar o modelo visível."; "Modifiez le modèle visible."]

let i18nTooltipFitGraph () = 
  i18n ["Fit visible graphics to the view box"; 
  "Ajustar gráficos vísiveis à caixa de visualização";
  "Ajuster les graphiques visibles à la zone d'aperçu"]

let i18nTooltipGenerate () = 
  i18n ["Gives a list of words, of size smaller or equal a given size, accepted by the automaton"; 
  "Dá uma lista de palavras aceites, até um determinado tamanho dado pelo autómato";
  "Donne une liste de mots, de taille inférieure ou égale à une taille donnée, acceptés par l'automate"]

let i18nTooltipTest () = 
  i18n ["Shows, with an animation, how the automaton accepts or rejects a given word; or shows, with a tree, the acceptance process of a regular expression; the input is asked through a box on the screen"; 
  "Mostra-se, com uma animação, como o autómato aceita ou rejeita uma dada palavra; ou mostra, com uma árvore, o processo de aceitação de uma expressão regular; a palavra é solicitada através de uma caixa no ecrã";
  "Montre, avec une animation, comment l'automate accepte ou rejette un mot donné; ou montre, avec un arbre, le processus d'acceptation d'une expression régulière; la saisie est demandée par une boîte à l'écran"]

let i18nTooltipStep () = 
  i18n ["Allows the user to see the acceptance process step-by-step; the input is asked through a box on the screen"; 
  "Permite que o utilizador veja o processo de aceitação passo a passo; a palavra é solicitada através de uma caixa no ecrã";
  "Permet à l'utilisateur de voir le processus d'acceptation pas à pas; la saisie est demandée par une boîte à l'écran"]

let i18nTooltipClear () = 
  i18n ["Removes all colors of the automaton"; 
  "Limpa as cores do autómato";
  "Supprime toutes les couleurs de l'automate"]

let i18nTooltipConvert () = 
  i18n ["In a new box converts the automaton in a regular expression or a regular expression in an automaton"; 
  "Numa nova caixa converte um autómato em expressão regular e uma expressão regular num autómato";
  "Dans une nouvelle boîte, il convertit l'automate en une expression régulière et une expression régulière en un automate"]

let i18nTooltipFile () = 
  i18n ["Allows the user to import a file from filesystem. Allowed formats txt or JSON"; 
  "Permite ao utilizador importar um ficheiro do sistema de ficheiros. Formatos permitidos txt ou JSON";
  "Permet à l'utilisateur d'importer un fichier à partir du système de fichiers. Formats autorisés txt ou JSON"]

let i18nTooltipExportModel () = 
  i18n ["Download the textual representation of the currently visible model."; 
  "Transfere a representação textual do modelo atualmente visivel.";
  "Téléchargez la représentation textuelle du modèle actuellement visible."]


let i18nTooltipAbout () = 
  i18n ["More about the page and some instructions"; 
  "Mais sobre a página e algumas instruções";
  "Pour en savoir plus sur la page et quelques instructions"]

let i18nTooltipFeedback () = 
  i18n ["Information page if you want to give feedback"; 
  "Página sobre como dar comentários";
  "Page d'informations si vous souhaitez donner votre avis"]

let i18nTooltipLang () = 
  i18n ["The user can choose the language of the page"; 
  "Permite ao utilizador escolher o idioma da página";
  "L'utilisateur peut choisir la langue de la page"]

let i18nTooltipCloseLeft () = 
  i18n ["If there is a second figure on the screen, closes the left and keeps the right one; otherwise cleans the screen"; 
  "Se existir uma segunda figura no ecrã, fecha a da esquerda e mantém a da direita; senão limpa tudo";
  "S'il y a une deuxième figure sur l'écran, fermez celle de gauche et gardez cellede droite; sinon nettoie l'écran"]

let i18nTooltipCloseRight () = 
  i18n ["Closes the right box on the screen"; 
  "Fecha a caixa direita";
  "Ferme la boîte de droite sur l'écran"]

let i18nTooltipDirection () = 
  i18n ["If the tree is horizontal becomes vertical and vice-versa"; 
  "Se a árvore é horizontal torna-se vertical e vice-versa";
  "Si l'arbre est horizontal devient vertical et vice-versa"]

let i18nTooltipVerify () = 
  i18n ["Verifies if the answer is correct or not, and if it is not, says where it fails"; 
  "Verifica se a resposta está correcta ou errada; se estiver errada diz onde falha";
  "Vérifie si la réponse est correcte ou fausse, si c'est faux, il dit où il échoue"]

let i18nTooltipClean () = 
  i18n ["Creates a new automaton removing all useless states"; 
  "Cria um novo autómato removendo os estados inúteis";
  "Crée un nouvel automate supprimant tous les états inutiles"]

let i18nTooltipDeterministic () = 
  i18n ["Creates on a new box a deterministic version of the automaton"; 
  "Cria numa nova caixa uma versão determinística do autómato";
  "Crée sur une nouvelle boîte une version déterministe de l'automate"]

let i18nTooltipMinimize () = 
  i18n ["Creates on a new box a minimized version of the automaton"; 
  "Cria numa nova caixa uma versão minimizada do autómato";
  "Crée sur une nouvelle boîte une version minimisée de l'automate"]

let i18nTooltipProductive () = 
  i18n ["Paints all the productive states of the automaton"; 
  "Pinta todos os estados produtivos do autómato";
  "Peint tous les états productifs de l'automate"]

let i18nTooltipAccessible () = 
  i18n ["Paints all the accessible states of the automaton"; 
  "Pinta todos os estados acessíveis do autómato";
  "Peint tous les états accessibles de l'automate"]

let i18nTooltipUseful () = 
  i18n ["Paints all the useful states of the automaton"; 
  "Pinta todos os estados úteis do autómato";
  "Peint tous les états utiles de l'automate"]

let i18nTooltipSpecification () = 
  i18n ["Show the specification of the model in a new box"; 
  "Mostra a especificação do modelo numa nova caixa";
  "Afficher la spécification du modèle dans une nouvelle case"]

let i18nPropertyTrue () =
  i18n ["Does not require any property.";
  "Não requer nenhuma propriedade";
  "Ne nécessite aucune propriété."]

let i18nPropertyFiniteAutomaton () =
  i18n ["The solution must be a finite automaton";
  "A solução deve ser um autómato finito";
  "La solution doit être un automate fini"]

let i18PropertyFiniteStateTransducer () =
  i18n ["The solution must be a finite state transducer";
  "A solução deve ser um transdutor de estados finitos";
  "La solution doit être un transducteur à états finis"]

let i18nPropertyRegularExpression () =
  i18n ["The solution must be a regular expression";
  "A solução deve ser uma expressão regular";
  "La solution doit être une expression régulière"]

let i18nPropertyDeterministic () =
  i18n ["The automaton must be deterministic";
  "O autómato deve ser determinista";
  "L'automate doit être déterministe"]

let i18nPropertyMinimized () =
    i18n ["The automaton must be minimal";
    "O autómato deve ser mínimo";
    "L'automate doit être minimal"]

    let i18nProperties () =
      i18n ["Properties to satisfy:";
      "Propriedades para satisfazer:";
      "Propriétés à satisfaire:"]


let i18nPromptTextTestWord () =
  i18n ["Please enter the word to test";
  "Por favor indique a palavra para testar";
  "Veuillez saisir le mot à tester"]

 let i18nTextRegex () =
  i18n ["Please enter the regular expression";
  "Por favor indique a expressão regular";
  "Veuillez saisir l'expression régulière"]

  let i18nWhichTransition () =
    i18n ["Which of the symbols do you want to delete?";
    "Qual dos simbolos quer apagar?";
    "Lequel des symboles souhaitez-vous supprimer?"]

  let i18nRenameStateQuestion () =
    i18n ["What is the new name of the state?";
    "Qual o novo nome do estado?";
    "Quel est le nouveau nom de l'état?"]
    
  let i18nRenameTransition () =
    i18n ["Which transition to rename?";
    "Qual a transição a renomear?";
    "Quelle transition pour renommer?"]

  let i18nTextMaximumSize () =
    i18n ["Please enter the maximum size for the words";
    "Por favor indique o tamanho máximo para as palavras";
    "Veuillez saisir la taille maximale des mots"]
  
  let i18nTextRemove () = 
      i18n["Remove"; "Remover";"Supprimer"]

  let i18nTextAdd () =
    i18n ["Add state";"Adicionar estado"; "Ajouter un état"]
      
  let i18nTextAddInitial () =
    i18n ["Add initial state"; "Adicionar estado inicial"; "Ajouter un état initial"]
      
  let i18nTextAddFinal () = 
    i18n ["Add final state"; "Adicionar estado final"; "Ajouter l'état final"]
  
  let i18nTextRenameState () =
    i18n ["Rename state"; "Renomear estado"; "Renommer l'état"]
      
  let i18nTextAddTransition () =
    i18n ["Add transition"; "Adicionar transição"; "Ajouter une transition"]
      
  let i18nTextTurnFinal () =
    i18n ["Make final"; "Tornar final"; "Rendre définitif"]
      
  let i18nTextRemoveFinal () =
     i18n ["Remove final"; "Remover final"; "Supprimer la finale"]
      
  let i18nTextTurnInitial () = 
     i18n ["Make initial"; "Tornar inicial"; "Faire initiale"]
      
  let i18nTextEnterState () =
    i18n ["Please enter the state name"; 
    "Por favor indique o nome do estado";
    "Veuillez saisir le nom de l'état"]
      
  let i18nTextEnterStartState () =
    i18n ["Please enter the start state";
    "Por favor indique o nome do estado de partida";
    "Veuillez saisir l'état de départ"]
      
  let i18nTextEnterTransition () =
    i18n ["Please enter the transition symbol";
    "Por favor indique o simbolo da transição";
    "Veuillez saisir le symbole de transition"]

  let i18nTextEnterTransitionTM () =
      i18n ["Please enter the the information in the following format: read symbol/write symbol/direction";
      "Por favor indique a informação no seguinte formato: simbolo lido/simbolo esrito/direção";
      "Veuillez saisir les informations au format suivant : symbole de lecture/symbole d'écriture/direction"]

  let i18nTextEnterTransitionPda () =
    i18n ["Please enter the transition";
    "Por favor indique a transição";
    "Veuillez saisir le transition"]

  let i18nChangeInitialStackSymbolPda () =
    i18n ["Please enter the initial stack symbol";
    "Por favor indique o símbolo inicial da pilha";
    "Veuillez indiquer le symbole initial de la pile"]
      
  let i18nTextEnterEndState () =
    i18n ["Please enter the end state";
    "Por favor indique o estado de chegada";
    "Veuillez saisir l'état final"]
  
let i18nTooltipCFGClean () =
  i18n ["Removes unproductive and inaccessible symbols from the grammar";
        "Remover símbolos não produtivos e inacessíveis da gramática";
        "Supprime les symboles improductifs et inaccessibles de la grammaire"]

let i18nTooltipGRKuroda () =
  i18n ["Convert grammar to Kuroda normal form";
        "Coverter para a forma normal de Kuroda";
        "Conversion en forme normale de Kuroda"]

let i18nTooltipGRPenttonen () =
  i18n ["Convert grammar to Penttonen normal form";
        "Converter para a forma normal de Penttonen";
        "Conversion en forme normale de Penttonen"]

let i18nTooltipnonContractingToCSG () =
  i18n ["Convert monotonic grammar to context-sensitive grammar";
        "Converter gramática monótona em gramática sensível ao contexto";
        "Convertir la grammaire monotone en grammaire sensible au contexte"]

let i18nTooltipRemoveLeftRecursion () =
  i18n ["Removes left recursion productions from the grammar";
        "Remover produções recursivas à esquerda da gramática";
        "Supprime les productions de récursivité gauche de la grammaire"]

let i18nTooltipLeftFactoring () =
  i18n ["Left factorize grammar productions";
        "Fazer factorização esquerda das produções da gramática";
        "Factoriser à gauche les productions de grammaire"]

let i18nTooltipRemoveEpsilonProductions () =
  i18n ["Removes all ε productions from the grammar";
        "Remove todas as produções ε da gramática";
        "Supprime toutes les productions ε de la grammaire"]

let i18nTooltipRemoveUnitProductions () =
  i18n ["Removes all unit productions from the grammar";
        "Remove todas as produções unitárias da gramática";
        "Supprime toutes les productions unitaires de la grammaire"]

let i18nTooltipTransformLL1 () =
  i18n ["Attempts to transform the current grammar into an equivalent LL1 grammar for the same language as the original grammar";
        "Tenta fazer a transformação da gramática atual para uma gramática equivalente LL1 para a mesma linguagem da gramática original";
        "Tente de transformer la grammaire actuelle en une grammaire LL1 équivalente pour la même langue que la grammaire d'origine"]

let i18nTooltipFirstAndFollow () =
  i18n ["Finds the all first and follow sets";
        "Encontra todos os conjuntos dos primeiros e seguintes";
        "Trouve tous les premiers et suivants ensembles"]

let i18nTooltipLookahead () =
  i18n ["Finds the lookahead set from a word";
        "Encontra o conjunto dos diretores a partir de uma palavra";
        "Recherche l'ensemble d'anticipation à partir d'un mot"]

let i18nTooltipCFGLL1Accept () =
  i18n ["Verifies if word is accepted by the language generated by a LL1 grammar using a parsing table";
        "Verifica se a palavra é aceite pela linguagem gerada pela gramática LL1 recorrendo ao uso da tabela de análise sintática";
        "Vérifie si le mot est accepté par le langage généré par une grammaire LL1 à l'aide d'une table d'analyse"]
        
let i18nTooltipSetTables () =
  i18n ["Show tables for first, follows and parsing table";
        "Mostra as tabelas dos primeiros, seguintes e de análise sintática";
        "Afficher les tables pour la première, les suivantes et la table d'analyse"]
        
let i18nTooltipRDParser () =
  i18n ["Generates code for a recursive descendent parser in the chosen programming language";
        "Gera código para um analisador descendente recursivo na linguagem de programação escolhida";
        "Génère du code pour un analyseur descendant récursif dans le langage de programmation choisi"]

let i18nTooltipToggleMode () =
  i18n ["Toggles between simple and traditional modes";
        "Alterna entre os modos simples e tradicional";
        "Bascule entre les modes simple et traditionnel"]

(** ----------------- Outros ------------------ **)
let i18nSave () =
  i18n ["Save"; "Guardar"; "sauvegarder"] (*Carolina*)
let i18nSaveText () =
  i18n ["Enter the name of the model"; "Enter the name of the model"; "Entrez le nom du modèle"]

let i18nConfirm () =
  i18n ["Confirm"; "Confirmar"; "Confirmer"]
  
let i18nCancel () =
  i18n ["Cancel"; "Cancelar"; "Annuler"]

let i18nErrorParsing () =
  i18n ["Failed to parse string"; "Falha ao analisar a string"; "Impossible d'analyser la chaîne"]

let i18nModelEditFA () =
  i18n ["Edit the model directly on the graph. Try right-clicking or long-pressing an element of the graph.";
        "Edita o modelo diretamente no grafo. Experimenta clicar com o lado direito do rato ou manter premido num elemento do grafo.";
        "Modifiez le modèle directement sur le graphique. Essayez de cliquer avec le bouton droit de la souris ou d'appuyer longuement sur un élément du graphique."]

(** ---------------- Antigos (fora de uso) -----------------

    let defineInputTitle lang =
      match lang with 
      "" -> ""
        | "en" -> "Input"
        | "pt" -> "Input"

    let defineSelectState lang =
      match lang with 
      "" -> ""
        | "en" -> "State"
        | "pt" -> "Estado"

    let defineSelectAdd lang =
      match lang with 
      "" -> ""
        | "en" -> "Add"
        | "pt" -> "Adicionar"

    let defineSelectInitial lang =
      match lang with 
      "" -> ""
        | "en" -> "Initial"
        | "pt" -> "Inicial"
    
    let defineSelectFinal lang =
      match lang with 
      "" -> ""
        | "en" -> "Final"
        | "pt" -> "Final"

    let defineSelectErase lang =
      match lang with 
      "" -> ""
        | "en" -> "Erase"
        | "pt" -> "Apagar"

    let defineSelectTransition lang =
      match lang with 
      "" -> ""
        | "en" -> "Transition"
        | "pt" -> "Transição"

    **)

    let i18nErrorConversion () =
      i18n ["Conversion not possible"; "Conversão não possível"; "Conversion impossible"]

  end
