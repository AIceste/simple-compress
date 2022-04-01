(* --------------------------------------------------------------------------*)
(* ----------------------- TP2 - IFT-3000 - Hiver 2020 ----------------------*)
(* --------------------------------------------------------------------------*)
(* Fichier permettant de tester les fonctions implantées du TP               *)
(* --------------------------------------------------------------------------*)
(* On entre dans l'interpréteur ce qui suit:                                 *)
(*                                                                           *)
(* # #use "testeur.ml";;                                                     *)
(*                                                                           *)
(* Par la suite:                                                             *)
(*                                                                           *)
(* # corrige();;  (* Teste toutes les fonctions de tests *)                  *)
(* # testn();;    (* n = 1 ou 2 ...; lance le test numéro n *)               *)
(*                                                                           *)
(* Lorsque le fichier tp1.ml est modifié, vous n'avez juste qu'à:            *)
(* - recharger le fichier testeur.ml.                                        *)
(* Par la suite, vous pouvez de nouveau effectuer les tests                  *)
(* --------------------------------------------------------------------------*)

(*  On charge les fichiers du Tp2 et on vérifie sa conformité par rapport aux
    signatures  *)
#use "zip.ml";;
module TestConformite1 = (Huffman : HUFFMAN);; 
module TestConformite2 = (Zip : ZIP);; 

open Utiles;;
open TestConformite1;;
open TestConformite2;;


(* Définitions de différentes valeurs globales                               *) 
(*****************************************************************************) 
let l_A = [('A',1)]
and l_AB = [('A',1); ('B',2)]
and l_BA = [('A',2); ('B',1)]
and l_ABC = [('A',1); ('B',2); ('C',4)]
and l_ABC' = [('A',2); ('B',3); ('C',4)]
and l_big = [('A',30); ('F',2); ('B',20); ('D',5); (' ',1); ('C',6); ('E',4)]
and l_big' = [(' ',1); ('F',2); ('E',4); ('D',8); ('C',20); ('B',40); ('A',80)]
;;

let h = new huffman ();;


(*****************************************************************************) 
(* Fonctions de tests                                                        *) 
(*****************************************************************************) 


(* Partie Huffman                                                            *) 
(*****************************************************************************) 

(* -------------------------------- ---------------------------------------- *)
(* Teste indirectement «create» en conjonction avec  initializer             *)
let test0() =
  let comment_l = ref [] in
  let ok = ref true in

  try
    if (new huffman())#toList <>
      ['a'; '\195'; 'c'; 'r'; '\168'; 'A'; 'U'; '\185'; '\135'; '\177'; '+'; 
       '<';'#'; '\175'; 'Z'; '\137'; '4'; '\''; 'R'; '\174'; 'V'; 'W'; '\170'; 
       '\171';'q'; '.'; 'd'; 't'; 'n'; 'e'; 's'; 'i'; 'f'; '-'; '\194'; 'x'; 'g'; 
       '?'; '@'; 'Y'; ';'; '\157'; '>'; '\130'; 'J'; 'I'; '('; ')'; 'C'; '\169'; 
       'P'; '"'; ']'; '['; 'K'; 'H'; 'w'; 'j'; 'B'; '\187'; 'b'; '9'; '\n'; '\r'; 
       'D'; '6'; '7'; 'E'; 'M'; '2'; '1'; '8'; '3'; ':'; 'Q'; '\156'; '='; '\147';
       '\142'; '\197'; '!'; '&'; '/'; '\180'; 'm'; 'l'; ' '; 'u'; '\153'; '\226';
       '\128'; 'z'; '5'; 'k'; '\160'; 'p'; 'o'; ','; 'v'; 'L'; 'y'; 'N'; '\167';
       '%'; 'X'; '\162'; 'S'; 'F'; 'G'; 'T'; 'O'; 'h'; '0']
    then 
      begin 
        ok := false; 
        comment_l := !comment_l @ ["create (toList) incorrect avec corpus_freq"]
      end;
    if (new huffman())#toStruct <>
      "<<<<,<,>>,<,<<<<,<,<,<<<,<<<<<,>,>,>,<,>>,>>,>,>>>>,<,<<,<<,>,>>,<,>>>>,<,>>,>>>,<<,>,>>,<<<,>,<<<<,<<,>,>>,<,<<<<<,<<,>,<,<,<,>>>>>,>,>,>,<,>>>>,<,<<<,<<,<<<,>,>,>>,>>,<,<,>>>,>>>,<<<<<,>,<,<,<,>>>>,<<<,>,>,<,<<,>,<,<<,<<,<<,>,<,>>>,>>,<<,>,>>>>>>>,>,>>>,<,<<,<<<,>,<,<<<,>,>,>>>,>>,<,<<,>,<<<,>,<<<,<,<<,>,>>>,>,<<,>,<,>>>>,<,>>>>>>>>"
    then 
      begin 
        ok := false; 
        comment_l := !comment_l @ ["create (toStruct) incorrect avec corpus_freq"]
      end;
    if (new huffman ~lf:l_big ())#toList <> ['A'; ' '; 'F'; 'E'; 'D'; 'C'; 'B']
    then 
      begin 
        ok := false; 
        comment_l := !comment_l @ ["create (toList) incorrect avec ~lf:l_big"]
      end;
    if (new huffman ~lf:l_big ())#toStruct <> "<,<<<<,>,>,<,>>,>>"
    then 
      begin 
        ok := false; 
        comment_l := !comment_l @ ["create (toStruct) incorrect avec ~lf:l_big"]
      end;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (8 PTS) ---------------------------------------- *)
(* @Méthode       : create : (char * int) list -> unit                       *)
(* @Description   : met à jour l'arbre à partir d'une liste de freq.         *)
let test1() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     l_A, Leaf 'A',"<h#create l_A; h#get>"; 
     l_AB, Node (Leaf 'A', Leaf 'B'),"<h#create l_AB; h#get>"; 
     l_BA, Node (Leaf 'B', Leaf 'A'),"<h#create l_BA; h#get>"; 
     l_ABC, Node (Node (Leaf 'A', Leaf 'B'), Leaf 'C'),
     "<h#create l_ABC; h#get>";
     l_ABC', Node (Leaf 'C', Node (Leaf 'A', Leaf 'B')),
     "<h#create l_ABC'; h#get>";
     l_big, Node (Leaf 'A', Node (Node (Node (Node (Leaf ' ', Leaf 'F'), 
            Leaf 'E'), Node (Leaf 'D', Leaf 'C')), Leaf 'B')),
     "<h#create l_big; h#get>";
     l_big', Node (Node (Node (Node (Node (Node (Leaf ' ', Leaf 'F'), 
             Leaf 'E'), Leaf 'D'), Leaf 'C'), Leaf 'B'), Leaf 'A'),
     "<h#create l_big'; h#get>"
    ]
  and jeu_donnees_exception =
    [
     [], "<h#create []>"
    ]
  in 

  try
    List.iter 
      ( fun (p, res, comment) ->
          if not ((h#create p; h#get) = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun (p, comment) ->
          try
            ignore(h#create p);
            ok := false; 
            comment_l := 
              !comment_l @ 
              [comment ^ " --> incorrect! Devrait soulever exception!"]
          with
            Failure _ -> ()
       ) jeu_donnees_exception;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (3 PTS) ---------------------------------------- *)
(* @Méthode       : toList : char list                                       *)
(* @Description   : retourne les éléments de l'arbre (profondeur d'abord)    *)
let test2() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     l_A, ['A'],"<h#create l_A; h#toList>"; 
     l_AB, ['A'; 'B'],"<h#create l_AB; h#toList>"; 
     l_BA, ['B'; 'A'],"<h#create l_BA; h#toList>"; 
     l_ABC, ['A'; 'B'; 'C'],
     "<h#create l_ABC; h#toList>";
     l_ABC', ['C'; 'A'; 'B'], "<h#create l_ABC'; h#toList>";
     l_big,  ['A'; ' '; 'F'; 'E'; 'D'; 'C'; 'B'], "<h#create l_big; h#toList>";
     l_big', [' '; 'F'; 'E'; 'D'; 'C'; 'B'; 'A'], "<h#create l_big'; h#toList>"
    ]
  in
  
  try
    List.iter 
      ( fun (p, res, comment) ->
          if not ((h#create p; h#toList) = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (4 PTS) ---------------------------------------- *)
(* @Méthode       : toStruct : string                                        *)
(* @Description   : retourne la structure de l'arbre (profondeur d'abord)    *)
let test3() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     l_A, "<>","<h#create l_A; h#toStruct>"; 
     l_AB, "<,>","<h#create l_AB; h#toStruct>"; 
     l_BA, "<,>","<h#create l_BA; h#toStruct>"; 
     l_ABC, "<<,>,>", "<h#create l_ABC; h#toStruct>";
     l_ABC', "<,<,>>", "<h#create l_ABC'; h#toStruct>";
     l_big,  "<,<<<<,>,>,<,>>,>>", "<h#create l_big; h#toStruct>";
     l_big', "<<<<<<,>,>,>,>,>,>", "<h#create l_big'; h#toStruct>"
    ]
  in
  
  try
    List.iter 
      ( fun (p, res, comment) ->
          if not ((h#create p; h#toStruct) = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (4 PTS) ---------------------------------------- *)
(* @Méthode       : toString : string                                        *)
(* @Description   : retourne l'arbre (profondeur d'abord)                    *)
let test4() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     l_A, "A","<h#create l_A; h#toString>"; 
     l_AB, "<A,B>","<h#create l_AB; h#toString>"; 
     l_BA, "<B,A>","<h#create l_BA; h#toString>"; 
     l_ABC, "<<A,B>,C>", "<h#create l_ABC; h#toString>";
     l_ABC', "<C,<A,B>>", "<h#create l_ABC'; h#toString>";
     l_big,  "<A,<<<< ,F>,E>,<D,C>>,B>>", "<h#create l_big; h#toString>";
     l_big', "<<<<<< ,F>,E>,D>,C>,B>,A>", "<h#create l_big'; h#toString>"
    ]
  in
  
  try
    List.iter 
      ( fun (p, res, comment) ->
          if not ((h#create p; h#toString) = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (10 PTS) --------------------------------------- *)
(* @Méthode       : subs : (char * char) list -> unit                        *)
(* @Description   : met à jour, éventuellement, les éléments d'un arbre      *)
let test5() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (l_A,[]), Leaf 'A',"<h#create l_A; h#subs []; h#get>"; 
     (l_A,['A','1']), Leaf '1',"<h#create l_A; h#subs ['A','1']; h#get>"; 
     (l_A,['B','1']), Leaf 'A',"<h#create l_A; h#subs ['B','1']; h#get>"; 
     (l_AB,[]), Node (Leaf 'A', Leaf 'B'),"<h#create l_A; h#subs []; h#get>"; 
     (l_AB,['A','1']), Node (Leaf '1', Leaf 'B'),
      "<h#create l_A; h#subs ['A','1']; h#get>"; 
     (l_AB,['A','1';'B','2']), Node (Leaf '1', Leaf '2'),
      "<h#create l_A; h#subs ['A','1';'B','1']; h#get>"; 
     (l_AB,['A','1';'B','2'; 'C', '3']), Node (Leaf '1', Leaf '2'),
      "<h#create l_A; h#subs ['A','1';'B','1'; C,'3']; h#get>"; 
     (l_big,[]), Node (Leaf 'A', Node (Node (Node (Node (Leaf ' ', Leaf 'F'), 
                 Leaf 'E'), Node (Leaf 'D', Leaf 'C')), Leaf 'B')),
      "<h#create l_big; h#subs []; h#get>";
     (l_big,[' ','?'; 'A', '2'; 'C','3']), 
      Node (Leaf '2', Node (Node (Node (Node (Leaf '?', Leaf 'F'), Leaf 'E'),
      Node (Leaf 'D', Leaf '3')), Leaf 'B')),
      "<h#create l_big; h#subs [' ','?'; 'A', '2'; 'C','3']; h#get>"
    ]
  in 

  try
    List.iter 
      ( fun ((p,p'), res, comment) ->
          if not ((h#create p; h#subs p'; h#get) = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (5 PTS) ---------------------------------------- *)
(* @Méthode       : charPath : char -> bin list                              *)
(* @Description   : retourne une liste de bin correspondant à un             *)
(*                  caractère passé en argument                              *)
let test6() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (l_A,'A'), [U], "<h#create l_A; h#charPath 'A'>"; 
     (l_AB,'A'), [Z], "<h#create l_AB; h#charPath 'A'>"; 
     (l_AB,'B'), [U], "<h#create l_AB; h#charPath 'B'>"; 
     (l_BA,'B'), [Z], "<h#create l_BA; h#charPath 'B'>"; 
     (l_BA,'A'), [U], "<h#create l_BA; h#charPath 'A'>"; 
     (l_ABC,'A'), [Z; Z], "<h#create l_ABC; h#charPath 'A'>"; 
     (l_ABC,'B'), [Z; U], "<h#create l_ABC; h#charPath 'B'>"; 
     (l_ABC,'C'), [U], "<h#create l_ABC; h#charPath 'C'>"; 
     (l_big','A'), [U], "<h#create l_big'; h#charPath 'A'>"; 
     (l_big','B'), [Z; U], "<h#create l_big'; h#charPath 'B'>"; 
     (l_big','C'), [Z; Z; U], "<h#create l_big'; h#charPath 'C'>"; 
     (l_big','D'), [Z; Z; Z; U], "<h#create l_big'; h#charPath 'D'>";
     (l_big','E'), [Z; Z; Z; Z; U], "<h#create l_big'; h#charPath 'E'>";
     (l_big','F'), [Z; Z; Z; Z; Z; U], "<h#create l_big'; h#charPath 'F'>";
     (l_big',' '), [Z; Z; Z; Z; Z; Z], "<h#create l_big'; h#charPath ' '>";
    ]
  and jeu_donnees_exception =
    [
     (l_A,'B'),  "<h#create l_A; h#charPath 'B'>"; 
     (l_AB,'C'), "<h#create l_AB; h#charPath 'C'>"; 
     (l_big,'X'), "<h#create l_big; h#charPath 'X'>"
    ]
  in
  
  try
    List.iter 
      ( fun ((p,p'), res, comment) ->
          if not ((h#create p; h#charPath p') = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun ((p,p'), comment) ->
          try
            h#create p; ignore(h#charPath p');
            ok := false; 
            comment_l := 
              !comment_l @ 
              [comment ^ " --> incorrect! Devrait soulever exception!"]
          with
            Failure _ -> ()
       ) jeu_donnees_exception;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e])


(* -- À IMPLANTER/COMPLÉTER (6 PTS) ---------------------------------------- *)
(* @Méthode       : decodeBinList : bin list -> char * bin list              *)
(* @Description   : retourne un caractère correspondant à une liste de       *)
(*                  bin passés en argument                                   *)
let test7() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (l_A,[U]), ('A', []), "<h#create l_A; h#decodeBinList [U]>"; 
     (l_A,[U;U]), ('A', [U]), "<h#create l_A; h#decodeBinList [U;U]>"; 
     (l_AB,[Z]), ('A', []), "<h#create l_AB; h#decodeBinList [Z]>"; 
     (l_AB,[U]), ('B', []), "<h#create l_AB; h#decodeBinList [U]>"; 
     (l_AB,[U;Z]), ('B', [Z]), "<h#create l_AB; h#decodeBinList [U;Z]>"; 
     (l_AB,[Z;Z;Z]), ('A', [Z; Z]), 
      "<h#create l_AB; h#decodeBinList [Z;Z;Z]>"; 
     (l_BA,[Z]), ('B', []), "<h#create l_BA; h#decodeBinList [Z]>"; 
     (l_BA,[U]), ('A', []), "<h#create l_BA; h#decodeBinList [U]>"; 
     (l_ABC,[Z; Z]),('A', []), "<h#create l_ABC; h#decodeBinList [Z; Z]>"; 
     (l_ABC,[Z; U]),('B', []), "<h#create l_ABC; h#decodeBinList [Z; U]>"; 
     (l_ABC,[U]), ('C', []), "<h#create l_ABC; h#decodeBinList [U]>"; 
     (l_ABC,[U; Z]), ('C', [Z]), "<h#create l_ABC; h#decodeBinList [U;Z]>"; 
     (l_big',[U]), ('A', []), "<h#create l_big'; h#decodeBinList [U]>"; 
     (l_big',[Z; U]), ('B', []), "<h#create l_big'; h#decodeBinList [Z; U]>"; 
     (l_big',[U; Z]), ('A', [Z]), "<h#create l_big'; h#decodeBinList [U; Z]>"; 
     (l_big',[Z; Z; U]), ('C', []), 
      "<h#create l_big'; h#decodeBinList [Z; Z; U]>"; 
     (l_big',[Z; Z; Z; U]), ('D', []), 
      "<h#create l_big'; h#decodeBinList [Z; Z; Z; U]>";
     (l_big',[Z; Z; Z; Z; U]), ('E', []), 
      "<h#create l_big'; h#decodeBinList [Z; Z; Z; Z; U]>";
     (l_big',[Z; Z; Z; Z; U; U]), ('E', [U]), 
      "<h#create l_big'; h#decodeBinList [Z; Z; Z; Z; U; U]>";
     (l_big',[Z; Z; Z; Z; Z; U]), ('F', []), 
      "<h#create l_big'; h#decodeBinList [Z; Z; Z; Z; Z; U]>";
     (l_big',[Z; Z; Z; Z; Z; Z]), (' ', []), 
      "<h#create l_big'; h#decodeBinList [Z; Z; Z; Z; Z; Z]>";
     (l_big',[Z; Z; Z; Z; Z; Z; Z]), (' ', [Z]), 
      "<h#create l_big'; h#decodeBinList [Z; Z; Z; Z; Z; Z; Z]>"
    ]
  and jeu_donnees_exception =
    [
     (l_A,[Z]),  "<h#create l_A; h#decodeBinList [Z]>"; 
     (l_A,[Z;U]),  "<h#create l_A; h#decodeBinList [Z;U]>"; 
     (l_big',[Z]), "<h#create l_big'; h#decodeBinList [Z]>";
     (l_big',[Z; Z]), "<h#create l_big'; h#decodeBinList [Z; Z]>"
    ]
  in
  
  try
    List.iter 
      ( fun ((p,p'), res, comment) ->
          if not ((h#create p; h#decodeBinList p') = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun ((p,p'), comment) ->
          try
            h#create p; ignore(h#decodeBinList p');
            ok := false; 
            comment_l := 
              !comment_l @ 
              [comment ^ " --> incorrect! Devrait soulever exception!"]
          with
            Failure _ -> ()
       ) jeu_donnees_exception;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;



(* Partie Zip                                                                *) 
(*****************************************************************************) 

(* -- À IMPLANTER/COMPLÉTER (20 PTS) --------------------------------------- *)
(* @Méthode       : codeStream : char Stream.t -> 
                                 char Stream.t*(unit -> int)*(unit -> int)   *)
(* @Description   : Est testée via codeStr                                   *)
let test8() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (l_A,""), ("", 0), "<(new zip ~lf:l_A ())#codeStr \"\">"; 
     (l_A,"A"), ("02<>A\001\001", -600), 
      "<(new zip ~lf:l_A ())#codeStr \"A\">"; 
     (l_A,"AA"), ("02<>A\003\002", -250), 
      "<(new zip ~lf:l_A ())#codeStr \"AA\">"; 
     (l_A,"AAAAAAA"), ("02<>A\127\007", 0), 
      "<(new zip ~lf:l_A ())#codeStr \"AAAAAAA\">"; 
     (l_ABC,""), ("", 0), "<(new zip ~lf:l_ABC ())#codeStr \"\">"; 
     (l_ABC,"A"), ("06<<,>,>ABC\000\002", -1200), 
      "<(new zip ~lf:l_ABC ())#codeStr \"A\">"; 
     (l_ABC,"B"), ("06<<,>,>ABC\001\002", -1200), 
      "<(new zip ~lf:l_ABC ())#codeStr \"B\">"; 
     (l_ABC,"C"), ("06<<,>,>ABC\001\001", -1200), 
      "<(new zip ~lf:l_ABC ())#codeStr \"C\">"; 
     (l_ABC,"AAAAAAAAAAAAAAAAA"), ("06<<,>,>ABC\000\000\000\000\000\002", 0), 
      "<(new zip ~lf:l_ABC ())#codeStr \"AAAAAAAAAAAAAAAAA\">"; 
     (l_ABC,"AAAAAAAAAAAAAAAAAA"), ("06<<,>,>ABC\000\000\000\000\000\004", 5), 
      "<(new zip ~lf:l_ABC ())#codeStr \"AAAAAAAAAAAAAAAAAA\">"; 
     (l_big,""), ("", 0), "<(new zip ~lf:l_big ())#codeStr \"\">"; 
     (l_big,"A"), ("018<,<<<<,>,>,<,>>,>>A FEDCB\000\001", -2900), 
      "<(new zip ~lf:l_big ())#codeStr \"A\">"; 
     (l_big,"AB"), ("018<,<<<<,>,>,<,>>,>>A FEDCB\003\003", -1400), 
      "<(new zip ~lf:l_big ())#codeStr \"AB\">"; 
     (l_big,"ABC"), ("018<,<<<<,>,>,<,>>,>>A FEDCB;\007", -900), 
      "<(new zip ~lf:l_big ())#codeStr \"ABC\">"; 
     (l_big,"ABCD"), ("018<,<<<<,>,>,<,>>,>>A FEDCBw\002\003", -675), 
      "<(new zip ~lf:l_big ())#codeStr \"ABCD\">"; 
     (l_big,"ABCDE"), ("018<,<<<<,>,>,<,>>,>>A FEDCBw)\007", -520), 
      "<(new zip ~lf:l_big ())#codeStr \"ABCDE\">"; 
     (l_big,"ABCDEF"), ("018<,<<<<,>,>,<,>>,>>A FEDCBwS\001\004", -433), 
      "<(new zip ~lf:l_big ())#codeStr \"ABCDEF\">"; 
     (l_big,"ABCDEF "), ("018<,<<<<,>,>,<,>>,>>A FEDCBwS\024\000\001", -371), 
      "<(new zip ~lf:l_big ())#codeStr \"ABCDEF \">"; 
    ]
  and jeu_donnees_exception =
    [
     (l_A,"B"),  "<(new zip ~lf:l_A ())#codeStr \"B\">"; 
     (l_ABC," "), "<(new zip ~lf:l_ABC ())#codeStr \" \">"; 
     (l_big,"X"), "<(new zip ~lf:l_big ())#codeStr \"X\">"
    ]
  in
  
  try
    List.iter 
      ( fun ((p,p'), res, comment) ->
          if not ((new zip ~lf:p ())#codeStr p' = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun ((p,p'), comment) ->
          try
            ignore((new zip ~lf:p ())#codeStr p');
            ok := false; 
            comment_l := 
              !comment_l @ 
              [comment ^ " --> incorrect! Devrait soulever exception!"]
          with
            Failure _ -> ()
       ) jeu_donnees_exception;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e])


(* -- À IMPLANTER/COMPLÉTER (15 PTS) --------------------------------------- *)
(* @Méthode       : decodeStream : char Stream.t -> char Stream.t            *)
(* @Description   : Est testée via decodeStr                                 *)
let test9() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (l_A,""), "", "<(new zip ~lf:l_A ())#decodeStr \"\">"; 
     (l_A,"02<>A\001\001"), "A", 
      "<(new zip ~lf:l_A ())#decodeStr \"02<>A\001\001\">"; 
     (l_A,"02<>A\003\002"), "AA", 
      "<(new zip ~lf:l_A ())#decodeStr \"02<>A\003\002\">"; 
     (l_A,"02<>A\127\007"), "AAAAAAA", 
      "<(new zip ~lf:l_A ())#decodeStr \"02<>A\127\007\">"; 
     (l_ABC,""), "", "<(new zip ~lf:l_ABC ())#decodeStr \"\">"; 
     (l_ABC,"06<<,>,>ABC\000\002"), "A", 
      "<(new zip ~lf:l_ABC ())#decodeStr \"06<<,>,>ABC\000\002\">"; 
     (l_ABC,"06<<,>,>ABC\001\002"), "B", 
      "<(new zip ~lf:l_ABC ())#decodeStr \"06<<,>,>ABC\001\002\">"; 
     (l_ABC,"06<<,>,>ABC\001\001"), "C", 
      "<(new zip ~lf:l_ABC ())#decodeStr \"06<<,>,>ABC\001\001\">"; 
     (l_ABC,"06<<,>,>ABC\000\000\000\000\000\002"), "AAAAAAAAAAAAAAAAA", 
      "<(new zip ~lf:l_ABC ())#decodeStr \"06<<,>,>ABC\000\000\000\000\000\002\">"; 
     (l_ABC,"06<<,>,>ABC\000\000\000\000\000\004"), "AAAAAAAAAAAAAAAAAA", 
      "<(new zip ~lf:l_ABC ())#decodeStr \"06<<,>,>ABC\000\000\000\000\000\004\">"; 
     (l_big,""), "", "<(new zip ~lf:l_big ())#decodeStr \"\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCB\000\001"), "A", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCB\000\001\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCB\003\003"), "AB", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCB\003\003\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCB;\007"), "ABC", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCB;\007\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCBw\002\003"), "ABCD", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCBw\002\003\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCBw)\007"), "ABCDE", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCBw)\007\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCBwS\001\004"), "ABCDEF", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCBwS\001\004\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCBwS\024\000\001"), "ABCDEF ", 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCBwS\024\000\001\">"; 
    ]
  and jeu_donnees_exception =
    [
     (l_A,"XY"), "<(new zip ~lf:l_A ())#decodeStr \"XY\">"; 
     (l_ABC,"05<<,>,>ABC\000\002"), 
      "<(new zip ~lf:l_ABC ())#decodeStr \"05<<,>,>ABC\000\002\">"; 
     (l_ABC,"07<<,>,>ABC\000\002"), 
      "<(new zip ~lf:l_ABC ())#decodeStr \"07<<,>,>ABC\000\002\">"; 
     (l_ABC,"06<<,>,>AB\000\002"), 
      "<(new zip ~lf:l_ABC ())#decodeStr \"06<<,>,>AB\000\002\">"; 
     (l_big,"018<,<<<<,>,>,<,>>,>>A FEDCB\001"), 
      "<(new zip ~lf:l_big ())#decodeStr \"018<,<<<<,>,>,<,>>,>>A FEDCB\001\">"
    ]
  in
  
  try
    List.iter 
      ( fun ((p,p'), res, comment) ->
          if not ((new zip ~lf:p ())#decodeStr p' = res) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun ((p,p'), comment) ->
          try
            ignore((new zip ~lf:p ())#decodeStr p');
            ok := false; 
            comment_l := 
              !comment_l @ 
              [comment ^ " --> incorrect! Devrait soulever exception!"]
          with
            Failure _ -> ()
       ) jeu_donnees_exception;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e])


(* Teste indirectement codeStream/decodeStream avec le corpus *)
let test9_1() =
  let comment_l = ref [] in
  let ok = ref true in

  try
    let s = "Le premier livre numérique est l’eText" in
    let z = new zip () in
    if (s <> z#decodeStr(fst (z#codeStr s))) ||
       ("" <> z#decodeStr(fst (z#codeStr "")))
    then 
      begin 
        ok := false; 
        comment_l := !comment_l @ ["codeStream ou decodeStream incorrect avec corpus"]
      end;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (10 PTS) --------------------------------------- *)
(* @Méthode       : codeFiles :string list -> string -> int                  *)
(* @Description   : code une liste de fichiers                               *)
let test10() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (["corpus2.txt";"corpus3.txt"],"ift3000_tmp.txt"),
      (40,"corpus23_coded.txt"), 
      "<(new zip())#codeFiles [\"corpus.txt\";\"corpus.txt\"] \"ift3000_tmp.txt\">"
    ]
  in
  
  try
    List.iter 
      ( fun ((p,p'), (res,res'), comment) ->
          let r = (new zip())#codeFiles p p' in
          if not (r = res && file_eq p' res') then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end;
          Sys.remove p'
      ) jeu_donnees;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* -- À IMPLANTER/COMPLÉTER (10 PTS) --------------------------------------- *)
(* @Méthode       : decodeFiles :string -> unit                              *)
(* @Description   : decode une liste de fichiers                             *)
let test11() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     "corpus23_coded.txt", 
     (["corpus2.txt";"corpus3.txt"],["corpus.txt";"corpus.txt"]), 
      "<(new zip())#decodeFiles \"corpus23_coded.txt\">"
    ]
  in
  
  try
    List.iter 
      (fun (p, (res,res'), comment) ->
          List.iter Sys.remove res;
          (new zip())#decodeFiles p;
          if not (List.for_all2 file_eq res res') then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    (!ok, !comment_l)
  with
  | e -> 
    (false, !comment_l @ ["Exception soulevée: " ^ Printexc.to_string e]);;


(* --------------------------------------------------------------------------- *)
(* -- TESTE TOUT ------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test() =
  let all_tests = [ 
                    "create/intializer", test0;
                    "create", test1;
                    "toList", test2;
                    "toStruct", test3;
                    "toString", test4;
                    "subs", test5;                    
                    "charPath", test6;
                    "decodeBinList", test7;
                    "codeStream", test8;
                    "decodeStream", test9;
                    "codeStream/decodeStream", test9_1;
                    (* "codeFiles", test10;
                    "decodeFiles", test11 *)
                  ] in
  List.fold_left 
    (fun l_res (nom_f,t) -> 
      let (ok,comment) = t () in 
      l_res @ [(nom_f, ok, comment)]
    ) [] all_tests;;


(* --------------------------------------------------------------------------- *)
(* -- CORRIGE ---------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let corrige () =
  print_endline "\nResultats des tests:";
  print_endline "--------------------\n";
  let ok = ref true in
  let open Printf in
    List.iter
      (fun (nom_f, ok', comment) -> 
         ok := !ok && ok';
         printf "%s : %s\n" nom_f (if ok' then "Ok" else "Problème");
         List.iter (fun c -> print_endline ("\t- " ^ c)) comment
      ) (test());
    if !ok 
    then 
      printf "\nTous les tests sont Ok!\n\n"
    else 
      printf "\nCertaines fonctions sont à corriger ou à compléter!\n\n"


(*
Utilisation du testeur avec le corrigé
--------------------------------------

# #use "testeur.ml";;
module Utiles :
  sig
    val timeRun : ('a -> 'b) -> 'a -> 'b * float
    val explode : string -> char list
    val implode : char list -> string
    val drop : int -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val lfreq : 'a Stream.t -> ('a * int) list
    val stream_next_until : 'a Stream.t -> 'a -> 'a list
    val stream_npeek : int -> 'a Stream.t -> 'a list
    val stream_eq : 'a Stream.t -> 'a Stream.t -> bool
    val file_eq : string -> string -> bool
    val corpus_lfreq : (char * int) list
  end
module type HUFFMAN =
  sig
    type tree = Leaf of char | Node of tree * tree
    type bin = U | Z
    class huffman :
      ?lf:(char * int) list ->
      unit ->
      object
        val mutable def_lfreq : bool
        val mutable t : tree
        method charFromPath : bin list -> char
        method charPath : char -> bin list
        method codeChar : char -> bin list
        method create : (char * int) list -> unit
        method decodeBinList : bin list -> char * bin list
        method get : tree
        method show : ?file:string -> unit -> int
        method subs : (char * char) list -> unit
        method toList : char list
        method toString : string
        method toStruct : string
      end
  end
module Huffman : HUFFMAN
module type ZIP =
  sig
    val toInt : Huffman.bin list -> int
    val toBin : int -> Huffman.bin list
    class zip :
      ?lf:(char * int) list ->
      unit ->
      object
        val mutable def_lfreq : bool
        val mutable t : Huffman.tree
        method charFromPath : Huffman.bin list -> char
        method charPath : char -> Huffman.bin list
        method codeChar : char -> Huffman.bin list
        method codeFile : string -> string -> int
        method codeFiles : string list -> string -> int
        method codeStr : string -> string * int
        method codeStream :
          char Stream.t -> char Stream.t * (unit -> int) * (unit -> int)
        method create : (char * int) list -> unit
        method decodeBinList : Huffman.bin list -> char * Huffman.bin list
        method decodeFile : string -> string -> unit
        method decodeFiles : string -> unit
        method decodeStr : string -> string
        method decodeStream : char Stream.t -> char Stream.t
        method get : Huffman.tree
        method show : ?file:string -> unit -> int
        method subs : (char * char) list -> unit
        method toList : char list
        method toString : string
        method toStruct : string
      end
  end
module Zip : ZIP
module TestConformite1 : HUFFMAN
module TestConformite2 : ZIP
val l_A : (char * int) list = [('A', 1)]
val l_AB : (char * int) list = [('A', 1); ('B', 2)]
val l_BA : (char * int) list = [('A', 2); ('B', 1)]
val l_ABC : (char * int) list = [('A', 1); ('B', 2); ('C', 4)]
val l_ABC' : (char * int) list = [('A', 2); ('B', 3); ('C', 4)]
val l_big : (char * int) list =
  [('A', 30); ('F', 2); ('B', 20); ('D', 5); (' ', 1); ('C', 6); ('E', 4)]
val l_big' : (char * int) list =
  [(' ', 1); ('F', 2); ('E', 4); ('D', 8); ('C', 20); ('B', 40); ('A', 80)]
val h : TestConformite1.huffman = <obj>
val test0 : unit -> bool * string list = <fun>
val test1 : unit -> bool * string list = <fun>
val test2 : unit -> bool * string list = <fun>
val test3 : unit -> bool * string list = <fun>
val test4 : unit -> bool * string list = <fun>
val test5 : unit -> bool * string list = <fun>
val test6 : unit -> bool * string list = <fun>
val test7 : unit -> bool * string list = <fun>
val test8 : unit -> bool * string list = <fun>
val test9 : unit -> bool * string list = <fun>
val test9_1 : unit -> bool * string list = <fun>
val test10 : unit -> bool * string list = <fun>
val test11 : unit -> bool * string list = <fun>
val test : unit -> (string * bool * string list) list = <fun>
val corrige : unit -> unit = <fun>
# corrige();;

Resultats des tests:
--------------------

create/intializer : Ok
create : Ok
toList : Ok
toStruct : Ok
toString : Ok
subs : Ok
charPath : Ok
decodeBinList : Ok
codeStream : Ok
decodeStream : Ok
codeStream/decodeStream : Ok

Tous les tests sont Ok!

- : unit = ()
*)