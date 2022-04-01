(* --------------------------------------------------------------------------*)
(* ----------------------- TP2 - IFT-3000 - Hiver 2020 ----------------------*)
(* --------------------------------------------------------------------------*)
(* Fichier permettant de tester les fonctions implantées du TP               *)
(* --------------------------------------------------------------------------*)
(* On entre dans l'interpréteur ce qui suit:                                 *)
(*                                                                           *)
(* # #use "testeur2.ml";;                                                    *)
(*                                                                           *)
(* Par la suite:                                                             *)
(*                                                                           *)
(* # corrige();;  (* Teste toutes les fonctions de tests *)                  *)
(* # testn();;    (* n = 1 ou 2 ...; lance le test numéro n *)               *)
(*                                                                           *)
(* Lorsque un des fichiers du Tp est modifié, vous n'avez juste qu'à:        *)
(* - recharger le fichier testeur2.ml.                                       *)
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


(* Définitions de fonctions utiles                                           *) 
(*****************************************************************************) 
let file_cp inf outf = 
  let ic1 = open_in_bin inf in
  let ic2 = open_out_bin outf in
  Stream.iter (output_char ic2) (Stream.of_channel ic1);
  close_in ic1;
  close_out ic2;;


(* Définitions de différentes valeurs globales                               *) 
(*****************************************************************************) 
let l = [('A',12);('B',11);('C',10);('D',9);('E',8);('F',7);
         ('G',6);('H',5);('I',4);('J',3);('K',2);('L',1)];;
(* voir graph_l.pdf *)

let h = new huffman();;

let z = new zip ~lf:l ();;

(*****************************************************************************) 
(* Fonctions de tests                                                        *) 
(*****************************************************************************) 


(* Partie Huffman                                                            *) 
(*****************************************************************************) 

(* -- À IMPLANTER/COMPLÉTER (8 PTS) ---------------------------------------- *)
(* @Méthode       : create : (char * int) list -> unit                       *)
(* @Description   : met à jour l'arbre à partir d'une liste de freq.         *)
let test1() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     l, Node (Node (Node (Leaf 'F', Leaf 'E'),Node (Node (Leaf 'I', Leaf 'H'), 
        Leaf 'D')), Node (Node (Leaf 'C', Leaf 'B'), Node (Node (Node (Node 
        (Leaf 'L', Leaf 'K'), Leaf 'J'), Leaf 'G'), Leaf 'A'))), 
     "<h#create l; h#get>"
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
     l, ['F'; 'E'; 'I'; 'H'; 'D'; 'C'; 'B'; 'L'; 'K'; 'J'; 'G'; 'A'], 
     "<h#create l; h#toList>"
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
     l, "<<<,>,<<,>,>>,<<,>,<<<<,>,>,>,>>>", "<h#create l; h#toStruct>"
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
     l, "<<<F,E>,<<I,H>,D>>,<<C,B>,<<<<L,K>,J>,G>,A>>>", 
     "<h#create l; h#toString>"
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

  let lc = ['F'; 'E'; 'I'; 'H'; 'D'; 'C'; 'B'; 'L'; 'K'; 'J'; 'G'; 'A'] in
  let lc' = List.map (fun c -> c, Char.lowercase_ascii c) lc in 
  let jeu_donnees =
    [
     (l,lc'), 
      Node (Node (Node (Leaf 'f', Leaf 'e'),Node (Node (Leaf 'i', Leaf 'h'), 
      Leaf 'd')), Node (Node (Leaf 'c', Leaf 'b'), Node (Node (Node (Node 
      (Leaf 'l', Leaf 'k'), Leaf 'j'), Leaf 'g'), Leaf 'a'))),
      "<h#create l; h#subs lc'; h#get>"
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
     (l,'E'), [Z; Z; U], "<h#create l; h#charPath 'E'>";
     (l,'F'), [Z; Z; Z], "<h#create l; h#charPath 'F'>";
     (l,'D'), [Z; U; U], "<h#create l; h#charPath 'D'>";
     (l,'C'), [U; Z; Z], "<h#create l; h#charPath 'C'>";
     (l,'L'), [U; U; Z; Z; Z; Z], "<h#create l; h#charPath 'L'>";
     (l,'K'), [U; U; Z; Z; Z; U], "<h#create l; h#charPath 'K'>";
     (l,'A'), [U; U; U], "<h#create l; h#charPath 'A'>"
    ]
  and jeu_donnees_exception =
    [
     (l,'X'),  "<h#create l; h#charPath 'X'>"
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
     (l,[Z; Z; U]), ('E', []), "<h#create l; h#decodeBinList [Z; Z; U]>"; 
     (l,[Z; Z; Z]), ('F', []), "<h#create l; h#decodeBinList [Z; Z; Z]>"; 
     (l,[Z; U; U]), ('D', []), "<h#create l; h#decodeBinList [Z; U; U]>"; 
     (l,[U; Z; Z]), ('C', []), "<h#create l; h#decodeBinList [U; Z; Z]>"; 
     (l,[U; U; Z; Z; Z; Z]), ('L', []), 
      "<h#create l; h#decodeBinList [U; U; Z; Z; Z; Z]>"; 
     (l,[U; U; Z; Z; Z; U]), ('K', []), 
      "<h#create l; h#decodeBinList [U; U; Z; Z; Z; U]>"; 
     (l,[U; U; U]), ('A', []), "<h#create l; h#decodeBinList [U; U; U]>"; 
     (l,[Z; U; U; Z]), ('D', [Z]), 
     "<h#create l; h#decodeBinList [Z; U; U; Z]>"
    ]
  and jeu_donnees_exception =
    [
     (l,[Z;U]), "<h#create l; h#decodeBinList [Z;U]>"; 
     (l,[Z;Z]), "<h#create l; h#decodeBinList [Z;Z]>"; 
     (l,[U;Z]), "<h#create l; h#decodeBinList [U;Z]>";
     (l,[U; U; Z]), "<h#create l; h#decodeBinList [U; U; Z]>"
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
(* @Description   : code un flot de caractères                               *)
let test8() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     "", ([], 0, 0), "<codeStream: cas \"\">";
     "L", ([48], 6, 1), "<codeStream: cas \"L\">";
     "LL", ([195; 0], 4, 2), "<codeStream: cas \"LL\">";
     "HH", ([85], 0, 2), "<codeStream: cas \"HH\">";
     "FEE", ([4; 1], 1, 3), "<codeStream: cas \"FEE\">";
     "AAAAAAAA", ([255; 255; 255], 0, 8), "<codeStream: cas \"AAAAAAAA\">";
     "FFFFFFFF", ([0; 0; 0], 0, 8), "<codeStream: cas \"FFFFFFFF\">";
     "ABCDEFGHIJKL", ([246; 50; 53; 83; 56; 112], 7, 12), 
     "<codeStream: cas \"ABCDEFGHIJKL\">";
    ]
  and jeu_donnees_exception =
    [
     "X", "<codeStream: cas \"X\">";
     "LIX", "<codeStream: cas \"LIX\">"
    ]
  in
  
  try
    List.iter 
      ( fun (str, res, comment) ->
          let out_s, f1, f2 = z#codeStream (Stream.of_string str) in
          let lc = ref [] in 
          let _ = Stream.iter (fun c -> lc := !lc @ [Char.code c]) out_s in 
          let res' = !lc, f1(), f2() in 
          if not (res = res') then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun (str, comment) ->
          let out_s, f1, f2 = z#codeStream (Stream.of_string str) in
          let lc = ref [] in 
          try
            Stream.iter (fun c -> lc := !lc @ [Char.code c]) out_s;
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
(* @Description   : decode un flot de caractères                             *)
let test9() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     [], "", "<decodeStream: cas []>";
     [48;6], "L", "<decodeStream: cas [48;1]>";
     [195; 0; 4], "LL", "<decodeStream: cas [195; 0; 4]>";
     [85; 0], "HH", "<decodeStream: cas [85; 0]>";
     [4; 1; 1], "FEE", "<decodeStream: cas [4; 1; 1]>";
     [255; 255; 255; 0], "AAAAAAAA", "<decodeStream: cas [255; 255; 255; 0]>";
     [0; 0; 0; 0], "FFFFFFFF", "<decodeStream: cas [0; 0; 0; 0]>";
     [246; 50; 53; 83; 56; 112; 7], "ABCDEFGHIJKL", 
     "<decodeStream: cas [246; 50; 53; 83; 56; 112; 7]>";
    ]
  and jeu_donnees_exception =
    [
     [255; 255; 255], "<decodeStream: cas [255; 255; 255]>";
     [0; 0; 0], "<decodeStream: cas [0; 0; 0]>";
   ]
  in
  
  try
    List.iter 
      ( fun (p, res, comment) ->
          let l =  List.map char_of_int p in 
          let lc = ref l in 
          let out_s = z#decodeStream (Stream.of_list !lc) in 
          let lc' = ref [] in 
          let _ = Stream.iter (fun c -> lc' := !lc' @ [c]) out_s in 
          let res' = implode !lc' in
          if not (res = res') then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end
      ) jeu_donnees;
    List.iter 
      ( fun (p, comment) ->
          let l =  List.map char_of_int p in 
          let lc = ref l in 
          let out_s = z#decodeStream (Stream.of_list !lc) in 
          let lc' = ref [] in 
          try
            Stream.iter (fun c -> lc' := !lc' @ [c]) out_s;
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


(* Teste indirectement codeStream/decodeStream via codeStr/decodeStr *)
let test9_1() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     "", "<z#decodeStr (fst (z#codeStr \"\"))>";
     "L", "<z#decodeStr (fst (z#codeStr \"L\"))>";
     "LL", "<z#decodeStr (fst (z#codeStr \"LL\"))>";
     "HH", "<z#decodeStr (fst (z#codeStr \"HH\"))>";
     "FEE", "<z#decodeStr (fst (z#codeStr \"FEE\"))>";
     "AAAAAAAA", "<z#decodeStr (fst (z#codeStr \"AAAAAAAA\"))>";
     "FFFFFFFF", "<z#decodeStr (fst (z#codeStr \"FFFFFFFF\"))>";
     "ABCDEFGHIJKL", 
     "<z#decodeStr (fst (z#codeStr \"ABCDEFGHIJKL\"))>";
    ]
  in 
  try
    List.iter 
      ( fun (p, comment) ->
          if not (z#decodeStr (fst (z#codeStr p)) = p) then
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
(* @Méthode       : codeFiles :string list -> string -> int                  *)
(* @Description   : code une liste de fichiers                               *)
let test10() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (["corpus2.txt";"corpus3.txt"],"ift3000_tmp.txt"),
      39, 
      "<(new zip())#codeFiles [\"corpus2.txt\";\"corpus3.txt\"] \"ift3000_tmp.txt\">";
     (["corpus2.txt"],"ift3000_tmp.txt"),
      38, 
      "<(new zip())#codeFiles [\"corpus2.txt\"] \"ift3000_tmp.txt\">";
     (["corpus_en1.txt";"corpus2.txt"],"ift3000_tmp.txt"),
      39, 
      "<(new zip())#codeFiles [\"corpus_en1.txt\";\"corpus2.txt\"] \"ift3000_tmp.txt\">";
      (["corpus_en1.txt"],"ift3000_tmp.txt"),
      40, 
      "<(new zip())#codeFiles [\"corpus_en1.txt\"] \"ift3000_tmp.txt\">";
   ]
  and jeu_donnees_exception =
    [
     ([],"ift3000_tmp.txt"), "<(new zip())#codeFiles [] \"ift3000_tmp.txt\">"      
   ]
  in
  
  try
    List.iter 
      ( fun ((p,p'), res, comment) ->
      	  let _ = try Sys.remove p' with _ -> () in 
          let _ = (new zip())#codeFiles p p' in
          let ics = List.map open_in_bin p in
          let ic = open_in_bin p' in 
          let lsizes = List.map in_channel_length ics in
          let psizes = List.fold_left (+) 0 lsizes in 
          let psize' = in_channel_length ic in
          let _ = close_in ic; List.iter close_in ics in
          let r = int_of_float 
                     ((1.0 -. ((float_of_int (psize')) /. 
                               (float_of_int (psizes)))) *. 100.0) in
          if not (r <= res + 2 && r >= res - 2) then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end;
          Sys.remove p'
      ) jeu_donnees;
    List.iter 
      ( fun ((p,p'), comment) ->
          try
            ignore((new zip())#codeFiles p p');
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


(* -- À IMPLANTER/COMPLÉTER (10 PTS) --------------------------------------- *)
(* @Méthode       : decodeFiles :string -> unit                              *)
(* @Description   : decode une liste de fichiers                             *)
let test11() =
  let comment_l = ref [] in
  let ok = ref true in

  let jeu_donnees =
    [
     (["corpus2.txt";"corpus3.txt"],"ift3000_tmp.txt"), 
      "<(new zip())#codeFiles [\"corpus2.txt\";\"corpus3.txt\"] \"ift3000_tmp.txt\"; 
        (new zip())#decodeFiles \"ift3000_tmp.txt\">";        
     (["corpus2.txt"],"ift3000_tmp.txt"), 
      "<(new zip())#codeFiles [\"corpus2.txt\"] \"ift3000_tmp.txt\"; 
        (new zip())#decodeFiles \"ift3000_tmp.txt\">";      
     (["corpus_en1.txt"],"ift3000_tmp.txt"), 
      "<(new zip())#codeFiles [\"corpus_en1.txt\"] \"ift3000_tmp.txt\"; 
        (new zip())#decodeFiles \"ift3000_tmp.txt\">";      
     (["corpus_en1.txt";"corpus2.txt"],"ift3000_tmp.txt"), 
      "<(new zip())#codeFiles [\"corpus_en1.txt\";\"corpus2.txt\"] \"ift3000_tmp.txt\"; 
        (new zip())#decodeFiles \"ift3000_tmp.txt\">";      
    ]
  in
  
  try
    List.iter 
      (fun ((p,p'), comment) ->
      	  let _ = try Sys.remove p' with _ -> () in
      	  let _ = List.iter (fun f -> file_cp f (f ^ ".tmp.bak")) p in
      	  let p'' = List.map (fun f -> f ^ ".tmp.bak") p in
          let _ = (new zip())#codeFiles p p' in 
          let _ = List.iter Sys.remove p in
          let _ = (new zip())#decodeFiles p' in 
          if not (List.for_all2 file_eq p p'') then
          begin 
            ok := false; 
            comment_l := !comment_l @ [comment ^ " --> incorrect!"]
          end;
          List.iter Sys.remove (p'::p'')
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
                    "codeFiles", test10;
                    "decodeFiles", test11
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

# #use "testeur2.ml";;
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
val file_cp : string -> string -> unit = <fun>
val l : (char * int) list =
  [('A', 12); ('B', 11); ('C', 10); ('D', 9); ('E', 8); ('F', 7); ('G', 6);
   ('H', 5); ('I', 4); ('J', 3); ('K', 2); ('L', 1)]
val h : TestConformite1.huffman = <obj>
val z : TestConformite2.zip = <obj>
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
# corrige ();;

Resultats des tests:
--------------------

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
codeFiles : Ok
decodeFiles : Ok

Tous les tests sont Ok!

- : unit = ()
*)
