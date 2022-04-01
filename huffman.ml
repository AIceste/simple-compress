(***************************************************************************) 
(* TP II Hiver 2020 (partie I)                                             *) 
(*                                                                         *) 
(* Arbre de Huffman et compression de fichiers                             *) 
(***************************************************************************) 
(* NOM: DESMANCHE            PRÉNOM: Gérémy                                *) 
(* MATRICULE: 111 232 013    PROGRAMME: BI mathématiques et informatique   *) 
(***************************************************************************) 
(*                                                                         *) 
(* Une fois l'implantation du TP terminée et une fois que vous aurez       *) 
(* chargé le fichier huffman.ml (#use "huffman.ml";;), vous pouvez ouvrir  *)
(* le module et exploiter la classe et les méthodes qui s'y trouvent.      *) 
(*                                                                         *) 
(* Aussi, en considérant un arbre, vous pouvez afficher cet arbre en       *)
(* utilisant la fonction afficherArbre. Notez que l'utilisation de cette   *)
(* fonction nécessite l'installation de l'outil graphviz:                  *)
(*     http://www.graphviz.org                                             *) 
(***************************************************************************) 

#use "utiles.ml";;
#use "huffman.mli";; 
           
(***************************************************************************) 
(* Implantation                                                            *) 
(***************************************************************************) 
module Huffman : HUFFMAN =
struct
  open Utiles

  (* Structure de données permettant de définir un arbre                   *)
  type tree = Leaf of char | Node of tree * tree
  type bin = U | Z

  let string_of_bins bl =
    let addbin b l = match b with | U -> 'U'::l | Z -> 'Z'::l in
    implode (List.fold_right addbin bl [])

  (* Classe huffman: certaines méthodes sont à compléter (voir (******))   *)
        
  class huffman ?lf () = object(this) 
    val mutable def_lfreq = true
      
    val mutable t = Leaf 'a' (* Initialisation arbitraire *)
    method get = t
  
    method codeChar c = 
     this#charPath c
  
    method show ?(file="graph") () =
      (* Cas particuliers à traiter *)
      let toStr c = match Char.escaped c with
        | "\"" -> "\\\""
        | "%" -> "\\%"
        | "\\n" -> "\\\\n"
        | "\\r" -> "\\\\r"
        | s -> s
      in
      let n = ref 0 in
      let lout = Buffer.create 10 in
      let rec f ar ch = match ar with
        | Leaf _ -> ch  
        | Node(Leaf c,Leaf c') -> 
          incr n; 
            ch ^ "\tn_" ^ (string_of_int (!n)) ^ " -> \"" ^ (toStr c) 
               ^ "\" [ label = \"Z\" ];\n" ^ "\tn_" 
               ^ (string_of_int (!n)) ^ " -> \"" ^ (toStr c') 
               ^ "\" [ label = \"U\" ];\n"
        | Node(Leaf c,rt) -> 
          incr n;
          let ch' = 
            ch ^ "\tn_" ^ (string_of_int (!n)) ^ " -> \"" ^ (toStr c) 
               ^ "\" [ label = \"Z\" ];\n" ^ "\tn_" 
               ^ (string_of_int (!n)) ^ " -> n_" 
               ^ (string_of_int (!n+1)) ^ " [ label = \"U\" ];\n"
          in
          f rt ch'
        | Node(lt, Leaf c) -> 
          incr n;
          Buffer.add_string 
            lout ("\tn_" ^ (string_of_int (!n+1)) ^ " [ordering=\"out\"];\n");
          let ch' =
            ch ^ "\tn_" ^ (string_of_int (!n)) ^ " -> n_" 
               ^ (string_of_int (!n+1)) ^ " [ label = \"Z\" ];\n"
               ^ "\tn_" ^ (string_of_int (!n)) ^ " -> \"" ^ (toStr c) 
               ^ "\" [ label = \"U\" ];\n"
          in
          f lt ch'
        | Node(lt, rt) -> 
          incr n;
          let old_n = !n in
          let ch1 = ch ^ "\tn_" ^ (string_of_int (!n)) ^ " -> n_" ^ 
                    (string_of_int (!n+1)) ^ " [ label = \"Z\" ];\n" in
          let chg = f lt ch1 in
          let ch2 = chg ^ "\tn_" ^ (string_of_int (old_n)) ^ " -> n_" ^ 
                    (string_of_int (!n+1)) ^ " [ label = \"U\" ];\n" in
          f rt ch2 
      in
      let lc = this#toList in
      let s1 = 
        "digraph G { \n\tnode [shape=\"record\", style=\"rounded\"];\n\n" in
      let s2 = 
        List.fold_left 
          (fun r c -> r ^ "\t\"" ^ (toStr c) ^ "\""  ^ 
             " [fillcolor=lightgray, style=\"rounded,filled\",shape=\"circle\"];\n"
          ) "" lc in
      let s3 = "\n" ^ (f t "") ^ "}" in
      let s1' = Buffer.contents lout in 
      let oc = open_out file in
      output_string oc (s1 ^ s1' ^ s2 ^ s3); 
      close_out oc; 
      ignore(Sys.command ("dot -Tpdf -Gcharset=latin1 " 
                          ^ file ^ " > " ^ file ^ ".pdf"));
      ( match Sys.os_type with
         | "Win32" -> Sys.command ("start " ^ file ^ ".pdf")
         | _ -> Sys.command ("xdg-open " ^ file ^ ".pdf")
      )

    initializer
      match lf with
        | None -> this#create corpus_lfreq
        | Some [] -> failwith "Liste lf vide!"
        | Some lf' -> def_lfreq <- false; this#create lf'


     (***********************************************************)
     (* Section des méthodes à implémanter dans le cadre du TP2 *)
     
    method create freq_list =
      let nodes = List.map (fun (c, a) -> Leaf(c), a) freq_list
      and combine (n0, c0) (n1, c1) = [Node(n0, n1), c0 + c1]
      and compare (_, c0) (_, c1) = c0 - c1
      in 
      let rec merge node_list =
        match node_list with
        | [] -> failwith "Mauvaise utilisation ! (cette méthode est privée)"
        | [(node, _)] -> node
        | n0::n1::nl -> merge (List.merge compare (combine n0 n1) nl)
      in 
        t <- merge (List.sort compare nodes);
        def_lfreq <- false

    (* Navigating from right to left allows usage of list prepending instead
       of list concatenations. *)
    method toList = 
      let rec aux n cl =
        match n with
        | Leaf(c) -> c::cl
        | Node(n0, n1) -> aux n0 (aux n1 cl)
      in aux this#get []

    method toStruct =
      match this#get with
      | Leaf(_) -> "<>"
      | n -> 
        let rec aux n s =
          match n with
          | Leaf(_) -> s
          | Node(n0, n1) -> '<'::(aux n0 (','::(aux n1 ('>'::s))))
        in implode (aux n [])

    method toString =
        let rec aux n s =
          match n with
          | Leaf(c) -> c::s
          | Node(n0, n1) -> '<'::(aux n0 (','::(aux n1 ('>'::s))))
        in implode (aux this#get [])

    (* Considering its usage, this method means we're back to left-to-right
       navigation. That being said, I'm also shortening the substitution list
       and,in the process of doing so, potentially altering its order. However,
       the aforementioned list, in planned usage of the subs function, should
       contain exactly the characters of the tree, in the very same order, so
       the list first entry should always contain the currently inspected leaf,
       allowing the left-to-right navigation to remain optimal while ultimately
       skipping a lot of list navigation. *)
    method subs substitution_list =
      let rec leaf_of c sl nu_sl =
        match sl with
        | (v, s)::tl -> if v = c
                        then Leaf(s), nu_sl @ sl (* hopefully nu_sl = [] c: *)
                        else leaf_of c tl ((v, s)::nu_sl)
        | [] -> Leaf(c), nu_sl
      and node_only (n, sl) = n
      in
      let rec aux sl n = 
        match n with
        | Leaf(c) -> leaf_of c sl []
        | Node(n0, n1) -> 
          let ln, nu_sl = aux sl n0 in Node(ln, node_only (aux nu_sl n1)), nu_sl
      in t <- node_only (aux substitution_list this#get)

    (* Actually tried the depht-last search, the gain was rather small (~15%) 
       but it could be greater with larger trees, and it's still that gained. *)
    method charPath c =
      let path = ref None in
      let rec aux c nodes =
        match !path with
        | Some(p) -> 
          if List.length p = 0
          then [U]
          else List.rev p
        | None ->
          match nodes with
          | [] -> failwith ("<" ^ (String.make 1 c) ^ "> introuvable dans l'arbre")
          | _ -> 
            let proc n_list (p, n) =
              match n with
              | Leaf(v) -> 
                if v = c
                then (
                  path := Some(p);
                  []
                )
                else n_list
              | Node(n0, n1) -> (Z::p, n0)::((U::p, n1)::n_list)
            in
            let next_nodes = List.fold_left proc [] nodes in
            aux c next_nodes
      in
      aux c [([], this#get)]
        
            
    method charFromPath path = 
      let rec aux p n =
        match p, n with
        | U::tl, Node(_, n) | Z::tl, Node(n, _) -> aux tl n
        | [], Leaf(c) -> c
        | _ -> failwith ("feuille introuvable dans l'arbre : " ^ (string_of_bins path))
      in
        match path, this#get with
        | [U], Leaf(c) -> c
        | p, n -> aux p n

    method decodeBinList pl = 
      let error = "liste binaire incorrecte : " in
      let rec aux p n =
        match p, n with
        | U::tl, Node(_, n) | Z::tl, Node(n, _) -> aux tl n
        | tl, Leaf(c) -> c, tl
        | [], Node(_, _) -> failwith (error ^ (string_of_bins pl))
      in
        match pl, this#get with
        | U::tl, Leaf(c) -> c, tl
        | _, Leaf(_) -> failwith (error ^ (string_of_bins pl))
        | pl, p -> aux pl p

     (* ffs it's so annoying always having to deal with single leaf edge cases *)
     (* a lot of the code would be much cleaner without it. :c *)
  end
end

(*
# open Huffman;;
# let h = new huffman ~lf:[('a',1)] ();;
val h : Huffman.huffman = <obj>
# h#get;;
- : Huffman.tree = Leaf 'a'
# h#toStruct;;
- : string = "<>"
# h#toList;;
- : char list = ['a']
# h#toString;;
- : string = "a"
# h#charPath 'a';;
- : Huffman.bin list = [U]
# h#charFromPath [U];;
- : char = 'a'
# open Utiles;;
# let h = new huffman ~lf:(lfreq (Stream.of_string "AABCAABADBACAAB")) ();;
val h : Huffman.huffman = <obj>
# h#create [('a',1)];;
- : unit = ()
# h#get;;
- : Huffman.tree = Leaf 'a'
# h#create [('a',2); ('b',1)];;
- : unit = ()
# h#get;;
- : Huffman.tree = Node (Leaf 'b', Leaf 'a')
# h#create [('a',2); ('b',1); ('c', 3)];;
- : unit = ()
# h#get;;
- : Huffman.tree = Node (Node (Leaf 'b', Leaf 'a'), Leaf 'c')
# h#create [];;
Exception: Failure "lf vide!".
# h#create (lfreq (Stream.of_string "AABCAABADBACAAB"));;
- : unit = ()
# h#get;;
- : Huffman.tree =
Node (Node (Node (Leaf 'D', Leaf 'C'), Leaf 'B'), Leaf 'A')
# h#toList;;
- : char list = ['D'; 'C'; 'B'; 'A']
# let ha = new huffman ~lf:[('a',1)] ();;
val ha : Huffman.huffman = <obj>
# ha#toList;;
- : char list = ['a']
# h#toStruct;;
- : string = "<<<,>,>,>"
# ha#toStruct;;
- : string = "<>"
# h#toString;;
- : string = "<<<D,C>,B>,A>"
# ha#toString;;
- : string = "a"
# h#subs [('A','1'); ('B','2'); ('C','3'); ('D','4')];;
- : unit = ()
# h#toString;;
- : string = "<<<4,3>,2>,1>"
# h#subs [('2','B'); ('1','A'); ('4','D'); ('3','C')];;
- : unit = ()
# h#toString;;
- : string = "<<<D,C>,B>,A>"
# ha#subs [('A','1'); ('B','2'); ('C','3'); ('D','4')];;
- : unit = ()
# ha#toString;;
- : string = "a"
# ha#subs [('a','b')];;
- : unit = ()
# ha#toString;;
- : string = "b"
# h#toString;;
- : string = "<<<D,C>,B>,A>"
# h#charPath 'D';;
- : Huffman.bin list = [Z; Z; Z]
# h#charPath 'C';;
- : Huffman.bin list = [Z; Z; U]
# h#charPath 'B';;
- : Huffman.bin list = [Z; U]
# h#charPath 'A';;
- : Huffman.bin list = [U]
# h#charPath 'Z';;
Exception: Failure "<Z> introuvable dans l'arbre".
# ha#toString;;
- : string = "b"
# ha#charPath 'b';;
- : Huffman.bin list = [U]
# h#charFromPath [Z;U];;
- : char = 'B'
# ha#charFromPath [U];;
- : char = 'b'
# h#charFromPath [Z;U;U];;
Exception: Failure "feuille introuvable dans l'arbre".
# ha#charFromPath [Z];;
Exception: Failure "feuille introuvable dans l'arbre".
# h#decodeBinList [Z;U;U];;
- : char * Huffman.bin list = ('B', [U])
# let c1, l1 = h#decodeBinList [U;Z;U];;
val c1 : char = 'A'
val l1 : Huffman.bin list = [Z; U]
# let c2, l2 = h#decodeBinList l1;;
val c2 : char = 'B'
val l2 : Huffman.bin list = []
# ha#decodeBinList [U;U];;
- : char * Huffman.bin list = ('b', [U])
# h#decodeBinList [Z];;
Exception: Failure "liste binaire incorrecte".
# let h' = new huffman ();;
val h' : Huffman.huffman = <obj>
# h'#toString;;
- : string =
"<<<...
# h'#toList;;
- : char list =
['a'; '\195'; 'c'; 'r'; '\168'; 'A'; 'U'; '\185'; '\135'; '\177'; '+'; '<';
 '#'; '\175'; 'Z'; '\137'; '4'; '\''; 'R'; '\174'; 'V'; 'W'; '\170'; '\171';
 'q'; '.'; 'd'; 't'; 'n'; 'e'; 's'; 'i'; 'f'; '-'; '\194'; 'x'; 'g'; '?';
 '@'; 'Y'; ';'; '\157'; '>'; '\130'; 'J'; 'I'; '('; ')'; 'C'; '\169'; 'P';
 '"'; ']'; '['; 'K'; 'H'; 'w'; 'j'; 'B'; '\187'; 'b'; '9'; '\n'; '\r'; 'D';
 '6'; '7'; 'E'; 'M'; '2'; '1'; '8'; '3'; ':'; 'Q'; '\156'; '='; '\147';
 '\142'; '\197'; '!'; '&'; '/'; '\180'; 'm'; 'l'; ' '; 'u'; '\153'; '\226';
 '\128'; 'z'; '5'; 'k'; '\160'; 'p'; 'o'; ','; 'v'; 'L'; 'y'; 'N'; '\167';
 '%'; 'X'; '\162'; 'S'; 'F'; 'G'; 'T'; 'O'; 'h'; '0']
# h'#charPath 'e';;
- : Huffman.bin list = [Z; U; U]
# h'#charPath 'Y';;
- : Huffman.bin list = [U; Z; U; Z; Z; U; U; Z; Z; Z; Z; U; Z; U]
*)
