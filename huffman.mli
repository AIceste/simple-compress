(***************************************************************************) 
(* TP Hiver 2020 (partie I)                                                *) 
(*                                                                         *) 
(* Implantation de l'algorithme d'Huffman pour la compressions de textes   *) 

(***************************************************************************) 
(* Sp�cification - HUFFMAN                                                 *) 
(***************************************************************************) 

module type HUFFMAN = 
sig
  (* Structure de donn�es permettant de d�finir un arbre *)
  type tree = Leaf of char | Node of tree * tree
  type bin = U | Z

  (* Interface: classes et m�thodes � implanter *)
  class huffman : ?lf:(char * int) list -> unit ->
  object
    val mutable def_lfreq : bool

    val mutable t : tree 
    method get : tree

    method create : (char * int) list -> unit

    method toList : char list
    method toString : string
    method toStruct : string
 
    method subs : (char * char) list -> unit

    method charPath : char -> bin list
    method charFromPath : bin list -> char

    method codeChar : char -> bin list
    method decodeBinList : bin list -> char * bin list

    method show : ?file:string -> unit -> int
  end
  
end
