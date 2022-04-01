(***************************************************************************) 
(* TP Hiver 2020 (partie II)                                               *) 
(*                                                                         *) 
(* Implantation de l'algorithme d'Huffman pour la compressions de textes   *) 

(***************************************************************************) 
(* Specification - ZIP -                                                   *) 
(***************************************************************************) 
#use "huffman.ml";;

module type ZIP =
sig

  val toInt : Huffman.bin list -> int
  val toBin : int -> Huffman.bin list

  class zip : ?lf:(char * int) list -> unit ->
  object
    (* méthode héritées de huffman *)
    val mutable def_lfreq : bool

    val mutable t : Huffman.tree 
    method get : Huffman.tree

    method create : (char * int) list -> unit
    method show : ?file:string -> unit -> int

    method toList : char list
    method toString : string
    method toStruct : string

    method subs : (char * char) list -> unit

    method charPath : char -> Huffman.bin list
    method charFromPath : Huffman.bin list -> char

    method codeChar : char -> Huffman.bin list
    method decodeBinList : Huffman.bin list -> char * Huffman.bin list

    (* nouvelles méthodes implantées dans cette classe *) 
    method codeStream : char Stream.t -> char Stream.t * (unit -> int) * (unit -> int)
    method decodeStream : char Stream.t -> char Stream.t

    method codeStr : string -> string * int
    method decodeStr : string -> string

    method codeFile : string -> string -> int
    method decodeFile : string -> string -> unit

    method codeFiles : string list -> string -> int
    method decodeFiles : string -> unit

   end

end
