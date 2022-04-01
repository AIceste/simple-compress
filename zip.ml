(***************************************************************************) 
(* TP II Hiver 2020 (partie II)                                            *) 
(*                                                                         *) 
(* Arbre de Huffman et compression de fichiers                             *) 
(***************************************************************************) 
(* NOM: DESMANCHE            PRÉNOM: Gérémy                                *) 
(* MATRICULE: 111 232 013    PROGRAMME: BI mathématiques et informatique   *) 
(***************************************************************************) 

#use "zip.mli";;

(***************************************************************************) 
(* Implantation                                                            *) 
(***************************************************************************) 

module Zip : ZIP =
struct
  open Huffman 
  open Utiles
  open Lazy

  open List

  let nb = 8

  (* PARTIE FONCTIONNELLE *)
  (* -------------------- *)

  (* toInt : bin list -> int                                          *)
  (*         Transforme une liste de codes binaires en un entier      *)
  let toInt l_bin =
    let _2exp n = int_of_float (2. ** (float n)) in
    let n = length l_bin in
    if n > nb || n = 0 then
      failwith "taille incorrecte de l_bin à convertir!"
    else
      fold_left (+) 0 
        (mapi (fun i -> function Z -> 0 | U -> _2exp (n-1-i)) l_bin)

  (* toBin : (i:int) -> bin list                                      *)
  (*         Transforme un entier en une liste de n codes binaires    *)
  let toBin n =
    let rec fillHd b n' l = 
      if n' = 0 then l else fillHd b (n'-1) (b::l) in
    let rec f n' =
      if n' = 0 
      then []
      else 
    		let n'' = n' / 2 in
    		let l = f n'' in
        if (n' mod 2 = 0) then l @ [Z] else l @ [U]
    in
    if n < 0 || n > 255 then
      failwith "entier incorrect à convertir en l_bin!"
    else
      let l = if n = 0 then [] else f n in
      let nb' = nb - (List.length l) in
      if nb' < 0 then l else fillHd Z nb' l
	                   

  (* PARTIE ORIENTÉE OBJET *)
  (* --------------------- *)

  class zip ?lf () = object(this)
    inherit huffman ?lf ()

    val mutable max_depth = match lf with
      | None -> 256
      (* Naive maximum that doesn't require knowing Huffman's implementation *)
      (* Same idea also used in fromStruct to keep max_depth up to date. *)
      | Some(l) -> List.length l

    (* PARTIE CODAGE *)
    (* ------------- *)

    method private codeStruct =
      let lc = 
        if def_lfreq then 
          ['1']
        else 
          ['0']
          @
          (
            let s1 = this#toStruct in
            let s2 = string_of_int (String.length s1) in
            let lc' = this#toList in
            (explode (s2 ^ s1)) @ lc'
          )
      in
      Stream.of_list lc

    method private code inStream f =     
      if Stream.peek inStream = None then
        0
      else
        let out_stream = this#codeStruct in
        let n_chars2 = ref 0 in
        let _ = Stream.iter (fun c -> incr n_chars2; f c) out_stream in
        let out_stream, getLsize, getNChar = this#codeStream inStream in
        let n_chars2' = ref 1 in
        let _ = Stream.iter (fun c -> incr n_chars2'; f c) out_stream in
        let _ = f (char_of_int (getLsize())) in
        let ratio = int_of_float 
                        ((1.0 -. ((float_of_int (!n_chars2 + !n_chars2')) /. 
                                  (float_of_int (getNChar())))) *. 100.0) in
        ratio

    method codeStr str  =
      let buff = Buffer.create (String.length str) in
      let ratio = this#code (Stream.of_string str) (Buffer.add_char buff) in
      Buffer.contents buff, ratio
 
	method codeFile inFile outFile  =
      let in_channel = open_in_bin inFile in
      let out_channel = open_out_bin outFile in
      let ratio = 
        this#code (Stream.of_channel in_channel) (output_char out_channel) in
      close_in in_channel;
      close_out out_channel;
  	  ratio


    (* PARTIE DECODAGE *)
    (* --------------- *)

    method private fromStruct =
      let cpt = ref 0 in
      let rec fromStructAux = function 
      	| '<'::r -> 
    	    let lt,r1 = fromStructAux' r in
    	      ( match r1 with
          		| ','::r2 -> 
                let rt,r3 = fromStructAux'' r2 in
    			       ( match r3 with 
          				 | '>'::r4 -> Node(lt,rt), r4
          				 | _ -> failwith "prob de parsing"
    			       )
          		| _ -> failwith "prob de parsing"
    	      )
      	| _ -> failwith "prob de parsing"
      and fromStructAux' = function 
      	| '<'::_ as l -> fromStructAux l
      	| (','::_) as l -> incr cpt; Leaf (char_of_int !cpt), l 
      	| _ -> failwith "prob de parsing"
      and fromStructAux'' = function 
      	| '<'::_ as l -> fromStructAux l
      	| ('>'::_) as l -> incr cpt; Leaf (char_of_int !cpt), l 
      	| _ -> failwith "prob de parsing"
      in
    	function 
    	  | '<'::'>'::[] ->
       		t <- Leaf (char_of_int 1);
       		max_depth <- 1;
            max_depth
        | '<'::'>'::_ ->
          failwith "prob de parsing"
    	  | l ->
          cpt := 0;  
  	      let t', _ = fromStructAux l in
        		t <- t';
        		max_depth <- !cpt;
                max_depth
		   
    method private decodeTree in_stream =
      let c = Stream.next in_stream in
      if c = '0' then 
        begin
          let l = stream_next_until in_stream '<' in
          let n1 = 
            try int_of_string (implode l) 
            with _ -> failwith "prob de parsing" 
          in
          let la = stream_npeek n1 in_stream in
          let n2 = this#fromStruct la in
          let lc = stream_npeek n2 in_stream in
          let l_subs = 
            let cpt = ref 0 in
            List.map (fun c -> incr cpt; char_of_int !cpt,c) lc 
          in
          this#subs l_subs
        end;
      in_stream

    method private decode inStream f =
      if Stream.peek inStream <> None then
        let in_stream = this#decodeTree inStream in
        let out_stream = this#decodeStream in_stream in
        Stream.iter f out_stream

    method decodeStr str  =
      let in_stream = Stream.of_string str in
      let buff = Buffer.create (String.length str) in
      this#decode in_stream (Buffer.add_char buff);
      Buffer.contents buff

    method decodeFile inFile outFile  =
      let in_channel = open_in_bin inFile in
      let out_channel = open_out_bin outFile in
      let in_stream = Stream.of_channel in_channel in
      this#decode in_stream (output_char out_channel);
      close_in in_channel;
      close_out out_channel

    (************************************)
    (* Partie complétée par l'étudiant. *)

    (* Note: It's kind of strange to have flexible code using nb constant only
       to forcefully declare codeStream & decodeStream to use char streams.
       ...guess it'll be easier to switch to another data type that way. *)

    (* A lot of my stream-generating functions are completely ignoring the arg
       since Streams are lazy loading things, and I have no idea if it's an ok
       practice or not. *)

    (* I could probably try some memoisation to see how it impacts perfs. *)
    method codeStream str =
      let curr_bits = ref [] in
      let last_len = ref 0 in
      let str_gen i =
        let rec get_bits curr = 
          let len = List.length curr in
          if len < nb
          then match Stream.next str with
            | exception Stream.Failure -> curr
            | c -> get_bits (curr @ (this#codeChar c))
          else curr
        in
        let bits = get_bits !curr_bits in
        let len = List.length bits in
        if len = 0
        then None
        else
          if len < nb
          then
            let num = toInt bits in 
            curr_bits := []; last_len := len; Some(Char.chr num)
          else
            let num = toInt (take nb bits) in
            curr_bits := drop nb bits; last_len := nb; Some(Char.chr num)
      in 
      let getLSize () = !last_len in
      let getNChar () = Stream.count str in
      Stream.from str_gen, getLSize, getNChar 
        
    (* Pretty sure (maybe?) this one could have an optimised version. 
       The npeek 3 thing is pretty ugly, too. *)
    method decodeStream str =
      let curr_bits = ref [] in 
      let to_bits num = toBin (Char.code num) in (* That's pretty messed up. *)
      let str_gen i = 
        let rec get_bits curr = 
          let len = List.length curr in
          if len < max_depth
          then match Stream.npeek 3 str with
            | [] -> curr
            | [last] -> failwith "Invalid input stream"
            | [last; nbit] -> 
              Stream.junk str; Stream.junk str;
              curr @ (drop (nb - Char.code nbit) (to_bits last))
            | num::tl -> Stream.junk str;
              get_bits (curr @ (to_bits num)) (* This here sure hurts. *)
          else curr
        in
        let bits = get_bits !curr_bits in
        if List.length bits = 0 
        then None
        else
          let value, tl = this#decodeBinList bits in
          curr_bits := tl; Some(value)
      in
      Stream.from str_gen

    (* I didn't want to create a temporary file since it seemed like a lot of 
       work, but more importantly, a risk of overwriting some innocent file of
       strange name.

       I tried making full advantage of Streams without too much overhead, but
       this version is, on my system where there's twice as much available RAM
       as there's used disk space, still significantly slower (~20% on cursus)
       than the naive implementation that just loaded all of the files in some
       large Buffer before #codeStr ing it. My best guess is that the buffered
       IO that Stream.of_channel probably implies use small enough buffer that 
       a few syscall to synchronous read are required to read the entire file,
       where the naive implementation needed a single call per file.
       I did not trace it so I'll never know.

       I could certainly try to limit channel's open time, but it make str_gen
       even more ugly. (Edit: there's now an example of how ugly it would get
       right underneath..)

       Code being private, I cannot use code if I create an internal zip object
       and I certainly don't think recoding all the private methods is a good 
       idea, so I actually have to save the object's current state, change it
       to a new state according to processed frequencies list, do the code call
       and finally revert back to previous state...

       For error handling, I didn't add custom messages for errors catched by
       the used stdlib functions, so the interface's error handling isn't well
       normalised.

       ..I talk too much.
    *)

    method codeFiles inpaths outpath =
      if Sys.file_exists outpath 
      then failwith (outpath ^ " existe déjà dans le répertoire")
      else if List.length inpaths = 0
      then failwith "Aucun fichier à compresser."
      else
        let add_unique acc file = 
          if List.exists (fun x -> x = file) acc
          then (
            print_endline (file ^ " ignoré : déjà dans la liste");
            acc
          )
          else file::acc 
        in
        (* Switched to fold_left to get tail recursion, so I reorder files. *)
        let inpaths_noduplicate = List.fold_left add_unique [] inpaths in
        let open_file file =
          let in_ch = open_in file in
          let size = in_channel_length in_ch in
          in_ch, String.concat "|" [string_of_int size; file]
        in
        (* Opening all files at once to get the size..
           oh well. It keeps cleanup simple. *)
        let inputs = List.map open_file inpaths_noduplicate in
        let descs = List.map (fun (_, desc) -> desc) inputs in
		(* Using two and three characters to split is slightly larger, but no
           collision is possible with those on either Unix-based or Windows. *)
        let head = (String.concat "//" descs) ^ "///" in
        let create_in_stream () =
          (* Reset read positions *)
          List.iter (fun (ch, desc) -> seek_in ch 0) inputs; 
          let nexts = ref inputs in
          let curr_str = ref (Stream.of_string head) in
          let rec str_gen i =
            match Stream.next !curr_str with
            | c -> Some(c)
            | exception Stream.Failure -> (
              match !nexts with
              | [] -> None
              | (chan, _)::tl ->
                curr_str := Stream.of_channel chan;
                nexts := tl;
                str_gen i
            )
          in
          Stream.from str_gen
        in
        let char_freqs = lfreq (create_in_stream ()) in
        let in_stream = create_in_stream () in
        let out = open_out outpath in
        let cleanup () = 
          close_out out;
          List.iter (fun (ch, desc) -> close_in ch) inputs
        in
        try
          (* Save olf file. *)
          let old_def_lfreq = def_lfreq
          and old_t = t
          in
          this#create char_freqs; 
          let ratio = this#code in_stream (output_char out) in
          cleanup (); 
          def_lfreq <- old_def_lfreq; t <- old_t; (* Reset to old state. *)
          ratio
        with e ->
          cleanup (); raise e

    (* Damn, error handling sure is taking up a lot of effort (aka code space)
       in this one ! :o 

       (Of course, the above comments on codeFiles still apply.)

       The function being pretty ugly (to the naked eyes), I might comment it.
       But I'll first try to normalise my own code style since it evolved quite
       a lot during the few hours this took, which seems normal considering my
       lack of prior OCaml experience.

       Also we tested that the function could perform better in the interpreter
       if I altered the order of some let ... in ''', but I decided to priorise
       namespace epuration over peak performances in interpreter.
    *)
    method decodeFiles inpath =
      let error_invalid_file = inpath ^ " n'est pas un fichier compressé valide" in
      let in_ch = open_in inpath in
      let in_cleanup () = close_in in_ch in
      let curr_cleanup = ref in_cleanup in
      try
        let in_str = Stream.of_channel in_ch in
        let curr_handler = ref (fun c -> ()) in
        
        (* Part managing the single-channel emulation as output. I'm still not
           satisfied with how this turned out and would like something lighter
           without using options, since they imply lot of match code lines. *)
        let files_rem = ref [] in
        let curr_rem = ref 0 in
        let curr_out_ch = ref None in
        let get_channel () =
          let close_file () =
            match !curr_out_ch with
            | None -> ()
            | Some(ch) -> 
              close_out ch;
              curr_cleanup := in_cleanup
          and open_file name size tl = (
            curr_out_ch := Some(open_out name);
            curr_rem := size;
            files_rem := tl;
            curr_cleanup := (
              fun () -> (
                match !curr_out_ch with
                | None -> ()
                | Some(ch) -> close_out ch
              ); in_cleanup ()
            )
          )
          in
          if !curr_rem <> 0
          then !curr_out_ch
          else (
            close_file ();
            match !files_rem with 
            | [] -> curr_out_ch := None; !curr_out_ch
            | (name, size)::tl -> open_file name size tl; !curr_out_ch
          )
        in

        (* Part doing the writing of decoded content to them files. *)
        let to_file c =
          let ch_opt = get_channel () in
          curr_rem := pred !curr_rem;
          match ch_opt with
          | None -> failwith error_invalid_file
          | Some(ch) -> output_char ch c
        in

        (* Part handling the index decoding and files validation. It's slightly
           heavy in code but I guess it's slightly my fault for handling every
           possible file names. Could probably do better, still. *)
        let slash_count = ref 0 in 
        let head_buf = Buffer.create 84 in 
        let decode_index c =
          let add_char = Buffer.add_char head_buf in
          let parse_head () =
            let head_str = Buffer.sub head_buf 0 (Buffer.length head_buf - 2) in
            let entry_from_str str = 
              try
                match String.split_on_char '|' str with
                | size_str::name_parts -> 
                  String.concat "|" name_parts, int_of_string size_str
                | _ -> failwith "" (* The content doesn't matter here.. *)
              with e -> failwith error_invalid_file
            in
            let entries_from_str str =
              let rec aux entries parts remain =
                let entry_of parts = String.concat "/" (List.rev parts) in
                match remain with
                | [] -> List.map entry_from_str (List.rev ((entry_of parts)::entries))
                | ""::tl -> aux ((entry_of parts)::entries) [] tl
                | part::tl -> aux entries (part::parts) tl
              in
              aux [] [] (String.split_on_char '/' str)
            in 
            let files = entries_from_str head_str in
            let check_file c (f, _) = 
              if Sys.file_exists f
              then (
                print_endline (f ^ " existe déjà dans le répertoire"); succ c
              )
              else c
            in
            let cnt = List.fold_left check_file 0 files in
            if cnt <> 0
            then (
				print_endline (
                    (string_of_int cnt) 
                  ^ " fichier(s) à écraser. Appuyer sur entrer pour continuer."
                );
                ignore(read_line ())
            );
            files_rem := files;
            Buffer.clear head_buf;
            curr_handler := to_file
          in
          match c with
          | '/' -> 
            slash_count := succ !slash_count;
            if !slash_count = 3 
            then parse_head ()
            else add_char c; 
          | _ -> slash_count := 0; add_char c
        in

        (* The actual decoding using above tools *)
        curr_handler := decode_index;
        let handle_char c = 
          !curr_handler c
        in
        this#decode in_str handle_char;
        !curr_cleanup (); 
      with e ->
        !curr_cleanup ();
        raise e

  end
end

(*
Exemples de l'énoncé:

# let o = new zip ~lf:[('z',2);('t',5);('c',6);('a',15);('i',20)] ();;
val o : Zip.zip = <obj>
# let s, ratio = o#codeStr "cat";;
val s : string = "012<,<<,<,>>,>>iczta■\001\001"
val ratio : int = -666
# o#decodeStr s;;
- : string = "cat"
# let o' = new zip ();;
val o' : Zip.zip = <obj>
# let s, ratio = o'#codeStr "cat";;
val s : string = "1\024\004\005"
val ratio : int = -33
# o'#decodeStr s;;
- : string = "cat"


# open Huffman;;
# open Zip;;
# open Utiles;;
# let z = new zip ~lf:(lfreq (Stream.of_string "AABCAABADBACAAB")) ();;
val z : Zip.zip = <obj>
# z#toString;;
- : string = "<<<D,C>,B>,A>"
# let out_stream, f1, f2 = z#codeStream (Stream.of_string "AABBCCDD");;
val out_stream : char Stream.t = <abstr>
val f1 : unit -> int = <fun>
val f2 : unit -> int = <fun>
# let lc = ref [];;
val lc : '_weak1 list ref = {contents = []}
# Stream.iter (fun c -> lc := !lc @ [c]) out_stream;;
- : unit = ()
# !lc;;
- : char list = ['\212'; '\144'; '\000']
# List.concat (List.map z#codeChar (explode "AABBCCDD"));;
- : Huffman.bin list = [U; U; Z; U; Z; U; Z; Z; U; Z; Z; U; Z; Z; Z; Z; Z; Z]
# toInt [U; U; Z; U; Z; U; Z; Z];;
- : int = 212
# toInt [U; Z; Z; U; Z; Z; Z; Z];;
- : int = 144
# toInt [Z;Z];;
- : int = 0
# f1();;
- : int = 2
# f2();;
- : int = 8
# lc := !lc @ [char_of_int 2];;
- : unit = ()
# !lc;;
- : char list = ['\212'; '\144'; '\000'; '\002']
# let out_stream' = z#decodeStream (Stream.of_list !lc);;
val out_stream' : char Stream.t = <abstr>
# let lc' = ref [];;
val lc' : '_weak2 list ref = {contents = []}
# Stream.iter (fun c -> lc' := !lc' @ [c]) out_stream';;
- : unit = ()
# implode (!lc');;
- : string = "AABBCCDD"
# let lb0 = toBin (int_of_char (List.nth !lc 0));;
val lb0 : Huffman.bin list = [U; U; Z; U; Z; U; Z; Z]
# let lb1 = toBin (int_of_char (List.nth !lc 1));;
val lb1 : Huffman.bin list = [U; Z; Z; U; Z; Z; Z; Z]
# let lb2 = toBin (int_of_char (List.nth !lc 2));;
val lb2 : Huffman.bin list = [Z; Z; Z; Z; Z; Z; Z; Z]
# let lb2 = drop ((List.length lb2) - (int_of_char (List.nth !lc 3))) lb2;;
val lb2 : Huffman.bin list = [Z; Z]
# let lbin = lb0 @ lb1 @ lb2;;
val lbin : Huffman.bin list =
  [U; U; Z; U; Z; U; Z; Z; U; Z; Z; U; Z; Z; Z; Z; Z; Z]
# let c1,lbin = z#decodeBinList lbin;;
val c1 : char = 'A'
val lbin : Huffman.bin list =
  [U; Z; U; Z; U; Z; Z; U; Z; Z; U; Z; Z; Z; Z; Z; Z]
# let c2,lbin = z#decodeBinList lbin;;
val c2 : char = 'A'
val lbin : Huffman.bin list =
  [Z; U; Z; U; Z; Z; U; Z; Z; U; Z; Z; Z; Z; Z; Z]
# let c3,lbin = z#decodeBinList lbin;;
val c3 : char = 'B'
val lbin : Huffman.bin list = [Z; U; Z; Z; U; Z; Z; U; Z; Z; Z; Z; Z; Z]
# let c4,lbin = z#decodeBinList lbin;;
val c4 : char = 'B'
val lbin : Huffman.bin list = [Z; Z; U; Z; Z; U; Z; Z; Z; Z; Z; Z]
# let c5,lbin = z#decodeBinList lbin;;
val c5 : char = 'C'
val lbin : Huffman.bin list = [Z; Z; U; Z; Z; Z; Z; Z; Z]
# let c6,lbin = z#decodeBinList lbin;;
val c6 : char = 'C'
val lbin : Huffman.bin list = [Z; Z; Z; Z; Z; Z]
# let c7,lbin = z#decodeBinList lbin;;
val c7 : char = 'D'
val lbin : Huffman.bin list = [Z; Z; Z]
# let c8,lbin = z#decodeBinList lbin;;
val c8 : char = 'D'
val lbin : Huffman.bin list = []
# let s = "AABBCCDDAABBCCDDAABBCCDDAABBCCDDAABBCCDD";;
val s : string = "AABBCCDDAABBCCDDAABBCCDDAABBCCDDAABBCCDD"
# let s', ratio = z#codeStr s;;
val s' : string = "09<<<,>,>,>DCBAԐ5$\rI\003R@Ԑ\000\002"
val ratio : int = 30
# let s'' = z#decodeStr s';;
val s'' : string = "AABBCCDDAABBCCDDAABBCCDDAABBCCDDAABBCCDD"
# s = s'';;
- : bool = true
# let z = new zip ();;
val z : Zip.zip = <obj>
# let s = "langages de programmation, hiver de chaque année";;
val s : string = "langages de programmation, hiver de chaque année"
# let s', ratio = z#codeStr s;;
val s' : string = "1■- ■■029■Ԉ-■\018|■7■e■■■■030\005QT\003\002"
val ratio : int = 36
# let s'' = z#decodeStr s';;
val s'' : string = "langages de programmation, hiver de chaque année"
# s = s'';;
- : bool = true
# let z = new zip();;
val z : Zip.zip = <obj>
# timeRun (z#codeFile "corpus.txt") "corpus.zzz";;
- : int * float = (40, 6.2760000000000105)
# timeRun (z#decodeFile "corpus.zzz") "corpus2.txt";;
- : unit * float = ((), 0.65000000000003411)
# timeRun (file_eq "corpus.txt") "corpus2.txt";;
- : bool * float = (true, 0.13299999999998136)
# let z = new zip();;
val z : Zip.zip = <obj>
# timeRun(z#codeFiles ["corpus0.txt"; "corpus1.txt"; "corpus2.txt"]) "corpus.zzz";;
- : int * float = (40, 19.952999999999975)
# timeRun z#decodeFiles "corpus.zzz";;
- : unit * float = ((), 2.1899999999999977)
# List.for_all (Utiles.file_eq "corpus.txt") ["corpus0.txt"; "corpus1.txt"; "corpus2.txt"];;
- : bool = true
*)
