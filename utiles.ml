module Utiles =
struct
  (* Fonction pouvant être utile *)
  (* --------------------------- *)
  (* ('a -> 'b) -> 'a -> 'b * float *)
  let timeRun f x =
    let time1 = Sys.time() in
    let r = f x in
    let time2 = Sys.time() in
    r,time2 -. time1
               
  (* Partie consacrée à la manipulation de chaines de caractères *)
  (* ----------------------------------------------------------- *)
  (* string -> char list *)
  let explode = function 
    | "" -> []
    | s -> 
      let rec loop acc = function
        | 0 -> s.[0] :: acc
        | x -> loop (s.[x] :: acc) (x - 1)
      in
      loop [] (String.length s - 1)
 
  (* char list -> string *)
  let implode l = 
    List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" l  
  
  (* Partie consacrée à la manipulation de listes *)
  (* -------------------------------------------- *)
  (* int -> 'a list -> 'a list *) 
  let rec drop n = function 
    | [] -> []
    | (_::r) as l -> if n <= 0 then l else drop (n-1) r

  (* int -> 'a list -> 'a list *) 
  let rec take n l = 
    let rec aux n l acc = match l with
      | [] -> acc
      | x::r -> if n <= 0 then acc else aux (n-1) r (acc@[x])
    in 
    aux n l []

  (* Partie consacrée à la manipulation de «Stream» *)
  (* ---------------------------------------------- *)
  (* 'a' stream -> ('a * int) list *)
  let lfreq in_stream = 
    let freq_hash = Hashtbl.create 255 in
    Stream.iter 
      (fun elt -> 
         if Hashtbl.mem freq_hash elt 
         then
           Hashtbl.replace freq_hash elt (Hashtbl.find freq_hash elt + 1)
         else
           Hashtbl.add freq_hash elt 1
      ) in_stream;
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) freq_hash []

  (* 'a Stream.t -> 'a -> 'a list *)
  let stream_next_until in_stream c =
    let rec aux l =
      match Stream.peek in_stream with
      | Some c' when c = c' -> l
      | Some c' -> aux (l@[Stream.next in_stream])
      | None -> l 
    in
    aux []

  (* int -> 'a Stream.t -> 'a list *)
  let stream_npeek n in_stream =
      let l = Stream.npeek n in_stream in
      for i=1 to List.length l do Stream.junk in_stream done;
      l

  (* 'a Stream.t -> 'a Stream.t -> bool *)
  let stream_eq in_stream1 in_stream2 =
    let rec aux in_stream1 in_stream2 =
      match Stream.peek in_stream1, Stream.peek in_stream2 with
        | None, None -> true
        | None, _ | _, None -> false
        | _ -> 
          if Stream.next in_stream1 = Stream.next in_stream2 then 
            aux in_stream1 in_stream2
          else
            false
    in
    aux in_stream1 in_stream2

  (* Partie consacrée à la manipulation de fichiers *)
  (* ---------------------------------------------- *)
  (* string -> string -> bool *)
  let file_eq in_file1 in_file2 = 
    let in_ch1 = open_in_bin in_file1 in
    let in_ch2 = open_in_bin in_file2 in
    let char_stream1 = Stream.of_channel in_ch1 in
    let char_stream2 = Stream.of_channel in_ch2 in
    let b = stream_eq char_stream1 char_stream2 in
    close_in in_ch1;
    close_in in_ch2;
    b

  (* Partie consacrée aux fréquences usuelles de caractères dans des textes Fr *)
  (* ------------------------------------------------------------------------- *)
  (* Données établies à partir du fichier «corpus.txt» pris du site Web du projet 
     Gutenberg; plus précisément, de ce fichier:
     https://www.gutenberg.org/ebooks/36987
     Le calcul a été fait grâce à la fonction lfreq
  *)
  let corpus_lfreq = 
    [('\135', 1); ('\177', 1); ('+', 2); ('<', 4); ('\142', 4); ('\197', 4);
     ('=', 4); ('\147', 4); ('#', 4); ('\175', 5); ('>', 5); ('\130', 6);
     ('\157', 10); ('\156', 11); ('Z', 17); ('@', 20); ('Y', 20); (';', 20);
     (']', 21); ('[', 21); ('!', 28); ('&', 28); ('/', 29); ('%', 30);
     ('X', 31); ('\185', 33); ('K', 43); ('Q', 53); ('\180', 57); ('\162', 62);
     ('\137', 68); ('\174', 72); ('V', 72); ('?', 76); ('H', 98);
     ('\167', 121); ('4', 142); ('W', 148); ('J', 158); ('"', 173); ('6', 205);
     ('7', 207); ('8', 208); ('3', 216); (':', 217); ('z', 223); ('5', 229);
     ('N', 239); ('F', 254); ('G', 254); ('T', 259); ('O', 266); ('U', 272);
     ('R', 287); ('\170', 296); ('\171', 306); ('I', 346); ('w', 362);
     ('B', 376); ('\187', 394); ('D', 409); ('E', 412); ('M', 414); ('k', 466);
     ('S', 499); ('A', 535); ('-', 607); ('\194', 614); ('(', 681); (')', 681);
     ('C', 686); ('P', 710); ('j', 748); ('9', 787); ('\n', 788); ('\r', 788);
     ('2', 827); ('1', 838); ('\160', 942); ('L', 943); ('y', 977);
     ('\168', 1033); ('\'', 1123); ('x', 1236); ('\153', 1773); ('\226', 1794);
     ('\128', 1827); ('h', 2085); ('0', 2108); ('q', 2338); ('.', 2394);
     ('f', 2434); ('g', 2704); ('b', 3010); (',', 3693); ('v', 3771);
     ('\169', 5912); ('m', 7105); ('p', 7440); ('\195', 8732); ('c', 8835);
     ('d', 10078); ('l', 14287); ('u', 14680); ('o', 14796); ('a', 17154);
     ('r', 18539); ('t', 19324); ('n', 19638); ('s', 20192); ('i', 20402);
     ('e', 39314); (' ', 51945)]

end
