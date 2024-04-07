(**
Copyright (c) 2024 Max Charrier.
All rights reserved.
*)

(**
  Divise une chaîne de caractère en groupe de 4 caractères.
  Nécessaire lors de la conversion en hexadécimal

  @author Max Charrier
  @param str chaîne de caractère
  @return liste de chaîne de caractère groupé par 4
*)
let rec split_string (str : string) : string list =
  let len = String.length str in
  let rec split_helper acc index =
    if index >= len then
      acc
    else
      (* Soustrait un groupe de 4 caractère à partir de l'index courant *)
      let group = String.sub str index 4 in
      (* Appel récursif en ajoutant les groupes à la liste *)
      split_helper (group :: acc) (index + 4)
  in
  (* Inversion pour obtenir l'ordre cohérent *)
  List.rev (split_helper [] 0)
;;

(**
  Convertit une chaîne de 4 bits en hexadécimal.

  @author Max Charrier
  @param bits représentation binaire sur 4 bits
  @return charactère hexadécimal
*)
let bin_to_hex (bits : string) : char =
  match bits with
  | "0000" -> '0'
  | "0001" -> '1'
  | "0010" -> '2'
  | "0011" -> '3'
  | "0100" -> '4'
  | "0101" -> '5'
  | "0110" -> '6'
  | "0111" -> '7'
  | "1000" -> '8'
  | "1001" -> '9'
  | "1010" -> 'A'
  | "1011" -> 'B'
  | "1100" -> 'C'
  | "1101" -> 'D'
  | "1110" -> 'E'
  | "1111" -> 'F'
  | _ -> failwith "Entrée non valide pour une chaîne binaire"
;;

(**
  Détermine le bit de signe du nombre flottant.

  @author Max Charrier
  @param x nombre flottant
  @return bit de sign (0 -> positif; 1 -> négatif)
*)
let sign_bit (x : float) : int =
  if x < 0. then 1 else 0
;;

(**
  Convertit un nombre entier en nombre binaire.

  @author Max Charrier
  @param n nombre entier
  @return liste de bits
*)
let int_to_binary (n : int) : int list =
  let rec aux (n : int) (acc : int list) : int list =
    if n = 0 then
      acc
    else
      (* On divise n et on ajoute son reste à la liste *)
      aux (n / 2) ((n mod 2) :: acc)
  in
  aux n []
;;

(**
  Calcul l'exposant d'un nombre flottant.

  Récursive terminale.

  @author Max Charrier
  @param x nombre flottant
  @param acc accesseur pour calculer l'exposant
  @return exposant du nombre flottant
*)
let rec exponents_bits_aux (x : float) (acc : int) : int =
  if x >= 2. then
    exponents_bits_aux (x /. 2.) (acc + 1)
  else if x < 1. then
    exponents_bits_aux (x *. 2.) (acc - 1)
  else
    acc
;;

(**
  Détermine les 8 bits d'exposant du nombre flottant.

  @author Max Charrier
  @param x nombre flottant
  @return liste de bits
*)
let exponent_bits (x : float) : int list =
  let bias : int = 127 in
  let exponent : int = exponents_bits_aux x 0 in

  int_to_binary (exponent + bias)
;;

(**
  Convertit la partie décimale d'un nombre flottant en nombre binaire.

  @author Max Charrier
  @param f nombre flottant
  @return liste de bits
*)
let fract_to_binary (f : float) : int list =
  let rec aux (f : float) (acc : int list) : int list =
    if f = 0. then
      acc
    else
      let f' = f *. 2. in (* Multiplication par 2 pour le prochain bit *)
      let digit = int_of_float f' in (* Partie entière uniquement *)
      aux (f' -. float_of_int digit) (digit :: acc)
  in
  (* Inversion pour obtenir l'ordre cohérent *)
  List.rev (aux f [])
;;

(**
  Réduit les élements de la liste à une longueur fixe.

  @author Max Charrier
  @param n longueur voulue
  @param lst liste à réduire
  @return liste réduite
*)
let take (n : int) (lst : 'a list) : 'a list =
  let rec aux n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> aux (n - 1) (x :: acc) xs
  in
  aux n [] lst
;;

(**
  Détermine les 23 bits de mantisse du nombre flottant.

  @author Max Charrier
  @param x nombre flottant
  @return liste de bits
*)
let significand_bits (x : float) : int list =
  let integer_part : int = int_of_float x in
  let integer_binary : int list = int_to_binary integer_part in

  let fractional_part : float = x -. float_of_int (integer_part) in
  let fractional_binary : int list = fract_to_binary fractional_part in

  let bits : int list = List.tl (integer_binary @ fractional_binary) in
  let total_bits = List.length bits in

  let padding = max 0 (23 - total_bits) in
  let padded_bits = bits @ (List.init padding (fun _ -> 0)) in

  if List.length padded_bits > 23 then
    take 23 padded_bits
  else
    padded_bits
;;

(**
  Fournit la représeantion binaire et hexadécimale d'un nombre flottant au format IEEE 754 simple précision.

  @author Max Charrier
  @param x nombre flottant
  @return représentation binaire et hexadécimale
*)
let float32 (x : float) : string =
  let msg : string = "IEEE 754: Encode FP32 \"" ^ string_of_float x ^ "\" -> " in

  if Float.is_infinite x then
    if x > 0. then
      msg ^ string_of_float Float.infinity
    else
      msg ^ string_of_float Float.neg_infinity
  else if Float.is_nan x then
    msg ^ string_of_float Float.nan
  else
    let abs_x : float = abs_float x in
    let float : int list =
      sign_bit x :: exponent_bits abs_x @ significand_bits abs_x
    in
    let float_str : string = String.concat "" (List.map string_of_int float) in

    let bin_groups : string list = split_string float_str in
    let hex_list : char list = List.map bin_to_hex bin_groups in
    let hex_str : string = String.of_seq (List.to_seq hex_list) in

    msg ^ float_str ^ " (" ^ hex_str ^ ")"
;;

(**
  Décode un nombre codé au format IEEE 754 simple précision en nombre réel.

  Opération inverse de `float32`.

  @author Max Charrier
  @param bits nombre binaire
  @return nombre flottant
*)
let float32_decoder (bits : string) : float =
  (* Bit de signe *)
  let sign_bit = if bits.[0] = '1' then -1.0 else 1.0 in

  (* Bits d'exposant *)
  let exponent = ref 0 in
  for i = 1 to 8 do
    exponent := !exponent * 2 + (int_of_char bits.[i] - int_of_char '0')
  done;

  (* Bits de mantisse *)
  let fraction = ref 0. in
  for i = 9 to 31 do
    fraction := !fraction +. float_of_int (int_of_char bits.[i] - int_of_char '0') *. (2. ** (-.float_of_int (i - 8)))
  done;

  (* Combinaison selon la formule, ne pas oublier de soustraire le biais *)
  sign_bit *. (2. ** (float_of_int (!exponent - 127))) *. (1. +. !fraction)
;;

(* Command-line Arguments *)
let () =
  if Array.length Sys.argv > 3 then
    failwith "IEEE 754: One number at a time."
  else if Sys.argv.(1) = "encode" then begin
    print_string (float32 (float_of_string (Sys.argv.(2))));
    print_newline ()
  end else if Sys.argv.(1) = "decode" then begin
    print_string ("IEEE 754: Decode FP32 \"" ^ Sys.argv.(2) ^ "\" -> ");
    print_float (float32_decoder Sys.argv.(2));
    print_newline ()
  end else
    failwith "IEEE 754: Please use 'encode' or 'decode' command followed by your number."
;;
