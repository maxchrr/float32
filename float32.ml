let rec split_string str =
  let len = String.length str in
  let rec split_helper acc index =
    if index >= len then
      acc
    else
      let group = String.sub str index 4 in
      split_helper (group :: acc) (index + 4)
  in
  List.rev (split_helper [] 0)
;;

let bin_to_hex (bin : string) : char =
  match bin with
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

let sign_bit (x : float) : int =
  if x < 0. then 1 else 0
;;

let int_to_binary (n : int) : int list =
  let rec aux (n : int) (acc : int list) : int list =
    if n = 0 then acc
    else aux (n / 2) ((n mod 2) :: acc)
  in
  aux n []
;;

let rec exponents_bits_aux (x : float) (acc : int) : int =
  if x >= 2. then
    exponents_bits_aux (x /. 2.) (acc + 1)
  else if x < 1. then
    exponents_bits_aux (x *. 2.) (acc - 1)
  else
    acc
;;

let exponent_bits (x : float) : int list =
  let bias : int = 127 in
  let exponent : int = exponents_bits_aux x 0 in

  int_to_binary (exponent + bias)
;;

let fract_to_binary (f : float) : int list =
  let rec aux (f : float) (acc : int list) : int list =
    if f = 0. then
      acc
    else
      let f' = f *. 2. in
      let digit = int_of_float f' in
      aux (f' -. float_of_int digit) (acc @ [digit])
  in
  aux f []
;;

let take (n : int) (lst : 'a list) : 'a list =
  let rec aux n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> aux (n - 1) (x :: acc) xs
  in
  aux n [] lst
;;

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

let float32_decoder (bits : string) : float =
  let sign_bit = if bits.[0] = '1' then -1.0 else 1.0 in
  let exponent = ref 0 in
  for i = 1 to 8 do
    exponent := !exponent * 2 + (int_of_char bits.[i] - int_of_char '0')
  done;
  let fraction = ref 0.0 in
  for i = 9 to 31 do
    fraction := !fraction +. float_of_int (int_of_char bits.[i] - int_of_char '0') *. (2.0 ** (-.float_of_int (i - 8)))
  done;
  sign_bit *. (2.0 ** (float_of_int (!exponent - 127))) *. (1.0 +. !fraction)
;;

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
