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
  let msg : string = "IEEE 754 : FP32 \"" ^ string_of_float x ^ "\" -> " in

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

    msg ^ float_str
;;
