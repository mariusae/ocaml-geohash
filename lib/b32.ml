open Geohash_ext

let alphabet = "0123456789bcdefghjkmnpqrstuvwxyz"
let alphabet' =
  let max = int_of_char 'z' in
  let a = Array.create (max + 1) (-1) in
  for i = 0 to max do
    let char = char_of_int i in
    if String.contains alphabet char
    then a.(i) <- String.index alphabet char
  done;
  a

let bits_of_ord i =
  [ i land 16 == 16;
    i land  8 ==  8;
    i land  4 ==  4;
    i land  2 ==  2;
    i land  1 ==  1 ]

let of_bitstring bitstring =
  Enum.from begin fun () ->
    let get () = match Enum.get bitstring with
        Some true -> 1
      | Some false -> 0
      | None -> raise Enum.No_more_elements in
    let b0 = get () lsl 4 in
    let b1 = get () lsl 3 in
    let b2 = get () lsl 2 in
    let b3 = get () lsl 1 in
    let b4 = get () in
    let i32 = b0 lor b1 lor b2 lor b3 lor b4 in
    alphabet.[i32]
  end

let decode_to_bitstring b32 =
  let l = String.to_list b32 in
  let vals = List.map (fun c -> alphabet'.(int_of_char c)) l in
  let bitss = List.map bits_of_ord vals in
  let bits = List.concat bitss in
  bits
