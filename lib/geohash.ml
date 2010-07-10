module Enum = struct
  include Enum

  let from_ init f =
    let state = ref init in
    let iter () =
      let (state', res) = f !state in
      state := state';
      res in
    from iter    

  let take n e =
    let rec collect i =
      if i == n then []
      else begin
        match get e with
          | None -> []
          | Some x -> x :: collect (i + 1)
      end in
    let xs = collect 0 in
    from_ xs begin function
      | [] -> raise No_more_elements
      | x :: xs' -> (xs', x)
    end
end

module String = struct
  include String

  let to_list s =
    let rec go i l = if i == 0 then l else go (i - 1) (s.[i - 1] :: l) in
    go (length s) []

  let of_list l =
    let len = List.length l in
    let s = make len ' ' in
    let rec go i = function
      | [] -> s
      | c :: cs -> s.[i] <- c; go (i + 1) cs in
    go 0 l

  let of_enum e =
    let l = Enum.fold (fun elem acc -> elem :: acc) [] e in
    of_list (List.rev l)
end

(* base32 alphabet *)
let charset = "0123456789bcdefghjkmnpqrstuvwxyz"
(* TODO: this could even be expanded to the bitstrings directly. *)
let charset_reverse =
  let max = (Char.code 'z') in
  let a = Array.create (max + 1) (-1) in
  for i = 0 to max do
    let char = Char.chr i in
    if String.contains charset char
    then a.(i) <- String.index charset char
  done;
  a

let bits_of_b32 i =
  [ i land 16 == 16;
    i land  8 ==  8;
    i land  4 ==  4;
    i land  2 ==  2;
    i land  1 ==  1 ]

let bitstring_of_float (a, b) value =
  Enum.from_ (a, b) begin fun (a, b) ->
    let mid = a +. ((b -. a) /. 2.) in
    if value > mid
    then ((mid, b), true)
    else ((a, mid), false)
  end

let b32string_of_bitstring bitstring =
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
    charset.[i32]
  end

let interleave x y =
  Enum.from_ (x, y) begin fun (x, y) ->
    match Enum.get x with
        Some(x') -> ((y, x), x')
      | None -> raise Enum.No_more_elements
  end

let deinterleave xs =
  let rec go xs ys zs =
    match xs with
        y :: z :: xs -> go xs (y :: ys) (z :: zs)
      | [] -> (List.rev ys, List.rev zs) 
      | _ -> invalid_arg "list not of even length!" in
  go xs [] []

let rec unbitstring (a, b) = function
  | x :: xs ->
    let mid = a +. ((b -. a) /. 2.) in
    if x then unbitstring (mid, b) xs
    else      unbitstring (a, mid) xs
  | [] -> (a, b)

let of_lat_lon ?(prec = 10) (lat, lon) =
  let lonbits = bitstring_of_float (-180., 180.) lon in
  let latbits = bitstring_of_float (-90., 90.) lat in
  let bits = interleave lonbits latbits in
  let b32s = b32string_of_bitstring bits in
  String.of_enum (Enum.take prec b32s)

let parse gh =
  let l = String.to_list gh in
  let vals = List.map (fun c -> charset_reverse.(Char.code c)) l in
  let bitss = List.map bits_of_b32 vals in
  let bits = List.concat bitss in
  let (lon_bits, lat_bits) = deinterleave bits in
  (unbitstring (-90., 90.) lat_bits, unbitstring (-180., 180.) lon_bits)
