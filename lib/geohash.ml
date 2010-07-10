open Geohash_ext

let bitstring_of_float (a, b) value =
  Enum.from_ (a, b) begin fun (a, b) ->
    let mid = a +. ((b -. a) /. 2.) in
    if value > mid
    then ((mid, b), true)
    else ((a, mid), false)
  end

let rec range_of_bitstring (a, b) = function
  | x :: xs ->
    let mid = a +. ((b -. a) /. 2.) in
    if x then range_of_bitstring (mid, b) xs
    else      range_of_bitstring (a, mid) xs
  | [] -> (a, b)

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

let of_lat_lon ?(prec = 10) (lat, lon) =
  let lonbits = bitstring_of_float (-180., 180.) lon in
  let latbits = bitstring_of_float (-90., 90.) lat in
  let bits = interleave lonbits latbits in
  let b32s = B32.of_bitstring bits in
  String.of_enum (Enum.take prec b32s)

let decode gh =
  let bitstring = B32.decode_to_bitstring gh in
  let (lon_bits, lat_bits) = deinterleave bitstring in
  (range_of_bitstring (-90., 90.) lat_bits,
   range_of_bitstring (-180., 180.) lon_bits)
