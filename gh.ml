open Printf

let () =
  if Array.length Sys.argv != 3
  then invalid_arg "usage..";

  let lat = float_of_string Sys.argv.(1) in
  let lon = float_of_string Sys.argv.(2) in
  let gh = Geohash.of_lat_lon (lat, lon) in
  printf "%s\n" gh
