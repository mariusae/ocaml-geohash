open Printf

let mid_error_str (min, max) =
  let error = (max -. min) /. 2. in
  let mid = min +. error in
  sprintf "%.6f +/- %.6f" mid error

let () =
  match Sys.argv with
      [|_; lat_str; lon_str|] ->
        let lat = float_of_string Sys.argv.(1) in
        let lon = float_of_string Sys.argv.(2) in
        let gh = Geohash.of_lat_lon (lat, lon) in
        printf "%s\n" gh
    | [|_; geohash|] ->
      let (lat_range, lon_range) = Geohash.decode geohash in
      printf "%s, %s\n" (mid_error_str lat_range) (mid_error_str lon_range)
    | _  -> invalid_arg "usage.."
