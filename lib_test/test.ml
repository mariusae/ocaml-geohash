open OUnit
open Printf

let test_san_francisco () =
  let (=~) = cmp_float ~epsilon:0.000005 in
  "encode san francisco" @?
    (Geohash.of_lat_lon (37.7749295, -122.4194155) = "9q8yyk8yuv");
  let lat, lon = Geohash.decode_ "9q8yyk8yuv" in
  "decode san francisco lat" @? (lat =~ 37.7749295);
  "decode san francisco lon" @? (lon =~ -122.4194155)

let suite = [
  "san francisco" >:: test_san_francisco;
]

let _ =
  run_test_tt_main ("geohash" >::: suite)
