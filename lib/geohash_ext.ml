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
