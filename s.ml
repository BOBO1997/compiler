(* customized version of Set *)
(* 主に自由変数を格納する *)
module S =
  Set.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include S

let of_list l = List.fold_left (fun s e -> add e s) empty l
