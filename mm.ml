(* 共通部分式削除で使う集合 *)

module MM =
	Map.Make
		(struct
			type t = KNormal.t
			let compare a b = 1
		end)
include MM

(*
let of_list l = List.fold_left (fun s e -> add e s) empty l
*)
