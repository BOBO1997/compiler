open KNormal

(**)
let add_common_exp env e name = (e, name)::env

(**)
(*
let rec remove_common_exp env e = 
*)

(* (KNormal.t * Id.t) list -> KNormal.t -> Id.t *)
let rec find_common_exp env e =
	match env with
	| [] -> "None"
	| (exp, name)::yd -> if exp = e then name else find_common_exp yd e

(* (KNormal.t * Id.t) list -> KNormal.t -> KNormal.t *)
let rec g env = function
	| Let ((x, t), e1, e2) ->
		(* (x, e1) を env から探し、有ったら置換、無かったら追加 *)

		(* e2 について、再帰的にgを作用させてやる *)
		
		(* letのスコープから外れたら削除 (alpha変換後なので一応これはしなくても良い) *)
	| e -> e

(* int -> KNormal.t -> KNormal.t *)
let f print_flag e =
	let a = fst (g [] e) in
	if print_flag = 1 
		then (print_string "<dump after elimsubexp>\n=================================================================================================\n";
			  print_code 0 a;
			  print_string "=================================================================================================\n\n";
			  a) 
		else a
