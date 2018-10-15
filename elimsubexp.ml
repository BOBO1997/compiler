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
		(* (x, e1) $B$r(B env $B$+$iC5$7!"M-$C$?$iCV49!"L5$+$C$?$iDI2C(B *)

		(* e2 $B$K$D$$$F!":F5"E*$K(Bg$B$r:nMQ$5$;$F$d$k(B *)
		
		(* let$B$N%9%3!<%W$+$i30$l$?$i:o=|(B (alpha$BJQ498e$J$N$G0l1~$3$l$O$7$J$/$F$bNI$$(B) *)
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
