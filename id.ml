type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let rec pp_list = function
	| [] -> ""
	| [x] -> x
	| x :: xs -> x ^ " " ^ pp_list xs

(* カウンター変数 *)
let counter = ref 0

(* alpha変換用 *)
let genid s =
	if (String.get s 0) = 'I'
	then
		Printf.sprintf "%s" s
	else
		(incr counter; (* カウンターをインクリメント *)
		Printf.sprintf "%s.%d" s !counter)

(*  *)
let rec id_of_typ = function
	| Type.Unit -> "u"
	| Type.Bool -> "b"
	| Type.Int -> "i"
	| Type.Float -> "d"
	| Type.Fun _ -> "f"
	| Type.Tuple _ -> "t"
	| Type.Array _ -> "a" 
	| Type.Var _ -> assert false

(* k正規化用 *)
let gentmp typ =
	incr counter;
	Printf.sprintf "T%s%d" (id_of_typ typ) !counter

let gentmp_int value =
	Printf.sprintf "Int%d" value

(* デバッグ用 *)
let print_t = print_string

let print_l (L l) = print_string l
