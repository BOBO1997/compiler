type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let rec pp_list = function
	| [] -> ""
	| [x] -> x
	| x :: xs -> x ^ " " ^ pp_list xs

(* カウンター変数 *)
let counter = ref 0

(* kNormalの時に新しい変数名を作るときに使う *)
let genid s =
	incr counter; (* カウンターをインクリメント *)
	Printf.sprintf "%s.%d" s !counter

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

(*  *)
let gentmp typ =
	incr counter;
	Printf.sprintf "T%s%d" (id_of_typ typ) !counter

(* デバッグ用 *)
let print_t = print_string
