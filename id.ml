type t = string (* �ϐ��̖��O (caml2html: id_t) *)
type l = L of string (* �g�b�v���x���֐���O���[�o���z��̃��x�� (caml2html: id_l) *)

let rec pp_list = function
	| [] -> ""
	| [x] -> x
	| x :: xs -> x ^ " " ^ pp_list xs

(* �J�E���^�[�ϐ� *)
let counter = ref 0

(* kNormal�̎��ɐV�����ϐ��������Ƃ��Ɏg�� *)
let genid s =
	incr counter; (* �J�E���^�[���C���N�������g *)
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

(* �f�o�b�O�p *)
let print_t = print_string
