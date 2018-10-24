type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
type prog = Prog of fundef list * t

let rec fv = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

let rec g env known = function (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Mul(x, y) -> Mul(x, y)
  | KNormal.Div(x, y) -> Div(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
         xに自由変数がない(closureを介さずdirectに呼び出せる)
         と仮定し、knownに追加してe1をクロージャ変換してみる *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
        if S.is_empty zs then known', e1' else
        (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
        (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
         Format.eprintf "function %s cannot be directly applied in fact@." x;
         toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
        MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
      else
        (Format.eprintf "eliminating closure(s) %s@." x;
         e2') (* 出現しなければMakeClsを削除 *)
  | KNormal.App(x, ys) when S.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)

(* ここからデバッグ用print関数 *)

let rec print_indent depth =
	if depth = 0 then ()
	else (print_string ".   "; print_indent (depth - 1))

let print_t depth x =
	print_indent depth;
	Id.print_t x

let rec print_id_list fv_list =
	List.iter (fun id -> Id.print_t id; print_string ", ") fv_list

let print_t_tuple (x, t) = Id.print_t x; print_string ", "

let print_closure {entry = label; actual_fv = fv_list} =
	Id.print_t label;
	print_id_list fv_list;

let newline_flag = ref 0

let is_already_newline flag = 
	if !flag = 0 then (print_newline (); flag := 1)
	else ()

let rec print_kNormal depth expr =
	print_indent depth; newline_flag := 0;
	match expr with
	| Unit 		 -> print_string "<UNIT> "
	| Int i 	 -> print_string "<INT> "; print_string (string_of_int i)
	| Float d 	 -> print_string "<FLOAT> "; print_string (string_of_float d)
	| Neg x      -> print_string "<NEG> "; Id.print_t x
	| Add (x, y) -> print_string "<ADD> "; print_newline ();
					print_t (depth + 1) x; print_newline ();
					print_t (depth + 1) y
	| Sub (x, y) -> print_string "<SUB> "; print_newline ();
					print_t (depth + 1) x; print_newline ();
					print_t (depth + 1) y
	| Mul (x, y) -> print_string "<MUL> "; print_newline ();
					print_t (depth + 1) x; print_newline ();
					print_t (depth + 1) y
	| Div (x, y) -> print_string "<DIV> "; print_newline ();
					print_t (depth + 1) x; print_newline ();
					print_t (depth + 1) y
	| FNeg x  	 -> print_string "<FNEG> "; Id.print_t x
	| FAdd (x, y)-> print_string "<FADD> "; print_newline ();
					print_t (depth + 1)  x; print_newline ();
					print_t (depth + 1)  y
	| FSub (x, y)-> print_string "<FSUB> "; print_newline ();
					print_t (depth + 1)  x; print_newline ();
					print_t (depth + 1)  y
	| FMul (x, y)-> print_string "<FMUL> "; print_newline ();
					print_t (depth + 1)  x; print_newline ();
					print_t (depth + 1)  y
	| FDiv (x, y)-> print_string "<FDIV> "; print_newline ();
					print_t (depth + 1)  x; print_newline ();
					print_t (depth + 1)  y
	| IfEq (x, y, e1, e2)	 -> print_string "<IF> "; print_newline ();
								print_indent (depth + 1); print_string "<EQ> "; print_newline ();
								print_t (depth + 2)  x; print_newline ();
								print_t (depth + 2)  y; print_newline ();
								print_indent depth; print_string "<THEN> "; print_newline ();
								print_code (depth + 1) e1;
								print_string "<ELSE> "; print_newline ();
								print_code (depth + 1) e2
	| IfLE (x, y, e1, e2)	 -> print_string "<IF> "; print_newline ();
								print_indent (depth + 1); print_string "<LE> "; print_newline ();
								print_t (depth + 2)  x; print_newline ();
								print_t (depth + 2)  y; print_newline ();
								print_string "<THEN> "; print_newline ();
								print_code (depth + 1) e1;
								print_string "<ELSE> "; print_newline ();
								print_code (depth + 1) e2
	| Let ((x, y), e1, e2)	 -> print_string "<LET> "	 ;
								Id.print_t x	 ; print_newline ();
								print_code (depth + 1) e1; 
								print_indent depth; print_string "<IN>"; print_newline ();
								(*
								print_code (depth + 1) e2
								*)
								print_code depth e2
	| Var x 			 	 -> print_string "<VAR> "		 ; Id.print_t x
	| MakeCls ((f, t), {entry = entry; actual_fv = fv_list}, exp)
							 -> print_string "<MakeCls>";
							 	Id.print_t f; print_newline ();
								print_indent (depth + 1); print_string "<CLOSURE> ";
								print_indent (depth + 2); print_string "<entry : > "; 
								Id.print_l entry; print_newline ();
								print_indent (depth + 2); print_stirng "<antual_fv : >";
								print_id_list fv_list; print_newline ();
								print_code (depth + 1) exp
	| AppCls (x, xs) 		 -> print_string "<APPCLS> "; print_newline ();
								print_indent (depth + 1); print_string "<FUN> ";
								Id.print_t x; print_newline ();
								print_indent (depth + 1); print_string "<ARGS> ";
								List.iter (fun x -> Id.print_t x; print_string ", ") xs;
								print_string "</ARGS> "
	| AppDir (x, xs)		 -> print_string "<APPDIR> "; print_newline ();
								print_indent (depth + 1); print_string "<FUN> ";
								Id.print_l x; print_newline ();
								print_indent (depth + 1); print_string "<ARGS> ";
								print_id_list xs;
								priint_string "</ARGS>"
	| Tuple xs  		 	 -> print_string "<TUPLE> "	 ; print_newline ();
								List.iter (fun x -> Id.print_t x; print_string ", ") xs
	| LetTuple (xts, y, e)	 -> print_string "<LETTUPLE> " ;
								print_indent (depth + 1); print_string "<TUPLE> ";
								List.iter print_t_tuple xts;
								print_string "</TUPLE>"; print_newline ();
								print_t (depth + 1)  y; print_newline ();
								print_code (depth + 1)  e
	| Get (x, y)     		 -> print_string "<GET> " ; print_newline ();
								print_t (depth + 1)  x; print_newline ();
								print_t (depth + 1)  y
	| Put (x, y, z) 		 -> print_string "<PUT> " ; print_newline ();
								print_t (depth + 1)  x; print_newline ();
								print_t (depth + 1)  y; print_newline ();
								print_t (depth + 1)  z
	| ExtArray x 			 -> print_string "<EXTARRAY> " ; 
								Id.print_t x
	| ExtFunApp (x, ys) 	 -> print_string "<EXTFUNAPP> "; print_newline ();
								print_indent (depth + 1); print_string "<FUN> ";
								Id.print_t x; print_newline ();
								print_indent (depth + 1); print_string "<ARGS> ";
								List.iter (fun y -> Id.print_t y; print_string ", ") ys;
								print_string "</ARGS> "

and print_code depth expr = print_kNormal depth expr; is_already_newline newline_flag

let f print_flag e =
  toplevel := [];
  let e' = g M.empty S.empty e in
    if print_flag = 1
	  then (print_string "<dump after closure>\n=================================================================================================\n";
	  		KNormal.print_code 0 e; 
			print_string "=================================================================================================\n\n";
			Prog(List.rev !toplevel, e'))
      else Prog(List.rev !toplevel, e')
