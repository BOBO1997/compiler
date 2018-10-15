type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec print_indent depth =
	if depth = 0 then ()
	else (print_string ".   "; print_indent (depth - 1))

let print_t depth x =
	print_indent depth;
	Id.print_t x;
	print_newline ()

let print_t_tuple (x, t) = Id.print_t x; print_string ", "

let newline_flag = ref 0

let is_already_newline flag = 
	if !flag = 0 then (print_newline (); flag := 1)
	else ()

let rec print_syntax depth expr =
	print_indent depth; newline_flag := 0;
	match expr with
	| Unit 		 -> print_string "<UNIT> "
	| Bool b 	 -> print_string "<BOOL> "; print_string (string_of_bool b)
	| Int i 	 -> print_string "<INT> "	; print_string (string_of_int i)
	| Float d 	 -> print_string "<FLOAT "; print_string (string_of_float d)
	| Not x      -> print_string "<NOT> "	; print_newline ();
					print_code (depth + 1) x
	| Neg x      -> print_string "<NEG> "	; print_newline (); 
					print_code (depth + 1) x
	| Add (x, y) -> print_string "<ADD> "	; print_newline ();
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| Sub (x, y) -> print_string "<SUB> "	; print_newline ();
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| Mul (x, y) -> print_string "<MUL> "	; print_newline ();
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| Div (x, y) -> print_string "<DIV> "	; print_newline ();
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| FNeg x  	 -> print_string "<FNEG> "	; print_newline ();
					print_code (depth + 1) x
	| FAdd (x, y)-> print_string "<FADD> "	; print_newline (); 
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| FSub (x, y)-> print_string "<FSUB> "	; print_newline (); 
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| FMul (x, y)-> print_string "<FMUL> "	; print_newline (); 
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| FDiv (x, y)-> print_string "<FDIV> "	; print_newline (); 
					print_code (depth + 1) x; 
					print_code (depth + 1) y
	| Eq (x, y)	 -> print_string "<EQ>"		; print_newline ();
					print_code (depth + 1) x;
					print_code (depth + 1) y
	| LE (x, y)	 -> print_string "<LE>"		; print_newline ();
					print_code (depth + 1) x;
					print_code (depth + 1) y
	| If (x, e1, e2)	 	 -> print_string "<IF>"; print_newline ();
								print_code (depth + 1)  x;
								print_indent depth; print_string "<THEN>"; print_newline ();
								print_code (depth + 1) e1;
								print_indent depth; print_string "<ELSE>"; print_newline ();
								print_code (depth + 1) e2
	| Let ((x, y), e1, e2)	 -> print_string "<LET> "	 ;
								Id.print_t x    		 ; print_newline ();
								print_code (depth + 1) e1; 
								print_indent depth; print_string "<IN>"; print_newline ();
								(*print_code (depth + 1) e2*)
								print_code depth e2
	| Var x					 -> print_string "<VAR> "	 ; Id.print_t x;
	| LetRec ({name = (x, t); args = yts; body = e1}, e2)
							 -> print_string "<LETREC> " ; 
								Id.print_t x    		 ; print_newline ();
								print_indent (depth + 1); print_string "<ARGS> ";
								List.iter print_t_tuple yts;
								print_string " </ARGS>"; print_newline ();
								print_code (depth + 2) e1;
								print_indent depth; print_string "<IN>"; print_newline ();
								(*print_code (depth + 1) e2*)
								print_code depth e2
	| App (x, xs) 		 	 -> print_string "<APP> "	 ; print_newline ();
								print_code (depth + 1)  x;
								List.iter (print_code (depth + 1)) xs
	| Tuple xs  		 	 -> print_string "<TUPLE> "	 ; print_newline ();
								List.iter (print_code (depth + 1)) xs
	| LetTuple (xts, y, e) -> print_string "<LETTUPLE> " ; print_newline ();
								List.iter print_t_tuple xts;
								print_code (depth + 1)  y;
								print_indent depth; print_string "<IN>"; print_newline ();
								(*print_code (depth + 1)  e*)
								print_code depth e
	| Array (x, y)     	 	 -> print_string "<ARRAY> "	 ; print_newline ();
								print_code (depth + 1)  x;
								print_code (depth + 1)  y
	| Get (x, y)     		 -> print_string "<GET> "	 ; print_newline ();
								print_code (depth + 1)  x;
								print_code (depth + 1)  y
	| Put (x, y, z) 		 -> print_string "<PUT> "	 ; print_newline ();
								print_code (depth + 1)  x;
								print_code (depth + 1)  y;
								print_code (depth + 1)  z

and print_code depth expr = print_syntax depth expr; is_already_newline newline_flag
