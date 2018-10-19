let limit = ref 1000
let syntax_flag = ref 0
let kNormal_flag = ref 0
let cse_flag = ref 0
let alpha_flag = ref 0
let beta_flag = ref 0
let inline_flag = ref 0
let closure_flag = ref 0

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
	Format.eprintf "iteration %d@." n;
	if n = 0 then e else
	let e' = 
			(Elim.f 
				(ConstFold.f 
					(Inline.f !inline_flag 
						(Assoc.f 
							(Beta.f !beta_flag 
								(ComSubexpElim.f !cse_flag
									e)))))) in
	if e = e' then e else
	iter (n - 1) e'

let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
	Id.counter := 0;
	Typing.extenv := M.empty;
	try
	Emit.f outchan
		(RegAlloc.f
			(Simm.f
				(Virtual.f
					(Closure.f !closure_flag
						(iter !limit
							(Alpha.f !alpha_flag
								(KNormal.f !kNormal_flag
									(Typing.f !syntax_flag
										(Parser.exp Lexer.token l)))))))))
	with
	| Parsing.Parse_error -> print_string "Parsing Error ...\n"

let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
	let inchan = open_in (f ^ ".ml") in
	let outchan = open_out (f ^ ".s") in
	try
		lexbuf outchan (Lexing.from_channel inchan);
		close_in inchan;
		close_out outchan;
	with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
	let files = ref [] in
	Arg.parse
		[("-inline_size", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
		 ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
		 ("-syntax", Arg.Unit(fun () -> syntax_flag := 1), "dump code after type checking");
		 ("-knormal", Arg.Unit(fun () -> kNormal_flag := 1), "dump code after kNormal");
		 ("-kNormal", Arg.Unit(fun () -> kNormal_flag := 1), "dump code after kNormal");
		 ("-alpha", Arg.Unit(fun () -> alpha_flag := 1), "dump code after alpha");
		 ("-cse", Arg.Unit(fun () -> cse_flag := 1), "dump code after ComSubexpElim");
		 ("-beta", Arg.Unit(fun () -> beta_flag := 1), "dump code after beta");
		 ("-inline", Arg.Unit(fun () -> inline_flag := 1), "dump code after inline");
		 ("-closure", Arg.Unit(fun () -> closure_flag := 1), "dump code before closure")
		 ]
		(fun s -> files := !files @ [s])
		("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
		 Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
	List.iter
		(fun f -> ignore (file f))
		!files
