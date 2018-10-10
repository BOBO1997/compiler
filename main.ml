let limit = ref 1000
let kNormal_flag = ref 0
let alpha_flag = ref 0
let beta_flag = ref 0
let closure_flag = ref 0



let rec iter n e = (* ��Ŭ�������򤯤꤫���� (caml2html: main_iter) *)
	Format.eprintf "iteration %d@." n;
	if n = 0 then e else
	let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f !beta_flag e)))) in
	if e = e' then e else
	iter (n - 1) e'

let lexbuf outchan l = (* �Хåե��򥳥�ѥ��뤷�ƥ����ͥ�ؽ��Ϥ��� (caml2html: main_lexbuf) *)
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
												 (Typing.f
														(Parser.exp Lexer.token l)))))))))
	with
	| Parsing.Parse_error -> print_string "Parsing Error ...\n"

let string s = lexbuf stdout (Lexing.from_string s) (* ʸ����򥳥�ѥ��뤷��ɸ����Ϥ�ɽ������ (caml2html: main_string) *)

let file f = (* �ե�����򥳥�ѥ��뤷�ƥե�����˽��Ϥ��� (caml2html: main_file) *)
	let inchan = open_in (f ^ ".ml") in
	let outchan = open_out (f ^ ".s") in
	try
		lexbuf outchan (Lexing.from_channel inchan);
		close_in inchan;
		close_out outchan;
	with e -> (close_in inchan; close_out outchan; raise e)

let () = (* �������饳��ѥ���μ¹Ԥ����Ϥ���� (caml2html: main_entry) *)
	let files = ref [] in
	Arg.parse
		[("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
		 ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
		 ("-knormal", Arg.Unit(fun () -> kNormal_flag := 1), "dump code before kNormal");
		 ("-kNormal", Arg.Unit(fun () -> kNormal_flag := 1), "dump code before kNormal");
		 ("-alpha", Arg.Unit(fun () -> alpha_flag := 1), "dump code after alpha");
		 ("-beta", Arg.Unit(fun () -> beta_flag := 1), "dump code after beta");
		 ("-closure", Arg.Unit(fun () -> closure_flag := 1), "dump code after closure")
		 ]
		(fun s -> files := !files @ [s])
		("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
		 Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
	List.iter
		(fun f -> ignore (file f))
		!files
