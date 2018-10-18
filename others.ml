
	| Add (a, b) -> 
		let name = find_common_exp !env (Add (a, b)) in 
		if name = "None"
			then Add (a, b)
			else Var name
	| Sub (a, b) -> 
		let name = find_common_exp !env (Sub (a, b)) in 
		if name = "None"
			then Sub (a, b)
			else Var name
	| Mul (a, b) -> 
		let name = find_common_exp !env (Mul (a, b)) in
		if name = "None"
			then Mul (a, b)
			else Var name
	| Div (a, b) -> 
		let name = find_common_exp !env (Div (a, b)) in
		if name = "None"
			then Div (a, b)
			else Var name
	| FAdd (a, b) -> 
		let name = find_common_exp !env (FAdd (a, b)) in
		if name = "None"
			then FAdd (a, b)
			else Var name
	| FSub (a, b) -> 
		let name = find_common_exp !env (FSub (a, b)) in
		if name = "None"
			then FSub (a, b)
			else Var name
	| FMul (a, b) -> 
		let name = find_common_exp !env (FMul (a, b)) in
		if name = "None"
			then FMul (a, b)
			else Var name
	| FDiv (a, b) -> 
		let name = find_common_exp !env (FDiv (a, b)) in
		if name = "None"
			then FDiv (a, b)
			else Var name
	| Neg a -> 
		let name = find_common_exp !env (Neg a) in
		if name = "None"
			then Neg a
			else Var name
	| FNeg a -> 
		let name = find_common_exp !env (FNeg a) in
		if name = "None"
			then FNeg a
			else Var name
