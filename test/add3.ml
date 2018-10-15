let x = 1 in
let y = 2 in
let a = x + y in
let b = x + y in
let c = 
	let y = x + y in
	y + b
in
let x = a in
let e = x + y in
print_int e
