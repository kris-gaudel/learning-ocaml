let _ = print_endline "Chapter 4 Exercises"

let cube (x: float) : float = 
  x *. x *. x
let _ = assert(cube 2.0 = 8.0)
let _ = assert(cube (-2.0) = -8.0)
let _ = assert(cube 0.0 = 0.0)
let _ = assert(cube 1.0 = 1.0)
let _ = print_endline "cube tests passed"

let sign (x: float) : float = 
  if x > 0.0 then 1.0
  else if x < 0.0 then -1.0
  else 0.0

let _ = assert(sign 2.0 = 1.0)
let _ = assert(sign (-2.0) = -1.0)
let _ = assert(sign 0.0 = 0.0)
let _ = print_endline "sign tests passed"

let radius (r: float) : float = 
  if r <= 0.0 then 0.0
  else r *. r *. 3.1415926535897932384

let _ = assert(radius 2.0 = 12.566370614359172464)
let _ = assert(radius (-2.0) = 0.0)
let _ = assert(radius 0.0 = 0.0)
let _ = print_endline "radius tests passed"

let rec sqrt_recurse (x: float) (guess: float) : float = 
  if abs_float(guess *. guess -. x) < 0.00001 then guess
  else sqrt_recurse x ((guess +. x /. guess) /. 2.0)

let my_sqrt (x: float) : float = 
  if x < 0.0 then 0.0
  else sqrt_recurse x 1.0

let rms (x: float) (y: float) : float = 
  my_sqrt((x *. x +. y *. y) /. 2.0)

let within_range (result: float) (lower: float) (upper: float): bool = 
  result >= lower && result <= upper
let _ = assert(within_range(rms 1.0 5.0) 3.6 3.61)
let _ = assert(within_range(rms 1.0 1.0) 1.0 1.1)
let _ = assert(within_range(rms (-1.0) (-31.0)) 21.92 21.94)
(* let _ = assert(within_range(rms 1.0 1.0) 1.0 1.1) *)
let _ = print_endline "rms tests passed"

let valid_date (d: int) (m: string): bool = 
  if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec" then d >= 1 && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then d >= 1 && d <= 30
  else if m = "Feb" then d >= 1 && d <= 28
  else false
let _ = assert(valid_date 1 "Jan")
let _ = assert(valid_date 31 "Jan")
let _ = assert(valid_date 32 "Jan" = false)
let _ = print_endline "date tests passed"

let rec fib (n: int) : int = 
  if n = 0 then 1
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2)

let _ = assert(fib 0 = 1)
let _ = assert(fib 1 = 1)
let _ = assert(fib 2 = 2)
let _ = assert(fib 3 = 3)
let _ = assert(fib 4 = 5)
let _ = assert(fib 10 = 89)
let _ = print_endline "fib tests passed"

let rec h (n : int) (pp : int) (p : int) : int = 
  if n = 1 then p
  else h (n - 1) p (pp + p)

let fib_fast (n: int) : int =
  if n = 0 then 0
  else h n 0 1
let _ = assert(fib_fast 0 = 0)
let _ = assert(fib_fast 50 = 12586269025)
let _ = print_endline "fib_fast tests passed"

let divide (numerator: float) (denominator: float) : float = 
  if denominator = 0.0 then 0.0
  else numerator /. denominator

let _ = assert(divide(divide (divide 16. 2.) 2.) 2. = 2.)
let _ = assert(divide(divide (divide 16. 2.) 0.) 2. = 0.)
let _ = print_endline "divide tests passed"

let (+/.) (x: float) (y: float) : float = 
  (x +. y) /. 2.
let _ = assert(2.0 +/. 4.0 = 3.0)
let _ = assert(2.0 +/. 2.0 = 2.0)
let _ = print_endline "+/. tests passed"