let _ = print_endline "Hello world!!!"

(** To enter toplevel:
  - go to terminal and type "utop"
  - This acts like a REPL for ocaml
  - You only need to end each line with a double semi-colon in the toplevel
*)

(** To compile:
  - Create a file called "dune" (outline executable name)
  - Create a file called "dune-project" (specify dune language)
  - run `dune build`
  - run `dune exec ./[EXECUTABLE_NAME]`
*)

let x_int = 32
let x_float = 32.
let product = x_int * x_int (* wouldn't work with x_float, as two types must be same*)
let _ = print_newline(print_int product)
let my_bool = true
let my_string = "Hello world"
let my_string_2 = ", it's me Kris!"
let my_string_concat = my_string ^ my_string_2
let _ = print_newline(print_string my_string_concat)
let x_string = string_of_int x_int
let x_string_2 = string_of_float x_float

let _ = print_newline(print_char my_string.[0])
let _ = print_newline(print_char my_string.[1])
let _ = print_newline(print_char my_string.[2])
let _ = print_newline(print_char my_string.[3])
let _ = print_newline(print_char my_string.[4])

let _ = print_newline(print_string x_string)
let _ = print_newline(print_string x_string_2)
let equals x y = x = y
let () = assert (my_bool)
let () = assert (equals x_int x_int)
let () = assert (equals x_float x_float)
let x = 32
let x_squared = x * x
let rec pow x y = if y = 0 then 1 else x * (pow x (y - 1))
let _ =  print_newline(print_int x)
let _ =  print_newline(print_int x_squared)
let _ = print_newline(print_int (pow x 2))

(** 4.9 Exercises *)
let num = 7 * (1 + 2 + 3)
let _ = print_newline(print_int num)
let string_concat = "CS " ^ string_of_int 3110 
let _ = print_newline(print_string string_concat)
let forty_two_ten = 42 * 10
let _ = print_newline(print_int forty_two_ten)
let division = 3.14 /. 2.0
let _ = print_newline(print_float division)
let rec pow_float (x: float) (y: int) : float = 
  if y = 0 then 1.
  else x *. (pow_float x (y - 1))

let _ = print_newline(print_float (pow_float 4.2 7))

