
type instruction = 
  | Jump of string * string
  | R of string * int * int * int
  | Branch of string * int * int * string 
  | Load of string * int * int
  | Empty

let addresses = Hashtbl.create 42 
let () = Hashtbl.add addresses "main" 0 
let pc = ref 0
