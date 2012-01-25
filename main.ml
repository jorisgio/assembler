 
open Ast
open Parser 

let rec bl_of_int li = function
  | 0 ->  li
  | k -> let b = if (k mod 2) = 0 then false else true in
	 bl_of_int (b::li)  (k / 2)

(* écrit un nombre sous la forme d'une liste booléene de taille size 
   poid fort à droite *)
let blOfInt size  n = 
  let na = abs n in
   let s = (n < 0 ) in
   let li = bl_of_int [] na in
   let rec comp l = function 
     | 0 -> l
     | k -> comp (false::l) (k-1)
   in
   let li = (comp li (size - (List.length li))) in
   let li = if s 
     then (
       let li = List.map (fun elt -> not elt) li in
       fst (List.fold_right (fun elt (acc,r) -> (((elt or r) && (not (elt && r)))::acc),(elt && r)) li ([],true) ))
     else
       li
   in
   List.rev li
   
	
  let jump = [false;true;true] 
  let r_type = [false;false;false;false;false;false]
  let load = [false;false;false;true;false;false]
  let branch = [false;false;false;false;false;true]
  let empty5 = [false;false;false;false;false]
    
  let assemble_branch rs rt addr instr =
    let rs = blOfInt 5 rs in
    let rt = blOfInt 5 rt in 
    let addr =
      try 
	Hashtbl.find addresses addr 
      with Not_found -> failwith "undefined label"
    in
    match instr with 
      | "beq" -> (blOfInt 16 (addr - !pc)) @ (rt @ (rs @ branch))
      | _ -> failwith "Not implemented" 
	
  let assemble_jump addr = function
    | "j" -> 
      let addr = 
	try 
	  Hashtbl.find addresses addr 
	with Not_found -> failwith "undefined label"
      in
      (blOfInt 29 addr) @ jump
    | _ -> failwith "Not implemented"

  let assemble_r_types rs rt rd instr= 
    if rs > 32 or rd > 32 or rt > 32 then failwith "bad register";
    let rs = blOfInt 5 rs in
    let rd = blOfInt 5 rd in
    let rt = blOfInt 5 rt in
    let li =   empty5 @ (rd @ (rt @ (rs @ r_type))) in
    let funct = match instr with
      | "add" -> [false;true;false;false;false;false]
      | "sub" -> [false;true;false;true;false;false]
      | "and" -> [false;false;false;false;false;false]
      | "nand" -> [false;false;false;true;false;false]
      | "or" -> [true;false;false;false;false;false]
      | _ -> failwith "not implemented"
    in
    funct @ li 

    let assemble_load rd num instr= 
      let rd = blOfInt 5 rd in
      match instr with
	| "li" -> (blOfInt 16 num ) @ (rd @ (empty5 @ load))
	| _ -> failwith "Not implemented"


    let toBin li = 
      let atom acc instr = 
	match instr with
	  | R(s,rs,rt,rd) -> incr pc; (assemble_r_types rs rt rd s)::acc
	  | Load(s, rd,n) -> incr pc; (assemble_load rd n s)::acc
	  | Jump(s,addr) -> incr pc; (assemble_jump addr s)::acc
	  | Branch(s,rs,rt,addr) -> incr pc; (assemble_branch rs rt addr s)::acc
	  | Empty -> acc
	
      in
      List.fold_left atom  []  li

let ifile = ref "" 
let set_file f s = f := s
   
let prettyPrint instr = 
  List.iter (fun elt -> 
    let bit = 
      match elt with
	| true -> 1
	| false -> 0 
    in
    Printf.printf "%d" bit) instr 
  
let usage = Printf.sprintf "Usage : %s  <file.s>" Sys.argv.(0) 
let () = 
  Arg.parse [] (set_file ifile) usage;
  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  let instrList = Parser.prog Asm.instr buf in
  close_in f;
  pc := 0 ;
  let instrList = toBin instrList in
  let binary  = Array.of_list instrList in
  Array.iter (fun i -> prettyPrint i; Printf.printf "\n" ) binary;
  Printf.printf "\n%d\n" (Array.length binary); 
  let f = open_out "a.out" in
  output_value f binary;
  close_out f;
  
