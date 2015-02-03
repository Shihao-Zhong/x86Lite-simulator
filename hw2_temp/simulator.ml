(* X86lite Simulator *)
(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)
open X86
  
(* simulator machine state -------------------------------------------------- *)
let mem_bot = 0x400000L
  
(* lowest valid address *)
let mem_top = 0x410000L
  
(* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
  
let nregs = 17
  
(* including Rip *)
let ins_size = 4L
  
(* assume we have a 4-byte encoding *)
let exit_addr = 0xfdeadL
  
(* halt when m.regs(%rip) = exit_addr *)
(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault
  
(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly four consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next three bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsB0 (Decq,  [~%Rdi])
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
*)
type sbyte =
  | InsB0 of ins
  | (* 1st byte of an instruction *)
  InsFrag
  | (* 2nd, 3rd, or 4th byte of an instruction *)
  Byte of char

(* non-instruction byte *)
(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool; mutable fs : bool; mutable fz : bool }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags; regs : regs; mem : mem }

(* simulator helper functions ----------------------------------------------- *)
(* The index of a register in the regs array *)
let rind : reg -> int =
  function
  | Rip -> 16
  | Rax -> 0
  | Rbx -> 1
  | Rcx -> 2
  | Rdx -> 3
  | Rsi -> 4
  | Rdi -> 5
  | Rbp -> 6
  | Rsp -> 7
  | R08 -> 8
  | R09 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15
  
(* Helper functions for reading/writing sbytes *)
(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i : int64) : sbyte list = let open Char
  in let open Int64
    in
      List.map
        (fun n ->
           Byte ((((shift_right i n) |> (logand 0xffL)) |> to_int) |> chr))
        [ 0; 8; 16; 24; 32; 40; 48; 56 ]
  
(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs : sbyte list) : int64 = let open Char
  in let open Int64
    in
      let f b i =
        match b with
        | Byte c -> logor (shift_left i 8) ((c |> code) |> of_int)
        | _ -> 0L
      in List.fold_right f bs 0L
  
(* Convert a string to its sbyte representation *)
let sbytes_of_string (s : string) : sbyte list =
  let rec loop acc =
    function | i when i < 0 -> acc | i -> loop ((Byte s.[i]) :: acc) (pred i)
  in (loop [ Byte '\000' ]) @@ ((String.length s) - 1)
  
(* Serialize an instruction to sbytes *)
let sbytes_of_ins ((op, args) : ins) : sbyte list =
  let check =
    function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 ((Lbl _), _) ->
        invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in (List.iter check args; [ InsB0 (op, args); InsFrag; InsFrag; InsFrag ])
  
(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list =
  function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"
  
(* It might be useful to toggle printing of intermediate states of your 
   simulator. *)
let debug_simulator = ref false
  
(* Interpret a condition code with respect to the given flags. *)
let interp_cnd { fo = fo; fs = fs; fz = fz } : cnd -> bool =
  fun x ->
    begin match x with
    | Eq -> fz
    | Neq -> not fz
    | Lt -> fs <> fo
    | Le -> (fs <> fo) || fz
    | Gt -> not ((fs <> fo) || fz)
    | Ge -> not (fs <> fo)
    end
    
(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr : quad) : int option =
  if (Int64.compare addr mem_top) > 0
  then None
  else
    if (Int64.compare addr mem_bot) < 0
    then None
    else Some (Int64.to_int (Int64.sub addr 0x400000L))
  
let (* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
  calculate_ind (ol : operand list) (ind : int) (m : mach) : int64 =
  let operand = List.nth ol ind
  in
    match operand with
    | Ind1 i1 ->
        (match i1 with | Lit l -> l | Lbl l -> failwith "Unresolved Label!")
    | Ind2 i2 -> m.regs.(rind i2)
    | Ind3 (i, r) ->
        (match i with
         | Lit l -> Int64.add m.regs.(rind r) l
         | Lbl l -> failwith "Unresolved Label!")
    | _ -> failwith "Need to have operand of type Ind"
  
let store_data_helper (i : int64) (data : int64) (m : mach) : unit =
  let addr_opt = map_addr i in
  let addr =
    match addr_opt with | Some x -> x | None -> raise X86lite_segfault in
  let end_opt = map_addr (Int64.add i 7L) in
  let end_addr =
    match end_opt with | Some x -> x | None -> raise X86lite_segfault in
  let sbl : sbyte list = sbytes_of_int64 data
  in
    (m.mem.(addr) <- List.nth sbl 0;
     m.mem.(addr + 1) <- List.nth sbl 1;
     m.mem.(addr + 2) <- List.nth sbl 2;
     m.mem.(addr + 3) <- List.nth sbl 3;
     m.mem.(addr + 4) <- List.nth sbl 4;
     m.mem.(addr + 5) <- List.nth sbl 5;
     m.mem.(addr + 6) <- List.nth sbl 6;
     m.mem.(addr + 7) <- List.nth sbl 7)
  
let store_data (ol : operand list) (ind : int) (m : mach) (data : int64) :
  unit =
  let addr_op = List.nth ol ind
  in
    match addr_op with
    | Reg r -> m.regs.(rind r) <- data
    | Ind1 _ | Ind2 _ | Ind3 _ ->
        let i = calculate_ind ol ind m in store_data_helper i data m
    | Imm i -> failwith "Can't use immediates as locations"

let get_data (l : int64) (m : mach) : int64 =
  let addr_opt = map_addr l in
  let addr =
    match addr_opt with | Some x -> x | None -> raise X86lite_segfault in
  let end_opt = map_addr (Int64.add l (Int64.of_int 7)) in
  let end_addr =
    match end_opt with | Some x -> x | None -> raise X86lite_segfault in
  let ret =
    int64_of_sbytes
      [ m.mem.(addr + 0); m.mem.(addr + 1); m.mem.(addr + 2);
        m.mem.(addr + 3); m.mem.(addr + 4); m.mem.(addr + 5);
        m.mem.(addr + 6); m.mem.(addr + 7) ]
  in ret

    
(* Decode_val simply returns the value of the operand or the value at the  *)
(* address denoted by the operand. It only returns values, no addresses.   *)
      
let decode_val (ol : operand list) (ind : int) (m : mach) : int64 =
  let operand = List.nth ol ind
  in
    match operand with
    | Imm i ->
        (match i with | Lit l -> l | Lbl l -> failwith "Unresolved Label!")
    | Reg r -> m.regs.(rind r)
    | Ind1 _ | Ind2 _ | Ind3 _ ->
        let l = calculate_ind ol ind m in get_data l m
  
let decode_amt (ol : operand list) (ind : int) (m : mach) : int64 =
  let operand = List.nth ol ind
  in
    match operand with
    | Imm i ->
        (match i with | Lit l -> l | Lbl l -> failwith "Unresolved Label!")
    | Reg Rcx -> m.regs.(rind Rcx)
    | _ -> failwith "invalid operand for AMT"
  
let store_byte (ol : operand list) (ind : int) (m : mach) (data : int64) :
  unit =
  let addr_op = List.nth ol ind
  in
    match addr_op with
    | Ind1 _ | Ind2 _ | Ind3 _ ->
        let i = calculate_ind ol ind m in
        let addr_opt = map_addr i in
        let addr =
          (match addr_opt with | Some x -> x | None -> raise X86lite_segfault) in
        let sbl : sbyte list = sbytes_of_int64 data
        in m.mem.(addr) <- List.nth sbl 0
    | Reg r ->
        m.regs.(rind r) <-
          Int64.logor
            (Int64.shift_left (Int64.shift_right_logical m.regs.(rind r) 8) 8)
            data
    | _ -> failwith "Immediates are invalid for addressing"
  
let condition_flags (res : int64) (m : mach) : unit =
  (m.flags.fs <- (Int64.shift_right_logical res 63) = Int64.one;
   m.flags.fz <- res = Int64.zero)
  
let set_condition_flags (res : Int64_overflow.t) (m : mach) : unit =
  (m.flags.fo <- res.Int64_overflow.overflow;
   condition_flags res.Int64_overflow.value m)
  
let arith (op : opcode) (ol : operand list) (m : mach) : unit =
  match op with
  | Negq ->
      let dest = decode_val ol 0 m in
      let data = Int64_overflow.neg dest
      in
        (store_data ol 0 m data.Int64_overflow.value;
         set_condition_flags data m;
         if dest = Int64.min_int then m.flags.fo <- true)
  | Addq ->
      let src = decode_val ol 0 m in
      let dest = decode_val ol 1 m in
         let res = Int64_overflow.add dest src in
           store_data ol 1 m res.Int64_overflow.value;
            set_condition_flags res m;
  | Cmpq ->
      let src = decode_val ol 0 m in
      let dest = decode_val ol 1 m in
      let res = Int64_overflow.sub dest src
      in
      set_condition_flags res m;
      if src = Int64.min_int then m.flags.fo <- true
  | Subq ->
      let src = decode_val ol 0 m in
      let dest = decode_val ol 1 m in
      let res = Int64_overflow.sub dest src
      in
        (store_data ol 1 m res.Int64_overflow.value;
         set_condition_flags res m;
         if src = Int64.min_int then m.flags.fo <- true);
     
  | Imulq ->
      let src = decode_val ol 0 m in
      let reg = decode_val ol 1 m in
      let res = Int64_overflow.mul reg src
      in
        (store_data ol 1 m res.Int64_overflow.value;
         set_condition_flags res m)
  | Incq ->
      let src = decode_val ol 0 m in
      let res = Int64_overflow.succ src
      in
        (store_data ol 0 m res.Int64_overflow.value;
         set_condition_flags res m)
  | Decq ->
      let src = decode_val ol 0 m in
      let res = Int64_overflow.pred src
      in
        (store_data ol 0 m res.Int64_overflow.value;
         set_condition_flags res m;
         if src = Int64.min_int then m.flags.fo <- true)
  | _ -> ()
  
let logic (op : opcode) (ol : operand list) (m : mach) : unit =
  match op with
  | Notq ->
      let dest = decode_val ol 0 m in
      let res = Int64.lognot dest in store_data ol 0 m res
  | Andq ->
      let src = decode_val ol 0 m in
      let dest = decode_val ol 1 m in
      let res = Int64.logand dest src
      in (store_data ol 1 m res; condition_flags res m; m.flags.fo <- false)
  | Orq ->
      let src = decode_val ol 0 m in
      let dest = decode_val ol 1 m in
      let res = Int64.logor dest src
      in (store_data ol 1 m res; condition_flags res m; m.flags.fo <- false)
  | Xorq ->
      let src = decode_val ol 0 m in
      let dest = decode_val ol 1 m in
      let res = Int64.logxor dest src
      in (store_data ol 1 m res; condition_flags res m; m.flags.fo <- false)
  | _ -> ()
  
let bitwise (op : opcode) (ol : operand list) (m : mach) : unit =
  match op with
  | Sarq ->
      let amt = Int64.to_int (decode_amt ol 0 m) in
      let dest = decode_val ol 1 m in
      let res = Int64.shift_right dest amt
      in
        (store_data ol 1 m res;
         if amt = 0 then () else condition_flags res m;
         if amt = 1 then m.flags.fo <- false else ())
  | Shlq ->
      let amt = Int64.to_int (decode_amt ol 0 m) in
      let dest = decode_val ol 1 m in
      let res = Int64.shift_left dest amt
      in
        (store_data ol 1 m res;
         if amt = 0 then () else condition_flags res m;
         if
           (amt = 1) &&
             (Int64.shift_right_logical dest 63
              <> (Int64.logand (Int64.shift_right_logical dest 62) 1L))
         then m.flags.fo <- true
         else ())
  | Shrq ->
      let amt = Int64.to_int (decode_amt ol 0 m) in
      let dest = decode_val ol 1 m in
      let res = Int64.shift_right_logical dest amt
      in
        (store_data ol 1 m res;
         if amt = 0 then () else condition_flags res m;
         if amt = 1
         then m.flags.fo <- (Int64.shift_right_logical dest 63) = Int64.one
         else ())
  | Set cc ->
      if interp_cnd {fo = m.flags.fo; fs = m.flags.fs; fz = m.flags.fz} cc
      then store_byte ol 0 m Int64.one
      else store_byte ol 0 m Int64.zero
  | _ -> ()
  
let push (ol : operand list) (m : mach) : unit =
  let src = decode_val ol 0 m
  in
    (m.regs.(rind Rsp) <- Int64.sub m.regs.(rind Rsp) 8L;
     let opl = [ Ind2 Rsp ] in store_data opl 0 m src)
  
let pop (ol : operand list) (m : mach) : unit =
  let opl = [Ind2 Rsp] in
  let value = decode_val opl 0 m
  in
    store_data ol 0 m value;
     m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) 8L
  
let dmove (op : opcode) (ol : operand list) (m : mach) : unit =
  match op with
  | Leaq -> let maddr = calculate_ind ol 0 m in store_data ol 1 m maddr
  | Movq -> let src = decode_val ol 0 m in store_data ol 1 m src
  | Pushq -> push ol m
  | Popq -> pop ol m
  | _ -> ()
  
let flow (op : opcode) (ol : operand list) (m : mach) : unit =
  begin match op with
  | Jmp -> let src = decode_val ol 0 m in m.regs.(rind Rip) <- src
  | Callq ->
      let src = decode_val ol 0 m in
      let ripl = [ Reg Rip ] in
      push ripl m; m.regs.(rind Rip) <- src
  | Retq -> let ripl = [Reg Rip] in pop ripl m
  | J cc ->
      let src = decode_val ol 0 m in
      if interp_cnd { fo = m.flags.fo; fs = m.flags.fs; fz = m.flags.fz} cc
      then m.regs.(rind Rip) <- src
      else m.regs.(rind Rip) <- (Int64.add m.regs.(rind Rip) 4L) 
  | _ -> ()
  end
  
let interpret (insn : ins) (m : mach) : unit =
  match insn with
  | (op, ol) ->
      begin match op with
       | Cmpq | Negq | Addq | Subq | Imulq | Incq | Decq -> arith op ol m; 
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
       | Notq | Andq | Orq | Xorq -> logic op ol m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
       | Shlq | Sarq | Shrq | Set _ -> bitwise op ol m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
       | Leaq | Movq | Pushq | Popq -> dmove op ol m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
       | Jmp | J _ | Callq | Retq -> flow op ol m
      end
      
let service (m : mach) (elem : sbyte) : unit =
  match elem with
  | InsB0 ins ->
      interpret ins m
  | InsFrag -> m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) Int64.one
  | Byte b -> m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) Int64.one
  
let step (m : mach) : unit =
  let cur_insn = m.regs.(rind Rip) in
  let addr_opt = map_addr cur_insn in
  let addr =
    match addr_opt with | Some x -> x | None -> 
      print_endline("step segfault"); raise X86lite_segfault
  in service m m.mem.(addr)
  
(* Runs the machine until the rip register reaches a designated memory     *)
(* address.                                                                *)
let run (m : mach) : int64 =
  (while m.regs.(rind Rip) <> exit_addr do step m done; m.regs.(rind Rax))
  
(* assembling and linking --------------------------------------------------- *)
(* A representation of the executable *)
type exec =
  { entry : quad; (* address of the entry point *) text_pos : quad;
    (* starting address of the code *) data_pos : quad;
    (* starting address of the data *) text_seg : sbyte list;
    (* contents of the text segment *) data_seg : sbyte list
  }

(* contents of the data segment *)
(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl
  
(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl
  
(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at address 0
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
(* Get size of text and data segments *)
let get_size (option : int) (size : int64) (e : elem) : int64 =
  (* Data size is computed differently for different data types *)
  let data_size (d_size : int64) (d : data) : int64 =
    match d with
    | Asciz a ->
        Int64.add (Int64.add d_size 1L) (Int64.of_int (String.length a))
    | Quad (Lit i) -> Int64.add d_size 8L
    | _ -> size
  in
    (* text size is just 4 times the length of the ins list *)
    match (option, (e.asm)) with
    | (0, Text t) -> Int64.add size (Int64.of_int ((List.length t) * 4))
    | (1, Data d) -> Int64.add size (List.fold_left data_size 0L d)
    | (_, _) -> size
  
(* Tuple to represent entry in the symbol table *)
type tpl = (lbl * quad)

(* Find label to address mapping in symbol table, return address or
   if not present, then -1L or raise exception depending on opt *)
let rec find_lbl (tl : tpl list) (l : lbl) (opt : int) : int64 =
  match tl with
  | (a, b) :: tail -> if a = l then b else find_lbl tail l opt
  | [] -> if opt = 0 then (-1L) else raise (Undefined_sym l)
  
(* Translate the labels into the addresses that they will be found in *)
let translate (opt : int) (tl, s) (e : elem) : ((tpl list) * int64) =
  let data_size (d_size : int64) (d : data) : int64 =
    match d with
    | Asciz a ->
        Int64.add (Int64.add d_size 1L) (Int64.of_int (String.length a))
    | Quad (Lit i) -> Int64.add d_size 8L
    | _ -> s
  in
    match (opt, (e.asm)) with
    | (0, Text t) ->
        let new_size = Int64.add s (Int64.of_int ((List.length t) * 4)) in
        let addr = find_lbl tl e.lbl 0
        in
          if addr = (-1L)
          then ((List.append tl [ ((e.lbl), s) ]), new_size)
          else raise (Redefined_sym e.lbl)
    | (1, Data d) ->
        let new_size = Int64.add s (List.fold_left data_size 0L d) in
        let addr = find_lbl tl e.lbl 0
        in
          if addr = (-1L)
          then ((List.append tl [ ((e.lbl), s) ]), new_size)
          else raise (Redefined_sym e.lbl)
    | (_, _) -> (tl, s)
  
(* Replace operands in the operands list, need to consider three cases where
   where labels can occur *)
let rec replace_operands (tl : tpl list) (ol : operand list) : operand list =
  match ol with
  | Imm (Lbl l) :: tail ->
      List.append [ Imm (Lit (find_lbl tl l 1)) ] (replace_operands tl tail)
  | Ind1 (Lbl l) :: tail ->
      List.append [ Ind1 (Lit (find_lbl tl l 1)) ] (replace_operands tl tail)
  | Ind3 ((Lbl l), r) :: tail ->
      List.append [ Ind3 ((Lit (find_lbl tl l 1)), r) ]
        (replace_operands tl tail)
  | [] -> []
  | r :: tail -> List.append [ r ] (replace_operands tl tail)
  
(* Replace each operand in operand list recursively *)
let replace_each (tl : tpl list) (s : sbyte list) (i : ins) : sbyte list =
  match i with
  | (op, ol) -> List.append s (sbytes_of_ins (op, (replace_operands tl ol)))
  
(* Replace data label, only one case is considered *)
let replace_data (tl : tpl list) (sl : sbyte list) (d : data) : sbyte list =
  match d with
  | Quad (Lit i) -> List.append sl (sbytes_of_data d)
  | Asciz s -> List.append sl (sbytes_of_data d)
  | Quad (Lbl l) ->
      List.append sl (sbytes_of_data (Quad (Lit (find_lbl tl l 1))))
  
(* General replace function*)
(* Folds left over all the insn in the insn list or the data in the data list
	   replacing each instance of a lable if possible *)
let replace (opt : int) (tl : tpl list) (sbl : sbyte list) (e : elem) :
  sbyte list =
  let replace_all = replace_each tl in
  let replace_dat = replace_data tl
  in
    match (opt, (e.asm)) with
    | (0, Text t) -> List.append sbl (List.fold_left replace_all [] t)
    | (1, Data d) -> List.append sbl (List.fold_left replace_dat [] d)
    | (_, _) -> sbl
  
(* Main Assemble program *)
let assemble (p : prog) : exec =
  let text_func = get_size 0 in (* Get size of text and data segments *)
  let text_size = List.fold_left text_func 0L p in
  let translate_text = translate 0 in
  let translate_data = translate 1 in
  (* Create symbol table. Consider ins first as text seg appears first *)
  let (addr, size) = List.fold_left translate_text ([], 0x400000L) p in
  let (addr2, size2) = List.fold_left translate_data (addr, size) p in
  let e = find_lbl addr "main" 1 in
  let replace_text = replace 0 addr2 in
  let replace_data = replace 1 addr2 in
  (* sbytes list of patched data and ins *)
  let ts = List.fold_left replace_text [] p in
  let ds = List.fold_left replace_data [] p
  in
    {
      entry = e;
      text_pos = 0x400000L;
      data_pos = Int64.add 0x400000L text_size;
      text_seg = ts;
      data_seg = ds;
    }
  
(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load
  {
    entry = entry;
    text_pos = text_pos;
    data_pos = data_pos;
    text_seg = text_seg;
    data_seg = data_seg
  } : mach = (* [[text_seg][data_seg]........[exit_addr]] *)
  (* 0                           FFF8     FFFF *)
  (* The following commands create the above structure, 
	   by making arrays for each segment and then apppending them *)
  let tmp = Array.make 0xFFF8 InsFrag in
  let tmp0 = Array.of_list text_seg in
  let tmp1 = Array.of_list data_seg in
  let tmp2 = Array.append tmp0 tmp1
  in
    (Array.blit tmp2 0 tmp 0 (Array.length tmp2);
     let tmp4 = Array.of_list (sbytes_of_int64 exit_addr) in
     let memory = Array.append tmp tmp4 in
     (* All flags are false in the beginning *)
     let flgs = { fo = false; fs = false; fz = false; } in
     (* Create 17 registers, fill Rip and Rsp *)
     let registers = Array.make 17 0L
     in
       (Array.set registers (rind Rip) entry;
        Array.set registers (rind Rsp) 0x40FFF8L;
        { flags = flgs; regs = registers; mem = memory; }))
  

