open Assert
open X86
open Simulator
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* We implemented the gcd program 
   It can handle negative numbers, zeroes and positive numbers.
	 It uses Euclid's algorithm for fast computation.
 *)

let gcd a b = [ text "main"
                     [ Movq,  [~$0; ~%Rax]
                     ; Movq,  [~$a; ~%Rdi]
					 ; Movq,  [~$b; ~%Rsi]
					 ; Cmpq,  [~$0; ~%Rdi]
                     ; J Lt,  [~$$"fix_Rdi"]
					 ; Cmpq,  [~$0; ~%Rsi]
                     ; J Lt,  [~$$"fix_Rsi"]
                     ]			  
              ; text "loop"
                     [ Cmpq,  [~$0; ~%Rdi]
                     ; J Eq,  [~$$"exit"]
					 ; Cmpq,  [~$0; ~%Rsi]
                     ; J Eq,  [~$$"exit"]
					 ; Cmpq,  [~%Rsi; ~%Rdi]
                     ; J Eq,  [~$$"done"]
					 ; J Lt,  [~$$"lesser"]
					 ; J Gt,  [~$$"greater"]
					 ]
			  ; text "fix_Rdi"
                     [ Negq, [~%Rdi] 
					 ; Cmpq,  [~$0; ~%Rsi]
                     ; J Lt,  [~$$"fix_Rsi"]
                     ]
			  ; text "fix_Rsi"
                     [ Negq, [~%Rsi]
					  ;	Jmp,   [~$$"loop"]
                     ]
			  ; text "greater"
                     [ Subq, [~%Rsi; ~%Rdi] 
					  ;	Jmp,   [~$$"loop"]
                     ]
			  ; text "lesser"
                     [ Subq, [~%Rdi; ~%Rsi]
					  ;	Jmp,   [~$$"loop"]
                     ]
			  ; text "done"
					 [ Movq,  [~%Rsi; ~%Rax]
					 ; Jmp, [~$$"exit"]
					 ]
              ; text "exit"
                     [ Retq,  [] 
                     ]
			  ]

																
let test_machine (bs: sbyte list): mach =
  let mem = (Array.make mem_size (Byte '\x00')) in
  Array.blit (Array.of_list bs) 0 mem 0 (List.length bs);
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- mem_bot;
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  { flags = {fo = false; fs = false; fz = false};
    regs = regs;
    mem = mem
  }
	
	let stack_offset (i: quad) : operand = Ind3 (Lit i, Rsp)
	
	let sbyte_list (a: sbyte array) (start: int) : sbyte list =
  Array.to_list (Array.sub a start 8)

let leaq = test_machine
  [InsB0 (Leaq, [stack_offset 0L; ~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let incq = test_machine 
  [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Incq, [~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let decq = test_machine 
  [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Decq, [~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let notq = test_machine
  [InsB0 (Notq, [~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Pushq, [~$1]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Notq, [stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ]

let xorq = test_machine
  [InsB0 (Xorq, [~$0;~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Pushq, [~$1]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Xorq, [~$1;stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Xorq, [~$1;stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ]

let orq = test_machine
  [InsB0 (Orq, [~$0;~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Pushq, [~$1]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Orq, [~$1;stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ]
	
let shulk = test_machine
  [InsB0 (Movq, [Imm (Lit 0xFFFFFFFFFFFFFFFFL); ~%Rax]); InsFrag; InsFrag; InsFrag
  ;InsB0 (Shlq, [~$8; ~%Rax]); InsFrag; InsFrag; InsFrag; InsFrag
  ]

let zork = test_machine
  [InsB0 (Movq, [Imm (Lit 0xFFFFFFFFFFFFFFFFL); ~%Rax]); InsFrag; InsFrag; InsFrag
  ;InsB0 (Movq, [Imm (Lit 0x5555555555555555L); ~%Rbx]); InsFrag; InsFrag; InsFrag
  ;InsB0 (Xorq, [~%Rbx; ~%Rax]); InsFrag; InsFrag; InsFrag
  ]

let addq = test_machine
  [InsB0 (Movq, [Imm (Lit 0xBA5EBA5EBA11BA5EL); ~%Rax]); InsFrag; InsFrag; InsFrag
  ;InsB0 (Movq, [Imm (Lit 0x248FF04F04DD3491L); ~%Rbx]); InsFrag; InsFrag; InsFrag
  ;InsB0 (Addq, [~%Rbx; ~%Rax]); InsFrag; InsFrag; InsFrag
  ]


let sarq = test_machine
  [InsB0 (Movq, [~$1;~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Sarq, [~$1;~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$7;~%Rbx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Sarq, [~%Rcx;~%Rcx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$(-7);stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Sarq, [~$2;stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ]
let shrq = test_machine
  [InsB0 (Movq, [~$1;~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Shrq, [~$1;~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$7;~%Rbx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Shrq, [~%Rcx;~%Rcx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$(-7);stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Shrq, [~$2;stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ]

let add_imm_into_reg = Gradedtests.test_machine
  [InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag]

let add_reg_into_reg = Gradedtests.test_machine
  [InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;
   InsB0 (Addq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag]

let add_imm_into_mem = Gradedtests.test_machine
  [InsB0 (Addq, [~$1; Ind1 (Lit 0x400038L)]);InsFrag;InsFrag;InsFrag]

let add_reg_into_mem = Gradedtests.test_machine
  [InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;
   InsB0 (Addq, [~%Rax; Ind1 (Lit 0x400038L)]);InsFrag;InsFrag;InsFrag]

let push_then_pop = Gradedtests.test_machine
    [InsB0 (Pushq, [~$1]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Popq, [~%Rax]);InsFrag;InsFrag;InsFrag]

let program_test (p:prog) (ans:int64) () =
  let res = assemble p |> load |> run in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ()
	
let machine_test (s:string) (n: int) (m: mach) (f:mach -> bool) () =
  for i=1 to n do step m done;
  if (f m) then () else failwith ("expected " ^ s)

let provided_tests : suite = [
 Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
		("gcd420,96", program_test (gcd (124556234) (4222556)) 2L);
  ]);
	
	
	Test ("Test cases from Sibner/Mumick",[
    ("retq sets Rip", machine_test "Rip = 411" 2
        begin test_machine
            [InsB0 (Pushq, [~$411]); InsFrag;InsFrag;InsFrag;
             InsB0 (Retq, []);InsFrag;InsFrag;InsFrag;] end
        (fun m -> m.regs.(rind Rip) = 411L);
     );

    ("JMP sets Rip", machine_test "Rip = 1864" 1
        begin test_machine
            [InsB0 (Jmp, [~$1864]); InsFrag;InsFrag;InsFrag] end
        (fun m -> m.regs.(rind Rip) = 1864L)
     );
    ("Callq sets Rip", machine_test "Rip = 1864" 1
        begin test_machine
            [InsB0 (Callq, [~$1864]); InsFrag;InsFrag;InsFrag] end
        (fun m -> m.regs.(rind Rip) = 1864L)
     );
     ("Callq saves Rip + 1 on stack", machine_test "mem(Reg RSP) = mem_bot" 1
        begin test_machine
            [InsB0 (Callq, [~$1864]); InsFrag;InsFrag;InsFrag;] end
        (fun m -> 
          let sbytes = Array.to_list (Array.sub m.mem (Int64.to_int (Int64.sub m.regs.(rind Rsp) 0x400000L)) 8) in
          (int64_of_sbytes sbytes) = (Int64.add mem_bot 4L)
        )
     );
     ("Callq sets Rip + 1 after more insns", machine_test "mem(Reg RSP) = mem_bot + 8" 3
        begin test_machine
          [ InsB0 (Decq, [~%Rax]);InsFrag;InsFrag;InsFrag;
            InsB0 (Decq, [~%Rax]);InsFrag;InsFrag;InsFrag;
            InsB0 (Callq, [~$1864]); InsFrag;InsFrag;InsFrag;] end
        (fun m -> 
          let sbytes = Array.to_list (Array.sub m.mem (Int64.to_int (Int64.sub m.regs.(rind Rsp) 0x400000L)) 8) in
          (int64_of_sbytes sbytes) = (Int64.add mem_bot 12L)
        )
     );

   ]);
	

Test ("Test cases from David", [
    ("shulk", machine_test "I'm really feelin' it! (Expected 0xFFFFFFFFFFFFFF00L)" 2
      shulk (fun m -> m.regs.(rind Rax) = 0xFFFFFFFFFFFFFF00L)
    );
    ("zork", machine_test "Get eaten by a Grue! (Expected 0xAAAAAAAAAAAAAAAAL)" 3
      zork (fun m -> m.regs.(rind Rax) = 0xAAAAAAAAAAAAAAAAL)
    );
    ("addq", machine_test "Addque. (Expected 0xDEEEAAADBEEEEEEFL)" 3
      addq (fun m -> m.regs.(rind Rax) = 0xDEEEAAADBEEEEEEFL)
    )
  ]);
	
	Test("Test Cases from Max", [
    ("add 1 into reg", Gradedtests.machine_test "rax=1" 1 add_imm_into_reg
      (fun m -> m.regs.(rind Rax) = 1L));
    ("add reg into reg", Gradedtests.machine_test "rax=1 and rbx=1" 2 add_reg_into_reg
      (fun m -> m.regs.(rind Rax) = 1L && m.regs.(rind Rbx) = 1L));
    ("add imm into mem", Gradedtests.machine_test "0x400038L=1" 1 add_imm_into_mem
      (fun m -> begin match map_addr 0x400038L with
        | None -> failwith "runtime error"
        | Some v -> int64_of_sbytes (Gradedtests.sbyte_list m.mem v) = 1L
      end));
    ("add reg into mem", Gradedtests.machine_test "rax=1 and 0x400038L=1" 2 add_reg_into_mem
      (fun m -> begin match map_addr 0x400038L with
        | None -> failwith "runtime error"
        | Some v -> m.regs.(rind Rax) = 1L && 
            int64_of_sbytes (Gradedtests.sbyte_list m.mem v) = 1L
      end));

    ("push and pop 1", Gradedtests.machine_test "rax=1" 2 push_then_pop
      (fun m -> m.regs.(rind Rax) = 1L));
  ]);
	
	Test ("Test Cases From Eric",

        [("incr", machine_test "rax=2" 2
            begin test_machine
                [InsB0 (Incq, [~%Rax]);InsFrag;InsFrag;InsFrag;
                 InsB0 (Incq, [~%Rax]);InsFrag;InsFrag;InsFrag;] end
            (fun m -> m.regs.(rind Rax) = 2L)
         );

         ("decr", machine_test "rax=-2" 2
            begin test_machine
                [InsB0 (Decq, [~%Rax]);InsFrag;InsFrag;InsFrag;
                 InsB0 (Decq, [~%Rax]);InsFrag;InsFrag;InsFrag;] end
            (fun m -> m.regs.(rind Rax) = -2L)
         );

         ("not", machine_test "rax=1" 2
            begin test_machine 
                [InsB0 (Movq, [~$(-2); ~%Rax]); InsFrag;InsFrag;InsFrag;
                 InsB0 (Notq, [~%Rax]);InsFrag;InsFrag;InsFrag;] end
              (fun m -> m.regs.(rind Rax) = 1L)
         )]
       );
			
		
	
	 Test ("Test Cases From Thomas/Max",[
    ("leaq loads to rax", machine_test "Rax = 2" 1 leaq
      (fun m -> m.regs.(rind Rax) = 0x40fff8L)
      );
    ("incq 2 in rax = 3", machine_test "Rax = 3" 2 incq
      (fun m -> m.regs.(rind Rax) = 3L)
      );
    ("decq 2 in rax = 1", machine_test "Rax = 1" 2 decq
      (fun m -> m.regs.(rind Rax) = 1L)
      );
    ("notq in regs and mem", machine_test "notq 0, notq 1" 3 notq
      (fun m -> m.regs.(rind Rax) = Int64.lognot 0L
                && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 
                   Int64.lognot 1L)
      );
    ("xorq in regs and mem", machine_test "xorq 0 0, xorq 1 0" 4 xorq
      (fun m -> m.regs.(rind Rax) = 0L
                && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 1L)
      );
    ("orq in regs and mem", machine_test "orq 0, orq 1" 3 orq
      (fun m -> m.regs.(rind Rax) = 0L
                && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 1L)
      );
    ("sarq in regs and mem", machine_test "sarq" 6 sarq
      (fun m -> m.regs.(rind Rax) = 0L
                && m.regs.(rind Rcx) = 0L
                && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 
                Int64.neg 2L)
      );
    ("shrq in regs and mem", machine_test "shrq" 6 shrq
      (fun m -> m.regs.(rind Rax) = 0L
                && m.regs.(rind Rcx) = 0L
                && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 
                4611686018427387902L)
      );
  ]);
] 


