open Assert
open X86
open Simulator
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

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
																	;	Jmp,   [~$$"loop"]
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

let provided_tests : suite = [
  (*Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
		("gcd420,96", program_test (gcd 420 96) 12L);
  ]);
	*)
] 


