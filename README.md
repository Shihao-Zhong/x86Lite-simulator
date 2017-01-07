# X86 Lite Interpreter #
This project was the second homework for the compilers class at Penn. I worked with a partner to implement a interpreter for a subset of X86 in Ocaml. Details about the X86Lite instruction set available [here](http://www.cis.upenn.edu/~cis341/15sp/hw/hw2/).

## Setup ##

Setup instructions for a relatively recent version of Ubuntu (>= 14.04):

### Install GCC ###

    sudo apt-get install gcc

### Install OCaml >= 4.01.0 ###

    sudo apt-get install m4 ocaml-native-compilers camlp4-extra opam

## Build ##

Navigate to the folder and type

    make

## Test cases ##

`main.native` is the executable that is built. It is setup to run the interpreter on the test cases in `gradedtests.ml` and `providedtest.ml`. Execute `main.native --test` to run these tests.

## Using the intepreter ##

You also also use the interpreter as a library to run your own X86Lite programs.

First import the following modules:
    
    open X86
    open Simulator
    open Asm

Then, run your program `p`:

    let res = assemble p |> load |> run
    
`res` is the `int64` value that is the result of your computation.

For an example of an X86Lite assembly program that can be run by the interpreter, see the gcd function below. When you call this function on two integers, it returns an X86Lite program implementing Euler's algorithm.

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

