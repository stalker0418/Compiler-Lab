signature TEMP = sig
    type temp  (* temporary variables of your program *)
    type label (* temporary labels for your program *)
    val newlabel : unit -> label
    val newtemp  : unit -> temp

end

structure Temp : TEMP = struct
type temp = int
type label= int       

val tempcount = ref 0;
val labelcount = ref 0;
fun newtemp () = let val new = !tempcount
in (tempcount := !tempcount+ 1; !tempcount)
end

fun newlabel () = let val new = !labelcount
in (labelcount := !labelcount +1; !labelcount)
end
end

structure MIPS= struct

datatype regs = Zero | At 
		| V0 | V1 	
		| A0 | A1 | A2 | A3 
		| TO | T1 | T2 | T3 | T4 | T5 | T6 | T7 
		| S0 | S1 | S2 | S3 |S4 | S5 | S6 | S7 
		| T8 | T9 
		| K0 | K1 
		| GP | SP | FP | RA

datatype  ('l,'t) inst = ADD of 't * 't * 't
			| SUB of 't * 't * 't
			| ADDI of 't * 't * int
			| ADDU of 't * 't * 't
			| SUBU of 't * 't * 't
			| ADDIU of 't * 't * int
			| MUL of 't * 't * 't
			| MULT of 't * 't
			| DIV of 't * 't
			| AND of 't * 't * 't
			| OR of 't * 't * 't
			| ANDI of 't * 't * int
			| ORI of 't * 't * int
			| SLL of 't * 't * 't
			| SRL of 't * 't * 't
			| LW of 't * int * 't
			| SW of 't * int * 't
			| LUI of 't * int
			| LA of 't * 'l
			| LI of 't * int
			| MFHI of 't
			| MFLO of 't
			| MOVE of 't * 't
			| BEQ of 't * 't * 'l
			| BNE of 't * 't * 'l
			| BGT of 't * 't * 'l
			| BGE of 't * 't * 'l
			| BLT of 't * 't * 'l
			| BLE of 't * 't * 'l
			| SLT of 't * 't * 't
			| SLTI of 't * 't * int
			| J of 'l
			| JR of 't
			| JAL of 'l
			| SYSCALL

fun p (ADD(a, b, c)) = "add $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)
 | p (SUB (a, b, c)) = "sub $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (ADDI (a, b, c)) = "addi $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (ADDU (a, b, c)) = "addu $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (SUBU (a, b, c)) = "subu $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (MUL (a, b, c)) = "mul $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (MULT (a, b)) = "mult $"^ Int.toString(a) ^" $"^ Int.toString(b)  
 | p (ADDIU(a, b, c)) = "addiu $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (DIV (a, b)) = "div $"^ Int.toString(a) ^" $"^ Int.toString(b)
 | p (AND (a, b, c)) = "and $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (OR (a, b, c)) = "or $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (ANDI (a, b, c)) = "andi $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (ORI (a, b, c)) = "ori $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (SLL (a, b, c)) = "sll $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (SRL (a, b, c)) = "srl $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (LW (a, b, c)) = "lw $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (SW (a, b, c)) = "sw $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (LUI (a, b)) = "lui $"^ Int.toString(a) ^" $"^ Int.toString(b)
 | p (LA (a, b)) = "la $"^ Int.toString(a) ^" $"^ Int.toString(b)
 | p (LI (a, b)) = "li $"^ Int.toString(a) ^" $"^ Int.toString(b)
 | p (MFHI (a)) = "mfhi $"^ Int.toString(a)
 | p (MFLO (a)) = "mflo $"^ Int.toString(a)
 | p (MOVE (a, b)) = "move $"^ Int.toString(a) ^" $"^ Int.toString(b)
 | p (BEQ (a, b, c)) = "beq $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (BNE (a, b, c)) = "bne $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (BGT (a, b, c)) = "bgt $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (BGE (a, b, c)) = "bge $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (BLT (a, b, c)) = "blt $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (BLE (a, b, c)) = "ble $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (SLT (a, b, c)) = "slt $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^ Int.toString(c)  
 | p (SLTI (a, b, c)) = "slti $"^ Int.toString(a) ^" $"^ Int.toString(b)^" $"^Int.toString(c)  
 | p (J a) = "j $"^ Int.toString(a)
 | p (JR a) = "jr $"^ Int.toString(a)
 | p (JAL a) = "jal $"^ Int.toString(a)
 | p SYSCALL = "syscall"
 
end
