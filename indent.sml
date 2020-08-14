(*structure Indent = struct

fun printspace 0     = ""
  | printspace space = "  " ^ (printspace (space-1))

fun indent space (Ast.Assign (a,b)) = (printspace space) ^ (indent space a) ^ " := " ^ (indent space b) ^ ";"
  | indent space (Ast.Op (a,Ast.Plus,b))	= (indent space a) ^ "+" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Minus,b))	= (indent space a) ^ "-" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Mul,b))	= (indent space a) ^ "*" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Div,b))	= (indent space a) ^ "/" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.GreaterEqualto,b))	= (indent space a) ^ ">=" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.LesserEqualto,b))	= (indent space a) ^ "<=" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.GreaterLesser,b))	= (indent space a) ^ "<>" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Lesser,b))	= (indent space a) ^ "<" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Greater,b))	= (indent spacmakee a) ^ ">" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.And,b))	= (indent space a) ^ "&" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Or,b))	= (indent space a) ^ "|" ^ (indent space b)
  | indent space (Ast.Var a)		= a
  | indent space (Ast.If (a,b))		= (printspace space) ^ ("if " ^ (indent space a) ^ " then " ^ (indent (space+2) b))
  | indent space (Ast.While (a,b))	= (printspace space) ^ "while " ^ (indent space a) ^ " do\n" ^ (indent (space+2) b)
  | indent space (Ast.For (a,b,c,d))    = (printspace space) ^ "for " ^ (indent space a) ^ " := " ^ (indent space b) ^ " to " ^ (indent space c) ^ " do\n" ^ (indent (space+2) d)
  | indent space (Ast.Let (a,b))	= (printspace space) ^ "let\n" ^ (indentlist (space+2) a) ^ (printspace space) ^ "in\n" ^ (indentlist (space+2) b) ^ "\n" ^ (printspace space) ^ "end"
  | indent space (Ast.Op (a,Ast.Equal,b))	= (indent space a) ^ " = " ^ (indent space b)
  | indent space (Ast.Const a)		= Int.toString a

and indentlist space [] 	= ""
  | indentlist space (x::xs) 	= (indent space x ^ "\n" ^ indentlist space xs)

end
*)
structure Indent = struct

fun P 0     = " "
  | P s = " " ^ (P (s-1))

fun Exp s (Ast.Nil)		= (P s)^"nil\n"
  | Exp s (Ast.Const a)		= Int.toString a
  | Exp s (Ast.Quote a)		= a
  | Exp s (Ast.IDENT (Ast.Var a))		= a
  | Exp s (Ast.IF (a,b))			= (P s) ^ "if " ^ (Exp s a) ^ " then \n" ^ (Exp (s+2) b)^"\n"
  | Exp s (Ast.ELSE (a,b,c))		= (P s) ^ "if " ^ (Exp s a) ^ " then\n " ^ (Exp (s+2) b)^"\n"^(P s) ^"else"^ (Exp (s+2) c)^"\n"
  | Exp s (Ast.WHILE (a,b))		= (P s) ^ "while " ^ (Exp s a) ^ " do\n" ^ (Exp (s+2) b)^"\n"
  | Exp s (Ast.FOR (a,b,c,d))    		= (P s) ^ "for" ^(P s) ^ (Id s a) ^ ":=" ^ (Exp s b) ^ " to " ^ (Exp s c) ^ " do\n" ^ (Exp (s+2) d)^"\n"
  | Exp s (Ast.Break )			= (P s) ^ "break\n"
  | Exp s (Ast.Let (a,b))			= (P s) ^ "let \n" ^(Decs (s+2) a)^(P s)^"\n in \n"^(Exps (s+2) b) ^ "\n" ^(P s) ^ "end\n"
  | Exp s (Ast.UMINUS a)		= "-"^(Exp s a)
  | Exp s (Ast.OP (a,Ast.PLUS,b))		= (Exp s a) ^ "+" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.MINUS,b))	= (Exp s a) ^ "-" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.MUL,b))		= (Exp s a) ^ "*" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.DIVIDE,b))		= (Exp s a) ^ "/" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.EQUAL,b))	= (Exp s a) ^ "=" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.LESSGREAT,b))		= (Exp s a) ^ "<>" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.GREAT,b))		= (Exp s a) ^ ">" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.LESS,b))		= (Exp s a) ^ "<" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.GREATEQUAL,b))	= (Exp s a) ^ ">=" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.LESSEQUAL,b))	= (Exp s a) ^ "<=" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.AND,b))		= (Exp s a) ^ "&" ^ (Exp s b)
  | Exp s (Ast.OP (a,Ast.OR,b))		= (Exp s a) ^ "|" ^ (Exp s b)
  | Exp s (Ast.Exps a)			= "("^(Exps1 s a)^")"
  | Exp s (Ast.ArrayCreate (a,b))	= (P s)^(Id s a)^"["^(Exp s b)^"]"
  | Exp s (Ast.RecordCreate (a,b))	= let
                                  	fun printRecbody []   	      = "" 
					                          	| printRecbody ((a,b)::[])  = (Id s a)^" = "^( Exp s b)
						                          | printRecbody ((a,b)::x )  = (Id s a)^" = "^( Exp s b)^", "^(printRecbody x)		
					                      in
						                          (Id s a)^"{"^(printRecbody b)^"}\n"	
					                      end
 
  | Exp s (Ast.New a)			= (P s)^"new "^(Id 0 a)
  | Exp s (Ast.Functioncall (a,b))	= (P s)^(Id s a)^"("^(Parguments s b)^")"
  | Exp s (Ast.Methodcall (a,b,c))	= (P s)^(Exp s a)^"."^(Id s b)^"("^(Parguments s c)^")"
  | Exp s (Ast.Assign (a,b)) 	= (P s) ^ (Exp s a) ^ ":=" ^ (Exp s b)
  | Exp s (Ast.Lvalue1 (a,b)) = ( P s) ^ (Exp s a) ^ "." ^ (Id s b)
  | Exp s (Ast.Lvalue2 (a,b)) = ( P s)^ (Exp s a) ^" [" ^(Exp s a) ^ "] "

and 
    Id s (Ast.Var a) = a

and
	Parguments s [] = ""
	|Parguments s (x::[]) = ( Exp s x)^""
	|Parguments s (x::xs) = ( Exp s x)^", "^(Parguments s xs)

and 
  Exps s [] = ""
 | Exps s (a :: []) = (Exp s a)
 | Exps s (a :: xs) = (Exp s a)^";\n "^(Exps s xs) 

and
  Exps1 s [] = ""
 | Exps1 s (a :: []) = (Exp 0 a)
 | Exps1 s (a :: xs) = (Exp 0 a)^"; "^(Exps 0 xs) 

and 
  Decs s [] = ""
 | Decs s (a :: []) = (Dec s a)
 | Decs s (a :: xs) = (Dec s a)^(Decs s xs)

and Dec s (Ast.Typedecl (a,b))	= (P s)^" type "^(Id s a)^" = "^(TY s b)
  | Dec s (Ast.ClassDec1 (a,b))	= (P s)^" class "^(Id s a)^(P s)^"\n{\n"^(CFS (s+2) b)^"}"^"\n"
  | Dec s (Ast.ClassDec2 (a,b,c))	= (P s)^" class "^(Id s a)^"extends"^(Id s b)^(P s)^"\n{"^(CFS (s+2) c)^"}"^"\n"
  | Dec s (Ast.DVarDec a)	= (P s)^(Var 0 a)
  | Dec s (Ast.Functiondecl1 (a,b,c))	= (P s)^" function "^(Id s a)^"("^(TYF s b)^")"^" = "^(Exp 0 c)^"\n"
  | Dec s (Ast.Functiondecl2 (a,b,c,d))	= (P s)^" function"^(Id s a)^"("^(TYF s c)^")"^":"^(Id s b)^"="^(Exp 0 d)^"\n"
  | Dec s (Ast.Primitivedecl1 (a,b))	= (P s)^" primitive"^(Id s a)^"("^(TYF s b)^")"^"\n"
  | Dec s (Ast.Primitivedecl2 (a,b,c))	= (P s)^" primitive"^(Id s a)^"("^(TYF s c)^")"^":"^(Id s b)^"\n"
  | Dec s (Ast.Import a)		= (P s)^"import "^(Exp 0 a)^"\n"
  
and 
  Var s (Ast.Vardec1 (a,b,c))   =  (P 0)^"var "^ (Id s a)^" : "^(Id s b)^" := "^(Exp s c)^"\n"
  | Var s (Ast.Vardec2 (a,b))   =  (P 0)^"var "^(Id s a)^" := "^(Exp s b)^"\n"

and CFS s [] = ""
 | CFS s (a :: []) = (CF s a)
 | CFS s (a :: xs) = (CF s a)^(CFS s xs)

and 
  CF s (Ast.ClassVar a)	= (P s)^(Var s a)
  | CF s (Ast.Method1 (a,b,c))		= (P s)^"method "^(Id s a)^"("^ (TYF s b)^")"^" = "^(Exp 0 c)^"\n"
  | CF s (Ast.Method2 (a,b,c,d))  = (P s)^"method "^(Id s a)^"("^(TYF s c)^")"^" : "^(Id s b)^" = "^ (Exp 0 d)^"\n"


and TY s (Ast.TyIDENT (Ast.Var a))		=  a
  | TY s ( Ast.Tyf a)	= " {"^(TYF s a)^"} "
  | TY s (Ast.Array a)	= (P s)^"array of "^(Id s a)
  | TY s (Ast.Classfields a)	= (P s)^"\n class \n{\n"^(CFS (s+2) a) ^"} \n"
  | TY s (Ast.Classfields2 (a,b))	= (P s)^"class extends "^(Id s a)^"\n{\n"^(CFS (s+2) b) ^"} \n"

and TYF s (Ast.Tyfields a)	= let
						fun p [] = " "
						|p ((a,b)::[]) = (Id s a)^" : "^(Id s a)
						|p ((a,b)::xs) = (Id s a)^" : "^(Id s a)^" , "^(p xs)
					in
						(p a)
					end
 


and Program s (Ast.Expression a)	= (Exp s a)
  | Program s (Ast.Decls a) 	= (Decs s a)

end
