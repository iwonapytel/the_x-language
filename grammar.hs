-- JPP 2018 - Interpreter, language grammar
-- Iwona Pytel, ip360730

-- Sources: https://github.com/BNFC/bnfc/blob/master/examples/C/C.cf
--          https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2017/Latte/Latte.cf

-- Main program semantics
entrypoints Program, Stm, Exp ;

Progr.  Program ::= [Instruction] ;

(:[]).    [Instruction] ::= Instruction;
(:).      [Instruction] ::= Instruction [Instruction];

StmInstruction.   Instruction ::= Stm ;
-- ExpInstruction.   Instruction ::= Exp ;
-- DecInstruction.   Instruction ::= Dec ;

Block. Block ::= "{" [Instruction] "}" ;


-- Types
TInt.    TypeName ::= "int";
TBool.   TypeName ::= "bool";
TString. TypeName ::= "string";
TAuto.   TypeName ::= "auto"
TTuple. TypeName ::= "(" [TypeName] ")" ;
separator nonempty TypeName ",";


-- Declarations
VarDec. Dec ::= TypeName [Item] ";" ;
FuncDec. Dec ::= TypeName Func ;
-- StructDec. Dec ::= Struct ";"

UninitedVar.  Item ::= Ident ;
UninitedArr.  Item ::= Ident [ArrSize] ;
InitedVar.  Item ::= Ident "=" Initializer ;
InitedArr.  Item ::= Ident [ArrSize] "=" Initializer ;
separator nonempty Item "," ;

SimpleArg. Arg ::= Ident ;
ArrArg.    Arg ::= Ident "[]" ;
separator Arg "," ;

Function. Func ::= Ident "(" [Arg] ")" Block ;
-- Struct.   Struct ::= "struct" Ident "{" [Arg] "}" ;

InitExpr.    Initializer ::= Exp2 ;
InitListOne. Initializer ::= "{" Initializers "}" ;
InitListTwo. Initializer ::= "{" Initializers "," "}" ;

AnInit.   Initializers ::= Initializer ;
MoreInit. Initializers ::= Initializers "," Initializer ;;

separator Dec "" ;

-- Statements
EmptyStm.  Stm ::= ";" ;
BlockStm.  Stm ::= Block ;
DecStm.    Stm ::= Dec ;
ExpStm.    Stm ::= Exp ";" ;
SelectStm. Stm ::= SelectionStm ;
ItStm.     Stm ::= IterStm ;
JStm.      Stm ::= JumpStm ;
PrintStm.  Stm ::= "print" "(" Exp ")" ";"

IfStm.    SelectionStm ::= "if" "(" Exp ")" Stm ;
IfElseStm.    SelectionStm ::= "if" "(" Exp ")" Stm "else" Stm ;

WhileStm.   IterStm ::= "while" "(" Exp ")" Stm;
ForStm1. IterStm ::= "for" "(" Stm Exp ")" Stm ;
ForStm2.  IterStm ::= "for" "(" Stm Exp Stm ")" Stm;

-- GoToStm.   Jump_stm ::= "goto" Ident ";" ;
ContStm.   JumpStm ::= "continue" ";" ;
BreakStm. JumpStm ::= "break" ";" ;
RetStm.  JumpStm ::= "return" ";" ;
RetExpStm.  JumpStm ::= "return" Exp ";" ;

separator Stmt "" ;

-- Expressions
EOr.         Exp   ::= Exp "||" Exp1;
EAnd.        Exp1  ::= Exp1 "&&" Exp2;
ERel.        Exp2  ::= Exp2 RelOp Exp3;
EAdd.        Exp3  ::= Exp3 AddOp Exp4;
EMul.        Exp4  ::= Exp4 MulOp Exp5;
Epreop.      Exp5  ::= UnaryOp Exp6;
Eref.        Exp6  ::= Ident "&"; 
Earray.      Exp6  ::= Ident "[" Exp "]" ;
Evar.        Exp6  ::= Ident;
ELitInt.     Exp6  ::= Integer;
ELitBool.    Exp6  ::= TBool;
ELitString.  Exp6  ::= String;
EFuncApp.    Exp6  ::= Ident "(" [Exp] ")";

ETrue.  TBool ::= "true" ;
EFalse. TBool ::= "false";

coercions Exp 6 ;
separator Exp "," ;

-- Operators
Plus.      AddOp ::= "+" ;
Minus.     AddOp ::= "-" ;
Times.     MulOp ::= "*" ;
Div.       MulOp ::= "/" ;
Mod.       MulOp ::= "%" ;
LTH.       RelOp ::= "<" ;
LE.        RelOp ::= "<=" ;
GTH.       RelOp ::= ">" ;
GE.        RelOp ::= ">=" ;
EQU.       RelOp ::= "==" ;
NE.        RelOp ::= "!=" ;
Address.     UnaryOp ::= "&" ;
Indirection. UnaryOp ::= "*" ;
Plus.        UnaryOp ::= "+" ;
Negative.    UnaryOp ::= "-" ;
Logicalneg.  UnaryOp ::= "!" ;

-- Comments
comment "/*" "*/" ;
comment "//";
comment "#";
