%{
open Ast
%}

(* Declaration des tokens avec leurs types *)
%token <int> NUMBER
%token <string> STRING VAR
%token <Ast.relop> RELOP
%token NL PLUS MINUS TIMES DIVIDE ASSIGN END PRINT IF THEN VAVERS INPUT REM CR COMMA LPAREN RPAREN
%token EOF

(* Declaration du point d'entree du parseur *)
%start <programme> programme
%%

(* Definition des regles de la grammaire *)

programme:
  (* Un programme est une liste de lignes suivie d'un EOF *)
  | ligne_list EOF { Programme($1) }

ligne_list:
  (* Une liste de lignes peut etre une seule ligne *)
  | ligne { [$1] }
  (* Ou une liste de lignes suivie d'une autre ligne *)
  | ligne_list ligne { $1 @ [$2] }
  (* Ou une liste de lignes suivie d'un retour chariot *)
  | ligne_list CR { $1 }

ligne:
  (* Une ligne est composee d'un numero et d'une instruction *)
  | NUMBER instr_list { Ligne($1, $2) }

instr_list:
  (* Une liste d'instructions peut etre une seule instruction *)
  | instr { [$1] }
  (* Ou une liste d'instructions suivie d'une autre instruction *)
  | instr_list COMMA instr { $1 @ [$3] }
relop:
  (* Un operateur relationnel est directement pris du token *)
  | RELOP { $1 }

instr:
  (* Instruction pour imprimer une liste d'expressions *)
  | PRINT expr_list { Print($2) }
  (* Instruction conditionnelle "si" avec opérateur relationnel et instruction *)
  | IF expression relop expression THEN instr { If($2, $3, $4, $6) }
  (* Instruction pour aller a une autre ligne *)
  | VAVERS expression { Goto($2) }
  (* Instruction pour lire une liste de variables *)
  | INPUT var_list { Input($2) }
  (* Instruction pour assigner une expression a une variable *)
  | VAR ASSIGN expression { Assign($1, $3) }
  (* Instruction pour terminer le programme *)
  | END { End }
  (* Instruction pour ajouter un commentaire *)
  | REM STRING { Rem($2) }
  (* Instruction pour ajouter un retour chariot *)
  | NL { Newline }

expr_list:
  (* Une liste d'expressions peut etre un seul element d'expression *)
  | expr_list_item { [$1] }
  (* Ou une liste d'expressions suivie d'une virgule et d'un autre element d'expression *)
  | expr_list COMMA expr_list_item { $1 @ [$3] }

expr_list_item:
  (* Un element de liste d'expressions peut etre une chaine de caracteres *)
  | STRING { StringExprItem($1) }
  (* Ou une expression *)
  | expression { ExprItem($1) }

var_list:
  (* Une liste de variables peut etre une seule variable *)
  | VAR { [$1] }
  (* Ou une liste de variables suivie d'une virgule et d'une autre variable *)
  | var_list COMMA VAR { $1 @ [$3] }

expression:
  (* Une expression peut etre un terme *)
  | simple_expression { $1 }
  (* Ou une expression additionnee a un terme *)
  | expression PLUS simple_expression { BinOp(Plus, $1, $3) }
  (* Ou une expression soustraite a un terme *)
  | expression MINUS simple_expression { BinOp(Minus, $1, $3) }

simple_expression:
  (* Une expression simple peut etre un terme *)
  | term { $1 }
  (* Ou une expression simple multiplie par un terme *)
  | simple_expression TIMES term { BinOp(Times, $1, $3) }
  (* Ou une expression simple divise par un terme *)
  | simple_expression DIVIDE term { BinOp(Divide, $1, $3) }

term:
  (* Un terme peut etre un facteur *)
  | factor { $1 }
  (* Ou un facteur negatif *)
  | MINUS factor { UnOp(Neg, $2) }

factor:
  (* Un facteur peut etre un nombre *)
  | NUMBER { Number($1) }
  (* Ou une variable *)
  | VAR { Var($1) }
  (* Ou une expression entre parentheses *)
  | LPAREN expression RPAREN { $2 }
