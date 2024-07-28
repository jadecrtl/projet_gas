(* Definition des types pour les operateurs unaires *)
type unop =
  | Neg

(* Definition des types pour les operateurs binaires *)  
type binop =
  | Plus
  | Minus
  | Times
  | Divide

(* Definition des types pour les expressions *)  
type expr =
  | Number of int
  | Var of string
  | StringExpr of string
  | Expr of expr
  | BinOp of binop * expr * expr
  | UnOp of unop * expr

(* Definition des types pour les elements d'expression *)
type expr_item =
  | StringExprItem of string
  | ExprItem of expr

(* Definition des types pour les listes de variables et d'expressions *)  
type var_list = string list
type expr_list = expr_item list

(* Definition des types pour les operateurs de comparaison *)
type relop =
  | Less
  | Greater
  | Equal
  | Different

(* Definition des types pour les instructions *)  
type instr =
  | Print of expr_list
  | If of expr * relop * expr * instr
  | Goto of expr
  | Input of var_list
  | Assign of string * expr
  | End
  | Rem of string
  | Newline

(* Definition des types pour les lignes de programme *)
type ligne = 
  | Ligne of int * instr list

(* Definition des types pour les programmes *)
type programme = 
  | Programme of ligne list

(* Fonctions de conversion en chaine de caracteres *)

(* Conversion d'un operateur unaire en chaine de caracteres *)
let show_unop = function
  | Neg -> "-"  

(* Conversion d'un operateur binaire en chaine de caracteres *)
let show_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"

(* Conversion d'une expression en chaine de caracteres *)  
let rec show_expr = function
  | Number i -> string_of_int i
  | Var v -> v
  | StringExpr s -> "\"" ^ s ^ "\"" 
  | Expr e -> "(" ^ show_expr e ^ ")"
  | BinOp (op, e1, e2) -> "(" ^ show_expr e1 ^ " " ^ show_binop op ^ " " ^ show_expr e2 ^ ")"
  | UnOp (op, e) -> "(" ^ show_unop op ^ show_expr e ^ ")"

(* Conversion d'un element d'expression en chaîne de caracteres *)  
let show_expr_item = function
  | StringExprItem s -> "\"" ^ s ^ "\""
  | ExprItem e -> show_expr e

(* Conversion d'une liste d'expressions en chaine de caracteres *)  
let show_expr_list el = String.concat ", " (List.map show_expr_item el)

(* Conversion d'une liste de variables en chaine de caracteres *)
let show_var_list vl = String.concat ", " vl

(* Conversion d'un operateur relationnel en chaine de caracteres *)
let show_relop = function
  | Less -> "<"
  | Greater -> ">"
  | Equal -> "="
  | Different -> "<>"

(* Conversion d'une instruction en chaine de caracteres *)  
let rec show_instr = function
  | Print el -> "Print(" ^ show_expr_list el ^ ")"
  | If (e1, ro, e2, i) -> "If(" ^ show_expr e1 ^ " " ^ show_relop ro ^ " " ^ show_expr e2 ^ ", " ^ show_instr i ^ ")"
  | Goto e -> "Goto(" ^ show_expr e ^ ")"
  | Input vl -> "Input(" ^ show_var_list vl ^ ")"
  | Assign (v, e) -> v ^ " = " ^ show_expr e
  | End -> "End"
  | Rem s -> "Rem(" ^ s ^ ")"
  | Newline -> "Newline"

(* Conversion d'une concaténation de plusieurs instructions*)
let show_instr_list il = String.concat ", " (List.map show_instr il)

(* Conversion d'une ligne de programme en chaine de caracteres *)  
let show_ligne (Ligne (n, i)) = string_of_int n ^ ": " ^ show_instr_list i

(* Conversion d'un programme en chaine de caracteres *)
let show_programme (Programme l) = String.concat "\n" (List.map show_ligne l)
