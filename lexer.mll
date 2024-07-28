{
  open Parser
}

(* Definition des ensembles de caracteres *)
let chiffre = ['0'-'9']
let var = ['A'-'Z']
let layout = [' ' '\t' '\r']
let relop = ['<' '>' '=']
let string_c = [' ' '\\' ''' ';' ':' '.' '_' '(' ')' 'a'-'z' 'A'-'Z']

(* Regle principale pour analyser les tokens *)
rule token = parse
  | layout+            { token lexbuf } (* Ignore les espaces et tabulations et appele a nouveau la fonction de tokenisation *)
  | chiffre+ as nombre { NUMBER(int_of_string nombre) }
  | var                { VAR (Lexing.lexeme lexbuf) }
  | "<>" | "><"        { RELOP Different }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIVIDE }
  | '='                { ASSIGN }
  | '<'                { RELOP Less }
  | '>'                { RELOP Greater }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | ','                { COMMA }
  | "NL"               { NL }
  | ['\n']             { CR }
  | "IMPRIME"          { PRINT }
  | "SI"               { IF }
  | "ALORS"            { THEN }
  | "VAVERS"           { VAVERS }
  | "ENTREE"           { INPUT }
  | "FIN"              { END }
  | "REM"              { REM }
  | '"'                { let str = read_string lexbuf in STRING str } (* Debut d'une chaine de caracteres *)
  | eof                { EOF }
  | _                  { failwith "unexpected character" }

(* Fonction pour lire une chaine de caracteres *)
and read_string = parse
  | '"'               { "" }
  | '\\' '"'          { "\"" ^ read_string lexbuf } (* Gerer le caractere d'echappement pour les guillemets *)
  | string_c+ as c    { c ^ read_string lexbuf } (* Lire les caracteres valides pour une chaine de caracteres *)
  | _                 { raise (Failure "String lexing error") } (* Gerer les erreurs dans les chaines de caracteres *)
