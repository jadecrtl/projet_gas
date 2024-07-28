open Ast
exception EndOfExecution

(* Table de symboles pour stocker les valeurs de chaque variable *)
let sym_table : (string, int) Hashtbl.t = Hashtbl.create 10

(* Fonction pour obtenir la valeur d'une variable *)
let get_var_value v =
  try Hashtbl.find sym_table v (* Tenter de trouver la valeur de la variable dans la table *)
  with Not_found -> failwith ("Variable " ^ v ^ " not defined") (* Erreur si la variable n'est pas definie *)

(* Fonction pour definir la valeur d'une variable *)
let set_var_value v i =
  Hashtbl.replace sym_table v i (* Remplace ou ajoute la valeur de la variable dans la table *)

(* Variable globale pour suivre la ligne courante *)
let global_current_line = ref 0

(* Lire a partir d'un fichier passe en argument *)
let lexbuf = 
  if Array.length Sys.argv > 1 then
    Lexing.from_channel (open_in Sys.argv.(1)) (* Lire le fichier passe en argument *)
  else begin
    print_endline "No input file provided. Reading from stdin."; (* Lire a partir de stdin si aucun fichier n'est passe *)
    Lexing.from_channel stdin (* Lire a partir de stdin *)
  end

(* Structure pour stocker le résultat d'une instruction *)
type instr_result = {
  result : int;
  jumped : bool; (* Indique si un saut a ete effectue *)
}

(* Fonction recursive pour evaluer les expressions *)
let rec eval_expr = function
  | Number i -> i
  | Var v -> get_var_value v (* Obtenir la valeur de la variable *)
  | Expr e -> eval_expr e
  | StringExpr _ -> failwith "String expressions cannot be evaluated to int"
  | UnOp (op, e) -> 
    let v = eval_expr e in
    (match op with
    | Neg -> -v)
  | BinOp (op, e1, e2) ->
    let v1 = eval_expr e1 in
    let v2 = eval_expr e2 in
    match op with
    | Plus -> v1 + v2
    | Minus -> v1 - v2
    | Times -> v1 * v2
    | Divide -> v1 / v2

(* Fonction pour evaluer et afficher les elements d'expression *)    
let eval_expr_item = function
  | StringExprItem s -> print_string s (* Afficher la chaine de caracteres *)
  | ExprItem e -> 
    let value = eval_expr e in 
    print_int value (* Afficher la valeur de l'expression *)

(* Fonction recursive pour evaluer les instructions *)    
let rec eval_instr sorted_lines = function
  | If (cond, relop, expr, instr) ->
    let cond_value = eval_expr cond in
    let expr_value = eval_expr expr in
    let comparison = match relop with
      | Less -> cond_value < expr_value
      | Greater -> cond_value > expr_value
      | Equal -> cond_value = expr_value
      | Different -> cond_value <> expr_value
    in
    if comparison then eval_instr sorted_lines instr else { result = 0; jumped = false } (* Evaluer l'instruction si la comparaison est vraie *)

  | Goto(expr) ->
    let line = eval_expr expr in
    if not (List.exists (fun (Ligne (n, _)) -> n = line) sorted_lines) then
      failwith ("Erreur : ligne cible " ^ string_of_int line ^ " non trouvée.");
    global_current_line := line;
    { result = 0; jumped = true } (* Mettre a jour la ligne courante et indiquer qu'un saut a ete effectue *)

  | Input(vars) ->
    let input_values = List.map (fun _ -> 
      print_string "Entrez une valeur pour la variable: "; flush stdout;
      read_int ()) vars in (* Lire les valeurs d'entree pour chaque variable *)
    List.iter2 set_var_value vars input_values;  (* Assigner les valeurs lues aux variables correspondantes *)
    { result = 0; jumped = false }  

  | Assign (var, expr) ->
    let value = eval_expr expr in
    Printf.printf "Assigning %d to %s\n" value var; (* Debug print *)
    set_var_value var value; (* Assigner la valeur evaluee a la variable *)
    { result = 0; jumped = false }

  | Print expr_list -> 
    List.iter (fun expr_item -> eval_expr_item expr_item; print_string " ") expr_list; (* Evaluer et afficher chaque element d'expression *)
    print_newline ();
    { result = 0; jumped = false }

  | Newline ->
    print_newline (); (* Afficher une nouvelle ligne *)
    { result = 0; jumped = false }

  | Rem _ ->
    { result = 0; jumped = false } (* Ignorer les commentaires *)

  | End ->
    raise EndOfExecution (* Lever une exception pour indiquer la fin de l'execution *)

(* filtrer les lignes du programme en ne gardant que la dernière occurrence de chaque numéro de ligne *)
let filter_last_occurrences lines =
  let line_map = List.fold_left (fun acc (Ligne (num, instr)) ->
      Hashtbl.replace acc num instr; (* Remplacer les instructions pour chaque numero de ligne *)
      acc
    ) (Hashtbl.create (List.length lines)) lines
  in
  Hashtbl.fold (fun num instr acc -> (Ligne (num, instr)) :: acc) line_map [] (* Convertir la table en liste de lignes *)

(* Fonction pour evaluer un programme *)
let eval_programme (Programme lines) =
  let unique_lines = filter_last_occurrences lines in (*Obtenir les lignes uniques*)
  let sorted_lines = List.sort (fun (Ligne (n1, _)) (Ligne (n2, _)) -> compare n1 n2) unique_lines in (* Trier les lignes par numero *)
  global_current_line := (match sorted_lines with
                          | (Ligne (n, _))::_ -> n
                          | [] -> 0); (* Initialiser la ligne courante *)
  
  let find_line_by_number num =
    List.find_opt (fun (Ligne(ln, _)) -> ln = num) sorted_lines (* Recherche de la ligne correspondante *)
  in
  
  (* Fonction recursive pour executer chaque ligne du programme *)
  let rec execute_line current_line =
    match current_line with
    | None -> ()  (* Fin du programme si aucune ligne correspondante *)
    | Some (Ligne(ln, instrs)) ->
      Printf.printf "Executing line %d\n" ln; (* Affichage de debogage *)
      let instr_results = List.map (eval_instr sorted_lines) instrs in
      let jumped = List.exists (fun res -> res.jumped) instr_results in
      if jumped then
        execute_line (find_line_by_number !global_current_line) (* Execution de la ligne du saut *)
      else
        let next_line_number = (try List.find (fun (Ligne (n, _)) -> n > ln) sorted_lines |> (fun (Ligne (n, _)) -> n) with Not_found -> ln + 1) in
        global_current_line := next_line_number;
        execute_line (find_line_by_number next_line_number) (* Exécution de la ligne suivante *)
  in
  
  execute_line (find_line_by_number !global_current_line)

(* Analyse syntaxique du fichier d'entree pour obtenir l'AST *)  
let ast = 
  try
    let ast = Parser.programme Lexer.token lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (show_programme ast); (* Affichage debogage *)
    ast
  with ex ->
    Printf.printf "Error while parsing: %s\n" (Printexc.to_string ex); (* Afficher une erreur si l'analyse echoue *)
    exit 1

(* Execution du programme principal *)    
let _ = 
  try 
    eval_programme ast;
    print_newline ();
  with
    | EndOfExecution -> print_string "Execution ended by FIN.\n" (* Afficher un message si le programme se termine par FIN *)
    | ex -> 
        print_string "Unhandled exception occurred: "; (* Afficher un message pour toute autre exception non geree *)
        print_string (Printexc.to_string ex);
        print_newline ();
