(* Module de la passe de gestion des types *)

module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Tds
  open Type
  open Exceptions
  open Code
  open Ast
  open AstType

  type t1 = Ast.AstPlacement.programme
  type t2 = string

  (* Fonction pour concaténer les codes d'une liste d'éxpressions*)
  let rec codeliste l =
    match l with
     |[] -> ""
     |t::q -> t^(codeliste q) 
  (* Fonction pour pour retourner la valeur du pop (décalage mémoire en haut *) 
  (* de la pile qui n'affecte pas les autres éléments)*)
  let pop_calcul instr  =
    match instr with
    | AstType.Declaration (_,info_ast,_) -> 
      begin
        match info_ast_to_info info_ast with
        | InfoVar(_,t,_,_) -> getTaille t
        | InfoFun(_,t,_) -> getTaille t
        | InfoConst(_,_) -> getTaille Int
        | _ -> failwith ""
      end
    | _ -> 0

(* analyse_type_affectable : AstType.affectable -> string *)
(* Paramètre aff : l'affectable à analyser *)
(* Donne le code tam de l'affectable en distinguant le cas en affectation et en evaluation *)
(* Erreur si mauvaise utilisation des types *)
  let rec analyse_affectable aff b  =
    match aff, b with
    |AstType.Variable(info_ast), false  ->                               
                                  begin match info_ast_to_info info_ast with
                                  |InfoVar(_,t,d,r) -> "LOAD "^"(" ^string_of_int(getTaille t)^") " ^ (string_of_int d) ^ "[" ^ r ^ "]"^"\n",t
                                  |InfoConst (_,n) -> "LOADL "^string_of_int(n)^"\n",Int
                                  |_ -> failwith ""
                                  end
    | AstType.Deref(a), false  -> let na,ta  = analyse_affectable a false   in                         
                        begin  match ta with
                        |Pointeur t -> (na^"LOADI ("^(string_of_int(getTaille t))^")\n",t)
                        | _ -> raise (PasPointeur)
                        end 
    | AstType.Acces(a,n), false -> let na,_  = analyse_affectable a false   in
                            begin match info_ast_to_info n with
                            |InfoVar(_,t,d,r) -> na^"LOAD "^"(" ^string_of_int(getTaille t)^") " ^ (string_of_int d) ^ "[" ^ r ^ "]"^"\n",t
                            |_ -> failwith ""
                            end
                                   
    |AstType.Variable(info_ast), true -> begin match info_ast_to_info info_ast with 
                        (* On peut affecter que les variables *)
                      |InfoVar(_,t,d,r) -> "STORE " ^ "("^ string_of_int (getTaille t)^ ") "^ string_of_int d ^ "[" ^ r ^ "]"^"\n",t
                      |_ -> failwith "" 
                      end
    | AstType.Deref(a),true -> let na,ta = (analyse_affectable a true)  in
                      na^"STOREI ("^(string_of_int(getTaille ta))^")\n " ,ta
    | AstType.Acces(_,n), true -> begin match info_ast_to_info n with 
                                  (* On peut affecter que les variables *)
                                |InfoVar(_,t,d,r) -> "STORE " ^ "("^ string_of_int (getTaille t)^ ") "^ string_of_int d ^ "[" ^ r ^ "]"^"\n",t
                                |_ -> failwith "" 
                                end
                      
    (* analyse_expression : AstType.expression -> string*)
(* Paramètre e : l'expression à analyser *)
(* donne le code tam de l'expression *)
  let rec analyse_expression expr = 
    match expr with  
    |Binaire(b,e1,e2) -> let x1 = (analyse_expression e1) in 
                        let x2 = (analyse_expression e2) in
                begin match b with
                | PlusInt -> x1 ^ x2 ^ "SUBR " ^"IAdd\n" 
                | MultInt -> x1 ^ x2 ^ "SUBR " ^"IMul\n" 
                | MultRat -> x1 ^ x2 ^"CALL (ST) RMul\n" 
                | PlusRat -> x1 ^ x2 ^"CALL (ST) RAdd\n" 
                | Fraction -> x1 ^x2 ^"CALL (ST) norm\n"
                | EquInt -> x1 ^x2 ^ "SUBR IEq\n" 
                | EquBool ->  x1 ^x2 ^ "SUBR IEq\n" 
                | Inf -> x1 ^ x2^ "SUBR ILss \n"
        end
   |Unaire(b,e)  -> begin 
     match b with
        |Numerateur -> (analyse_expression e)^"POP (0) 1\n"
        |Denominateur -> (analyse_expression e)^"POP (1) 1\n"
      end 
   |Entier(n) -> "LOADL "^string_of_int(n)^"\n"
   |Booleen(b) -> begin match b with 
                  |true -> "LOADL 1\n"
                  |false -> "LOADL 0\n"
                 end
   |AppelFonction(info_ast, le) ->
        begin
        match info_ast_to_info info_ast with
        | InfoFun(n,_,_) -> codeliste (List.map (analyse_expression ) le) ^ "CALL (ST) " ^ n ^ "\n"
        | _ -> failwith "Erreur"
        end

   |Initilisation(t) -> "LOADL "^(string_of_int(getTaille t))^"\n"^"SUBR MAlloc\n"
   |Val(a) ->  let code,_ = analyse_affectable a false in code 
   |Addresse(info_ast) -> begin match info_ast_to_info info_ast with
                          |InfoVar(_,_,d,r) -> "LOADA "^(string_of_int d)^"["^r^"]\n"
                          | _ -> failwith("")  
                           end  
  |Null -> "LOADL 0\n"  
  |Listexp(l) -> let nl = List.map(analyse_expression) l in codeliste nl
  |_ -> ""                                   
 (* analyse_expression : (Tds.info_ast * expression ) -> string*)
(* Paramètre e : l'expression associé à l'info de la variable de struct *)
(* Paramètre info_ast : l'info_ast de la variable de struct *)
(* donne le code tam de la déclaration de chaque variable du struct  *)
let analyse_struct (info_ast,e) = 
  (match info_ast_to_info info_ast with
  |InfoVar(_,t,d,r) -> "PUSH " ^string_of_int(getTaille t) ^"\n" ^analyse_expression e 
   ^ "STORE " ^ "("^ string_of_int (getTaille t)^ ") "^ (string_of_int d) ^ "[" ^ r ^ "]\n"
  | _ -> failwith "" )
 (* analyse_instruction : AstType.instruction -> string*)
(* Paramètre e : l'instruction à analyser *)
(* donne le code tam de l'instruction *)
let rec analyse_instruction  instr  = 
  match instr with                         
  | Declaration(ls,info_ast,e) -> begin match info_ast_to_info info_ast with 
                                      |InfoVar(_,t,d,r) -> begin match t with 
                                                |Type.Struct _ -> (match e with 
                                                                |AstType.Listexp l -> let l0 = List.combine ls l in
                                                                                      codeliste ((List.map(analyse_struct)l0))
                                                                |_ -> failwith "")


                                                | _-> "PUSH " ^string_of_int(getTaille t) ^"\n"^analyse_expression e ^ "STORE " 
                                                ^ "("^ string_of_int (getTaille t)^ ") "^ (string_of_int d) ^ "[" ^ r ^ "]\n"
                                        
                                                
                                                   end 
                                          
                                      
                                      | _ -> failwith "" 
                                end
 
  | Affectation(a,e) -> let na,_ = analyse_affectable a true in
                        (analyse_expression e)^na
  | Assignation(a,e) -> let na,_ = analyse_affectable a true in
                        (analyse_expression e)^na
  |TantQue (b,e) ->    let xb  = analyse_expression b in
                       let xe,pop_bloc = analyse_bloc e in
                       let whileDebut = getEtiquette () in
                       let whileFin = getEtiquette () in                      
                      whileDebut^"\n"^xb^"JUMPIF(0) "^ whileFin^"\n" ^
                      xe ^"POP (0)"^(string_of_int pop_bloc)^"\n"^
                      "JUMP "^ whileDebut ^"\n"^whileFin^"\n"
  |Conditionnelle(b,e1,e2) -> let x1 = analyse_expression b in
                              let xb1, pop_bloc1 = analyse_bloc e1 in
                              let xb2, pop_bloc2= analyse_bloc e2 in
                              let etiq_else = (getEtiquette ()) in
                              let etiq_FinIf = (getEtiquette ()) in
                              x1^
                              "JUMPIF (0) "^etiq_else^
                              "\n"^xb1^
                              "POP (0)"^(string_of_int pop_bloc1)^"\n"^
                              "JUMP "^etiq_FinIf^
                              "\n"^etiq_else^
                              "\n"^xb2^
                              "POP (0)"^(string_of_int pop_bloc2)^"\n"
                              ^etiq_FinIf^"\n"
  |AffichageInt(e) -> analyse_expression e ^ "SUBR IOut\n"
  |AffichageBool(e) -> analyse_expression e ^ "SUBR Bout\n"
  |AffichageRat(e) -> analyse_expression e ^ "CALL (ST) ROut\n"
  |Retour(e) -> analyse_expression e ^"\n"
  |_ -> ""                            
    
 
 (* analyse_bloc : AstType.bloc -> string*)
(* Paramètre li : la liste des instructions à analyser *)
(* donne le code tam du bloc*)
and analyse_bloc li =
let l = List.map( analyse_instruction) li in
let taille = List.fold_right (fun i ti -> (pop_calcul i) + ti) li 0 in
(codeliste l),taille
 




 (* analyse_fonction : AstType.fonction -> string*)
(* Paramètre Fonction (_) : la fonction à analyser *)
(* donne le code tam de la fonction *)
let analyse_fonction (AstPlacement.Fonction(n,_,li)) = 
  match info_ast_to_info n with
  |InfoFun(n,t,lt) -> 
         let x1,pop = analyse_bloc li in
          n^"\n"^x1^
         "POP ("^(string_of_int (getTaille t))^")"^(string_of_int pop)^
         "\nRETURN ("^(string_of_int (getTaille t))^") "^
         (string_of_int(List.fold_left (fun nb t -> (getTaille t) + nb) 0 lt))^"\n"            
  |_ -> failwith ""

         





  (* analyse : AstType.Programme -> string*)
(* Paramètre programme(_) : le programme à analyser *)
(* donne le code tam du programme*)   
let analyser (AstPlacement.Programme (fonctions,prog)) =
  let entete_prog = getEntete () in
  let nfl = List.map (analyse_fonction) fonctions in
  let nf = codeliste nfl in
  let nb, pop = (analyse_bloc prog)
  in entete_prog^"LABEL "^nf^"main\n" ^ nb ^"POP (0)"^(string_of_int pop)^ "\n\nHALT"

end
