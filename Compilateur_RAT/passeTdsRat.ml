(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme


(* Fonction pour obtenir le  type réel de chaque type *)
  let rec get_type tds t =
    match t with 
    | Type.TypeNomme(n) -> 
      begin match chercherGlobalement tds n with
        | None -> raise (IdentifiantNonDeclare n)
        | Some info -> 
          (match info_ast_to_info info with
            | InfoTypeDef(_,typ) -> get_type tds typ
            | _ -> failwith ""
         )
       end
    |Struct(l) -> let _ = (List.map(fun (a,b)->
                                    let infov = InfoVar (b,Undefined, 0, "") in
                                    (* Création du pointeur sur l'information *)
                                    let iaa = info_to_info_ast infov in
                                    let nt = get_type tds a in
                                    (* Ajout de l'information (pointeur) dans tds *)
                                    ajouter tds b iaa;  
                                    (nt, iaa)                  
                          )) l in Type.Struct(l)
                   
    | _ -> t
(* analyse_tds_typenomme : AstSyntaxe.Typenomme -> AstTds.Typenomme *)
(* Paramètre AstTds.Typenomme : le type nommé  à analyser *)
(* Vérifie la bonne utilisation des identifiants pour le type nommé global et tranforme le type nommé
en un type npmmé  de type AstTds.Typenomme *)
(* Erreur si mauvaise utilisation des identifiants *)   
let analyse_tds_typenomme tds (AstSyntax.Typenomme(n,t)) = 
      begin
        match (chercherGlobalement tds n) with
        | Some _ -> raise (DoubleDeclaration n)
        | None -> 
            let nt  = get_type tds t in
            let info = InfoTypeDef (n,nt) in
            let ia = info_to_info_ast info in
            let () = ajouter tds n ia in
            Typenomme(ia,nt)
      end 
 
(* analyse_tds_affectable : AstSyntaxe.affectable -> AstTds.affectable *)
(* Paramètre aff : l'affectable à analyser *)
(* Vérifie la bonne utilisation des identifiants dans le cas d'une affectation et tranforme l'affectable
en un affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
  let rec  analyse_tds_affectable_affectation tds aff =
    match aff with
    | AstSyntax.Deref(a) -> let na = analyse_tds_affectable_affectation tds a in
                            Deref(na)
    | AstSyntax.Variable(n) -> 
        begin 
          match (chercherGlobalement tds n) with
          |None -> raise (IdentifiantNonDeclare n)
          |Some info_ast -> begin match info_ast_to_info info_ast with
                       |InfoVar _ -> Variable(info_ast)
                       | _ ->  raise (MauvaiseUtilisationIdentifiant n) 
                              end
        end

    | AstSyntax.Acces(a,n) -> let na = analyse_tds_affectable_affectation tds a in
                     (match na with 
                     |Variable info_ast -> (match info_ast_to_info info_ast with
                                        |InfoVar(nom,_,_,_) -> begin 

                                          match (chercherGlobalement tds ("("^nom^"."^n^")")) with
                                          |None -> raise (IdentifiantNonDeclare n)
                                          |Some info_ast -> begin match info_ast_to_info info_ast with
                                                      |InfoVar _ -> Acces(na,info_ast)
                                                      | _ ->  raise (MauvaiseUtilisationIdentifiant n) 
                                                              end
                                        
                                        end
                                        | _ -> failwith "2"
                                        
                     
                     
                     
                     
                                        )
                    | _ -> failwith "3"
                     
                     
                     )
     
(* analyse_tds_affectable_value : AstSyntaxe.affectable -> AstTds.affectable *)
(* Paramètre aff : l'affectable à analyser *)
(* Vérifie la bonne utilisation des identifiants dans le cas d'une evaluation et tranforme l'affectable
en un affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
  let analyse_tds_affectable_value tds aff =
    match aff with
    | AstSyntax.Deref(a) -> let na = analyse_tds_affectable_affectation tds a in
                            Deref(na)
    | AstSyntax.Variable(n) -> 
        begin 
          match (chercherGlobalement tds n) with
          |None -> raise (IdentifiantNonDeclare n)
          |Some info_ast -> begin match info_ast_to_info info_ast with
                       |InfoVar _ -> Variable(info_ast)
                       |InfoConst _ -> Variable(info_ast)
                       | _ ->  raise (MauvaiseUtilisationIdentifiant n) 
                              end
        end 
    | AstSyntax.Acces(a,n) -> let na = analyse_tds_affectable_affectation tds a in
    (match na with 
    |Variable info_ast -> (match info_ast_to_info info_ast with
                       |InfoVar(nom,_,_,_) -> begin 

                         match (chercherGlobalement tds ("("^nom^"."^n^")")) with
                         |None -> raise (IdentifiantNonDeclare n)
                         |Some info_ast -> begin match info_ast_to_info info_ast with
                                     |InfoVar _ -> Acces(na,info_ast)
                                     | _ ->  raise (MauvaiseUtilisationIdentifiant n) 
                                             end
                       
                       end
                       | _ -> failwith "2"
                       
    
    
    
    
                       )
                  | _ -> failwith "3"
                    
                    
                    )
 
               
(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = 
     match e with
     |AstSyntax.AppelFonction(nom,le) ->
          begin
            match chercherGlobalement tds nom with
               |None -> raise (IdentifiantNonDeclare nom)
               |Some info ->
                       match info_ast_to_info info with
                          |InfoFun _ ->
                                   let x = List.map (analyse_tds_expression tds) le in
                                            AppelFonction(info,x)
                          | _ -> 
                                     raise (MauvaiseUtilisationIdentifiant nom) 
          end    
      |AstSyntax.Entier(n) -> Entier(n)
      |AstSyntax.Booleen(b) -> Booleen(b)
      |AstSyntax.Binaire(op,e1,e2) ->
                                      let x1 = analyse_tds_expression tds e1 in
                                       let x2 = analyse_tds_expression tds e2 in
                                         Binaire(op,x1,x2)
      |AstSyntax.Unaire(op,e) -> 
                                      let x = analyse_tds_expression tds e in
                                        Unaire(op,x)
      |AstSyntax.Null -> Null
      |AstSyntax.Initilisation(t) -> Initilisation(get_type tds t)
      |AstSyntax.Addresse(n) -> let info_ast = chercherGlobalement tds n in
                                begin match info_ast with
                                   |None -> raise (IdentifiantNonDeclare n)
                                   | Some a -> Addresse(a)
                                end
      |AstSyntax.Val(a) -> let res = analyse_tds_affectable_value tds a in
                            Val(res)
      |AstSyntax.Listexp(l) -> let res  = List.map(analyse_tds_expression tds) l in Listexp(res)
      
                            


                

                            
                  


(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (match t with 
            |Struct l -> let l0 = l in
            let ls = (List.map(fun (a,b)->
            let infov = InfoVar (n^"."^b,a, 0, "") in
            (* Création du pointeur sur l'information *)
            let iaa = info_to_info_ast infov in
            
            (* Ajout de l'information (pointeur) dans tds *)
            ajouter tds ("("^n^"."^b^")") iaa; iaa)) l0 in
            let _ = List.map (fun (t0,s) -> (t0,("("^n^"."^s^")"))) l in
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,t, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
            Declaration (ls,Struct(l), ia, ne) 

            |_ ->  (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let t2 = get_type tds t in
            let info = InfoVar (n,t2, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            let ls =[] in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
            Declaration (ls,t2, ia, ne) 
            
            
            
            
            
            
            
            
            
            )
           
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->
      let aff = analyse_tds_affectable_affectation tds a in
      let ne = analyse_tds_expression tds e in
      Affectation(aff,ne)
      
  | AstSyntax.Constante (n,v) -> 
      begin
        match chercherLocalement tds n with
        | None -> 
        (* L'ideanalyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,lintifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc couraise (Identifianant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)
  | AstSyntax.Retour (e) -> 
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in
      Retour (ne)
  | AstSyntax.DeclTypeNomme(n,t) -> 
      begin match chercherLocalement tds n with
          | None ->
              let info = InfoTypeDef (n,t) in
              let ia = info_to_info_ast info in
              ajouter tds n ia;
              DeclTypeNomme(ia,t)
          | Some _ ->
              raise (DoubleDeclaration n)
     end 
  | AstSyntax.Assignation(a,e) ->  let aff = analyse_tds_affectable_affectation tds a in
                                    let nb = AstSyntax.Binaire (AstSyntax.Plus,AstSyntax.Val(a),e) in
                                    let ne = analyse_tds_expression tds nb in
                                   Assignation(aff,ne)
  
  
  
  
  
  
  
  

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
  match chercherGlobalement maintds n with
        | None ->  let x= List.map(fun (a,_)-> a) lp in 
                  let info = InfoFun (n,t,x) in
                  let ia = info_to_info_ast info in
                   ajouter maintds n ia;
                   let tds_fille = creerTDSFille maintds in 
                   let z = (List.map(fun (a,b)->
                       begin 
                          match chercherGlobalement tds_fille b with   
                              |None ->
                                let infov = InfoVar (b,Undefined, 0, "") in
                                (* Création du pointeur sur l'information *)
                                let iaa = info_to_info_ast infov in
                                let nt = get_type tds_fille a in
                                (* Ajout de l'information (pointeur) dans tds_fille *)
                                ajouter tds_fille b iaa;  
                                (nt, iaa) 
                              
                              |Some _ -> raise (DoubleDeclaration b)
                          end                  
                   ) lp ) in
                    (* Analyse des instructions du bloc li avec la tds tds_fille du nouveau bloc 
                     Cette tds est modifiée par effet de bord *)
                   let nli = List.map (analyse_tds_instruction tds_fille ) li in 
                   let tr = get_type maintds t in
                   Fonction(tr,ia,z,nli)

          
        | Some _ -> 
                    raise (DoubleDeclaration n)
          

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (typeNommes,fonctions,prog)) =
  let tds = creerTDSMere () in
  let ntn = List.map (analyse_tds_typenomme tds) typeNommes in
  let nf = List.map (analyse_tds_fonction tds) fonctions in 
  let nb = analyse_tds_bloc tds prog in
  Programme (ntn,nf,nb)

end
