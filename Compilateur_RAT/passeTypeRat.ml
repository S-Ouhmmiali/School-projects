(* Module de la passe de gestion des types *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Type
  open Exceptions
  open Ast
  open AstType

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme
 
(* analyse_type_typenomme : AstTds.Typenomme -> _ *)
(* Paramètre AstTds.Typenomme : le type nommé  à analyser *)
(* Vérifie la bonne utilisation des types pour le type nommé global et tranforme le type nommé
en un type npmmé  de type AstTds.Typenomme *)
(* Erreur si mauvaise utilisation des types *) 
  let analyse_type_typenomme (AstTds.Typenomme(info_ast,_)) =
    begin
     match (info_ast_to_info info_ast) with
          | InfoTypeDef (nom,_) -> modifier_type_info (TypeNomme nom) info_ast;
          | _ -> raise (PasPointeur)
    end 

(* analyse_type_affectable : AstTds.affectable -> AstType.affectable *)
(* Paramètre aff : l'affectable à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'affectable
en un affectable de type AstType.affectable et son type *)
(* Erreur si mauvaise utilisation des types *)
  let rec analyse_type_affectable aff = 
    match aff with
    |AstTds.Variable(info_ast) -> begin 
                    let info = info_ast_to_info info_ast in 
                  match info with
                      |Tds.InfoVar(_,t,_,_) -> modifier_type_info t info_ast;
                                               (Variable(info_ast),t) 
                      |Tds.InfoConst(_,_) -> 
                                              (Variable(info_ast),Int)
                      | _ -> failwith "" 
                  end
      |AstTds.Deref(a) -> let na,ta = analyse_type_affectable a in
                         begin match (na,ta ) with
                          |(na', Pointeur ta') -> (Deref(na'),ta')
                          | _ -> failwith ""
                        end
      | AstTds.Acces(a,info_ast) -> let na,_ = analyse_type_affectable a in
                 (match info_ast_to_info info_ast with
                  |InfoVar(_,t,_,_) -> modifier_type_info t info_ast;
                                        (Acces(na,info_ast),t) 
                  | _-> failwith "3" )
                                     
(* analyse_type_expression : AstTds.expression -> AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et transforme l'expression
en une expression de type AstType.expression et son type *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e = 
     match e with  
      |AstTds.Entier(n) -> (Entier(n),Int)
      |AstTds.Booleen(b) -> (Booleen(b),Bool)
      |AstTds.Binaire(op,e1,e2) ->
                                      let (x1,t1) = analyse_type_expression e1 in
                                       let (x2,t2) = analyse_type_expression e2 in
                                         if est_compatible t1 t2 then
                                            begin
                                            match op, t1, t2 with
                                            |(Plus,Int,Int) -> (Binaire(PlusInt,x1,x2),Int)
                                            |(Mult,Int,Int) -> (Binaire(MultInt,x1,x2),Int)
                                            |(Equ,Int,Int) -> (Binaire(EquInt,x1,x2),Bool)
                                            |(Fraction,Int,Int) -> (Binaire(Fraction,x1,x2),Rat)
                                            |(Mult,Rat,Rat) -> (Binaire(MultRat,x1,x2),Rat)
                                            |(Plus,Rat,Rat) -> (Binaire(PlusRat,x1,x2),Rat)
                                            |(Inf,Int,Int) -> (Binaire(Inf,x1,x2),Bool)
                                            |(Equ,Bool,Bool) -> (Binaire(EquBool,x1,x2),Bool)
                                            | _ -> raise (TypeBinaireInattendu(op,t1,t2))
                                          end
                                          else raise (TypeBinaireInattendu(op,t1,t2)) 

      |AstTds.Unaire(op,e) -> 
                                  let (x1,t1) = analyse_type_expression e in
                                  if (t1 = Rat) then
                                        match op with
                                        |Numerateur -> (Unaire(Numerateur,x1),Int)
                                        |Denominateur -> (Unaire(Denominateur,x1),Int)
                                  else raise (TypeInattendu(t1,Rat))
     
      |AstTds.AppelFonction(info_ast,es) -> begin match Tds.info_ast_to_info info_ast with 
                       | Tds.InfoFun (_,tr,ts) -> 
                        let es', ts' = List.split (List.map analyse_type_expression es ) in 
                           if est_compatible_list ts' ts 
                              then 
                              (AppelFonction (info_ast,es'),tr) 
                           else raise  (TypesParametresInattendus(ts,ts'))
                       | _ -> assert false
                          end
      |AstTds.Null -> Null, Undefined
      |AstTds.Initilisation(t) -> (Initilisation(t),Pointeur t)
      |AstTds.Addresse(info_ast) -> begin match info_ast_to_info info_ast with
                                    |InfoVar(_,t,_,_) -> modifier_type_info t info_ast; (Addresse(info_ast),Pointeur t)
                                    | _ -> failwith ""
                                  end
      |AstTds.Val(a) -> let na,ta = analyse_type_affectable a in
                        (Val(na),ta)
      |AstTds.Listexp(l) -> let ln = List.map(analyse_type_expression ) l in 
                             let l1,l1' = List.split ln in 
                             let l2 = List.map(fun x -> (x,"")) l1' in 
                             (Listexp(l1),Struct(l2))
            
      

                       

                            
                  


(* analyse_type_instruction : AstTds.instruction -> tds -> AstType.instruction *)
(* Paramètre tr : type de retour *)
(* Paramètre instr : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)

let rec analyse_type_instruction tr instr = 
  match instr with                         
  

                                                                              
  |AstTds.Conditionnelle(e,b1,b2) -> let x,t = analyse_type_expression e in
                                        if ( t=Bool) then
                                           let x1 = analyse_type_bloc tr b1 in
                                           let x2 = analyse_type_bloc tr b2 in
                                           Conditionnelle(x,x1,x2)
                                        else raise (TypeInattendu(t,Bool))
   
  |AstTds.TantQue(e,b) -> let x,t= analyse_type_expression e in
                              if ( t=Bool) then
                               let x1 = analyse_type_bloc tr b in
                               TantQue(x,x1)
                              else raise (TypeInattendu(t,Bool))
  |AstTds.Retour(e) -> let x,t = analyse_type_expression e in
                   if tr = Undefined then 
                   raise (RetourDansMain)
                   else 
                      if est_compatible tr t then
                            Retour(x)
                      else
                            raise(TypeInattendu(t,tr))
  |AstTds.Empty -> Empty
  
  | AstTds.Declaration(ls,t,info_ast,e) -> 
                                 let x1,t1 = analyse_type_expression e in
                                 (match t1 with 
                                 |Struct(l) -> (match t with
                                             |Struct(l') -> let l1,_ = List.split l in
                                                            let l1',_ = List.split l' in
                                                            if (est_compatible_list l1 l1') then
                                                                let _ = modifier_type_info t info_ast in
                                                                     Declaration(ls,info_ast,x1)
                                                            else raise(TypeInattendu(t1,t))
                                            | _ -> failwith "")
                                  | _ -> if est_compatible t1 t then
                                   let _ = modifier_type_info t info_ast in
                                     Declaration(ls,info_ast,x1)
                                      else
                                       raise(TypeInattendu(t1,t)))

 |AstTds.Affichage(e) -> let x1,t1 = analyse_type_expression e in
        begin
            match t1 with
                      |Int -> AffichageInt(x1)
                      |Rat -> AffichageRat(x1)
                      |Bool -> AffichageBool(x1)
                      |_ -> assert false
            end
  |AstTds.Affectation(a,e) -> let na,ta = analyse_type_affectable a in
                              let ne,te = analyse_type_expression e in
                              if (est_compatible ta te) then
                                   Affectation(na,ne)
                              else raise(TypeInattendu(te,ta))
  |AstTds.Assignation(a,e) -> let na,ta = analyse_type_affectable a in
                              let ne,te = analyse_type_expression e in
                              if (est_compatible ta te) then
                                  Affectation(na,ne)
                              else raise(TypeInattendu(te,ta))
  |AstTds.DeclTypeNomme(ia,t) -> modifier_type_info t ia; Empty
  
                                    
  
  
                                         
    
(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre tr : type de retour *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc
en un bloc de type AstType.bloc *)
and analyse_type_bloc tr li =
 let nli =  List.map (analyse_type_instruction tr) li in
   nli  




(* analyse_tds_fonction : AsTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type AstType.fonction *)
let analyse_type_fonction (AstTds.Fonction(t,n,lp,li)) =

            let llp = List.map(fun (x,y) -> let () =  modifier_type_info x y in (x,y) ) lp in
            let tpl = List.map (function x,_ -> x) llp
            in let () = modifier_type_fonction_info t tpl n
            in let v2 = analyse_type_bloc t li in
            let nlp = List.map (function _,y -> y) lp
            in Fonction(n,nlp,v2)

(* analyser : AstTds.ast -> AstType.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.ast *)

let analyser (AstTds.Programme (typesnommes,fonctions,prog)) =
  let  _ = List.map (analyse_type_typenomme ) typesnommes in 
  let nf = List.map (analyse_type_fonction) fonctions in 
  let nb = analyse_type_bloc Undefined prog in
  Programme (nf,nb)

end
