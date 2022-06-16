(* Module de la passe de gestion des types *)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Ast

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme
let analyse_info base reg info_ast =
  (match info_ast_to_info info_ast with
  |InfoVar(_,t,_,_) -> Tds.modifier_adresse_info base reg info_ast;
                        Type.getTaille t 
  | _ -> failwith "")

(* analyse_placement_instruction : AstType.instruction -> int *)
  (* Paramètre base : le décalage courant *)
  (* Paramètre reg : l'adresse de registre *)
  (* Paramètre instr : l'instructions à analyser *)
  (* renvoie la base de décalage mémoire *)
  let rec analyse_placement_instruction  base reg instr = 
    match instr with                        
    
  
                                                                                
    |AstType.Conditionnelle(_,b1,b2) -> analyse_placement_bloc base reg b1;
                                        analyse_placement_bloc base reg b2;
                                        0
     
    |AstType.TantQue(_,b) -> analyse_placement_bloc base reg  b;
                            0
    
    | AstType.Declaration(ls,info,_) ->  (match ls with
                                                      |[] -> analyse_info base reg info
                                                      | _ -> let l = info::ls in let _ = List.fold_left(fun base i -> base + analyse_info base reg i ) base l in
                                                             let n = List.fold_left(fun  na info -> (match info_ast_to_info info with
                                                             |InfoVar(_,t,_,_) -> na + Type.getTaille t 
                                                             | _ -> failwith "") ) 0 l in
                                                             n
                                                      )
                                   
    | AstType.Empty -> 0
                                    
                                           
  | _ -> 0  
  (* analyse_placement_bloc : AstType.bloc -> AstPlacement.bloc *)
  (* Paramètre base : le décalage courant *)
  (* Paramètre reg : l'adresse de registre *)
  (* Paramètre li : liste d'instructions à analyser *)
  (* renvoie la base de décalage mémoire *)
  and analyse_placement_bloc base reg li =
   List.fold_left (fun base i -> base + analyse_placement_instruction base reg i)  base li |>
   ignore
  
  
  
   let analyse_parametres  infos = 
    (List.fold_right (fun info base -> match info_ast_to_info info with
                                        |InfoVar (_,t,_,_)-> let base' = base - Type.getTaille t in 
                                         let () = Tds.modifier_adresse_info base' "LB" info in
                                         base'
                                         | _ -> assert false) infos
                                         (0)) |> ignore
  (* analyse_tds_fonction : AstType.fonction -> AstPlacement.fonction *)
  (* Paramètre : la fonction à analyser *)
  (* analyse les params et le bloc de la fonction.*)
  let analyse_placement_fonction  (AstType.Fonction(info,infos,li)) =
    let () = analyse_parametres  infos in
    let _ = analyse_placement_bloc 3 "LB" li in
    AstPlacement.Fonction (info,infos,li)
   
  
  
  




let analyser (AstType.Programme(fonctions,prog)) =
  let lf = List.map (analyse_placement_fonction) fonctions in 
  let () = analyse_placement_bloc 0 "SB" prog in
  AstPlacement.Programme(lf,prog)

end