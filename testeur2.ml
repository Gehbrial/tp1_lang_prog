(* --------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2018 ----------------------- *)
(* --------------------------------------------------------------------------- *)
(* Fichier pertmettant de tester les fonctions implantées du TP                *)
(* --------------------------------------------------------------------------- *)
(* On suppose que le Tp (ptree.ml) est chargé dans la mémoire de l'interpréteur*)
(* il suffit alors d'entrer dans l'interpréteur ce qui suit:                   *)
(*                                                                             *)
(* # #use "testeur2.ml";;                                                      *)
(*                                                                             *)
(* Par la suite:                                                               *)
(*                                                                             *)
(* # corrige();;  (* Teste toutes les fonctions                 *)             *)
(* # testn'();;   (* n = 1 ou 2 ...; teste la fonction numéro n *)             *)
(*                                                                             *)
(* Lorsque le fichier ptree.ml est modifié, vous n'avez juste qu'à:            *)
(* - recharger le fichier ptree.ml;                                            *)
(* - recharger le fichier testeur2.ml.                                         *)
(* Par la suite, vous pouvez de nouveau effectuer les tests                    *)
(* --------------------------------------------------------------------------- *)

open PTree;; 


(* -- À IMPLANTER/COMPLÉTER (8 PTS) --------------------------------------- *)
(* @Fonction      : includeSep : string -> string list -> string            *)
(* @Description   : retourne une liste d'elts en format string, séparés par
                    une valeur passée en argument                           *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : aucune                                                  *)
let test1'() =
  let note = ref 0. in
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test       , points accordés*)
    [ (("",[]), "", "CasChaineVideListeVide", 1.);
      (("",["a"]), "a", "CasChaineVideListeUn", 1.);
      (("",["a"; "b";"c";"d"]), "abcd", "CasChaineVideListeQuatre", 1.);
      (("---", []), "---", "CasChaineNonVideListeVide", 1.);
      (("---", ["a"]), "---a---", "CasChaineNonVideListeUn", 1.);
      (("---", ["a";"b";"c";"d";"e"]), 
        "---a---b---c---d---e---", "CasChaineNonVideListeCinq", 3.)
    ] in

  try
    List.iter 
      ( fun ((p1,p2), res, comment, pts) ->
          if includeSep p1 p2 = res  
          then note := !note +. pts 
          else comment_l := !comment_l @ [comment ^ " incorrect!"]
      ) jeu_donnees;
      (!note, !comment_l, false)
  with
  | Non_Implante _ -> 
    (!note, !comment_l @ ["Non_implantee"], true)
  | _ -> 
    (!note, !comment_l @ ["Test_non_complete"], true);;


(* -- À IMPLANTER/COMPLÉTER (12 PTS) -------------------------------------- *)
(* @Fonction      : height : strTree -> int                                 *)
(* @Description   : retourne la taille d'un arbre passé en argument         *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : la valeur retournée est positive et correspond à taille *)
let test2'() =
  let note = ref 0. in
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test       , points accordés*)
    [ (St 10  , 0 , "CasSt"                 , 0.5);
      (Leaf "a", 0, "CasLeaf", 0.5);
      (Tree("a","b",[]), 1, "CasFilsVide", 1.);
      (Tree("a","b",[St 10]), 1, "CasFilsUn", 1.);
      (Tree("a","b",[St 10;Leaf "a"]), 1, "CasFilsDeux-1", 0.5);
      (Tree("a","b",[St 10;St 10]), 1, "CasFilsDeux-2", 0.5);
      (Tree("a","b",[Leaf "a";Leaf "a"; St 10]), 1, "CasFilsTrois", 1.);
      (Tree("a","b",[Leaf "a";Tree("a","b",[St 10])]), 2, "CasHaut2", 3.);
      (Tree("a","b",[Tree("a","b",[Leaf "a";Tree("a","b",[St 10])]);
                     Tree("a","b",[Leaf "a"; St 10])]), 3, "CasHaut3", 4.)
    ] in

  try
    List.iter 
      ( fun (p, res, comment, pts) ->
          if height p = res  
          then note := !note +. pts 
          else comment_l := !comment_l @ [comment ^ " incorrect!"]
      ) jeu_donnees;
      (!note, !comment_l, false)
  with
  | Non_Implante _ -> 
    (!note, !comment_l @ ["Non_implantee"], true)
  | _ -> 
    (!note, !comment_l @ ["Test_non_complete"], true);;


(* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
(* @Fonction      : ptree2stree : ('formula -> string)-> ('rule -> string) -> 
                                  ('formula, 'rule) pTree -> strTree        *)
(* @Description   : traduit un arbre prrofTree en un arbre strTree          *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : l'arbre retourné est correct                            *)
let test3'() =
  let note = ref 0. in
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test       , points accordés*)
    [ (PF 1., Leaf "1.", "CasPF", 1.);
      (PT(1,2.,[]), Tree ("1", "2.", []), "CasFilsVide", 1.);
      (PT(1,2.,[PF 3.]), Tree ("1", "2.", [Leaf "3."]), "CasFilsUn", 3.);
      (PT(1,2.,[PF 3.; PF 4.]), 
       Tree ("1", "2.", [Leaf "3."; Leaf "4."]), "CasFilsDeux", 5.);
      (PT(1,2.,[PT(3,4.,[PF 5.; PT(6,7.,[]); PF 8.])]), 
       Tree("1","2.",[Tree("3","4.",[Leaf "5.";Tree("6","7.",[]);Leaf "8."])]), 
       "CasGeneral", 10.)
    ] in

  try
    List.iter 
      ( fun (p, res, comment, pts) ->
          if ptree2stree string_of_float string_of_int p = res  
          then note := !note +. pts 
          else comment_l := !comment_l @ [comment ^ " incorrect!"]
      ) jeu_donnees;
      (!note, !comment_l, false)
  with
  | Non_Implante _ -> 
    (!note, !comment_l @ ["Non_implantee"], true)
  | _ -> 
    (!note, !comment_l @ ["Test_non_complete"], true);;


(* -- À IMPLANTER/COMPLÉTER (40 PTS) -------------------------------------- *)
(* @Fonction      : tree2mtree : ?l:int->strTree->(int * strTree) list      *)
(* @Description   : transforme un arbre en liste de sous-arbres             *)
(* @Precondition  : «l» doit être positive ou nulle                         *)
(* @Postcondition : les arbres retournées sont correctement liées           *)
let test4'() =
  let note = ref 0. in
  let comment_l = ref [] in

  let a1 = St 10 in 
  let a2 = Leaf "a" in 
  let a3 = Tree("a","b",[]) in 

  let a4 = Tree("a","b",[a1]) in
  let r4 = [(1, Tree ("a", "b", [a1]))] in
  let a4' = Tree("a","b",[a2]) in
  let r4' = [(1, Tree ("a", "b", [a2]))] in

  let a5 = Tree("a","b",[a1; a4]) in
  let r5 = [(1,Tree ("a","b",[St 10;St 2])); 
            (2,Tree ("a","b",[St 10]))] in
  let r5' = [(1, Tree ("a", "b", [St 10; Tree ("a", "b", [St 10])]))] in
  
  let a6 = Tree("a","b",[a5;a4]) in
  let r6 = [(1, Tree ("a", "b", [St 2; St 4])); 
            (2, Tree ("a", "b", [St 10; St 3])); 
            (3, Tree ("a", "b", [St 10])); 
            (4, Tree ("a", "b", [St 10]))] in
  let r6' = [(1, Tree ("a", "b", [St 2; 
                                  Tree ("a", "b", [St 10])])); 
             (2, Tree ("a", "b", [St 10; 
                                  Tree ("a", "b", [St 10])]))] in
  let r6'' = [(1, Tree ("a", "b", [Tree ("a", "b", [St 10; 
              Tree ("a", "b", [St 10])]); 
              Tree ("a", "b", [St 10])]))] in

  let a7 = Tree("a","b",[a6;a4;a5]) in
  let r7 = [(1, Tree ("a", "b", [St 2; St 6; St 7])); 
            (2, Tree ("a", "b", [St 3; St 5])); 
            (3, Tree ("a", "b", [St 10; St 4]));
            (4, Tree ("a", "b", [St 10])); 
            (5, Tree ("a", "b", [St 10]));
            (6, Tree ("a", "b", [St 10])); 
            (7, Tree ("a", "b", [St 10; St 8]));
            (8, Tree ("a", "b", [St 10]))] in
  let r7' = [(1, Tree ("a", "b", [St 2; Tree ("a", "b", [St 10]); St 4]));
             (2, Tree ("a", "b", [St 3; Tree ("a", "b", [St 10])]));
             (3, Tree ("a", "b", [St 10; Tree ("a", "b", [St 10])]));
             (4, Tree ("a", "b", [St 10; Tree ("a", "b", [St 10])]))] in
  let r7'' = [(1, Tree ("a", "b", [St 2; Tree ("a", "b", [St 10]); 
                    Tree ("a", "b", [St 10; Tree ("a", "b", [St 10])])])); 
              (2, Tree ("a", "b", [Tree ("a", "b", [St 10; 
                    Tree ("a", "b", [St 10])]); Tree ("a", "b", [St 10])]))] in
  let r7''' = [(1, Tree ("a", "b", [Tree ("a", "b", [Tree ("a", "b", [St 10; 
                     Tree ("a", "b", [St 10])]); Tree ("a", "b", [St 10])]); 
                       Tree ("a", "b", [St 10]); Tree ("a", "b", [St 10; 
                         Tree ("a", "b", [St 10])])]))] in

  let jeu_donnees =
  (*   param    ,res, commentaires_test, points accordés*)
    [ ((a1,0), [], "CasSt-0"        , 0.5);
      ((a1,1), [], "CasSt-1", 0.5);
      ((a2,0), [], "CasLeaf-0", 0.5);
      ((a2,1), [], "CasLeaf-1", 0.5);
      ((a3,0), [(1, Tree ("a", "b", []))], "CasFilsVide-0", 1.);
      ((a3,1), [(1, Tree ("a", "b", []))], "CasFilsVide-1", 1.);
      ((a4,0), r4, "CasFilsUn-0", 1.);
      ((a4',1), r4', "CasFilsUn-1", 1.);
      ((a5,0), r5, "CasFilsDeux-0", 2.);
      ((a5,1), r5', "CasFilsDeux-1", 1.);
      ((a5,2), r5', "CasFilsDeux-2", 1.);
      ((a6,0), r6, "CasFilsDeux'-0", 4.);
      ((a6,1), r6', "CasFilsDeux'-1", 2.);
      ((a6,2), r6'', "CasFilsDeux'-2", 3.);
      ((a6,3), r6'', "CasFilsDeux'-3", 1.);
      ((a7,0), r7, "CasFils3-0", 7.);
      ((a7,1), r7', "CasFils3-1", 3.);
      ((a7,2), r7'', "CasFils3-2", 3.);
      ((a7,3), r7''', "CasFils3-3", 6.);
      ((a7,4), r7''', "CasFils3-4", 1.)
    ] in

  try
    List.iter 
      ( fun ((t,l), res, comment, pts) ->
          if tree2mtree ~l:l t = res  
          then note := !note +. pts 
          else comment_l := !comment_l @ [comment ^ " incorrect!"]
      ) jeu_donnees;
      (!note, !comment_l, false)
  with
  | Non_Implante _ -> 
    (!note, !comment_l @ ["Non_implantee"], true)
  | _ -> 
    (!note, !comment_l @ ["Test_non_complete"], true);;


(* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
(* @Fonction      : mtree2pretty : (strTree -> string) -> (int -> string) -> 
                                   string->(int * strTree) list->string list*)
(* @Description   : retourne une liste de chaines de caractères correspondant
                    à l'arbre, ou aux arbres, à afficher                    *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : aucune                                                  *)
let test5'() =
  let note = ref 0. in
  let comment_l = ref [] in

  let lt4 = [(1,Tree ("a", "b",[St 9; St 2])); (2,Tree ("a", "b",[St 9]))] in 
  let r4 = ["A1:"; "\t   A9   A2   \n\t------------- a\n\t      b\n"; 
            "A2:"; "\t   A9   \n\t-------- a\n\t   b\n"] in

  let lt5 = [(1, Tree ("a", "b", [St 2; St 4])); 
             (2, Tree ("a", "b", [St 9; St 3])); 
             (3, Tree ("a", "b", [St 9])); 
             (4, Tree ("a", "b", [St 9]))] in
  let r5 = ["A1:"; "\t   A2   A4   \n\t------------- a\n\t      b\n"; 
            "A2:"; "\t   A9   A3   \n\t------------- a\n\t      b\n"; 
            "A3:"; "\t   A9   \n\t-------- a\n\t   b\n"; 
            "A4:"; "\t   A9   \n\t-------- a\n\t   b\n"] in

  let lt6 = [(1, Tree ("a", "b", [St 2; St 6; St 7])); 
             (2, Tree ("a", "b", [St 3; St 5])); 
             (3, Tree ("a", "b", [St 9; St 4]));
             (4, Tree ("a", "b", [St 9])); 
             (5, Tree ("a", "b", [St 9]));
             (6, Tree ("a", "b", [St 9])); 
             (7, Tree ("a", "b", [St 9; St 8]));
             (8, Tree ("a", "b", [St 9]))] in
  let r6 = ["A1:"; "\t   A2   A6   A7   \n\t------------------ a\n\t        b\n"; "A2:";
            "\t   A3   A5   \n\t------------- a\n\t      b\n"; "A3:";
            "\t   A9   A4   \n\t------------- a\n\t      b\n"; "A4:";
            "\t   A9   \n\t-------- a\n\t   b\n"; "A5:";
            "\t   A9   \n\t-------- a\n\t   b\n"; "A6:";
            "\t   A9   \n\t-------- a\n\t   b\n"; "A7:";
            "\t   A9   A8   \n\t------------- a\n\t      b\n"; "A8:";
            "\t   A9   \n\t-------- a\n\t   b\n"] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test       , points accordés*)
    [ ([], [], "CasVide", 1.);
      ([(1, Tree("a", "b", []))], ["\t   \n\t--- a\n\t b"], "CasFilsUn", 2.);
      ([(1, Tree ("a", "b", [St 9]))], 
       ["\t   A9   \n\t-------- a\n\t   b"], "CasFilsUn-1", 1.);
       ([(1, Tree ("a", "b", [Leaf "c"]))], 
       ["\t   c   \n\t------- a\n\t   b"], "CasFilsUn-2", 1.);
      (lt4, r4, "CasFilsUn", 4.);
      (lt5, r5, "CasArbre2", 5.);
      (lt6, r6, "CasArbre4", 6.)
    ] in

  try
    List.iter 
      ( fun (p, res, comment, pts) ->
          if mtree2pretty tree2str id2str "\n" p = res  
          then note := !note +. pts 
          else comment_l := !comment_l @ [comment ^ " incorrect!"]
      ) jeu_donnees;
      (!note, !comment_l, false)
  with
  | Non_Implante _ -> 
    (!note, !comment_l @ ["Non_implantee"], true)
  | _ -> 
    (!note, !comment_l @ ["Test_non_complete"], true);;


(* --------------------------------------------------------------------------- *)
(* -- TESTE TOUT ------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test'() =
  let all_tests = [ "includeSep", 8, test1';
                    "height", 12, test2';
                    "ptree2stree", 20, test3';
                    "tree2mtree", 40, test4';
                    "mtree2pretty", 20, test5'
                  ] in
  List.fold_left 
    (fun (l_res,erreur) (nom_f,bareme,t) -> 
       let (note, comment, err) = t () in
         (l_res @ [(nom_f, bareme, note, comment, err)], erreur || err)
    ) ([],false) all_tests;;


(* --------------------------------------------------------------------------- *)
(* -- CORRIGE ---------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let corrige () =
  print_endline "Resultats:";
  print_endline "----------\n";
  let notef = ref 0. in
    List.iter
      (fun (nom_f, bareme, note, comment, _) -> 
         notef := !notef +. note;
         Printf.printf "%s : %.1f/%d\n" nom_f note bareme;
         List.iter (fun c -> print_endline ("\t" ^ c)) comment
      ) (fst(test'()));
    Printf.printf "\nNote finale: %.1f/100\n\n" !notef;;