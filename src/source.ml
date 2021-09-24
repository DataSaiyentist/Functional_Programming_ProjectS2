open Squelette;;


(* Séquences ADN/ ARN *)


(* QUESTION 1 *)

(* @requires brin est une liste de nucléotides
    assigns rien
    ensures retourne la proportion de Guanine ou Cytosine dans le brin = liste de nucléotides
            sinon une erreur si la liste est vide
            
   Cette fonction contient deux lambda expressions :
   - freq(couple) qui retourne le flottant couple[1]/couple[2]
   - couple tel que couple[1] contient le nombre de Guanine ou Cytosine dans le brin
                    couple[2] le nombre de nucléotides dans le brin *)
let contenu_gc brin = let freq couple = if (snd couple)=0 then failwith "contenu_gc : brin vide"
                                                          else ( float_of_int (fst couple) )/.( float_of_int (snd couple) )
                      in let couple = List.fold_left (fun acc n -> if n = C || n = G then ((fst acc) + 1, (snd acc) + 1) 
                                                                                     else ((fst acc, (snd acc) + 1))) (0,0) brin
                         in freq couple;;


(* QUESTION 2 *)

(* @requires brin est une liste de nucléotides
    assigns rien
    ensures retourne une liste de nucléotide correspondant au complémentaire du brin donné en entrée
            sinon une erreur si la liste est vide
            
    On utilise un List.fold pour appliquer à chaque terme de la liste de nucléotide une fonction qui identifie le nucléotide
    et ajoute son complémentaire dans l'accumulateur.
    On utilise en particulier un fold_right pour que la liste soit retourné dans le bon sens *)
let brin_complementaire brin = if brin=[] then failwith "brin_complementaire : brin vide"
                                          else List.fold_right (fun n acc -> if n = A then T::acc 
                                                                             else if n = C then G::acc 
                                                                             else if n = G then C::acc
                                                                             else if n = T then A::acc
                                                                             else acc) brin [];;


(* QUESTION 3 *)

(* @requires les deux brins sont des listes de nucléotides
    assigns rien
    ensures retourne la distance d'édition des deux brins
            sinon une erreur si l'une des deux listes et vide ou les deux listes n'ont pas la même taille
            
    Cette fonction contient une lambda expression, il s'agit d'une fonction récursive :
        - bool, faisant office de booléen, permet de vérifier si distance a été appliqué au moins une fois
        - acc est un accumulateur qui est incrémenté de 1 lorsque les nucléotides comparés sont différents *)
let distance brin_1 brin_2 = let rec rec_distance brin_1 brin_2 acc bool =
                               match brin_1 with
                               | [] -> if bool=0 then failwith "distance : brin_1 est vide"
                                                 else if brin_2=[] then acc
                                                                   else failwith "distance : brin_1 et brin_2 n'ont pas la même taille"
                               | n1::ns1 -> ( match brin_2 with
                                   | [] -> failwith "distance : brin_1 et brin_2 n'ont pas la même taille"
                                   | n2::ns2 -> rec_distance ns1 ns2 (if n1=n2 then acc else acc + 1) 1 )
                                in rec_distance brin_1 brin_2 0 0;;


(* QUESTION 4 *)

(* @requires les deux brins sont des listes de nucléotides
    assigns rien
    ensures retourne la similarité procentuelle entre les deux brins
            sinon une erreur si l'une des deux listes et vide ou les deux listes n'ont pas la même taille
            
    Comme demandé, on utilise la fonction précédente pour calculé la similarité procentuelle.
    Avant d'utiliser la fonction, on vérifie si les conditions sont remplies. *)
let similarite brin_1 brin_2 = let n1 = List.length brin_1 
                               in let n2 = List.length brin_2 
                                  in if n1 = 0 then failwith "distance : brin_1 est vide"
                                               else if n2 = 0 then failwith "distance : brin_2 est vide" 
                                               else if n1!=n2 then failwith "distance : brin_1 et brin_2 n'ont pas la même taille"
                                               else 1. -. ( float_of_int (distance brin_1 brin_2) ) /.( float_of_int n1 );;


(* QUESTION 5 *)

(* @requires brin est une liste de nucléotides
    assigns rien
    ensures retourne la première chaîne d'acides codé par le brin (le brin est bien formé car code au moins une chaîne)
            sinon une erreur car la liste est vide ou ne code pas de chaîne d'acides, ie est mal formé
            
    Dans cette fonction, on avance de trois en trois dès qu'on trouve un acide aminé correspondant à START.
    C'est pourquoi, on utilise un couple verif tel que :
    - verif[1] = 1 dès qu'on a trouvé un START, 0 sinon
    - verif[2] = 1 dès qu'on a trouvé au moins une chaîne d'acides aminé, sinon 0 *)
let brin_vers_chaine brin = let bvc_end snd_verif chain = if snd_verif = 0 then failwith "brin_vers_chaine : brin invalide (ie vide ou mal formé)"
                              else chain
  in let rec rec_brin_vers_chaine brin chain verif =
       match brin with
       | [] -> bvc_end ( snd verif ) chain
       | n1::[] -> bvc_end ( snd verif ) chain
       | n1::n2::[] ->  bvc_end ( snd verif ) chain
       | n1::n2::n3::ns -> let a = codon_vers_acide n1 n2 n3 in
           if (( fst verif ) = 0 && ( snd verif ) = 0) then ( if a = START then rec_brin_vers_chaine ns chain (1,0)
                                                              else rec_brin_vers_chaine (n2::n3::ns) chain verif )
           else match a with
             | STOP -> if chain = [] then rec_brin_vers_chaine ns chain (0,0)
                 else chain 
             | START -> rec_brin_vers_chaine ns [] (1,0)
             | _ -> rec_brin_vers_chaine ns (a::chain) verif 
  in List.rev(rec_brin_vers_chaine brin [] (0,0));;


(* Arbres phylogénétiques *)


(* QUESTION 1 *)

(* @requires brin est une liste de nucléotides
    assigns rien
    ensures retourne une représentation unique d'un brin en chaîne de caractères
            sinon lève une exception car la liste entrée est invalide, ie n'est pas une liste de nucléotides *)
let rec brin_vers_string brin = match brin with
  | [] -> "[]"
  | n::ns -> if n = A then " A::"^( brin_vers_string ns )
             else if n = C then " C::"^( brin_vers_string ns )
             else if n = G then " G::"^( brin_vers_string ns )
             else if n = T then " T::"^( brin_vers_string ns )
             else failwith "print_brin : argument invalide (on attend une liste de nucléotides)";;


(* @requires a est un arbre phylogénétique
    essigns rien
    ensures retourne une entier positif :
        - 0 si l'arbre est complet (ie le calcul du malus à chaque branche est respecté, tous les bris ont la même taille et les brins sont non vides)
        - n > 0 si l'une des conditions cité précédemment n'est pas respecté
        
    is_complet est une fonction récursive
    elle traite 5 cas majeurs :
        - si a est une feuille
        - si b est une branche b = Br(l, x, i, r) :
            - si l est une feuille et r aussi
            - si l est une feuille et r une branche
            - si l est une branche et r une feuille
            - si l est une branche et r aussi *)
let rec is_complet a = match a with
  | Lf(b) -> if b = [] then 1 else 0
  | Br(a1, b, i, a2) -> if b = [] then 1 else (
      match a1 with
      | Lf(bg) -> if bg = [] then 1 else (
          match a2 with
          | Lf(bd) -> if bd = [] then 1
              else ( let n = List.length b in 
                     if n!= ( List.length bg ) || n!= ( List.length bd ) then 1
                     else if i!= ((distance b bd) + (distance b bg)) then 1 else 0 )
          | Br(ad1, bd, id, ad2) -> if bd = [] then 1
              else ( let n = List.length b in
                     if n!= ( List.length bg ) || n!= ( List.length bd ) then 1
                     else if i!= ((distance b bd) + (distance b bg)+id) then 1
                     else is_complet a2 ) )
      | Br(ag1, bg, ig, ag2) -> if bg = [] then 1 else (
          match a2 with
          | Lf(bd) -> if bd = [] then 1
              else ( let n = List.length b in 
                     if n!= ( List.length bg ) || n!= ( List.length bd ) then 1
                     else if i!= ((distance b bd) + (distance b bg)+ig) then 1
                     else is_complet a1 )
          | Br(ad1, bd, id, ad2) -> if bd = [] then 1
              else ( let n = List.length b in
                     if n!= ( List.length bg ) || ( n!= List.length bd ) then 1
                     else if i!= ((distance b bd) + (distance b bg)+ig+id) then 1
                     else ( is_complet a1 ) + ( is_complet a2  ) ) ) );;


(* @requires a est un arbre phylogénétique
    assigns rien
    ensures retourne une représentation unique d'un arbre phylogénétique en chaîne de caractères
            sinon lève une exception car l'arbre n'est pas complet
            
    Pour cela, on utilise deux fonctions auxilliaires :
        - brin_vers_string qui retourne une représentation unique d'un brin en chaîne de caractères
        - is_complet qui permet de vérifier si l'arbre est complet ou non *)
let rec arbre_phylo_vers_string a = let n = is_complet a in
  if n!=0 then failwith " arbre_phylo_vers_string : l'arbre n'est pas complet"
  else ( match a with
      | Lf(b) -> "Lf("^( brin_vers_string b )^")"
      | Br(a1, b, i, a2) -> "Br("^( arbre_phylo_vers_string a1 )^", "^
        ( brin_vers_string b )^", "^(string_of_int i)^", "^(arbre_phylo_vers_string a2)^")" );;  


(* QUESTION 2 *)

(* @requires t est un arbre phylogénétique, f de type cohérent avec les brins
    assigns rien
    ensures retourne acc en composant l'application f sur les éléments d'un arbre phylgénétique 
    
    Cette fonction auxiliaire permettra notamment de calculer la taille d'un arbre.
    On vérifiera bien sûr, avant d'appliquer cette fonction, que l'arbre est bien complet. *)
let rec fold_tree f t acc = match t with
    | Lf(b) -> acc
    | Br(l, x, i, r) -> let acc_x = f x acc in
                            let acc_l = fold_tree f l acc_x in
                                fold_tree f r acc_l;;

(* @requires a est un arbre phylogénétique
    assigns rien
    ensures retourne la taille de l'arbre
    
    Cette fonction sera utilisé que si l'arbre est complet.
    De plus, celle-ci retourne la taille de l'arbre (utile lors de la comparaison de deux arbres
    phylogénétiques). *)
let tree_size a = fold_tree (fun _ acc -> acc + 1) a 0;;

(* @requires t1 et t2 sont des arbres phylogénétique, f de type cohérent avec les brins
    assigns rien
    ensures retourne acc en composant l'application f sur les couples d'éléments des deux arbres phylogénétiques
            sinon lève une exception car t1 et t2 n'ont pas la même taille
    
    Cette fonction auxiliaire permettra notamment de calculer la similarité entre deux arbres phylogénétiques.
    On vérifiera bien sûr, avant d'appliquer cette fonction, que l'arbre est bien complet.
    
    fold_tree_compare traite 4 cas majeurs :
            - si t1 est une feuille et t2 aussi
            - si t1 est une feuille et t2 une branche
            - si t1 est une branche et t2 une feuille
            - si t1 est une branche et t2 aussi *)
let rec fold_tree_compare f t1 t2 acc = let n = tree_size t1 in
    if n != ( tree_size t2 ) then failwith "fold_tree_compare : t1 et t2 n'ont pas la même hauteur"
    else match t1, t2 with
        | (Lf(b1), Lf(b2)) -> acc
        | (Lf(b1), Br(ld, b2, id, rd)) -> failwith "fold_tree_compare : t1 et t2 n'ont pas la même hauteur"
        | (Br(lg, b1, ig, rg), Lf(b2)) -> failwith "fold_tree_compare : t1 et t2 n'ont pas la même hauteur"
        | (Br(lg, b1, ig, rg), Br(ld, b2, id, rd)) -> let acc_x = f b1 b2 acc in
                                                          let acc_l = fold_tree_compare f lg ld acc_x in
                                                              fold_tree_compare f rg rd acc_l;;

(* @requires a est un arbre phylogénétique et liste_a une liste d'arbres phylogénétiques
    assigns rien
    ensures retourne l'arbre de liste_a le plus similaire à a
            sinon une erreur :
                - si liste_a est vide
                - si l'arbre a n'est pas complet
                - si liste_arbre contient un arbre non complet ou un des arbres contient une liste vide ou n'a pas la même forme (ie la même hauteur et taille de brins
    
    Derrière cette fonction se cache une fonction récursive contenant deux lambda expressions :
    - is_compare_size vérifie si tous les brins des deux arbres phylogénétiques complets ont exactement la même taille
      (les arbres étant complets, il suffirait juste de regarder la taille de deux brins quelconque)
    - is_compare vérifie si les arbres de liste_a sont complets, ont la même hauteur que a et utilise compare_size_verifie
    
    Si les conditions cités précédemment ne sont pas respecté, une exception est levée, sinon on calcule la similarité des arbres de liste_a avec a
    puis on retourne à la fin celui qui est le plus similaire à a
    
    acc est un couple tel que :
    - acc[1] est l'arbre le plus similaire à a
    - acc[2] est un couple tel que :
        - acc[2][1] est la similarité entre acc[1] et a
        - acc[2][2] est un booléen de type int tel que si acc[2][2]=1 alors liste_a contient au moins un élément, sinon acc[2][2]=0 *)
let similaire a liste_a = let rec rec_similaire a liste_a acc =
    if liste_a = [] && ( snd ( snd acc ) ) = 0 then failwith "similaire : liste_a est une liste vide" else (
                          let is_compare_size a1 a2 = ( fold_tree_compare (fun b1 b2 acc -> let n = List.length b1 in
                                                                                            if n != ( List.length b2 ) then 1
                                                                                            else acc) a1 a2 0 ) in 
                          let is_compare a liste_a = ( List.fold_left(fun acc a_i -> if (is_complet a_i) != 0 then 1
                                                                                   else ( let n = tree_size a in if n != (tree_size a_i) then 1  
                                                                                   else if (is_compare_size a a_i) != 0 then 1
                                                                                   else acc) ) 0 liste_a ) in
                                 if (is_complet a) != 0 then failwith "similaire : l'arbre a n'est pas complet"
                                 else if (is_compare a liste_a) !=0 then failwith "similaire :liste_a contient un arbre non complet 
                                 ou a et un arbre de liste_a n'ont pas la même hauteur\ ou les brins de a n'ont pas la même taille que les brins d'un arbre de liste_a"
                                 else ( match liste_a with
                                    | [] -> acc
                                    | ai::an -> let s = fold_tree_compare (fun b b_i acc -> acc +. ( similarite b b_i)) a ai 0. in 
                                                    if ( snd ( snd acc ) ) = 0 then rec_similaire a an (ai, (s,1))
                                                    else if s > (fst (snd acc)) then rec_similaire a an (ai, (s,1))
                                                    else rec_similaire a an acc ) )
                            in fst ( rec_similaire a liste_a (a, (1.,0) ));;


(* QUESTION 3 *)
  
(* @requires a est un arbre phylogénétique
    assigns rien
    ensures retourne le brin extrait de la racine
            sinon lève une erreur :
            - si l'arbre n'est pas complet
            - si a est seulement une feuille *)
let get_root a = if (is_complet a) != 0
    then failwith "get_root : a n'est pas un arbre complet"
    else match a with
    | Lf(b) -> failwith "get_root : a est une feuille"
    | Br(l, x, i, r) -> x;;

(* @requires a est un arbre phylogénétique
    assigns rien
    ensures retourne le malus extrait de la racine
            sinon lève une erreur :
            - si l'arbre n'est pas complet
            - si a est seulement une feuille *)
let get_malus a = if (is_complet a) != 0
    then failwith "get_malus : a n'est pas un arbre complet"
    else match a with
    | Lf(b) -> failwith "get_malus : a est une feuille"
    | Br(l, x, i, r) -> i;; 

(* @requires l et x sont des arbres phylogénétiques et x un brin
    assigns rien
    ensures retourne l'arbre nouvellement formée Br(l, x, i, r) si :
        - l et r sont bien complets
        - Br(l, x, i, r) est bien complet avec i le malus associé
        
    br traite 4 cas majeurs :
            - si l est une feuille et r aussi
            - si l est une feuille et r une branche
            - si l est une branche et r une feuille
            - si l est une branche et r aussi *)
let br (l, x, r) = if x = [] then failwith "br : le second terme du triplet , x, est une liste vide"
    else if ( is_complet l ) != 0 then failwith "br : le premier terme du triplet, l, n'est pas un arbre complet"
    else if ( is_complet r ) != 0 then failwith "br : le troisième terme du triplet, r, n'est pas un arbre complet"
    else let n = ( List.length x ) in ( match r, l with
        | (Lf(bg), Lf(bd)) -> if n != ( List.length bg ) || n != ( List.length bd ) then failwith " br :les brins n'ont pas la même taille, l'arbre serait incomplet"
                              else Br(l, x, ( distance bg x ) + ( distance bd x ), r)
        | (Lf(bg), Br(ld, bd, id, rd)) -> if n!= ( List.length bg ) || n != ( List.length bd ) then failwith " br :les brins n'ont pas la même taille, l'arbre serait incomplet"
                                          else Br(l, x, ( distance bg x ) + ( distance bd x ) + id, r)
        | (Br(lg, bg, ig, rg), Lf(bd)) -> if n!= ( List.length bg ) || n != ( List.length bd ) then failwith " br :les brins n'ont pas la même taille, l'arbre serait incomplet"
                                          else Br(l, x, ( distance bg x ) + ( distance bd x ) + ig, r)
        | (Br(lg, bg, ig, rg), Br(ld, bd, id, rd)) -> if n!= ( List.length bg ) || n != ( List.length bd ) then failwith " br :les brins n'ont pas la même taille, l'arbre serait incomplet"
                                                      else Br(l, x, ( distance bg x ) + ( distance bd x ) + ig + id, r) );;


(* QUESTION 4 *)

(* @requires b1, b2 et b3 des listes de nucléotides
    assigns rien
    ensures retourne la liste des différents arbres phylogénétiques que l'on peut former avec ces trois brins (en excluant les cas triviaux, ie juste une feuille)
            sinon lève une exception car l'un des brins est vide ou les brins n'ont pas la même taille
            
    acc est la liste des différents arbres que l'on peut former avec les trois brins
    n est la taille de la liste acc (dès qu'elle atteint 3, on arrête la récursion) *)  
let gen_phylo b1 b2 b3 = let rec rec_gen_phylo b1 b2 b3 acc n = 
    let s = ( List.length b1 ) in
        if s = 0 || s != ( List.length b2 ) || s != ( List.length b3 ) then failwith "gen_phylo : l'un des brins est vide ou les brins n'ont pas la même taille"
        else ( if n = 3 then acc
                        else rec_gen_phylo b2 b3 b1 ((br (Lf(b1),b2,Lf(b3)))::acc) (n+1) )
    in rec_gen_phylo b1 b2 b3 [] 0 ;;
  
  
(* QUESTION 5 *)

(* @requires liste_a est une liste d'arbres complets
    assigns rien
    ensures retourne la première occurence de l'arbre phylogénétique ayant le plus petit malus
            sinon lève une exception car la liste est vide, un des arbres n'est pas complet ou un des termes est juste une feuille
            
    Dans le fold_left, acc est un couple tel que :
    - acc[1] est également un couple tel que : 
        - acc[1][1] est booléen de type int tel que dès que la liste contient un arbre incomplet, acc[1][1]=1 sinon 0
        - acc[1][2] contient le malus de acc[2]
    - acc[2] est la première occurence dans liste_a tel que son malus est le plus petit *)  
let min_malus liste_a = match liste_a with
    | [] -> failwith "min_malus : la liste d'arbres est vide"
    | a1::an -> if (is_complet a1) != 0 then failwith "min_malus : la liste contient un arbre non complet"
        else let min = List.fold_left( fun acc x -> if (fst (fst acc)) = 0 then acc
                                                    else if (is_complet x)!=0 then ((0, snd (fst acc)), snd acc)
                                                    else ( match x with
                                                    | Lf(b) -> ((0, snd (fst acc)), snd acc)
                                                    | Br(l, b, i, d) -> if i >= (snd (fst acc)) then acc
                                                                        else ((1, i), x) ) ) ((1,get_malus a1),a1) an
             in if (fst (fst min)) = 0 then failwith "min_malus : la liste contient un arbre non complet ou une feuille seulement"
                else snd min;;
  
  
(* QUESTION 6 *)

(* Non traitée *)