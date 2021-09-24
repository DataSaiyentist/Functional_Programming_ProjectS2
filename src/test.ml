open Squelette;;
open Source;;


(* Séquences ADN/ ARN *)
Printf.printf "--Séquences ADN/ ARN--\n\n";;


(* @requires brin est une liste de nucléotides
    assigns rien
    ensures retourne rien
            affiche sur la console le brin
            sinon lève une exception car la liste entrée est invalide, ie n'est pas une liste de nucléotides *)
let rec print_brin brin = match brin with
  | [] -> Printf.printf("[]")
  | n::ns -> if n = A then ( Printf.printf " A::" ; print_brin ns )
             else if n = C then ( Printf.printf " C::" ; print_brin ns )
             else if n = G then ( Printf.printf " G::" ; print_brin ns )
             else if n = T then ( Printf.printf " T::" ; print_brin ns )
             else invalid_arg "print_brin : argument invalide (on attend une liste de nucléotides)";;


(* Test de la fonction contenu_gc 
    - Liste vide -> retourne bien un Failure, ie lever une exception
    - Quatres différents brins : 
        - Un brin qui ne contient ni de Guanine, ni de Cytosine
        - Un brin qui contient des Guanines et des Cytosines
        - Un brin qui ne contient que des Cytosines
        - Un brin qui ne contient que des Guanines *)

Printf.printf "**QUESTION 1**\n";;
Printf.printf "**Test de la fonction contenu_gc**\n\n";;


let brin1 = [];;    
Printf.printf "Pour ";;
print_brin brin1;; 
Printf.printf ", contenu_gc(brin1) devrait retourner une erreur, car brin1 est vide\n";;
(* contenu_gc brin1;; *)

let brin2 = A::A::A::T::A::[];;    
Printf.printf "Pour ";;
print_brin brin2;; 
Printf.printf ", contenu_gc(brin2) devrait retourner 0 et retourne : %f\n" (contenu_gc brin2);;

let brin3 = A::T::G::T::T::G::A::C::[];;    
Printf.printf "Pour ";;
print_brin brin3;; 
Printf.printf ", contenu_gc(brin3) devrait retourner 0.375 et retourne : %f\n" (contenu_gc brin3);;

let brin4 = C::T::T::A::[];;    
Printf.printf "Pour ";;
print_brin brin4;; 
Printf.printf ", contenu_gc(brin4) devrait retourner 0.25 et retourne : %f\n" (contenu_gc brin4);;

let brin5 = G::T::G::G::G::[];;    
Printf.printf "Pour ";;
print_brin brin4;; 
Printf.printf ", contenu_gc(brin5) devrait retourner 0.8 et retourne : %f\n" (contenu_gc brin5);;


Printf.printf "\n";;


(* Test de la fonction brin_complementaire
    - Liste vide -> lève bien une exception
    - Trois brins différents :
        - Un brin avec un seul nucléotide
        - Un brin avec quasiment un même type de nucléotides
        - Un brin contenant plusieurs types de nucléotides *)

Printf.printf "**QUESTION 2**\n";;
Printf.printf "**Test de la fonction brin_complementaire**\n\n";;


let brin1 = [];;
let brin2 = T::[];;
let brin3 = C::T::C::C::[];;
let brin4 = C::T::A::A::A::G::T::[];;

Printf.printf "Pour ";;
print_brin brin1;; 
Printf.printf ", brin_complementaire(brin1) devrait retourner une erreur, car brin1 est vide\n";;
(* brin_complementaire brin1;; *)

Printf.printf "Le complémentaire de ";;
print_brin brin2;;
Printf.printf " est ";;
print_brin (brin_complementaire brin2);;
Printf.printf "\n";;

Printf.printf "Le complémentaire de ";;
print_brin brin3;;
Printf.printf " est ";;
print_brin (brin_complementaire brin3);;
Printf.printf "\n";;

Printf.printf "Le complementaire de ";;
print_brin brin4;;
Printf.printf " est ";;
print_brin (brin_complementaire brin4);;


Printf.printf "\n\n";;


(* Test de la fonction distance
    - L'une des deux liste est vide -> lève bien une exception
    - Les deux brins ont une taille différente
    - Deux brins de même taille = 1 et identique (en fait lui-même)
    - Deux brins de même taille = 1 mais différente
    - Deux brins de même taille et plus grands mais différents  *)

Printf.printf "**QUESTION 3**\n";;
Printf.printf "**Test de la fonction distance**\n\n";;


let brin1 = [];;
let brin2 = T::[];;
let brin3 = C::[];;
let brin4 = G::A::G::[];;
let brin5 = A::G::G::[];;

Printf.printf "Pour ";;
print_brin brin1;;
Printf.printf " et";;
print_brin brin2;; 
Printf.printf ", distance(brin1, brin2) devrait retourner une erreur, car brin1 est vide\n";;
(* distance brin1 brin2;; *)

Printf.printf "Pour ";;
print_brin brin3;;
Printf.printf " et";;
print_brin brin1;; 
Printf.printf ", distance(brin3, brin 1) devrait retourner une erreur, car brin1 est vide\n";;
(* distance brin3 brin1;; *)

Printf.printf "Pour ";;
print_brin brin2;;
Printf.printf " et";;
print_brin brin4;; 
Printf.printf ", distance(brin2, brin4) devrait retourner une erreur, car brin2 et brin4 n'ont pas la même taille\n";;
(* distance brin2 brin4;; *)

Printf.printf "La distance entre ";;
print_brin brin2;;
Printf.printf " et";;
print_brin brin2;;
Printf.printf " devrait être 0, et distance(brin2, brin2) retourne : %i\n" (distance brin2 brin2);;

Printf.printf "La distance entre ";;
print_brin brin2;;
Printf.printf " et";;
print_brin brin3;;
Printf.printf " devrait être 1, et distance(brin2, brin3) retourne : %i\n" (distance brin2 brin3);;

Printf.printf "La distance entre ";;
print_brin brin4;;
Printf.printf " et";;
print_brin brin5;;
Printf.printf " devrait être 2, et distance(brin4, brin5) retourne : %i\n" (distance brin4 brin5);;


Printf.printf "\n";;


(* Test de la fonction similarite
    - L'une des deux liste est vide -> lève bien une exception
    - Les deux brins ont une taille différente
    - Plusieurs brins de même taille  *)

Printf.printf "**QUESTION 4**\n";;
Printf.printf "**Test de la fonction similarite**\n\n";;


let brin1 = [];;
let brin2 = G::T::[];;
let brin3 = C::G::A::T::[];;
let brin4 = T::A::G::T::[];;
let brin5 = A::G::C::T::[];;
let brin6 = T::A::A::G::[];;


Printf.printf "Pour ";;
print_brin brin1;;
Printf.printf " et";;
print_brin brin2;; 
Printf.printf ", similarite(brin1, brin2) devrait retourner une erreur, car brin1 est vide\n";;
(* similarite brin1 brin2;; *)

Printf.printf "Pour ";;
print_brin brin3;;
Printf.printf " et";;
print_brin brin1;; 
Printf.printf ", similarite(brin3, brin 1) devrait retourner une erreur, car brin1 est vide\n";;
(* similarite brin3 brin1;; *)

Printf.printf "Pour ";;
print_brin brin2;;
Printf.printf " et";;
print_brin brin3;; 
Printf.printf ", similarite(brin2, brin3) devrait retourner une erreur, car brin2 et brin3 n'ont pas la même taille\n";;
(* similarite brin2 brin3;; *)

Printf.printf "La similarité entre ";;
print_brin brin3;;
Printf.printf " et";;
print_brin brin4;;
Printf.printf " devrait être 0.25, et similarite(brin3, brin4) retourne : %f\n" (similarite brin3 brin4);;

Printf.printf "La similarité entre ";;
print_brin brin5;;
Printf.printf " et";;
print_brin brin6;;
Printf.printf " devrait être 0, et similarite(brin5, brin6) retourne : %f\n" (similarite brin5 brin6);;

Printf.printf "La similarité entre ";;
print_brin brin5;;
Printf.printf " et";;
print_brin brin5;;
Printf.printf " devrait être 1, et similarite(brin5, brin5) retourne : %f\n" (similarite brin5 brin5);;


Printf.printf "\n";;


(* @requires chain est une chaine d'acides
    assigns rien
    ensures retourne rien
            affiche sur la console la chaine
            sinon lève une exception car la liste entrée est invalide, ie n'est pas une liste d'acides *)
let rec print_chain chain = match chain with
  | [] -> Printf.printf("[]")
  | a::an -> if a=Ala then ( Printf.printf " Ala::" ; print_chain an )
             else if a = Arg then ( Printf.printf " Arg::" ; print_chain an )
             else if a = Asn then ( Printf.printf " Asn::" ; print_chain an )
             else if a = Asp then ( Printf.printf " Asp::" ; print_chain an )
             else if a = Cys then ( Printf.printf " Cys::" ; print_chain an )
             else if a = Glu then ( Printf.printf " Glu::" ; print_chain an )
             else if a = Gln then ( Printf.printf " Gln::" ; print_chain an )
             else if a = Gly then ( Printf.printf " Gly::" ; print_chain an )
             else if a = His then ( Printf.printf " His::" ; print_chain an )
             else if a = Ile then ( Printf.printf " Ile::" ; print_chain an )
             else if a = Leu then ( Printf.printf " Leu::" ; print_chain an )
             else if a = Lys then ( Printf.printf " Lys::" ; print_chain an )
             else if a = Phe then ( Printf.printf " Phe::" ; print_chain an )
             else if a = Pro then ( Printf.printf " Pro::" ; print_chain an )
             else if a = Ser then ( Printf.printf " Ser::" ; print_chain an )
             else if a = Thr then ( Printf.printf " Thr::" ; print_chain an )
             else if a = Trp then ( Printf.printf " Trp::" ; print_chain an )
             else if a = Tyr then ( Printf.printf " Tyr::" ; print_chain an )
             else if a = Val then ( Printf.printf " Val::" ; print_chain an )
             else invalid_arg "print_chain : argument invalide (on attend une liste d'acides)";;


(* Test de la fonction brin_vers_chaine
    - Liste vide -> lève bien une exception
    - Quatres brins différents :
        - deux brins bien formés
        - deux brins mal formés *)

Printf.printf "**QUESTION 5**\n";;
Printf.printf "**Test de la fonction brin_vers_chaine**\n\n";;


let brin1 = [];;
let brin2 = T::A::C::G::G::C::T::A::G::A::T::T::T::A::C::G::C::T::A::A::T::A::T::C::[];;
let brin3 = T::A::C::G::G::C::T::A::G::T::A::G::T::A::C::G::C::T::A::A::T::A::T::C::[];;
let brin4 = T::A::C::T::A::C::[];;
let brin5 = T::A::C::G::G::A::T::C::[];;


Printf.printf "Pour ";;
print_brin brin1;; 
Printf.printf ", brin_vers_chaine(brin1) devrait retourner une erreur, car brin1 est vide\n";;
(* brin_vers_chaine brin1;; *)

Printf.printf "La première chaîne (codons) de ";;
print_brin brin2;;
Printf.printf " est ";;
print_chain (brin_vers_chaine brin2);;

Printf.printf "\nLa première chaîne (codons) de ";;
print_brin brin3;;
Printf.printf " est ";;
print_chain (brin_vers_chaine brin3);;

Printf.printf "\nPour ";;
print_brin brin4;; 
Printf.printf ", brin_vers_chaine(brin4) devrait retourner une erreur, car brin4 est mal formé\n";;
(* brin_vers_chaine brin4;; *)

Printf.printf "Pour ";;
print_brin brin4;; 
Printf.printf ", brin_vers_chaine(brin5) devrait retourner une erreur, car brin5 est mal formé";;
(* brin_vers_chaine brin5;; *)


Printf.printf "\n\n";;


Printf.printf "--Arbres phylogénétiques--\n\n";;


(* Test de la fonction arbre_phylo_vers_string
    - Des arbres non complet :
        - une feuille est vide
        - un brin d'une des branche est vide
        - un brin a une taille différente avec les autres
        - définition de l'arbre complet non respecté, ie mauvais calcul du malus pour une branche
    - Un arbre complet *)

Printf.printf "**QUESTION 1**\n";;
Printf.printf "**Test de la fonction arbre_phylo_vers_string**\n\n";;


let arbre1 = Br(Br(Lf([]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre2 = Br(Br(Lf(G::C::A::T::[]), [], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre3 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::A::[], 2, Lf(G::A::G::A::[])));;
let arbre4 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 7, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre5 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;


Printf.printf "Pour arbre1, is_complet(arbre1) devrait retourner un entier positif > 0, car une des feuilles est vide et retourne : %i\n" (is_complet arbre1);;
Printf.printf "Donc arbre_phylo_vers_string(arbre1) devrait retourner une erreur\n";;
(* arbre_phylo_vers_string arbre1;; *)
Printf.printf "\n";;

Printf.printf "Pour arbre2, is_complet(arbre2) devrait retourner un entier positif > 0, car le brin d'une des branches est vide et retourne : %i\n" (is_complet arbre2);;
Printf.printf "Donc arbre_phylo_vers_string(arbre2) devrait retourner une erreur\n";;
(* arbre_phylo_vers_string arbre2;; *)
Printf.printf "\n";;


Printf.printf "Pour arbre3, is_complet(arbre3) devrait retourner un entier positif > 0, car tous les brins n'ont pas la même taille : %i\n" (is_complet arbre3);;
Printf.printf "Donc arbre_phylo_vers_string(arbre3) devrait retourner une erreur\n";;
(* arbre_phylo_vers_string arbre3;; *)
Printf.printf "\n";;


Printf.printf "Pour arbre4, is_complet(arbre4) devrait retourner un entier positif > 0, car l'arbre n'est pas complet : %i\n" (is_complet arbre4);;
Printf.printf "Donc arbre_phylo_vers_string(arbre4) devrait retourner une erreur\n";;
(* arbre_phylo_vers_string arbre4;; *)
Printf.printf "\n";;


Printf.printf "En utilisant arbre_phylo_vers_string pour arbre5, on obtient :\n";;
print_string (arbre_phylo_vers_string arbre5);;
Printf.printf "\navec lequel la fonction is_complet devrait retourner 0 et retourne : %i\n" (is_complet arbre5);;


Printf.printf "\n";;


(* Test de la fonction similaire
    - Deux arbres dont un incomplet
    - Deux arbres ayant une hauteur différente
    - Deux arbres ayant une taille de brins différente
    - Deux arbres complets ayant la même forme
    - Un arbre et une liste de deux arbres *)

Printf.printf "**QUESTION 2**\n";;
Printf.printf "**Test de la fonction similaire**\n\n";;

let arbre1 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 7, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre2 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre3 = Br(Br(Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])), A::C::A::T::[], 7, Lf(T::C::G::T::[])), A::A::A::A::[], 12, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre4 = Br(Br(Lf(G::C::A::T::C::[]), A::C::A::T::G::[], 5, Lf(T::C::G::T::A::[])), A::A::A::A::A::[], 14, Br(Lf(T::A::G::A::C::[]), A::A::G::A::T::[], 4, Lf(G::A::G::A::G::[])));;
let arbre5 = Br(Br(Lf(G::A::A::T::[]), G::C::T::T::[], 5, Lf(C::G::A::T::[])), A::A::T::A::[], 12, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 3, Lf(G::T::G::A::[])));;
let arbre6 = Br(Br(Lf(G::A::A::T::[]), G::C::T::T::[], 5, Lf(T::G::G::T::[])), A::A::T::A::[], 12, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 3, Lf(G::T::G::A::[])));;

Printf.printf "Lors des tests de la fonction similaire, on cherche à comparer une liste d'arbres avec arbre2 :\n";;
print_string (arbre_phylo_vers_string arbre2);;

Printf.printf "\nsimilaire(arbre1, [arbre2]) devrait retourner une erreur, car arbre1 n'est pas complet (is_complet(arbre1) retourne %i > 0)\n" (is_complet arbre1);;
(* similaire arbre2 (arbre1::[]);; *)
Printf.printf "\n\n";;

print_string (arbre_phylo_vers_string arbre3);;
Printf.printf "\nn'ayant pas la même forme (car n'ont pas la même taille) que arbre2\n";;
Printf.printf "similaire(arbre2, [arbre3]) devrait retourner une erreur\n";;
(* similaire arbre2 (arbre3::[]);; *)
Printf.printf "\n";;

print_string (arbre_phylo_vers_string arbre4);;
Printf.printf "\nn'ayant pas la même forme (car les brins n'ont pas la même taille) que arbre2\n";;
Printf.printf "similaire(arbre2, [arbre4]) devrait retourner une erreur\n";;
(* similaire arbre2 (arbre4::[]);; *)
Printf.printf "\n";;

Printf.printf "Avec la liste contenant seulement l'arbre\n";;
print_string (arbre_phylo_vers_string arbre5);;
Printf.printf "\nsimilaire(arbre2, [arbre5]) devrait seulement retourner arbre5 et retourne :\n";;
print_string (arbre_phylo_vers_string (similaire arbre2 (arbre5::[])));;

Printf.printf "\n\nAvec la liste contenant les deux arbres\n";;
print_string (arbre_phylo_vers_string arbre5);;
Printf.printf "\net\n";;
print_string (arbre_phylo_vers_string arbre6);;
Printf.printf "\nsimilaire(arbre2, [arbre5, arbre6]) devrait retourner arbre6 et retourne : \n";;
print_string (arbre_phylo_vers_string (similaire arbre2 (arbre5::arbre6::[])));;


Printf.printf "\n\n";;


Printf.printf "**QUESTION 3**\n";;
Printf.printf "**Test des fonctions auxilliaires**\n\n";;

(* Test de la fonction get_root
    - Un arbre incomplet
    - Une feuille
    - Une branche
    - Un arbre phylogénétique *)

Printf.printf "*Test de la fonciton get_root*\n\n";;

let arbre1 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 7, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre2 = Lf(T::A::A::G::[]);;
let arbre3 = Br(Lf(G::C::A::T::C::[]), A::C::A::T::G::[], 5, Lf(T::C::G::T::A::[]));;
let arbre4 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;

Printf.printf "Pour arbre1, get_root(arbre1) devrait lever une erreur, car arbre1 n'est pas complet (is_complet(arbre1) retourne %i > 0)\n\n" (is_complet arbre1);;
(* get_root arbre1;; *)

Printf.printf "Pour\n";;
print_string (arbre_phylo_vers_string arbre2);; 
Printf.printf ", \nget_root(arbre2) devrait retourner une erreur, car arbre2 est juste une feuille";;
(* get_root arbre2;; *)

Printf.printf "\n\nPour\n";;
print_string (arbre_phylo_vers_string arbre3);; 
Printf.printf ", \nget_root(arbre3) devrait retourner ";;
print_brin (A::C::A::T::G::[]);;
Printf.printf " et retoune :" ;;
print_brin (get_root arbre3);;

Printf.printf "\n\nPour\n";;
print_string (arbre_phylo_vers_string arbre4);; 
Printf.printf ", \nget_root(arbre4) devrait retourner ";;
print_brin (A::A::A::A::[]);;
Printf.printf " et retoune :" ;;
print_brin (get_root arbre4);;

(* Test de la fonction get_malus
    - Un arbre incomplet
    - Une feuille
    - Une branche
    - Un arbre phylogénétique *)

Printf.printf "\n\n*Test de la fonciton get_malus*\n\n";;

Printf.printf "Pour arbre1, get_malus(arbre1) devrait retourner une erreur, car arbre1 n'est pas complet (is_complet(arbre1) retourne %i > 0)\n\n" (is_complet arbre1);;
(* get_malus arbre1;; *)

Printf.printf "Pour\n";;
print_string (arbre_phylo_vers_string arbre2);; 
Printf.printf ", \nget_malus(arbre2) devrait retourner une erreur, car arbre2 est juste une feuille\n\n";;
(* get_malus arbre2;; *)

Printf.printf "Pour\n";;
print_string (arbre_phylo_vers_string arbre3);; 
Printf.printf ", \nget_malus(arbre3) devrait retourner 5 et retourne : %i\n\n" (get_malus arbre3);;

Printf.printf "Pour\n";;
print_string (arbre_phylo_vers_string arbre4);; 
Printf.printf ", \nget_malus(arbre4) devrait retourner 8 et retourne : %i\n\n" (get_malus arbre4);;

(* Test de la fonction br
    - Une des branches est incomplètes
    - Une des branches possède des brins qui n'ont pas la même taille que les brins des autres
    - Un brin vide
    - Un brin qui n'a pas la même taille que les autres brins
    - Deux arbres complets et un brin valide *)

Printf.printf "*Test de la fonciton br*\n\n";;

let arbre1 = Br(Lf(G::C::A::T::[]), A::C::A::T::[], 2, Lf(T::C::G::T::[]));;
let arbre2 = Lf(T::A::A::G::[]);;
let arbre3 = Br(Lf(G::C::A::T::C::[]), A::C::A::T::G::[], 5, Lf(T::C::G::T::A::[]));;
let arbre4 = Br(Lf(G::A::A::T::[]), G::C::T::T::[], 5, Lf(T::G::G::T::[]));;

let brin1 = [];;
let brin2 = G::T::G::G::G::[];;
let brin3 = C::T::C::C::[];;

Printf.printf "Pour arbre1, br(arbre1, brin3, arbre2) devrait lever une erreur, car arbre1 n'est pas complet (is_complet(arbre1) retourne %i > 0)\n\n" (is_complet arbre1);;
(* br (arbre1, brin3, arbre2);; *)

Printf.printf "Pour le brin ";;
print_brin brin3;;
Printf.printf " et les deux arbres\n";;
print_string (arbre_phylo_vers_string arbre2);;
Printf.printf "\net\n";;
print_string (arbre_phylo_vers_string arbre3);;
Printf.printf "\nbr(arbre2, brin3, arbre3) devrait retourner une erreur, car les brins d'arbre3 n'ont pas la même taille que ceux d'arbre2 ou de brin3\n\n";;
(* br (arbre2, brin3, arbre3);; *)

Printf.printf "Pour le brin ";;
print_brin brin1;;
Printf.printf " et les deux arbres\n";;
print_string (arbre_phylo_vers_string arbre2);;
Printf.printf "\net\n";;
print_string (arbre_phylo_vers_string arbre4);;
Printf.printf "\nbr(arbre2, brin1, arbre4) devrait retourner une erreur, car le brin donné en argument est vide\n\n";;
(* br (arbre2, brin1, arbre4);; *)

Printf.printf "Pour le brin ";;
print_brin brin2;;
Printf.printf " et les deux arbres\n";;
print_string (arbre_phylo_vers_string arbre2);;
Printf.printf "\net\n";;
print_string (arbre_phylo_vers_string arbre4);;
Printf.printf "\nbr(arbre2, brin2, arbre4) devrait retourner une erreur, car brin2 n'a pas la même taille que les brins d'arbre3 ou ceux d'arbre2\n\n";;
(* br (arbre2, brin2, arbre4);; *)

Printf.printf "Pour le brin ";;
print_brin brin3;;
Printf.printf " et les deux arbres\n";;
print_string (arbre_phylo_vers_string arbre2);;
Printf.printf "\net\n";;
print_string (arbre_phylo_vers_string arbre4);;
Printf.printf "\nbr(arbre2, brin3, arbre4) devrait retourner un arbre phylogénétique tel que la racine est Br(arbre2, brin3, arbre4) et retourne\n";;
print_string (arbre_phylo_vers_string(br (arbre2, brin3, arbre4)));;


Printf.printf "\n\n";;

(* @requires liste_a est une liste d'arbres phylogénétiques
    assigns rien
    ensures retourne une représentation unique d'une liste d'arbres
            sinon lève une erreur car un des arbres de la liste n'est pas complet *)
let rec arbres_vers_string liste_a = match liste_a with
    | [] -> "[]"
    | a::an -> if (is_complet a) != 0 then invalid_arg "arbres_vers_string : liste_a contient un arbre non complet"
               else " "^(arbre_phylo_vers_string a)^"\n::"^(arbres_vers_string an);;

Printf.printf "**QUESTION 4**\n";;
Printf.printf "**Test de la fonction gen_phylo**\n\n";;

let brin1 = [];;
let brin2 = G::C::T::A::C::[];;
let brin3 = C::G::A::T::[];;
let brin4 = T::A::G::T::[];;
let brin5 = A::G::C::T::[];;

Printf.printf "Pour\n";;
print_brin brin1;;
Printf.printf ", ";;
print_brin brin3;;
Printf.printf " et ";;
print_brin brin4;;
Printf.printf "\ngen_phylo(brin1, brin3, brin4) devrait retourner une erreur, car brin1 est un brin vide\n\n";;
(* gen_phylo brin1 brin3 brin4;; *)

Printf.printf "Pour\n";;
print_brin brin3;;
Printf.printf ", ";;
print_brin brin2;;
Printf.printf " et ";;
print_brin brin4;;
Printf.printf "\ngen_phylo(brin3, brin2, brin4) devrait retourner une erreur, car brin2 n'a pas la même taille que brin3 ou brin4\n\n";;
(* gen_phylo brin3 brin2 brin4;; *)

Printf.printf "Pour\n";;
print_brin brin3;;
Printf.printf ", ";;
print_brin brin4;;
Printf.printf " et ";;
print_brin brin5;;
Printf.printf ", \ngen_phylo(brin3, brin4, brin5) devrait retourner une liste de trois branches et retourne :\n";;
print_string (arbres_vers_string(gen_phylo brin3 brin4 brin5));;


Printf.printf "\n\n";;

(* Test de la fonction min_malus
    - Une liste vide
    - Une liste contenant un arbre incomplet
    - Une liste d'arbre phylogénétique *)

Printf.printf "**QUESTION 5**\n";;
Printf.printf "**Test de la fonction min_malus**\n\n";;

let arbre1 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 8, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre2 = Br(Br(Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])), A::C::A::T::[], 7, Lf(T::C::G::T::[])), A::A::A::A::[], 12, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;
let arbre3 = Br(Br(Lf(G::C::A::T::[]), A::C::A::T::[], 3, Lf(T::C::G::T::[])), A::A::A::A::[], 7, Br(Lf(T::A::G::A::[]), A::A::G::A::[], 2, Lf(G::A::G::A::[])));;


let liste1 = [];;
let liste2 = arbre2::arbre3::[];;
let liste3 = arbre1::arbre2::[];;

Printf.printf "liste1 étant une liste vide, min_malus(liste1) devrait retourner une erreur\n\n";;
(* min_malus liste1;; *)

Printf.printf "liste2 contenant un arbre incomplet, min_malus(liste2) devrait retourner une erreur\n\n";;
(* min_malus liste2;; *)

Printf.printf "Pour \n";;
print_string (arbres_vers_string liste3);;
Printf.printf "\n min_malus(liste3) devrait retourner arbre1 (car son malus est de %i, tandis que celui de arbre2 est %i) et retourne\n" (get_malus arbre1) (get_malus arbre2);;
print_string(arbre_phylo_vers_string(min_malus liste3));;


Printf.printf "\n\n";;


Printf.printf "**QUESTION 6**\n";;
Printf.printf "**Test de la fonction gen_min_malus_phylo**\n\n";;

Printf.printf "La question 6 n'a pas été traitée";;