(* Les types OCaml correspondant à un nucléotide et à un brin d’ADN  *)

type nucleotide = A | C | G | T;;
type brin = nucleotide list;;


(* Les types OCaml correspondant aux acides aminés *)

type acide = Ala | Arg | Asn | Asp | Cys | Glu | Gln
           | Gly | His | Ile | Leu | Lys | Phe | Pro
           | Ser | Thr | Trp | Tyr | Val | START | STOP;;

(* @requires trois nucléotides
    assigns rien
    ensures l'acide aminé encodé par le triplet de nucléotides (nommés aussicodons) donné en entrée *)
let codon_vers_acide n1 n2 n3 =match n1, n2, n3 with
    | (A,A,A) -> Phe | (A,A,G) -> Phe | (A,A,T) -> Leu
    | (A,A,C) -> Leu | (G,A,A) -> Leu | (G,A,G) -> Leu
    | (G,A,T) -> Leu | (G,A,C) -> Leu | (T,A,A) -> Ile
    | (T,A,G) -> Ile | (T,A,T) -> Ile | (T,A,C) -> START
    | (C,A,A) -> Val | (C,A,G) -> Val | (C,A,T) -> Val
    | (C,A,C) -> Val | (A,G,A) -> Ser | (A,G,G) -> Ser
    | (A,G,T) -> Ser | (A,G,C) -> Ser | (G,G,A) -> Pro
    | (G,G,G) -> Pro | (G,G,T) -> Pro | (G,G,C) -> Pro
    | (T,G,A) -> Thr | (T,G,G) -> Thr | (T,G,T) -> Thr
    | (T,G,C) -> Thr | (C,G,A) -> Ala | (C,G,G) -> Ala
    | (C,G,T) -> Ala | (C,G,C) -> Ala | (A,T,A) -> Tyr
    | (A,T,G) -> Tyr | (A,T,T) -> STOP| (A,T,C) -> STOP
    | (G,T,A) -> His | (G,T,G) -> His | (G,T,T) -> Gln
    | (G,T,C) -> Gln | (T,T,A) -> Asn | (T,T,G) -> Asn
    | (T,T,T) -> Lys | (T,T,C) -> Lys | (C,T,A) -> Asp
    | (C,T,G) -> Asp | (C,T,T) -> Glu | (C,T,C) -> Glu
    | (A,C,A) -> Cys | (A,C,G) -> Cys | (A,C,T) -> STOP
    | (A,C,C) -> Trp | (G,C,A) -> Arg | (G,C,G) -> Arg
    | (G,C,T) -> Arg | (G,C,C) -> Arg | (T,C,A) -> Ser
    | (T,C,G) -> Ser | (T,C,T) -> Arg | (T,C,C) -> Arg
    | (C,C,A) -> Gly | (C,C,G) -> Gly | (C,C,T) -> Gly
    | (C,C,C) -> Gly;;


(* Les types OCaml correspondant aux arbres phylogénétiques *)

type arbre_phylo =
    Lf of brin
  | Br of arbre_phylo * brin * int * arbre_phylo;;