(* -----------Exercice 1 .---------*)

type  bintree = Node of (int *  bintree * bintree) | Leaf of int | EMPTY;;

let example_tree = Node (2,Node(7,Leaf 2,Node(6,Leaf 5,Leaf 11)),Node(5,EMPTY,Node(9,Leaf 4,EMPTY)));;


(* -----------Exercice 2 .---------*)


(* Question 1. *)
let rec bintree_count_nodes arbre=match arbre with
| EMPTY -> 0
| Leaf v -> 1
| Node(v,g,d)  -> 1 + bintree_count_nodes g + bintree_count_nodes d;;


(* Question 2. *)

let rec bintree_count_leaves arbre = match arbre with
| EMPTY -> 0
| Leaf v -> 1
| Node(v,g,d) -> bintree_count_leaves g + bintree_count_leaves d;;

(* Question 3. *)
let rec bintree_count_internal_nodes arbre = match arbre with
| EMPTY -> 0
| Leaf v -> 0
| Node(v,g,d) -> 1 + bintree_count_internal_nodes g + bintree_count_internal_nodes d;;


(* -----------Exercice 3 .---------*)

(* Question 1. *)


let rec bintree_height arbre = match arbre with
| EMPTY -> 0
| Leaf v -> 1
| Node(v,g,d) -> 1 + max (bintree_height g)  (bintree_height d);;

(* Question 2. *)
let rec bintree_is_mirror arbre1 arbre2 = match arbre1, arbre2 with
|EMPTY,EMPTY-> true
|EMPTY,_ -> false
|_,EMPTY-> false
|Leaf v,Leaf v1-> true
|Leaf v1,Node(v,g,d) -> false
|Node(v,g,d),Leaf v2-> false
|Node (v1,g1,d1), Node (v2,g2,d2) -> bintree_is_mirror g1 d2 && bintree_is_mirror d1 g2;;


(* Question 3. *)
let  bintree_is_symetric arbre = match arbre with
| EMPTY -> true
| Leaf v -> true
| Node(v,g,d) -> bintree_is_mirror g d ;;


(* -----------Exercice 4 .---------*)
(* Question 1. *)
let rec bintree_pre arbre = match arbre with
| EMPTY -> []
| Leaf v -> [v]
| Node(v,g,d) -> [v]@(bintree_pre g)@(bintree_pre d);;

(* Question 2. *)
let rec bintree_post arbre = match arbre with
| EMPTY -> []
| Leaf v -> [v]
| Node(v,Leaf v1,Leaf v2) -> v1::v2::v::[]
| Node(v,Leaf v1,EMPTY) -> v1::v::[]
| Node(v,EMPTY,Leaf v2) -> v2::v::[]
| Node (v,EMPTY,EMPTY) -> v::[]
| Node (v,g,d) -> (bintree_post g)@(bintree_post d)@[v];;

(* Question 3. *)
let rec bintree_in arbre = match arbre with
| EMPTY -> []
| Leaf v -> [v]
| Node(v,g,d) -> (bintree_in g)@[v]@(bintree_in d);;



(*------------Exercice 5.---------*)
(*  Question 1.*)

let rec bintree_insert arbre n = match arbre with
| EMPTY -> Leaf n
| Leaf v when n > v -> Node(v,EMPTY,Leaf n)
| Leaf v -> Node(v,Leaf n,EMPTY)
| Node(v,g,d) when n>v -> Node(v,g,bintree_insert d n)
| Node (v,g,d) -> Node(v,bintree_insert g n,d) ;;


(* Question 2.*)

let rec bintree_search arbre n =match arbre with
| EMPTY -> false
| Leaf v when v=n -> true
| Leaf v -> false
| Node(v,g,d) when v=n -> true
| Node(v,g,d) when n<v -> bintree_search g n
| Node(v,g,d) -> bintree_search d n;;
