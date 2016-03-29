(* Tri par selection du minimum *)

(* Question 1 *)

let selectionne inf l =
    let rec selectionne_bis inf l n = (* n = valeur a comparer *)
        match l with
        [] -> n
        |x::r ->    if inf x n
                    then selectionne_bis inf r x
                    else selectionne_bis inf r n in
    selectionne_bis inf l (List.hd l);;

(* Question 2 *)

let rec supprime n l =
    match l with
    [] -> []
    |x::r ->    if x <> n
                then x::(supprime n r)
                else r;;  

(* Question 3*)    

let tri_selection_min inf l =
