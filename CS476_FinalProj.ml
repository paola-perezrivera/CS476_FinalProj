(* Authors: Ashley Stojak (653366198)
            Shruti Italia (674095977)
            Paola Perez-Rivera (653901920)
*)

open List

type exp = Num of float
        | NumList of float list
        | EarnedVal of exp * exp
        | ActualVal of exp * exp
        | CostVar of exp * exp
        | CostPer of exp * exp
        | Sum of exp
        | Avg of exp 
        | Budget of exp * exp * exp;;

type value = FloatVal of float | FloatList of float list | FloatTuple of (float * float) list;;

let rec sum el =
        match el with
        | [] -> 0.
        | hd :: tl -> hd +. (sum tl);;

let rec count el =
        match el with
        | [] -> 0.
        | hd :: tl -> 1. +. (count tl);;

let findMax b findingMax = 
        b /. findingMax


let rec findAllJ b c1 c2 i j max = 
        if (j >= max+.1.) then (0.0,0.0) else 
                if (c1 *. i +. c2 *. j) = b then (i,j) else (findAllJ b c1 c2 i (j+.1.) max)


let rec findAll max secondMax b c1 c2 i = 
        if (i >= max+.1.) then [] else 
                let j = (findAllJ b c1 c2 i 0. secondMax) in
                if (j = (0.0,0.0)) then (findAll max secondMax b c1 c2 (i+.1.)) else 
                j::(findAll max secondMax b c1 c2 (i+.1.))


let rec eval_exp ( e:exp ) =
        match e with
        | Num e -> Some( FloatVal e )
        | NumList el -> Some( FloatList el)
        | EarnedVal (e1, e2) -> (match eval_exp e1, eval_exp e2 with
                                        | Some( FloatVal f1), Some( FloatVal f2) -> Some( FloatVal( f1 *. f2))
                                        | _,_ -> None)
        | ActualVal( e1, e2) -> (match eval_exp e1, eval_exp e2 with
                                        | Some( FloatVal f1), Some( FloatVal f2) -> Some( FloatVal( f1 *. f2))
                                        | _, _ -> None)
        | CostVar( e1, e2) -> (match eval_exp e1, eval_exp e2 with
                                        | _, Some( FloatVal 0.) -> None
                                        | Some( FloatVal f1), Some(FloatVal f2) -> Some( FloatVal( f1 /. f2))
                                        | _, _ -> None)
        | CostPer( e1, e2) -> (match eval_exp e1, eval_exp e2 with
                                        | Some( FloatVal f1), Some(FloatVal f2) -> Some( FloatVal( f1 -. f2))
                                        | _, _ -> None)
        | Sum( el) -> (match eval_exp el with
                        | Some( FloatList el) -> Some( FloatVal( sum(el)))
                        | _ -> None)
        | Avg( el) -> (match eval_exp el with
                        | Some( FloatList f1) -> Some( FloatVal ( sum(f1) /. count(f1)))
                        | _ -> None)
        | Budget (b, e1, e2) -> (match eval_exp b, eval_exp e1, eval_exp e2 with
                                | Some( FloatVal f1), Some(FloatVal f2), Some(FloatVal f3) -> let max = findMax f1 f2 in
                                                                                              let secondMax = findMax f1 f3 in
                                                                                              Some(FloatTuple(findAll max secondMax f1 f2 f3 0.))
                                | _ -> None);;

(* tests for num *)
(* let e = eval_exp( Num 5) (* throws an error because expects float, not int *) *)
let e2 = eval_exp( Num 5.) (* return Some( FloatVal  5.)*)

(* tests for NumList *)
(* let el = eval_exp( NumList[1;2;3;4;5]) (* throws an error because expects float list, not int list*)*)
let el = eval_exp( NumList[ 1.; 2.; 3.; 4.; 5.]);; (* return Some( FloatList [1.;2.;3.;4.;5.]) *)

(* tests for EarnedVal *)
let earned_test1 = eval_exp( EarnedVal( Num 5., Num 4.));; (* return Some( FloatVal 20.) *)

(* CostVar tests *)
let costvar_test1 = eval_exp( CostVar( EarnedVal( Num 5., Num 4.), ActualVal( Num 8., Num 9.)));; (* return Some( FloatVal (0.27777779)) *)
let costvar_test2 = eval_exp( CostVar( Num 7., Num 0.)) (* return None *)
let costvar_test3 = eval_exp( CostVar( Num 7., Num 2.)) (* return Some(FloatVal 3.5) *)

(* CostPer tests *)
let costper_test1 = eval_exp( CostPer( EarnedVal( Num 8., Num 2.), ActualVal( Num 9., Num 3.)));; (* return Some( FloatVal ( -11.)) *)
let costper_test2 = eval_exp( CostPer( Num 7., Num 4.));; (* return Some( FloatVal 3.) *)

(* tests for sum *)
let sum_test1 = eval_exp (Sum (NumList [3.0; 6.0; 9.0; 23.0]));; (* should be (FloatVal 41.) *)
let sum_test2 = eval_exp (Sum (NumList [5.0; 10.0; 6.1; 0.8]));; (* should be (FloatVal 21.9) *)
let sum_test3 = eval_exp( Sum( NumList [1.; 2.; 3.; 4.; 5.]));; (* return Some( FloatVal 15.) *)
let avg_test4 = eval_exp( Avg( NumList [1.; 2.; 3.; 4.; 5.]));; (* return Some( FloatVal 3.) *)

(* tests for average *)
let avg_test1 = eval_exp (Avg (NumList [1.0; 4.0; 5.0; 8.0]));; (* should be (FloatVal 4.5) *)
let avg_test2 = eval_exp (Avg (NumList [8.0; 9.0; 20.1; 5.8]));; (* should be (FloatVal 10.7250000000000014) *)
let avg_test3 = eval_exp( Avg( NumList [1.; 2.; 3.; 4.; 5.]));; (* return Some( FloatVal 3.) *)

(* tests for budget *)
let budget_test1 = eval_exp (Budget (Num 10.0, Num 2.0, Num 0.5));; (* should be Some (FloatTuple [(0., 20.); (1., 16.); (2., 12.); (3., 8.); (4., 4.); (5., 0.)]) *)
let budget_test2 = eval_exp (Budget (Num 36.0, Num 4.0, Num 3.0));; (* should be Some (FloatTuple [(0., 12.); (3., 8.); (6., 4.); (9., 0.)]) *)
let budget_test3 = eval_exp (Budget (Num 36.0, Num 2.0, Num 12.0));; (* should be Some (FloatTuple [(0., 3.); (6., 2.); (12., 1.); (18., 0.)]) *)
let budget_test4 = eval_exp (Budget (Num 36.0, Num 2.0, Num 5.0));; (* should be Some (FloatTuple [(3., 6.); (8., 4.); (13., 2.); (18., 0.)]) *)