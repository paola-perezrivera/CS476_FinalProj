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
        if (j = max+.1.) then (0.0,0.0) else 
                if (c1 *. i +. c2 *. j) = b then (i,j) else (findAllJ b c1 c2 i (j+.1.) max)


let rec findAll max secondMax b c1 c2 i = 
        if (i = max+.1.) then [] else 
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

let test1 = eval_exp (Budget (Num 10.0, Num 2.0, Num 0.5));;
let test2 = eval_exp (Budget (Num 36.0, Num 4.0, Num 3.0));;