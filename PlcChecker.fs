module PlcChecker

open Absyn
open Environ


(* Type checker for the first-order version of Micro-ML *)


// The type checker can be seen as an interpreter that computes
// the type of an expression instead of its value.

let rec matchChecker (v : expr) (elist: (expr option * expr) list) : expr =
  match v, elist with
  | v1, [] -> failwith "Checker: No compatible matches found!"
  | v1, (Some (e6), e1) :: t -> if (v1 = e6) then e1 else (matchChecker v t)
  | v1, (None, e1) :: t -> e1

let rec teval (e : expr) (env : plcType env) : plcType =
    match e with
    | List [] -> ListT []

    | List es -> ListT (List.map (fun e -> teval e env) es)

    | ConI i -> IntT

    | ConB b -> BoolT

    | Var x -> lookup env x

    | ESeq s -> 
      match s with
      | SeqT _ -> s

    | Let(x, e1, lBody) -> 
      let varType = teval e1 env 
      let lBodyEnv = (x, varType) :: env
      teval lBody lBodyEnv  

    | Prim1 (op, e1) -> 
      let i1 = teval e1 env in
        match op with
        | "hd" -> match i1 with 
                  | SeqT x -> x 
                  | _ -> failwith ("Checker: Header (hd) cannot be used for Prim1  on type ")
        | "tl" -> match i1 with 
                  | SeqT x -> SeqT x 
                  | _ -> failwith ("Checker: Tail (tl) cannot be used for Prim1  on type ")
        | "ise" -> match i1 with 
                   | SeqT _ -> BoolT
                   | _ -> failwith ("Checker: ise cannot be used for Prim1 on type ")
        | "print" -> ListT []
        | "-" -> if (i1 = IntT) then (IntT; IntT) else failwith ("Checker: 'negation' cannot be used for Prim1 on type ")
        | "!" -> if (i1 = BoolT) then (BoolT; BoolT) else failwith ("Checker: 'not' cannot be used for Prim1 on type ")
        | _ -> failwith ("Checker: The unary operator used for Prim1 has not been defined ")
    
    | Prim2 (op, e1, e2) -> 
      let i1 = teval e1 env in
        let i2 = teval e2 env in
          match op with
          | ";" -> (i1; i2; i2) 
          | "::" -> if (i2 = SeqT i1) then i2 else failwith "Checker: Same types expected!"
          | "&&" -> if (i1 = BoolT && i1 = i2) then (BoolT; BoolT; BoolT) 
                    else failwith ("Checker: Unable to perform addition operation. Int types required!")
          | "<=" -> if (i1 = IntT && i1 = i2) then (IntT; IntT; BoolT) 
                    else failwith ("Checker: Unable to perform less than equal to operation. Int types required!")
          | "/" -> if (i1 = IntT && i1 = i2) then (IntT; IntT; IntT) 
                   else failwith ("Checker: Unable to perform division operation. Int types required!")
          | "*" -> if (i1 = IntT && i1 = i2) then (IntT; IntT; IntT) 
                   else failwith ("Checker: Unable to perform multiplication operation. Int types required!")
          | "+" -> if (i1 = IntT && i1 = i2) then (IntT; IntT; IntT) 
                   else failwith ("Checker: Unable to perform addition operation. Int types required!")
          | "-" -> if (i1 = IntT && i1 = i2) then (IntT; IntT; IntT) 
                   else failwith ("Checker: Unable to perform subtraction operation. Int types required!")
          | "!=" -> if (i1 = i2) then (i1; i2; BoolT) 
                    else failwith ("Checker: Same return types required!")
          | "=" -> if (i1 = i2) then (i1; i2; BoolT) 
                   else failwith ("Checker: Same return types required!")
          | "<" -> if (i1 = IntT && i1 = i2) then (IntT; IntT; BoolT) 
                   else failwith ("Checker: Unable to perform less than operation. Int types required!")
          | _   -> failwith ("Checker: The given binary operator is not defined in PLC")
    
    | Anon (varType, x, lBody) -> 
      let varBodyEnv = (x, varType) :: env in
        let returnType = (teval lBody varBodyEnv) in
          FunT (varType, returnType)
    
    | Match (e1, elist) -> teval (matchChecker e1 elist) env
    
    | Letrec(f, varType, x, retTyp, funcBody, letBody) -> 
      let funcType = FunT(varType, retTyp) 
      let funcBodyEnv = (x, varType) :: (f, funcType) :: env
      let letBodyEnv = (f, funcType) :: env
      if teval funcBody funcBodyEnv = retTyp
        then teval lBody lBodyEnv
      else failwith ("Checker: The return type of the given function is not a match with " + type2string (teval fBody fBodyEnv))   
   
    | If (e1, e2, e3) -> 
      match (teval e1 env), (teval e2 env), (teval e3 env) with
      | BoolT, t2, t3 when (t2 = t3) -> t2
      | BoolT, t2, t3 when (t2 <> t3) -> 
        failwith ("Checker: The return types were expected to be the same ")
      | t1, t2, t3 -> 
        failwith ("Checker: The first expresssion was expected to be of type bool")
    
    | Call(f, args1) -> 
      match (teval f env) with
      | FunT(varType, retTyp) -> 
        if teval args1 env = varType 
          then retTyp 
        else failwith ("Call: The given arguements are not compatible with PLC")
      | _ -> failwith ("Call: The given function has not been defined ")
    
    | Item (n, e1) -> 
    match teval e1 env with
    | ListT vs -> 
      try 
        List.item (n - 1) vs
      with
        | :? System.ArgumentException -> failwith ("Checker: Index out of bounds! ")
    | _ -> failwith ("Checker: The given input is not of a list type ")
