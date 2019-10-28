{--
Practica 4
El lenguaje MiniHS (EAB extendido con cáculo lambda). Semántica
Autores:
Edgar Quiroz Castañeda
Sandra del Mar Soto Corderi
--}

--normal cambiado
--Falta agregar el fix en los eval
--Pura semántica dinamica
--eval le pide al algoritmo de inferencia y pide el contexto vacío

module BAE.Dynamic where

  import BAE.Sintax
  import qualified BAE.Static as Static
  import qualified BAE.Type as Type

  eval1 :: Expr -> Expr
  eval1 expr =
    case expr of
      I n -> error "blocked state: integer"
      B p -> error "blocked state: boolean"
      V x -> error "blocked state: variable"
      Add (I n) (I m) -> I (n + m)
      Add (I n) e -> let e' = eval1 e in Add (I n) e'
      Add e1 e2 -> let e1' = eval1 e1 in Add e1' e2
      Mul (I n) (I m) -> I (n * m)
      Mul (I n) e -> let e' = eval1 e in Mul (I n) e'
      Mul e1 e2 -> let e1' = eval1 e1 in Mul e1' e2
      Succ (I n) -> I (n + 1)
      Succ e -> Succ (eval1 e)
      Pred (I 0) -> I 0
      Pred (I n) -> I (n - 1)
      Pred e -> Pred (eval1 e)
      Not (B p) -> B (not p)
      Not e -> Not (eval1 e)
      And (B p) (B q) -> B (p && q)
      And (B p) e -> let e' = eval1 e in And (B p) e'
      And e1 e2 -> let e1' = eval1 e1 in And e1' e2
      Or (B p) (B q) -> B (p || q)
      Or (B p) e -> let e' = eval1 e in Or (B p) e'
      Or e1 e2 -> let e1' = eval1 e1 in Or e1' e2
      Lt (I n) (I m) -> B (n < m)
      Lt (I n) e -> let e' = eval1 e in Lt (I n) e'
      Lt e1 e2 -> let e1' = eval1 e1 in Lt e1' e2
      Gt (I n) (I m) -> B (n > m)
      Gt (I n) e -> let e' = eval1 e in Gt (I n) e'
      Gt e1 e2 -> let e1' = eval1 e1 in Gt e1' e2
      Eq (I n) (I m) -> B (n == m)
      Eq (I n) e -> let e' = eval1 e in Eq (I n) e'
      Eq e1 e2 -> let e1' = eval1 e1 in Eq e1' e2
      If (B q) e1 e2 -> if q then e1 else e2
      If e1 e2 e3 -> If (eval1 e1) e2 e3
      Let i e1 e2 -> 
        if blocked e1
          then subst e2 (i, e1)
          else Let i (eval1 e1) e2
      Fn x e1 ->  Fn x (eval1 e1)
      Fix f e1 -> Fn f (Fix f e1)
      App f@(Fn x e3) e2 -> 
        if blocked e2
          then subst e3 (x, e2)
          else App f (eval1 e2)
      App e1 e2 -> App (eval1 e1) e2
        

  blocked :: Expr -> Bool
  blocked expr =
    case expr of
      I n -> True
      B p -> True
      V x -> True
      Add (I _) (I _) -> False
      Add (I _) e -> blocked e
      Add e1 e2 -> blocked e1
      Mul (I _) (I _) -> False
      Mul (I _) e -> blocked e
      Mul e1 e2 -> blocked e1
      Succ (I _) -> False
      Succ e -> blocked e
      Pred (I 0) -> False
      Pred (I n) -> False
      Pred e -> blocked e
      Not (B p) -> False
      Not e -> blocked e
      And (B p) (B q) -> False
      And (B p) e -> blocked e
      And e1 e2 -> blocked e1
      Or (B p) (B q) -> False
      Or (B p) e -> blocked e
      Or e1 e2 -> blocked e1
      Lt (I n) (I m) -> False
      Lt (I n) e -> blocked e
      Lt e1 e2 -> blocked e1
      Gt (I n) (I m) -> False
      Gt (I n) e -> blocked e
      Gt e1 e2 -> blocked e1
      Eq (I n) (I m) -> False
      Eq (I n) e -> blocked e
      Eq e1 e2 -> blocked e1
      If (B q) e1 e2 -> False
      If e1 e2 e3 -> blocked e1
      Let i e1 e2 -> False
      Fn i e1 -> blocked e1
      Fix _ _ ->False
      App e1 e2 -> 
        if blocked e1
          then 
            if blocked e2
              then case e1 of
                Fn _ _ -> False
                _ -> True
              else False
          else False

  evals :: Expr -> Expr
  evals expr =
    if blocked expr
      then expr
      else evals (eval1 expr)

  evale :: Expr -> Expr
  evale ex =
    case evals ex of
      I n -> I n
      B p -> B p
      V x -> error "[Var] Unasigned variable"
      Add _ _ -> error "[Add] Expected two Integer"
      Mul _ _ -> error "[Mul] Expected two Integer"
      Succ _ -> error "[Succ] Expected one Integer"
      Pred _ -> error "[Pred] Expected one Integer"
      Not _ -> error "[Not] Expected one Boolean"
      And _ _ -> error "[And] Expected two Boolean"
      Or _ _ -> error "[Or] Expected two Boolean"
      Lt _ _ -> error "[Lt] Expected two Integer"
      Gt _ _ -> error "[Gt] Expected two Integer"
      Eq _ _ -> error "[Eq] Expected two Integer"
      If _ _ _ -> error "[If] Expected one Boolean as first argument"
      Let _ _ _ -> error "[Let] Expected one value as variable asigment"
      Fn _ _ -> error "[Fn] Expected argument"
      App _ _ -> error "[App] Expected function as first argument"

  eval :: Expr -> Type.Type -> Expr
  eval e t =
    let (ctx, t') = Static.infer e
      in
        if ctx /= [] then error ("Expression with unbounded variables: " ++ (show ctx))
        else 
          if (t /= t')
            then error ("Type error: " ++ (show t) ++ " is not " ++ (show t'))
            else evale e 