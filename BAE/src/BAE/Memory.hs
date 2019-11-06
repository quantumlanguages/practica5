{--
Practica 5
El lenguaje MiniC (MiniHS con efectos). Memoria
Autores:
Edgar Quiroz Castañeda
Sandra del Mar Soto Corderi
--}

module BAE.Memory where

    import BAE.Sintax

    -- | Definiendo tipos y alias para trabajar con memoria
    type Address = Int
    type Value = Expr
    type Cell = (Address, Value)
    type Memory = [Cell]

    -- Genera una uneva dirección de memoria que no esté en la lista
    newAddress :: Memory -> Expr
    newAddress m = newAddresAux m 0

    -- Revisa si una dirección de memoria está en una lista. Si es el caso, 
    -- incrementa la dirección y sigue buscando
    newAddressAux :: Memory -> Address -> Expr
    newAddressAux [] i = L i
    newAddresAux m@((l, v):ms) i = 
        if i == l
            then newAddresAux m (i+1)
            else newAddresAux ms i

    -- Devuelve el valor guardado en memoria
    access :: Address -> Memory -> Maybe Value
    access i m = 
        if checkMemory m
            then accessUnsafe i m
            else error "Corrupted memory"

    -- Devuelve el valor guardado sin revisar la integridad de la memoria
    accessUnsafe :: Address -> Memory -> Maybe Value
    accessUnsafe _ [] = Nothing
    accessUnsasfe i ((l, v):ms) =
        if i == l
            then Just v
            else access i ms

    -- Revisa si la memoria es válida
    checkMemory :: Memory -> Bool
    checkMemory [] = True
    checkMemory ((l, v):ms) = 
        case (filter (\(m, u) -> m == l) ms) of
            [] -> checkMemory ms
            _ -> False

    -- Actualiza el valor de una celda
    update :: Cell -> Memory -> Maybe Memory
    update (i, k) m = 
        if blocked k
            then if checkMemory m
                then updateUnsafe (i, k) [] m
                else error "Corrupted memory"
            else error "Memory can only store values"

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
        

    -- Actualiza el valor de una celda sin revisar la integridad de la memoria
    -- no de los valores a guardar
    updateUnsafe :: Cell -> Memory -> Memory -> Maybe Memory
    updateUnsafe _ _ [] = Nothing
    updateUnsafe n@(i, k) acc (h@(l, v):ms) = 
        if i == l
            then Just (acc ++ (n:ms))
            else updateUnsafe n (acc ++ [h]) ms
        
        