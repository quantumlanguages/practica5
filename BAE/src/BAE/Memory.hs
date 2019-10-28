{--
Practica 5
El lenguaje MiniC (MiniHS con efectos). Memoria
Autores:
Edgar Quiroz Castañeda
Sandra del Mar Soto Corderi
--}

module BAE.Memory where

    import BAE.Sintax
    import qualified BAE.Dynamic as Dynamic

    -- | Definiendo tipos y alias para trabajar con memoria
    type Address = Int
    type Value = Expr
    type Cell = (Address, Value)
    type Memory = [Cell]

    -- Genera una uneva dirección de memoria queno esté en la lista
    newAddress :: Memory -> Expr
    newAddress m e = newAddresAux m 0

    -- Revisa si una dirección de memoria está en una lista. Si es el caso, 
    -- incrementa la dirección y sigue buscando
    newAddressAux :: Memory -> Addres -> Expr
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
    accessUnsafe :: Address -> Memory Maybe Value
    accessUnsafe _ [] = Nothing
    accessUnsasfe i ((l, v):ms) =
        if i == l
            then Just v
            else access i ms

    -- Revisa si la memoria es válida
    checkMemory :: Memory -> Boolean
    checkMemory [] = True
    checkMemory ((l, v):ms) = 
        case (filter (\(m, u) -> m == l) ms) of
            [] -> checkMemory ms
            _ -> False

    -- Actualiza el valor de una celda
    update :: Cell -> Memory -> Maybe Memory
    update (i, k) m = 
        if Dynamic.blocked k
            then if checkMemory m
                then updateUnsafe (i, k) [] m
                else error "Corrupted memory"
            else error "Memory cn only store values"
        

    -- Actualiza el valor de una celda sin revisar la integridad de la memoria
    -- no de los valores a guardar
    updateUnsafe :: Cell -> Memory -> Memory -> Maybe Memory
    updateUnsafe _ _ [] = Nothing
    updateUnsafe n@(i, k) acc (h@(l, v):ms) = 
        if i == l
            then Just (acc ++ (n:ms))
            else updateUnsafe n (acc ++ [h]) ms
        
        