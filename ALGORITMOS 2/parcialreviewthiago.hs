module Parcial where
    data EU = Emp | Str Int Bool EU                                                         deriving Show

    instance (Eq EU) where
        (==) Emp Emp = True
        (==) (Str i0 p0 _) (Str i1 p1 _) = i0 == i1 && p0 == p1
        (==)  _ _ = False

    idEU::EU->Int
    priorityEU::EU->Bool
    emptyEU::EU
    isemptyEU::EU->Bool
    insertEU::Int->Bool->EU->EU
    quickEU::EU->Bool
    firstEU::EU->EU
    deleteEU::EU->EU
    checkfastEU::EU->EU

    idEU Emp = 0
    idEU (Str i _ _) = i

    priorityEU Emp = False
    priorityEU (Str _ p _) = p

    emptyEU = Emp

    isemptyEU Emp = True
    isempty _ = False

    insertEU id pri Emp = Str id pri Emp
    insertEU id pri (Str i b sig) = Str id pri (Str i b sig)

    quickEU Emp = False
    quickEU (Str _ True _) = True
    quickEU _ = False

    firstEU Emp = Emp
    firstEU eu@(Str id pri Emp) = eu
    firstEU eu@(Str id pri sig) = if fast == Emp then Str id pri Emp else Str (idEU fast) (priorityEU fast) Emp
        where
            fast = checkfastEU eu

    deleteEU Emp = Emp
    deleteEU eu@(Str id pri Emp) = Emp
    deleteEU eu@(Str id pri sig) = if fast == Emp then sig else deletefast fast eu 
        where
            fast = checkfastEU eu

    checkfastEU Emp = Emp
    checkfastEU (Str id pri sig) = case pri of
                                            True -> Str id pri Emp
                                            False -> checkfastEU sig

    deletefast e u@(Str id pri sig) = if e == u then sig else (Str id pri (deletefast e sig))