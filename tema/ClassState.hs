module ClassState
where

import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq, Ord)

--am definit ClassState ca fiind o mapare de la cheia de tip InstrType (variabila sau functie) la o lista de lista de string-uri
--pentru a putea concatena functii care au aceeasi denumire, dar alti parametri
type ClassState = Map.Map InstrType [[String]]

initEmptyClass :: ClassState
initEmptyClass = Map.empty

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass a b c = Map.insertWith (++) b [c] a

getValues :: ClassState -> InstrType -> [[String]]
getValues map b = Map.foldrWithKey (\k a l -> if k == b then a++l else l) [] map
