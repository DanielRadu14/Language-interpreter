module Parser
where

import Util
import Data.List (sort)
import Data.Maybe
import InferenceDataType
import ClassState

--Program va fi definit ca o lista de elemente de tipul (clasa, nume_clasa, parinte_clasa)
data Program = Program [(ClassState,String, String)] deriving Show

--la initializarea unui program va exista automat clasa "Global"
initEmptyProgram :: Program
initEmptyProgram = (Program [(initEmptyClass, "Global", "Global")])

--intoarce ClassState-ul din tuplu
getClassState :: (ClassState, String, String) -> ClassState
getClassState (c, _, _) = c

--intoarce numele clasei din tuplu
getName :: (ClassState, String, String) -> String
getName (_,n,_) = n

--intoarce numele parintelui din tuplu
getParent :: (ClassState, String, String) -> String
getParent (_,_,p) = p

getVars :: Program -> [[String]]
getVars (Program []) = []
getVars (Program (x:xs)) = (getValues (getClassState x) Var) ++ (getVars (Program xs))

getClasses :: Program -> [String]
getClasses (Program []) = []
getClasses (Program (x:xs)) = sort ([getName x] ++ (getClasses (Program xs)))

getParentClass :: String -> Program -> String
getParentClass clasa (Program []) = []
getParentClass clasa (Program (x:xs))
                               | getName x == clasa = getParent x
                               | otherwise = getParentClass clasa (Program xs)

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass clasa (Program []) = []
getFuncsForClass clasa (Program (x:xs))
                               | getName x == clasa = getValues (getClassState x) Func
                               | otherwise = getFuncsForClass clasa (Program xs)

-- Instruction va fi o lista de String
data Instruction = Instruction [String] deriving Show

--prelucreaza fiecare linie intr-o lista de cuvinte
word :: String -> String -> [String]
word [] [] = []
word linie [] = filter (\x -> x /= "") [(foldl (flip(:)) [] linie)]
word linie (x:xs)
                 | x == ' ' || x == ':' || x == ',' || x == '(' || x == ')' || x == '=' = filter (\x -> x /= "") (((foldl (flip(:)) [] linie):(word [] xs)))
                 | otherwise = (word (x:linie) xs)
 

parse :: String -> [Instruction]
parse str = let parseAux [] = []
                parseAux (x:xs) = ((Instruction x):(parseAux xs))
            in parseAux (map (\x -> word [] x) (filter (\x -> x /= "") (lines str)))

--intoarce clasa ceruta din programul dat
getClass :: String -> Program -> ClassState
getClass clasa (Program (x:xs))
                       | getName x == clasa = getClassState x
                       | otherwise = getClass clasa (Program xs)


--adauga variabila cu nume si tip in programul dat in clasa "Global"
--am certitudinea ca exista clasa "Global"
addVar :: String -> String -> Program -> [(ClassState, String, String)] -> [(ClassState, String, String)]
addVar nume tip (Program (x:xs)) acc
                              | getName x == "Global" = [((insertIntoClass (getClassState x) Var [nume, tip]), "Global", "Global")]++xs++acc
                              | otherwise = addVar nume tip (Program xs) (x:acc)

--adauga functia in program, in clasa respectiva
--daca nu este gasita clasa, programul ramane neschimbat
addFunc :: String -> [String] -> Program -> [(ClassState, String, String)] -> [(ClassState, String, String)]
addFunc clasa functie (Program []) acc = acc
addFunc clasa functie (Program (x:xs)) acc
                              | getName x == clasa = [((insertIntoClass (getClassState x) Func functie), clasa, (getParent x))]++xs++acc
                              | otherwise = addFunc clasa functie (Program xs) (x:acc)


--verifica daca exista clasa cu numele dat in program
classExists :: Program -> String -> Bool
classExists (Program []) nume = False
classExists (Program (x:xs)) nume
                             | getName x == nume = True
                             | otherwise = classExists (Program xs) nume

--intoarce lista de String continuta in Instruction
getInstr :: Instruction -> [String]
getInstr (Instruction instr) = instr

--intoarce numele clasei ce trebuie adaugata
getClassName :: Instruction -> String
getClassName (Instruction instr) = (head (tail instr))

--intoarce numele clasei pe care trebuie sa o extinda clasa creata
getExtends :: Instruction -> String
getExtends (Instruction instr) = head (tail (tail (tail instr)))

--intoarce numele variabilei ce trebuie adaugata
getVarName :: Instruction -> String
getVarName (Instruction instr) = (head (tail instr))

--intoarce tipul variabilei ce trebuie adaugata
getVarType :: Instruction -> String
getVarType (Instruction instr) = (head (tail (tail instr)))

--intoarce tipul functiei ce trebuie adaugata
getFuncClass :: Instruction -> String
getFuncClass (Instruction instr) = head (tail instr)

--creeaza lista de string-uri pe care o voi retine ca functie
createFunc :: Instruction -> [String]
createFunc (Instruction instr) = [(head (tail (tail instr)))]++[head instr]++(tail (tail (tail instr)))

--verifica daca tipul returnat de functie si parametrii acesteia sunt valizi
verificaFunc :: [String] -> Program -> Bool
verificaFunc [] p = True
verificaFunc (x:xs) p
              | classExists p x == True = verificaFunc xs p
              | otherwise = False

interpret :: Instruction -> Program -> Program
interpret instr (Program p)
                     --adaug variabila noua in program daca tipul acesteia este valid
                     | head (getInstr instr) == "newvar" && classExists (Program p) (getVarType instr) = Program (addVar (getVarName instr) (getVarType instr) (Program p) [])

                     --daca tipul variabilei de adaugat nu este valid, program ramane neschimbat
                     | head (getInstr instr) == "newvar" && classExists (Program p) (getVarType instr) == False = Program p

                     --adaug clasa noua daca aceasta nu exista deja in program si daca parintele acesteia exista
                     | head (getInstr instr) == "class" && length (getInstr instr) == 4 && classExists (Program p) (getExtends instr) && classExists (Program p) (getClassName instr) == False = Program ((initEmptyClass, getClassName instr, getExtends instr):p)

                     --adaug clasa noua daca aceasta nu exista in program, avand parintele "Global" daca acesta nu e specificat
                     | head (getInstr instr) == "class"  && classExists (Program p) (head (tail (getInstr instr))) == False = Program ((initEmptyClass, head (tail (getInstr instr)), "Global"):p)

                     --daca parametrii functiei sunt valizi, adaug functia in program
                     | verificaFunc (tail (createFunc instr)) (Program p) = Program ((addFunc (getFuncClass instr) (createFunc instr)) (Program p) [])
                     --orice alt caz presupune ca programul sa ramana neschimbat
                     | otherwise = Program p


--verifica daca variabila data este continuta in program
verificaVar :: String -> [[String]]-> Bool
verificaVar var [] = False
verificaVar var (x:xs)
                | var == head x = True
                | otherwise = verificaVar var xs

--intoarce tipul variabilei var din program
returnType :: String -> [[String]]-> String
returnType var (x:xs)
                | var == head x = head (tail x)
                | otherwise = returnType var xs 

infer :: Expr -> Program -> Maybe String
infer (Va var) prog
                    | verificaVar var (getVars prog) = Just (returnType var (getVars prog))
                    | otherwise = Nothing
infer (FCall s1 s2 l) prog = Nothing
