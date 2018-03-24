module StackMachine where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Stack as Stack

import qualified Data.ByteString.Char8 as C

import Control.Program

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = Seq.Seq Value
-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = Seq.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = Map.Map Address Value
-- | Lookup directory of subprograms (labels are marking starts of parts of a program where you can jump with JU or JZ)
type SubprogramDir = Map.Map Label Program
-- | Computer stack can store addresses and values
type ComputerStack = Stack.Stack (Either Address Value)


-- | Run program with given input (memory and stack should be empty at start)
-- | If there is a problem, error is raised ("Empty stack", "Not value", "Not address", "No input", "Unknown label", "Division by 0", "Uninitialized memory"), see tests
-- TODO: implement running the program
runProgram :: Program -> Input -> Output
runProgram prg inp = programRunner Stack.empty Map.empty Map.empty prg inp Seq.empty

-- Feel free to create more helper functions
programRunner :: ComputerStack -> Memory -> SubprogramDir -> Program -> Input -> Output -> Output
programRunner s m d (EOP) inp outp = outp 
programRunner s m d ((TA add) `Then` prg) inp outp = programRunner (Stack.push (Left add) s) m d prg inp outp
programRunner s m d ((TV val) `Then` prg) inp outp = programRunner (Stack.push (Right val) s) m d prg inp outp

programRunner s m d (DR `Then` prg) inp outp = case topThatAddress s of
    Left err -> err
    Right add -> case takeValueFromMemory add m of
        Left err -> err
        Right val -> programRunner (Stack.push (Right val) (Stack.pop s)) m d prg inp outp
    where
        takeValueFromMemory :: Address -> Memory -> Either Output Value
        takeValueFromMemory a mem = case mem Map.!? a of
            Nothing -> Left (error "Uninitialized memory")
            Just v -> Right v 

programRunner s m d (ST `Then` prg) inp outp = case topThatValue s of
    Left err -> err
    Right val -> case topThatAddress (Stack.pop s) of
        Left err -> err
        Right add -> programRunner (Stack.pop (Stack.pop s)) (Map.insert add val m) d prg inp outp

programRunner s m d (WR `Then` prg) inp outp = programRunner (Stack.pop s) m d prg inp noutp
    where 
        noutp = case topThatValue s of 
            Left err -> err
            Right val -> outp Seq.|> val 

programRunner s m d (RD `Then` prg) inp outp 
    | inp == Seq.empty = error "No input"
    | otherwise = programRunner (Stack.push (Right nv) s) m d prg (Seq.drop 1 inp) outp
        where
            nv = Seq.index inp 0

programRunner s m d (AD `Then` prg) inp outp = case mathCalculator s 0 of
    Left err -> err
    Right ns -> programRunner ns m d prg inp outp

programRunner s m d (SB `Then` prg) inp outp = case mathCalculator s 1 of
    Left err -> err
    Right ns -> programRunner ns m d prg inp outp

programRunner s m d (MT `Then` prg) inp outp = case mathCalculator s 2 of
    Left err -> err
    Right ns -> programRunner ns m d prg inp outp

programRunner s m d (DI `Then` prg) inp outp = case mathCalculator s 3 of
    Left err -> err
    Right ns -> programRunner ns m d prg inp outp

programRunner s m d ((JU lbl) `Then` prg) inp outp = programRunner s m nd nprg inp outp
    where 
        nd = findMarking d lbl prg
        nprg = case nd Map.!? lbl of
            Nothing -> error "Unknown label"
            Just p -> p

programRunner s m d ((JZ lbl) `Then` prg) inp outp = case topThatValue s of
    Left err -> err
    Right val -> case val == 0 of
        True -> programRunner ns m d nprg inp outp
        False -> programRunner ns m d prg inp outp
    where 
        ns = Stack.pop s
        nprg =  (JU lbl) `Then` prg

programRunner s m d (lbl `Marks` prg) inp outp = programRunner s m nd prg inp outp
    where
        nd = Map.insert lbl prg d

-- Another helper functions
topThatValue :: ComputerStack -> Either Output Value
topThatValue s = case (Stack.topSafe s) of
                    Nothing -> Left (error "Empty stack")
                    Just (Left _) -> Left (error "Not value" )
                    Just (Right val) -> Right val 

topThatAddress :: ComputerStack -> Either Output Address
topThatAddress s = case (Stack.topSafe s) of
                    Nothing -> Left (error "Empty stack")
                    Just (Left add) -> Right add 
                    Just (Right _) -> Left (error "Not address")  
                    
mathCalculator :: ComputerStack -> Int -> Either Output ComputerStack
mathCalculator s flag = case topThatValue s of
                    Left err -> Left err
                    Right val1 -> case topThatValue (Stack.pop s) of
                        Left err -> Left err
                        Right val2 -> case mathSolver val1 val2 flag of
                            Left err -> Left err
                            Right val -> Right (Stack.push (Right val) (Stack.pop (Stack.pop s)))
                        where
                            mathSolver :: Value -> Value -> Int -> Either Output Value
                            mathSolver _ 0 3 = Left (error "Division by 0")
                            mathSolver a b 0 = Right (a + b)  
                            mathSolver a b 1 = Right (a - b)  
                            mathSolver a b 2 = Right (a * b)  
                            mathSolver a b 3 = Right (div a b)  

findMarking :: SubprogramDir -> Label -> Program -> SubprogramDir
findMarking d l EOP = d
findMarking d l (_ `Then` prg) = findMarking d l prg
findMarking d l (lbl `Marks` prg) 
    | (Map.member l d) = d
    | l == lbl = Map.insert lbl prg d
    | otherwise = findMarking nextD l prg
        where 
            nextD = Map.insert lbl prg d 
