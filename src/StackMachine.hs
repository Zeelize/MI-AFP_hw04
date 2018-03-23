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
programRunner s m d (DR `Then` prg) inp outp = undefined
programRunner s m d (ST `Then` prg) inp outp = undefined
programRunner s m d (WR `Then` prg) inp outp = programRunner (Stack.pop s) m d prg inp noutp
    where 
        noutp = case (Stack.top s) of
            (Left _) -> error "Not value"
            (Right val) -> outp Seq.|> val 

programRunner s m d (RD `Then` prg) inp outp 
    | inp == Seq.empty = error "No input"
    | otherwise = programRunner (Stack.push (Right nv) s) m d prg (Seq.drop 1 inp) outp
        where
            nv = case Seq.null inp of
                True -> error "No input"
                False -> Seq.index inp 0

programRunner s m d (AD `Then` prg) inp outp = undefined
programRunner s m d (SB `Then` prg) inp outp = undefined
programRunner s m d (MT `Then` prg) inp outp = undefined
programRunner s m d (DI `Then` prg) inp outp = undefined
programRunner s m d ((JU lbl) `Then` prg) inp outp = undefined
programRunner s m d ((JZ lbl) `Then` prg) inp outp = undefined
programRunner s m d (lbl `Marks` prg) inp outp = undefined
