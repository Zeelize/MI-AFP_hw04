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
--runProgram prg inp = programRunner Stack.empty Map.empty Map.empty prg inp

runProgram (EOP) input = input 
runProgram ((TA add) `Then` prg) input = runProgram prg (stackToInput (pushToStack (Left add)) input)
runProgram ((TV val) `Then` prg) input = runProgram prg (stackToInput (pushToStack (Right val)) input)
runProgram (DR `Then` prg) input = undefined
runProgram (ST `Then` prg) input = undefined
runProgram (WR `Then` prg) input = undefined
runProgram (RD `Then` prg) input = undefined
runProgram (AD `Then` prg) input = undefined
runProgram (SB `Then` prg) input = undefined
runProgram (MT `Then` prg) input = undefined
runProgram (DI `Then` prg) input = undefined
runProgram ((JU lbl) `Then` prg) input = undefined
runProgram ((JZ lbl) `Then` prg) input = undefined
runProgram (lbl `Marks` prg) input = undefined


-- Feel free to create more helper functions
--programRunner :: ComputerStack -> Memory -> SubprogramDir -> Program -> Input -> Output

stackToInput :: ComputerStack -> Input -> Output
stackToInput _ input = input 

pushToStack :: (Either Address Value) -> ComputerStack
pushToStack a = Stack.push a Stack.empty


