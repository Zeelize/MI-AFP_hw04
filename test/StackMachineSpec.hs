{-# LANGUAGE OverloadedStrings #-}

module StackMachineSpec (spec) where

import Test.Hspec
import Control.Exception

import StackMachine

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Stack as Stack

import Control.Program
import qualified Fixtures.Programs as Progs

spec :: Spec
spec = do
  describe "runProgram (input/output and constants)" $ do
    it "runs trivial 'empty' program" $
      runProgram EOP Seq.empty `shouldBe` Seq.empty
    it "runs WR to put on output from top of stack" $ do
      runProgram (TV 7 $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 7
      runProgram (TV 7 $. TV 10 $. WR $. WR $. EOP) Seq.empty `shouldBe` Seq.fromList [10, 7]
      runProgram (RD $. TV 10 $. WR $. WR $. EOP) (Seq.singleton 7) `shouldBe` Seq.fromList [10, 7]
      runProgram (RD $. RD $. WR $. WR $. EOP) (Seq.fromList [1, 2, 3]) `shouldBe` Seq.fromList [2, 1]
    it "fails with WR with no value on top of stack" $ do
      evaluate (runProgram (WR $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TA 7 $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TV 10 $. TA 7 $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
    it "fails if RD with empty input" $
      evaluate (runProgram (RD $. EOP) Seq.empty) `shouldThrow` errorCall "No input"

  describe "runProgram (math)" $ do
    it "runs AD to add two numbers at the top of stack" $ do
      runProgram (TV 3 $. TV 5 $. AD $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 8
      runProgram (TV 3 $. TV (-5) $. AD $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton (-2)
      evaluate (runProgram (TV 10 $. TA 7 $. AD $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TA 10 $. TV 7 $. AD $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (AD $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TV 5 $. AD $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
    it "runs SB to subtract two numbers at the top of stack" $ do
      runProgram (TV 3 $. TV 5 $. SB $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 2
      runProgram (TV 6 $. TV 2 $. SB $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton (-4)
      evaluate (runProgram (TV 10 $. TA 7 $. SB $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TA 10 $. TV 7 $. SB $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (SB $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TV 5 $. SB $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
    it "runs MT to multiply two numbers at the top of stack" $ do
      runProgram (TV 3 $. TV 5 $. MT $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 15
      runProgram (TV (-3) $. TV 5 $. MT $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton (-15)
      evaluate (runProgram (TV 10 $. TA 7 $. MT $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TA 10 $. TV 7 $. MT $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (MT $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TV 5 $. MT $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
    it "runs DI to divides two numbers at the top of stack" $ do
      runProgram (TV 5 $. TV 15 $. DI $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 3
      runProgram (TV 3 $. TV 2 $. DI $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 0
      runProgram (TV 2 $. TV (-8) $. DI $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton (-4)
      runProgram (TV 2 $. TV 0 $. DI $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 0
      evaluate (runProgram (TV 0 $. TV 3 $. DI $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Division by 0"
      evaluate (runProgram (TV 10 $. TA 7 $. DI $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TA 10 $. TV 7 $. DI $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (DI $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TV 5 $. DI $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"

  describe "runProgram (memory)" $ do
    it "stores values to the memory" $ do
      runProgram (TA 127 $. TV 5 $. ST $. TA 127 $. DR $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 5
      runProgram (TA 0 $. TV 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Seq.singleton 1
      runProgram (TA 0 $. TV 1 $. ST $. TV 10 $. TA 0 $. DR $. WR $. WR $. EOP) Seq.empty `shouldBe` Seq.fromList [1, 10]
    it "fails with incorrect address/value" $ do
      evaluate (runProgram (TV 0 $. TV 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Not address"
      evaluate (runProgram (TV 0 $. TA 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TA 0 $. TA 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TA 0 $. TV 1 $. ST $. TV 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Not address"
    it "fails with uninitialized memory cell" $ do
      evaluate (runProgram (TA 0 $. TV 1 $. ST $. TA 1 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Uninitialized memory"
      evaluate (runProgram (TA 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Uninitialized memory"
    it "fails with not enough entries on stack" $ do
      evaluate (runProgram (ST $. TA 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TV 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"
      evaluate (runProgram (TA 0 $. TV 1 $. ST $. DR $. WR $. EOP) Seq.empty) `shouldThrow` errorCall "Empty stack"


  describe "runProgram (jumps)" $ do
    it "jumps unconditionally (JU) to label" $ do
      runProgram (TV 4 $. JU "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Seq.singleton 4
      runProgram (RD $. JU "end" $. "faulty" $: RD $. WR $. AD $. "end" $: WR $. EOP) (Seq.singleton 10) `shouldBe` Seq.singleton 10
    it "jumps conditionally (JZ) to label if zero at the top of stack" $ do
      runProgram (TV 4 $. TV 0 $. JZ "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Seq.singleton 4
      runProgram (TV 4 $. TV 1 $. JZ "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Seq.singleton 5
      runProgram (TV 4 $. TV (-1) $. JZ "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Seq.singleton 5
    it "fails if not value at the top of stack (JZ)" $ do
      evaluate (runProgram (TA 0 $. JZ "end" $. WR $. "end" $: EOP) Seq.empty) `shouldThrow` errorCall "Not value"
      evaluate (runProgram (TV 0 $. TA 10 $. JZ "end" $. WR $. "end" $: EOP) Seq.empty) `shouldThrow` errorCall "Not value"
    it "fails if label not found" $ do
      evaluate (runProgram (TV 0 $. JZ "nope" $. WR $. "end" $: EOP) Seq.empty) `shouldThrow` errorCall "Unknown label"
      evaluate (runProgram (JU "abc" $. WR $. "end" $: EOP) Seq.empty) `shouldThrow` errorCall "Unknown label"

  describe "runProgram (complex programs)" $ do
    it "can sum list of numbers in cycle (first is input length)" $ do
      runProgram Progs.sumList (Seq.fromList (10:[1..10])) `shouldBe` Seq.singleton 55
      runProgram Progs.sumList (Seq.fromList (11:[-5..5])) `shouldBe` Seq.singleton 0
      runProgram Progs.sumList (Seq.fromList (5:[1..100])) `shouldBe` Seq.singleton 15
      runProgram Progs.sumList (Seq.fromList (2:[-6..0])) `shouldBe` Seq.singleton (-11)
      runProgram Progs.sumList (Seq.singleton 0) `shouldBe` Seq.singleton 0
      evaluate (runProgram Progs.sumList (Seq.singleton 10)) `shouldThrow` errorCall "No input"
    it "can compute GCD based on euclidean algorithm" $ do
      runProgram Progs.gcd (Seq.fromList [18, 12]) `shouldBe` Seq.singleton 6
      runProgram Progs.gcd (Seq.fromList [15, 11]) `shouldBe` Seq.singleton 1
      runProgram Progs.gcd (Seq.fromList [0, 1]) `shouldBe` Seq.singleton 1
      runProgram Progs.gcd (Seq.fromList [10, 10]) `shouldBe` Seq.singleton 10
      runProgram Progs.gcd (Seq.fromList [2154, 126]) `shouldBe` Seq.singleton 6
      runProgram Progs.gcd (Seq.fromList [12547505, 1654235]) `shouldBe` Seq.singleton 95
    it "can compute fibonacci number" $ do
      runProgram Progs.fib (Seq.singleton 0) `shouldBe` Seq.singleton 0
      runProgram Progs.fib (Seq.singleton 1) `shouldBe` Seq.singleton 1
      runProgram Progs.fib (Seq.singleton 10) `shouldBe` Seq.singleton 55
      runProgram Progs.fib (Seq.singleton 17) `shouldBe` Seq.singleton 1597
    it "can compute fibonacci sequence" $ do
      runProgram Progs.fibSeq (Seq.singleton 1) `shouldBe` Seq.fromList [0, 1]
      runProgram Progs.fibSeq (Seq.singleton 7) `shouldBe` Seq.fromList [0, 1, 1, 2, 3, 5, 8, 13]
      runProgram Progs.fibSeq (Seq.singleton 10) `shouldBe` Seq.fromList [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
