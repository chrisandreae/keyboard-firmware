module OutputSpec (spec) where

import BasicTypes
import Errors
import Parser
import TypedAST
import BlockIR
import Output

import Control.Monad

import Test.Hspec

sourceToIR :: String -> Either CompileError (IRProgram IRInstruction)
sourceToIR = (parseProgram >=> buildTProgram >=> optTProgram >=> buildIR)

spec :: Spec
spec = do
    describe "Emitting a basic program" $  do
      let (Right ir) = sourceToIR "void main(){buzzAt(0,0);}"
          (Right (BytecodeProgram _ [m])) = outputProgram ir
          (BytecodeMethod _ _ codes) = m
      it "should emit something" $ do
        codes `shouldBe` [SCONST_n 0, BCONST_n 0, SYSCALL BuzzAt, RET Void]
