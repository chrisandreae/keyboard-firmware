module BlockIRSpec (spec) where

import ErrorState
import BlockIR

import Test.Hspec

import Data.Graph.Inductive.Graph

spec :: Spec
spec = do
    describe "Building a basic block" $  do
      let (Right resultState) =
            execThrowsState emptyState $ do
              block <- newBasicBlock
              commitBasicBlock [] block
          g = blockGraph resultState
      it "should have a single node" $ do
        length (nodes g) `shouldBe` 1
      it "should have a single node after cleaning" $ do
        length (nodes . cleanGraph $ g) `shouldBe` 1

