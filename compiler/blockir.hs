{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module BlockIR where

import BasicTypes
import Parser
import Errors
import ErrorState
import Indexes
import TypedAST

import Text.Printf

import Data.Maybe
import Data.List
import Data.Int

import Control.Monad
import Control.Monad.Error

import Control.Monad.State(State)
import qualified Control.Monad.State as State

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree -- implementation: provides Gr

-- Blocked IR

data IRProgram a = IRProgram (Index (IRMethod a)) (Index IRVariable)

data IRMethod a = IRMethod MethodID [Type] (IRGraph a) (Index IRVariable) -- arg types

data IRBlock a = IRBlock [a]

-- Edges contain the results of that operation.
-- We always have exactly one default edge, and zero or one conditional edge (or when we add switch, n value edges)
data IREdge = ConditionalEdge CondOp
            | DefaultEdge
            -- | ValueEdge Int
            deriving (Show, Eq, Ord) -- order is used in outputting: default must be last


type IRGraph a = Gr (IRBlock a) IREdge

type IRVariable = TVariable

data IRInstruction = IRStore VariableID
                   | IRLoad VariableID
                   | IRGStore VariableID
                   | IRGLoad VariableID
                   | IRBConst Int8
                   | IRSConst Int16
                   | IRDup Type
                   | IRPop Type
                   | IRSwap Type
                   | IRArith Type ArithOp
                   | IRS2B
                   | IRB2S
                   | IRCall MethodID
                   | IRReturn Type
                   | IRExit
                   | IRSyscall SyscallOp
                   | IRDummy String
                 deriving (Show, Eq)

data ArithOp = IRAdd
             | IRSubtract
             | IRMultiply
             | IRDivide
             | IRMod
             | IRAnd
             | IROr
             | IRXor
             | IRComplement
             | IRCmp
             | IRLshift
             | IRRshift
             deriving (Show, Eq)

data CondOp = IfEq | IfNe | IfLt | IfGt | IfLe | IfGe
            deriving (Show, Eq, Ord)

instance Show a => Show (IRProgram a) where
  show (IRProgram meths vars) = "Methods:\n" ++ (show meths) ++ "Global Variables:\n"  ++ (show vars)

instance Show a => Show (IRMethod a) where
  show (IRMethod id types blocks locals) = printf "=== method%d(%s) ===\nBlocks:\n%s\nLocals:\n%s\n" id (showlistwithsep ", " types) (showIRGraph blocks) (show locals)

instance Show a => Show (IRBlock a) where
  show (IRBlock insns) = unlines $ map show insns

instance Show a => Show (IRGraph a) where
  show gr = showIRGraph gr

showIRGraph :: Show a => IRGraph a -> String
showIRGraph gr = unlines $ map (printNode gr) (nodes gr)
  where
    printNode :: IRGraph a -> Node -> String
    printNode g n = printf "= [%s] -> Block %d -> [%s] ==\n%s"
                     (unwords $ map show $ pre g n)
                     n
                     (printOutEdges $ sort $ lsuc g n)
                     (show.fromJust $ lab gr n)
    printOutEdges = unwords . (map printOutEdge)
    printOutEdge (n, DefaultEdge) = printf "T->%d" n
    printOutEdge (n, ConditionalEdge op) = printf "%s->%d" (show op) n
-----

data BlockingState = BlockingState { blockGraph       :: IRGraph IRInstruction,
                                     continueBlockId  :: Node,
                                     breakBlockId     :: Node
                                   };


type ThrowsBlockingState = ThrowsState CompileError BlockingState

noBlock :: Node
noBlock = -1

buildIR :: TProgram -> ThrowsError (IRProgram IRInstruction)
buildIR (TProgram tmethIdx gVarIdx _ _) = do
  irMeths <- mapM buildIRMethod $ indexElems tmethIdx
  let irMethIdx = foldl (\idx m@(IRMethod id _ _ _) -> indexInsert id m idx) newIndex irMeths
  return $ IRProgram irMethIdx gVarIdx

buildIRMethod :: TMethod -> ThrowsError (IRMethod IRInstruction)
buildIRMethod (TMethod mId rType aTypes varIdx _ tStats) = do
  let inputState = BlockingState { blockGraph      = empty,
                                   continueBlockId = noBlock,
                                   breakBlockId    = noBlock }
  resultState <- execThrowsState inputState $ do { accum <- newBasicBlock; foldM (flip buildIRStatement) accum tStats >>= commitBasicBlock [] }
  let blockGraph' = cleanGraph (blockGraph resultState)
  blockGraph'' <- checkReturns rType blockGraph'
  return $ IRMethod mId aTypes blockGraph'' varIdx
  where
    -- Remove (transitively) nodes with no in edges except for node 1
    cleanGraph :: IRGraph a -> IRGraph a
    cleanGraph g | deadCodeNodes g == [] = g
                 | otherwise             = cleanGraph $ delNodes (deadCodeNodes g) g
    deadCodeNodes :: IRGraph a -> [Node]
    deadCodeNodes graph = filter (\n -> n /= 1 && (null $ pre graph n)) (nodes graph)
    -- Checks that every block with no out edges has a valid return or
    -- exit and return the graph or error.  If the method has void
    -- return type, add return statements.
    checkReturns :: Type -> IRGraph IRInstruction -> ThrowsError (IRGraph IRInstruction)
    checkReturns Void gr = updateNodes (\(IRBlock is) -> IRBlock (is ++ [IRReturn Void])) gr (noReturnExitBlocks gr)
    checkReturns _ gr = if null $ noReturnExitBlocks gr
                        then return gr
                        else throwError (NoReturnError $ printf "Method %d missing return" mId)
    noReturnExitBlocks :: IRGraph IRInstruction -> [Node]
    noReturnExitBlocks gr = filter (\n -> (null $ suc gr n) && noReturnBlock (fromJust $ lab gr n)) (nodes gr)
      where
      noReturnBlock :: IRBlock IRInstruction -> Bool
      noReturnBlock (IRBlock []) = True
      noReturnBlock (IRBlock instrs) = case (last instrs) of
        IRExit     -> False
        IRReturn _ -> False
        _          -> True


-- IRInstructions is a reversed list of instructions (for easy appending), newtyped to prevent mixing with unreversed lists
data BlockAccum = BlockAccum Node [IRInstruction]

appendInstructions :: [IRInstruction] -> BlockAccum -> BlockAccum
appendInstructions is (BlockAccum id rest) = BlockAccum id ((reverse is) ++ rest)

appendInstructionsM :: Monad m => [IRInstruction] -> BlockAccum -> m BlockAccum
appendInstructionsM is acc = return $ appendInstructions is acc

-- helper methods

-- Open a new basic block, puts its skeleton in the index and returns
-- an accumulator for it
newBasicBlock :: ThrowsBlockingState BlockAccum
newBasicBlock = do
  -- add the new block to the state
  bGraph <- State.gets blockGraph
  let [newId] = newNodes 1 bGraph
      bGraph' = insNode (newId, (IRBlock [])) bGraph
  State.modify $ \state -> state { blockGraph = bGraph' }
  return (BlockAccum newId [])


matchThrows :: Graph g => Node -> g a b -> ThrowsError (GDecomp g a b)
matchThrows n g = case match n g of
  (Nothing, _)   -> throwError $ GraphLookupError n
  (Just ctx, g') -> return (ctx, g')

updateNode :: (IRBlock a -> IRBlock a) -> IRGraph a -> Node -> ThrowsError (IRGraph a)
updateNode fn gr node = do
  ((ctx_in, ctx_id, old, ctx_out), gr') <- matchThrows node gr
  let new = fn old
  return $ (ctx_in, ctx_id, new, ctx_out) & gr'

updateNodes :: (IRBlock a -> IRBlock a) -> IRGraph a -> [Node] -> ThrowsError (IRGraph a)
updateNodes fn gr ns = foldM (updateNode fn) gr ns

updateBlockContents :: BlockAccum -> IRGraph IRInstruction -> ThrowsBlockingState (IRGraph IRInstruction)
updateBlockContents (BlockAccum blockId revInstrs) bGraph = do
  -- look up (by decomposing) the given block in the index for its in-edges, check that it was uncommitted (no instructions)
  IRBlock oldInsns <- liftThrows $ maybeToError (InternalError "Block doesn't exist") $ lab bGraph blockId
  guardError (InternalError "Updating already committed block") (null oldInsns)
  let newBlock = IRBlock (reverse revInstrs)
  liftThrows $ updateNode (const newBlock) bGraph blockId

-- Links the block for the argument BlockAccum with the destination
-- IREdge, puts the accumulated instructions into it, saves it into
-- the index and returns its block ID
commitBasicBlock :: [(IREdge, Node)] -> BlockAccum -> ThrowsBlockingState ()
commitBasicBlock edges accum@(BlockAccum srcId _) = do
  -- Update the node in the graph
  bGraph' <- State.gets blockGraph >>= updateBlockContents accum
  let bGraph'' = foldl (\g (elabel, dstId) -> insEdge (srcId, dstId, elabel) g) bGraph' edges
  State.modify $ \s -> s { blockGraph = bGraph'' }

idOf :: BlockAccum -> Node
idOf (BlockAccum id _) = id

withLoopEnvironment :: Node -> Node -> ThrowsBlockingState a -> ThrowsBlockingState a
withLoopEnvironment contId breakId body = do
  -- save old env
  olds@BlockingState { continueBlockId = oldContId, breakBlockId = oldBreakId } <- State.get
  -- push in new env
  State.put olds { continueBlockId = contId, breakBlockId = breakId }
  -- run the body
  bodyret <- body
  -- restore
  State.modify $ \s -> s { continueBlockId = oldContId, breakBlockId = oldBreakId }
  return bodyret

--

buildIRStatement :: TStatement -> BlockAccum -> ThrowsBlockingState BlockAccum

buildIRStatement (TBlock _ stats) accum = do
  foldM (flip buildIRStatement) accum stats

buildIRStatement (TIfStatement expr thenBody elseBody) inAccum = do
  -- arrange for the new block
  thenAccum      <- newBasicBlock
  followingAccum <- newBasicBlock -- if there's no else, this is where we go to directly on the else condition
  elseAccum      <- if (isJust elseBody) then newBasicBlock else return followingAccum

  let thenEntryId      = idOf thenAccum
      followingEntryId = idOf followingAccum
      elseEntryId      = idOf elseAccum

  -- visit the conditional expression (commits itself)
  buildConditionalIRExpression thenEntryId elseEntryId expr inAccum

  -- handle the then block
  buildIRStatement thenBody thenAccum >>= commitBasicBlock [(DefaultEdge, followingEntryId)]

  -- if linked to else, then evaluate else and link to following
  when (isJust elseBody) $ do
    buildIRStatement (fromJust elseBody) elseAccum >>= commitBasicBlock [(DefaultEdge, followingEntryId)]
    return ()

  -- and continue with the followingBlock
  return followingAccum

buildIRStatement (TWhileStatement cond body) inAccum = do
  accs@[condAccum, bodyAccum, followingAccum] <- replicateM 3 newBasicBlock
  let [condId, bodyId, followingId] = map idOf accs
  -- Commit our entry accum with edge to the condition
  commitBasicBlock [(DefaultEdge, condId)] inAccum
  -- Then evaluate the condition
  buildConditionalIRExpression bodyId followingId cond condAccum
  -- and the body
  withLoopEnvironment bodyId followingId $ buildIRStatement body bodyAccum >>= commitBasicBlock [(DefaultEdge, condId)]
  return followingAccum

buildIRStatement (TForStatement init cond iter body) inAccum = do
  accs@[condAccum, bodyAccum, followingAccum] <- replicateM 3 newBasicBlock
  let [condId, bodyId, followingId] = map idOf accs
  -- evaluate and commit initializer
  buildIRExpression VoidContext init inAccum >>= commitBasicBlock [(DefaultEdge, condId)]
  -- Then evaluate the condition
  buildConditionalIRExpression bodyId followingId cond condAccum
  -- and the body and the iter
  withLoopEnvironment bodyId followingId $ do
    buildIRStatement body bodyAccum >>= buildIRExpression VoidContext iter >>= commitBasicBlock [(DefaultEdge, condId)]
  -- and return the following block
  return followingAccum

-- break/continue/return/exit: all terminate their block
buildIRStatement TBreakStatement inAccum = do
  breakId <- State.gets breakBlockId
  guardError (InternalError "Cannot break, not in a loop body (should be caught at TAST)") (breakId /= noBlock)
  commitBasicBlock [(DefaultEdge, breakId)] inAccum
  newBasicBlock -- Code reachable only from this new block is dead code.
                -- We'll identify that dead code by blocks that have no in edges and prune it.

buildIRStatement TContinueStatement inAccum = do
  continueId <- State.gets continueBlockId
  guardError (DefaultError "Cannot continue, not in a loop body") (continueId /= noBlock)
  commitBasicBlock [(DefaultEdge, continueId)] inAccum
  newBasicBlock

buildIRStatement TExitStatement inAccum = do
  appendInstructionsM [IRExit] inAccum >>= commitBasicBlock []
  newBasicBlock

buildIRStatement TReturnStatement inAccum = do
  appendInstructionsM [IRReturn Void] inAccum >>= commitBasicBlock []
  newBasicBlock -- this block is unreachable and can be safely eliminated

buildIRStatement (TReturnValueStatement expr) inAccum = do
  buildIRExpression ValueContext expr inAccum >>= appendInstructionsM [IRReturn (typeOf expr)] >>= commitBasicBlock []
  newBasicBlock

buildIRStatement (TExpressionStatement expr) inAccum = buildIRExpression VoidContext expr inAccum

buildIRStatement TEmpty inAccum = return inAccum


data ExpressionContext = ValueContext -- cond expressions commit their resulting block, so different type signature
                       | VoidContext

buildIRExpression :: ExpressionContext -> TExpression -> BlockAccum -> ThrowsBlockingState BlockAccum

-- Global store
buildIRExpression ValueContext x@(TGlobalStore t id expr) accum =
  buildIRExpression ValueContext expr accum >>= appendInstructionsM [IRDup t, IRGStore id]
buildIRExpression VoidContext (TGlobalStore t id expr) accum =
  buildIRExpression ValueContext expr accum >>= appendInstructionsM [IRGStore id]

-- Local store
buildIRExpression ValueContext x@(TLocalStore t id expr) accum =
  buildIRExpression ValueContext expr accum >>= appendInstructionsM [IRDup t, IRStore id]
buildIRExpression VoidContext (TLocalStore t id expr) accum =
  buildIRExpression ValueContext expr accum >>= appendInstructionsM [IRStore id]

-- Global load
buildIRExpression VoidContext  (TGlobalLoad t id) acc = return acc
buildIRExpression ValueContext (TGlobalLoad t id) acc = appendInstructionsM [IRGLoad id] acc

-- Local load
buildIRExpression VoidContext  (TLocalLoad t id) acc = return acc
buildIRExpression ValueContext (TLocalLoad t id) acc = appendInstructionsM [IRLoad id] acc

-- Prefix

-- side-effectful - pre-inc/dec: we've checked for lvalue while building the typed AST
buildIRExpression VoidContext (TPrefixExpression _ op (TGlobalLoad t id)) acc | op == Preincrement || op == Predecrement = do
  appendInstructionsM [IRGLoad id, irConst t 1, IRArith t (if op == Preincrement then IRAdd else IRSubtract), IRGStore id] acc

buildIRExpression ValueContext (TPrefixExpression _ op (TGlobalLoad t id)) acc | op == Preincrement || op == Predecrement = do
  appendInstructionsM [IRGLoad id, irConst t 1, IRArith t (if op == Preincrement then IRAdd else IRSubtract), IRDup t, IRGStore id] acc

buildIRExpression VoidContext (TPrefixExpression _ op (TLocalLoad t id)) acc | op == Preincrement || op == Predecrement = do
  appendInstructionsM [IRLoad id, irConst t 1, IRArith t (if op == Preincrement then IRAdd else IRSubtract), IRStore id] acc

buildIRExpression ValueContext (TPrefixExpression _ op (TLocalLoad t id)) acc | op == Preincrement || op == Predecrement = do
  appendInstructionsM [IRLoad id, irConst t 1, IRArith t (if op == Preincrement then IRAdd else IRSubtract), IRDup t, IRStore id] acc

-- conditional - not (typed IR assures us that we have a TBooleanConversion if necessary)
buildIRExpression VoidContext  ex@(TPrefixExpression _ Not _) accum = conditionalToVoid ex accum
buildIRExpression ValueContext ex@(TPrefixExpression _ Not _) accum = conditionalToBooleanValue ex accum

-- value expressions
-- void context: ignore and recurse
buildIRExpression VoidContext (TPrefixExpression _ _ x) acc = buildIRExpression VoidContext x acc

-- or perform
buildIRExpression ValueContext (TPrefixExpression t Complement x) accum =
  buildIRExpression ValueContext x accum >>= appendInstructionsM [IRArith t IRComplement]

buildIRExpression ValueContext (TPrefixExpression t Minus x) accum =
  appendInstructionsM [irConst t 0] accum >>= buildIRExpression ValueContext x >>= appendInstructionsM [IRArith t IRSubtract]

-- Postfix
buildIRExpression VoidContext (TPostfixExpression _ (TGlobalLoad t id) op) accum =
  appendInstructionsM [IRGLoad id, irConst t 1, IRArith t (if op == Postincrement then IRAdd else IRSubtract), IRGStore id] accum

buildIRExpression VoidContext (TPostfixExpression _ (TLocalLoad t id) op) accum =
  appendInstructionsM [IRLoad id, irConst t 1, IRArith t (if op == Postincrement then IRAdd else IRSubtract), IRStore id] accum

buildIRExpression ValueContext (TPostfixExpression _ (TGlobalLoad t id) op) accum =
  appendInstructionsM [IRGLoad id, IRDup t, irConst t 1, IRArith t (if op == Postincrement then IRAdd else IRSubtract), IRGStore id] accum

buildIRExpression ValueContext (TPostfixExpression _ (TLocalLoad t id) op) accum =
  appendInstructionsM [IRLoad id, IRDup t, irConst t 1, IRArith t (if op == Postincrement then IRAdd else IRSubtract), IRStore id] accum

-- Binary
-- Evaluate booleans (Disj/Conj/comparisons):
buildIRExpression ValueContext ex@(TBinaryExpression _ _ op _) accum | op `elem` [Disj, Conj, Gt, Ge, Lt, Le, Eq, Ne] =
  conditionalToBooleanValue ex accum

-- Conditionals in void context (need to evaluate them properly for short-circuiting side-effects)
buildIRExpression VoidContext ex@(TBinaryExpression _ _ Conj _) accum = conditionalToVoid ex accum
buildIRExpression VoidContext ex@(TBinaryExpression _ _ Disj _) accum = conditionalToVoid ex accum

-- others
-- In void context we ignore the operator and evaluate for side-effects
buildIRExpression VoidContext ex@(TBinaryExpression _ lhs _ rhs) accum =
  buildIRExpression VoidContext lhs accum >>= buildIRExpression VoidContext rhs

-- In value context, we use the values.
buildIRExpression ValueContext ex@(TBinaryExpression t lhs op rhs) accum =
  buildIRExpression ValueContext lhs accum >>= buildIRExpression ValueContext rhs >>= appendInstructionsM [opFor op t]
  where
    opFor Add t      = IRArith t IRAdd
    opFor Subtract t = IRArith t IRSubtract
    opFor Multiply t = IRArith t IRMultiply
    opFor Divide t   = IRArith t IRDivide
    opFor Mod t      = IRArith t IRMod
    opFor And t      = IRArith t IRAnd
    opFor Or t       = IRArith t IROr
    opFor Xor t      = IRArith t IRXor
    opFor Lshift t   = IRArith t IRLshift
    opFor Rshift t   = IRArith t IRRshift

-- Literals
buildIRExpression ValueContext (TShortLiteral val) accum = appendInstructionsM [IRSConst val] accum
buildIRExpression ValueContext (TByteLiteral val)  accum = appendInstructionsM [IRBConst val] accum
buildIRExpression VoidContext (TShortLiteral val)  accum = return accum
buildIRExpression VoidContext (TByteLiteral val)   accum = return accum

-- Method calls
buildIRExpression ValueContext (TMethodCall t id args) accum =
  foldM (flip (buildIRExpression ValueContext)) accum args >>= appendInstructionsM [IRCall id]

buildIRExpression ValueContext (TSyscall t op args) accum =
  foldM (flip (buildIRExpression ValueContext)) accum args >>= appendInstructionsM [IRSyscall op]

-- Type conversions
-- void context just recurses
buildIRExpression VoidContext (TTypeConversion _ expr) accum =
  buildIRExpression VoidContext expr accum

-- conversion to void switches into void context
buildIRExpression ValueContext (TTypeConversion Void expr) accum =
  buildIRExpression VoidContext expr accum

-- byte and short are convertible with b2s/s2b
buildIRExpression ValueContext (TTypeConversion Short expr) accum = do
  guardError (InternalError "Unexpected value context") (typeOf expr == Byte)
  buildIRExpression ValueContext expr accum >>= appendInstructionsM [IRB2S]

buildIRExpression ValueContext (TTypeConversion Byte expr) accum = do
  guardError (InternalError "Unexpected value context") (typeOf expr == Short)
  buildIRExpression ValueContext expr accum >>= appendInstructionsM [IRS2B]

-- Boolean conversions
buildIRExpression VoidContext (TBooleanConversion expr) accum =
  buildIRExpression VoidContext expr accum

buildIRExpression ValueContext (TBooleanConversion expr) accum =
  buildIRExpression ValueContext expr accum >>= booleanCast (typeOf expr)
  where
    booleanCast :: Type -> BlockAccum -> ThrowsBlockingState BlockAccum
    booleanCast Byte acc = return acc
    booleanCast Short acc = appendInstructionsM [(IRArith Byte IROr)] acc
    booleanCast t _ = throwError $ InternalError $ printf "Bad type at boolean cast %s" (show t)

-- Void context catch-all: build in value context and pop if void
buildIRExpression VoidContext expr accum = do
  buildIRExpression ValueContext expr accum >>= discardValue (typeOf expr)
  where
    discardValue :: Type -> BlockAccum -> ThrowsBlockingState BlockAccum
    discardValue Void = return
    discardValue t = appendInstructionsM [IRPop t]

buildIRExpression ValueContext expr _ = throwError $ InternalError $ printf "Value context TExpression not matched for IR: %s" (show expr)

-- Conditional Expression context
conditionalToBooleanValue :: TExpression -> BlockAccum -> ThrowsBlockingState BlockAccum
conditionalToBooleanValue ex accum = do
  accs@[thenAccum, elseAccum, afterAccum] <- replicateM 3 newBasicBlock
  let [thenId, elseId, afterId] = map idOf accs
  buildConditionalIRExpression thenId elseId ex accum
  appendInstructionsM [IRBConst 1] thenAccum >>= commitBasicBlock [(DefaultEdge, afterId)]
  appendInstructionsM [IRBConst 0] elseAccum >>= commitBasicBlock [(DefaultEdge, afterId)]
  return afterAccum

conditionalToVoid :: TExpression -> BlockAccum -> ThrowsBlockingState BlockAccum
conditionalToVoid ex accum = do
  afterAccum <- newBasicBlock
  let afterId = idOf afterAccum
  buildConditionalIRExpression afterId afterId ex accum
  return afterAccum

buildConditionalIRExpression :: Node -> Node -> TExpression -> BlockAccum -> ThrowsBlockingState ()

buildConditionalExpression ifId elseId (TPrefixExpression t Not expr) accum =
  buildConditionalExpression elseId ifId expr accum

buildConditionalIRExpression ifId elseId (TBinaryExpression _ lhs Disj rhs) accum = do
  rhsAccum <- newBasicBlock
  let rhsId = idOf rhsAccum
  buildConditionalIRExpression ifId rhsId lhs accum
  buildConditionalIRExpression ifId elseId rhs rhsAccum

buildConditionalIRExpression ifId elseId (TBinaryExpression _ lhs Conj rhs) accum = do
  rhsAccum <- newBasicBlock
  let rhsId = idOf rhsAccum
  buildConditionalIRExpression rhsId elseId lhs accum
  buildConditionalIRExpression ifId  elseId rhs rhsAccum

buildConditionalIRExpression ifId elseId (TBinaryExpression _ lhs op rhs) accum | elem op [Gt, Ge, Lt, Le, Eq, Ne] = do
  let t = typeOf lhs -- typedAST ensures lhstype == rhstype
      zLhs  = isZero lhs
      zRhs  = isZero rhs
      noCmp = zLhs || zRhs -- constant expression evaluation ensures that one is non-zero, or we wouldn't be here
      op'   = if zLhs then flipOp op else op
      lhs' = if zRhs then (TBooleanConversion lhs) else lhs
      rhs' = if zLhs then (TBooleanConversion rhs) else rhs
  guardError (InternalError "Saw 0==0, what happened to constant folding?") $ not (zLhs && zRhs)
  return accum
    >>= (if zLhs  then return else buildIRExpression ValueContext lhs')
    >>= (if zRhs  then return else buildIRExpression ValueContext rhs')
    >>= (if noCmp then return else appendInstructionsM [IRArith t IRCmp])
    >>= commitBasicBlock [(ConditionalEdge (conditionFor op'), ifId), (DefaultEdge, elseId)]
    >> return ()
  where
    isZero (TShortLiteral 0) = True
    isZero (TByteLiteral 0) = True
    isZero _ = False
    flipOp Gt = Le
    flipOp Lt = Ge
    flipOp Ge = Lt
    flipOp Le = Gt
    flipOp o  = o
    conditionFor Gt = IfGt
    conditionFor Lt = IfLt
    conditionFor Le = IfLe
    conditionFor Ge = IfGe
    conditionFor Eq = IfEq
    conditionFor Ne = IfNe

-- Constants as conditionals can only go to one branch. Make a direct
-- edge so we can prune the impossible block later.
buildConditionalIRExpression ifId elseId (TShortLiteral x) accum = do
  commitBasicBlock [(DefaultEdge, if x /= 0 then ifId else elseId)] accum

buildConditionalIRExpression ifId elseId (TByteLiteral x) accum = do
  commitBasicBlock [(DefaultEdge, if x /= 0 then ifId else elseId)] accum


-- catch-all: value context
buildConditionalIRExpression ifId elseId expr accum = do
  buildIRExpression ValueContext expr accum >>= commitBasicBlock [(ConditionalEdge IfNe, ifId),
                                                                  (DefaultEdge, elseId)]
  return ()

irConst :: Type -> Int -> IRInstruction
irConst Byte i = IRBConst $ fromIntegral i
irConst Short i = IRSConst $ fromIntegral i
