module TypedAST where
import BasicTypes
import Parser
import Errors
import ErrorState
import Indexes

import Text.Printf

import Data.Bits
import Data.Int

import Data.Maybe
import Control.Monad
import Control.Monad.Except

import qualified Control.Monad.State as State

import Data.Map(Map)
import qualified Data.Map as Map

-- Typed and Bound AST

type MethodID = Int
type VariableID = Int

type VariableScope = Map Ident VariableID
type MethodScope   = Map Ident MethodID
emptyScope :: Map Ident a
emptyScope = Map.empty

-- global method/variable index and scope
data TProgram  = TProgram (Index TMethod) (Index TVariable) MethodScope VariableScope

data TVariable = TVariable VariableID Type

data TMethod = TMethod MethodID Type [Type] (Index TVariable) VariableScope [TStatement]

data TStatement = TBlock VariableScope [TStatement]
                | TIfStatement TExpression TStatement (Maybe TStatement)
                | TWhileStatement TExpression TStatement
                | TForStatement TExpression TExpression TExpression TStatement
                | TExpressionStatement TExpression
                | TReturnValueStatement TExpression
                | TReturnStatement
                | TExitStatement
                | TBreakStatement
                | TContinueStatement
                | TEmpty
                deriving (Eq)

data TExpression = TGlobalStore Type VariableID TExpression
                 | TLocalStore Type VariableID TExpression
                 | TGlobalLoad Type VariableID
                 | TLocalLoad Type VariableID
                 | TPrefixExpression Type PrefixOp TExpression
                 | TPostfixExpression Type TExpression PostfixOp
                 | TBinaryExpression Type TExpression BinaryOp TExpression
                 | TShortLiteral Int16
                 | TByteLiteral Int8
                 | TMethodCall Type MethodID [TExpression]
                 | TSyscall Type SyscallOp [TExpression]
                 | TTypeConversion Type TExpression  -- explicit and implicit type conversions turn into this
                 -- convert argument to a boolean (byte) value -- not just truncating
                 | TBooleanConversion TExpression
                 deriving (Eq)

data SyscallOp = PressKey | ReleaseKey | CheckKey | CheckPhysKey | WaitKey | WaitPhysKey
               | Delay | GetUptimeMS | GetUptime | Buzz | BuzzAt
               | MoveMouse | PressMouseButtons | ReleaseMouseButtons
               deriving (Show, Eq)

syscalls :: [(Ident, (SyscallOp, Type, [Type]))]
syscalls = [("pressKey",            (PressKey           , Void,  [Byte]))
           ,("releaseKey",          (ReleaseKey         , Void,  [Byte]))
           ,("checkKey",            (CheckKey           , Byte,  [Byte]))
           ,("checkPhysKey",        (CheckPhysKey       , Byte,  [Byte]))
           ,("waitKey",             (WaitKey            , Byte,  [Byte, Short]))
           ,("waitPhysKey",         (WaitPhysKey        , Byte,  [Byte, Short]))
           ,("delay",               (Delay              , Void,  [Short]))
           ,("getUptimeMS",         (GetUptimeMS        , Short, []))
           ,("getUptime",           (GetUptime          , Short, []))
           ,("buzz",                (Buzz               , Void,  [Short]))
           ,("buzzAt",              (BuzzAt             , Void,  [Short, Byte]))
           ,("moveMouse",           (MoveMouse          , Void,  [Byte, Byte]))
           ,("pressMouseButtons",   (PressMouseButtons  , Void,  [Byte]))
           ,("releaseMouseButtons", (ReleaseMouseButtons, Void,  [Byte]))]

-- Show instances

instance Show TProgram where
  show (TProgram meths vars _ _) = unlines $ map show (indexElems meths) ++ map show (indexElems vars)

instance Show TVariable where
  show (TVariable i typ) = printf "%s var%d;" (show typ) i

instance Show TMethod where
  show (TMethod i rTyp aTyps _ _ body) =
    printf "%s method%d(%s){\n%s\n}" (show rTyp) i (showlistwithsep ", " aTyps) (unlines $ map show body)

instance Show TStatement where
  show (TBlock _ body) = printf "{\n%s\n}" $ unlines ((map show) body)
  show (TIfStatement expr tb fb)      = printf "if(%s)%s%s" (show expr) (show tb) (maybe "" (("else "++).show) fb)
  show (TWhileStatement expr body)    = printf "while(%s)%s"  (show expr) (show body)
  show (TForStatement i cnd adv body) = printf "for(%s; %s; %s)%s" (show i) (show cnd) (show adv) (show body)
  show (TExpressionStatement expr)    = printf "%s;" (show expr)
  show (TReturnValueStatement expr)   = printf "return %s;" (show expr)
  show TReturnStatement               = "return;"
  show TExitStatement                 = "exit;"
  show TBreakStatement                = "break;"
  show TContinueStatement             = "continue;"
  show TEmpty                         = ";"

instance Show TExpression where
  show (TGlobalStore t i expr)      = printf "global%d:%s = %s" i (show t) (show expr)
  show (TLocalStore t i expr)       = printf "local%d:%s = %s" i (show t) (show expr)
  show (TGlobalLoad t i)            = printf "global%d:%s" i (show t)
  show (TLocalLoad t i)             = printf "local%d:%s" i (show t)
  show (TPrefixExpression t op ex)  = printf "%s(%s):%s" (show op) (show ex) (show t)
  show (TPostfixExpression t ex op) = printf "(%s)%s:%s" (show ex) (show op) (show t)
  show (TBinaryExpression t l op r) = printf "((%s) %s (%s)):%s" (show l) (show op) (show r) (show t)
  show (TShortLiteral v)            = (show v) ++ ":short"
  show (TByteLiteral b)             = show b ++ ":byte"
  show (TMethodCall t i args)       = printf "method%d(%s):%s" i ((showlistwithsep ", ") args) (show t)
  show (TSyscall t op args)         = printf "%s(%s):%s" (show op) ((showlistwithsep ", ") args) (show t)
  show (TTypeConversion typ expr)   = printf "(%s)%s" (show typ) (show expr)
  show (TBooleanConversion expr)    = printf "(bool)%s" (show expr)

-- Traverse AST building a scope

emptyTProgram :: TProgram
emptyTProgram = TProgram newIndex newIndex emptyScope emptyScope

-- Build a skeleton TProgram from the declarations
scanDecs :: [Declaration] -> ThrowsError TProgram
scanDecs decs = foldM addToProgram emptyTProgram decs
  where
    addToProgram :: TProgram -> Declaration -> ThrowsError TProgram
    addToProgram (TProgram meths vars methScp varScp) (MethodDeclaration typ nam args _) = do
      let newid = indexNextKey meths
          (localVars, localScope) = foldl addMethodArg (newIndex, emptyScope) args
          newMethod = TMethod newid typ (map fst args) localVars localScope []
          meths' = indexAppend newMethod meths
          methScp' = Map.insert nam newid methScp
      when (Map.member nam methScp) $ throwError (Redefine $ printf "Method '%s' already defined" nam)
      return $ TProgram meths' vars methScp' varScp
    addToProgram (TProgram meths vars methScp varScp) (GlobalVariableDeclaration typ nam) = do
      let newid = indexNextKey vars
          newVar = TVariable newid typ
          vars' = indexAppend newVar vars
          varScp' = Map.insert nam newid varScp
      when (Map.member nam varScp) $ throwError (Redefine $ printf "Global variable '%s' already defined" nam)
      return $ TProgram meths vars' methScp varScp'
    addMethodArg :: ((Index TVariable), VariableScope) -> (Type, Ident) -> ((Index TVariable), VariableScope)
    addMethodArg (vIdx, vScp) (typ, nam) =
      let newid = indexNextKey vIdx in
      ((indexAppend (TVariable newid typ) vIdx), (Map.insert nam newid vScp))


buildTProgram :: [Declaration] -> ThrowsError TProgram
buildTProgram decs = do
  p  <- scanDecs decs
  p' <- foldM buildTMethod p (filter isMethod decs)
  return p'
    where
      isMethod (MethodDeclaration _ _ _ _) = True
      isMethod _ = False

optTProgram :: TProgram -> ThrowsError TProgram
optTProgram p = return $ constantEvalProgram p

-- To build a TMethod we traverse the tree building up binding
-- information as we go.
data BindingState = BindingState { methodIndex    :: Index TMethod,
                                   methodScope    :: MethodScope,
                                   globalVarIndex :: Index TVariable,
                                   globalVarScope :: VariableScope,
                                   varIndex       :: Index TVariable,
                                   varScopes      :: [VariableScope],
                                   returnType     :: Type,
                                   withinLoop     :: Bool
                                   } deriving (Show);

type ThrowsBindingState = ThrowsState CompileError BindingState

buildTMethod :: TProgram -> Declaration -> ThrowsError TProgram
buildTMethod (TProgram gMIndex gVIndex gMScope gVScope) (MethodDeclaration _ nam _ body) = do
  unless (isNothing $ lookup nam syscalls) $
    throwError (Redefine $ printf "Method \"%s\" conflicts with built-in" nam)
  -- find the method id by name in the global method scope
  methodId <- mapLookup "MethodScope" nam gMScope
  -- and look up its skeleton IRMethod in the index
  (TMethod _ mRetType mArgTypes vIndex initialVScope _) <- indexLookup "IRMethod" methodId gMIndex

  -- construct an initial IR building state for the method
  let inputState = BindingState { methodIndex    = gMIndex,
                                  methodScope    = gMScope,
                                  globalVarIndex = gVIndex,
                                  globalVarScope = gVScope,
                                  varIndex       = vIndex,
                                  varScopes      = [initialVScope],
                                  returnType     = mRetType,
                                  withinLoop     = False }

  -- then in the BindingThrowsState monad with the input state, run
  -- buildTStatement over each statement in the body, returning the
  -- resulting program in the ThrowsError monad
  fst $ runThrowsState inputState $ do
    tStats <- mapM buildTStatement body
    BindingState { varIndex = vIndex', varScopes = [initialVScope'] } <- State.get
    let method = TMethod methodId mRetType mArgTypes vIndex' initialVScope' tStats
    return $ TProgram (indexInsert methodId method gMIndex) gVIndex gMScope gVScope

--- utility functions

enterLoop :: ThrowsBindingState Bool
enterLoop = do
  loopState <- State.gets withinLoop
  State.modify (\s->s{ withinLoop = True })
  return loopState

leaveLoop :: Bool -> ThrowsBindingState ()
leaveLoop x = State.modify $ \s->s{withinLoop = x}

typeOf :: TExpression -> Type
typeOf (TGlobalStore t _ _) = t
typeOf (TGlobalLoad t _) = t
typeOf (TLocalStore t _ _) = t
typeOf (TLocalLoad t _) = t
typeOf (TPrefixExpression t _ _) = t
typeOf (TPostfixExpression t _ _) = t
typeOf (TBinaryExpression t _ _ _) = t
typeOf (TShortLiteral _) = Short
typeOf (TByteLiteral _) = Byte
typeOf (TMethodCall t _ _) = t
typeOf (TTypeConversion t _) = t
typeOf (TBooleanConversion _) = Byte
typeOf (TSyscall t _ _) = t

-- Type promotion - always allowed to become a 'bigger' type
typePromoteCheck :: Type -> TExpression -> ThrowsBindingState TExpression
typePromoteCheck toType tExpr
  | toType == (typeOf tExpr) = return tExpr
  | toType > (typeOf tExpr)  = return $ TTypeConversion toType tExpr -- if t1 < t2, t1 can become t2 without a cast
  | otherwise                = throwError $ TypeError toType (typeOf tExpr) (show tExpr)

-- type cast - also allowed to become a smaller type, unless void
typeCastCheck :: Type -> TExpression -> ThrowsBindingState TExpression
typeCastCheck toType tExpr | toType == (typeOf tExpr) = return tExpr
typeCastCheck toType tExpr | toType == Void           = return $ TTypeConversion toType tExpr
typeCastCheck _      tExpr | ((typeOf tExpr) == Void) = throwError $ VoidCastError (show tExpr)
typeCastCheck toType tExpr | otherwise                = return $ TTypeConversion toType tExpr

valueTypeCheck :: TExpression -> ThrowsBindingState TExpression
valueTypeCheck ex = do
  guardError (NeedsValueError (show ex)) $ (typeOf ex) /= Void
  return ex

booleanConvertCheck :: TExpression -> ThrowsBindingState TExpression
booleanConvertCheck ex = do
  ex' <- valueTypeCheck ex
  return $ if typeOf(ex') == Byte
           then ex'
           else TBooleanConversion ex'

lvalueCheck :: Expression -> ThrowsBindingState ()
lvalueCheck (VariableAccess _) = return ()
lvalueCheck ex = throwError $ LValueError (show ex)

lookupLocalVar :: Ident -> ThrowsBindingState TVariable
lookupLocalVar nam = do
  state <- State.get
  liftThrows $ do -- in Either monad
    varid <- nestedMapLookup "LocalScope" nam (varScopes state)
    indexLookup "LocalVarID" varid (varIndex state)

lookupGlobalVar :: Ident -> ThrowsBindingState TVariable
lookupGlobalVar nam = do
  state <- State.get
  liftThrows $ do
    varid <- mapLookup "GlobalScope" nam (globalVarScope state)
    indexLookup "GlobalVarID" varid (globalVarIndex state)

lookupMethod :: Ident -> ThrowsBindingState TMethod
lookupMethod nam = do
  state <- State.get
  liftThrows $ do
    methid <- mapLookup "MethodScope" nam (methodScope state)
    indexLookup "MethodID" methid (methodIndex state)

--- statements

buildTStatement :: Statement -> ThrowsBindingState TStatement
buildTStatement (Block stats) = do
  pushStateVarScope
  tStats <- mapM buildTStatement stats
  blockScope <- popStateVarScope
  return $ TBlock blockScope tStats
  where
    pushStateVarScope = State.modify (\s@BindingState{ varScopes = vs } -> s{ varScopes = Map.empty : vs })
    popStateVarScope  = do
      (vs:vss) <- State.gets varScopes
      State.modify (\s -> s{ varScopes = vss })
      return vs

buildTStatement (IfStatement expr thenBody elseBody) = do
  tExpr <- buildTExpression expr >>= booleanConvertCheck
  tThenBody <- buildTStatement thenBody
  tElseBody <- maybe (return Nothing) (\eb -> (liftM Just) $ buildTStatement eb) elseBody
  return $ TIfStatement tExpr tThenBody tElseBody

buildTStatement (WhileStatement cond body) = do
  tCond <- buildTExpression cond >>= booleanConvertCheck
  oldLoopState <- enterLoop
  tBody <- buildTStatement body
  leaveLoop oldLoopState
  return $ TWhileStatement tCond tBody

buildTStatement (ForStatement initializer cond iter body) = do
  tInit <- buildTExpression initializer
  tCond <- buildTExpression cond >>= booleanConvertCheck
  tIter <- buildTExpression iter
  oldLoopState <- enterLoop
  tBody <- buildTStatement body
  leaveLoop oldLoopState
  return $ TForStatement tInit tCond tIter tBody

buildTStatement BreakStatement = do
  State.gets withinLoop >>= guardError (DefaultError "Cannot break, not in a loop body")
  return TBreakStatement

buildTStatement ContinueStatement = do
  State.gets withinLoop >>= guardError (DefaultError "Cannot continue, not in a loop body")
  return TContinueStatement

buildTStatement (VariableDeclarationStatement typ nam initializer) = do
  when (typ == Void) $ throwError $ (DefaultError "Cannot declare variable as void")
  varId <- addLocalVariable typ nam
  case initializer of
    Nothing -> return TEmpty
    Just expr -> do
      tExpr <- buildTExpression expr >>= typePromoteCheck typ
      return $ TExpressionStatement (TLocalStore typ varId tExpr)
  where
    addLocalVariable :: Type -> Ident -> ThrowsBindingState VariableID
    addLocalVariable lvTyp lvNam = do
      BindingState { varIndex = vidx, varScopes = (vscope:vsrest) } <- State.get
      guardError (Redefine $ printf "Variable %s already exists in scope" lvNam) $ not (Map.member lvNam vscope)
      let newid = indexNextKey vidx
      State.modify $ \s -> s{ varIndex  = (indexAppend (TVariable newid lvTyp) vidx),
                              varScopes = ((Map.insert lvNam newid vscope):vsrest) }
      return newid

buildTStatement (ExpressionStatement expr) = do
  fmap TExpressionStatement $ buildTExpression expr >>= typePromoteCheck Void

buildTStatement r@ReturnStatement = do
  retTyp <- State.gets returnType
  guardError (ReturnTypeError retTyp Void (show r)) $ retTyp == Void
  return TReturnStatement

buildTStatement ExitStatement = return TExitStatement

buildTStatement r@(ReturnValueStatement expr) = do
  retTyp <- State.gets returnType
  tExpr <- buildTExpression expr >>= typePromoteCheck retTyp
  guardError (ReturnTypeError retTyp (typeOf tExpr) (show r)) $
    retTyp /= Void -- in addition to promotion check, we can't return a value to a void return type
  return $ TReturnValueStatement tExpr

buildTExpression :: Expression -> ThrowsBindingState TExpression

buildTExpression e@(Assignment (VariableAccess varName) e2) = do
  (tStor, typ) <- (lookupLocalVar varName >>= mkStor TLocalStore)
                  `orTry` (lookupGlobalVar varName >>= mkStor TGlobalStore)
                  `orTry` (throwError $ UndefinedVarError varName (show e))
  tE2' <- buildTExpression e2 >>= typePromoteCheck typ
  return $ tStor tE2'
  where
    mkStor :: (Type -> VariableID -> TExpression -> TExpression) ->
              TVariable -> ThrowsBindingState ((TExpression -> TExpression), Type)
    mkStor constr (TVariable vid typ) = return (constr typ vid, typ)

buildTExpression (Assignment e1 _) = throwError $ LValueError (show e1)

buildTExpression (PrefixExpression Not e1) = do
  tE1 <- buildTExpression e1 >>= booleanConvertCheck
  return $ TPrefixExpression Byte Not tE1

-- Parser seems to grab negative numbers as unary minus - fix this nicely
buildTExpression (PrefixExpression Minus (ShortLiteral s)) = buildTExpression (ShortLiteral $ -s)
buildTExpression (PrefixExpression Minus (ByteLiteral b)) = buildTExpression (ByteLiteral $ -b)

buildTExpression (PrefixExpression op e1) = do
  when (op == Predecrement || op == Preincrement) $ lvalueCheck e1
  tE1 <- buildTExpression e1 >>= valueTypeCheck
  return $ TPrefixExpression (typeOf tE1) op tE1

buildTExpression (PostfixExpression e1 op) = do
  lvalueCheck e1
  tE1 <- buildTExpression e1 >>= valueTypeCheck
  return $ TPostfixExpression (typeOf tE1) tE1 op

buildTExpression (BinaryExpression e1 op e2) | (op == Disj || op == Conj) = do
  tE1 <- buildTExpression e1 >>= booleanConvertCheck
  tE2 <- buildTExpression e2 >>= booleanConvertCheck
  return $ TBinaryExpression Byte tE1 op tE2

buildTExpression (BinaryExpression e1 op e2) | otherwise = do
  tE1 <- buildTExpression e1 >>= valueTypeCheck
  tE2 <- buildTExpression e2 >>= valueTypeCheck
  let e1Type = typeOf(tE1)
      e2Type = typeOf(tE2)
      maxType = max e1Type e2Type
  tE1' <- typePromoteCheck maxType tE1
  tE2' <- typePromoteCheck maxType tE2
  return $ TBinaryExpression (outType maxType op) tE1' op tE2'
  where
    outType :: Type -> BinaryOp -> Type
    outType t Add      = t
    outType t Subtract = t
    outType t Multiply = t
    outType t Divide   = t
    outType t Mod      = t
    outType t And      = t
    outType t Or       = t
    outType t Xor      = t
    outType t Lshift   = t
    outType t Rshift   = t
    outType _ Gt       = Byte
    outType _ Lt       = Byte
    outType _ Ge       = Byte
    outType _ Le       = Byte
    outType _ Eq       = Byte
    outType _ Ne       = Byte

buildTExpression (ShortLiteral val) = do
  return $ TShortLiteral val

buildTExpression (ByteLiteral val) = do
  return $ TByteLiteral val

buildTExpression (VariableAccess varName) = do
  (lookupLocalVar varName >>= mkLoad TLocalLoad)
  `orTry` (lookupGlobalVar varName >>= mkLoad TGlobalLoad)
  `orTry` (throwError $ UndefinedVarError varName "context not implemented")
  where
    mkLoad constr (TVariable vid typ) = return $ constr typ vid

buildTExpression m@(MethodInvocation methName args) = do
  (constr, argTypes) <- resolveInvocation methName
  guardError (MethodArgsError methName (length argTypes) (show m)) (length argTypes == length args)
  tArgs <- mapM buildTExpression args
  tArgs' <- mapM (uncurry typePromoteCheck) (zip argTypes tArgs)   -- check and conform to arg types
  return $ constr tArgs'
  where
    resolveInvocation :: Ident -> ThrowsBindingState (([TExpression] -> TExpression), [Type])
    resolveInvocation mName = resolveMethod mName `orTry` resolveSyscall mName
    resolveMethod mName = do
      (TMethod mId retType argTypes _ _ _) <- lookupMethod mName
      return ((TMethodCall retType mId), argTypes)
    resolveSyscall mName = do
      (op, retTyp, argTypes) <- liftThrows $
        maybeToError (UndefinedMethodError methName (show m)) $ lookup mName syscalls
      return ((TSyscall retTyp op), argTypes)

buildTExpression (TypeCast typ expr) = do
  buildTExpression expr >>= typeCastCheck typ


-- map a function on expressions across a TPRogram
exMap :: (TExpression -> TExpression) -> TProgram -> TProgram
exMap fn (TProgram meths vars mScp vScp) = TProgram (indexMap (exMap' fn) meths) vars mScp vScp
  where
    exMap' :: (TExpression -> TExpression) -> TMethod -> TMethod
    exMap' f (TMethod i rTyp aTyps varI varS stats) = TMethod i rTyp aTyps varI varS $ map (exMap'' f) stats
    exMap'' :: (TExpression -> TExpression) -> TStatement -> TStatement
    exMap'' f (TBlock vs stats) = TBlock vs (map (exMap'' f) stats)
    exMap'' f (TIfStatement ex tb fb) = TIfStatement (f ex) (exMap'' f tb) $ maybe Nothing (Just.(exMap'' f)) fb
    exMap'' f (TWhileStatement ex body) = TWhileStatement (f ex) (exMap'' f body)
    exMap'' f (TForStatement e1 e2 e3 body) = TForStatement (f e1) (f e2) (f e3) (exMap'' f body)
    exMap'' f (TExpressionStatement ex) = TExpressionStatement (f ex)
    exMap'' f (TReturnValueStatement ex) = TReturnValueStatement (f ex)
    exMap'' _ s = s


-- Pre-evaluate constant expressions
constantEvalProgram :: TProgram -> TProgram
constantEvalProgram = exMap constantEval

constantEval :: TExpression -> TExpression

constantEval (TMethodCall t i args) = TMethodCall t i (map constantEval args)

constantEval (TSyscall t op args)   = TSyscall t op (map constantEval args)

constantEval (TGlobalStore t i ex)  = TGlobalStore t i (constantEval ex)

constantEval (TLocalStore t i ex)   = TLocalStore t i (constantEval ex)

constantEval (TBooleanConversion ex) =
  let ex' = constantEval ex in
  case ex' of
    (TShortLiteral l) -> if (l /= 0) then (TByteLiteral 1) else (TByteLiteral 0)
    (TByteLiteral l)  -> if (l /= 0) then (TByteLiteral 1) else (TByteLiteral 0)
    _                -> TBooleanConversion ex'

constantEval (TTypeConversion t ex) =
  let ex' = constantEval ex in
  case (t, ex') of
    (Byte, (TShortLiteral l)) -> (TByteLiteral  $ fromIntegral l)
    (Short, (TByteLiteral l)) -> (TShortLiteral $ fromIntegral l)
    _                        -> TTypeConversion t ex'

constantEval (TPrefixExpression t op ex) =
  let ex' = constantEval ex
  in maybe (TPrefixExpression t op ex') id $ evalPrefix op ex'
  where
    evalPrefix Not        (TByteLiteral l)  = Just $ TByteLiteral $ if (l == 0) then 1 else 0
    evalPrefix Complement (TShortLiteral l) = Just $ TShortLiteral $ complement l
    evalPrefix Complement (TByteLiteral l)  = Just $ TByteLiteral  $ complement l
    evalPrefix Minus      (TShortLiteral l) = Just $ TShortLiteral $ -l
    evalPrefix Minus      (TByteLiteral l)  = Just $ TByteLiteral  $ -l
    evalPrefix _ _ = Nothing

constantEval (TBinaryExpression tp la op ra) =
  let la' = constantEval la
      ra' = constantEval ra
  in maybe (TBinaryExpression tp la' op ra') id $ evalBinary la' op ra'
  where
    -- bytes
    evalBinary (TByteLiteral l) Add      (TByteLiteral r) = Just $ TByteLiteral (l + r)
    evalBinary (TByteLiteral l) Subtract (TByteLiteral r) = Just $ TByteLiteral (l - r)
    evalBinary (TByteLiteral l) Multiply (TByteLiteral r) = Just $ TByteLiteral (l * r)
    evalBinary (TByteLiteral l) Divide   (TByteLiteral r) = Just $ TByteLiteral (l `div` r)
    evalBinary (TByteLiteral l) Mod      (TByteLiteral r) = Just $ TByteLiteral (l `mod` r)
    evalBinary (TByteLiteral l) And      (TByteLiteral r) = Just $ TByteLiteral (l .&. r)
    evalBinary (TByteLiteral l) Or       (TByteLiteral r) = Just $ TByteLiteral (l .|. r)
    evalBinary (TByteLiteral l) Xor      (TByteLiteral r) = Just $ TByteLiteral (l `xor` r)
    evalBinary (TByteLiteral l) Gt       (TByteLiteral r) = Just $ TByteLiteral (if l>r  then 1 else 0)
    evalBinary (TByteLiteral l) Ge       (TByteLiteral r) = Just $ TByteLiteral (if l>=r then 1 else 0)
    evalBinary (TByteLiteral l) Lt       (TByteLiteral r) = Just $ TByteLiteral (if l<r  then 1 else 0)
    evalBinary (TByteLiteral l) Le       (TByteLiteral r) = Just $ TByteLiteral (if l<=r then 1 else 0)
    evalBinary (TByteLiteral l) Eq       (TByteLiteral r) = Just $ TByteLiteral (if l==r then 1 else 0)
    evalBinary (TByteLiteral l) Ne       (TByteLiteral r) = Just $ TByteLiteral (if l/=r then 1 else 0)
    evalBinary (TByteLiteral l) Lshift   (TByteLiteral r) = Just $ TByteLiteral (shiftL l $ fromIntegral r)
    evalBinary (TByteLiteral l) Rshift   (TByteLiteral r) = Just $ TByteLiteral (shiftR l $ fromIntegral r)
    -- shorts
    evalBinary (TShortLiteral l) Add      (TShortLiteral r) = Just $ TShortLiteral (l + r)
    evalBinary (TShortLiteral l) Subtract (TShortLiteral r) = Just $ TShortLiteral (l - r)
    evalBinary (TShortLiteral l) Multiply (TShortLiteral r) = Just $ TShortLiteral (l * r)
    evalBinary (TShortLiteral l) Divide   (TShortLiteral r) = Just $ TShortLiteral (l `div` r)
    evalBinary (TShortLiteral l) Mod      (TShortLiteral r) = Just $ TShortLiteral (l `mod` r)
    evalBinary (TShortLiteral l) And      (TShortLiteral r) = Just $ TShortLiteral (l .&. r)
    evalBinary (TShortLiteral l) Or       (TShortLiteral r) = Just $ TShortLiteral (l .|. r)
    evalBinary (TShortLiteral l) Xor      (TShortLiteral r) = Just $ TShortLiteral (l `xor` r)
    evalBinary (TShortLiteral l) Gt       (TShortLiteral r) = Just $ TByteLiteral (if l>r  then 1 else 0)
    evalBinary (TShortLiteral l) Ge       (TShortLiteral r) = Just $ TByteLiteral (if l>=r then 1 else 0)
    evalBinary (TShortLiteral l) Lt       (TShortLiteral r) = Just $ TByteLiteral (if l<r  then 1 else 0)
    evalBinary (TShortLiteral l) Le       (TShortLiteral r) = Just $ TByteLiteral (if l<=r then 1 else 0)
    evalBinary (TShortLiteral l) Eq       (TShortLiteral r) = Just $ TByteLiteral (if l==r then 1 else 0)
    evalBinary (TShortLiteral l) Ne       (TShortLiteral r) = Just $ TByteLiteral (if l/=r then 1 else 0)
    evalBinary (TShortLiteral l) Lshift   (TShortLiteral r) = Just $ TShortLiteral (shiftL l $ fromIntegral r)
    evalBinary (TShortLiteral l) Rshift   (TShortLiteral r) = Just $ TShortLiteral (shiftR l $ fromIntegral r)
    --- boolean operators: constant and shortcircuit
    evalBinary (TByteLiteral l) Disj (TByteLiteral r) = Just $ TByteLiteral (if (l /= 0) || (r /= 0) then 1 else 0)
    evalBinary (TByteLiteral l) Conj (TByteLiteral r) = Just $ TByteLiteral (if (l /= 0) && (r /= 0) then 1 else 0)
    evalBinary (TByteLiteral l) Disj _ | l /= 0      = Just $ TByteLiteral 1
    evalBinary (TByteLiteral 0) Conj _               = Just $ TByteLiteral 0
    evalBinary _ _ _ = Nothing

-- catch-all
constantEval ex = ex
