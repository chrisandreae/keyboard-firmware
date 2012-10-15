module Output where

import BasicTypes
import Indexes
import BlockIR
import Errors
import TypedAST

import Text.Printf

import Control.Monad
import Control.Monad.Error

import Data.Maybe
import Data.List
import Data.Bits(shiftR, (.&.))

import Data.Int
import Data.Word

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.DFS

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B

data BytecodeProgram = BytecodeProgram Word8 [BytecodeMethod] -- nglobals, methods

instance Show BytecodeProgram where
  show (BytecodeProgram nGlobals meths) = printf "globals size: %d bytes\n methods:\n\n%s" nGlobals (unlines $ map show meths)

data BytecodeMethod = BytecodeMethod Word8 Word8 [Bytecode] -- nlocals, argSize, body

instance Show BytecodeMethod where
  show (BytecodeMethod nLocals argBytes insns) = printf "=============\nargs size: %d\nlocals size: %d\n\n%s" argBytes nLocals (unlines $ map show insns)


data Bytecode = ImmediateByte Word8
              | Relocation Node -- for use during block ordering
              | STORE Type
              | STORE_n Type Int
                -- local variable load
              | LOAD Type
              | LOAD_n Type Int
                -- global variable store
              | GSTORE Type
              | GLOAD Type
                -- immediate
              | BCONST
              | BCONST_n Int8
              | SCONST
              | SCONST_n Int16
              | DUP Type
              | POP Type
              | SWAP Type
                -- Arithmetic
              | ADD      Type
              | SUBTRACT Type
              | MULTIPLY Type
              | DIVIDE   Type
              | MOD      Type
              | AND      Type
              | OR       Type
              | XOR      Type
              | NOT      Type
              | CMP      Type
              | LSHIFT   Type
              | RSHIFT   Type
                -- Type conversion
              | B2S
              | S2B
                -- control flow
              | COND CondOp
              | GOTO
              | NOP
              | CALL
              | RET Type
              | VMEXIT
                -- System calls:
              | SYSCALL SyscallOp
                deriving (Show, Eq)

-- see interpreter.h
bytecodeByte :: Bytecode -> Word8
bytecodeByte (ImmediateByte b)      = b
bytecodeByte (Relocation _)         = error "Stray relocation"
bytecodeByte (STORE Byte)           = 0
bytecodeByte (STORE Short)          = 5
bytecodeByte (STORE_n Byte i)       = 1 + (fromIntegral i)
bytecodeByte (STORE_n Short i)      = 6 + (fromIntegral i)
bytecodeByte (LOAD Byte)            = 10
bytecodeByte (LOAD Short)           = 15
bytecodeByte (LOAD_n Byte i)        = 11 + (fromIntegral i)
bytecodeByte (LOAD_n Short i)       = 16 + (fromIntegral i)
bytecodeByte (GSTORE Byte)          = 20
bytecodeByte (GSTORE Short)         = 22
bytecodeByte (GLOAD Byte)           = 21
bytecodeByte (GLOAD Short)          = 23
bytecodeByte (BCONST)               = 24
bytecodeByte (BCONST_n i)           = 25 + (fromIntegral i)
bytecodeByte (SCONST)               = 29
bytecodeByte (SCONST_n i)           = 30 + (fromIntegral i)
bytecodeByte (DUP Byte)             = 34
bytecodeByte (DUP Short)            = 35
bytecodeByte (POP Byte)             = 36
bytecodeByte (POP Short)            = 37
bytecodeByte (SWAP Byte)            = 38
bytecodeByte (SWAP Short)           = error "Short swap not implemented"
bytecodeByte (ADD      Byte)        = 48
bytecodeByte (SUBTRACT Byte)        = 49
bytecodeByte (MULTIPLY Byte)        = 50
bytecodeByte (DIVIDE   Byte)        = 51
bytecodeByte (MOD      Byte)        = 52
bytecodeByte (AND      Byte)        = 53
bytecodeByte (OR       Byte)        = 54
bytecodeByte (XOR      Byte)        = 55
bytecodeByte (NOT      Byte)        = 56
bytecodeByte (CMP      Byte)        = 57
bytecodeByte (LSHIFT   Byte)        = 58
bytecodeByte (RSHIFT   Byte)        = 59
bytecodeByte (ADD      Short)       = 60
bytecodeByte (SUBTRACT Short)       = 61
bytecodeByte (MULTIPLY Short)       = 62
bytecodeByte (DIVIDE   Short)       = 63
bytecodeByte (MOD      Short)       = 64
bytecodeByte (AND      Short)       = 65
bytecodeByte (OR       Short)       = 66
bytecodeByte (XOR      Short)       = 67
bytecodeByte (NOT      Short)       = 68
bytecodeByte (CMP      Short)       = 69
bytecodeByte (LSHIFT   Short)       = 70
bytecodeByte (RSHIFT   Short)       = 71
bytecodeByte (B2S)                  = 72
bytecodeByte (S2B)                  = 73
bytecodeByte (COND IfEq)            = 74
bytecodeByte (COND IfNe)            = 75
bytecodeByte (COND IfLt)            = 76
bytecodeByte (COND IfGt)            = 77
bytecodeByte (COND IfGe)            = 78
bytecodeByte (COND IfLe)            = 79
bytecodeByte (GOTO)                 = 80
bytecodeByte (NOP)                  = 81
bytecodeByte (CALL)                 = 82
bytecodeByte (RET Byte)             = 83
bytecodeByte (RET Short)            = 84
bytecodeByte (RET Void)             = 85
bytecodeByte (VMEXIT)               = 86

bytecodeByte (SYSCALL PressKey)     = 87
bytecodeByte (SYSCALL ReleaseKey)   = 88
bytecodeByte (SYSCALL CheckKey)     = 89
bytecodeByte (SYSCALL CheckPhysKey) = 90
bytecodeByte (SYSCALL WaitKey)      = 91
bytecodeByte (SYSCALL WaitPhysKey)  = 92

bytecodeByte (SYSCALL Delay)        = 95
bytecodeByte (SYSCALL GetUptimeMS)  = 96
bytecodeByte (SYSCALL GetUptime)    = 97
bytecodeByte (SYSCALL Buzz)         = 98
bytecodeByte (SYSCALL BuzzAt)       = 99
bytecodeByte (SYSCALL MoveMouse)           = 100
bytecodeByte (SYSCALL PressMouseButtons)   = 101
bytecodeByte (SYSCALL ReleaseMouseButtons) = 102


type VarAllocation = IntMap (Int, Type)

-- naively allocates variables to slots (todo: check for running out.)
allocateVarSlots :: [IRVariable] -> (VarAllocation, Word8)
allocateVarSlots idx = foldl addVar (IntMap.empty, 0) idx
  where
    addVar :: (VarAllocation, Word8) -> IRVariable -> (VarAllocation, Word8)
    addVar (map, nVars) (TVariable id typ) = ((IntMap.insert id (fromIntegral nVars, typ) map), (nVars + typeSize typ))
    typeSize Byte  = 1
    typeSize Short = 2

varLookup :: VarAllocation -> Int -> ThrowsError (Int, Type)
varLookup vs i = maybeToError (MapLookupError "VariableAllocation" (show i)) $ IntMap.lookup i vs

-- monad versions of ufold and gmap (note that different from definitions in D.G.I.Monad - correspond to foldM etc)

ufoldM :: (Graph gr, Monad m) => ((Context a b) -> c -> m c) -> c -> gr a b -> m c
-- ufoldM f acc g | isEmpty g = return acc
               -- | otherwise = f c acc >>= (\acc' -> ufoldM f acc' g')
                             -- where (c,g') = matchAny g

ufoldM f acc g | isEmpty g = return acc
               | otherwise = ufoldM f acc g' >>= (f c)
                             where (c,g') = matchAny g

gmapM :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
gmapM f gr = ufoldM (\ctx acc -> do { ctx' <- f ctx; return $ ctx' & acc }) empty gr

nmapM :: (DynGraph gr, Monad m) => (a -> m c) -> gr a b -> m (gr c b)
nmapM f = gmapM (\(p,v,l,s) -> do { l' <- f l; return (p, v, l', s) })

-- struct program { uint8_t nglobals; uint8_t nmethods; method methods[1]; }
-- struct method  { uint8_t nargs; uint8_t nlocals; uint16_t code_offset; }
-- would be sensible to make sure we don't overflow nmeths.
binaryProgram :: BytecodeProgram -> ByteString
binaryProgram (BytecodeProgram nGlobals meths) =
  let (index, programData) = calculateMethods 0 meths
  in (B.pack [nGlobals, fromIntegral $ length meths]) `B.append` (B.concat index) `B.append` (B.concat programData)
  where
    calculateMethods ::  Word16 -> [BytecodeMethod] -> ([ByteString], [ByteString])
    calculateMethods pos [] = ([], [])
    calculateMethods pos ((BytecodeMethod nLocals argSize body):rest) =
      let l = fromIntegral $ length body
          offsetL = lowByte $ fromIntegral pos
          offsetH = highByte $ fromIntegral pos
          (restidx, restbody) = calculateMethods (pos + l) rest
      in
      (B.pack [argSize, nLocals, offsetL, offsetH] : restidx, B.pack (map bytecodeByte body) : restbody)



outputProgram :: IRProgram IRInstruction -> ThrowsError BytecodeProgram
outputProgram (IRProgram meths vars) = do
  let (globalMap, nGlobals) = allocateVarSlots (indexElems vars)
  methods <- mapM (outputMethod globalMap) (indexElems meths)
  return $ BytecodeProgram nGlobals methods

outputMethod :: VarAllocation -> IRMethod IRInstruction -> ThrowsError BytecodeMethod
outputMethod globalMap (IRMethod id aTypes graph vars) = do
  let (localMap, nLocals) = allocateVarSlots (indexElems vars) -- improve later with liveness analysis + graph coloring
  bcGraph <- nmapM (outputBlock globalMap localMap) graph
  -- now traverse the graph building up the code as we go
  bytecodes <- scheduleBlocks bcGraph
  -- let dummy = concat $ map (\(_, IRBlock b) -> b) $ labNodes bcGraph
  return $ BytecodeMethod nLocals (argLength aTypes) bytecodes
    where
      argLength = sum.(map sizeOf)
      sizeOf Byte  = 1
      sizeOf Short = 2

scheduleBlocks :: IRGraph Bytecode -> ThrowsError [Bytecode]
scheduleBlocks graph = do
  let nodeOrder = orderBlocks graph
      (placement, instructions) = scheduleBlocks' nodeOrder IntMap.empty 0 graph
  return $ handleRelocations placement 0 instructions
  where
    -- place blocks from the argument list, recording their placement and adding links:
    scheduleBlocks' :: [Node] -> (IntMap Int) -> Int -> IRGraph Bytecode -> (IntMap Int, [Bytecode])
    scheduleBlocks' [] placement _ _ = (placement, [])
    scheduleBlocks' (node:rest) placement pos graph =
        let IRBlock bytecodes = fromJust $ lab graph node
            sucs = lsucSort $ lsuc graph node
            linkages = map (linkage rest) sucs
            blockData = concat (bytecodes : linkages)
            blockLength = length blockData
            (placement', restData) = scheduleBlocks' rest (IntMap.insert node pos placement) (pos + blockLength) graph
        in (placement', blockData ++ restData)
    -- for DefaultEdge, if it's the next block, no link otherwise GOTO
    -- for ConditionalEdge, a conditional jump
    linkage :: [Node] -> (Node, IREdge) -> [Bytecode]
    linkage next (n, DefaultEdge) = if (next /= [] && head next == n) then [] else [GOTO, Relocation n, NOP]
    linkage _ (n, ConditionalEdge op) = [COND op, Relocation n, NOP]
    -- given a map of absolute positions, fill out relocation records
    handleRelocations :: (IntMap Int) -> Int -> [Bytecode] -> [Bytecode]
    handleRelocations _ _ [] = []
    handleRelocations placement pos ((Relocation n):NOP:rest) =
      let targetPos = fromJust $ IntMap.lookup n placement
          offset = fromIntegral $ targetPos - (pos - 1) -- from the jump instruction, not the reloc itself
      in (ImmediateByte $ lowByte offset) : (ImmediateByte $ highByte offset) : handleRelocations placement (pos + 2) rest
    handleRelocations placement pos (h:rest) = h : handleRelocations placement (pos+1) rest

-- DFS always choosing default edges first - Not an ideal ordering, but fine for now
orderBlocks :: IRGraph a -> [Node]
orderBlocks = xdfsWith edgeSelFn node' [1]
  where
    edgeSelFn :: CFun (IRBlock a) IREdge [Node]
    edgeSelFn = (map fst) . lsucSort . lsuc'

-- sort labelled nodes by label
lsucSort :: Ord a => [(Node, a)] -> [(Node, a)]
lsucSort = sortBy (\(_, a) (_, b) -> compare a b)


outputBlock :: VarAllocation -> VarAllocation -> IRBlock IRInstruction -> ThrowsError (IRBlock Bytecode)
outputBlock gVars lVars (IRBlock instructions) = do
  bytecodes <- fmap reverse $ foldM (outputInstruction gVars lVars) [] instructions
  return $ IRBlock bytecodes

outputInstruction :: VarAllocation -> VarAllocation -> [Bytecode] -> IRInstruction -> ThrowsError [Bytecode]

outputInstruction _ lVars rest (IRStore id) = do
  (varSlot, varType) <- varLookup lVars id
  return $ if varSlot < 4
           then (STORE_n varType varSlot) : rest
           else (ImmediateByte $ fromIntegral varSlot) : (STORE varType) : rest

outputInstruction _ lVars rest (IRLoad id) = do
  (varSlot, varType) <- varLookup lVars id
  return $ if varSlot < 4
           then (LOAD_n varType varSlot) : rest
           else (ImmediateByte $ fromIntegral varSlot) : (LOAD varType) : rest

outputInstruction gVars _ rest (IRGLoad id) = do
  (varSlot, varType) <- varLookup gVars id
  return $ (ImmediateByte $ fromIntegral varSlot) : (GLOAD varType) : rest

outputInstruction gVars _ rest (IRGStore id) = do
  (varSlot, varType) <- varLookup gVars id
  return $ (ImmediateByte $ fromIntegral varSlot) : (GLOAD varType) : rest

outputInstruction _ _ rest (IRBConst b) = return $ if b >= 0 && b < 4
                                                   then (BCONST_n b) : rest
                                                   else (ImmediateByte $ byte b) : BCONST : rest

outputInstruction _ _ rest (IRSConst s) = return $ if s >= 0 && s < 4
                                                   then (SCONST_n s) : rest
                                                   else (ImmediateByte (highByte s)) : (ImmediateByte (lowByte s)) : SCONST : rest

outputInstruction _ _ rest (IRDup typ)  = return $ (DUP typ) : rest
outputInstruction _ _ rest (IRPop typ)  = return $ (POP typ) : rest
outputInstruction _ _ rest (IRSwap typ) = return $ (SWAP typ) : rest

outputInstruction _ _ rest (IRArith typ IRAdd)        = return $ (ADD typ) : rest
outputInstruction _ _ rest (IRArith typ IRSubtract)   = return $ (SUBTRACT typ) : rest
outputInstruction _ _ rest (IRArith typ IRMultiply)   = return $ (MULTIPLY typ) : rest
outputInstruction _ _ rest (IRArith typ IRDivide)     = return $ (DIVIDE typ) : rest
outputInstruction _ _ rest (IRArith typ IRMod)        = return $ (MOD typ) : rest
outputInstruction _ _ rest (IRArith typ IRAnd)        = return $ (AND typ) : rest
outputInstruction _ _ rest (IRArith typ IROr)         = return $ (OR typ) : rest
outputInstruction _ _ rest (IRArith typ IRXor)        = return $ (XOR typ) : rest
outputInstruction _ _ rest (IRArith typ IRComplement) = return $ (NOT typ) : rest
outputInstruction _ _ rest (IRArith typ IRCmp)        = return $ (CMP typ) : rest
outputInstruction _ _ rest (IRArith typ IRLshift)     = return $ (LSHIFT typ) : rest
outputInstruction _ _ rest (IRArith typ IRRshift)     = return $ (RSHIFT typ) : rest

outputInstruction _ _ rest IRB2S = return $ B2S : rest
outputInstruction _ _ rest IRS2B = return $ S2B : rest

outputInstruction _ _ rest (IRCall id) = return $ (ImmediateByte $ fromIntegral id) : CALL : rest

outputInstruction _ _ rest (IRSyscall op) = return $ (SYSCALL op) : rest

outputInstruction _ _ rest (IRReturn typ) = return $ (RET typ) : rest

outputInstruction _ _ rest IRExit = return $ VMEXIT : rest

outputInstruction _ _ rest (IRDummy _) = return rest

byte :: Int8 -> Word8
byte b = fromIntegral $ b -- "preserves representation not sign"
lowByte :: Int16 -> Word8
lowByte s = fromIntegral $ s .&. 0xFF
highByte :: Int16 -> Word8
highByte s = fromIntegral $ (s `shiftR` 8) .&. 0xFF
