{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, GADTs #-}
module LibKeyc where

import Parser
import TypedAST
import BlockIR
import Indexes
import Output
import Errors

import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Maybe(fromMaybe)
import Data.List
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as BU

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

compile' :: String -> IO (Either CompileError B.ByteString)
compile' sourcePath = do
  source   <- readFile sourcePath
  return $ do
         ast      <- parseProgram source >>= buildTProgram
         optast   <- optTProgram ast
         ir       <- buildIR optast
         bytecode <- outputProgram ir
         return $ binaryProgram bytecode

copyChunks :: B.ByteString -> IO CStringLen
copyChunks bs = do
  buffer <- mallocBytes (fromIntegral . B.length $ bs)
  go (B.toChunks bs) buffer 0
  where go []    buffer offset  = return (buffer, offset)
        go (x:xs) buffer offset = do
          len <- BU.unsafeUseAsCStringLen x $ \(rawcode, codelen) -> do
            copyBytes (buffer `plusPtr` offset) rawcode codelen
            return codelen
          go xs buffer (offset + len)

data Result where
     Error     :: CompileError -> Result
     Exception :: forall e . Exception e => e -> Result
     Success   :: B.ByteString -> Result

writeString :: String -> Ptr CSize -> Ptr CString -> IO ()
writeString s outlen outbuf = do
  (buf, len) <- newCStringLen s
  poke outlen (fromIntegral len)
  poke outbuf buf

compile :: CString
        -> Ptr CSize -> Ptr CString
        -> Ptr CSize -> Ptr CString
        -> IO Int
compile path codelen codebuf errlen errbuf  = do
  result <- do path'  <- peekCString path
               (either Error Success <$>
                 (evaluate <=< compile') path')
              `catch` (\(e :: SomeException) ->
                return $ Exception e)
  case result of
       Success code -> do
         (buf, len) <- copyChunks code
         poke codelen (fromIntegral len)
         poke codebuf buf
         return 0
       Error err -> do
         writeString (show err) errlen errbuf
         return 1
       Exception ex -> do
         writeString (displayException ex) errlen errbuf
         return 1

foreign export ccall compile :: CString
        -> Ptr CSize -> Ptr CString
        -> Ptr CSize -> Ptr CString
        -> IO Int
