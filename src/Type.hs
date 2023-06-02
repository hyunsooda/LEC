module Type where

import Control.Monad.RWS hiding (void)
import qualified Data.Map as M
import Data.ByteString.Short (ShortByteString)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Operand as AST
import qualified LLVM.AST.Global as AST

import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM


data StateMap = StateMap { intMap :: M.Map AST.Name Integer
                         , debugMap :: M.Map AST.MetadataNodeID AST.MDNode
                         , sourceMap :: M.Map AST.Name AST.DIVariable
                         , taintMap :: M.Map AST.Name [AST.Name]
                         , codeMap :: M.Map AST.Name AST.Global -- <function name, Global.Function>
                         , demangledFuncMap :: M.Map String AST.Name -- <demangled function name, mangled function name>
                         } deriving Show

type PassInput = [String]
type PassState = StateMap
type PassOutput = [String]
type Env = RWST PassInput PassOutput PassState ModuleBuilder
type Pass a = AST.Module -> Bool -> IRBuilderT Env a
