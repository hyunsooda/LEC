{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Emit where

import Type
import Metadata
import Util
import Global
import Operand

import System.Demangle.Pure (demangle)
import Data.Maybe (isJust, fromJust)
import Data.List (isInfixOf, findIndex)
import Control.Monad.State hiding (void)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G

import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

defineGlobalStr :: (MonadIRBuilder m, MonadModuleBuilder m) => String -> String -> m C.Constant
defineGlobalStr varName value = LLVM.globalStringPtr varName (AST.mkName value)

outOfBoundErrAssertFmt :: IRBuilderT Env ()
outOfBoundErrAssertFmt =
  let oobFormatter = [mtstr|%sFound out of bound access: [%s:%d:%d]%s
    array length: %d, indexed by: %d
    variable name: %s, allocated at: %d
|] in do
  defineGlobalStr oobFormatter outOfBoundAssertFmtNm >> pure ()

mapKeyExistAssertFmt :: IRBuilderT Env ()
mapKeyExistAssertFmt =
  let oobFormatter = [mtstr|%sFound unitialized key access: [%s:%d:%d]%s
    variable name: %s, allocated at: %d
|] in do
  defineGlobalStr oobFormatter mapUninitAccessAssertFmtNm >> pure ()

emitGlobalAnsiStr :: (MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitGlobalAnsiStr = do
  defineGlobalStr "B[37m" ansiWhite >>
    defineGlobalStr "B[31m" ansiRed >>
      pure ()

emitLibcExit :: MonadModuleBuilder m => m AST.Operand
emitLibcExit = LLVM.extern libcExitNm [AST.i32] AST.void

emitLibcPrintf :: MonadModuleBuilder m => m AST.Operand
emitLibcPrintf = LLVM.extern libcPrintfNm [AST.i32] AST.void

voidRetTyp :: AST.Type
voidRetTyp = AST.FunctionType AST.void [] False

i64RetTyp :: AST.Type
i64RetTyp = AST.FunctionType AST.i64 [] False

ptrRetTyp :: AST.Type
ptrRetTyp = AST.FunctionType AST.ptr [] False

libcExit :: AST.Operand
libcExit = AST.ConstantOperand (C.GlobalReference libcExitNm)

libcPrintf :: AST.Operand
libcPrintf = AST.ConstantOperand (C.GlobalReference libcPrintfNm)

getLocalOperandName :: AST.Operand -> AST.Name
getLocalOperandName (AST.LocalReference _ varName) = varName

makeInt32Operand :: Integer -> AST.Operand
makeInt32Operand n = AST.ConstantOperand (C.Int 32 n)

makeInt64Operand :: Integer -> AST.Operand
makeInt64Operand n = AST.ConstantOperand (C.Int 64 n)

getVoidOperand :: AST.Operand
getVoidOperand = AST.ConstantOperand (C.Null AST.void)

callMapUninitChecker
  :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    AST.Operand
    -> AST.Operand
    -> [(a1, AST.MDRef a2)]
    -> m (AST.Named AST.Instruction)
callMapUninitChecker mapObj key metadata = do
  (varName, _, allocatedLine) <- getVarSource (fromJust . getOperandName $ mapObj)
  (accessFileName, _, accessLine, accessCol) <- getMDScope metadata
  let accessLine' = makeInt32Operand accessLine
      accessCol' = makeInt32Operand accessCol
      allocatedLine' = makeInt32Operand allocatedLine
   in do
     accessFileName' <- LLVM.globalStringPtr accessFileName reportFileNm
     varName' <- LLVM.globalStringPtr varName reportVarNm
     pure $ AST.Do
       AST.Call {
        tailCallKind = Nothing,
        AST.callingConvention = AST.C,
        AST.returnAttributes = [],
        AST.type' = ptrRetTyp,
        AST.function = Right (AST.ConstantOperand (C.GlobalReference mapUninitCheckerFnNm)),
        AST.arguments = [
          (mapObj, []), (key, []),
          (accessLine', []), (accessCol', []),
          (AST.ConstantOperand accessFileName', []),
          (AST.ConstantOperand varName', []), (allocatedLine', [])
        ],
        AST.functionAttributes = [],
        AST.metadata = []
      }

callBoundAssert
  :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    Integer
    -> Integer
    -> AST.Name
    -> [(a1, AST.MDRef a2)]
    -> m (AST.Named AST.Instruction)
callBoundAssert arrSize idx targetAddrName metadata = do
  (varName, _, allocatedLine) <- getVarSource targetAddrName
  (accessFileName, _, accessLine, accessCol) <- getMDScope metadata
  let arrSize' = makeInt32Operand arrSize
      idx' = makeInt32Operand idx
      accessLine' = makeInt32Operand accessLine
      accessCol' = makeInt32Operand accessCol
      allocatedLine' = makeInt32Operand allocatedLine
   in do
     accessFileName' <- LLVM.globalStringPtr accessFileName reportFileNm
     varName' <- LLVM.globalStringPtr varName reportVarNm
     pure $ AST.Do
       AST.Call {
        tailCallKind = Nothing,
        AST.callingConvention = AST.C,
        AST.returnAttributes = [],
        AST.type' = voidRetTyp,
        AST.function = Right (AST.ConstantOperand (C.GlobalReference oobCheckerFnNm)),
        AST.arguments = [
          (arrSize', []), (idx', []), (accessLine', []), (accessCol', []),
          (AST.ConstantOperand accessFileName', []),
          (AST.ConstantOperand varName', []), (allocatedLine', [])
        ],
        AST.functionAttributes = [],
        AST.metadata = []
      }

defUMAChecker :: AST.Global -> IRBuilderT Env AST.Definition
defUMAChecker f@(AST.Function { .. }) = do
  let bbIdx = fromJust . findBBIdxEMplaceHintUniq $ basicBlocks
      panicBB = replaceBB2Panic (basicBlocks !! bbIdx)
      bbs = map filterDbgInstr basicBlocks
      (bbs1, _:bbs2) = splitAt bbIdx bbs
      newBBs = bbs1 ++ [panicBB] ++ bbs2
      newFn = AST.GlobalDefinition $ f {
        G.name = AST.mkName "LEC_uninitMapAccessChecker"
      , G.parameters = ([
          (fst parameters) !! 0
        , (fst parameters) !! 1
        , AST.Parameter AST.ptr (AST.mkName "line") []
        , AST.Parameter AST.ptr (AST.mkName "col") []
        , AST.Parameter AST.ptr (AST.mkName "fileName") []
        , AST.Parameter AST.ptr (AST.mkName "varName") []
        , AST.Parameter AST.ptr (AST.mkName "keyName") []
        ], False)
      , G.basicBlocks = newBBs
      , G.metadata = []
      }
   in
   pure newFn
     where
      findBBIdxEMplaceHintUniq bbs = find bbs 0 Nothing
      find ((AST.BasicBlock name instrs term) : residual) idx Nothing =
        case findIndex aux instrs of
          Just _ -> Just idx
          _ -> find residual (idx + 1) Nothing

      aux (_ AST.:= AST.Call {..}) =
        let fnNm = fromJust . getFuncName $ function
            demangled = fromJust . demangle $ fnNm
         in
        "_M_emplace_hint_unique" `isInfixOf` demangled
      aux _ = False

      replaceBB2Panic (AST.BasicBlock name _ _) =
        AST.BasicBlock name [reportCall, exitCall] term
        where
          reportCall = AST.Do AST.Call {
            AST.tailCallKind = Nothing
          , AST.callingConvention = AST.C
          , AST.returnAttributes = []
          , AST.type' = voidRetTyp
          , AST.function = Right libcPrintf
          , AST.arguments =
            [ (AST.ConstantOperand (C.GlobalReference (AST.Name "MAP_UNINIT_ACCESS_ASSERT_FMT")), [])
            , (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk")), [])
            , (AST.LocalReference AST.ptr (AST.mkName "fileName"), [])
            , (AST.LocalReference AST.ptr (AST.mkName "line"), [])
            , (AST.LocalReference AST.ptr (AST.mkName "col"), [])
            , (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk")), [])
            , (AST.LocalReference AST.ptr (AST.mkName "varName"), [])
            , (AST.LocalReference AST.ptr (AST.mkName "keyName"), [])
            ]
          , AST.functionAttributes = []
          , AST.metadata = []
          }
          exitCall = AST.Do AST.Call {
            AST.tailCallKind = Nothing
          , AST.callingConvention = AST.C
          , AST.returnAttributes = []
          , AST.type' = voidRetTyp
          , AST.function = Right libcExit
          , AST.arguments = [(makeInt32Operand 1, [])]
          , AST.functionAttributes = []
          , AST.metadata = []
          }
          term = AST.Do AST.Ret {
            AST.returnOperand = Just $ AST.ConstantOperand (C.Null AST.ptr)
          , AST.metadata' = []
          }

defBoundChecker :: IRBuilderT Env AST.Operand
defBoundChecker = mdo
  LLVM.function oobCheckerFnNm [arrSize, idx, line, col, fileName, varName, allocatedLine] AST.void body
    where
      arrSize = (AST.i32, LLVM.ParameterName "arrSize")
      idx = (AST.i32, LLVM.ParameterName "idx")
      line = (AST.i32, LLVM.ParameterName "line")
      col = (AST.i32, LLVM.ParameterName "col")
      fileName = (AST.ptr, LLVM.ParameterName "fileName")
      varName = (AST.ptr, LLVM.ParameterName "varName")
      allocatedLine = (AST.ptr, LLVM.ParameterName "allocatedLine")

      body [arrSize', idx', line', col', fileName', varName', allocatedLine'] =  mdo
        outOfBoundAccess <- LLVM.icmp AST.SGT idx' arrSize'
        LLVM.condBr outOfBoundAccess panic ret
        panic <- LLVM.block `LLVM.named` "panic"
        _ <- LLVM.call voidRetTyp libcPrintf [
          (AST.ConstantOperand (C.GlobalReference (AST.mkName outOfBoundAssertFmtNm)), []),
          (AST.ConstantOperand (C.GlobalReference (AST.mkName ansiRed)), []),
          (AST.LocalReference AST.ptr (getLocalOperandName fileName'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName line'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName col'), []),
          (AST.ConstantOperand (C.GlobalReference (AST.mkName ansiWhite)), []),
          (AST.LocalReference AST.ptr (getLocalOperandName arrSize'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName idx'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName varName'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName allocatedLine'), [])]
        _ <- LLVM.call voidRetTyp libcExit [(makeInt32Operand 1, [])]
        ret <- LLVM.block `LLVM.named` "ret"
        pure ()

