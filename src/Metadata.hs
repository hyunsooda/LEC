{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Metadata where

import Type
import CFG
import Util

import System.Demangle.Pure (demangle)
import Data.ByteString.Short (ShortByteString)
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Word (Word16, Word32)
import Data.Maybe (isJust, fromJust)
import Data.Text.Lazy (unpack)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Monad.State hiding (void)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Operand as AST hiding (PointerType)

import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

addDemangledFnStr :: MonadState StateMap m => AST.Definition -> m ()
addDemangledFnStr (AST.GlobalDefinition (G.Function {..})) = do
  dm <- gets demangledFuncMap
  -- Sometimes, type string may differ, while keeping the semantic
  -- e.g., `const int` and `int const`
  -- Thus, the key is sorted name of demangled 
  modify $ \sm -> sm { demangledFuncMap = M.insert (sort demangled) name dm }
    where 
      fnNm = getName name
      demangled = case demangle fnNm of
                    Just d -> d
                    Nothing -> fnNm

addDemangledFnStr _ = pure ()

getDemangledFnStr :: MonadState StateMap m => String -> m (Maybe AST.Name)
getDemangledFnStr nm = do
  dm <- gets demangledFuncMap
  pure $ M.lookup (sort nm) dm 

findMapCountFns :: MonadState StateMap m => m ([AST.Name])
findMapCountFns = do
  dm <- gets demangledFuncMap
  pure . M.elems $ M.filter isCountFn dm
    where
      isCountFn nm = 
        case demangle . getName $ nm of
          Just demangled -> isMapCountFn demangled
          Nothing -> False
      isMapCountFn fnNm =
        "std::map<" `isPrefixOf` fnNm && "::count(" `isInfixOf` fnNm

findMapTmplFn :: MonadState StateMap m => String -> m ([AST.Name])
findMapTmplFn targetFnNm = do
  dm <- gets demangledFuncMap
  pure . M.elems $ M.filter isAccessFn dm
    where
      isAccessFn nm = 
        case demangle . getName $ nm of
          Just demangled -> strCmp demangled
          Nothing -> False
      strCmp fnFullNm =
        "std::map<" `isPrefixOf` fnFullNm && ("::" ++ targetFnNm) `isInfixOf` fnFullNm

findMapAccessFnStrs :: MonadState StateMap m => m ([AST.Name])
findMapAccessFnStrs = findMapTmplFn "operator[]"

getOperandName :: AST.Operand -> Maybe AST.Name
getOperandName (AST.LocalReference _ varName) = Just varName
getOperandName (AST.ConstantOperand (C.GlobalReference varName)) = Just varName
getOperandName _ = Nothing

getFuncName :: AST.CallableOperand -> Maybe String
getFuncName =
  \case
    Right (AST.ConstantOperand (C.GlobalReference funcName)) -> Just . getName $ funcName
    _ -> Nothing

getFileName :: MonadState StateMap m => Maybe (AST.MDRef a) -> m String
getFileName (Just mdRef) = do
  md <- getMD mdRef
  pure $ fileName md
  where
    fileName (AST.DINode (AST.DIScope (AST.DIFile (AST.File {..})))) = show filename

getVarSource ::
  (MonadState StateMap m, Num c) => AST.Name -> m (String, String, c)
getVarSource targetVarName = do
  sourceInfo <- gets sourceMap
  tm <- gets taintMap
  let sourceVar = M.filterWithKey findSourceVar tm

  if not . null $ sourceVar then
    let (sourceVarName, _) =  M.elemAt 0 sourceVar
        source = (M.!) sourceInfo sourceVarName 
     in
    getSourceInfo source
  else
    let source = fromJust $ M.lookup targetVarName sourceInfo 
     in
    getSourceInfo source

  where 
    getSourceInfo (AST.DILocalVariable (AST.LocalVariable {..})) = do
      fileName' <- getFileName file
      pure (show name, fileName', fromIntegral line)
    findSourceVar var referVars =
      let sourceVar = filter (== targetVarName) referVars in
          case length sourceVar of
            0 -> if var == targetVarName then True else False
            1 -> True
            _ -> error "error `findSourceVar`"

getDemangledFuncName :: AST.CallableOperand -> String
getDemangledFuncName func =
  case getFuncName func of
    Just name ->
      case demangle name of
        Just demangled -> demangled
        _              -> name
    _ -> error "Failed to get demangled function name: "

getMDNodeID :: AST.MDRef a -> AST.MetadataNodeID
getMDNodeID (AST.MDRef id) = id

getMD :: MonadState StateMap m => AST.MDRef a -> m AST.MDNode
getMD mdRef = do
  md <- gets debugMap
  pure $ (M.!) md $ getMDNodeID mdRef

getMDTyp :: AST.MDNode -> AST.DIType
getMDTyp (AST.DINode (AST.DIScope (AST.DIType typ))) = typ

unpackDIType :: MonadState StateMap m => AST.DIType -> m String
unpackDIType (AST.DIBasicType AST.BasicType {..}) = pure . show $ name
unpackDIType (AST.DICompositeType AST.DIClassType {..}) = pure . show $ name
unpackDIType (AST.DICompositeType AST.DIEnumerationType {..}) = pure . show $ name
unpackDIType (AST.DICompositeType AST.DIStructureType {..}) = pure . show $ name
unpackDIType (AST.DICompositeType AST.DIUnionType {..}) = pure . show $ name
unpackDIType (AST.DICompositeType AST.DIArrayType {..}) = do
  md <- getMD $ fromJust elementTy
  unpackDIType . getMDTyp $ md
unpackDIType (AST.DIDerivedType AST.DerivedType {..}) = do
  md <- getMD $ fromJust baseType
  unpackDIType . getMDTyp $ md

getFnTyps :: MonadState StateMap m => G.Global -> m (String, [String])
getFnTyps AST.Function {..} = do
  md <- getMD $ firstMD metadata
  getTypList md
    where
      firstMD ((_, mdNode):_) = mdNode
      getTypList (AST.DINode (AST.DIScope (AST.DILocalScope (AST.DISubprogram (AST.Subprogram {..}))))) = do
        md <- getMD $ fromJust type'
        fnTypMDs <- mapM getMD $ fnTypList md
        let typs = map getMDTyp fnTypMDs
        unpacked <- mapM unpackDIType typs
        pure (head unpacked, tail unpacked)
          where
            fnTypList (AST.DINode (AST.DIScope (AST.DIType (AST.DISubroutineType (AST.SubroutineType {..}))))) =
              let typList = filter isJust typeArray
               in 
              map fromJust typList

getMDLocation :: AST.MDNode -> (Word32, Word16, AST.MDRef AST.DILocalScope)
getMDLocation (AST.DILocation (AST.Location {..})) = (line, column, scope)

getVarName :: (AST.Operand, a) -> AST.Name
getVarName (AST.MetadataOperand (AST.MDValue (AST.LocalReference _ varName)), _) = varName

getVarInfo :: MonadState StateMap m => (AST.Operand, a) -> m AST.DINode
getVarInfo (AST.MetadataOperand (AST.MDNode mdRef), _) = do
  md <- getMD mdRef
  pure $ diVar md
    where diVar (AST.DINode var) = var

addVarInfo :: MonadState StateMap m => AST.Name -> AST.DINode -> m ()
addVarInfo name (AST.DIVariable var) = do
  modify $ \sm -> sm { sourceMap = M.insert name var (sourceMap sm) }

getMDFuncFileNames :: MonadState StateMap m => AST.MDNode -> m (String, String)
getMDFuncFileNames (AST.DINode (AST.DIScope (AST.DILocalScope (AST.DISubprogram (AST.Subprogram {..}))))) = do
  fileName' <- getFileName file
  pure (fileName', funcName)
    where funcName = show name

getMDScope :: (MonadState StateMap m) => [(a1, AST.MDRef a2)] -> m (String, String, Integer, Integer)
getMDScope [(_, AST.MDRef id)] = do
  md <- gets debugMap
  let (line, col, mdRef) = getMDLocation $ (M.!) md id
      mdScope = (M.!) md $ getMDNodeID mdRef
   in do
   (fileName, funcName) <- getMDFuncFileNames mdScope
   pure (fileName, funcName, fromIntegral line, fromIntegral col)
