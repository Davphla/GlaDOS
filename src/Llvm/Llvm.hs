{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Llvm.Llvm (compileModuleToObj) where
import LLVM.AST as LLAST
    ( defaultModule,
      functionDefaults,
      Definition(GlobalDefinition),
      BasicBlock(BasicBlock),
      Parameter(Parameter),
      Operand(ConstantOperand, LocalReference), Name (UnName), Module, moduleName, moduleDefinitions )
import LLVM.AST.Global as Global
    ( Global(name, returnType, parameters, basicBlocks) )
import LLVM.AST.Type as Type ( i32, i1 )
import LLVM.AST.Name as Name ( Name(Name) )
import LLVM.AST.Visibility as Visibility ()
import LLVM.AST.AddrSpace as AddrSpace ()
import LLVM.AST.FloatingPointPredicate as FloatingPointPredicate ()
import LLVM.AST.IntegerPredicate as IntegerPredicate ()
import LLVM.AST.Constant as Constant ( Constant(Int) )
import LLVM.AST.DataLayout as DataLayout ()
import LLVM.AST.Attribute as Attribute ()
import LLVM.AST.ParameterAttribute as ParameterAttribute ()
import LLVM.Target as Target ()
import LLVM.Context as Context (withContext)

import LLVM.Internal.Module as Module (withModuleFromAST, writeLLVMAssemblyToFile, File (File))

import LLVM.Internal.OrcJIT.LinkingLayer as LinkingLayer ()

import qualified Data.ByteString.Char8 as C
import LLVM.Module as LModule ()

import Ast.Ast (Ast(..))
import Data.ByteString.Short (toShort)
import LLVM.IRBuilder.Module ()
import LLVM.AST.Instruction
    ( Named(Do, (:=)),
      Instruction(Select, Add, Sub, Mul, SDiv, SRem, ICmp),
      Terminator(Ret) )
import qualified LLVM.AST.IntegerPredicate as IPred


s2n :: String -> Name.Name
s2n s = Name.Name (toShort $ C.pack s)

defAdd :: Definition
defAdd =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "add",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Add False False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defSub :: Definition
defSub =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "sub",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Sub False False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defMul :: Definition
defMul =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "mul",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Mul False False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defDiv :: Definition
defDiv =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "div",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := SDiv False (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defMod :: Definition
defMod =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "mod",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := SRem (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 0))) [])
    ]
  }

defIf :: Definition
defIf =  GlobalDefinition
  functionDefaults {
    Global.name = s2n "if",
    Global.returnType = Type.i32,
    Global.parameters = ([Parameter Type.i1 (s2n "cond") [], Parameter Type.i32 (s2n "a") [], Parameter Type.i32 (s2n "b") []], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := ICmp IPred.EQ (LocalReference Type.i1 (s2n "cond")) (ConstantOperand $ Constant.Int 1 1) [],
        UnName 1 := Select (LocalReference Type.i1 (UnName 0)) (LocalReference Type.i32 (s2n "a")) (LocalReference Type.i32 (s2n "b")) []
      ] (Do $ Ret (Just (LocalReference Type.i32 (UnName 1))) [])
    ]
  }

defMain :: Definition
defMain = GlobalDefinition
  functionDefaults {
    Global.name = s2n "main",
    Global.returnType = Type.i32,
    Global.parameters = ([], False),
    Global.basicBlocks = [
      BasicBlock (s2n "entry") [
        UnName 0 := Add False False (ConstantOperand $ Constant.Int 32 1) (ConstantOperand $ Constant.Int 32 2) [],
        UnName 1 := Sub False False (ConstantOperand $ Constant.Int 32 3) (ConstantOperand $ Constant.Int 32 4) [],
        UnName 2 := Mul False False (ConstantOperand $ Constant.Int 32 5) (ConstantOperand $ Constant.Int 32 6) [],
        UnName 3 := SDiv False (ConstantOperand $ Constant.Int 32 7) (ConstantOperand $ Constant.Int 32 8) []        
    ] (Do $ Ret (Just (ConstantOperand $ Constant.Int 32 0)) [])
    ]
  }


generateModule :: [Ast] -> LLAST.Module
generateModule _ = defaultModule {
  moduleName = toShort $ C.pack "glados",
  moduleDefinitions = [defAdd, defSub, defMul, defDiv, defMod, defIf, defMain]}


compileModuleToObj :: [Ast] -> IO ()
compileModuleToObj a = withContext $ \context ->
     withModuleFromAST context (generateModule a)  $ \p ->
        writeLLVMAssemblyToFile (File "glados.ll") p