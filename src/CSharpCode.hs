module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                                                 -- Class
type M = Environment -> (Code, Environment)                   -- Member
type S = Environment -> (Code, Environment)                   -- Statement
type E = Environment -> ValueOrAddress -> (Code, Environment) -- Expression

-- The environment that stores local variables and global variables in Maps.
-- nextLocal and nextGlobal hold the index of the next variable that might be declared. 
data Environment = Environment {
  locals :: M.Map String Int
  , nextLocal :: Int
  , globals :: M.Map String Int
  , nextGlobal :: Int
}


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass

  fMembDecl
  fMembMeth

  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock

  fExprConstInt
  fExprConstChar
  fExprConstBool
  fExprVar
  fExprOp
  fExprCall

-- Helper functions
combine :: (Code, Environment) -> (Code, Environment) -> (Code, Environment)
combine (c1, e1) (c2, e2) = (c1 ++ c2, e2)

addCode :: (Code, Environment) -> Code -> (Code, Environment)
addCode (c1, e) c2 = (c1 ++ c2, e)

emptyEnvironment :: Environment
emptyEnvironment = Environment { locals = M.empty, nextLocal = 1, globals = M.empty, nextGlobal = 0 } -- 1 to reserve space for return address (0)

addParam :: Environment -> Int -> String -> Environment
addParam Environment{locals = loc, nextLocal = nextLoc, globals = glob, nextGlobal = nextGlob} offset name = Environment { locals = M.insert name offset loc, nextLocal = nextLoc, globals = glob, nextGlobal = nextGlob }

addVar :: Environment -> String -> Environment
addVar Environment{locals = loc, nextLocal = nextLoc, globals = glob, nextGlobal = nextGlob} name = Environment { locals = M.insert name nextLoc loc, nextLocal = nextLoc + 1, globals = glob, nextGlobal = nextGlob }

addGlobal :: Environment -> String -> Environment
addGlobal Environment{locals = loc, nextLocal = nextLoc, globals = glob, nextGlobal = nextGlob} name = Environment { locals = loc, nextLocal = nextLoc, globals =  M.insert name nextGlob glob, nextGlobal = nextGlob + 1 }


fClass :: String -> [M] -> C
fClass c ms = spaceForGlobals ++ [LDR MP, Bsr "main", HALT] ++ code
  where 
    spaceForGlobals = replicate (length ms) NOP           -- reserve space for globals (members of the global class)
    code = concat (map (\m -> fst (m env)) ms)            -- actual code
    env = foldl (\e m -> (snd (m e))) emptyEnvironment ms -- add all member declarations as global variables


fMembDecl :: Decl -> M
fMembDecl (Decl _ name) env = ([], addGlobal env name)

fMembMeth :: Type -> String -> [Decl] -> S -> M
fMembMeth t x ps statement env = ([LABEL x, LDRR MP SP] ++ fst (statement paramEnv) ++ [LDRR SP MP, RET], env)
  where
    paramEnv = snd $ foldl (\(n, env') (Decl _ name) -> (n + 1, addParam env' (n - length ps) name)) (-1, env) ps


fStatDecl :: Decl -> S
fStatDecl (Decl _ name) env = ([LDC 0], addVar env name)

fStatExpr :: E -> S
fStatExpr e env = addCode (e env Value) [pop]

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ c1 ++ [BRA n2] ++ c2, env) -- return the old environment, because variables declared within if block are out of scope
    where
        (c, env')  = e env Value
        (c1, _) = s1 env
        (c2, _) = s2 env
        (n1, n2)  = (codeSize c1, codeSize c2)

fStatWhile :: E -> S -> S
fStatWhile e s1 env = ([BRA n] ++ c1 ++ c ++ [BRT (-(n + k + 2))], env)
    where
        (c, env') = e env Value
        (c1, env1) = s1 env
        (n, k) = (codeSize c1, codeSize c)

fStatReturn :: E -> S
fStatReturn e env = addCode (e env Value) ([STL 1, LDRR SP MP] ++ [RET]) -- STL 1 to store return value 1 places after mark pointer

fStatBlock :: [S] -> S
--fStatBlock statements env = foldl (\(c, e) s -> combine (c, e) (s e)) ([], env) statements
fStatBlock statements env = (fst (foldl (\(c, e) s -> combine (c, e) (s e)) ([], env) statements), env)


fExprConstInt :: Int -> E
fExprConstInt n env va = ([LDC n], env)

fExprConstChar :: Char -> E
fExprConstChar c env va = ([LDC (fromEnum c)], env)

fExprConstBool :: Bool -> E
fExprConstBool True  env va = ([LDC 1], env)
fExprConstBool False env va = ([LDC 0], env)

fExprVar :: String -> E
fExprVar x env@Environment{locals = loc, nextLocal = nextLoc, globals = glob, nextGlobal = nextGlob} va 
  | M.member x loc = let location = loc M.! x in case va of
    Value    ->  ([LDL  location], env)
    Address  ->  ([LDLA location], env)
  | M.member x glob = let location = glob M.! x in case va of
    Value    ->  ([LDC 0, LDA  location], env)
    Address  ->  ([LDC 0, LDAA location], env)
  -- If a called variable is not declared locally or globally, it is undeclared, thus an error message is returned. 
  | otherwise = error ("\ESC[31;1mUndeclared variable: '" ++ x ++ "'\ESC[0m")

fExprOp :: String -> E -> E -> E
fExprOp "=" e1 e2 env va = (c2 ++ [LDS 0] ++ c1 ++ [STA 0], env)
  where
    (c1, env1) = e1 env Address
    (c2, env2) = e2 env Value
fExprOp op  e1 e2 env va = result matchedOp
  where
    matchedOp = opCodes M.! op
    result :: Instr -> (Code, Environment)
    -- Lazy evaluation
    result AND = (c1 ++ [LDS 0, BRF n2] ++ c2 ++ [AND], env) -- if the first parameter of an AND operator is false, the second is irrelevant
    result OR  = (c1 ++ [LDS 0, BRT n2] ++ c2 ++ [OR], env)  -- if the first parameter of an OR operator is true, the second is irrelevant
    result mOp = (c1 ++ c2 ++ [mOp], env)
    (c1, env1) = e1 env Value
    (c2, env2) = e2 env Value
    n2 = codeSize c2 + 1
    opCodes :: M.Map String Instr
    opCodes = M.fromList [ ("+", ADD), ("-",  SUB)
                          , ("*", MUL), ("/", DIV), ("%", MOD)
                          , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT)
                          , ("==", EQ) , ("!=", NE)
                          , ("&&", AND), ("||", OR), ("^", XOR)
                          ]

fExprCall :: String -> [E] -> E
fExprCall "print" ps env va = addCode (foldl (\(c, e) p -> addCode (combine (c, e) (p e va)) [TRAP 0]) ([], env) ps) [LDC 0]
fExprCall method ps env va  = addCode (foldl (\(c, e) p -> combine (c, e) (p e va)) ([], env) ps) 
  ([LDR MP, Bsr method, STR MP] ++ 
  (replicate (length ps) pop) ++        -- remove parameters from stack
  [LDS (3 + (length ps))])              -- compensate for parameters


-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
