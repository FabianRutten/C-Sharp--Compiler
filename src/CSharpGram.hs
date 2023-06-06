module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConstInt  Int
          | ExprConstChar Char
          | ExprConstBool Bool
          | ExprVar       String
          | ExprOper      String Expr Expr
          | ExprCall      String [Expr]
          deriving Show

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)

data ExprDecl = ExprDeclExpr Expr 
              | ExprDeclDecl Decl 


pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pAssignExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pAssignExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pAssignExpr <*> pStat
     <|> statFor    <$ symbol KeyFor <* symbol POpen <*> pExprDecls <* symbol Semicolon <*> pAssignExpr <* symbol Semicolon <*> pExprDecls <* symbol PClose <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pAssignExpr               <*  sSemi
     <|> pBlock
     where 
          optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])
          statFor ed1 e ed2 s = StatBlock ((map toStat ed1) ++ [(StatWhile e (StatBlock ([s] ++ (map toStat ed2))))])
          toStat (ExprDeclExpr e) = StatExpr e
          toStat (ExprDeclDecl d) = StatDecl d


-- For loops
pExprDecl :: Parser Token ExprDecl
pExprDecl = ExprDeclExpr <$> pAssignExpr
         <|> ExprDeclDecl <$> pDecl

pExprDecls :: Parser Token [ExprDecl]
pExprDecls = option (listOf pExprDecl (symbol Comma)) []

-- Expression parsers
pExprSimple :: Parser Token Expr
pExprSimple =  ExprConstInt <$> sConstInt
           <|> ExprConstChar <$> sConstChar
           <|> ExprConstBool <$> sConstBool
           <|> ExprVar   <$> sLowerId
           <|> ExprCall  <$> sLowerId <*> parenthesised (option (listOf pAssignExpr (symbol Comma)) [])
           <|> parenthesised pAssignExpr

-- Parsers are ordered so that operators have the correct priorities. 
-- Parsers for left associative operators use chainl, parsers for right associative operators use chainr. 
pMulExpr :: Parser Token Expr
pMulExpr = chainl pExprSimple (ExprOper <$> sMulOperator)

pAddExpr :: Parser Token Expr
pAddExpr = chainl pMulExpr (ExprOper <$> sAddOperator)

pRelExpr :: Parser Token Expr
pRelExpr = chainl pAddExpr (ExprOper <$> sRelOperator)

pEqExpr :: Parser Token Expr
pEqExpr = chainl pRelExpr (ExprOper <$> sEqOperator)

pXorExpr :: Parser Token Expr
pXorExpr = chainl pEqExpr (ExprOper <$> sXorOperator)

pAndExpr :: Parser Token Expr
pAndExpr = chainl pXorExpr (ExprOper <$> sAndOperator)

pOrExpr :: Parser Token Expr
pOrExpr = chainl pAndExpr (ExprOper <$> sOrOperator)

pAssignExpr :: Parser Token Expr
pAssignExpr = chainr pOrExpr (ExprOper <$> sAssignOperator)

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType
     <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}
