module CSharpLex where

import Data.Char
import Control.Monad (guard)
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>), sequence)
import Data.Maybe

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyFor
           | StdType        String       -- the 8 standard types
           | MulOperator    String
           | AddOperator    String
           | RelOperator    String
           | EqOperator     String
           | XorOperator    String
           | AndOperator    String
           | OrOperator     String
           | AssignOperator String
           | UpperId        String       -- uppercase identifiers
           | LowerId        String       -- lowercase identifiers
           | ConstInt       Int
           | ConstChar      Char
           | ConstBool      Bool
           deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexComment *> lexToken <* lexWhiteSpace) <* eof

lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum MulOperator multiplicativeOperators
             , lexEnum AddOperator additiveOperators
             , lexEnum RelOperator relationalOperators
             , lexEnum EqOperator equalityOperators
             , lexEnum XorOperator xorOperators
             , lexEnum AndOperator andOperators
             , lexEnum OrOperator orOperators
             , lexEnum AssignOperator assignmentOperators
             , lexConstInt
             , lexConstChar
             , lexConstBool
             , lexLowerId
             , lexUpperId
             ]


lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ ( POpen     , "("      )
      , ( PClose    , ")"      )
      , ( SOpen     , "["      )
      , ( SClose    , "]"      )
      , ( COpen     , "{"      )
      , ( CClose    , "}"      )
      , ( Comma     , ","      )
      , ( Semicolon , ";"      )
      , ( KeyIf     , "if"     )
      , ( KeyElse   , "else"   )
      , ( KeyWhile  , "while"  )
      , ( KeyFor    , "for"    )
      , ( KeyReturn , "return" )
      , ( KeyTry    , "try"    )
      , ( KeyCatch  , "catch"  )
      , ( KeyClass  , "class"  )
      , ( KeyVoid   , "void"   )
      ]


lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]
--operators :: [String]
--operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

multiplicativeOperators :: [String]
multiplicativeOperators = ["*", "/", "%"]
additiveOperators :: [String]
additiveOperators = ["+", "-"]
relationalOperators :: [String]
relationalOperators = ["<=", "<", ">=", ">"]
equalityOperators :: [String]
equalityOperators = ["==", "!="]
xorOperators :: [String]
xorOperators = ["^"]
andOperators :: [String]
andOperators = ["&&"]
orOperators :: [String]
orOperators = ["||"]
assignmentOperators :: [String]
assignmentOperators = ["="]

lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit)

-- Lex characters
lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$> (symbol '\'' *> satisfy (const True) <* symbol '\'')

-- Lex booleans
lexConstBool :: Parser Char Token
lexConstBool = (\b -> ConstBool (b == "true")) <$> (token "true" <|> token "false")

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)


lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty

-- Lex single line comments
lexComment :: Parser Char [String]
lexComment = greedy (token "//" *> greedy (satisfy (/= '\n')) <* lexWhiteSpace) <|> succeed []
----- End Lexer -----


----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = pFromMaybe fromStdType
  where fromStdType (StdType x) = Just x
        fromStdType _           = Nothing

sUpperId :: Parser Token String
sUpperId = pFromMaybe fromUpperId
    where fromUpperId (UpperId x) = Just x
          fromUpperId _           = Nothing

sLowerId :: Parser Token String
sLowerId = pFromMaybe fromLowerId
  where fromLowerId (LowerId x) = Just x
        fromLowerId _           = Nothing

sConstInt :: Parser Token Int
sConstInt  = pFromMaybe fromConstInt
  where fromConstInt (ConstInt  x) = Just x
        fromConstInt _             = Nothing

sConstChar :: Parser Token Char
sConstChar  = pFromMaybe fromConstChar
  where fromConstChar (ConstChar  x) = Just x
        fromConstChar _             = Nothing

sConstBool :: Parser Token Bool
sConstBool  = pFromMaybe fromConstBool
  where fromConstBool (ConstBool  x) = Just x
        fromConstBool _             = Nothing

--sOperator :: Parser Token String
--sOperator = pFromMaybe fromOperator
--  where 
--    fromOperator (Operator x) = Just x
--    fromOperator _ = Nothing

sMulOperator :: Parser Token String
sMulOperator = pFromMaybe fromOperator
  where 
    fromOperator (MulOperator x) = Just x
    fromOperator _ = Nothing

sAddOperator :: Parser Token String
sAddOperator = pFromMaybe fromOperator
  where 
    fromOperator (AddOperator x) = Just x
    fromOperator _ = Nothing

sRelOperator :: Parser Token String
sRelOperator = pFromMaybe fromOperator
  where 
    fromOperator (RelOperator x) = Just x
    fromOperator _ = Nothing

sEqOperator :: Parser Token String
sEqOperator = pFromMaybe fromOperator
  where 
    fromOperator (EqOperator x) = Just x
    fromOperator _ = Nothing

sXorOperator :: Parser Token String
sXorOperator = pFromMaybe fromOperator
  where 
    fromOperator (XorOperator x) = Just x
    fromOperator _ = Nothing

sAndOperator :: Parser Token String
sAndOperator = pFromMaybe fromOperator
  where 
    fromOperator (AndOperator x) = Just x
    fromOperator _ = Nothing

sOrOperator :: Parser Token String
sOrOperator = pFromMaybe fromOperator
  where 
    fromOperator (OrOperator x) = Just x
    fromOperator _ = Nothing

sAssignOperator :: Parser Token String
sAssignOperator = pFromMaybe fromOperator
  where 
    fromOperator (AssignOperator x) = Just x
    fromOperator _ = Nothing

sSemi :: Parser Token Token
sSemi =  symbol Semicolon


pFromMaybe :: (s -> Maybe a) -> Parser s a
pFromMaybe f = fromJust . f <$> satisfy (isJust . f)
