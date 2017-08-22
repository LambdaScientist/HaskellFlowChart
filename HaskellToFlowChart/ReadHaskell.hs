import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char
import System.Environment

import Data.Monoid

type HasType = String
type Code = String
type CommentString = String
type Name = String

data Input = Var String | MatchOn String deriving (Show)
data Hask = Comment CommentString | TypeSig Name [HasType] | Args [Input] | Body Code  deriving (Show)


data Function = Fun Name [Hask] deriving (Show)





parseComment :: Parser Hask
parseComment = many (char '-') >> many (noneOf "\n") >>= return.Comment 

parseTypeSig :: Parser Hask
parseTypeSig = do name <- many (noneOf ":")
                  many $ char ':'
                  types <- sepBy consumeNotArrow consumeArrow  -- many readTypSigParts
                --   returnType <- many anyChar
                  return $ TypeSig name $ types -- ++ [returnType]
    where
        consumeNotArrow = many (noneOf "->")
        consumeArrow = string "->"

readTypSigParts :: Parser HasType
readTypSigParts = manyTill anyChar 
                $ try 
                $ string "->" -- <|> endOfLine


comTest = "-------Yay" 
sigTest = "foo :: Meh -> Meh -> Blah -> Bloop" 
funTest = "foo Yay boo" 

testCom = parseTest parseComment comTest
testSig = parseTest parseTypeSig sigTest