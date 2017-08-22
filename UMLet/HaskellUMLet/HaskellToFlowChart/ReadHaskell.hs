import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment

import Data.Monoid

import Data.String.Utils

type HasType = String
type Code = String
type CommentString = String
type Name = String

data Input = Var String | MatchOn String deriving (Show)
data Hask = Comment CommentString | TypeSig Name [HasType] | Body Code | Fun Name Args [Hask]  deriving (Show)


type Args = [Input]
-- data Function = Fun Name Args [Hask] deriving (Show)



parseComment :: Parser Hask
parseComment = string "--" >> (many $ char '-')
            >> many (noneOf "\n") -- (try $ string "--" *> (many $ char '-'))
            >> many (noneOf "\n") 
           >>= return.Comment 

parseTypeSig :: Parser Hask
parseTypeSig = do name <- manyTill (noneOf "\n") (string "::") <* spaces 
                  types <- manyTill token (char '\n')
                  char '\n'
                  return $ TypeSig name types 
    where
      token = (:) <$> anyToken <*> manyTill (noneOf "\n") tryConsumeSep
      consumeSep = spaces *> string "->" <* spaces
      tryConsumeSep = trySep <|> end
      trySep = try (() <$ consumeSep)
      end = try eof

-- parseFunction :: Parser Function
parseFunction :: Parser Hask
parseFunction = do name <- grabTilSpace
                   spaces
                   args <- manyTill (manyTill makeInput (oneOf " ")) (spaces *> char '=' <* spaces)
                   codeBody <- many (noneOf "\n")  <* char '\n'
                   return $ Fun name (concat args) [Body codeBody]
    where
      contentWrap [] = ""
      contentWrap x = "{" ++ x ++ "}"
      grabTilSpace :: Parser Name
      grabTilSpace = many1 $ noneOf " "
      makeInput :: Parser Input 
      makeInput = (getParaContents >>= return.Var) <|> makePattern <|> makeVar
      makePattern = do firstLetter <- upper 
                       remainder <- many getRestOfAnInput 
                       maybeBrakContents <- option ""  getBracketContents                
                       return $ MatchOn $ firstLetter : remainder ++ contentWrap maybeBrakContents
      makeVar = do firstLetter <- lower 
                   remainder <- many getRestOfAnInput 
                   maybeBrakContents <- option "" getBracketContents                
                   return $ Var $ firstLetter : remainder ++ contentWrap maybeBrakContents
      getRestOfAnInput = alphaNum <|> oneOf "'_"


getParaContents :: Parser String
getParaContents = getGenContents '(' ')'

getBracketContents :: Parser String
getBracketContents = getGenContents '{' '}'

getGenContents :: Char -> Char -> Parser String
getGenContents x y = char x *> insideContents <* char y
  where 
    notEnd = noneOf [y]
    insideContents = getGenContents x y <|> many notEnd
    
parseCode :: Parser [Hask]
parseCode = manyTill posCode eof
  where
    posCode = try parseTypeSig -- <|> try parseFunction <|> parseComment
-- matchBrace :: Parser String
-- matchBrace = between (char '{') (char '}') $ many (matchBrace <|> noneOf "}")
-- matchParenthesis  :: Parser String
-- matchParenthesis = between (char '(') (char ')') $ many (matchParenthesis <|> noneOf ")")
-- matchBracket  :: Parser String
-- matchBracket = between (char '[') (char ']') $ many (matchBracket <|> noneOf "]")


-- Testing 
comTest = "-------Yay" 
comTest2 = "this is a fun -----Also Yay" 
sigTest = "foo :: Meh -> Monad eh -> Blah-> Answer Bloop" 
sigTest2 = "foo :: Meh -> Monad eh -> Blah    -> Answer Bloop  ->" 
funTest = "foo Yay{..} = undefined" 
funTest2 = "foo Yay boo = undefined" 
bracketTest = "{{Hi}}" 

testCom = parseTest parseComment comTest
testCom2 = parseTest parseComment comTest2
testSig = parseTest parseTypeSig sigTest
testSig2 = parseTest parseTypeSig sigTest2

testFun = parseTest parseFunction funTest
testFun2 = parseTest parseFunction funTest2


brackt = parseTest getBracketContents bracketTest


createHaskell = do result <- parseFromFile parseCode "testHaskellCode.hs"  
                   case result of
                        Left err -> print err
                        Right par -> writeFile "readFunction.txt" $ show par 

