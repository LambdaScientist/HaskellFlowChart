module FlowChart where


import Prelude
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import StringTools


main = undefined

type Title = String
type Code  = String
type BranchPtr = String

data Path = TitlePath (Title, Tree) deriving (Show)
instance Pretty Path where
  pPrint (TitlePath (title, tree)) = text (nestString Bracket title)
                                $+$ pPrint tree
                                $+$ text ""
                                
data Tree = Branch UMLetActiviyAIO 
          | Node Code 
          deriving (Show)
instance Pretty Tree where
  pPrint (Branch uml) = pPrint uml
  pPrint (Node uml)   = pPrint uml

type Condition = String
data UMLetActiviyAIO = Empty 
                     | Cell Code UMLetActiviyAIO 
                     | If [Path] 
                     | While Condition [Path]
                     deriving (Show)
instance Pretty UMLetActiviyAIO where
  pPrint Empty = text ""
  pPrint (Cell code uml) = text code
                       $+$ pPrint uml
  pPrint (If paths) = text "If"
                  $+$ (nest 8 $ foldl prettyAndMerge (text "") paths)
                  $+$ text "EndIf"
  pPrint (While cond paths) = text "While" <> text (nestString Bracket cond)
                  $+$ (nest 8 $ foldl prettyAndMerge (text "") paths)
                  

prettyAndMerge :: Doc -> Path -> Doc 
prettyAndMerge acc path = acc $+$ (pPrint path)

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : map (p <>) ds

tab :: Doc
tab = text "\t"