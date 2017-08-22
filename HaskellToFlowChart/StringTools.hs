module StringTools where

import Prelude



--Helper functions for Printintg

data EnclosingGlyph = Bracket

nestString :: EnclosingGlyph -> String -> String
nestString Bracket str = "[" ++ str ++ "]"
nestString _ str = str