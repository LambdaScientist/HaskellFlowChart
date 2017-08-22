nestString :: EnclosingGlyph -> String -> String
nestString Bracket str = "[" ++ str ++ "]"
nestString _ str = str