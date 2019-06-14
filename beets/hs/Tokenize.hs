module Tokenize(Token,tokenLocation,token,tokenize)
where

data Token = Token String Int Int String
    deriving (Eq,Show)

tokenLocation :: Token -> String
tokenLocation (Token file line column _) =
    file ++ ":" ++ show line ++ ":" ++ show column

token :: Token -> String
token (Token _ _ _ value) = value

tokenize :: String -> String -> [Token]
tokenize filename src = toks 1 1 src
  where
    toks _ _ "" = []
    toks line col ('=':'=':cs) = toks line col (dropWhile (/= '\n') cs)
    toks line col ('\n':cs) = toks (line+1) 1 cs
    toks line col (c:cs)
      | c `elem` " \t\f\r\v" = toks line (col+1) cs
      | c `elem` ".<>(),01?=" =
            Token filename line col [c] : toks line (col+1) cs
      | otherwise = identTok line col [c] (col+1) cs
    identTok line startCol tnedi col "" =
        [Token filename line startCol (reverse tnedi)]
    identTok line startCol tnedi col (c:cs)
      | c `elem` " \t\f\r\v\n.<>(),?=" =
            Token filename line startCol (reverse tnedi) : toks line col (c:cs)
      | otherwise = identTok line startCol (c:tnedi) (col+1) cs
