module Tokenizer(Token(Symbol,Zero,One,Equal,Dot,Nil),tokenize) where

data Token = Symbol String | Zero | One | Equal | Dot | Nil

tokenize :: String -> [Token]

tokenize ('=':'=':cs) = tokenize (dropComment cs)
tokenize ('0':cs) = Zero : tokenize cs
tokenize ('1':cs) = One : tokenize cs
tokenize ('=':cs) = Equal : tokenize cs
tokenize ('.':cs) = Dot : tokenize cs
tokenize ('_':cs) = Nil : tokenize cs
tokenize (' ':cs) = tokenize cs
tokenize ('\t':cs) = tokenize cs
tokenize ('\r':cs) = tokenize cs
tokenize ('\n':cs) = tokenize cs
tokenize [] = []
tokenize cs = tokenizeSymbol cs ""

tokenizeSymbol s@(c:cs) symbol =
    case c of
        '=' -> Symbol (reverse symbol) : tokenize s
        '0' -> Symbol (reverse symbol) : tokenize s
        '1' -> Symbol (reverse symbol) : tokenize s
        '.' -> Symbol (reverse symbol) : tokenize s
        '_' -> Symbol (reverse symbol) : tokenize s
        ' ' -> Symbol (reverse symbol) : tokenize cs
        '\t' -> Symbol (reverse symbol) : tokenize cs
        '\r' -> Symbol (reverse symbol) : tokenize cs
        '\n' -> Symbol (reverse symbol) : tokenize cs
        _ -> tokenizeSymbol cs (c:symbol)
tokenizeSymbol [] symbol = [Symbol (reverse symbol)]

dropComment ('\n':cs) = cs
dropComment (_:cs) = dropComment cs
dropComment [] = []
