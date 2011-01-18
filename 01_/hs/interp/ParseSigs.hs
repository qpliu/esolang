module ParseSigs(Signature(Sig),parseSigs) where

import Tokenizer(Token(Symbol,Zero,One,Equal,Dot,Nil))
import SyntaxTree(Bits,Pattern(Literal,Binding,Wildcard))

data Signature = Sig String [Pattern] [Token]

parseSigs :: [Token] -> [Signature]

parseSigs (Symbol s:tokens) = parseSig s [] tokens []
parseSigs [] = []
parseSigs _ = error "name expected to start definition"

parseSig name patterns (Equal:tokens) [] =
    collectBody name (reverse patterns) tokens []
parseSig name patterns (Equal:tokens) bits =
    collectBody name (reverse (Literal (reverse bits) : patterns)) tokens []
parseSig name patterns (Zero:tokens) bits =
    parseSig name patterns tokens (False:bits)
parseSig name patterns (One:tokens) bits =
    parseSig name patterns tokens (True:bits)
parseSig name patterns (Nil:tokens) bits =
    parseSig name (Literal (reverse bits) : patterns) tokens []
parseSig name patterns (Symbol binding:tokens) bits =
    parseSig name (Binding (reverse bits) binding : patterns) tokens []
parseSig name patterns (Dot:tokens) bits =
    parseSig name (Wildcard (reverse bits) : patterns) tokens []

collectBody name patterns (Dot:tokens) body =
    Sig name patterns (reverse body) : parseSigs tokens
collectBody name patterns (token:tokens) body =
    collectBody name patterns tokens (token:body)
collectBody name patterns [] body = error ("unexpected end of body of "++name)
