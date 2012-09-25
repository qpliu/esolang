module Tokenize(
    Location(Location),
    Token(Number,Identifier,
          Equals,Comma,Slash,Percent,LeftParen,RightParen,Dot),
    tokenize,tokenLocation)
where

data Location = Location String Int Int
    deriving Show

data Token =
    Number Integer Location
  | Identifier String Location
  | Equals Location
  | Comma Location
  | Slash Location
  | Percent Location
  | LeftParen Location
  | RightParen Location
  | Dot Location
    deriving Show

tokenize :: String -> String -> [Token]
tokenize name chars = tokenize' name 1 1 chars

tokenLocation :: Token -> Location
tokenLocation (Number _ location) = location
tokenLocation (Identifier _ location) = location
tokenLocation (Equals location) = location
tokenLocation (Comma location) = location
tokenLocation (Slash location) = location
tokenLocation (Percent location) = location
tokenLocation (LeftParen location) = location
tokenLocation (RightParen location) = location
tokenLocation (Dot location) = location

data CharClass =
    Newline | Space | CharToken (Location -> Token) | NumberOrIdentifier

charClass :: Char -> CharClass
charClass c
  | c == '\n' = Newline
  | c `elem` " \t\r\f" = Space
  | c == '=' = CharToken Equals
  | c == ',' = CharToken Comma
  | c == '/' = CharToken Slash
  | c == '%' = CharToken Percent
  | c == '(' = CharToken LeftParen
  | c == ')' = CharToken RightParen
  | c == '.' = CharToken Dot
  | otherwise = NumberOrIdentifier

notNumberOrIdentifier :: Char -> Bool
notNumberOrIdentifier c =
    case charClass c of
      NumberOrIdentifier -> False
      _ -> True

tokenize' :: String -> Int -> Int -> String -> [Token]
tokenize' name line col chars
  | null chars = []
  | take 2 chars == "==" = skipComment name (line+1) chars
  | otherwise =
      case (charClass (head chars)) of
        Newline -> tokenize' name (line+1) 1 (tail chars)
        Space -> tokenize' name line (col+1) (tail chars)
        CharToken token -> token (Location name line col)
                                : tokenize' name line (col+1) (tail chars)
        NumberOrIdentifier ->
            numberOrIdentifier (Location name line col) (col+1)
                (take 1 chars) (drop 1 chars)

skipComment :: String -> Int -> String -> [Token]
skipComment name line chars
  | null chars = []
  | head chars == '\n' = tokenize' name line 1 (tail chars)
  | otherwise = skipComment name line (tail chars)

numberOrIdentifier :: Location -> Int -> String -> String -> [Token]
numberOrIdentifier location@(Location name line _) col id chars
  | null chars = [toNumberOrIdentifier location (reverse id)]
  | notNumberOrIdentifier (head chars) =
        toNumberOrIdentifier location (reverse id)
            : tokenize' name line col chars
  | otherwise =
      numberOrIdentifier location (col+1) (head chars:id) (tail chars)

toNumberOrIdentifier :: Location -> String -> Token
toNumberOrIdentifier location chars
  | chars == "0" = Number 0 location
  | all (`elem` "0123456789") chars && head chars /= '0' =
      Number (read chars) location
  | otherwise = Identifier chars location
