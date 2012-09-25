module Parse(
    parseDef1,parseStream,def1Arity,Def1,
    compileDef,Def(Def),DefBody(DefBody),DefGuard(DefGuard),
    compileExpr,Expr(ExprNumber,ExprParam,ExprFuncall,ExprDiv,ExprRem,ExprMul,
                   ExprCompileError),
    compile)
where

import Data.List(elemIndex)
import Data.Map(Map)
import qualified Data.Map as M

import Tokenize(
    Location(Location),
    Token(Number,Identifier,
          Equals,Comma,Slash,Percent,LeftParen,RightParen,Dot),
    tokenLocation)

data Def1 = Def1 Location String Int [Def1Body]
    deriving Show

data Def1Body = Def1Body [Def1Expr] [([Def1Expr],[Def1Expr])]
    deriving Show

data Def1Expr =
    Def1Number Location Integer
  | Def1Param Location Int
  | Def1Func Location String
  | Def1Div Location
  | Def1Rem Location
  | Def1Mul Location [Def1Expr] [Def1Expr]
    deriving Show

def1ExprLocation :: Def1Expr -> Location
def1ExprLocation (Def1Number location _) = location
def1ExprLocation (Def1Param location _) = location
def1ExprLocation (Def1Func location _) = location
def1ExprLocation (Def1Div location) = location
def1ExprLocation (Def1Rem location) = location
def1ExprLocation (Def1Mul location _ _) = location

-- Basic recursive descent
-- First pass of two passes
parseDef1 :: [Token] -> Either (Location,String) (Def1,[Token])
parseDef1 [] = Left (Location "" 0 0,"Unexpected EOF")
parseDef1 (Identifier id location:tokens) = parseDef1Args id location [] tokens
parseDef1 (token:_) = Left (tokenLocation token,"Unexpected token")

parseDef1Args :: String -> Location -> [String] -> [Token] -> Either (Location,String) (Def1,[Token])
parseDef1Args name location args (Identifier arg _:tokens) =
    parseDef1Args name location (arg:args) tokens
parseDef1Args name location args (Equals _:tokens) =
    parseDef1Bodies name location (reverse args) [] tokens
parseDef1Args _ _ _ (token:_) = Left (tokenLocation token,"Unexpected token")

parseDef1Bodies :: String -> Location -> [String] -> [Def1Body] -> [Token] -> Either (Location,String) (Def1,[Token])
parseDef1Bodies name location args bodies tokens =
    case parseDef1Exprs args tokens [] of
      Left err -> Left err
      Right (_,[]) -> Left (location,"Incomplete definition")
      Right ([],token:_) -> Left (tokenLocation token,"Unexpected token")
      Right (exprs,Dot _:tokens) ->
        case bodies of
          (Def1Body (expr:_) []:_) -> Left (def1ExprLocation expr,"Unguarded expression must be last")
          _ -> Right (Def1 location name (length args) (reverse (Def1Body exprs []:bodies)),tokens)
      Right (exprs,Comma _:tokens) ->
        case bodies of
          (Def1Body (expr:_) []:_) -> Left (def1ExprLocation expr,"Unguarded expression must be last")
          _ -> parseDef1Bodies name location args (Def1Body exprs []:bodies) tokens
      Right (exprs,Equals _:tokens) ->
        case bodies of
          (Def1Body def1Body def1Guards:previousBodies) ->
            case parseDef1Exprs args tokens [] of
              Left err -> Left err
              Right (_,[]) -> Left (location,"Incomplete definition")
              Right ([],token:_) -> Left (tokenLocation token,"Unexpected token")
              Right (rightExprs,Comma _:tokens) ->
                parseDef1Bodies name location args (Def1Body def1Body (def1Guards ++ [(exprs,rightExprs)]):previousBodies) tokens
              Right (_,token:_) -> Left (tokenLocation token,"Unexpected token")
          _ -> Left (def1ExprLocation (head exprs), "Guard must follow an expression")
      Right (_,token:_) -> Left (tokenLocation token,"Unexpected token")

parseDef1Exprs :: [String] -> [Token] -> [Def1Expr] -> Either (Location,String) ([Def1Expr],[Token])
parseDef1Exprs args (Number number location:tokens) exprs =
    parseDef1Exprs args tokens (Def1Number location number:exprs)
parseDef1Exprs args (Identifier id location:tokens) exprs =
    case id `elemIndex` args of
      Just i -> parseDef1Exprs args tokens (Def1Param location i:exprs)
      _ -> parseDef1Exprs args tokens (Def1Func location id:exprs)
parseDef1Exprs args (Slash location:tokens) exprs =
    parseDef1Exprs args tokens (Def1Div location:exprs)
parseDef1Exprs args (Percent location:tokens) exprs =
    parseDef1Exprs args tokens (Def1Rem location:exprs)
parseDef1Exprs args (LeftParen location:tokens) exprs =
    case parseDef1Mul args location tokens of
      Left err -> Left err
      Right (expr,tokens) -> parseDef1Exprs args tokens (expr:exprs)
parseDef1Exprs _ _ (Def1Div location:_) =
    Left (location,"Missing argument to /")
parseDef1Exprs _ _ (Def1Rem location:_) =
    Left (location,"Missing argument to %")
parseDef1Exprs _ tokens exprs = Right (reverse exprs,tokens)

parseDef1Mul :: [String] -> Location -> [Token] -> Either (Location,String) (Def1Expr,[Token])
parseDef1Mul args location tokens =
    case parseDef1Exprs args tokens [] of
      Left err -> Left err
      Right (_,[]) -> Left (location,"Incomplete definition")
      Right ([],token:_) -> Left (tokenLocation token,"Unexpected token")
      Right (leftExprs,Comma _:tokens) ->
        case parseDef1Exprs args tokens [] of
          Left err -> Left err
          Right (_,[]) -> Left (location,"Incomplete definition")
          Right ([],token:_) -> Left (tokenLocation token,"Unexpected token")
          Right (rightExprs,RightParen _:tokens) ->
            Right (Def1Mul location leftExprs rightExprs,tokens)
          Right (_,token:_) -> Left (tokenLocation token,"Unexpected token")
      Right (_,token:_) -> Left (tokenLocation token,"Unexpected token")

parseStream :: Map String Def1 -> [Token] -> ([(Location,String)],Map String Def1)
parseStream def1s tokens = parseStream' def1s tokens []
  where
    parseStream' def1s [] errors = (reverse errors,def1s)
    parseStream' def1s tokens errors =
        case parseDef1 tokens of
          Left err -> parseStream' def1s (skipToDot tokens) (err:errors)
          Right (def1@(Def1 _ name _ _),tokens') ->
            parseStream' (M.insert name def1 def1s) tokens' errors
    skipToTot [] = []
    skipToDot (Dot _:tokens) = tokens
    skipToDot (_:tokens) = skipToDot tokens

def1Arity :: Map String Def1 -> String -> Maybe Int
def1Arity def1s name = fmap (\ (Def1 _ _ a _) -> a) (M.lookup name def1s)


data Def = Def Location String Int [DefBody]
    deriving Show

data DefBody = DefBody Expr [DefGuard]
    deriving Show

data DefGuard = DefGuard Expr Expr
    deriving Show

data Expr =
    ExprNumber Integer
  | ExprParam Int
  | ExprFuncall String Location [Expr]
  | ExprDiv Expr
  | ExprRem Expr
  | ExprMul Expr Expr
  | ExprCompileError (Location,String)
    deriving Show

-- Second pass of two passes
compileDef :: (String -> Maybe Int) -> Def1 -> Def
compileDef arity (Def1 location name nargs bodies) =
    Def location name nargs (map (compileBody arity) bodies)

compileBody :: (String -> Maybe Int) -> Def1Body -> DefBody
compileBody arity (Def1Body exprs guards) =
    DefBody (compileDef1ExprsToExpr arity exprs) (map (compileGuard arity) guards)

compileGuard :: (String -> Maybe Int) -> ([Def1Expr],[Def1Expr]) -> DefGuard
compileGuard arity (leftExprs,rightExprs) =
    DefGuard (compileDef1ExprsToExpr arity leftExprs) (compileDef1ExprsToExpr arity rightExprs)

compileDef1ExprsToExpr :: (String -> Maybe Int) -> [Def1Expr] -> Expr
compileDef1ExprsToExpr arity exprs =
    case compileExprs arity exprs of
      (expr,[]) -> expr
      (_,expr:_) -> ExprCompileError (def1ExprLocation expr, "Unexpected expression")

compileExprs :: (String -> Maybe Int) -> [Def1Expr] -> (Expr,[Def1Expr])
compileExprs arity [] =
    (ExprCompileError (Location "" 0 0, "Unexpected EOF"),[])
compileExprs arity (Def1Number _ number:exprs) = (ExprNumber number,exprs)
compileExprs arity (Def1Param _ i:exprs) = (ExprParam i,exprs)
compileExprs arity (Def1Func location name:exprs) =
    case arity name of
      Nothing -> (ExprCompileError (location,"Undefined symbol:"++name),[])
      Just nargs -> compileFuncall arity location name nargs [] exprs
compileExprs arity (Def1Div _:exprs) =
    let (expr,exprs') = compileExprs arity exprs in (ExprDiv expr,exprs')
compileExprs arity (Def1Rem _:exprs) =
    let (expr,exprs') = compileExprs arity exprs in (ExprRem expr,exprs')
compileExprs arity (Def1Mul _ leftExprs rightExprs:exprs) =
    case (compileExprs arity leftExprs,compileExprs arity rightExprs) of
      ((leftExpr,[]),(rightExpr,[])) -> (ExprMul leftExpr rightExpr,exprs)
      ((_,expr:_),_) -> (ExprCompileError (def1ExprLocation expr,"Unexpected expression"),exprs)
      (_,(_,expr:_)) -> (ExprCompileError (def1ExprLocation expr,"Unexpected expression"),exprs)

compileFuncall :: (String -> Maybe Int) -> Location -> String -> Int -> [Expr] -> [Def1Expr] -> (Expr,[Def1Expr])
compileFuncall arity location name nargs (err@(ExprCompileError _):_) exprs =
    (err,[])
compileFuncall arity location name 0 args exprs =
    (ExprFuncall name location (reverse args),exprs)
compileFuncall arity location name nargs args [] =
    (ExprCompileError (location,"Missing arguments to "++name++":"++show nargs), [])
compileFuncall arity location name nargs args exprs =
    let (arg,exprs') = compileExprs arity exprs
    in  compileFuncall arity location name (nargs-1) (arg:args) exprs'

compile :: Map String Def1 -> Map String Def
compile def1s = fmap (compileDef (def1Arity def1s)) def1s

-- For REPL
compileExpr :: (String -> Maybe Int) -> [Token] -> Expr
compileExpr arity tokens =
    case parseDef1Exprs [] tokens [] of
      Left err -> ExprCompileError err
      Right ([],[]) -> ExprCompileError (Location "" 0 0,"Expression missing")
      Right (exprs,[]) ->
        case compileExprs arity exprs of
          (expr,[]) -> expr
          (_,expr:_) -> ExprCompileError (def1ExprLocation expr,"Unexpected expression")
      Right (exprs,[Dot _]) ->
        case compileExprs arity exprs of
          (expr,[]) -> expr
          (_,expr:_) -> ExprCompileError (def1ExprLocation expr,"Unexpected expression")
      Right (_,token:_) -> ExprCompileError (tokenLocation token,"Unexpected token")
