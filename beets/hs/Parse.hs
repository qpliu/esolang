module Parse(parse)
where

import Data.List(elemIndex)
import Data.Map(Map,empty,foldWithKey,insert,member,(!))
import qualified Data.Map

import Ast
       (Function(Function),
       Expr(ExprParam,ExprCall,ExprTree,ExprLeft,ExprRight,ExprCond))
import Tokenize(Token,tokenLocation,token)

parse :: [Token] -> Either [String] (Map String Function)
parse tokens = pass1 tokens >>= pass2

isIdent :: Token -> Bool
isIdent t = not (token t `elem` ["=",".","?",",","<",">","0","1","(",")"])

data FuncToks = FuncToks Token [Token] [Token] Token

arity :: FuncToks -> Int
arity (FuncToks _ params _ _) = length params

pass1 :: [Token] -> Either [String] (Map String FuncToks)
pass1 tokens = collect empty tokens
  where
    collect funcToks [] = Right funcToks
    collect funcToks (t:ts)
      | not (isIdent t) = Left [tokenLocation t ++ ": unexpected token"]
      | member (token t) funcToks = Left [tokenLocation t ++ ": duplicate function name"]
      | otherwise = collectParams t [] ts
      where
        collectParams funcName params [] = Left [tokenLocation funcName ++ ": unexpected EOF"]
        collectParams funcName params (t:ts)
          | token t `elem` map token params = Left [tokenLocation t ++ ": duplicate parameter name"]
          | isIdent t = collectParams funcName (t:params) ts
          | token t == "=" = collectBody funcName (reverse params) [] ts
        collectBody funcName params body [] = Left [tokenLocation funcName ++ ": unexpected EOF"]
        collectBody funcName params body (t:ts)
          | token t == "." = collect (insert (token funcName) (FuncToks funcName params (reverse body) t) funcToks) ts
          | otherwise = collectBody funcName params (t:body) ts

pass2 :: Map String FuncToks -> Either [String] (Map String Function)
pass2 funcToks = foldWithKey check2 (Right empty) funcs
  where
    check2 name (Left funcErrs) (Left errs) = Left (errs ++ funcErrs)
    check2 name (Left funcErrs) (Right _) = Left funcErrs
    check2 name (Right _) (Left errs) = Left errs
    check2 name (Right func) (Right funcs) = Right (insert name func funcs)
    funcs = fmap parseFunc funcToks
    parseFunc (FuncToks nameTok paramToks bodyToks dotTok) = do
        (expr,toks) <- parseExpr bodyToks
        checkTrailingGarbage toks
        return (Function nameTok paramToks expr)
      where
        checkTrailingGarbage [] = return ()
        checkTrailingGarbage (tok:_) = Left [tokenLocation tok ++ ": unexpected token"]
        checkExpected expected [] = Left [tokenLocation dotTok ++ ": expected " ++ expected]
        checkExpected expected (tok:toks)
          | token tok == expected = Right toks
          | otherwise = Left [tokenLocation tok ++ ": expected " ++ expected]
        params = map token paramToks
        parseExpr [] = Left [tokenLocation dotTok ++ ": expected expression"]
        parseExpr (t:toks)
          | isIdent t = maybe (parseCall t toks) (parseExprContinuation toks . ExprParam t) (elemIndex (token t) params)
          | token t == "0" = do
              (lexpr,toks) <- parseExpr toks
              toks <- checkExpected "," toks
              (rexpr,toks) <- parseExpr toks
              parseExprContinuation toks (ExprTree t False lexpr rexpr)
          | token t == "1" = do
              (lexpr,toks) <- parseExpr toks
              toks <- checkExpected "," toks
              (rexpr,toks) <- parseExpr toks
              parseExprContinuation toks (ExprTree t True lexpr rexpr)
          | token t == "<" = do
              (expr,toks) <- parseExpr toks
              parseExprContinuation toks (ExprLeft t expr)
          | token t == ">" = do
              (expr,toks) <- parseExpr toks
              parseExprContinuation toks (ExprRight t expr)
          | token t == "(" = do
              (expr,toks) <- parseExpr toks
              toks <- checkExpected ")" toks
              parseExprContinuation toks expr
          | token t `elem` ["?",",",")"] = Left [tokenLocation t ++ ": unexpected token"]
          | otherwise = Left [tokenLocation t ++ ": parser bug"]
        parseExprContinuation [] expr = Right (expr,[])
        parseExprContinuation (t:toks) expr
          | token t == "?" = do
              (lexpr,toks) <- parseExpr toks
              toks <- checkExpected "," toks
              (rexpr,toks) <- parseExpr toks
              return (ExprCond t expr lexpr rexpr,toks)
          | otherwise = Right (expr,t:toks)
        parseCall calleeTok toks = maybe (Left [tokenLocation calleeTok ++ ": undefined identifier"]) (parseArgs [] toks . arity) (Data.Map.lookup (token calleeTok) funcToks)
          where
            parseArgs args toks nargs
              | nargs <= 0 = parseExprContinuation toks (ExprCall calleeTok (let Right func = funcs!token calleeTok in func) (reverse args))
              | otherwise = do
                    (expr,toks) <- parseExpr toks
                    parseArgs (expr:args) toks (nargs-1)
