module Parser2(parser2) where

import Data.Map(Map,empty,insert,lookup)
import Prelude hiding (lookup)
import Text.ParserCombinators.Parsec(ParseError)
import Text.ParserCombinators.Parsec.Error(Message(Message),newErrorMessage)

import Parser1(Defn1(Defn1),Param1,Expr1)

-- Parser pass 2.

parser2 :: [Defn1] -> Either ParseError (Map String [Defn1])
parser2 defns = fmap (fmap reverse) (foldl addDefn (Right empty) defns)

addDefn :: Either ParseError (Map String [Defn1]) -> Defn1
        -> Either ParseError (Map String [Defn1])
addDefn map defn = either Left (tryAddDefn defn) map

tryAddDefn :: Defn1 -> Map String [Defn1]
                    -> Either ParseError (Map String [Defn1])
tryAddDefn defn@(Defn1 _ name _ _) defns =
    maybe (Right (insert name [defn] defns)) (tryUpdateDefn defn defns)
          (lookup name defns)

tryUpdateDefn :: Defn1 -> Map String [Defn1] -> [Defn1]
                       -> Either ParseError (Map String [Defn1])
tryUpdateDefn defn@(Defn1 position name params _) defns
              defnList@(Defn1 _ _ params' _:_)
  | length params == length params' = Right (insert name (defn:defnList) defns)
  | otherwise = Left $ newErrorMessage
                        (Message ("Arity mismatch: " ++ name
                                  ++ " defined with " ++ show (length params)
                                  ++ " arguments, previously defined with "
                                  ++ show (length params') ++ " arguments."))
                        position
