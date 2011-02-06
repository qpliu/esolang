import System.Environment(getArgs)

import Compiler(compile)
import Flattener(flatten)
import Parser(parse)
import Reader(Value(Cons,Nil))

main :: IO ()
main = do
    args <- getArgs
    src <- handleArgs args
    (putStr . compile . flatten . parse . read) src

handleArgs :: [String] -> IO String
handleArgs [] = getContents
handleArgs (filename:_) = readFile filename
