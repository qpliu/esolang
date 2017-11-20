import Control.Monad(zipWithM)
import System.Environment(getArgs)
import Ast(Module)
import Link(link)
import Parse(parse)
import StdLib(stdlib)

main :: IO ()
main = getArgs >>= dgol

dgol :: [String] -> IO ()
dgol args = do
     files <- mapM readFile args
     either putStrLn run (zipWithM parse args files)

run :: [Module] -> IO ()
run modules = either putStrLn id (link modules stdlib)
