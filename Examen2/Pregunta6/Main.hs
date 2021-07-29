import System.Environment
import System.Directory 

import qualified Data.Map as Map
import qualified Machine as M
import Data.List (elemIndex)
import Data.List.Extra (trim)

-- Main function where the execution starts
main :: IO()
main = do
    f <- getArgs
    case f of
        [filePath] -> do 
            check <- doesFileExist filePath
            if check then do
                program <- readFile filePath
                let insts = map words $ lines program
                    (labels, parsedInst) = parse insts 
                M.run parsedInst labels
            else error $ "error: file '" ++ filePath ++ "' does not exist"
        _ -> error "error: no file path provided"
 
 -- Helper to insert more parameters to the parse' function
parse :: [[String]] -> (M.Labels, M.Instructions)
parse = parse' 0 

-- Function to parse instructions from input. It will fail with an error
-- if input is incorrect and can not be parsed. Returns the map containing
-- the position of the instruction for each label found and the instructions 
-- parsed
parse' :: Int -> [[String]] -> (M.Labels, M.Instructions)
parse' _ [] = (Map.empty, [])
parse' pos (x: xs)
    | null x = parse' pos xs
    | isLabel $ head x 
        = (Map.insert (init $ head x) pos labels, getInst (tail x) : inst)
    | otherwise
        = (labels, getInst x : inst)
    where (labels, inst) = parse' (pos+1) xs

-- Function to check if a string is a label by searching for the character ':'
isLabel :: String -> Bool
isLabel inst = case elemIndex ':' inst of
    Nothing -> False
    Just _  -> True

-- Function to parse a single instruction
getInst :: [String] -> M.Inst
getInst [] = M.Skip
getInst ["PUSH", "true"] = M.Push $ M.Boolean True
getInst ["PUSH", "false"] = M.Push $ M.Boolean False
getInst ["PUSH", val]
    | M.isValidNumber val = M.Push $ M.Numerical (read val :: Int)
    | otherwise = error $ "error: expression " ++ val ++ " is not a valid input"
getInst ["POP"] = M.Pop
getInst ["ADD"] = M.NumericalOp (+)
getInst ["SUB"] = M.NumericalOp (-)
getInst ["MUL"] = M.NumericalOp (*)
getInst ["DIV"] = M.NumericalOp div
getInst ["AND" ] = M.BooleanOp (&&)
getInst ["OR"] = M.BooleanOp (||)
getInst ["LT"] = M.RelationalOp (<)
getInst ["GT"] = M.RelationalOp (>)
getInst ["LE"] = M.RelationalOp (<=)
getInst ["GE"] = M.RelationalOp (>=)
getInst ["EQ"] = M.Eq
getInst ["NEQ"] = M.Neq
getInst ["UMINUS"] = M.Uminus
getInst ["NOT"] = M.Not 
getInst ["RVALUE", id] = M.Rval id
getInst ["LVALUE", id] = M.Lval id
getInst ["ASSIGN"] = M.Assign 
getInst ["GOTO", label] = M.Goto label 
getInst ["GOTRUE", label] = M.Gotrue label 
getInst ["GOFALSE", label] = M.Gofalse label
getInst ["READ", id] = M.Read id 
getInst ["PRINT", id] = M.Print id 
getInst ["RESET"] = M.Reset 
getInst ["EXIT"] = M.Exit
getInst s = error $ "error: instruction " ++ unwords s ++ " could not be parsed"