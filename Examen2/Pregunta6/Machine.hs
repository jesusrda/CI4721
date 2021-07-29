module Machine where

import qualified Data.Map as M
import Data.List.Extra (trim)
import Data.Char (isDigit)
import Data.Functor ((<&>))

-- Data type definitions to use in the execution of the machine

type Id = String
type Label = String

type Labels = M.Map Label Int
type IdValues = M.Map Id Value

type Instructions = [Inst]
type Stack = [Value]

-- Possible values
data Value  = Numerical Int
            | Boolean Bool
            | Id String
            deriving(Eq)

instance Show Value where
    show (Numerical n) = show n
    show (Boolean b) = show b
    show (Id s) = s 

-- Instructions
data Inst = Push Value
          | Pop
          
          | NumericalOp (Int -> Int -> Int) 
          | BooleanOp (Bool -> Bool -> Bool)
          | RelationalOp (Int -> Int -> Bool)

          | Eq
          | Neq

          | Uminus
          | Not

          | Rval Id
          | Lval Id
          | Assign
          
          | Goto Label
          | Gotrue Label 
          | Gofalse Label
          
          | Read Id
          | Print Id
        
          | Skip
          | Reset
          | Exit

instance Show Inst where
    show (Push _) = "PUSH"
    show Pop = "POP"
    show (NumericalOp _) = "NUMERICAL OPERATOR"
    show (BooleanOp _) = "BOOLEAN OPERATOR"
    show (RelationalOp _) = "RELATIONAL OPERATOR"
    show Uminus = "UMINUS"
    show Not = "NOT"
    show Eq = "EQ"
    show Neq = "NEQ"
    show (Rval val) = "RVAL " ++ val
    show (Lval val) = "LVAL " ++ val
    show Assign = "ASSIGN"
    show (Goto label) = "GOTO " ++ label
    show (Gotrue label) = "GOTRUE " ++ label
    show (Gofalse label) = "GOFALSE " ++ label
    show (Read var) = "READ " ++ var
    show (Print var) = "PRINT " ++ var
    show Reset = "RESET"
    show Exit = "EXIT"


-- Function to check if a string can be parsed as a number
isValidNumber :: String -> Bool
isValidNumber (c:s) = (isDigit c || c == '-') && all isDigit s

-- Helper function to add more parameters to the run' function
run :: Instructions -> Labels -> IO ()
run [] _ = return ()
run inst labels = run' 0 inst [] labels M.empty


-- Main flow of execution. Uses pattern matching to figure out the
-- instruction to process, checks if the conditions are met to be executed,
-- runs the command and updates the stack and memory data structures
run' :: Int -> Instructions -> Stack -> Labels -> IdValues -> IO ()
run' pos inst stack labels values = do
    if pos >= length inst 
        then return ()
        else do
            let currInst = inst !! pos
            case currInst of
                Skip -> run' (pos+1) inst stack labels values
                Reset -> run' (pos+1) inst [] M.empty M.empty
                Exit -> return ()
                Read id -> do
                    value <- getLine <&> trim
                    case value of
                        "true" ->
                            run' (pos+1) inst stack labels $ M.insert id (Boolean True) values
                        "false" ->
                            run' (pos+1) inst stack labels $ M.insert id (Boolean True) values
                        val -> if isValidNumber val 
                                then run' (pos+1) inst stack labels $ M.insert id (Numerical (read val :: Int)) values
                                else do
                                    putStrLn $ "error: expression " ++ show value ++ " is not a valid input"
                                    run' pos inst stack labels values
                Print id -> do
                    case M.lookup id values of
                        Just val -> do 
                            print val
                            run' (pos+1) inst stack labels values
                        Nothing -> do
                            putStrLn $ "Id " ++ id ++ " has no value associated"
                            run' (pos+1) inst stack labels values
                _ -> do
                    case runInst currInst pos stack labels values of
                        Left (newPos, newStack, newValues) ->
                            run' newPos inst newStack labels newValues
                        Right error -> do
                            putStrLn error
                            run' (pos+1) inst stack labels values

-- Function that executes a single command
runInst :: Inst -> Int -> Stack -> Labels -> IdValues -> Either (Int, Stack, IdValues) String

runInst (Push val) pos stack _ values = Left (pos+1, val : stack, values)

runInst Pop pos (x:xs) _ values = Left (pos+1, xs, values)

runInst inst@(NumericalOp op) pos (opr1:opr2:ops) _ values = 
    case (opr1, opr2) of
        (Numerical a, Numerical b) -> Left (pos+1, Numerical (op b a) : ops, values) 
        _ -> Right $ "error: can not use numerical operator over " ++ show opr1 ++ " and " ++ show opr2


runInst inst@(BooleanOp op) pos (opr1:opr2:ops) _ values = 
    case (opr1, opr2) of
        (Boolean a, Boolean b) -> Left (pos+1, Boolean (op b a) : ops, values) 
        _ -> Right $ "error: can not use boolean operator over " ++ show opr1 ++ " and " ++ show opr2

runInst inst@(RelationalOp op) pos (opr1:opr2:ops) _ values = 
    case (opr1, opr2) of
        (Numerical a, Numerical b) -> Left (pos+1, Boolean (op b a) : ops, values) 
        _ -> Right $ "error: can not use relational operator over " ++ show opr1 ++ " and " ++ show opr2

runInst Eq pos (opr1:opr2:ops) _ values = 
    case (opr1, opr2) of
        (Numerical a, Numerical b) -> Left (pos+1, Boolean (b == a) : ops, values)
        (Boolean a, Boolean b) -> Left (pos+1, Boolean (b == a) : ops, values) 
        _ -> Right $ "error: can not use equal operator over " ++ show opr1 ++ " and " ++ show opr2

runInst Neq pos (opr1:opr2:ops) _ values = 
    case (opr1, opr2) of
        (Numerical a, Numerical b) -> Left (pos+1, Boolean (b /= a) : ops, values)
        (Boolean a, Boolean b) -> Left (pos+1, Boolean (b /= a) : ops, values) 
        _ -> Right $ "error: can not use not equal operator over " ++ show opr1 ++ " and " ++ show opr2

runInst Uminus pos (opr:ops) _ values = 
    case opr of
        (Numerical a) -> Left (pos+1, Numerical (-a) : ops, values) 
        _ -> Right $ "error: can not use numerical operator minus over " ++ show opr

runInst Not pos (opr:ops) _ values = 
    case opr of
        (Boolean a) -> Left (pos+1, Boolean (not a) : ops, values) 
        _ -> Right $ "error: can not use boolean operator not over " ++ show opr

runInst (Rval idName) pos stack _ values = 
    case M.lookup idName values of
        Just val -> Left (pos+1, val : stack, values) 
        _ -> Right $ "error: RVAL: id " ++ idName ++ " has not value associated"

runInst (Lval idName) pos stack _ values = 
    Left (pos+1, Id idName : stack, values) 

runInst Assign pos (lval:rval:xs) _ values = 
    case lval of
        Id idName -> case rval of
            Id _ -> Right "error: ASSIGN: an lvalue can not be assigned"
            val -> Left(pos+1, xs, M.insert idName val values)
        _ -> Right "error: ASSIGN: expression not assignable"

runInst (Goto label) _ stack labels values =
    case M.lookup label labels of
        Just pos -> Left (pos, stack, values)
        _ -> Right $ "error: GOTO: label " ++ label ++ " is not associated with an instruction"

runInst (Gotrue label) pos (val : vals) labels values =
    case M.lookup label labels of
        Just newPos -> case val of
            Boolean True -> Left (newPos, vals, values)
            Boolean False -> Left (pos+1, vals, values)
            _ -> Right "error: GOTRUE: top of stack is not a boolean value"
        _ -> Right $ "error: GOTRUE: label " ++ label ++ " is not associated with an instruction"

runInst (Gofalse label) pos (val : vals) labels values =
    case M.lookup label labels of
        Just newPos -> case val of
            Boolean True -> Left (pos+1, vals, values)
            Boolean False -> Left (newPos, vals, values)
            _ -> Right "error: GOFALSE: top of stack is not a boolean value"
        _ -> Right $ "error: GOFALSE: label " ++ label ++ " is not associated with an instruction"

runInst inst _ _ _ _ = Right $ "error: " ++ show inst ++ ": not enough arguments in stack"
