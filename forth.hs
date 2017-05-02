module Main where

-- TODO Maybe this should be a a Union Type
-- with one option being [Int] -> [Int] and
-- another being [Int] -> (Int, [Int]
type StackOp = [Int] -> (Maybe Int, [Int])

type OpWithResult = (Maybe Int, [Int])
instance Monad OpWithResult where
    op1 >>= op2 = \(_, stk) -> op2
    return stk = \res -> (res, stk)

push :: Int -> StackOp
push n = \stk -> (Nothing, n : stk)

add :: StackOp
add [] = error "empty list"
add [x] = error "not enough items"
add (a:b:xs) = (Nothing, a+b : xs)

mul :: StackOp
mul [] = error "empty list"
mul [x] = error "not enough items"
mul (a:b:xs) = (Nothing, a*b : xs)

dot :: StackOp
dot [] = error "empty list"
dot (x:xs) = (Just x, xs)

run :: [Int] -> [StackOp] -> (Maybe Int, [Int])
run stk [] = (Nothing, stk)
run stk [op] = op stk
run stk (op: ops) = run (op stk) ops

-- TODO maybe this isn't needed
showResult :: (Maybe Int, [Int]) -> String
showResult (Nothing, _) = ""
showResult (Just x, _) = show x

main :: IO ()
main = putStrLn $ show $ run [] [push 4, push 5]
