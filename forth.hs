module Main where

-- StackOp is an operation between two stacks
-- Each stack contains the stack [Int] and maybe a result
-- From the previous Op.
-- TODO This is very messy, maybe this is what monads are for?
type StackOp = (Maybe Int, [Int]) -> (Maybe Int, [Int])

push :: Int -> StackOp
push n = \(x, stk) -> (x, n : stk)

add :: StackOp
add (_, []) = error "empty list"
add (_, [x]) = error "not enough items"
add (_, (a:b:xs)) = (Nothing, a+b : xs)

mul :: StackOp
mul (_, []) = error "empty list"
mul (_, [x]) = error "not enough items"
mul (_, (a:b:xs)) = (Nothing, a*b : xs)

dot :: StackOp
dot (_, []) = error "empty list"
dot (_, (x:xs)) = (Just x, xs)

run :: [Int] -> [StackOp] -> (Maybe Int, [Int])
run stk [] = (Nothing, stk)
run stk [op] = op Nothing stk
run stk ((_, op): ops) = run (op stk) ops

-- TODO maybe this isn't needed
showResult :: (Maybe Int, [Int]) -> String
showResult (Nothing, _) = ""
showResult (Just x, _) = show x

main :: IO ()
main = putStrLn $ show $ run [] [push 4, push 5]
