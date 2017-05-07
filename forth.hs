module Main where

-- TODO Maybe this should be a a Union Type
-- with one option being [Int] -> [Int] and
-- another being [Int] -> (Int, [Int]
type StackOp = [Int] -> [Int]

push :: Int -> StackOp
push n = \stk -> n : stk

add :: StackOp
add [] = error "empty list"
add [x] = error "not enough items"
add (a:b:xs) = a+b : xs

mul :: StackOp
mul [] = error "empty list"
mul [x] = error "not enough items"
mul (a:b:xs) = a*b : xs

-- TODO decide how dot should work.
dot :: StackOp
dot [] = error "empty list"
dot (x:xs) = xs

run :: [Int] -> [StackOp] -> [Int]
run stk [] = stk
run stk [op] = op stk
run stk (op: ops) = run (op stk) ops

parse :: [String] -> [StackOp]
parse [] = []
parse ("*": xs) = mul : parse xs
parse ("+": xs) = add : parse xs
parse (x: xs) = push (read x :: Int) : parse xs

main :: IO ()
main = putStrLn $ show $ run [] $ parse $ words "5 6 + 7 8 + *"
