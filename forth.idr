-- Page 390 (pdf) of Type Drive Development
module Main
import Data.Vect

data StackOp : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackOp () height (S height)
  Pop : StackOp Integer (S height) height
  Top : StackOp Integer (S height) (S height) -- this means there has to be at least one item on the stack

  Pure : ty -> StackOp ty height height
  (>>=) : StackOp a height1 height2 ->
       (a -> StackOp b height2 height3) ->
       StackOp b height1 height3

testAdd : StackOp Integer 0 0
testAdd = do Push 10
             Push 2 --try removing this line, you'll see it fails to compile.
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)

-- try this with runStack [] testOnePopAdd - it should fail
-- then try runStakc [1] testOnePopAdd - should return 11
testOnePopAdd : StackOp Integer 1 0
testOnePopAdd = do Push 10
                   val1 <- Pop
                   val2 <- Pop
                   Pure (val1 + val2)

runStack : (stk : Vect inHeight Integer) ->
           StackOp ty inHeight outHeight ->
           (ty, Vect outHeight Integer)

runStack stk (Push val) = ((), val :: stk)
runStack (val :: stk) Pop = (val, stk)
runStack (val :: stk) Top = (val, val :: stk)

runStack stk (Pure x) = (x, stk)
runStack stk (cmd >>= next)
  = let (cmdRes, newStk) = runStack stk cmd in
        runStack newStk (next cmdRes)

rAdd : StackOp () (S (S height)) (S height)
rAdd = do val1 <- Pop
          val2 <- Pop
          Push (val1 + val2)

rDot : StackOp Integer (S height) (height)
rDot = do Pop

stackResult : (Integer, Vect 0 Integer) -> String
stackResult (result, _) = show result

main : IO ()
main = putStrLn $ stackResult $ runStack [1, 3] (do rAdd; rDot)
