-- Page 390 (pdf) of Type Drive Development
module Main
import Data.Vect

data StackOp : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackOp () height (S height)
  Pop : StackOp Integer (S height) height
  Top : StackOp Integer (S height) (S height) -- this means there has to be at least one item on the stack

  GetStr : StackOp String height height
  PutStr : String -> StackOp () height height

  Pure : ty -> StackOp ty height height
  (>>=) : StackOp a height1 height2 ->
       (a -> StackOp b height2 height3) ->
       StackOp b height1 height3

data StackIO : Nat -> Type where
  Do : StackOp  a height1 height2 ->
       (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
  (>>=) : StackOp a height1 height2 ->
       (a -> Inf (StackIO height2)) -> StackIO height1
  (>>=) = Do

runStack : (stk : Vect inHeight Integer) ->
           StackOp ty inHeight outHeight ->
           IO (ty, Vect outHeight Integer)

runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)

runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr val) = do putStr val
                               pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (cmd >>= next) = do (cmdRes, newStk) <- runStack stk cmd 
                                 runStack newStk (next cmdRes)

rAdd : StackOp () (S (S height)) (S height)
rAdd = do val1 <- Pop
          val2 <- Pop
          Push (val1 + val2)

rMul : StackOp () (S (S height)) (S height)
rMul = do val1 <- Pop
          val2 <- Pop
          Push (val1 * val2)

rDot : StackOp Integer (S height) (height)
rDot = do Pop

stackResult : (Integer, Vect n Integer) -> String
stackResult (result, _) = show result

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f)
                        = do (res, newStk) <- runStack stk c
                             run fuel newStk (f res)
run Dry stk p = pure ()

data StkInput = Number Integer
              | Add

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "+" = Just Add
strToInput n = if all isDigit (unpack n)
                  then Just (Number (cast n))
                  else Nothing

mutual
  tryAdd : StackIO height
  tryAdd { height = (S (S h))}
      = do rAdd
           result <- Top
           PutStr (show result ++ "\n")
           stackCalc
  tryAdd = do PutStr "Fewer than two items on the stack\n"
              stackCalc

  stackCalc : StackIO height
  stackCalc = do PutStr ">"
                 input <- GetStr
                 case strToInput input of
                        Nothing => do PutStr "invalid op\n"
                                      stackCalc
                        Just (Number x) => do Push x
                                              stackCalc
                        Just Add => tryAdd

main : IO ()
main = run forever [] stackCalc
--main = putStrLn $ stackResult $ runStack [] (do Push 5; Push 6; rAdd; Push 7; Push 8; rAdd; rMul; rDot)
-- TODO Add subtract and multiply
-- TODO Add negate
-- TODO Add discard
-- TODO Add duplicate
-- TODO Add command to quit the stack
-- TODO Add printing the stack
-- TODO Add support for taking a string in - rather than command line interactive
