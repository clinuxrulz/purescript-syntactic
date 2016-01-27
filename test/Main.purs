module Test.Main where

import Language.Syntactic.Syntax
import Language.Syntactic.Functional

import Prelude
import Control.Monad.Eff
import Data.Inject
import Unsafe.Coerce

import Test.Unit
import Test.Unit.Console

main :: forall eff. Eff (testOutput :: TestOutput | eff) Unit
main = runTest do
  test1

data Math a
  = Lit Number
  | Add
  | Sub
  | Times
  | Divide

lit' :: forall dom. (Inject Math dom)
    => Number
    -> AST dom Number
lit' a = inj (Lit a)

add' :: forall dom. (Inject Math dom)
     => AST dom Number
     -> AST dom Number
     -> AST dom Number
add' a b = inj Add :$ a :$ b

sub' :: forall dom. (Inject Math dom)
     => AST dom Number
     -> AST dom Number
     -> AST dom Number
sub' a b = inj Sub :$ a :$ b

times' :: forall dom. (Inject Math dom)
       => AST dom Number
       -> AST dom Number
       -> AST dom Number
times' a b = inj Times :$ a :$ b

divide' :: forall dom. (Inject Math dom)
        => AST dom Number
        -> AST dom Number
        -> AST dom Number
divide' a b = inj Divide :$ a :$ b    

{-
evalMath :: forall a. Math a -> a
evalMath (Lit a) = unsafeCoerce $ a
evalMath Add = unsafeCoerce $ (+) :: Number -> Number -> Number
evalMath Sub = unsafeCoerce $ (-) :: Number -> Number -> Number
evalMath Times = unsafeCoerce $ (*) :: Number -> Number -> Number
evalMath Divide = unsafeCoerce $ (/) :: Number -> Number -> Number
-}

instance evalMath :: Eval Math where
  eval (Lit a) = unsafeCoerce $ a
  eval Add = unsafeCoerce $ (+) :: Number -> Number -> Number
  eval Sub = unsafeCoerce $ (-) :: Number -> Number -> Number
  eval Times = unsafeCoerce $ (*) :: Number -> Number -> Number
  eval Divide = unsafeCoerce $ (/) :: Number -> Number -> Number

expr1 :: forall dom. (Inject Math dom)
      => AST dom Number
expr1 = (lit' 3.0) `times'` ((lit' 4.0) `add'` (lit' 5.0))

test1 :: forall eff. Test (testOutput :: TestOutput | eff)
test1 = do
  test "test1" do
    assert "test1 failed" $ (eval (expr1 :: AST Math Number)) == 27.0
  return unit
