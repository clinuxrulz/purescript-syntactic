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
    -> ASTF dom Number
lit' a = inj (Lit a)

add' :: forall dom. (Inject Math dom)
     => ASTF dom Number
     -> ASTF dom Number
     -> ASTF dom Number
add' a b = inj Add :$ a :$ b

sub' :: forall dom. (Inject Math dom)
     => ASTF dom Number
     -> ASTF dom Number
     -> ASTF dom Number
sub' a b = inj Sub :$ a :$ b

times' :: forall dom. (Inject Math dom)
       => ASTF dom Number
       -> ASTF dom Number
       -> ASTF dom Number
times' a b = inj Times :$ a :$ b

divide' :: forall dom. (Inject Math dom)
        => ASTF dom Number
        -> ASTF dom Number
        -> ASTF dom Number
divide' a b = inj Divide :$ a :$ b    

evalMathSym :: forall a b. Math a -> b
evalMathSym (Lit a) = unsafeCoerce a
evalMathSym Add = unsafeCoerce $ (+) :: Number -> Number -> Number
evalMathSym Sub = unsafeCoerce $ (-) :: Number -> Number -> Number
evalMathSym Times = unsafeCoerce $ (*) :: Number -> Number -> Number
evalMathSym Divide = unsafeCoerce $ (/) :: Number -> Number -> Number

instance evalMath :: Eval Math where
  evalSym = evalMathSym

expr1 :: forall dom. (Inject Math dom)
      => ASTF dom Number
expr1 = (lit' 3.0) `times'` ((lit' 4.0) `add'` (lit' 5.0))

test1 :: forall eff. Test (testOutput :: TestOutput | eff)
test1 = do
  test "test1" do
    assert "test1 failed" $ (evalDen (expr1 :: ASTF Math Number)) == 27.0
  return unit
