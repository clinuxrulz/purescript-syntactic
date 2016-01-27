module Language.Syntactic.Syntax where

import Prelude
import Data.Maybe
import Data.Exists
import Data.Inject

data AST sym sig
  = Sym (sym sig)
  | Ap (ApData sym sig)

infixl 1 :$

(:$) :: forall sym sig a. AST sym (a -> sig) -> AST sym a -> AST sym sig
(:$) a b = Ap $ mkExists $ ApDataF a b

data ApDataF sym sig a = ApDataF (AST sym (a -> sig)) (AST sym a)
type ApData sym sig = Exists (ApDataF sym sig)

instance functorAST :: (Functor sym) => Functor (AST sym) where
  map f (Sym s) = Sym (map f s)
  map f (Ap apData) =
    runExists
      (\(ApDataF s a) -> Ap $ mkExists $ ApDataF (map (map f) s) a)
      apData

instance injectAST :: (Inject sub sup) => Inject sub (AST sup) where
  prj (Sym s) = prj s
  prj _ = Nothing
  inj = Sym <<< inj

size :: forall sym sig. AST sym sig -> Int
size (Sym _) = 1
size (Ap apData) = runExists (\(ApDataF s a) -> size s + size a) apData
