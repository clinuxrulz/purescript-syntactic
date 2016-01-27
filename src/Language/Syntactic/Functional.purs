module Language.Syntactic.Functional where

import Language.Syntactic.Syntax

import Prelude
import Control.Monad.Cont.Trans
import Data.Exists
import Data.Identity
import Data.Inject
import Data.Either
import Data.Functor.Coproduct

newtype Name = Name Int

data LiteralF sig a = LiteralF a

newtype Literal sig = Literal (Exists (LiteralF sig))

literalLit :: forall a. a -> Literal a
literalLit a = Literal $ mkExists $ LiteralF a

lit :: forall dom a. (Inject (AST Literal) dom)
    => a
    -> AST dom a
lit a = inj <<< Sym $ literalLit a

data Binding sig
  = Var Name
  | Lam Name

bindingVar :: forall a. Name -> Binding a
bindingVar = Var

bindingLam :: forall a b. Name -> Binding (b -> (a -> b))
bindingLam = Lam

var :: forall dom a. (Inject (AST Binding) dom)
    => Name
    -> AST dom a
var name = inj <<< Sym $ bindingVar name

lam :: forall dom a b. (Inject (AST Binding) dom)
    => Name
    -> AST dom (b -> (a -> b))
lam name = inj <<< Sym $ bindingLam name

data MONAD (m :: * -> *) sig
  = Return
  | Bind

monadReturn :: forall m a. MONAD m (a -> m a)
monadReturn = Return

monadBind :: forall m a b. MONAD m (m a -> (a -> m b) -> m b)
monadBind = Bind

mReturn :: forall dom m a. (Inject (MONAD m) dom) => AST dom a -> AST dom (m a)
mReturn a = Sym (inj monadReturn) :$ a

mBind :: forall dom m a b. (Inject (MONAD m) dom) => AST dom (m a) -> AST dom (a -> m b) -> AST dom (m b)
mBind ma f = Sym (inj monadBind) :$ ma :$ f

newtype Remon sym m a = Remon (forall r. ContT (AST sym (m r)) Identity a)

unRemon :: forall sym m a. Remon sym m a -> (forall r. ContT (AST sym (m r)) Identity a)
unRemon (Remon a) = a

instance functorRemon :: Functor (Remon sym m) where
  map f a = Remon (f <$> (unRemon a))

instance applyRemon :: Apply (Remon sym m) where
  apply f a = Remon ((unRemon f) <*> (unRemon a))

instance applicativeRemon :: Applicative (Remon sym m) where
  pure a = Remon (pure a)

instance bindRemon :: Bind (Remon sym m) where
  bind ma f = Remon ((unRemon ma) >>= (\x -> unRemon (f x)))

instance monadRemon :: Monad (Remon sym m)

desugarMonad :: forall sym m a. (Inject (MONAD m) sym) => Remon sym m (AST sym a) -> AST sym (m a)
desugarMonad a = runIdentity (runContT (unRemon a) (\x -> return $ mReturn x))

class Eval s where
  eval :: forall sig. s sig -> sig

instance evalCoproduct :: (Eval s, Eval t) => Eval (Coproduct s t) where
  eval (Coproduct a) = either eval eval a

instance evalAst :: (Eval s) => Eval (AST s) where
  eval (Sym a) = eval a
  eval (Ap apData) = runExists (\(ApDataF f a) -> eval f $ eval a) apData
