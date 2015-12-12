module Language.Syntactic.Functional where

import Language.Syntactic.Syntax

import Prelude
import Control.Monad.Cont.Trans
import Data.Exists
import Data.Identity
import Data.Inject
import Data.Either
import Data.Functor.Coproduct
import Type.Proxy
import Unsafe.Coerce

newtype Name = Name Int

data LiteralF sig a = LiteralF a

newtype Literal sig = Literal (Exists (LiteralF sig))

literalLit :: forall a. a -> Literal (Full a)
literalLit a = Literal $ mkExists $ LiteralF a

lit :: forall dom a. (Inject (AST Literal) dom)
    => a
    -> ASTF dom a
lit a = inj <<< Sym $ literalLit a

data Binding sig
  = Var Name
  | Lam Name

bindingVar :: forall a. Name -> Binding (Full a)
bindingVar = Var

bindingLam :: forall a b. Name -> Binding (Partial b (Full (a -> b)))
bindingLam = Lam

var :: forall dom a. (Inject (AST Binding) dom)
    => Name
    -> ASTF dom a
var name = inj <<< Sym $ bindingVar name

lam :: forall dom a b. (Inject (AST Binding) dom)
    => Name
    -> AST dom (Partial b (Full (a -> b)))
lam name = inj <<< Sym $ bindingLam name

data MONAD m sig
  = Return (Proxy (m sig))
  | Bind (Proxy (m sig))

monadReturn :: forall m a. MONAD m (Partial a (Full (m a)))
monadReturn = Return (Proxy :: Proxy (m (Partial a (Full (m a)))))

monadBind :: forall m a b. MONAD m (Partial (m a) (Partial (a -> m b) (Full (m b))))
monadBind = Bind (Proxy :: Proxy (m (Partial (m a) (Partial (a -> m b) (Full (m b))))))

mReturn :: forall dom m a. (Inject (MONAD m) dom) => ASTF dom a -> ASTF dom (m a)
mReturn a = Sym (inj monadReturn) :$ a

mBind :: forall dom m a b. (Inject (MONAD m) dom) => ASTF dom (m a) -> ASTF dom (a -> m b) -> ASTF dom (m b)
mBind ma f = Sym (inj monadBind) :$ ma :$ f

newtype Remon sym m a = Remon (forall r. ContT (ASTF sym (m r)) Identity a)

unRemon :: forall sym m a. Remon sym m a -> (forall r. ContT (ASTF sym (m r)) Identity a)
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

desugarMonad :: forall sym m a. (Inject (MONAD m) sym) => Remon sym m (ASTF sym a) -> ASTF sym (m a)
desugarMonad a = runIdentity (runContT (unRemon a) (\x -> return $ mReturn x))

class Denotation a b

instance partialDenotation :: (Denotation b c) => Denotation (Partial a b) (a -> c)

instance fullDenotation :: Denotation (Full a) a

class Eval s where
  evalSym :: forall sig sig2. (Denotation sig sig2) => s sig -> sig2

instance evalCoproduct :: (Eval s, Eval t) => Eval (Coproduct s t) where
  evalSym (Coproduct a) = either evalSym evalSym a

evalDen :: forall s sig sig2. (Denotation sig sig2, Eval s) => AST s sig -> sig2
evalDen = go
  where
    go :: (Denotation sig sig2, Eval s) => AST s sig -> sig2
    go (Sym s) = evalSym s
    go (Ap apData) = runExists
      (\(ApDataF s a) -> unsafeCoerce (go (unsafeCoerce s)) $ (go (unsafeCoerce a)))
      apData
