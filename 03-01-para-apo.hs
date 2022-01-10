{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Arrow

data Expr a
    = Index { target :: a, idx :: a }
    | Call { func :: a, args :: [a]}
    | Unary { op :: String, target :: a }
    | Binary { lhs :: a, op :: String, rhs :: a }
    | Paren { target :: a }
    | Ident { name :: String }
    | Literal { intVal :: Int }
    deriving ( Show, Eq, Functor, Foldable, Traversable )

newtype Term f
    = In { out :: f (Term f) }

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata fn =
    out
    >>> fmap (cata fn)
    >>> fn

type RAlgebra f a = f (Term f, a) -> a

para :: Functor f -> RAlgebra f a -> Term f -> a
