{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Arrow
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

import Data.List ( intercalate )

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

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]})

bottomUp, topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
    out >>> fmap (bottomUp fn) >>> In >>> fn
topDown fn =
    In <<< fmap (topDown fn) <<< out <<< fn

-- type given by checking in GHCi
mystery :: (Functor f) => (f a -> a) -> Term f -> a
mystery fn =
    out
    >>> fmap (mystery fn)
    >>> fn


countNodes :: Expr Int -> Int
countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + 1 + right
countNodes (Call fn args)        = 1 + fn + sum args
countNodes (Index it idx)        = 1 + it + idx
countNodes (Paren arg)           = arg + 1
countNodes (Literal _)           = 1
countNodes (Ident  _)           = 1

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata = mystery

showExpression :: Algebra Expr String
showExpression (Unary op arg)             = op ++ arg
showExpression (Binary left op arg)       = left ++ " " ++ op ++ " " ++ arg
showExpression (Call fn args)             = fn ++ "(" ++ (intercalate "," args)  ++ ")"
showExpression (Index it idx)             = it ++ "[" ++ idx ++ "]"
showExpression (Paren arg)                = "(" ++ arg ++ ")"
showExpression (Literal l)                = show l
showExpression (Ident id)                 = id

type Expr' = Term Expr

instance Show Expr' where
     show = cata showExpression

comma = P.text ","
-- now using a nice library
prettyPrint :: Algebra Expr Doc
prettyPrint (Literal l) = P.int l
prettyPrint (Ident id) = P.text id
prettyPrint (Call fn as) = fn <> P.parens (mconcat (P.punctuate comma as))
prettyPrint (Index it idx) = it <> P.brackets idx
prettyPrint (Unary op it) = P.text op <> it
prettyPrint (Binary l op r) = l <> P.text op <> r
prettyPrint (Paren exp) = P.parens exp

bottomUp' f = cata (In >>> f)

-- ana with a _hole for type-checking:
-- reversed f = In <<< fmap (_what f) <<< f

type Coalgebra f a = a -> f a -- for eg building Expr out of String
-- Coalgebra implements read-type opertions, where Algebra implements show

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

nested :: Int -> Term Expr -- where Expr is nested Paren around an int
nested n = ana go n where
  -- go-style of recursion, also captures n in closure
  go :: Coalgebra Expr Int
  go 0 = Literal n
  go n = Paren (n - 1)

tenplusten = In (Binary {lhs = ten, op = "+", rhs = ten} )
tenplus_tenplusten = In (Binary {lhs = ten, op ="+", rhs = tenplusten})

-- intSum :: [Int] -> Term Expr
-- want this to become a + b + c ...
--  but can't get the type of rhs of Binary to work out

-- readExpression :: Coalgebra Expr String
-- run into trouble with parens versus calls,
--   maybe we need a para/apomorphism here?
--   ie the fact that show is a cata is due to degeneracy,
--   ie show is better thought of as a higher-level scheme
--   that in this case simplifies down to a cata
