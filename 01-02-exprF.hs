{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
import Control.Arrow

-- import Numeric.Natural
-- where is fix defined? and when? is this pseudocode?
--countDown :: Natural -> IO ()
--countDown = fix $ \recur n -> do
--  putStrLn (show n)
--  when (n /= 0) (recur (n - 1))


data Lit
    = StrLit String
    | IntLit Int
    | Ident String
    deriving ( Show, Eq )

-- this is a bunch of types, just like [a]
data ExprF a -- forall a, including ExprF a
    = Index a a
    | Call a [a]
    | Unary String a
    | Binary a String a
    | Paren a
    | Literal Lit
    deriving ( Show, Eq, Functor, Foldable, Traversable )

newtype Term f
    = In { out :: f (Term f) }

-- this is a single type, packing them all together
type Expr = Term ExprF

ten, add, call :: Expr
ten  = In (Literal (IntLit 10))
add  = In (Literal (Ident "add"))
call = In (Call add [ten, ten])

topDown, bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f =
    In <<< fmap (topDown f) <<< out <<< f
bottomUp fn =
    out >>> fmap (bottomUp fn) >>> In >>> fn

flattenTerm :: Expr -> Expr
flattenTerm (In (Paren e)) = e
flattenTerm other = other

flatten'' :: Expr -> Expr
flatten'' = bottomUp flattenTerm

is_ten :: Expr -> Bool
is_ten (In (Literal (IntLit 10))) = True
is_ten _ = False
