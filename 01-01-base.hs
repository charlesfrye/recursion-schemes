data Lit
    = StrLit String
    | IntLit Int
    | Ident String
    deriving ( Show, Eq )

data Expr
    = Index Expr Expr
    | Call Expr [Expr]
    | Unary String Expr
    | Binary Expr String Expr
    | Paren Expr
    | Literal Lit
    deriving ( Show, Eq )

data Stmt
    = Break
    | Continue
    | Empty
    | IfElse Expr [Stmt] [Stmt]
    | Return (Maybe Expr)
    | While Expr [Stmt]
    | Expression Expr
    deriving ( Show, Eq )

-- turning (((anArray[(10)])))
-- into anArray[10]

flatten :: Expr -> Expr
flatten (Literal i) = Literal i
flatten (Paren e) = flatten e -- remove Paren, recurse
-- want to say something like
-- flatten x = recurse flatten x,
--  but can't so we need to explicitly pattern-match
flatten (Index e i) = Index (flatten e) (flatten i)
flatten (Call e args) = Call (flatten e) (map flatten args)
flatten (Unary op arg) = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)

applyExpr :: (Expr -> Expr) -> Expr -> Expr
applyExpr f (Literal i) = Literal i
applyExpr f (Paren p) = Paren (f p)
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Unary op arg) = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)

flatten' :: Expr -> Expr
flatten' (Paren e) = flatten' e
flatten' x = applyExpr flatten' x
