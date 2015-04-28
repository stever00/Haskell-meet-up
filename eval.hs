---Very simple evaluation for arithmetic expressions with only constants 
---(and only "plus"...obviously extendable to other operations)

data Expr = C Float |
            Expr :+ Expr
            deriving Show

eval :: Expr -> Float
eval (C x)         = x
eval (e1 :+ e2)    =  let v1 = eval e1
                          v2 = eval e2
                       in (v1 + v2)

e0 = (C 2.0) :+ (C 3.0)
