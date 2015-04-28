---Evaluation example with variables and lets and with the plumbing explicit

data Expr = C Float |
            Expr :+ Expr| 
            V String |
            Let String Expr Expr
            deriving Show

type Env = [(String, Float)]

eval :: Expr -> Env -> (Env, Float)
eval (C x)  env       = (env, x)
eval (e1 :+ e2) env   =  let (env1, v1) = eval e1 env
                             (env2, v2) = eval e2 env1
                          in (env2, v1 + v2)

eval (V v)  env       = (env, find v env)

eval (Let v e1 e2) env = let (env1, v1) = eval e1 env
                             env2       = extend v v1 env1 
                             ans = eval e2 env2
                         in  ans

---Find a variable's value by looking in the environment
find :: String -> Env -> Float
find v  []          = error ("Unbound variable: " ++ v)
find v1 ((v2,e):es) = if v1 == v2 then e else find v1 es
                     
---We extend with variables that may already appear in
---the environment so as to have a sensible block
---structure, so, for example,
---evaluate (Let “x” (C 5) (Let “x” (C 4) (V “x”))
---gives 4.0 and not 5.0

extend :: String -> Float -> Env -> Env
extend v e env = (v,e):env

---Finally answer to start the computation with an empty environment, and returns final answer
answer :: Expr -> (Env, Float)
answer e = eval e []

e0 = Let "x" (C 2.0) (V "x" :+ C 3.0)

e1 = Let "x" (C 2.0) (Let "y" (C 3.0) (V "x" :+ V "y"))

e2 = Let "x" (C 2.0) (Let "y" (C 3.0) (V "x" :+ V "x"))