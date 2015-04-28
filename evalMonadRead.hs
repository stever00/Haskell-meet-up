--- Using a monad to implement the evaluation example
---(c) Steve Reeves, THe University of Waikato, April 2015
---Following the Haskell Meet-Up at Movio
---My attempts at one of the problems I suggested :-)
---This follows on from evalMonad.hs

---This adds an input stream so we can have a read expression...
---The only extra lines or chnages needed are denoted with XXXX
---and most of those chnages are just forced by having the context now
---be and environment (as before) *and* an input stream

import Control.Applicative
import Control.Monad

data Expr = C Float |
            Expr :+ Expr| 
            V String |
            Let String Expr Expr |
---XXXX
            Read
            deriving Show

type Env = [(String, Float)]

---XXXX
type Stream = [Float]

--- Yes, this could be done using the pre-defined State monad...but this is more useful for learning from....

---XXXX
newtype Compute a = Comp{ compExpr :: (Env, Stream) -> (Env, Stream , a)}

instance Monad Compute where

---XXXX
  return x = Comp (\(env, is) -> (env, is , x))

---XXXX
  e >>= f = Comp(\(env, is) ->  let  (env', is', v) = compExpr e (env, is) in compExpr (f v) (env', is'))

instance Functor Compute where
  fmap = liftM

instance Applicative Compute where
  pure = return
  (<*>) = ap

---eval takes an expression and threads an environment through it, ready to compute the answer! A typical use of a monadic type

eval :: Expr -> Compute Float
eval (C x)         = return x
eval (e1 :+ e2)    = eval e1 >>= \v1 ->
                     eval e2 >>= \v2 ->
                     return (v1 + v2)

eval (V v)         = find' v

eval (Let v e1 e2) = eval e1 >>= \v1 ->
                     extend' v v1 >>= \_ -> 
                     eval e2 >>= \v2 ->
                     return v2
---XXXX
eval Read          = readis

---XXXX
---read returns the head of the input stream is
readis :: Compute Float
readis = Comp(\(env, is) -> (env, tail is, head is))


---Find a variable's value by looking in the environment
find :: String -> Env -> Float
find v  []          = error ("Unbound variable: " ++ v)
find v1 ((v2,e):es) = if v1 == v2 then e else find v1 es

---Use find properly
---find' :: String -> Env -> (Env, Float)
---find' v env = (env, find v env)
find' :: String -> Compute Float
---XXXX
find' v = Comp(\(env, is) -> (env,is, find v env))
                     
---We extend with variables that may already appear in
---the environment so as to have a sensible block
---structure, so, for example,
---evaluate (Let “x” (C 5) (Let “x” (C 4) (V “x”))
---gives 4.0 and not 5.0

extend :: String -> Float -> Env -> Env
extend v e env = (v,e):env

---Use extend properly
---extend' :: String -> Float -> Env -> (Env, Float)
---extend' v e env = (extend v e env, e)
extend' :: String -> Float -> Compute Float
---XXXX
extend' v e = Comp(\(env, is) -> (extend v e env ,is, e))

---Finally answer to start the computation with an empty environment, and returns final answer
---answer :: Expr -> Float
---XXXX
answer e =  compExpr (eval e) ([],[2.0, 1.0])

e0 = Let "x" (C 2.0) (V "x" :+ C 3.0)

e1 = Let "x" (C 2.0) (Let "y" (C 3.0) (V "x" :+ V "y"))

e2 = Let "x" (C 2.0) (Let "y" (C 3.0) (V "x" :+ V "x"))

e3 = Let "x" Read (Let "y" Read (V "x" :+ V "x"))