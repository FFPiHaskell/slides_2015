module Main where

{- Hier eine Passende Definition der State-Monade einfügen...  -}
{- z.B. Control.Monad.Trans.State aus dem transformers-package -}
{- oder Control.Monad.State aus dem veralteten mtl-package     -}

type CountState = (Bool, Int)
 
startState :: CountState
startState = (False, 0)

play :: String -> State CountState Int
play []     = do
              (_, score) <- get
              return score
play (x:xs) = do
 (on, score) <- get
 case x of
   'C' -> if on then put (on, score + 1) else put (on, score)
   'A' -> if on then put (on, score - 1) else put (on, score)
   'T' -> put (False, score)
   'G' -> put (True, score)
   _   -> put (on, score)
 play xs

main = print $ runState (play "GACAACTCGAAT") startState
-- -> (-3,(False,-3))
