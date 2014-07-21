module Control.Trigger.Aux
    ( 
      singleton
    ) where

singleton :: a -> [a]
singleton = (: [])
