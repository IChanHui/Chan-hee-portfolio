
{-# LANGUAGE TemplateHaskell #-}

module LensTest where 

import Control.Lens 


data Person a b = Person { _name :: a 
                         , _age :: b 
                         } 


makeLenses ''Person

