{-# LANGUAGE TemplateHaskell #-}

module LensTest where

import Prelude hiding ((.))
import Control.Category
import Data.Lens.Lazy
import Data.Lens.Template 

data Address = Address { _city :: String, _street :: String } deriving Show

data Person = Person { _name :: String, _address :: Address } deriving Show
 
$( makeLenses [''Person, ''Address] )

test = Person "Yakov" (Address "Ryazan" "Polyetaeva")

main = print $ name ^= "Artem" $ test