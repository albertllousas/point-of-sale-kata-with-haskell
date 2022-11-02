module PointOfSale where

import Control.Monad.State
import Text.Printf
import Data.Map as Map

data Product = Product { code :: String, name :: String, price :: Float } deriving (Eq, Show)

data ShoppingCart = ShoppingCart [Product] deriving (Eq, Show)

type ProductCode = String

type FindProduct = ProductCode -> Maybe Product

data Display = Display String deriving (Eq, Show)

emptyShoppingCart :: ShoppingCart
emptyShoppingCart = ShoppingCart []

type Scan = ProductCode -> State ShoppingCart Display

buildScan :: FindProduct -> Scan
buildScan findProduct = (\productCode ->
                            state $ \shoppingCart ->
                              case findProduct productCode of
                                Nothing -> (Display "invalid product", shoppingCart)
                                Just p -> (Display (((printf "%.2f") . price) p), addProduct p shoppingCart)
                        )

remove :: ProductCode -> State ShoppingCart Display
remove productCode = state $ \shoppingCart ->
                       case removeProduct productCode shoppingCart of
                         Nothing -> (Display "invalid product", shoppingCart)
                         Just (p,s) -> (Display (((printf "-%.2f") . price) p), s)

checkout :: State ShoppingCart Display
checkout = state $ \(ShoppingCart products) -> (Display ( (printf "%.2f") (sum (price <$> products))), ShoppingCart(products))

addProduct :: Product -> ShoppingCart -> ShoppingCart
addProduct p (ShoppingCart ps) = ShoppingCart (p : ps)

removeProduct :: ProductCode -> ShoppingCart -> Maybe (Product, ShoppingCart)
removeProduct pc sc = removeFirst pc [] sc
  where removeFirst c acc (ShoppingCart (x:xs)) = if ((code x) == c)
                                                  then Just (x, ShoppingCart (acc ++ xs))
                                                  else removeFirst c (x:acc) (ShoppingCart xs)
        removeFirst _ _ (ShoppingCart []) = Nothing


buildFindProductInMemory :: Map ProductCode Product -> FindProduct
buildFindProductInMemory mapOfProducts = \productCode -> Map.lookup productCode mapOfProducts
