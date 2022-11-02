module PointOfSaleSpec where

import Test.Hspec
import PointOfSale
import Control.Monad.State as StateMonad
import Data.Map as Map

spec :: Spec
spec = do

  let chips = Product { code = "901234", name = "Chips", price = 1.50 }
  let redBull = Product { code = "507780", name = "Red Bull", price = 2.35 }
  let findProduct = buildFindProductInMemory (Map.fromList [("901234", chips), ("507780", redBull)])

  describe "Acceptance test" $ do

    it "Point of sale should perform a sale with multiple products" $ do
      let scan =  buildScan findProduct
      let result = do
                   scan "901234"
                   scan "901234"
                   scan "unknown"
                   scan "507780"
                   scan "507780"
                   remove "507780"
                   checkout
      StateMonad.runState result emptyShoppingCart `shouldBe`  (Display "5.35", ShoppingCart [redBull, chips, chips])

  describe "Point of sale" $ do

    it "When a product is scanned, the price is displayed" $ do
      let scan :: Scan = buildScan (\_ -> Just chips)
      let result = scan "901234"
      fst (StateMonad.runState result emptyShoppingCart) `shouldBe` (Display "1.50")

    it "When a scanned product is not present, a message 'invalid product' is displayed" $ do
      let scan :: Scan = buildScan $ \_ -> Nothing
      let result = scan "901234"
      StateMonad.runState result emptyShoppingCart `shouldBe` (Display "invalid product", emptyShoppingCart)

    it "When a scan succeed, product is added to the current digital shopping cart." $ do
      let scan :: Scan = buildScan (\_ -> Just chips)
      let result = scan "901234"
      StateMonad.runState result emptyShoppingCart `shouldBe` (Display "1.50", ShoppingCart([chips]))

    it "We can remove a product from the current cart by product-id, the price will be displayed in negative" $ do
      let shoppingCart = ShoppingCart [chips, chips]
      let removeState = remove "901234"
      StateMonad.runState removeState shoppingCart `shouldBe` (Display "-1.50", ShoppingCart([chips]))

    it "If we can not remove a product from the current cart, a message 'invalid product' is displayed" $ do
      let shoppingCart = ShoppingCart [chips, chips]
      let removeState = remove "invalid-code"
      StateMonad.runState removeState shoppingCart `shouldBe` (Display "invalid product", ShoppingCart([chips, chips]))

    it "When the sale is finished, the point of sale will check out, calculating the total price and displaying it" $ do
      let shoppingCart = ShoppingCart [chips, chips, redBull]
      StateMonad.runState checkout shoppingCart `shouldBe` (Display "5.35", ShoppingCart([chips, chips, redBull]))

  describe "Product finder in memory implementation" $ do

    it "Should find an existent product" $ do
      findProduct "507780" `shouldBe` Just redBull

    it "Should not find a non existent product" $ do
      findProduct "unknown" `shouldBe` Nothing

