# point-of-sale-kata-with-haskell

Point of sale kata implementation, [here](https://github.com/albertllousas/point-of-sale-kata-definition) the problem statement.


## Tests

```shell
> stack test


PointOfSale
  Acceptance test
    Point of sale should perform a sale with multiple products
  Point of sale
    When a product is scanned, the price is displayed
    When a scanned product is not present, a message 'invalid product' is displayed
    When a scan succeed, product is added to the current digital shopping cart.
    We can remove a product from the current cart by product-id, the price will be displayed in negative
    If we can not remove a product from the current cart, a message 'invalid product' is displayed
    When the sale is finished, the point of sale will check out, calculating the total price and displaying it
  Product finder in memory implementation
    Should find an existent product
    Should not find a non existent product

Finished in 0.0027 seconds
9 examples, 0 failures
```
