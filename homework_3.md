Homework 3
================
Sarah McLarnan
2019-10-07

``` r
library(p8105.datasets)
data("instacart")
```

The `instacart` data set has 1384617 observations of 15 metrics. Each row corresponds to a single item in an order. The columns contain information including the item ordered (`product_id`, `product_name`), the department (`department_id`, `department`) and aisle (`aisle_id`, `aisle`) it is from, the day of the week (`order_dow`) and time (`order_hour_of_day`) it was ordered, the order in which the item was added to the order (`add_to_cart_order`) in addition to other metrics. For example the first item in the table is Bulgarian Yogurt from the yogurt aisle in the dairy and eggs department. The item was ordered at 10 a.m. on a Wednesday, it was the first item the shopper added to their order and it had been 9 days since that shopper's previous order.
