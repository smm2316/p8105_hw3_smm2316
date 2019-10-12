Homework 3
================
Sarah McLarnan
2019-10-07

``` r
library(p8105.datasets)
data("instacart")
```

The `instacart` data set has 1384617 observations of 15 metrics. Each row corresponds to a single item in an order. The columns contain information including the item ordered (`product_id`, `product_name`), the department (`department_id`, `department`) and aisle (`aisle_id`, `aisle`) it is from, the day of the week (`order_dow`) and time (`order_hour_of_day`) it was ordered, the order in which the item was added to the order (`add_to_cart_order`) in addition to other metrics. For example the first item in the table is Bulgarian Yogurt from the yogurt aisle in the dairy and eggs department. The item was ordered at 10 a.m. on a Wednesday, it was the first item the shopper added to their order and it had been 9 days since that shopper's previous order.

``` r
n_distinct(pull(instacart, aisle_id))
```

    ## [1] 134

``` r
instacart %>%
  group_by(aisle) %>%
  summarize(n = n()) %>%
  mutate(aisle_rank = rank(desc(n))) %>%
  filter(aisle_rank <= 3)
```

    ## # A tibble: 3 x 3
    ##   aisle                           n aisle_rank
    ##   <chr>                       <int>      <dbl>
    ## 1 fresh fruits               150473          2
    ## 2 fresh vegetables           150609          1
    ## 3 packaged vegetables fruits  78493          3

There are 134 aisles. The top three aisles with the most products ordered from are fresh vegetables, fresh fruits, and packaged vegetables and fruits.

``` r
instacart %>%
  group_by(aisle) %>%
  summarize(n = n()) %>%
  filter(n > 10000) %>%
  ggplot(aes(x = aisle, y = n)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Aisle", y = "Number of Products Ordered", title = "Number of Products Ordered from Aisle")
```

<img src="homework_3_files/figure-markdown_github/problem_1_aisle_graph-1.png" width="90%" />
