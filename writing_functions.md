writing_functions
================
Hanrui Li
2024-10-24

``` r
library(tidyverse)
library(rvest)

set.seed(1)
```

## Do something simple

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

Want a function to compute z-zcores

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

Try my functions on some other things. These should give errors.

``` r
z_scores(3)
```

    ## [1] NA

``` r
# z_scores("my name is jeff") # error: non-numeric

# z_scores(iris) # error: cannot be coerced to type 'double'

z_scores(c(TRUE,TRUE,FALSE,TRUE))
```

    ## [1]  0.5  0.5 -1.5  0.5

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ##  [1] -0.7348469  1.3063945 -0.7348469 -0.7348469  1.3063945  1.3063945
    ##  [7] -0.7348469 -0.7348469 -0.7348469  1.3063945  1.3063945 -0.7348469
    ## [13] -0.7348469 -0.7348469 -0.7348469 -0.7348469 -0.7348469  1.3063945
    ## [19] -0.7348469 -0.7348469 -0.7348469 -0.7348469  1.3063945  1.3063945
    ## [25]  1.3063945

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
```

Alternatively, we might store values in a data frame.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

Check that the function works.

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.88  3.67

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = tibble(
  x = rnorm(n = 100, mean = 4, sd = 3))

sim_data |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.16  3.00

``` r
sim_mean_sd = function(sample_size, mu, sigma) {
  
  sim_data = tibble(
    x = rnorm(n = sample_size, mean = mu, sd = sigma),
  )
  
  sim_data |> 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.09  3.00

``` r
sim_mean_sd = function(sample_size, mu = 3, sigma = 3) {
  
  sim_data = tibble(
    x = rnorm(n = sample_size, mean = mu, sd = sigma),
  )
  
  sim_data |> 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.02  3.06

## Learning Assignment: Loading LoTR data

``` r
fellowship_ring = readxl::read_excel("LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything())
```

Write a function that can be used to abstract the data loading and
cleaning process.

Use this function to recreate the tidied LoTR dataset.

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = 
    readxl::read_excel(path, range = range) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_name) |> 
    select(movie, everything())
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("LotR_Words.xlsx", "J3:L6", "return_king"))

lotr_tidy
```

    ## # A tibble: 18 × 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Write a function to scrape review information for other tables on this
page.

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}
```

Use this to get a few different tables and combine the results.

``` r
nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )

nsduh_results
```

    ## # A tibble: 1,530 × 5
    ##    State   age   year      percent name         
    ##    <chr>   <chr> <chr>       <dbl> <chr>        
    ##  1 Alabama 12+   2013-2014    9.98 marj_one_year
    ##  2 Alabama 12+   2014-2015    9.6  marj_one_year
    ##  3 Alabama 12-17 2013-2014    9.9  marj_one_year
    ##  4 Alabama 12-17 2014-2015    9.71 marj_one_year
    ##  5 Alabama 18-25 2013-2014   27.0  marj_one_year
    ##  6 Alabama 18-25 2014-2015   26.1  marj_one_year
    ##  7 Alabama 26+   2013-2014    7.1  marj_one_year
    ##  8 Alabama 26+   2014-2015    6.81 marj_one_year
    ##  9 Alabama 18+   2013-2014    9.99 marj_one_year
    ## 10 Alabama 18+   2014-2015    9.59 marj_one_year
    ## # ℹ 1,520 more rows

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  summ_func(x)
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 2.646269

``` r
median(x_vec)
```

    ## [1] 2.091605

``` r
my_summary(x_vec, mean)
```

    ## [1] 2.646269

``` r
my_summary(x_vec, sd)
```

    ## [1] 7.792839

``` r
my_summary(x_vec, IQR)
```

    ## [1] 11.41527

``` r
my_summary(x_vec, var)
```

    ## [1] 60.72834
