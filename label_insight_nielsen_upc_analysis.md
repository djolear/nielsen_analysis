Label Insight UPC Analysis
================
Daniel O’Leary
10/15/2020

  - [Load packages](#load-packages)
  - [Set path](#set-path)
  - [Load functions](#load-functions)
  - [Load data](#load-data)
  - [Analysis](#analysis)
      - [Label Insight coverage of UPC’s in Nielsen dataset by Nielsen
        department
        description](#label-insight-coverage-of-upcs-in-nielsen-dataset-by-nielsen-department-description)
          - [Calories](#calories)
          - [Sugar](#sugar)
          - [Saturated Fat](#saturated-fat)
      - [Does missing data vary by household
        demographics?](#does-missing-data-vary-by-household-demographics)
          - [Calories](#calories-1)
              - [Calculate the percentage of missing calorie labels per
                household](#calculate-the-percentage-of-missing-calorie-labels-per-household)
              - [Examine relationship with
                demographics](#examine-relationship-with-demographics)
          - [Sugar](#sugar-1)
              - [Calculate the percentage of missing sugar labels per
                household](#calculate-the-percentage-of-missing-sugar-labels-per-household)
              - [Examine relationship with
                demographics](#examine-relationship-with-demographics-1)
          - [Saturated Fat](#saturated-fat-1)
              - [Calculate the percentage of missing saturated fat
                labels per
                household](#calculate-the-percentage-of-missing-saturated-fat-labels-per-household)
              - [Examine relationship with
                demographics](#examine-relationship-with-demographics-2)

# Load packages

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr
)
```

# Set path

``` r
sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )
```

# Load functions

``` r
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/bind_nielsen_to_label_insight_fn.R"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Added Sugars` = col_double(),
    ##   Calories = col_double(),
    ##   `Calories from Fat` = col_double(),
    ##   `Calories From Fat %` = col_double(),
    ##   `Calories From Saturated Fat %` = col_double(),
    ##   `Calories From Sugar %` = col_double(),
    ##   Cholesterol = col_double(),
    ##   `Dietary Fiber` = col_double(),
    ##   Protein = col_double(),
    ##   `Saturated Fat` = col_double(),
    ##   Sodium = col_double(),
    ##   Sugars = col_double(),
    ##   `Total Carbohydrate` = col_double(),
    ##   `Total Fat` = col_double(),
    ##   `Total Sugars` = col_logical(),
    ##   `Total Sugars UOM` = col_logical(),
    ##   `Trans Fat` = col_double(),
    ##   `Added Sugars Per 100` = col_double(),
    ##   `Calories From Fat % Per 100` = col_double(),
    ##   `Calories From Sugar % Per 100` = col_double()
    ##   # ... with 1 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 399308 parsing failures.
    ## row col   expected     actual                                                       file
    ##   1  -- 44 columns 51 columns 'G:/My Drive/research/projects/niel/label_insight_upc.csv'
    ##   2  -- 44 columns 51 columns 'G:/My Drive/research/projects/niel/label_insight_upc.csv'
    ##   3  -- 44 columns 51 columns 'G:/My Drive/research/projects/niel/label_insight_upc.csv'
    ##   4  -- 44 columns 51 columns 'G:/My Drive/research/projects/niel/label_insight_upc.csv'
    ##   5  -- 44 columns 51 columns 'G:/My Drive/research/projects/niel/label_insight_upc.csv'
    ## ... ... .......... .......... ..........................................................
    ## See problems(...) for more details.

# Load data

``` r
# Set uear
year = "2015"

niel_df <- bind_labels_to_purchases_fn(year)
```

    ## Parsed with column specification:
    ## cols(
    ##   trip_code_uc = col_double(),
    ##   upc = col_character(),
    ##   upc_ver_uc = col_double(),
    ##   quantity = col_double(),
    ##   total_price_paid = col_double(),
    ##   coupon_value = col_double(),
    ##   deal_flag_uc = col_double()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   trip_code_uc = col_double(),
    ##   household_code = col_double(),
    ##   purchase_date = col_date(format = ""),
    ##   retailer_code = col_double(),
    ##   store_code_uc = col_double(),
    ##   panel_year = col_double(),
    ##   store_zip3 = col_character(),
    ##   total_spent = col_double(),
    ##   method_of_payment_cd = col_character()
    ## )

    ## Joining, by = "trip_code_uc"

``` r
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/bind_purchases_to_products_master.R"))
```

    ## Parsed with column specification:
    ## cols(
    ##   upc = col_character(),
    ##   upc_ver_uc = col_double(),
    ##   upc_descr = col_character(),
    ##   product_module_code = col_double(),
    ##   product_module_descr = col_character(),
    ##   product_group_code = col_double(),
    ##   product_group_descr = col_character(),
    ##   department_code = col_double(),
    ##   department_descr = col_character(),
    ##   brand_code_uc = col_double(),
    ##   brand_descr = col_character(),
    ##   multi = col_double(),
    ##   size1_code_uc = col_double(),
    ##   size1_amount = col_double(),
    ##   size1_units = col_character(),
    ##   dataset_found_uc = col_character(),
    ##   size1_change_flag_uc = col_double()
    ## )

    ## Warning: 22 parsing failures.
    ##     row                  col           expected                        actual                                                                                                                                             file
    ## 1329497 brand_descr          delimiter or quote                               'G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv'
    ## 1329497 size1_code_uc        a double           GARDEN, LAWN & PLANT SUPPLIES 'G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv'
    ## 1329497 size1_change_flag_uc a double           GENERAL MERCHANDISE           'G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv'
    ## 1329497 NA                   17 columns         25 columns                    'G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv'
    ## 3700308 brand_descr          delimiter or quote                               'G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv'
    ## ....... .................... .................. ............................. ................................................................................................................................................
    ## See problems(...) for more details.

``` r
panelists <-
  read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Panelist_ZipCd = col_character(),
    ##   Fips_State_Desc = col_character(),
    ##   Fips_County_Desc = col_character(),
    ##   Scantrack_Market_Identifier_Desc = col_character(),
    ##   DMA_Name = col_character()
    ## )

    ## See spec(...) for full column specifications.

# Analysis

## Label Insight coverage of UPC’s in Nielsen dataset by Nielsen department description

### Calories

``` r
niel_df %>% 
  count(department_descr, !is.na(calories)) %>% 
  left_join(
    niel_df %>% 
      count(department_descr) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  filter(department_descr %in% c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT", NA)) %>% 
  mutate(per = n / total) %>% 
  filter(`!is.na(calories)` == TRUE) %>% 
  ggplot(aes(fct_reorder(department_descr, per), per, fill = department_descr)) +
  geom_col() +
  labs(
    x = "department description",
    y = "proportion coverage of calorie NI by label insight upc's"
  ) +
  coord_flip() +
  theme_bw()
```

    ## Joining, by = "department_descr"

![](label_insight_nielsen_upc_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Sugar

``` r
niel_df %>% 
  count(department_descr, !is.na(sugar)) %>% 
  left_join(
    niel_df %>% 
      count(department_descr) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  filter(department_descr %in% c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT", NA)) %>% 
  mutate(per = n / total) %>% 
  filter(`!is.na(sugar)` == TRUE) %>% 
  ggplot(aes(fct_reorder(department_descr, per), per, fill = department_descr)) +
  geom_col() +
  labs(
    x = "department description",
    y = "proportion coverage of sugar NI by label insight upc's"
  ) +
  coord_flip() +
  theme_bw()
```

    ## Joining, by = "department_descr"

![](label_insight_nielsen_upc_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Saturated Fat

``` r
niel_df %>% 
  count(department_descr, !is.na(saturated_fat)) %>% 
  left_join(
    niel_df %>% 
      count(department_descr) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  filter(department_descr %in% c("DAIRY", "DELI", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT", NA)) %>% 
  mutate(per = n / total) %>% 
  filter(`!is.na(saturated_fat)` == TRUE) %>% 
  ggplot(aes(fct_reorder(department_descr, per), per, fill = department_descr)) +
  geom_col() +
  labs(
    x = "department description",
    y = "proportion coverage of saturated fat NI by label insight upc's"
  ) +
  coord_flip() +
  theme_bw()
```

    ## Joining, by = "department_descr"

![](label_insight_nielsen_upc_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Does missing data vary by household demographics?

### Calories

#### Calculate the percentage of missing calorie labels per household

``` r
# Calculate the percentage of missing calories labels per household

household_calories_upc_na <-
  niel_df %>% 
  count(household_code, !is.na(calories)) %>% 
  left_join(
    niel_df %>% 
      count(household_code) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  mutate(per = n / total) %>% 
  filter(`!is.na(calories)` == TRUE) 
```

    ## Joining, by = "household_code"

``` r
# Join to demographics

household_calories_upc_na <-
  household_calories_upc_na %>% 
  left_join(
    panelists %>% 
      mutate(household_code = Household_Cd) %>% 
      dplyr::select(
        household_code,
        Household_Income,
        Male_Head_Education,
        Female_Head_Education
      )
  )
```

    ## Joining, by = "household_code"

#### Examine relationship with demographics

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Household_Income),
    data = household_calories_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Household_Income), data = household_calories_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7830 -0.6614  0.0167  0.6913  4.2243 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             -7.666e-16  4.034e-03    0.00        1    
    ## scale(Household_Income) -4.250e-02  4.034e-03  -10.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9991 on 61345 degrees of freedom
    ## Multiple R-squared:  0.001806,   Adjusted R-squared:  0.00179 
    ## F-statistic:   111 on 1 and 61345 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## Warning: Predicate functions must be wrapped in `where()`.
    ## 
    ##   # Bad
    ##   data %>% select(is.numeric)
    ## 
    ##   # Good
    ##   data %>% select(where(is.numeric))
    ## 
    ## i Please update your code.
    ## This message is displayed once per session.

    ## # A tibble: 2 x 5
    ##   term                    estimate std.error statistic p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                0         0.004       0         1
    ## 2 scale(Household_Income)   -0.043     0.004     -10.5       0

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Male_Head_Education),
    data = household_calories_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Male_Head_Education), data = household_calories_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8205 -0.6597  0.0167  0.6882  4.3747 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -7.961e-16  4.035e-03   0.000        1    
    ## scale(Male_Head_Education)  3.316e-02  4.035e-03   8.217   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9995 on 61345 degrees of freedom
    ## Multiple R-squared:  0.0011, Adjusted R-squared:  0.001083 
    ## F-statistic: 67.52 on 1 and 61345 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                       estimate std.error statistic p.value
    ##   <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                   0         0.004      0          1
    ## 2 scale(Male_Head_Education)    0.033     0.004      8.22       0

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Female_Head_Education),
    data = household_calories_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Female_Head_Education), data = household_calories_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8903 -0.6595  0.0182  0.6897  4.2393 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -6.683e-16  4.032e-03    0.00        1    
    ## scale(Female_Head_Education) -5.023e-02  4.032e-03  -12.46   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9987 on 61345 degrees of freedom
    ## Multiple R-squared:  0.002523,   Adjusted R-squared:  0.002507 
    ## F-statistic: 155.2 on 1 and 61345 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                         estimate std.error statistic p.value
    ##   <chr>                           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                      0        0.004       0         1
    ## 2 scale(Female_Head_Education)    -0.05     0.004     -12.5       0

### Sugar

#### Calculate the percentage of missing sugar labels per household

``` r
# Calculate the percentage of missing sugar labels per household

household_sugar_upc_na <-
  niel_df %>% 
  count(household_code, !is.na(sugar)) %>% 
  left_join(
    niel_df %>% 
      count(household_code) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  mutate(per = n / total) %>% 
  filter(`!is.na(sugar)` == TRUE) 
```

    ## Joining, by = "household_code"

``` r
# Join to demographics

household_sugar_upc_na <-
  household_sugar_upc_na %>% 
  left_join(
    panelists %>% 
      mutate(household_code = Household_Cd) %>% 
      dplyr::select(
        household_code,
        Household_Income,
        Male_Head_Education,
        Female_Head_Education
      )
  )
```

    ## Joining, by = "household_code"

#### Examine relationship with demographics

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Household_Income),
    data = household_sugar_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Household_Income), data = household_sugar_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6671 -0.6642  0.0071  0.6830  4.2687 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              3.463e-16  4.034e-03    0.00        1    
    ## scale(Household_Income) -4.319e-02  4.034e-03  -10.71   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9991 on 61343 degrees of freedom
    ## Multiple R-squared:  0.001866,   Adjusted R-squared:  0.001849 
    ## F-statistic: 114.7 on 1 and 61343 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                    estimate std.error statistic p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                0         0.004       0         1
    ## 2 scale(Household_Income)   -0.043     0.004     -10.7       0

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Male_Head_Education),
    data = household_sugar_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Male_Head_Education), data = household_sugar_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7037 -0.6641  0.0093  0.6837  4.4200 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                2.954e-16  4.035e-03   0.000        1    
    ## scale(Male_Head_Education) 3.267e-02  4.035e-03   8.097 5.75e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9995 on 61343 degrees of freedom
    ## Multiple R-squared:  0.001068,   Adjusted R-squared:  0.001051 
    ## F-statistic: 65.56 on 1 and 61343 DF,  p-value: 5.753e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                       estimate std.error statistic p.value
    ##   <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                   0         0.004      0          1
    ## 2 scale(Male_Head_Education)    0.033     0.004      8.10       0

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Female_Head_Education),
    data = household_sugar_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Female_Head_Education), data = household_sugar_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7700 -0.6645  0.0086  0.6831  4.2885 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   2.522e-16  4.033e-03       0        1    
    ## scale(Female_Head_Education) -4.841e-02  4.033e-03     -12   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9988 on 61343 degrees of freedom
    ## Multiple R-squared:  0.002344,   Adjusted R-squared:  0.002327 
    ## F-statistic: 144.1 on 1 and 61343 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                         estimate std.error statistic p.value
    ##   <chr>                           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                     0         0.004       0         1
    ## 2 scale(Female_Head_Education)   -0.048     0.004     -12.0       0

### Saturated Fat

#### Calculate the percentage of missing saturated fat labels per household

``` r
# Calculate the percentage of missing saturated fat labels per household

household_saturated_fat_upc_na <-
  niel_df %>% 
  count(household_code, !is.na(saturated_fat)) %>% 
  left_join(
    niel_df %>% 
      count(household_code) %>% 
      mutate(total = n) %>% 
      dplyr::select(-n)
  ) %>%
  mutate(per = n / total) %>% 
  filter(`!is.na(saturated_fat)` == TRUE) 
```

    ## Joining, by = "household_code"

``` r
# Join to demographics

household_saturated_fat_upc_na <-
  household_saturated_fat_upc_na %>% 
  left_join(
    panelists %>% 
      mutate(household_code = Household_Cd) %>% 
      dplyr::select(
        household_code,
        Household_Income,
        Male_Head_Education,
        Female_Head_Education
      )
  )
```

    ## Joining, by = "household_code"

#### Examine relationship with demographics

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Household_Income),
    data = household_saturated_fat_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Household_Income), data = household_saturated_fat_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6273 -0.6667  0.0052  0.6768  4.5816 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              8.104e-16  4.035e-03   0.000        1    
    ## scale(Household_Income) -3.543e-02  4.035e-03  -8.781   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9994 on 61341 degrees of freedom
    ## Multiple R-squared:  0.001255,   Adjusted R-squared:  0.001239 
    ## F-statistic: 77.11 on 1 and 61341 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                    estimate std.error statistic p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                0         0.004      0          1
    ## 2 scale(Household_Income)   -0.035     0.004     -8.78       0

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Male_Head_Education),
    data = household_saturated_fat_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Male_Head_Education), data = household_saturated_fat_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6139 -0.6675  0.0057  0.6782  4.6082 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.056e-15  4.036e-03   0.000        1    
    ## scale(Male_Head_Education) 3.120e-02  4.036e-03   7.732 1.08e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9995 on 61341 degrees of freedom
    ## Multiple R-squared:  0.0009735,  Adjusted R-squared:  0.0009573 
    ## F-statistic: 59.78 on 1 and 61341 DF,  p-value: 1.079e-14

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                       estimate std.error statistic p.value
    ##   <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                   0         0.004      0          1
    ## 2 scale(Male_Head_Education)    0.031     0.004      7.73       0

``` r
lm1 <-
  lm(
    scale(per) ~
      scale(Female_Head_Education),
    data = household_saturated_fat_upc_na
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = scale(per) ~ scale(Female_Head_Education), data = household_saturated_fat_upc_na)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6565 -0.6650  0.0056  0.6765  4.5204 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   1.064e-15  4.035e-03   0.000        1    
    ## scale(Female_Head_Education) -3.730e-02  4.035e-03  -9.245   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9993 on 61341 degrees of freedom
    ## Multiple R-squared:  0.001391,   Adjusted R-squared:  0.001375 
    ## F-statistic: 85.47 on 1 and 61341 DF,  p-value: < 2.2e-16

``` r
broom::tidy(lm1) %>% 
  mutate(across(is.numeric, round, 3))
```

    ## # A tibble: 2 x 5
    ##   term                         estimate std.error statistic p.value
    ##   <chr>                           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                     0         0.004      0          1
    ## 2 scale(Female_Head_Education)   -0.037     0.004     -9.24       0
