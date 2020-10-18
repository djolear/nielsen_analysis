Nielsen Secondary Data Analysis - Nutrition
================
Daniel O’Leary
10/15/2020

  - [Setup](#setup)
      - [Load packages](#load-packages)
      - [Set path](#set-path)
      - [Load functions](#load-functions)
      - [Load data](#load-data)
  - [Analysis](#analysis)
      - [Calories](#calories)
          - [Income](#income)
          - [Income and median income](#income-and-median-income)
          - [Income difference score](#income-difference-score)
          - [Income difference score x
            gini](#income-difference-score-x-gini)
          - [Regression table](#regression-table)
          - [Own income](#own-income)
          - [Own income minus median income in your zip
            code](#own-income-minus-median-income-in-your-zip-code)
          - [Own income minus median income x gini in your zip
            code](#own-income-minus-median-income-x-gini-in-your-zip-code)
      - [Sugar content](#sugar-content)
          - [Income](#income-1)
          - [Income and median income](#income-and-median-income-1)
          - [Income difference score](#income-difference-score-1)
          - [Income difference score x
            gini](#income-difference-score-x-gini-1)
          - [Regression table](#regression-table-1)
          - [Own income](#own-income-1)
          - [Own income minus median income in your zip
            code](#own-income-minus-median-income-in-your-zip-code-1)
          - [Own income minus median income x gini in your zip
            code](#own-income-minus-median-income-x-gini-in-your-zip-code-1)
      - [Saturated fat content](#saturated-fat-content)
          - [Income](#income-2)
          - [Income and median income](#income-and-median-income-2)
          - [Income difference score](#income-difference-score-2)
          - [Income difference score x
            gini](#income-difference-score-x-gini-2)
          - [Regression table](#regression-table-2)
          - [Own income](#own-income-2)
          - [Own income minus median income in your zip
            code](#own-income-minus-median-income-in-your-zip-code-2)
          - [Own income minus median income x gini in your zip
            code](#own-income-minus-median-income-x-gini-in-your-zip-code-2)

# Setup

## Load packages

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr,
  lme4,
  sjPlot,
  effects,
  stargazer
)
```

## Set path

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

## Load functions

``` r
lm.beta.lmer <- function(mod) {
   b <- fixef(mod)[-1]
   sd.x <- apply(getME(mod,"X")[,-1],2,sd)
   sd.y <- sd(getME(mod,"y"))
   b*sd.x/sd.y
}
```

## Load data

``` r
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/calculate_nutrition_per_spend_fn.R"))
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

``` r
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/bind_nps_to_zip_code_census_fn.R"))

year = "2015"

nps_2015 <- 
  nutrition_per_spend_fn(year)
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

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Joining, by = "household_code"

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Joining, by = "household_code"

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Joining, by = "household_code"
    ## Joining, by = "household_code"
    ## Joining, by = "household_code"
    ## Joining, by = "household_code"

``` r
nps_2015 <-
  bind_zip_code_census_data_function(nps_2015, year)  
```

    ## Joining, by = "zip"

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   zip = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   ZIP = col_character(),
    ##   county1 = col_double(),
    ##   county2 = col_double(),
    ##   county3 = col_double(),
    ##   county4 = col_double(),
    ##   county5 = col_double(),
    ##   county6 = col_double(),
    ##   county7 = col_double(),
    ##   county1_ratio = col_double(),
    ##   county2_ratio = col_double(),
    ##   county3_ratio = col_double(),
    ##   county4_ratio = col_double(),
    ##   county5_ratio = col_double(),
    ##   county6_ratio = col_double(),
    ##   county7_ratio = col_double(),
    ##   n = col_double()
    ## )

    ## Warning: Missing column names filled in: 'X24' [24]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `County Name` = col_character(),
    ##   `Commuting Zone Name` = col_character(),
    ##   State = col_character(),
    ##   X24 = col_logical()
    ## )

    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   cty = col_character(),
    ##   Name = col_character(),
    ##   Household_Income_rP_gP_p50 = col_double()
    ## )

    ## Joining, by = "zip"
    ## Joining, by = "zip"
    ## Joining, by = "zip"

# Analysis

## Calories

### Income

``` r
lm_inc <-
  lmer(
    cal_per ~
      inc_mid +
      Household_Size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## cal_per ~ inc_mid + Household_Size + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 477529.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5447 -0.6525 -0.0963  0.5345 18.7182 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept)   5.641   2.375  
    ##  Residual             135.465  11.639  
    ## Number of obs: 61347, groups:  zip, 15891
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error t value
    ## (Intercept)              4.225e+01  4.390e-01  96.244
    ## inc_mid                 -9.110e-05  2.096e-06 -43.469
    ## Household_Size          -1.075e-02  4.762e-02  -0.226
    ## Male_Head_Education     -5.892e-01  5.499e-02 -10.715
    ## Female_Head_Education   -7.612e-01  5.454e-02 -13.958
    ## Race2                    1.341e+00  1.623e-01   8.262
    ## Race3                   -3.024e+00  2.749e-01 -10.998
    ## Race4                   -1.339e+00  2.314e-01  -5.787
    ## Marital_Status2          1.857e+00  2.778e-01   6.686
    ## Marital_Status3          1.506e+00  2.420e-01   6.224
    ## Marital_Status4          2.036e+00  2.423e-01   8.402
    ## Male_Head_Employment1    4.262e+00  5.073e-01   8.401
    ## Male_Head_Employment2    4.382e+00  5.406e-01   8.105
    ## Male_Head_Employment3    4.985e+00  4.462e-01  11.173
    ## Male_Head_Employment9    3.773e+00  4.781e-01   7.893
    ## Female_Head_Employment1  2.919e+00  4.734e-01   6.166
    ## Female_Head_Employment2  3.136e+00  4.996e-01   6.277
    ## Female_Head_Employment3  3.807e+00  4.536e-01   8.393
    ## Female_Head_Employment9  2.501e+00  4.616e-01   5.418
    ## Male_Head_Age           -2.739e-01  4.485e-02  -6.106
    ## Female_Head_Age         -1.602e-01  4.036e-02  -3.970

    ## 
    ## Correlation matrix not shown by default, as p = 21 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc)
```

    ##                 inc_mid          Household_Size     Male_Head_Education 
    ##            -0.215382724            -0.001123928            -0.097935508 
    ##   Female_Head_Education                   Race2                   Race3 
    ##            -0.100120039             0.033245240            -0.043228143 
    ##                   Race4         Marital_Status2         Marital_Status3 
    ##            -0.022494327             0.039307202             0.042987700 
    ##         Marital_Status4   Male_Head_Employment1   Male_Head_Employment2 
    ##             0.055969423             0.072816662             0.054293811 
    ##   Male_Head_Employment3   Male_Head_Employment9 Female_Head_Employment1 
    ##             0.197962420             0.130459373             0.076471577 
    ## Female_Head_Employment2 Female_Head_Employment3 Female_Head_Employment9 
    ##             0.052497701             0.144101649             0.098374590 
    ##           Male_Head_Age         Female_Head_Age 
    ##            -0.077573696            -0.036493823

### Income and median income

``` r
lm_med_inc <-
  lmer(
    cal_per ~
      median_income + 
      inc_mid + 
      median_monthly_housing_cost + 
      pov_status_below_per +
      Household_Size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_med_inc)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: cal_per ~ median_income + inc_mid + median_monthly_housing_cost +  
    ##     pov_status_below_per + Household_Size + Male_Head_Education +  
    ##     Female_Head_Education + Race + Marital_Status + Male_Head_Employment +  
    ##     Female_Head_Employment + Male_Head_Age + Female_Head_Age +      (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 473697.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6589 -0.6524 -0.0970  0.5348 18.8854 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept)   3.885   1.971  
    ##  Residual             135.069  11.622  
    ## Number of obs: 60955, groups:  zip, 15533
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  4.265e+01  5.506e-01  77.463
    ## median_income                7.345e-05  6.731e-06  10.912
    ## inc_mid                     -8.006e-05  2.147e-06 -37.290
    ## median_monthly_housing_cost -5.811e-03  2.827e-04 -20.550
    ## pov_status_below_per         6.582e+00  1.012e+00   6.505
    ## Household_Size               8.431e-03  4.749e-02   0.178
    ## Male_Head_Education         -5.280e-01  5.502e-02  -9.597
    ## Female_Head_Education       -7.175e-01  5.443e-02 -13.183
    ## Race2                        1.433e+00  1.659e-01   8.636
    ## Race3                       -2.226e+00  2.762e-01  -8.061
    ## Race4                       -9.865e-01  2.325e-01  -4.243
    ## Marital_Status2              1.932e+00  2.770e-01   6.975
    ## Marital_Status3              1.685e+00  2.415e-01   6.978
    ## Marital_Status4              2.288e+00  2.419e-01   9.457
    ## Male_Head_Employment1        4.035e+00  5.062e-01   7.972
    ## Male_Head_Employment2        4.156e+00  5.397e-01   7.700
    ## Male_Head_Employment3        4.617e+00  4.455e-01  10.365
    ## Male_Head_Employment9        3.510e+00  4.772e-01   7.355
    ## Female_Head_Employment1      2.627e+00  4.722e-01   5.563
    ## Female_Head_Employment2      2.855e+00  4.984e-01   5.729
    ## Female_Head_Employment3      3.397e+00  4.527e-01   7.504
    ## Female_Head_Employment9      2.185e+00  4.604e-01   4.745
    ## Male_Head_Age               -2.868e-01  4.471e-02  -6.415
    ## Female_Head_Age             -1.505e-01  4.024e-02  -3.739

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_med_inc)
```

    ##               median_income                     inc_mid 
    ##                0.1266754547               -0.1894730394 
    ## median_monthly_housing_cost        pov_status_below_per 
    ##               -0.1889698282                0.0440195409 
    ##              Household_Size         Male_Head_Education 
    ##                0.0008825192               -0.0878616579 
    ##       Female_Head_Education                       Race2 
    ##               -0.0944158944                0.0355348897 
    ##                       Race3                       Race4 
    ##               -0.0319064526               -0.0165568351 
    ##             Marital_Status2             Marital_Status3 
    ##                0.0409604306                0.0481157664 
    ##             Marital_Status4       Male_Head_Employment1 
    ##                0.0629212934                0.0690352513 
    ##       Male_Head_Employment2       Male_Head_Employment3 
    ##                0.0514817768                0.1836004841 
    ##       Male_Head_Employment9     Female_Head_Employment1 
    ##                0.1214794386                0.0689537336 
    ##     Female_Head_Employment2     Female_Head_Employment3 
    ##                0.0478321495                0.1287314450 
    ##     Female_Head_Employment9               Male_Head_Age 
    ##                0.0860416030               -0.0813424543 
    ##             Female_Head_Age 
    ##               -0.0343001731

### Income difference score

``` r
lm_inc_diff <-
  lmer(
    cal_per ~
      inc_mid +     
      inc_diff + 
      Household_Size +
      pov_status_below_per +
      median_monthly_housing_cost + 
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc_diff)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## cal_per ~ inc_mid + inc_diff + Household_Size + pov_status_below_per +  
    ##     median_monthly_housing_cost + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 473697.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6589 -0.6524 -0.0970  0.5348 18.8854 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept)   3.885   1.971  
    ##  Residual             135.069  11.622  
    ## Number of obs: 60955, groups:  zip, 15533
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  4.265e+01  5.506e-01  77.463
    ## inc_mid                     -6.611e-06  7.054e-06  -0.937
    ## inc_diff                    -7.345e-05  6.731e-06 -10.912
    ## Household_Size               8.431e-03  4.749e-02   0.178
    ## pov_status_below_per         6.582e+00  1.012e+00   6.505
    ## median_monthly_housing_cost -5.811e-03  2.827e-04 -20.550
    ## Male_Head_Education         -5.280e-01  5.502e-02  -9.597
    ## Female_Head_Education       -7.175e-01  5.443e-02 -13.183
    ## Race2                        1.433e+00  1.659e-01   8.636
    ## Race3                       -2.226e+00  2.762e-01  -8.061
    ## Race4                       -9.865e-01  2.325e-01  -4.243
    ## Marital_Status2              1.932e+00  2.770e-01   6.975
    ## Marital_Status3              1.685e+00  2.415e-01   6.978
    ## Marital_Status4              2.288e+00  2.419e-01   9.457
    ## Male_Head_Employment1        4.035e+00  5.062e-01   7.972
    ## Male_Head_Employment2        4.156e+00  5.397e-01   7.700
    ## Male_Head_Employment3        4.617e+00  4.455e-01  10.365
    ## Male_Head_Employment9        3.510e+00  4.772e-01   7.355
    ## Female_Head_Employment1      2.627e+00  4.722e-01   5.563
    ## Female_Head_Employment2      2.855e+00  4.984e-01   5.729
    ## Female_Head_Employment3      3.397e+00  4.527e-01   7.504
    ## Female_Head_Employment9      2.185e+00  4.604e-01   4.745
    ## Male_Head_Age               -2.868e-01  4.471e-02  -6.415
    ## Female_Head_Age             -1.505e-01  4.024e-02  -3.739

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc_diff)
```

    ##                     inc_mid                    inc_diff 
    ##               -0.0156463210               -0.1813161674 
    ##              Household_Size        pov_status_below_per 
    ##                0.0008825192                0.0440195409 
    ## median_monthly_housing_cost         Male_Head_Education 
    ##               -0.1889698282               -0.0878616579 
    ##       Female_Head_Education                       Race2 
    ##               -0.0944158944                0.0355348897 
    ##                       Race3                       Race4 
    ##               -0.0319064526               -0.0165568351 
    ##             Marital_Status2             Marital_Status3 
    ##                0.0409604306                0.0481157665 
    ##             Marital_Status4       Male_Head_Employment1 
    ##                0.0629212933                0.0690352513 
    ##       Male_Head_Employment2       Male_Head_Employment3 
    ##                0.0514817768                0.1836004841 
    ##       Male_Head_Employment9     Female_Head_Employment1 
    ##                0.1214794386                0.0689537336 
    ##     Female_Head_Employment2     Female_Head_Employment3 
    ##                0.0478321496                0.1287314450 
    ##     Female_Head_Employment9               Male_Head_Age 
    ##                0.0860416030               -0.0813424543 
    ##             Female_Head_Age 
    ##               -0.0343001731

### Income difference score x gini

``` r
lm_inc_diff_g <-
  lmer(
    cal_per ~
      inc_mid +     
      inc_diff * gini +
      Household_Size +
      pov_status_below_per +
      median_monthly_housing_cost + 
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc_diff_g)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## cal_per ~ inc_mid + inc_diff * gini + Household_Size + pov_status_below_per +  
    ##     median_monthly_housing_cost + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 473652.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6987 -0.6532 -0.0982  0.5357 18.8986 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept)   3.718   1.928  
    ##  Residual             135.077  11.622  
    ## Number of obs: 60955, groups:  zip, 15533
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  4.587e+01  7.188e-01  63.805
    ## inc_mid                     -3.834e-06  7.045e-06  -0.544
    ## inc_diff                    -1.977e-05  1.479e-05  -1.336
    ## gini                        -8.813e+00  1.246e+00  -7.075
    ## Household_Size              -6.494e-03  4.752e-02  -0.137
    ## pov_status_below_per         9.786e+00  1.091e+00   8.970
    ## median_monthly_housing_cost -5.769e-03  2.821e-04 -20.448
    ## Male_Head_Education         -5.034e-01  5.509e-02  -9.138
    ## Female_Head_Education       -6.947e-01  5.449e-02 -12.750
    ## Race2                        1.418e+00  1.657e-01   8.558
    ## Race3                       -2.193e+00  2.761e-01  -7.943
    ## Race4                       -1.006e+00  2.324e-01  -4.328
    ## Marital_Status2              1.925e+00  2.769e-01   6.951
    ## Marital_Status3              1.674e+00  2.414e-01   6.932
    ## Marital_Status4              2.295e+00  2.420e-01   9.486
    ## Male_Head_Employment1        3.917e+00  5.062e-01   7.739
    ## Male_Head_Employment2        4.043e+00  5.397e-01   7.492
    ## Male_Head_Employment3        4.474e+00  4.457e-01  10.038
    ## Male_Head_Employment9        3.381e+00  4.773e-01   7.084
    ## Female_Head_Employment1      2.469e+00  4.724e-01   5.227
    ## Female_Head_Employment2      2.698e+00  4.986e-01   5.410
    ## Female_Head_Employment3      3.229e+00  4.530e-01   7.129
    ## Female_Head_Employment9      2.018e+00  4.608e-01   4.380
    ## Male_Head_Age               -2.854e-01  4.470e-02  -6.386
    ## Female_Head_Age             -1.453e-01  4.023e-02  -3.613
    ## inc_diff:gini               -1.311e-04  3.145e-05  -4.168

    ## 
    ## Correlation matrix not shown by default, as p = 26 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc_diff_g)
```

    ##                     inc_mid                    inc_diff 
    ##                -0.009073196                -0.048794037 
    ##                        gini              Household_Size 
    ##                -0.033385775                -0.000679805 
    ##        pov_status_below_per median_monthly_housing_cost 
    ##                 0.065453228                -0.187633886 
    ##         Male_Head_Education       Female_Head_Education 
    ##                -0.083761771                -0.091410606 
    ##                       Race2                       Race3 
    ##                 0.035178909                -0.031426469 
    ##                       Race4             Marital_Status2 
    ##                -0.016883322                 0.040802318 
    ##             Marital_Status3             Marital_Status4 
    ##                 0.047780294                 0.063129161 
    ##       Male_Head_Employment1       Male_Head_Employment2 
    ##                 0.067015643                 0.050088224 
    ##       Male_Head_Employment3       Male_Head_Employment9 
    ##                 0.177886891                 0.117025575 
    ##     Female_Head_Employment1     Female_Head_Employment2 
    ##                 0.064816081                 0.045187437 
    ##     Female_Head_Employment3     Female_Head_Employment9 
    ##                 0.122380108                 0.079468949 
    ##               Male_Head_Age             Female_Head_Age 
    ##                -0.080936598                -0.033134014 
    ##               inc_diff:gini 
    ##                -0.140073471

### Regression table

``` r
stargazer(
  lm_inc, lm_med_inc, lm_inc_diff, lm_inc_diff_g,
  coef = 
    list(
      lm.beta.lmer(lm_inc), 
      lm.beta.lmer(lm_med_inc), 
      lm.beta.lmer(lm_inc_diff),
      lm.beta.lmer(lm_inc_diff_g)
    ),
  type = "html"
)
```

<table style="text-align:center">

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

cal\_per

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

median\_income

</td>

<td>

</td>

<td>

0.127<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.00001)

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_mid

</td>

<td>

\-0.215

</td>

<td>

\-0.189<sup>\*\*\*</sup>

</td>

<td>

\-0.016<sup>\*\*\*</sup>

</td>

<td>

\-0.009<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00001)

</td>

<td>

(0.00001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_diff

</td>

<td>

</td>

<td>

</td>

<td>

\-0.181<sup>\*\*\*</sup>

</td>

<td>

\-0.049<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(0.00001)

</td>

<td>

(0.00001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

gini

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

\-0.033

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(1.246)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

median\_monthly\_housing\_cost

</td>

<td>

</td>

<td>

\-0.189<sup>\*\*\*</sup>

</td>

<td>

\-0.189<sup>\*\*\*</sup>

</td>

<td>

\-0.188<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.0003)

</td>

<td>

(0.0003)

</td>

<td>

(0.0003)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

pov\_status\_below\_per

</td>

<td>

</td>

<td>

0.044

</td>

<td>

0.044

</td>

<td>

0.065

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(1.012)

</td>

<td>

(1.012)

</td>

<td>

(1.091)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Household\_Size

</td>

<td>

\-0.001<sup>\*\*\*</sup>

</td>

<td>

0.001

</td>

<td>

0.001

</td>

<td>

\-0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.048)

</td>

<td>

(0.047)

</td>

<td>

(0.047)

</td>

<td>

(0.048)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Education

</td>

<td>

\-0.098<sup>\*\*</sup>

</td>

<td>

\-0.088

</td>

<td>

\-0.088

</td>

<td>

\-0.084

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.055)

</td>

<td>

(0.055)

</td>

<td>

(0.055)

</td>

<td>

(0.055)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Education

</td>

<td>

\-0.100<sup>\*</sup>

</td>

<td>

\-0.094<sup>\*</sup>

</td>

<td>

\-0.094<sup>\*</sup>

</td>

<td>

\-0.091<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.055)

</td>

<td>

(0.054)

</td>

<td>

(0.054)

</td>

<td>

(0.054)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race2

</td>

<td>

0.033

</td>

<td>

0.036

</td>

<td>

0.036

</td>

<td>

0.035

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.162)

</td>

<td>

(0.166)

</td>

<td>

(0.166)

</td>

<td>

(0.166)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race3

</td>

<td>

\-0.043

</td>

<td>

\-0.032

</td>

<td>

\-0.032

</td>

<td>

\-0.031

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.275)

</td>

<td>

(0.276)

</td>

<td>

(0.276)

</td>

<td>

(0.276)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race4

</td>

<td>

\-0.022

</td>

<td>

\-0.017

</td>

<td>

\-0.017

</td>

<td>

\-0.017

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.231)

</td>

<td>

(0.233)

</td>

<td>

(0.233)

</td>

<td>

(0.232)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status2

</td>

<td>

0.039

</td>

<td>

0.041

</td>

<td>

0.041

</td>

<td>

0.041

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.278)

</td>

<td>

(0.277)

</td>

<td>

(0.277)

</td>

<td>

(0.277)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status3

</td>

<td>

0.043

</td>

<td>

0.048

</td>

<td>

0.048

</td>

<td>

0.048

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.242)

</td>

<td>

(0.242)

</td>

<td>

(0.242)

</td>

<td>

(0.241)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status4

</td>

<td>

0.056

</td>

<td>

0.063

</td>

<td>

0.063

</td>

<td>

0.063

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.242)

</td>

<td>

(0.242)

</td>

<td>

(0.242)

</td>

<td>

(0.242)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment1

</td>

<td>

0.073

</td>

<td>

0.069

</td>

<td>

0.069

</td>

<td>

0.067

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.507)

</td>

<td>

(0.506)

</td>

<td>

(0.506)

</td>

<td>

(0.506)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment2

</td>

<td>

0.054

</td>

<td>

0.051

</td>

<td>

0.051

</td>

<td>

0.050

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.541)

</td>

<td>

(0.540)

</td>

<td>

(0.540)

</td>

<td>

(0.540)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment3

</td>

<td>

0.198

</td>

<td>

0.184

</td>

<td>

0.184

</td>

<td>

0.178

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.446)

</td>

<td>

(0.445)

</td>

<td>

(0.445)

</td>

<td>

(0.446)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment9

</td>

<td>

0.130

</td>

<td>

0.121

</td>

<td>

0.121

</td>

<td>

0.117

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.478)

</td>

<td>

(0.477)

</td>

<td>

(0.477)

</td>

<td>

(0.477)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment1

</td>

<td>

0.076

</td>

<td>

0.069

</td>

<td>

0.069

</td>

<td>

0.065

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.473)

</td>

<td>

(0.472)

</td>

<td>

(0.472)

</td>

<td>

(0.472)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment2

</td>

<td>

0.052

</td>

<td>

0.048

</td>

<td>

0.048

</td>

<td>

0.045

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.500)

</td>

<td>

(0.498)

</td>

<td>

(0.498)

</td>

<td>

(0.499)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment3

</td>

<td>

0.144

</td>

<td>

0.129

</td>

<td>

0.129

</td>

<td>

0.122

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.454)

</td>

<td>

(0.453)

</td>

<td>

(0.453)

</td>

<td>

(0.453)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment9

</td>

<td>

0.098

</td>

<td>

0.086

</td>

<td>

0.086

</td>

<td>

0.079

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.462)

</td>

<td>

(0.460)

</td>

<td>

(0.460)

</td>

<td>

(0.461)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Age

</td>

<td>

\-0.078

</td>

<td>

\-0.081<sup>\*</sup>

</td>

<td>

\-0.081<sup>\*</sup>

</td>

<td>

\-0.081<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.045)

</td>

<td>

(0.045)

</td>

<td>

(0.045)

</td>

<td>

(0.045)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Age

</td>

<td>

\-0.036

</td>

<td>

\-0.034

</td>

<td>

\-0.034

</td>

<td>

\-0.033

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_diff:gini

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

\-0.140<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.00003)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.439)

</td>

<td>

(0.551)

</td>

<td>

(0.551)

</td>

<td>

(0.719)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

61,347

</td>

<td>

60,955

</td>

<td>

60,955

</td>

<td>

60,955

</td>

</tr>

<tr>

<td style="text-align:left">

Log Likelihood

</td>

<td>

\-238,764.600

</td>

<td>

\-236,848.800

</td>

<td>

\-236,848.800

</td>

<td>

\-236,826.100

</td>

</tr>

<tr>

<td style="text-align:left">

Akaike Inf. Crit.

</td>

<td>

477,575.300

</td>

<td>

473,749.500

</td>

<td>

473,749.500

</td>

<td>

473,708.300

</td>

</tr>

<tr>

<td style="text-align:left">

Bayesian Inf. Crit.

</td>

<td>

477,782.800

</td>

<td>

473,984.000

</td>

<td>

473,984.000

</td>

<td>

473,960.800

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="4" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

### Own income

``` r
effect("inc_mid", lm_inc) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_mid, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE) + sd(nps_2015$inc_mid, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE) - sd(nps_2015$inc_mid, na.rm = TRUE)) +
  labs(
    x = "own income",
    y = "calories per $1 spent"
  ) +
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Own income minus median income in your zip code

``` r
effect("inc_diff", lm_inc_diff) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_diff, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) + sd(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) - sd(nps_2015$inc_diff, na.rm = TRUE)) +
  labs(
    x = "own income - median income in zip code",
    y = "calories per $1 spent"
  ) + 
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Own income minus median income x gini in your zip code

``` r
effect("inc_diff * gini", lm_inc_diff_g) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_diff, fit, group = gini, color = gini, fill = gini)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2, color = NA) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) + sd(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) - sd(nps_2015$inc_diff, na.rm = TRUE)) +
  labs(
    x = "own income - median income in zip code",
    y = "grams of saturated fat per $1 spent"
  ) + 
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Sugar content

### Income

``` r
lm_inc <-
  lmer(
    sug_per ~
      inc_mid +
      Household_Size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## sug_per ~ inc_mid + Household_Size + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 173478.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9003 -0.6445 -0.1250  0.4858 26.5174 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.02612  0.1616  
    ##  Residual             0.96335  0.9815  
    ## Number of obs: 61345, groups:  zip, 15889
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error t value
    ## (Intercept)              2.625e+00  3.681e-02  71.322
    ## inc_mid                 -6.552e-06  1.755e-07 -37.322
    ## Household_Size           2.621e-02  3.994e-03   6.563
    ## Male_Head_Education     -3.891e-02  4.611e-03  -8.438
    ## Female_Head_Education   -5.526e-02  4.574e-03 -12.081
    ## Race2                    3.647e-01  1.352e-02  26.965
    ## Race3                   -1.239e-01  2.302e-02  -5.382
    ## Race4                    4.766e-02  1.940e-02   2.457
    ## Marital_Status2          1.130e-01  2.331e-02   4.848
    ## Marital_Status3          7.677e-02  2.031e-02   3.781
    ## Marital_Status4          1.079e-01  2.032e-02   5.308
    ## Male_Head_Employment1    2.887e-01  4.256e-02   6.784
    ## Male_Head_Employment2    3.044e-01  4.536e-02   6.712
    ## Male_Head_Employment3    3.126e-01  3.742e-02   8.353
    ## Male_Head_Employment9    2.586e-01  4.010e-02   6.448
    ## Female_Head_Employment1  2.667e-01  3.971e-02   6.716
    ## Female_Head_Employment2  2.596e-01  4.191e-02   6.194
    ## Female_Head_Employment3  2.869e-01  3.805e-02   7.540
    ## Female_Head_Employment9  2.501e-01  3.872e-02   6.459
    ## Male_Head_Age           -2.131e-02  3.763e-03  -5.663
    ## Female_Head_Age         -1.106e-02  3.386e-03  -3.268

    ## 
    ## Correlation matrix not shown by default, as p = 21 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc)
```

    ##                 inc_mid          Household_Size     Male_Head_Education 
    ##            -0.186697039             0.033037132            -0.077954251 
    ##   Female_Head_Education                   Race2                   Race3 
    ##            -0.087604037             0.108960562            -0.021349755 
    ##                   Race4         Marital_Status2         Marital_Status3 
    ##             0.009651689             0.028820598             0.026407830 
    ##         Marital_Status4   Male_Head_Employment1   Male_Head_Employment2 
    ##             0.035755887             0.059457331             0.045468464 
    ##   Male_Head_Employment3   Male_Head_Employment9 Female_Head_Employment1 
    ##             0.149629264             0.107762028             0.084219960 
    ## Female_Head_Employment2 Female_Head_Employment3 Female_Head_Employment9 
    ##             0.052375256             0.130890672             0.118575302 
    ##           Male_Head_Age         Female_Head_Age 
    ##            -0.072765275            -0.030375826

### Income and median income

``` r
lm_med_inc <-
  lmer(
    sug_per ~
      median_income + 
      inc_mid + 
      median_monthly_housing_cost + 
      pov_status_below_per +
      Household_Size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_med_inc)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: sug_per ~ median_income + inc_mid + median_monthly_housing_cost +  
    ##     pov_status_below_per + Household_Size + Male_Head_Education +  
    ##     Female_Head_Education + Race + Marital_Status + Male_Head_Employment +  
    ##     Female_Head_Employment + Male_Head_Age + Female_Head_Age +      (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 171828.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9117 -0.6461 -0.1248  0.4844 26.7104 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.01782  0.1335  
    ##  Residual             0.96116  0.9804  
    ## Number of obs: 60953, groups:  zip, 15531
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  2.665e+00  4.593e-02  58.026
    ## median_income                4.641e-06  5.543e-07   8.373
    ## inc_mid                     -5.694e-06  1.803e-07 -31.574
    ## median_monthly_housing_cost -3.950e-04  2.329e-05 -16.959
    ## pov_status_below_per         4.540e-01  8.349e-02   5.437
    ## Household_Size               2.804e-02  3.990e-03   7.028
    ## Male_Head_Education         -3.444e-02  4.621e-03  -7.452
    ## Female_Head_Education       -5.222e-02  4.572e-03 -11.421
    ## Race2                        3.660e-01  1.387e-02  26.384
    ## Race3                       -6.312e-02  2.318e-02  -2.723
    ## Race4                        7.094e-02  1.953e-02   3.633
    ## Marital_Status2              1.188e-01  2.327e-02   5.104
    ## Marital_Status3              8.849e-02  2.029e-02   4.360
    ## Marital_Status4              1.284e-01  2.032e-02   6.318
    ## Male_Head_Employment1        2.691e-01  4.253e-02   6.329
    ## Male_Head_Employment2        2.882e-01  4.535e-02   6.355
    ## Male_Head_Employment3        2.845e-01  3.742e-02   7.601
    ## Male_Head_Employment9        2.374e-01  4.009e-02   5.922
    ## Female_Head_Employment1      2.458e-01  3.967e-02   6.195
    ## Female_Head_Employment2      2.388e-01  4.188e-02   5.704
    ## Female_Head_Employment3      2.561e-01  3.803e-02   6.733
    ## Female_Head_Employment9      2.273e-01  3.868e-02   5.875
    ## Male_Head_Age               -2.204e-02  3.757e-03  -5.867
    ## Female_Head_Age             -1.031e-02  3.381e-03  -3.050

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_med_inc)
```

    ##               median_income                     inc_mid 
    ##                  0.09643694                 -0.16235524 
    ## median_monthly_housing_cost        pov_status_below_per 
    ##                 -0.15475901                  0.03657698 
    ##              Household_Size         Male_Head_Education 
    ##                  0.03536396                 -0.06904250 
    ##       Female_Head_Education                       Race2 
    ##                 -0.08278137                  0.10937563 
    ##                       Race3                       Race4 
    ##                 -0.01089868                  0.01434764 
    ##             Marital_Status2             Marital_Status3 
    ##                  0.03033505                  0.03043867 
    ##             Marital_Status4       Male_Head_Employment1 
    ##                  0.04255332                  0.05547379 
    ##       Male_Head_Employment2       Male_Head_Employment3 
    ##                  0.04301454                  0.13628044 
    ##       Male_Head_Employment9     Female_Head_Employment1 
    ##                  0.09901587                  0.07773276 
    ##     Female_Head_Employment2     Female_Head_Employment3 
    ##                  0.04820448                  0.11691386 
    ##     Female_Head_Employment9               Male_Head_Age 
    ##                  0.10783739                 -0.07530614 
    ##             Female_Head_Age 
    ##                 -0.02832030

### Income difference score

``` r
lm_inc_diff <-
  lmer(
    sug_per ~
      inc_mid +     
      inc_diff + 
      Household_Size +
      pov_status_below_per +
      median_monthly_housing_cost + 
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc_diff)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## sug_per ~ inc_mid + inc_diff + Household_Size + pov_status_below_per +  
    ##     median_monthly_housing_cost + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 171828.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9117 -0.6461 -0.1248  0.4844 26.7104 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.01782  0.1335  
    ##  Residual             0.96116  0.9804  
    ## Number of obs: 60953, groups:  zip, 15531
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  2.665e+00  4.593e-02  58.026
    ## inc_mid                     -1.053e-06  5.819e-07  -1.809
    ## inc_diff                    -4.641e-06  5.543e-07  -8.373
    ## Household_Size               2.804e-02  3.990e-03   7.028
    ## pov_status_below_per         4.540e-01  8.349e-02   5.437
    ## median_monthly_housing_cost -3.950e-04  2.329e-05 -16.959
    ## Male_Head_Education         -3.444e-02  4.621e-03  -7.452
    ## Female_Head_Education       -5.222e-02  4.572e-03 -11.421
    ## Race2                        3.660e-01  1.387e-02  26.384
    ## Race3                       -6.312e-02  2.318e-02  -2.723
    ## Race4                        7.094e-02  1.953e-02   3.633
    ## Marital_Status2              1.188e-01  2.327e-02   5.104
    ## Marital_Status3              8.849e-02  2.029e-02   4.360
    ## Marital_Status4              1.284e-01  2.032e-02   6.318
    ## Male_Head_Employment1        2.691e-01  4.253e-02   6.329
    ## Male_Head_Employment2        2.882e-01  4.535e-02   6.355
    ## Male_Head_Employment3        2.845e-01  3.742e-02   7.601
    ## Male_Head_Employment9        2.374e-01  4.009e-02   5.922
    ## Female_Head_Employment1      2.458e-01  3.967e-02   6.195
    ## Female_Head_Employment2      2.388e-01  4.188e-02   5.704
    ## Female_Head_Employment3      2.561e-01  3.803e-02   6.733
    ## Female_Head_Employment9      2.273e-01  3.868e-02   5.875
    ## Male_Head_Age               -2.204e-02  3.757e-03  -5.867
    ## Female_Head_Age             -1.031e-02  3.381e-03  -3.050

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc_diff)
```

    ##                     inc_mid                    inc_diff 
    ##                 -0.03002114                 -0.13803737 
    ##              Household_Size        pov_status_below_per 
    ##                  0.03536396                  0.03657698 
    ## median_monthly_housing_cost         Male_Head_Education 
    ##                 -0.15475901                 -0.06904250 
    ##       Female_Head_Education                       Race2 
    ##                 -0.08278137                  0.10937563 
    ##                       Race3                       Race4 
    ##                 -0.01089868                  0.01434764 
    ##             Marital_Status2             Marital_Status3 
    ##                  0.03033505                  0.03043867 
    ##             Marital_Status4       Male_Head_Employment1 
    ##                  0.04255332                  0.05547379 
    ##       Male_Head_Employment2       Male_Head_Employment3 
    ##                  0.04301454                  0.13628044 
    ##       Male_Head_Employment9     Female_Head_Employment1 
    ##                  0.09901587                  0.07773276 
    ##     Female_Head_Employment2     Female_Head_Employment3 
    ##                  0.04820448                  0.11691386 
    ##     Female_Head_Employment9               Male_Head_Age 
    ##                  0.10783739                 -0.07530614 
    ##             Female_Head_Age 
    ##                 -0.02832030

### Income difference score x gini

``` r
lm_inc_diff_g <-
  lmer(
    sug_per ~
      inc_mid +     
      inc_diff * gini +
      Household_Size +
      pov_status_below_per +
      median_monthly_housing_cost + 
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc_diff_g)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## sug_per ~ inc_mid + inc_diff * gini + Household_Size + pov_status_below_per +  
    ##     median_monthly_housing_cost + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: 171821.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9232 -0.6468 -0.1245  0.4836 26.7202 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.01722  0.1312  
    ##  Residual             0.96120  0.9804  
    ## Number of obs: 60953, groups:  zip, 15531
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  2.866e+00  5.976e-02  47.963
    ## inc_mid                     -8.883e-07  5.819e-07  -1.526
    ## inc_diff                    -1.738e-06  1.235e-06  -1.408
    ## gini                        -5.483e-01  1.028e-01  -5.335
    ## Household_Size               2.709e-02  3.993e-03   6.785
    ## pov_status_below_per         6.502e-01  9.015e-02   7.212
    ## median_monthly_housing_cost -3.924e-04  2.327e-05 -16.864
    ## Male_Head_Education         -3.289e-02  4.628e-03  -7.106
    ## Female_Head_Education       -5.078e-02  4.578e-03 -11.091
    ## Race2                        3.650e-01  1.387e-02  26.327
    ## Race3                       -6.100e-02  2.318e-02  -2.632
    ## Race4                        6.973e-02  1.953e-02   3.572
    ## Marital_Status2              1.184e-01  2.327e-02   5.088
    ## Marital_Status3              8.783e-02  2.029e-02   4.328
    ## Marital_Status4              1.292e-01  2.033e-02   6.355
    ## Male_Head_Employment1        2.618e-01  4.254e-02   6.154
    ## Male_Head_Employment2        2.811e-01  4.535e-02   6.198
    ## Male_Head_Employment3        2.754e-01  3.745e-02   7.353
    ## Male_Head_Employment9        2.293e-01  4.011e-02   5.716
    ## Female_Head_Employment1      2.359e-01  3.970e-02   5.941
    ## Female_Head_Employment2      2.289e-01  4.190e-02   5.463
    ## Female_Head_Employment3      2.455e-01  3.807e-02   6.448
    ## Female_Head_Employment9      2.167e-01  3.872e-02   5.597
    ## Male_Head_Age               -2.194e-02  3.756e-03  -5.840
    ## Female_Head_Age             -9.991e-03  3.381e-03  -2.955
    ## inc_diff:gini               -7.117e-06  2.635e-06  -2.701

    ## 
    ## Correlation matrix not shown by default, as p = 26 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc_diff_g)
```

    ##                     inc_mid                    inc_diff 
    ##                 -0.02532893                 -0.05168617 
    ##                        gini              Household_Size 
    ##                 -0.02502271                  0.03417003 
    ##        pov_status_below_per median_monthly_housing_cost 
    ##                  0.05238712                 -0.15375126 
    ##         Male_Head_Education       Female_Head_Education 
    ##                 -0.06593461                 -0.08049314 
    ##                       Race2                       Race3 
    ##                  0.10908609                 -0.01053175 
    ##                       Race4             Marital_Status2 
    ##                  0.01410310                  0.03023686 
    ##             Marital_Status3             Marital_Status4 
    ##                  0.03021165                  0.04281818 
    ##       Male_Head_Employment1       Male_Head_Employment2 
    ##                  0.05395316                  0.04195386 
    ##       Male_Head_Employment3       Male_Head_Employment9 
    ##                  0.13191681                  0.09560179 
    ##     Female_Head_Employment1     Female_Head_Employment2 
    ##                  0.07459705                  0.04619533 
    ##     Female_Head_Employment3     Female_Head_Employment9 
    ##                  0.11207226                  0.10283177 
    ##               Male_Head_Age             Female_Head_Age 
    ##                 -0.07495269                 -0.02743968 
    ##               inc_diff:gini 
    ##                 -0.09162545

### Regression table

``` r
stargazer(
  lm_inc, lm_med_inc, lm_inc_diff, lm_inc_diff_g,
  coef = 
    list(
      lm.beta.lmer(lm_inc), 
      lm.beta.lmer(lm_med_inc), 
      lm.beta.lmer(lm_inc_diff),
      lm.beta.lmer(lm_inc_diff_g)
    ),
  type = "html"
)
```

<table style="text-align:center">

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

sug\_per

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

median\_income

</td>

<td>

</td>

<td>

0.096<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.00000)

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_mid

</td>

<td>

\-0.187<sup>\*\*\*</sup>

</td>

<td>

\-0.162<sup>\*\*\*</sup>

</td>

<td>

\-0.030<sup>\*\*\*</sup>

</td>

<td>

\-0.025<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_diff

</td>

<td>

</td>

<td>

</td>

<td>

\-0.138<sup>\*\*\*</sup>

</td>

<td>

\-0.052<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

gini

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

\-0.025

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.103)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

median\_monthly\_housing\_cost

</td>

<td>

</td>

<td>

\-0.155<sup>\*\*\*</sup>

</td>

<td>

\-0.155<sup>\*\*\*</sup>

</td>

<td>

\-0.154<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.00002)

</td>

<td>

(0.00002)

</td>

<td>

(0.00002)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

pov\_status\_below\_per

</td>

<td>

</td>

<td>

0.037

</td>

<td>

0.037

</td>

<td>

0.052

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.083)

</td>

<td>

(0.083)

</td>

<td>

(0.090)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Household\_Size

</td>

<td>

0.033<sup>\*\*\*</sup>

</td>

<td>

0.035<sup>\*\*\*</sup>

</td>

<td>

0.035<sup>\*\*\*</sup>

</td>

<td>

0.034<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Education

</td>

<td>

\-0.078<sup>\*\*\*</sup>

</td>

<td>

\-0.069<sup>\*\*\*</sup>

</td>

<td>

\-0.069<sup>\*\*\*</sup>

</td>

<td>

\-0.066<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Education

</td>

<td>

\-0.088<sup>\*\*\*</sup>

</td>

<td>

\-0.083<sup>\*\*\*</sup>

</td>

<td>

\-0.083<sup>\*\*\*</sup>

</td>

<td>

\-0.080<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race2

</td>

<td>

0.109<sup>\*\*\*</sup>

</td>

<td>

0.109<sup>\*\*\*</sup>

</td>

<td>

0.109<sup>\*\*\*</sup>

</td>

<td>

0.109<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.014)

</td>

<td>

(0.014)

</td>

<td>

(0.014)

</td>

<td>

(0.014)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race3

</td>

<td>

\-0.021

</td>

<td>

\-0.011

</td>

<td>

\-0.011

</td>

<td>

\-0.011

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.023)

</td>

<td>

(0.023)

</td>

<td>

(0.023)

</td>

<td>

(0.023)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race4

</td>

<td>

0.010

</td>

<td>

0.014

</td>

<td>

0.014

</td>

<td>

0.014

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.019)

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status2

</td>

<td>

0.029

</td>

<td>

0.030

</td>

<td>

0.030

</td>

<td>

0.030

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.023)

</td>

<td>

(0.023)

</td>

<td>

(0.023)

</td>

<td>

(0.023)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status3

</td>

<td>

0.026

</td>

<td>

0.030

</td>

<td>

0.030

</td>

<td>

0.030

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status4

</td>

<td>

0.036<sup>\*</sup>

</td>

<td>

0.043<sup>\*\*</sup>

</td>

<td>

0.043<sup>\*\*</sup>

</td>

<td>

0.043<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

<td>

(0.020)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment1

</td>

<td>

0.059<sup>\*\*\*</sup>

</td>

<td>

0.055

</td>

<td>

0.055

</td>

<td>

0.054

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.043)

</td>

<td>

(0.043)

</td>

<td>

(0.043)

</td>

<td>

(0.043)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment2

</td>

<td>

0.045

</td>

<td>

0.043

</td>

<td>

0.043

</td>

<td>

0.042

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.045)

</td>

<td>

(0.045)

</td>

<td>

(0.045)

</td>

<td>

(0.045)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment3

</td>

<td>

0.150<sup>\*\*\*</sup>

</td>

<td>

0.136<sup>\*\*\*</sup>

</td>

<td>

0.136<sup>\*\*\*</sup>

</td>

<td>

0.132<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.037)

</td>

<td>

(0.037)

</td>

<td>

(0.037)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment9

</td>

<td>

0.108<sup>\*\*\*</sup>

</td>

<td>

0.099<sup>\*\*</sup>

</td>

<td>

0.099<sup>\*\*</sup>

</td>

<td>

0.096<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment1

</td>

<td>

0.084<sup>\*\*</sup>

</td>

<td>

0.078<sup>\*</sup>

</td>

<td>

0.078<sup>\*</sup>

</td>

<td>

0.075<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

<td>

(0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment2

</td>

<td>

0.052

</td>

<td>

0.048

</td>

<td>

0.048

</td>

<td>

0.046

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.042)

</td>

<td>

(0.042)

</td>

<td>

(0.042)

</td>

<td>

(0.042)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment3

</td>

<td>

0.131<sup>\*\*\*</sup>

</td>

<td>

0.117<sup>\*\*\*</sup>

</td>

<td>

0.117<sup>\*\*\*</sup>

</td>

<td>

0.112<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

<td>

(0.038)

</td>

<td>

(0.038)

</td>

<td>

(0.038)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment9

</td>

<td>

0.119<sup>\*\*\*</sup>

</td>

<td>

0.108<sup>\*\*\*</sup>

</td>

<td>

0.108<sup>\*\*\*</sup>

</td>

<td>

0.103<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.039)

</td>

<td>

(0.039)

</td>

<td>

(0.039)

</td>

<td>

(0.039)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Age

</td>

<td>

\-0.073<sup>\*</sup>

</td>

<td>

\-0.075<sup>\*\*\*</sup>

</td>

<td>

\-0.075<sup>\*\*\*</sup>

</td>

<td>

\-0.075<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Age

</td>

<td>

\-0.030<sup>\*\*\*</sup>

</td>

<td>

\-0.028<sup>\*\*\*</sup>

</td>

<td>

\-0.028<sup>\*\*\*</sup>

</td>

<td>

\-0.027<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.003)

</td>

<td>

(0.003)

</td>

<td>

(0.003)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_diff:gini

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

\-0.092<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.046)

</td>

<td>

(0.046)

</td>

<td>

(0.060)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

61,345

</td>

<td>

60,953

</td>

<td>

60,953

</td>

<td>

60,953

</td>

</tr>

<tr>

<td style="text-align:left">

Log Likelihood

</td>

<td>

\-86,739.070

</td>

<td>

\-85,914.090

</td>

<td>

\-85,914.090

</td>

<td>

\-85,910.820

</td>

</tr>

<tr>

<td style="text-align:left">

Akaike Inf. Crit.

</td>

<td>

173,524.100

</td>

<td>

171,880.200

</td>

<td>

171,880.200

</td>

<td>

171,877.600

</td>

</tr>

<tr>

<td style="text-align:left">

Bayesian Inf. Crit.

</td>

<td>

173,731.700

</td>

<td>

172,114.600

</td>

<td>

172,114.600

</td>

<td>

172,130.200

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="4" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

### Own income

``` r
effect("inc_mid", lm_inc) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_mid, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE) + sd(nps_2015$inc_mid, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE) - sd(nps_2015$inc_mid, na.rm = TRUE)) +
  labs(
    x = "own income",
    y = "grams of sugar per $1 spent"
  ) +
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Own income minus median income in your zip code

``` r
effect("inc_diff", lm_inc_diff) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_diff, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) + sd(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) - sd(nps_2015$inc_diff, na.rm = TRUE)) +
  labs(
    x = "own income - median income in zip code",
    y = "grams of sugar per $1 spent"
  ) + 
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Own income minus median income x gini in your zip code

``` r
effect("inc_diff * gini", lm_inc_diff_g) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_diff, fit, group = gini, color = gini, fill = gini)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2, color = NA) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) + sd(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) - sd(nps_2015$inc_diff, na.rm = TRUE)) +
  labs(
    x = "own income - median income in zip code",
    y = "grams of saturated fat per $1 spent"
  ) + 
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Saturated fat content

### Income

``` r
lm_inc <-
  lmer(
    sf_per ~
      inc_mid +
      Household_Size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## sf_per ~ inc_mid + Household_Size + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: -11835.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0657 -0.6443 -0.1015  0.5171 18.6432 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.002035 0.04511 
    ##  Residual             0.046290 0.21515 
    ## Number of obs: 61343, groups:  zip, 15889
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error t value
    ## (Intercept)              6.908e-01  8.123e-03  85.044
    ## inc_mid                 -1.565e-06  3.878e-08 -40.357
    ## Household_Size          -5.473e-03  8.809e-04  -6.213
    ## Male_Head_Education     -1.519e-02  1.018e-03 -14.930
    ## Female_Head_Education   -1.626e-02  1.009e-03 -16.114
    ## Race2                    5.905e-03  3.006e-03   1.964
    ## Race3                   -4.048e-02  5.089e-03  -7.955
    ## Race4                   -2.244e-02  4.281e-03  -5.241
    ## Marital_Status2          2.744e-02  5.139e-03   5.340
    ## Marital_Status3          2.819e-02  4.478e-03   6.297
    ## Marital_Status4          3.092e-02  4.483e-03   6.897
    ## Male_Head_Employment1    7.775e-02  9.386e-03   8.284
    ## Male_Head_Employment2    8.079e-02  1.000e-02   8.077
    ## Male_Head_Employment3    8.569e-02  8.254e-03  10.382
    ## Male_Head_Employment9    6.645e-02  8.845e-03   7.513
    ## Female_Head_Employment1  4.077e-02  8.758e-03   4.656
    ## Female_Head_Employment2  4.868e-02  9.243e-03   5.266
    ## Female_Head_Employment3  5.996e-02  8.392e-03   7.145
    ## Female_Head_Employment9  3.822e-02  8.540e-03   4.475
    ## Male_Head_Age           -2.183e-03  8.298e-04  -2.631
    ## Female_Head_Age         -4.481e-04  7.466e-04  -0.600

    ## 
    ## Correlation matrix not shown by default, as p = 21 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc)
```

    ##                 inc_mid          Household_Size     Male_Head_Education 
    ##            -0.200046823            -0.030940482            -0.136506956 
    ##   Female_Head_Education                   Race2                   Race3 
    ##            -0.115607980             0.007914181            -0.031281586 
    ##                   Race4         Marital_Status2         Marital_Status3 
    ##            -0.020378759             0.031396846             0.043500223 
    ##         Marital_Status4   Male_Head_Employment1   Male_Head_Employment2 
    ##             0.045958980             0.071817097             0.054121785 
    ##   Male_Head_Employment3   Male_Head_Employment9 Female_Head_Employment1 
    ##             0.183976689             0.124211523             0.057748464 
    ## Female_Head_Employment2 Female_Head_Employment3 Female_Head_Employment9 
    ##             0.044053567             0.122700642             0.081259460 
    ##           Male_Head_Age         Female_Head_Age 
    ##            -0.033433581            -0.005518566

### Income and median income

``` r
lm_med_inc <-
  lmer(
    sf_per ~
      median_income + 
      inc_mid + 
      median_monthly_housing_cost + 
      pov_status_below_per +
      Household_Size +
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_med_inc)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: sf_per ~ median_income + inc_mid + median_monthly_housing_cost +  
    ##     pov_status_below_per + Household_Size + Male_Head_Education +  
    ##     Female_Head_Education + Race + Marital_Status + Male_Head_Employment +  
    ##     Female_Head_Employment + Male_Head_Age + Female_Head_Age +      (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: -12525.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0932 -0.6442 -0.1018  0.5208 18.8344 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.001456 0.03815 
    ##  Residual             0.046123 0.21476 
    ## Number of obs: 60951, groups:  zip, 15531
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  6.953e-01  1.020e-02  68.136
    ## median_income                1.248e-06  1.252e-07   9.971
    ## inc_mid                     -1.349e-06  3.972e-08 -33.962
    ## median_monthly_housing_cost -1.026e-04  5.258e-06 -19.515
    ## pov_status_below_per         1.478e-01  1.881e-02   7.861
    ## Household_Size              -5.178e-03  8.786e-04  -5.893
    ## Male_Head_Education         -1.392e-02  1.018e-03 -13.677
    ## Female_Head_Education       -1.550e-02  1.007e-03 -15.396
    ## Race2                        5.540e-03  3.072e-03   1.803
    ## Race3                       -2.597e-02  5.112e-03  -5.081
    ## Race4                       -1.702e-02  4.301e-03  -3.957
    ## Marital_Status2              2.842e-02  5.124e-03   5.546
    ## Marital_Status3              3.109e-02  4.468e-03   6.958
    ## Marital_Status4              3.527e-02  4.475e-03   7.880
    ## Male_Head_Employment1        7.325e-02  9.364e-03   7.822
    ## Male_Head_Employment2        7.586e-02  9.985e-03   7.598
    ## Male_Head_Employment3        7.846e-02  8.241e-03   9.520
    ## Male_Head_Employment9        6.107e-02  8.828e-03   6.918
    ## Female_Head_Employment1      3.596e-02  8.735e-03   4.117
    ## Female_Head_Employment2      4.378e-02  9.221e-03   4.748
    ## Female_Head_Employment3      5.264e-02  8.374e-03   6.286
    ## Female_Head_Employment9      3.261e-02  8.518e-03   3.828
    ## Male_Head_Age               -2.436e-03  8.272e-04  -2.945
    ## Female_Head_Age             -2.637e-04  7.444e-04  -0.354

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_med_inc)
```

    ##               median_income                     inc_mid 
    ##                 0.116330930                -0.172521372 
    ## median_monthly_housing_cost        pov_status_below_per 
    ##                -0.180324004                 0.053435664 
    ##              Household_Size         Male_Head_Education 
    ##                -0.029291675                -0.125197602 
    ##       Female_Head_Education                       Race2 
    ##                -0.110231190                 0.007426804 
    ##                       Race3                       Race4 
    ##                -0.020111644                -0.015439669 
    ##             Marital_Status2             Marital_Status3 
    ##                 0.032559296                 0.047969940 
    ##             Marital_Status4       Male_Head_Employment1 
    ##                 0.052423051                 0.067724231 
    ##       Male_Head_Employment2       Male_Head_Employment3 
    ##                 0.050791523                 0.168603971 
    ##       Male_Head_Employment9     Female_Head_Employment1 
    ##                 0.114245352                 0.051019610 
    ##     Female_Head_Employment2     Female_Head_Employment3 
    ##                 0.039636748                 0.107804435 
    ##     Female_Head_Employment9               Male_Head_Age 
    ##                 0.069400837                -0.037338654 
    ##             Female_Head_Age 
    ##                -0.003248761

### Income difference score

``` r
lm_inc_diff <-
  lmer(
    sf_per ~
      inc_mid +     
      inc_diff + 
      Household_Size +
      pov_status_below_per +
      median_monthly_housing_cost + 
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc_diff)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: sf_per ~ inc_mid + inc_diff + Household_Size + pov_status_below_per +  
    ##     median_monthly_housing_cost + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: -12525.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0932 -0.6442 -0.1018  0.5208 18.8344 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.001456 0.03815 
    ##  Residual             0.046123 0.21476 
    ## Number of obs: 60951, groups:  zip, 15531
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  6.953e-01  1.020e-02  68.136
    ## inc_mid                     -1.008e-07  1.311e-07  -0.769
    ## inc_diff                    -1.248e-06  1.252e-07  -9.971
    ## Household_Size              -5.178e-03  8.786e-04  -5.893
    ## pov_status_below_per         1.478e-01  1.881e-02   7.861
    ## median_monthly_housing_cost -1.026e-04  5.258e-06 -19.515
    ## Male_Head_Education         -1.392e-02  1.018e-03 -13.677
    ## Female_Head_Education       -1.550e-02  1.007e-03 -15.396
    ## Race2                        5.540e-03  3.072e-03   1.803
    ## Race3                       -2.597e-02  5.112e-03  -5.081
    ## Race4                       -1.702e-02  4.301e-03  -3.957
    ## Marital_Status2              2.842e-02  5.124e-03   5.546
    ## Marital_Status3              3.109e-02  4.468e-03   6.958
    ## Marital_Status4              3.527e-02  4.475e-03   7.880
    ## Male_Head_Employment1        7.325e-02  9.364e-03   7.822
    ## Male_Head_Employment2        7.586e-02  9.985e-03   7.598
    ## Male_Head_Employment3        7.846e-02  8.241e-03   9.520
    ## Male_Head_Employment9        6.107e-02  8.828e-03   6.918
    ## Female_Head_Employment1      3.596e-02  8.735e-03   4.117
    ## Female_Head_Employment2      4.378e-02  9.221e-03   4.748
    ## Female_Head_Employment3      5.264e-02  8.374e-03   6.286
    ## Female_Head_Employment9      3.261e-02  8.518e-03   3.828
    ## Male_Head_Age               -2.436e-03  8.272e-04  -2.945
    ## Female_Head_Age             -2.637e-04  7.444e-04  -0.354

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc_diff)
```

    ##                     inc_mid                    inc_diff 
    ##                -0.012892798                -0.166505364 
    ##              Household_Size        pov_status_below_per 
    ##                -0.029291675                 0.053435664 
    ## median_monthly_housing_cost         Male_Head_Education 
    ##                -0.180324004                -0.125197602 
    ##       Female_Head_Education                       Race2 
    ##                -0.110231190                 0.007426804 
    ##                       Race3                       Race4 
    ##                -0.020111644                -0.015439669 
    ##             Marital_Status2             Marital_Status3 
    ##                 0.032559296                 0.047969940 
    ##             Marital_Status4       Male_Head_Employment1 
    ##                 0.052423051                 0.067724231 
    ##       Male_Head_Employment2       Male_Head_Employment3 
    ##                 0.050791523                 0.168603971 
    ##       Male_Head_Employment9     Female_Head_Employment1 
    ##                 0.114245352                 0.051019610 
    ##     Female_Head_Employment2     Female_Head_Employment3 
    ##                 0.039636748                 0.107804435 
    ##     Female_Head_Employment9               Male_Head_Age 
    ##                 0.069400837                -0.037338654 
    ##             Female_Head_Age 
    ##                -0.003248761

### Income difference score x gini

``` r
lm_inc_diff_g <-
  lmer(
    sf_per ~
      inc_mid +     
      inc_diff * gini +
      Household_Size +
      pov_status_below_per +
      median_monthly_housing_cost + 
      Male_Head_Education +
      Female_Head_Education +
      Race +
      Marital_Status +
      Male_Head_Employment +
      Female_Head_Employment +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = nps_2015
  )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(lm_inc_diff_g)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## sf_per ~ inc_mid + inc_diff * gini + Household_Size + pov_status_below_per +  
    ##     median_monthly_housing_cost + Male_Head_Education + Female_Head_Education +  
    ##     Race + Marital_Status + Male_Head_Employment + Female_Head_Employment +  
    ##     Male_Head_Age + Female_Head_Age + (1 | zip)
    ##    Data: nps_2015
    ## 
    ## REML criterion at convergence: -12557.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0967 -0.6448 -0.1007  0.5201 18.8488 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  zip      (Intercept) 0.001391 0.03729 
    ##  Residual             0.046130 0.21478 
    ## Number of obs: 60951, groups:  zip, 15531
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error t value
    ## (Intercept)                  7.563e-01  1.334e-02  56.702
    ## inc_mid                     -4.825e-08  1.309e-07  -0.369
    ## inc_diff                    -2.233e-07  2.741e-07  -0.814
    ## gini                        -1.670e-01  2.315e-02  -7.213
    ## Household_Size              -5.460e-03  8.790e-04  -6.211
    ## pov_status_below_per         2.087e-01  2.027e-02  10.293
    ## median_monthly_housing_cost -1.018e-04  5.245e-06 -19.406
    ## Male_Head_Education         -1.346e-02  1.019e-03 -13.206
    ## Female_Head_Education       -1.507e-02  1.008e-03 -14.956
    ## Race2                        5.270e-03  3.069e-03   1.717
    ## Race3                       -2.532e-02  5.110e-03  -4.954
    ## Race4                       -1.738e-02  4.299e-03  -4.043
    ## Marital_Status2              2.826e-02  5.122e-03   5.517
    ## Marital_Status3              3.085e-02  4.466e-03   6.906
    ## Marital_Status4              3.538e-02  4.476e-03   7.903
    ## Male_Head_Employment1        7.103e-02  9.364e-03   7.586
    ## Male_Head_Employment2        7.374e-02  9.984e-03   7.386
    ## Male_Head_Employment3        7.576e-02  8.245e-03   9.189
    ## Male_Head_Employment9        5.866e-02  8.830e-03   6.643
    ## Female_Head_Employment1      3.299e-02  8.739e-03   3.775
    ## Female_Head_Employment2      4.080e-02  9.224e-03   4.423
    ## Female_Head_Employment3      4.948e-02  8.380e-03   5.904
    ## Female_Head_Employment9      2.947e-02  8.524e-03   3.457
    ## Male_Head_Age               -2.412e-03  8.268e-04  -2.917
    ## Female_Head_Age             -1.672e-04  7.441e-04  -0.225
    ## inc_diff:gini               -2.502e-06  5.824e-07  -4.296

    ## 
    ## Correlation matrix not shown by default, as p = 26 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
lm.beta.lmer(lm_inc_diff_g)
```

    ##                     inc_mid                    inc_diff 
    ##                -0.006171381                -0.029785693 
    ##                        gini              Household_Size 
    ##                -0.034187904                -0.030887285 
    ##        pov_status_below_per median_monthly_housing_cost 
    ##                 0.075427604                -0.178896694 
    ##         Male_Head_Education       Female_Head_Education 
    ##                -0.121033370                -0.107189196 
    ##                       Race2                       Race3 
    ##                 0.007064384                -0.019603036 
    ##                       Race4             Marital_Status2 
    ##                -0.015770136                 0.032373373 
    ##             Marital_Status3             Marital_Status4 
    ##                 0.047595932                 0.052588295 
    ##       Male_Head_Employment1       Male_Head_Employment2 
    ##                 0.065672937                 0.049372075 
    ##       Male_Head_Employment3       Male_Head_Employment9 
    ##                 0.162804161                 0.109729811 
    ##     Female_Head_Employment1     Female_Head_Employment2 
    ##                 0.046802316                 0.036938863 
    ##     Female_Head_Employment3     Female_Head_Employment9 
    ##                 0.101331845                 0.062711689 
    ##               Male_Head_Age             Female_Head_Age 
    ##                -0.036957623                -0.002059978 
    ##               inc_diff:gini 
    ##                -0.144468288

### Regression table

``` r
stargazer(
  lm_inc, lm_med_inc, lm_inc_diff, lm_inc_diff_g,
  coef = 
    list(
      lm.beta.lmer(lm_inc), 
      lm.beta.lmer(lm_med_inc), 
      lm.beta.lmer(lm_inc_diff),
      lm.beta.lmer(lm_inc_diff_g)
    ),
  type = "html"
)
```

<table style="text-align:center">

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

sf\_per

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

median\_income

</td>

<td>

</td>

<td>

0.116<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.00000)

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_mid

</td>

<td>

\-0.200<sup>\*\*\*</sup>

</td>

<td>

\-0.173<sup>\*\*\*</sup>

</td>

<td>

\-0.013<sup>\*\*\*</sup>

</td>

<td>

\-0.006<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_diff

</td>

<td>

</td>

<td>

</td>

<td>

\-0.167<sup>\*\*\*</sup>

</td>

<td>

\-0.030<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

gini

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

\-0.034

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.023)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

median\_monthly\_housing\_cost

</td>

<td>

</td>

<td>

\-0.180<sup>\*\*\*</sup>

</td>

<td>

\-0.180<sup>\*\*\*</sup>

</td>

<td>

\-0.179<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.00001)

</td>

<td>

(0.00001)

</td>

<td>

(0.00001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

pov\_status\_below\_per

</td>

<td>

</td>

<td>

0.053<sup>\*\*\*</sup>

</td>

<td>

0.053<sup>\*\*\*</sup>

</td>

<td>

0.075<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

(0.019)

</td>

<td>

(0.019)

</td>

<td>

(0.020)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Household\_Size

</td>

<td>

\-0.031<sup>\*\*\*</sup>

</td>

<td>

\-0.029<sup>\*\*\*</sup>

</td>

<td>

\-0.029<sup>\*\*\*</sup>

</td>

<td>

\-0.031<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Education

</td>

<td>

\-0.137<sup>\*\*\*</sup>

</td>

<td>

\-0.125<sup>\*\*\*</sup>

</td>

<td>

\-0.125<sup>\*\*\*</sup>

</td>

<td>

\-0.121<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Education

</td>

<td>

\-0.116<sup>\*\*\*</sup>

</td>

<td>

\-0.110<sup>\*\*\*</sup>

</td>

<td>

\-0.110<sup>\*\*\*</sup>

</td>

<td>

\-0.107<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race2

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

<td>

0.007<sup>\*\*</sup>

</td>

<td>

0.007<sup>\*\*</sup>

</td>

<td>

0.007<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.003)

</td>

<td>

(0.003)

</td>

<td>

(0.003)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race3

</td>

<td>

\-0.031<sup>\*\*\*</sup>

</td>

<td>

\-0.020<sup>\*\*\*</sup>

</td>

<td>

\-0.020<sup>\*\*\*</sup>

</td>

<td>

\-0.020<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Race4

</td>

<td>

\-0.020<sup>\*\*\*</sup>

</td>

<td>

\-0.015<sup>\*\*\*</sup>

</td>

<td>

\-0.015<sup>\*\*\*</sup>

</td>

<td>

\-0.016<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status2

</td>

<td>

0.031<sup>\*\*\*</sup>

</td>

<td>

0.033<sup>\*\*\*</sup>

</td>

<td>

0.033<sup>\*\*\*</sup>

</td>

<td>

0.032<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status3

</td>

<td>

0.044<sup>\*\*\*</sup>

</td>

<td>

0.048<sup>\*\*\*</sup>

</td>

<td>

0.048<sup>\*\*\*</sup>

</td>

<td>

0.048<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Marital\_Status4

</td>

<td>

0.046<sup>\*\*\*</sup>

</td>

<td>

0.052<sup>\*\*\*</sup>

</td>

<td>

0.052<sup>\*\*\*</sup>

</td>

<td>

0.053<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment1

</td>

<td>

0.072<sup>\*\*\*</sup>

</td>

<td>

0.068<sup>\*\*\*</sup>

</td>

<td>

0.068<sup>\*\*\*</sup>

</td>

<td>

0.066<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment2

</td>

<td>

0.054<sup>\*\*\*</sup>

</td>

<td>

0.051<sup>\*\*\*</sup>

</td>

<td>

0.051<sup>\*\*\*</sup>

</td>

<td>

0.049<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.010)

</td>

<td>

(0.010)

</td>

<td>

(0.010)

</td>

<td>

(0.010)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment3

</td>

<td>

0.184<sup>\*\*\*</sup>

</td>

<td>

0.169<sup>\*\*\*</sup>

</td>

<td>

0.169<sup>\*\*\*</sup>

</td>

<td>

0.163<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Employment9

</td>

<td>

0.124<sup>\*\*\*</sup>

</td>

<td>

0.114<sup>\*\*\*</sup>

</td>

<td>

0.114<sup>\*\*\*</sup>

</td>

<td>

0.110<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment1

</td>

<td>

0.058<sup>\*\*\*</sup>

</td>

<td>

0.051<sup>\*\*\*</sup>

</td>

<td>

0.051<sup>\*\*\*</sup>

</td>

<td>

0.047<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment2

</td>

<td>

0.044<sup>\*\*\*</sup>

</td>

<td>

0.040<sup>\*\*\*</sup>

</td>

<td>

0.040<sup>\*\*\*</sup>

</td>

<td>

0.037<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment3

</td>

<td>

0.123<sup>\*\*\*</sup>

</td>

<td>

0.108<sup>\*\*\*</sup>

</td>

<td>

0.108<sup>\*\*\*</sup>

</td>

<td>

0.101<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Employment9

</td>

<td>

0.081<sup>\*\*\*</sup>

</td>

<td>

0.069<sup>\*\*\*</sup>

</td>

<td>

0.069<sup>\*\*\*</sup>

</td>

<td>

0.063<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

<td>

(0.009)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Male\_Head\_Age

</td>

<td>

\-0.033<sup>\*\*\*</sup>

</td>

<td>

\-0.037<sup>\*\*\*</sup>

</td>

<td>

\-0.037<sup>\*\*\*</sup>

</td>

<td>

\-0.037<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Female\_Head\_Age

</td>

<td>

\-0.006<sup>\*\*\*</sup>

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

<td>

\-0.002<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

inc\_diff:gini

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

\-0.144<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.010)

</td>

<td>

(0.010)

</td>

<td>

(0.013)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

61,343

</td>

<td>

60,951

</td>

<td>

60,951

</td>

<td>

60,951

</td>

</tr>

<tr>

<td style="text-align:left">

Log Likelihood

</td>

<td>

5,917.740

</td>

<td>

6,262.615

</td>

<td>

6,262.615

</td>

<td>

6,278.629

</td>

</tr>

<tr>

<td style="text-align:left">

Akaike Inf. Crit.

</td>

<td>

\-11,789.480

</td>

<td>

\-12,473.230

</td>

<td>

\-12,473.230

</td>

<td>

\-12,501.260

</td>

</tr>

<tr>

<td style="text-align:left">

Bayesian Inf. Crit.

</td>

<td>

\-11,581.920

</td>

<td>

\-12,238.770

</td>

<td>

\-12,238.770

</td>

<td>

\-12,248.760

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="4" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

### Own income

``` r
effect("inc_mid", lm_inc) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_mid, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE) + sd(nps_2015$inc_mid, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_mid, na.rm = TRUE) - sd(nps_2015$inc_mid, na.rm = TRUE)) +
  labs(
    x = "own income",
    y = "grams of saturated fat per $1 spent"
  ) +
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### Own income minus median income in your zip code

``` r
effect("inc_diff", lm_inc_diff) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_diff, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) + sd(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) - sd(nps_2015$inc_diff, na.rm = TRUE)) +
  labs(
    x = "own income - median income in zip code",
    y = "grams of saturated fat per $1 spent"
  ) + 
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Own income minus median income x gini in your zip code

``` r
effect("inc_diff * gini", lm_inc_diff_g) %>% 
  as_tibble() %>% 
  ggplot(aes(inc_diff, fit, group = gini, color = gini, fill = gini)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha  = 0.2, color = NA) +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) + sd(nps_2015$inc_diff, na.rm = TRUE)) +
  geom_vline(xintercept = mean(nps_2015$inc_diff, na.rm = TRUE) - sd(nps_2015$inc_diff, na.rm = TRUE)) +
  labs(
    x = "own income - median income in zip code",
    y = "grams of saturated fat per $1 spent"
  ) + 
  theme_bw()
```

![](nielsen_secondary_data_analysis_nutrition_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->
