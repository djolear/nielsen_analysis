Nielsen Relative Status Regression Analysis
================
Daniel O’Leary
4/12/2021

  - [Setup](#setup)
      - [Load Packages](#load-packages)
      - [Load Data](#load-data)
  - [Analysis](#analysis)
      - [Demographic reference income](#demographic-reference-income)
          - [Outcome: % of monthly household calorie budget that goes to
            QFAHPD healthy
            categories](#outcome--of-monthly-household-calorie-budget-that-goes-to-qfahpd-healthy-categories)
              - [Fit model](#fit-model)
              - [Plot results](#plot-results)
          - [Outcome: % of monthly household food spend that goes to
            QFAHPD healthy
            categories](#outcome--of-monthly-household-food-spend-that-goes-to-qfahpd-healthy-categories)
              - [Fit model](#fit-model-1)
              - [Plot results](#plot-results-1)
      - [Geographic reference income](#geographic-reference-income)
          - [Outcome: % of monthly household calorie budget that goes to
            QFAHPD healthy
            categories](#outcome--of-monthly-household-calorie-budget-that-goes-to-qfahpd-healthy-categories-1)
              - [Fit model](#fit-model-2)
              - [Plot results](#plot-results-2)
          - [Outcome: % of monthly household food spend that goes to
            QFAHPD healthy
            categories](#outcome--of-monthly-household-food-spend-that-goes-to-qfahpd-healthy-categories-1)
              - [Fit model](#fit-model-3)
              - [Plot results](#plot-results-3)

# Setup

## Load Packages

## Load Data

``` r
qh_calories_imputed_sc_by_household_monthly <-
  read_csv("D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/combined/qh_calories_imputed_sc_by_household_monthly.csv") %>% 
  mutate(across(c(month, year), as.factor))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   Wic_Indicator_Current = col_logical(),
    ##   state_fips = col_character(),
    ##   cty_fips = col_character(),
    ##   fips_code = col_character(),
    ##   zip = col_character(),
    ##   dentists_scale = col_logical(),
    ##   therapists_scale = col_logical(),
    ##   Panel_Year_scale = col_logical()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 10100733 parsing failures.
    ##     row            col           expected              actual                                                                                                                                                file
    ## 4746932 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/combined/qh_calories_imputed_sc_by_household_monthly.csv'
    ## 4746933 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/combined/qh_calories_imputed_sc_by_household_monthly.csv'
    ## 4746934 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/combined/qh_calories_imputed_sc_by_household_monthly.csv'
    ## 4746935 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/combined/qh_calories_imputed_sc_by_household_monthly.csv'
    ## 4746936 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/combined/qh_calories_imputed_sc_by_household_monthly.csv'
    ## ....... .............. .................. ................... ...................................................................................................................................................
    ## See problems(...) for more details.

``` r
qh_spend_by_household_monthly <-
  read_csv("D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_monthly/combined/qh_spend_by_household_monthly.csv") %>% 
  mutate(across(c(month, year), as.factor))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   Wic_Indicator_Current = col_logical(),
    ##   state_fips = col_character(),
    ##   cty_fips = col_character(),
    ##   fips_code = col_character(),
    ##   zip = col_character(),
    ##   dentists_scale = col_logical(),
    ##   therapists_scale = col_logical(),
    ##   Panel_Year_scale = col_logical()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 10167706 parsing failures.
    ##     row            col           expected              actual                                                                                                                 file
    ## 5000344 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_monthly/combined/qh_spend_by_household_monthly.csv'
    ## 5000345 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_monthly/combined/qh_spend_by_household_monthly.csv'
    ## 5000346 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_monthly/combined/qh_spend_by_household_monthly.csv'
    ## 5000347 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_monthly/combined/qh_spend_by_household_monthly.csv'
    ## 5000348 dentists_scale 1/0/T/F/TRUE/FALSE 0.04515308219498692 'D:/data/nielsen/spend_extracts/qfahpd_health_spend_by_household_monthly/combined/qh_spend_by_household_monthly.csv'
    ## ....... .............. .................. ................... ....................................................................................................................
    ## See problems(...) for more details.

# Analysis

## Demographic reference income

### Outcome: % of monthly household calorie budget that goes to QFAHPD healthy categories

#### Fit model

``` r
lm1 <-
  lm(
    yes_scale ~
      income_demo_ranger_sar_scale +
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale + 
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_home_value_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Marital_Status +
      household_size_scale +
      month + 
      year,
      # (1|quarter) +
      # (1 + income_demo_ranger_sar_scale|fip_code) +
      # (1 + income_scale | fips_code),
    data = 
      qh_calories_imputed_sc_by_household_monthly %>% 
      filter(year %in% c(2004:2016))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ income_demo_ranger_sar_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_calories_imputed_sc_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1993 -0.7032 -0.1977  0.4847  5.6187 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                    -0.0283284  0.0027365  -10.352  < 2e-16 ***
    ## income_demo_ranger_sar_scale   -0.1313017  0.0006729 -195.131  < 2e-16 ***
    ## income_scale                    0.0599830  0.0004303  139.402  < 2e-16 ***
    ## Male_Head_Education_scale       0.1207757  0.0007130  169.393  < 2e-16 ***
    ## Female_Head_Education_scale     0.0673240  0.0005203  129.403  < 2e-16 ***
    ## Male_Head_Age_scale             0.0391015  0.0008228   47.523  < 2e-16 ***
    ## Female_Head_Age_scale           0.0135427  0.0005728   23.643  < 2e-16 ***
    ## Male_Head_Employment            0.0015758  0.0001579    9.983  < 2e-16 ***
    ## Female_Head_Employment          0.0027801  0.0001193   23.305  < 2e-16 ***
    ## median_home_value_county_scale  0.0178170  0.0003878   45.949  < 2e-16 ***
    ## land_area_2010_scale           -0.0138996  0.0003691  -37.662  < 2e-16 ***
    ## total_pop_county_scale         -0.0009144  0.0004056   -2.255   0.0242 *  
    ## Race                            0.0503608  0.0004947  101.803  < 2e-16 ***
    ## Marital_Status                 -0.0055397  0.0005090  -10.883  < 2e-16 ***
    ## household_size_scale           -0.0660481  0.0004441 -148.732  < 2e-16 ***
    ## month2                         -0.0554409  0.0017034  -32.547  < 2e-16 ***
    ## month3                         -0.0616553  0.0016963  -36.347  < 2e-16 ***
    ## month4                         -0.0762160  0.0016975  -44.899  < 2e-16 ***
    ## month5                         -0.1348105  0.0016976  -79.412  < 2e-16 ***
    ## month6                         -0.1450418  0.0017005  -85.295  < 2e-16 ***
    ## month7                         -0.1508114  0.0016996  -88.734  < 2e-16 ***
    ## month8                         -0.1479463  0.0016998  -87.040  < 2e-16 ***
    ## month9                         -0.1230457  0.0017014  -72.321  < 2e-16 ***
    ## month10                        -0.1307284  0.0017004  -76.881  < 2e-16 ***
    ## month11                         0.0301934  0.0017031   17.728  < 2e-16 ***
    ## month12                        -0.0962020  0.0017065  -56.375  < 2e-16 ***
    ## year2005                       -0.0572992  0.0025525  -22.448  < 2e-16 ***
    ## year2006                        0.0201229  0.0025563    7.872 3.49e-15 ***
    ## year2007                        0.0440370  0.0023698   18.582  < 2e-16 ***
    ## year2008                        0.0465489  0.0023800   19.558  < 2e-16 ***
    ## year2009                        0.0490917  0.0023865   20.571  < 2e-16 ***
    ## year2010                        0.0504579  0.0023877   21.132  < 2e-16 ***
    ## year2011                        0.0514353  0.0023805   21.607  < 2e-16 ***
    ## year2012                        0.0331761  0.0023807   13.936  < 2e-16 ***
    ## year2013                        0.0322551  0.0023778   13.565  < 2e-16 ***
    ## year2014                        0.0419585  0.0023788   17.639  < 2e-16 ***
    ## year2015                        0.0510034  0.0023850   21.385  < 2e-16 ***
    ## year2016                        0.0586198  0.0023805   24.625  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9872 on 8131760 degrees of freedom
    ##   (221061 observations deleted due to missingness)
    ## Multiple R-squared:  0.02599,    Adjusted R-squared:  0.02598 
    ## F-statistic:  5864 on 37 and 8131760 DF,  p-value: < 2.2e-16

``` r
regclass::VIF(lm1)
```

    ##                                    GVIF Df GVIF^(1/(2*Df))
    ## income_demo_ranger_sar_scale   3.702402  1        1.924163
    ## income_scale                   1.532642  1        1.237999
    ## Male_Head_Education_scale      4.231935  1        2.057167
    ## Female_Head_Education_scale    2.231456  1        1.493806
    ## Male_Head_Age_scale            5.639747  1        2.374815
    ## Female_Head_Age_scale          2.700611  1        1.643353
    ## Male_Head_Employment           2.255808  1        1.501935
    ## Female_Head_Employment         1.458850  1        1.207829
    ## median_home_value_county_scale 1.246158  1        1.116315
    ## land_area_2010_scale           1.132284  1        1.064088
    ## total_pop_county_scale         1.347087  1        1.160641
    ## Race                           1.053514  1        1.026408
    ## Marital_Status                 2.811228  1        1.676672
    ## household_size_scale           1.629530  1        1.276531
    ## month                          1.000035 11        1.000002
    ## year                           1.144598 12        1.005643

``` r
lm.beta::lm.beta(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ income_demo_ranger_sar_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_calories_imputed_sc_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Standardized Coefficients::
    ##                    (Intercept)   income_demo_ranger_sar_scale 
    ##                   0.0000000000                  -0.1299442928 
    ##                   income_scale      Male_Head_Education_scale 
    ##                   0.0597281304                   0.1206020613 
    ##    Female_Head_Education_scale            Male_Head_Age_scale 
    ##                   0.0669005542                   0.0390592068 
    ##          Female_Head_Age_scale           Male_Head_Employment 
    ##                   0.0134468652                   0.0051892159 
    ##         Female_Head_Employment median_home_value_county_scale 
    ##                   0.0097420386                   0.0177523862 
    ##           land_area_2010_scale         total_pop_county_scale 
    ##                  -0.0138696964                  -0.0009056233 
    ##                           Race                 Marital_Status 
    ##                   0.0361634387                  -0.0063149771 
    ##           household_size_scale                         month2 
    ##                  -0.0657092576                  -0.0152792348 
    ##                         month3                         month4 
    ##                  -0.0171231930                  -0.0211388919 
    ##                         month5                         month6 
    ##                  -0.0373864297                  -0.0400996941 
    ##                         month7                         month8 
    ##                  -0.0417342128                  -0.0409341427 
    ##                         month9                        month10 
    ##                  -0.0339852939                  -0.0361448529 
    ##                        month11                        month12 
    ##                   0.0083239679                  -0.0264266172 
    ##                       year2005                       year2006 
    ##                  -0.0130151127                   0.0044747228 
    ##                       year2007                       year2008 
    ##                   0.0125513285                   0.0130730609 
    ##                       year2009                       year2010 
    ##                   0.0136798909                   0.0140626360 
    ##                       year2011                       year2012 
    ##                   0.0145169033                   0.0092495065 
    ##                       year2013                       year2014 
    ##                   0.0090304333                   0.0117970837 
    ##                       year2015                       year2016 
    ##                   0.0142996097                   0.0166595936

``` r
tidy_lm1 <- tidy(lm1)
```

#### Plot results

``` r
est <- 
  tidy_lm1 %>% 
  dplyr::filter(term == "income_demo_ranger_sar_scale") %>% 
  dplyr::select(estimate)

tidy_lm1 <-
  tidy_lm1 %>% 
  mutate(
    dot_color = ifelse(estimate < 0, "red1", ifelse(estimate > 0, "dodgerblue2", NA)),
    se = std.error
  )

tidy_lm1 <-
  tidy_lm1 %>% 
  filter(
    term == "income_demo_ranger_sar_scale" |
    term == "income_scale" |
    term == "Male_Head_Education_scale" | 
    term == "Female_Head_Education_scale" 
  ) %>% 
  mutate(
    variable = 
      case_when(
        term == "income_demo_ranger_sar_scale" ~ "demographic reference \n median income",
        term == "income_scale" ~ "household income",
        term == "Male_Head_Education_scale" ~ "male head education",
        term == "Female_Head_Education_scale" ~ "female head education"
      )
  ) 

col <- as.character(tidy_lm1$dot_color)
names(col) <- as.character(tidy_lm1$dot_color)

qh_dri_calories_monthly <-
  tidy_lm1 %>% 
  ggplot(aes(reorder(as.factor(variable), estimate), estimate)) +
  geom_point(aes(color = dot_color), size = 4) +
  geom_errorbar(aes(ymin = estimate - 2 * se, ymax = estimate + 2 * se), width = 0) + 
  scale_color_manual(values = col) +
  geom_hline(yintercept = -abs(est$estimate), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = abs(est$estimate), linetype = "dashed", color = "red") +
  scale_y_continuous(
    breaks = c( -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
    limits = c(-0.35, 0.35)
  ) +
  labs(
    y = "standardized beta",
    x = "variable"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  coord_flip()

qh_dri_calories_monthly
```

![](qfahpd_health_cross_sectional_regression_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave(
  "G:/My Drive/research/projects/niel/nielsen_relative_income/plots/qh_dri_calories_monthly.png",
  qh_dri_calories_monthly,
  width = 4,
  height = 2,
  dpi = 750
)
```

### Outcome: % of monthly household food spend that goes to QFAHPD healthy categories

#### Fit model

``` r
lm1 <-
  lm(
    yes_scale ~
      income_demo_ranger_sar_scale +
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale + 
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_home_value_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Marital_Status +
      household_size_scale +
      month + 
      year,
      # (1|quarter) +
      # (1 + income_demo_ranger_sar_scale|fip_code) +
      # (1 + income_scale | fips_code),
    data = 
      qh_spend_by_household_monthly %>% 
      filter(year %in% c(2004:2016))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ income_demo_ranger_sar_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_spend_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0807 -0.6604 -0.0903  0.5514  7.7506 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                    -0.0537586  0.0022685  -23.698  < 2e-16 ***
    ## income_demo_ranger_sar_scale   -0.1778094  0.0006493 -273.841  < 2e-16 ***
    ## income_scale                    0.0745903  0.0004173  178.749  < 2e-16 ***
    ## Male_Head_Education_scale       0.1852530  0.0006910  268.108  < 2e-16 ***
    ## Female_Head_Education_scale     0.1109551  0.0005035  220.355  < 2e-16 ***
    ## Male_Head_Age_scale             0.0368711  0.0007968   46.277  < 2e-16 ***
    ## Female_Head_Age_scale           0.0258698  0.0005547   46.637  < 2e-16 ***
    ## Male_Head_Employment           -0.0012661  0.0001534   -8.256  < 2e-16 ***
    ## Female_Head_Employment          0.0008826  0.0001158    7.624 2.46e-14 ***
    ## median_home_value_county_scale  0.0695110  0.0003763  184.725  < 2e-16 ***
    ## land_area_2010_scale           -0.0067125  0.0003585  -18.726  < 2e-16 ***
    ## total_pop_county_scale         -0.0023705  0.0003941   -6.015 1.80e-09 ***
    ## Race                            0.0315651  0.0004772   66.140  < 2e-16 ***
    ## Marital_Status                 -0.0087775  0.0004922  -17.832  < 2e-16 ***
    ## household_size_scale           -0.0879755  0.0004300 -204.590  < 2e-16 ***
    ## month2                          0.0026430  0.0016521    1.600    0.110    
    ## month3                          0.0126760  0.0016455    7.703 1.32e-14 ***
    ## month4                          0.0114859  0.0016462    6.977 3.01e-12 ***
    ## month5                         -0.0019935  0.0016463   -1.211    0.226    
    ## month6                          0.0081841  0.0016486    4.964 6.90e-07 ***
    ## month7                         -0.0236451  0.0016480  -14.348  < 2e-16 ***
    ## month8                         -0.0734148  0.0016480  -44.547  < 2e-16 ***
    ## month9                         -0.0583246  0.0016493  -35.362  < 2e-16 ***
    ## month10                        -0.0775279  0.0016486  -47.025  < 2e-16 ***
    ## month11                        -0.0174343  0.0016516  -10.556  < 2e-16 ***
    ## month12                        -0.1056957  0.0016542  -63.895  < 2e-16 ***
    ## year2005                       -0.0821932  0.0020732  -39.646  < 2e-16 ***
    ## year2006                        0.0229663  0.0020759   11.063  < 2e-16 ***
    ## year2007                        0.0526159  0.0018475   28.479  < 2e-16 ***
    ## year2008                        0.0554678  0.0018601   29.820  < 2e-16 ***
    ## year2009                        0.0593894  0.0018681   31.792  < 2e-16 ***
    ## year2010                        0.0618781  0.0018695   33.100  < 2e-16 ***
    ## year2011                        0.0638042  0.0018604   34.296  < 2e-16 ***
    ## year2012                        0.0393556  0.0018606   21.152  < 2e-16 ***
    ## year2013                        0.0381817  0.0018569   20.562  < 2e-16 ***
    ## year2014                        0.0514459  0.0018572   27.701  < 2e-16 ***
    ## year2015                        0.0638835  0.0018635   34.282  < 2e-16 ***
    ## year2016                        0.0741399  0.0018571   39.923  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9746 on 8436196 degrees of freedom
    ##   (191306 observations deleted due to missingness)
    ## Multiple R-squared:  0.05036,    Adjusted R-squared:  0.05036 
    ## F-statistic: 1.209e+04 on 37 and 8436196 DF,  p-value: < 2.2e-16

``` r
regclass::VIF(lm1)
```

    ##                                    GVIF Df GVIF^(1/(2*Df))
    ## income_demo_ranger_sar_scale   3.684653  1        1.919545
    ## income_scale                   1.536756  1        1.239660
    ## Male_Head_Education_scale      4.230132  1        2.056729
    ## Female_Head_Education_scale    2.225429  1        1.491787
    ## Male_Head_Age_scale            5.629604  1        2.372679
    ## Female_Head_Age_scale          2.699329  1        1.642964
    ## Male_Head_Employment           2.263101  1        1.504361
    ## Female_Head_Employment         1.461413  1        1.208889
    ## median_home_value_county_scale 1.245931  1        1.116213
    ## land_area_2010_scale           1.132021  1        1.063965
    ## total_pop_county_scale         1.347216  1        1.160696
    ## Race                           1.054177  1        1.026731
    ## Marital_Status                 2.809165  1        1.676056
    ## household_size_scale           1.630544  1        1.276927
    ## month                          1.000030 11        1.000001
    ## year                           1.141678 12        1.005536

``` r
lm.beta::lm.beta(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ income_demo_ranger_sar_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_spend_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Standardized Coefficients::
    ##                    (Intercept)   income_demo_ranger_sar_scale 
    ##                   0.0000000000                  -0.1763611092 
    ##                   income_scale      Male_Head_Education_scale 
    ##                   0.0743448981                   0.1850088384 
    ##    Female_Head_Education_scale            Male_Head_Age_scale 
    ##                   0.1102900284                   0.0368388894 
    ##          Female_Head_Age_scale           Male_Head_Employment 
    ##                   0.0257077736                  -0.0041669906 
    ##         Female_Head_Employment median_home_value_county_scale 
    ##                   0.0030922159                   0.0691797388 
    ##           land_area_2010_scale         total_pop_county_scale 
    ##                  -0.0066848110                  -0.0023424310 
    ##                           Race                 Marital_Status 
    ##                   0.0227838049                  -0.0100277002 
    ##           household_size_scale                         month2 
    ##                  -0.0876509691                   0.0007282578 
    ##                         month3                         month4 
    ##                   0.0035186834                   0.0031856833 
    ##                         month5                         month6 
    ##                  -0.0005528872                   0.0022639242 
    ##                         month7                         month8 
    ##                  -0.0065454858                  -0.0203214877 
    ##                         month9                        month10 
    ##                  -0.0161211177                  -0.0214454384 
    ##                        month11                        month12 
    ##                  -0.0048067406                  -0.0290570967 
    ##                       year2005                       year2006 
    ##                  -0.0184130207                   0.0050368785 
    ##                       year2007                       year2008 
    ##                   0.0148096348                   0.0153814872 
    ##                       year2009                       year2010 
    ##                   0.0163385264                   0.0170250671 
    ##                       year2011                       year2012 
    ##                   0.0177772159                   0.0108431846 
    ##                       year2013                       year2014 
    ##                   0.0105659044                   0.0143029982 
    ##                       year2015                       year2016 
    ##                   0.0177297500                   0.0208679529

``` r
tidy_lm1 <- tidy(lm1)
```

#### Plot results

``` r
est <- 
  tidy_lm1 %>% 
  dplyr::filter(term == "income_demo_ranger_sar_scale") %>% 
  dplyr::select(estimate)

tidy_lm1 <-
  tidy_lm1 %>% 
  mutate(
    dot_color = ifelse(estimate < 0, "red1", ifelse(estimate > 0, "dodgerblue2", NA)),
    se = std.error
  )

tidy_lm1 <-
  tidy_lm1 %>% 
  filter(
    term == "income_demo_ranger_sar_scale" |
    term == "income_scale" |
    term == "Male_Head_Education_scale" | 
    term == "Female_Head_Education_scale" 
  ) %>% 
  mutate(
    variable = 
      case_when(
        term == "income_demo_ranger_sar_scale" ~ "demographic reference \n median income",
        term == "income_scale" ~ "household income",
        term == "Male_Head_Education_scale" ~ "male head education",
        term == "Female_Head_Education_scale" ~ "female head education"
      )
  ) 

col <- as.character(tidy_lm1$dot_color)
names(col) <- as.character(tidy_lm1$dot_color)

qh_dri_spend_monthly <-
  tidy_lm1 %>% 
  ggplot(aes(reorder(as.factor(variable), estimate), estimate)) +
  geom_point(aes(color = dot_color), size = 4) +
  geom_errorbar(aes(ymin = estimate - 2 * se, ymax = estimate + 2 * se), width = 0) + 
  scale_color_manual(values = col) +
  geom_hline(yintercept = -abs(est$estimate), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = abs(est$estimate), linetype = "dashed", color = "red") +
  scale_y_continuous(
    breaks = c( -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
    limits = c(-0.35, 0.35)
  ) +
  labs(
    y = "standardized beta",
    x = "variable"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  coord_flip()

qh_dri_calories_monthly
```

![](qfahpd_health_cross_sectional_regression_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggsave(
  "G:/My Drive/research/projects/niel/nielsen_relative_income/plots/qh_dri_spend_monthly.png",
  qh_dri_spend_monthly,
  width = 6,
  height = 4,
  dpi = 750
)
```

## Geographic reference income

### Outcome: % of monthly household calorie budget that goes to QFAHPD healthy categories

#### Fit model

``` r
lm1 <-
  lm(
    yes_scale ~
      median_income_county_scale +
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale + 
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_home_value_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Marital_Status +
      household_size_scale +
      month + 
      year,
      # (1|quarter) +
      # (1 + median_income_county_scale|fip_code) +
      # (1 + income_scale | fips_code),
    data = 
      qh_calories_imputed_sc_by_household_monthly %>% 
      filter(year %in% c(2004:2016))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ median_income_county_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_calories_imputed_sc_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1231 -0.7065 -0.1989  0.4869  5.6385 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                    -0.0547424  0.0027287  -20.062  < 2e-16 ***
    ## median_income_county_scale     -0.0135904  0.0005007  -27.144  < 2e-16 ***
    ## income_scale                    0.0567015  0.0004322  131.193  < 2e-16 ***
    ## Male_Head_Education_scale       0.0923448  0.0006970  132.490  < 2e-16 ***
    ## Female_Head_Education_scale     0.0413347  0.0005025   82.254  < 2e-16 ***
    ## Male_Head_Age_scale            -0.0093446  0.0007853  -11.899  < 2e-16 ***
    ## Female_Head_Age_scale           0.0334538  0.0005627   59.454  < 2e-16 ***
    ## Male_Head_Employment            0.0056837  0.0001564   36.337  < 2e-16 ***
    ## Female_Head_Employment          0.0046019  0.0001189   38.700  < 2e-16 ***
    ## median_home_value_county_scale  0.0301890  0.0005389   56.017  < 2e-16 ***
    ## land_area_2010_scale           -0.0135871  0.0003683  -36.893  < 2e-16 ***
    ## total_pop_county_scale         -0.0018834  0.0004114   -4.578  4.7e-06 ***
    ## Race                            0.0442246  0.0004930   89.699  < 2e-16 ***
    ## Marital_Status                  0.0245361  0.0004843   50.665  < 2e-16 ***
    ## household_size_scale           -0.0931185  0.0004199 -221.750  < 2e-16 ***
    ## month2                         -0.0553623  0.0017034  -32.501  < 2e-16 ***
    ## month3                         -0.0617051  0.0016962  -36.378  < 2e-16 ***
    ## month4                         -0.0762283  0.0016975  -44.907  < 2e-16 ***
    ## month5                         -0.1348267  0.0016976  -79.424  < 2e-16 ***
    ## month6                         -0.1450706  0.0017004  -85.314  < 2e-16 ***
    ## month7                         -0.1507741  0.0016996  -88.714  < 2e-16 ***
    ## month8                         -0.1481312  0.0016997  -87.150  < 2e-16 ***
    ## month9                         -0.1229634  0.0017014  -72.274  < 2e-16 ***
    ## month10                        -0.1305992  0.0017004  -76.807  < 2e-16 ***
    ## month11                         0.0301939  0.0017031   17.729  < 2e-16 ***
    ## month12                        -0.0959405  0.0017065  -56.222  < 2e-16 ***
    ## year2005                        0.0001772  0.0025253    0.070    0.944    
    ## year2006                        0.0005877  0.0025386    0.231    0.817    
    ## year2007                        0.0028027  0.0023568    1.189    0.234    
    ## year2008                        0.0034815  0.0023656    1.472    0.141    
    ## year2009                        0.0028028  0.0023707    1.182    0.237    
    ## year2010                        0.0016482  0.0023707    0.695    0.487    
    ## year2011                        0.0016603  0.0023625    0.703    0.482    
    ## year2012                        0.0027286  0.0023716    1.151    0.250    
    ## year2013                        0.0019439  0.0023689    0.821    0.412    
    ## year2014                        0.0019771  0.0023665    0.835    0.403    
    ## year2015                        0.0017644  0.0023685    0.745    0.456    
    ## year2016                        0.0020497  0.0023597    0.869    0.385    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9894 on 8169345 degrees of freedom
    ##   (183476 observations deleted due to missingness)
    ## Multiple R-squared:  0.02153,    Adjusted R-squared:  0.02153 
    ## F-statistic:  4859 on 37 and 8169345 DF,  p-value: < 2.2e-16

``` r
regclass::VIF(lm1)
```

    ##                                    GVIF Df GVIF^(1/(2*Df))
    ## median_income_county_scale     2.094364  1        1.447192
    ## income_scale                   1.549158  1        1.244652
    ## Male_Head_Education_scale      4.042790  1        2.010669
    ## Female_Head_Education_scale    2.089271  1        1.445431
    ## Male_Head_Age_scale            5.136875  1        2.266468
    ## Female_Head_Age_scale          2.619429  1        1.618465
    ## Male_Head_Employment           2.214076  1        1.487977
    ## Female_Head_Employment         1.450752  1        1.204472
    ## median_home_value_county_scale 2.418895  1        1.555280
    ## land_area_2010_scale           1.137290  1        1.066438
    ## total_pop_county_scale         1.411652  1        1.188130
    ## Race                           1.055947  1        1.027593
    ## Marital_Status                 2.549819  1        1.596815
    ## household_size_scale           1.463262  1        1.209654
    ## month                          1.000035 11        1.000002
    ## year                           1.006084 12        1.000253

``` r
lm.beta::lm.beta(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ median_income_county_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_calories_imputed_sc_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Standardized Coefficients::
    ##                    (Intercept)     median_income_county_scale 
    ##                   0.000000e+00                  -1.359514e-02 
    ##                   income_scale      Male_Head_Education_scale 
    ##                   5.651169e-02                   9.219401e-02 
    ##    Female_Head_Education_scale            Male_Head_Age_scale 
    ##                   4.114669e-02                  -9.333526e-03 
    ##          Female_Head_Age_scale           Male_Head_Employment 
    ##                   3.330125e-02                   1.871238e-02 
    ##         Female_Head_Employment median_home_value_county_scale 
    ##                   1.613197e-02                   3.015137e-02 
    ##           land_area_2010_scale         total_pop_county_scale 
    ##                  -1.361630e-02                  -1.882364e-03 
    ##                           Race                 Marital_Status 
    ##                   3.189975e-02                   2.799895e-02 
    ##           household_size_scale                         month2 
    ##                  -9.283336e-02                  -1.525749e-02 
    ##                         month3                         month4 
    ##                  -1.713742e-02                  -2.114250e-02 
    ##                         month5                         month6 
    ##                  -3.739158e-02                  -4.010794e-02 
    ##                         month7                         month8 
    ##                  -4.172442e-02                  -4.098535e-02 
    ##                         month9                        month10 
    ##                  -3.396204e-02                  -3.610991e-02 
    ##                        month11                        month12 
    ##                   8.324092e-03                  -2.635439e-02 
    ##                       year2005                       year2006 
    ##                   4.046822e-05                   1.322557e-04 
    ##                       year2007                       year2008 
    ##                   7.977038e-04                   9.774349e-04 
    ##                       year2009                       year2010 
    ##                   7.807097e-04                   4.590776e-04 
    ##                       year2011                       year2012 
    ##                   4.684833e-04                   7.599235e-04 
    ##                       year2013                       year2014 
    ##                   5.435191e-04                   5.548892e-04 
    ##                       year2015                       year2016 
    ##                   4.937288e-04                   5.814271e-04

``` r
tidy_lm1 <- tidy(lm1)
```

#### Plot results

``` r
est <- 
  tidy_lm1 %>% 
  dplyr::filter(term == "median_income_county_scale") %>% 
  dplyr::select(estimate)

tidy_lm1 <-
  tidy_lm1 %>% 
  mutate(
    dot_color = ifelse(estimate < 0, "red1", ifelse(estimate > 0, "dodgerblue2", NA)),
    se = std.error
  )

tidy_lm1 <-
  tidy_lm1 %>% 
  filter(
    term == "median_income_county_scale" |
    term == "income_scale" |
    term == "Male_Head_Education_scale" | 
    term == "Female_Head_Education_scale" 
  ) %>% 
  mutate(
    variable = 
      case_when(
        term == "median_income_county_scale" ~ "demographic reference \n median income",
        term == "income_scale" ~ "household income",
        term == "Male_Head_Education_scale" ~ "male head education",
        term == "Female_Head_Education_scale" ~ "female head education"
      )
  ) 

col <- as.character(tidy_lm1$dot_color)
names(col) <- as.character(tidy_lm1$dot_color)

qh_gri_calories_monthly <-
  tidy_lm1 %>% 
  ggplot(aes(reorder(as.factor(variable), estimate), estimate)) +
  geom_point(aes(color = dot_color), size = 4) +
  geom_errorbar(aes(ymin = estimate - 2 * se, ymax = estimate + 2 * se), width = 0) + 
  scale_color_manual(values = col) +
  geom_hline(yintercept = -abs(est$estimate), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = abs(est$estimate), linetype = "dashed", color = "red") +
  scale_y_continuous(
    breaks = c( -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
    limits = c(-0.35, 0.35)
  ) +
  labs(
    y = "standardized beta",
    x = "variable"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  coord_flip()

qh_gri_calories_monthly
```

![](qfahpd_health_cross_sectional_regression_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave(
  "G:/My Drive/research/projects/niel/nielsen_relative_income/plots/qh_gri_calories_monthly.png",
  qh_gri_calories_monthly,
  width = 6,
  height = 4,
  dpi = 750
)
```

### Outcome: % of monthly household food spend that goes to QFAHPD healthy categories

#### Fit model

``` r
lm1 <-
  lm(
    yes_scale ~
      median_income_county_scale +
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale + 
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_home_value_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Marital_Status +
      household_size_scale +
      month + 
      year,
      # (1|quarter) +
      # (1 + median_income_county_scale|fip_code) +
      # (1 + income_scale | fips_code),
    data = 
      qh_spend_by_household_monthly %>% 
      filter(year %in% c(2004:2016))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ median_income_county_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_spend_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9230 -0.6655 -0.0929  0.5543  7.6066 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                    -0.0997203  0.0022633  -44.059  < 2e-16 ***
    ## median_income_county_scale      0.0148672  0.0004862   30.580  < 2e-16 ***
    ## income_scale                    0.0665321  0.0004198  158.484  < 2e-16 ***
    ## Male_Head_Education_scale       0.1450528  0.0006766  214.397  < 2e-16 ***
    ## Female_Head_Education_scale     0.0759941  0.0004874  155.910  < 2e-16 ***
    ## Male_Head_Age_scale            -0.0263953  0.0007622  -34.631  < 2e-16 ***
    ## Female_Head_Age_scale           0.0526095  0.0005458   96.382  < 2e-16 ***
    ## Male_Head_Employment            0.0044328  0.0001523   29.115  < 2e-16 ***
    ## Female_Head_Employment          0.0035013  0.0001156   30.289  < 2e-16 ***
    ## median_home_value_county_scale  0.0615238  0.0005236  117.497  < 2e-16 ***
    ## land_area_2010_scale           -0.0045859  0.0003583  -12.797  < 2e-16 ***
    ## total_pop_county_scale          0.0016941  0.0004003    4.232 2.32e-05 ***
    ## Race                            0.0255374  0.0004766   53.586  < 2e-16 ***
    ## Marital_Status                  0.0322357  0.0004694   68.677  < 2e-16 ***
    ## household_size_scale           -0.1252089  0.0004074 -307.365  < 2e-16 ***
    ## month2                          0.0024905  0.0016551    1.505   0.1324    
    ## month3                          0.0124452  0.0016484    7.550 4.36e-14 ***
    ## month4                          0.0112595  0.0016492    6.827 8.65e-12 ***
    ## month5                         -0.0022035  0.0016492   -1.336   0.1815    
    ## month6                          0.0079480  0.0016516    4.812 1.49e-06 ***
    ## month7                         -0.0239679  0.0016509  -14.518  < 2e-16 ***
    ## month8                         -0.0737430  0.0016510  -44.666  < 2e-16 ***
    ## month9                         -0.0585374  0.0016523  -35.428  < 2e-16 ***
    ## month10                        -0.0776731  0.0016516  -47.030  < 2e-16 ***
    ## month11                        -0.0173768  0.0016546  -10.502  < 2e-16 ***
    ## month12                        -0.1056200  0.0016572  -63.734  < 2e-16 ***
    ## year2005                        0.0004473  0.0020445    0.219   0.8268    
    ## year2006                        0.0005950  0.0020604    0.289   0.7727    
    ## year2007                        0.0025734  0.0018380    1.400   0.1615    
    ## year2008                        0.0032336  0.0018491    1.749   0.0803 .  
    ## year2009                        0.0025860  0.0018555    1.394   0.1634    
    ## year2010                        0.0016182  0.0018555    0.872   0.3832    
    ## year2011                        0.0020064  0.0018453    1.087   0.2769    
    ## year2012                        0.0035483  0.0018554    1.912   0.0558 .  
    ## year2013                        0.0028725  0.0018519    1.551   0.1209    
    ## year2014                        0.0031042  0.0018486    1.679   0.0931 .  
    ## year2015                        0.0030232  0.0018501    1.634   0.1022    
    ## year2016                        0.0033530  0.0018389    1.823   0.0683 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9787 on 8478130 degrees of freedom
    ##   (149372 observations deleted due to missingness)
    ## Multiple R-squared:  0.04211,    Adjusted R-squared:  0.04211 
    ## F-statistic: 1.007e+04 on 37 and 8478130 DF,  p-value: < 2.2e-16

``` r
regclass::VIF(lm1)
```

    ##                                    GVIF Df GVIF^(1/(2*Df))
    ## median_income_county_scale     2.090254  1        1.445771
    ## income_scale                   1.552855  1        1.246136
    ## Male_Head_Education_scale      4.039577  1        2.009870
    ## Female_Head_Education_scale    2.085781  1        1.444223
    ## Male_Head_Age_scale            5.132107  1        2.265415
    ## Female_Head_Age_scale          2.618532  1        1.618188
    ## Male_Head_Employment           2.221540  1        1.490483
    ## Female_Head_Employment         1.453079  1        1.205437
    ## median_home_value_county_scale 2.415320  1        1.554130
    ## land_area_2010_scale           1.136936  1        1.066272
    ## total_pop_county_scale         1.410595  1        1.187685
    ## Race                           1.056937  1        1.028074
    ## Marital_Status                 2.550971  1        1.597176
    ## household_size_scale           1.464250  1        1.210062
    ## month                          1.000030 11        1.000001
    ## year                           1.004326 12        1.000180

``` r
lm.beta::lm.beta(lm1)
```

    ## 
    ## Call:
    ## lm(formula = yes_scale ~ median_income_county_scale + income_scale + 
    ##     Male_Head_Education_scale + Female_Head_Education_scale + 
    ##     Male_Head_Age_scale + Female_Head_Age_scale + Male_Head_Employment + 
    ##     Female_Head_Employment + median_home_value_county_scale + 
    ##     land_area_2010_scale + total_pop_county_scale + Race + Marital_Status + 
    ##     household_size_scale + month + year, data = qh_spend_by_household_monthly %>% 
    ##     filter(year %in% c(2004:2016)))
    ## 
    ## Standardized Coefficients::
    ##                    (Intercept)     median_income_county_scale 
    ##                   0.0000000000                   0.0148608667 
    ##                   income_scale      Male_Head_Education_scale 
    ##                   0.0663830525                   0.1448414423 
    ##    Female_Head_Education_scale            Male_Head_Age_scale 
    ##                   0.0756857661                  -0.0263707096 
    ##          Female_Head_Age_scale           Male_Head_Employment 
    ##                   0.0524244100                   0.0145862909 
    ##         Female_Head_Employment median_home_value_county_scale 
    ##                   0.0122727187                   0.0613791879 
    ##           land_area_2010_scale         total_pop_county_scale 
    ##                  -0.0045866610                   0.0016894097 
    ##                           Race                 Marital_Status 
    ##                   0.0185176598                   0.0368698112 
    ##           household_size_scale                         month2 
    ##                  -0.1250168195                   0.0006862831 
    ##                         month3                         month4 
    ##                   0.0034548872                   0.0031231074 
    ##                         month5                         month6 
    ##                  -0.0006111880                   0.0021987617 
    ##                         month7                         month8 
    ##                  -0.0066353761                  -0.0204137915 
    ##                         month9                        month10 
    ##                  -0.0161808579                  -0.0214874273 
    ##                        month11                        month12 
    ##                  -0.0047912086                  -0.0290377255 
    ##                       year2005                       year2006 
    ##                   0.0001007251                   0.0001320680 
    ##                       year2007                       year2008 
    ##                   0.0007232490                   0.0008963192 
    ##                       year2009                       year2010 
    ##                   0.0007110730                   0.0004449023 
    ##                       year2011                       year2012 
    ##                   0.0005588576                   0.0009764844 
    ##                       year2013                       year2014 
    ##                   0.0007938001                   0.0008614169 
    ##                       year2015                       year2016 
    ##                   0.0008373503                   0.0009418973

``` r
tidy_lm1 <- tidy(lm1)
```

#### Plot results

``` r
est <- 
  tidy_lm1 %>% 
  dplyr::filter(term == "median_income_county_scale") %>% 
  dplyr::select(estimate)

tidy_lm1 <-
  tidy_lm1 %>% 
  mutate(
    dot_color = ifelse(estimate < 0, "red1", ifelse(estimate > 0, "dodgerblue2", NA)),
    se = std.error
  )

tidy_lm1 <-
  tidy_lm1 %>% 
  filter(
    term == "median_income_county_scale" |
    term == "income_scale" |
    term == "Male_Head_Education_scale" | 
    term == "Female_Head_Education_scale" 
  ) %>% 
  mutate(
    variable = 
      case_when(
        term == "median_income_county_scale" ~ "demographic reference \n median income",
        term == "income_scale" ~ "household income",
        term == "Male_Head_Education_scale" ~ "male head education",
        term == "Female_Head_Education_scale" ~ "female head education"
      )
  ) 

col <- as.character(tidy_lm1$dot_color)
names(col) <- as.character(tidy_lm1$dot_color)

qh_gri_spend_monthly <-
  tidy_lm1 %>% 
  ggplot(aes(reorder(as.factor(variable), estimate), estimate)) +
  geom_point(aes(color = dot_color), size = 4) +
  geom_errorbar(aes(ymin = estimate - 2 * se, ymax = estimate + 2 * se), width = 0) + 
  scale_color_manual(values = col) +
  geom_hline(yintercept = -abs(est$estimate), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = abs(est$estimate), linetype = "dashed", color = "red") +
  scale_y_continuous(
    breaks = c( -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
    limits = c(-0.35, 0.35)
  ) +
  labs(
    y = "standardized beta",
    x = "variable"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  coord_flip()

qh_gri_calories_monthly
```

![](qfahpd_health_cross_sectional_regression_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave(
  "G:/My Drive/research/projects/niel/nielsen_relative_income/plots/qh_gri_spend_monthly.png",
  qh_gri_spend_monthly,
  width = 6,
  height = 4,
  dpi = 750
)
```
