# spend_per_cat2 <-
#   spend_per_cat %>% 
total_sugar2 <-
  total_sugar %>%   
  mutate(zip = Panelist_ZipCd) %>% 
  left_join(
    census_ses_zip_2015_wide %>% 
      dplyr::select(
        zip,
        gini,
        pov_status_below_per:ea_bach_or_higher_per,
        median_income,
        median_monthly_housing_cost
      ),
    by = "zip"
  ) %>% 
  left_join(
    zip_mob
  )

spend_per_cat2 <-
  spend_per_cat2 %>% 
  left_join(
    wb_county
  )

wb_county <- dfg %>% group_by(fips_code) %>% summarise(ladder_now = mean(ladder_now, na.rm = TRUE), ladder_5yrs = mean(ladder_5yrs, na.rm = TRUE), cl_diff = mean(cl_diff, na.rm = TRUE))

lm1 <-
  lmer(
    spend_per_cat_per ~ 
      #I(Household_Income/sqrt(Household_Size)) * log(median_income + 1) * gini, 
      Household_Income + median_income + median_monthly_housing_cost + cl_diff +
      (1|zip), 
    data = spend_per_cat2 %>%  
      filter(product_module_descr == "FRESH VEGETABLES-REMAINING")
    )

summary(lm1)

effect("median_income * mob25_ga", lm1) %>% 
  as_tibble() %>% 
  ggplot(aes(median_income, fit, group = mob25_ga, color = mob25_ga)) +
  geom_line()


effect("median_income * gini", lm1) %>% 
  as_tibble() %>% 
  ggplot(aes(median_income, fit, group = gini, color = gini)) +
  geom_line()

wbf <-
  spend_per_cat2 %>%  
  filter(product_module_descr == "FRESH VEGETABLES-REMAINING") %>% 
  group_by(fips_code) %>% 
  summarise(mean = mean(spend_per_cat_per)) %>% 
  left_join(
    wb_county
  ) %>% 
  left_join(
    census_ses_zip_2018_wide  %>% 
      mutate(fips_code = zip)
  )

summary(lm(mean ~ ladder_5yrs * ladder_now, wbf))

lm1 <-
  lm(
    mean ~
      ladder_5yrs * ladder_now,
    wbf
  )

plot_model(lm1, type = "int")

effect("ladder_5yrs * ladder_now", lm1) %>% 
  as_tibble() %>% 
  ggplot(aes(ladder_5yrs, fit, group = ladder_now, color = ladder_now, fill = ladder_now)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), color = NA, alpha = 0.2) +
  labs(
    y = "percentage of spend to fresh vegetables (other)"
  ) +
  theme_bw()





###
total_sugar2 <- 
  total_sugar2 %>% 
  mutate(inc_adj = scale(I(log(inc_mid  / sqrt(Household_Size) + 1)))) %>% 
  mutate_at(
    vars(
      Male_Head_Education,
      Female_Head_Education,
      Race,
      Marital_Status,
      Male_Head_Employment,
      Female_Head_Employment
    ),
    as.numeric
  )

lm1 <- 
  lmer(
    scale(sug_per) ~ 
      inc_mid + 
      I(inc_mid - median_income) + 
      Household_Size +
      median_monthly_housing_cost + 
      as.factor(Male_Head_Education) +
      as.factor(Female_Head_Education) +
      as.factor(Race) +
      as.factor(Marital_Status) +
      as.factor(Male_Head_Employment) +
      as.factor(Female_Head_Employment) +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = total_sugar2 
  )

summary(lm1)

lm1 <- 
  lmer(
    scale(sug_per) ~ 
      inc_mid + 
      median_income + 
      Household_Size +
      median_monthly_housing_cost + 
      as.factor(Male_Head_Education) +
      as.factor(Female_Head_Education) +
      as.factor(Race) +
      as.factor(Marital_Status) +
      as.factor(Male_Head_Employment) +
      as.factor(Female_Head_Employment) +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = total_sugar2 
  )

summary(lm1)

lm1 <- 
  lmer(
    scale(sug_per) ~ 
      inc_mid * gini +
      Male_Head_Age +
      Female_Head_Age +
      (1|zip), 
    data = total_sugar2 
  )

summary(lm1)

lm1 <- 
  lm(
    scale(log(total_sugar / total_spend)) ~ 
      inc_adj + 
      scale(log(median_income + 1)) + 
      scale(log(median_monthly_housing_cost + 1)), 
      # as.factor(Male_Head_Education) + 
      # as.factor(Female_Head_Education) + 
      # as.factor(Race) + 
      # as.factor(Marital_Status) + 
      # as.factor(Male_Head_Employment) + 
      # as.factor(Female_Head_Employment) + 
      # Male_Head_Age + 
      # Female_Head_Age +
      #(1|zip), 
    data = total_sugar2 
  )

summary(lm1)


effect("median_income", lm1) %>% as_tibble() %>% ggplot(aes(median_income, fit)) + geom_line() + geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2, color = NA)

effect(" inc_adj * scale(log(median_income + 1))", lm1) %>% as_tibble() %>% ggplot(aes(inc_adj, fit, group = median_income, color = median_income)) + geom_line() + geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2, color = NA)
