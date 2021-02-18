
###################
## Load Packages ##
###################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr
)


##############
## Set Path ##
##############

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

####################
## Load Functions ##
####################

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/calculate_spend_per_module_fn.R"))


#########################
## Load Auxiliary Data ##
#########################

products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")


##########
## 2017 ##
##########

year = "2017"

spend_per_cat_17 <- calculate_spend_per_module_fn(year, products_master)

spend_per_cat_candy <-
  niel_df %>%
  group_by(
    household_code,
    candy
  ) %>%
  summarise(
    spend_per_cat = sum(total_price_paid)
  ) %>% 
  mutate(
    candy = spend_per_cat
  ) %>% 
  dplyr::select(-spend_per_cat)

spend_per_cat_veg <-
  niel_df %>%
  group_by(
    household_code,
    vegetable
  ) %>%
  summarise(
    spend_per_cat = sum(total_price_paid)
  ) %>% 
  mutate(
    vegetable = spend_per_cat
  ) %>% 
  dplyr::select(-spend_per_cat)

spend_per_cat <-
  spend_per_cat_candy %>% 
  left_join(
    spend_per_cat_veg
  )


spend_per_cat <-
  spend_per_cat %>% 
  left_join(
    spend,
    by = "household_code"
  )

spend_per_cat <-
  spend_per_cat %>% 
  mutate(
    veg_candy = vegetable / candy
    #spend_per_cat_per = spend_per_cat / total_spend
  )

spend_per_cat <-
  spend_per_cat %>% 
  left_join(
    panelists %>% mutate(household_code = Household_Cd) %>% 
      dplyr::select(
        household_code,
        Household_Income,
        Household_Size,
        Male_Head_Education,
        Female_Head_Education,
        Panelist_ZipCd,
        Fips_State_Cd,
        Fips_County_Cd,
        Panelist_ZipCd
      )
  )
