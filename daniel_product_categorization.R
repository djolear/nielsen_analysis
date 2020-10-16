

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

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/calculate_spend_per_dc_fn.R"))


#########################
## Load Auxiliary Data ##
#########################

products_master <-
  readr::read_tsv("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")

products_master <- 
  products_master %>% 
  mutate(
    candy = str_detect(product_module_descr, "CANDY"),
    vegetable = str_detect(product_module_descr, "VEGETABLE"),
    cookie = str_detect(product_module_descr, "COOKIE"),
    cake = str_detect(product_module_descr, "CAKE"),
    fruit = str_detect(product_module_descr, "FRUIT"),
    chips = str_detect(product_module_descr, "CHIPS"),
    nuts = str_detect(product_module_descr, "NUTS"),
    ice_cream = str_detect(product_module_descr, "ICE CREAM"),
    soft_drinks_reg = str_detect(product_module_descr, "SOFT DRINKS - CARBONATED")
  )

products_master <-
  products_master %>% 
  mutate(
    dc = 
      case_when(
        candy == "TRUE" ~ "candy",
        vegetable == "TRUE" ~ "vegetable",
        cookie == "TRUE" ~ "cookie",
        cake == "TRUE" ~ "cake",
        fruit == "TRUE" ~ "fruit",
        chips == "TRUE" ~ "chips",
        nuts == "TRUE" ~ "nuts",
        ice_cream == "TRUE" ~ "ice_cream",
        soft_drinks_reg == "TRUE" ~ "soft_drinks_reg"
      )
  )

products_master <- 
  products_master %>% 
  mutate(
    healthy = 
      case_when(
        dc == "candy" ~ "unhealthy",
        dc == "vegetable" ~ "healthy",
        dc == "cookie" ~ "unhealthy",
        dc == "cake" ~ "unhealthy",
        dc == "fruit" ~ "healthy",
        dc == "chips" ~ "unhealthy",
        dc == "nuts" ~ "healthy",
        dc == "ice_cream" ~ "unhealthy",
        dc == "soft_drinks_reg" ~ "unhealthy"
        
      )
  )


##

year = "2017"

spend_per_dc_17 <- calculate_spend_per_module_fn(year, products_master)

