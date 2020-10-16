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


###############
## Load Data ##
###############


li_upc <-
  read_csv(paste0(machine_path, "research/projects/niel/label_insight_upc.csv"))

##############################
## Create 13 Character UPC's ##
##############################

# This only works for 12-character UPC's

li_upc <-
  li_upc %>% 
  mutate(
    char = nchar(UPC)
  )

li_upc <-
  li_upc %>% 
  mutate(
    upc_new = ifelse(char == 12, paste0("0", UPC), UPC)
  )
