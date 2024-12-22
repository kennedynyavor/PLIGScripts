#*******************************************************************************
#* Date Created:   12/22/2024
#* Purpose:        Generate GWP per policy
#* Date Updated:   12/22/2024
#* ----------------------------------------------------------------------------#
#*******************************************************************************

# Load libraries
require(tidyverse)
require(readxl)
require(janitor)

# Define Variables 
prod_month <- ymd("2024-10-01")
start_date_of_aggregation <- ymd('2023-01-01')


# Import dimension files 
# Update the product table to add par and non-par classification 
#*****Date table****************************************************************
alloc_df <- 
  read_excel(
    path = "~/z_data/data_prep/dim_files/datetable.xlsx",
    sheet = "dateTable",
    .name_repair = janitor::make_clean_names,
    trim_ws = TRUE
  ) 

#*****Product Table*************************************************************
product_df <-
  read_excel(
    path = "~/z_data/data_prep/dim_files/product_details.xlsx",
    sheet = "Sheet1",
    .name_repair = janitor::make_clean_names,
    trim_ws = TRUE
  ) %>% 
  distinct(product_code,.keep_all = TRUE) %>% 
  select(-c(sdr_group,never_lapsed))
  
