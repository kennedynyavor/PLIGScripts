#*******************************************************************************
#* Date Created:   12/22/2024
#* Purpose:        Generate GWP per policy
#* Date Updated:   12/22/2024
#* NOTE: ALL CODE CHUNKS DENOTED {Manual Update}, SHOULD BE UPDATED MANUALLY
#* BEFORE YOU RUN THIS FILE, SET THE WORKING DIRECTORY TO THE "~/z_data/data_prep"
#*******************************************************************************


# Load libraries
require(tidyverse)
require(readxl)
require(janitor)


#*****Variables {Manual update}*************************************************
# Define Variables 
prod_month <- ymd("2024-10-01")
start_date_of_aggregation <- ymd('2023-01-01')
data_start_year <- 2018 # Filter all payment data 


#*****Load Scripts {Auto update}************************************************
#*
source("~/z_data/data_prep/r_files1/r_files/prep_functions2.R")


#*****Date table {Auto update}**************************************************
# Import dimension files 
# Update the data table with latest allocation cut off dates for both agency and SCB
# Agency and all Banca partners use the same allocation cut off dates except SCB
# Request cut off dates from 
alloc_df <- 
  read_excel(
    path = "~/z_data/data_prep/dim_files/datetable.xlsx",
    sheet = "dateTable",
    .name_repair = janitor::make_clean_names,
    trim_ws = TRUE
  ) %>% 
  mutate(
    date_key = ymd(date_key),
    agency   = ymd(agency),
    scb      = ymd(scb)
  ) %>% 
  filter(
    year(date_key) <= year(prod_month)
  )


#*****Product Table {Auto Update}***********************************************
#* Ensure the product table is updated with the details of any new product launched
#* Update the product table to add par and non-par classification 
#* Watch out for re-priced products, they come with new product codes
product_df <-
  read_excel(
    path = "~/z_data/data_prep/dim_files/product_details.xlsx",
    sheet = "Sheet1",
    .name_repair = janitor::make_clean_names,
    trim_ws = TRUE
  ) %>% 
  distinct(product_code,.keep_all = TRUE) %>% 
  select(-c(sdr_group,never_lapsed))
  

#*****GWP by Month {Auto updated}***********************************************
#* Create an excel file of GWP by Channel for individual line of business 
#* Agency, FD Banca, SCB Banca, Cal Banca, ZN Banca, SocGen Banca
#* Save this file as | Actual_GWP_By_Channel.xlsx | in the dim_files folder
#* Name the sheet as "Actual_GWP"
#* Update this file monthly before you run this 
actual_GwP <-
  read_excel(
    path = "~/z_data/data_prep/dim_files/Actual_GWP_By_Channel.xlsx",
    trim_ws = TRUE
  ) %>% 
  rename_with(tolower,starts_with("Month")) %>% 
  pivot_longer(cols = -c('month'),names_to = "prod_month",values_to = "GWP" ) %>% 
  rename(channel = month) %>% 
  mutate(
    prod_month = janitor::excel_numeric_to_date(as.numeric(prod_month))
  ) %>% 
  mutate(
    joinkey = paste0(channel,year(prod_month)*100 + month(prod_month))
  ) 
  

#*****All Payment {Auto Manual}*************************************************
#*Premium payment data
pmt_df <-
  # Import the data using read_delim
  read_delim(
    file = get_file_name(prod_month,"PAYMENT"),
    delim = ";",
    trim_ws = TRUE,
    name_repair = janitor::make_clean_names
  ) %>% 
  # Convert all columns ending in "date" into the correct date format
  mutate_at(vars(matches("date$")),mdy) %>% 
  # Remove client name from the data - not needed
  select(-c(prp_other_names,prp_surname)) %>% 
  # Filter the data to include data after 
  # Merge the all payment data with the allocation date table
  left_join(alloc_df,by = c("transaction_date" = "date_key"),keep = FALSE) %>% 
  # Add two columns, allocation date and product code
  mutate(
    alloc_date = if_else(str_detect(agent_branch,"SC BANCAS"),scb,agency),
    product_code = str_sub(policy_number,1,4)
  ) %>% 
  # Remove the allocation dates for agency and scb
  select(-c(scb,agency)) %>% 
  # Merge the data with product table
  # Any new columns added to the product table will automatically be added to the data
  left_join(product_df,by = c('product_code'='product_code'),keep = FALSE) %>% 
  # Find inception date, group by policy number and find the minimum payment date
  group_by(policy_number) %>% 
  mutate(incepted_date = min(payment_date)) %>% 
  ungroup() %>% 
  filter(
    year(transaction_date) >= data_start_year
  ) 
  

  #-----------------------------------------------------------------------------#
  # Further cleaning of the payment data, new variable added
  pmt_df1 <-
    pmt_df %>% 
    # Remove legacy products
    filter(
      str_detect(product_code,"EMFP|EMSP|EFPP|EPPP",negate = TRUE),
      alloc_date <= prod_month,
      alloc_date >= start_date_of_aggregation
    ) %>% 
    # Replace NABCO branches with the correct branch name
    mutate(
      agent_branch = ifelse(str_detect(agent_branch,"NABCO"),agent_team,agent_branch),
      incepted_date = rollback(incepted_date,roll_to_first=TRUE)
    ) %>% 
    # Creates join key
    mutate(
      join_key = paste0(sub_channel,year(alloc_date)*100+month(alloc_date))
    ) %>% 
    # Merge with actual GWP data
    left_join(
      actual_GwP %>% select(-c(channel,prod_month)),
      by = c('join_key'= 'joinkey'),
      keep = FALSE
    ) %>% 
    # Split actual GWP by premium paid by each policy
    group_by(join_key) %>% 
    mutate(pct_cont = amount/sum(amount),
           GwP_Income = (amount/sum(amount))*GWP
           ) %>% 
    ungroup() %>% 
    select(-c(join_key))
    
  
#*****Final File {Auto Manual}*************************************************
#*Premium payment data
