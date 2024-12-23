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

# Current Production Month 
prod_month <- ymd("2024-10-01")

# Start date of GWP aggregation
# Currently, the actual GWP file starts from 2023 hence, this date should start from 2023
# IF you want to use actual from 2022, then set this to 2022-01-01, and also update the excel file
# with actual starting from 2022. 
start_date_of_aggregation <- ymd('2023-01-01') 


#*****Load Scripts {Auto update}************************************************
#* Load predefined r functions
source("./r_files/prep_functions2.R")


#*****Date table {Auto update}**************************************************
# Import dimension files 
# Update the data table with latest allocation cut off dates for both agency and SCB
# Agency and all Banca partners use the same allocation cut off dates except SCB
# Request cut off dates from 
alloc_df <- 
  read_excel(
    path = "./dim_files/datetable.xlsx",
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
    path = "./dim_files/product_details.xlsx",
    sheet = "Sheet1",
    .name_repair = janitor::make_clean_names,
    trim_ws = TRUE
  ) %>% 
  distinct(product_code,.keep_all = TRUE) %>% 
  select(-c(sdr_group,never_lapsed))


#*****Branches Table {Auto Update}**********************************************
#* The branch table contains info about each branch and the respective channel
#* Check for new branch branches each month and update any new branches that have been added
#* Establish communication channels with Agency Support so you get notified of any new branch creations
#* Email agency support contact to request new branch additions
branch_info <- 
  read_excel(
    path = "./dim_files/NEW_GwP_Branches.xlsx",
    sheet = 'branches',
    trim_ws = TRUE,
    .name_repair = janitor::make_clean_names
  ) %>% 
  distinct(branchkey,.keep_all = TRUE)



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


#*****All Payment {Auto Update}*************************************************
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
  ungroup() 


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
    agent_branch =
      ifelse(str_detect(agent_branch,"NABCO"),agent_team,agent_branch),
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
  select(-c(join_key)) %>% 
  # Merge branch info with all payment data
  left_join(
    branch_info,by = c("agent_branch"="branchkey"),keep = FALSE
  ) %>% 
  mutate(
    first_ann = incepted_date + months(12),
    renewal_status = if_else(
      rollback(payment_date,roll_to_first=TRUE) < first_ann,
      "FY GwP","Renewal GwP"
    )
  )


#***** Final Detailed Data {Auto Update}**************************************
#* The is the final data and it has policy level data
#* Use this only if you can't find a particular column in the summary data
pmt_final_detailed <-
  pmt_df1 %>% 
  # Rename columns
  rename(prodmonth = alloc_date, pay_freq = endr_freq_of_payment) %>% 
  # Remove columns not needed
  select(-c(first_ann,pct_cont, GWP,amount,receipt_no,agent_team,product_code)) %>% 
  # Re-define the channels columns
  mutate(
    channel_lvl0 = product_channel,
    channel_lvl1 = sub_channel,
    channel_lvl2 = ifelse(str_detect(product_channel,'Banca'),sub_channel,
                          ifelse(str_detect(product_channel,'Banca',negate=TRUE) & 
                                   str_detect(agent_branch,"BANCASSUR"),"Agency FC",channel_lvl2) ),
    payment_date = rollback(payment_date,roll_to_first=TRUE)
  ) %>% 
  # Remove duplicate columns
  select(
    -c(product_channel,sub_channel)
  ) %>% 
  # Re-arrange columns
  select(
    policy_number,prodmonth,incepted_date,payment_date,transaction_date,status_reason,
    fundprovider:agent_branch, product_type:product_class,branch_2:renewal_status,GwP_Income
  )


# Snapshot of the data
glimpse(pmt_final_detailed)


#***** Final Data Summary {Auto Update}***************************************
#* The is the final data and it has policy level data
#* Use this only if you can't find a particular column in the summary data
# Save detailed data for the month
pmt_final_summary <-
  pmt_final_detailed %>% 
  select(-c(policy_number,payment_date:agent_branch)) %>% 
  group_by(prodmonth,product_type,product_name,product_class,branch_2,channel_lvl0,
           channel_lvl1,channel_lvl2,renewal_status) %>% 
  summarise(GwP_Income = sum(GwP_Income ))


files_in_folder <- dir("./GwP_by_Product/results/")
mmyyyy <-  paste0(toupper(month(prod_month,label = T)),year(prod_month))


#*****Saving the Data {Auto Update}*******************************************
# This assumes the the first time the report would be run is OCT2024
# The aggregate data from Jan2023 to Oct2024 would be saved once in the result folder
# Subsequently, only the current month's data will be saved in the folder

# Checks if the production month is Oct 2024
if (mmyyyy == 'OCT2024' ) { 
  
  # Save detail policy level data for JAN2023 - OCT2024 only if the production month is OCT2024
  pmt_final_detailed %>% 
    write_delim(
      paste0("./GwP_by_Product/results/gwp_detl_",mmyyyy,".txt"),
      delim = ";",
      na = ""
    )
  
  # Save summary data for Jan2023-Oct2024 only if the production month is OCT2024
  pmt_final_summary %>% 
    write_csv(
      paste0("./GwP_by_Product/results/gwp_sum_",mmyyyy,".csv"),
      na = ""
    )
  
  # If the production month is not Oct 2024, data for the current production would be saved
} else {
  
  # Save the current month's policy level GWP data
  pmt_final_detailed %>% 
    filter(prodmonth == prod_month) %>% 
    write_delim(
      paste0("./GwP_by_Product/results/gwp_detl_",mmyyyy,".txt"),
      delim = ";",
      na = ""
    )
  
  # Save the current month summary data
  pmt_final_summary %>% 
    filter(prodmonth == prod_month) %>% 
    write_csv(
      paste0("./GwP_by_Product/results/gwp_sum_",mmyyyy,".csv"),
      na = ""
    )
}


#*****Saving the Data {Auto Update}*******************************************
# Check for any branch info mismatch
# if this returns "integer(0)" then there is no new branch
# if this returns "integer(n)" then there are new branches
# The new branches are then saved in the data_pred folder
new_br_file <- "./GwP_by_Product/new_branches.csv"
if (dir.exists(new_br_file)){
  unlink(new_br_file)
}


#*****Save Aggregate data {Auto Update}*****************************************
#* Aggregates the monthly data in results folder and save it in the aggregate_files folder
#* Both the policy level data and summary data sets are saved in the same folder
#* This is the data you should use for your reporting 
#* IF you want to use power query, then use the aggregate data
#* IF you want to put the data in an excel template, use the summary data

res_files <- dir("./GwP_by_Product/results/")

# Save the Policy level data  
map_df(
  paste0("./GwP_by_Product/results/", res_files[str_detect(res_files, "_detl_")]),
  ~ read_delim(., delim = ";")
) %>% 
write_delim(
  "./GwP_by_Product/aggregate_files/GWP_Detailed.txt",
  delim = ";",
  na = ""
)

# Save the Summary level data  
aggr_summ <-
map_df(
  paste0("./GwP_by_Product/results/", res_files[str_detect(res_files, "_sum_")]),
  ~ read_csv(.)
) 

aggr_summ %>% 
  write_csv(
    "./GwP_by_Product/aggregate_files/GWP_Summary.csv",
    na = ""
  )


# Check if there are new branches
if (length(which(is.na(pmt_df1$channel_lvl0))) > 0){
  pmt_df1$agent_branch[which(is.na(pmt_df1$channel_lvl0))] %>% 
    as.data.frame() %>% 
    write_csv(new_br_file)
} else {
  print("No New Branches")
}




#*****Updating Excel Summary File {Auto Update}*********************************
#*

# END