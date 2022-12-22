#_____________________________________________________________________________
### Dataload and data processing ----
#_____________________________________________________________________________

# Clear out existing lists if taking memory
lfs_dataset_list <- list()
lfs_dataset_list_adj <- list()

# Load the LFS datatets
dataset_nm_1014 <- c("lfsp_aj10_end_user","lfsp_aj11_end_user","lfsp_aj12_end_user","lfsp_aj13_end_user","lfsp_aj14_end_user")
dataset_ext_names <- c("lfsp_aj15_eul","lfsp_aj16_eul","lfsp_aj17_eul","lfsp_aj18_eul",
                       "lfsp_aj19_eul_pwt18","lfsp_aj20_eul_pwt22","lfsp_aj21_eul_pwt22","lfsp_aj22_eul_pwt22")
dataset_years <- c(2015:2022)

#.............................................................................
#### Load LFS stats, automatically clean and save ----
#.............................................................................

lfs_dataset_nm <- c()

for (y in 1:length(dataset_ext_names)) {
  
  # Produce a list with dataframe, name and year
  temp_list <- import_save_dta(dta_num = y,
                  loadRDS = TRUE,
                  old_dat17 = FALSE,
                  years_vector = dataset_years)
  
  # Create dataframe with name and save its name
  assign(temp_list[["name"]],temp_list[["dta"]])
  lfs_dataset_nm <- c(lfs_dataset_nm,temp_list[["name"]])

  # Delete list
  rm(temp_list)
  
}

# Give years as names to the vector
names(lfs_dataset_nm) <- as.character(dataset_years)

#.............................................................................
#### Adjustments to data ----
#.............................................................................

# Create list with datasets, including their name

lfs_dataset_list <- setNames(lapply(lfs_dataset_nm, get),lfs_dataset_nm)

lfs_dataset_list_adj <- lapply(lfs_dataset_list,recode_dta) 


#.............................................................................
#### Summary statistics across characteristics ----
#.............................................................................

# Using standard binary NTE variable

# Full empty dataset
lfsp_aj_full <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                           nte_worker=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                           industry=numeric(0),test_var=numeric(0))



for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])

  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("ILODEFR","london_worker"),
                            nte_var="nte_worker", 
                            agg_vars=c("industry_job","test_var")) 
  
  
  #assign(paste0(dta_nm,"_wt"),temp_list[["lfsp_wt"]])
  assign(paste0(dta_nm,"_sum"),temp_list[["lfsp_sum"]])
  
  # Add to full table
  lfsp_aj_full <- lfsp_aj_full %>% 
    bind_rows(temp_list[["lfsp_sum"]])
  
  rm(temp_list)
  
  # Now extract using industries
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("ILODEFR","london_worker","industry_job"),
                            nte_var="nte_worker",
                            agg_vars=c("test_var"))
  
  
  #assign(paste0(dta_nm,"_wt_ind"),temp_list[["lfsp_wt"]])
  assign(paste0(dta_nm,"_sum_ind"),temp_list[["lfsp_sum"]])
  
  # Add to full table
  lfsp_aj_full <- lfsp_aj_full %>% 
    bind_rows(temp_list[["lfsp_sum"]])
}


# Using detailed NTE breakdown variable

# Full empty dataset
lfsp_aj_full_detail <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                           nte_worker=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                           industry=numeric(0),test_var=numeric(0))


for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("ILODEFR","london_worker"),
                            nte_var="nte_worker_detail", 
                            agg_vars=c("industry_job","test_var")) 
  
  
  #assign(paste0(dta_nm,"_wt"),temp_list[["lfsp_wt"]])
  #assign(paste0(dta_nm,"_sum"),temp_list[["lfsp_sum"]])
  
  # Add to full table
  lfsp_aj_full_detail <- lfsp_aj_full_detail %>% 
    bind_rows(temp_list[["lfsp_sum"]])
  
  rm(temp_list)
  
  # Now extract using industries
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("ILODEFR","london_worker","industry_job"),
                            nte_var="nte_worker_detail",
                            agg_vars=c("test_var"))
  
  
  #assign(paste0(dta_nm,"_wt_ind"),temp_list[["lfsp_wt"]])
  assign(paste0(dta_nm,"_sum_ind"),temp_list[["lfsp_sum"]])
  
  # Add to full table
  lfsp_aj_full_detail <- lfsp_aj_full_detail %>% 
    bind_rows(temp_list[["lfsp_sum"]])
}

#.............................................................................
#### Export tables to Excel ----
#.............................................................................


# Headline figures
lfsp_aj_head <- lfsp_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & industry_job==9999) %>% 
  mutate(id=paste(dta_year,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

lfsp_aj_head_detail <- lfsp_aj_full_detail %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & industry_job==9999) %>% 
  mutate(id=paste(dta_year,london_worker,nte_worker_detail,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_worker_detail,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

# Industry figures
lfsp_aj_ind <- lfsp_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & industry_job!=9999) %>% 
  mutate(id=paste(dta_year,industry_job,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,industry_job,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

# -- This one for newer method
wb <- loadWorkbook(paste0(DATA_OUT,"/NTE data.xlsx"))
writeData(wb, sheet = "nte_headline",lfsp_aj_head, colNames = T)
writeData(wb, sheet = "nte_ind",lfsp_aj_ind, colNames = T)
writeData(wb, sheet = "nte_data",lfsp_aj_full, colNames = T)
writeData(wb, sheet = "nte_detail_headline",lfsp_aj_head_detail, colNames = T)
saveWorkbook(wb,paste0(DATA_OUT,"/NTE data.xlsx"),overwrite = T)



# -- This one when checking against old analysis, i.e. using PWT 2017 data
# wb <- loadWorkbook(paste0(DATA_OUT,"/NTE data - consistent.xlsx"))
# writeData(wb, sheet = "nte_workers",lfsp_aj_ldn, colNames = T)
# writeData(wb, sheet = "nte_data",lfsp_aj_full, colNames = T)
# saveWorkbook(wb,paste0(DATA_OUT,"/NTE data - consistent.xlsx"),overwrite = T)
