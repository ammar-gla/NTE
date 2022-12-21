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
                  old_dat17 = TRUE,
                  years_vector = dataset_years)
  
  # Create dataframe with name and save its name
  assign(temp_list[["name"]],temp_list[["dta"]])
  lfs_dataset_nm <- c(lfs_dataset_nm,temp_list[["name"]])

  # Delete list
  rm(temp_list)
  
}

# Give years as names
names(lfs_dataset_nm) <- as.character(dataset_years)

#.............................................................................
#### Adjustments to data ----
#.............................................................................

# Create list with datasets, including their name

lfs_dataset_list <- setNames(lapply(lfs_dataset_nm, get),lfs_dataset_nm)

lfs_dataset_list_adj <- lapply(lfs_dataset_list,recode_dta) 

# Full dataset
lfsp_aj_full <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                           nte_worker=character(0),wt_pop=numeric(0),unwt_pop=numeric(0))


for (dta_nm in lfs_dataset_nm) {
  
  london_wt_dta <- new_weight(lfs_dataset_list_adj[[dta_nm]],
                              cons_method=FALSE)
  
  uk_wt_dta <- new_weight(lfs_dataset_list_adj[[dta_nm]],
                          uk_tot = TRUE,
                          cons_method=FALSE)
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # Merge on the weights
  lfsp_wt <- lfs_dataset_list_adj[[dta_nm]] %>%
    left_join(london_wt_dta,by=c("london_worker","ILODEFR")) %>%
    left_join(uk_wt_dta,by=c("ILODEFR")) %>%
    mutate(weight_val_ldn = weight_val * uprate_weight_ldn,
           weight_val_uk = weight_val * uprate_weight_uk,
           dta_year = dta_year)
  
  # ALT - consistent with old method, merge all NA into non-London
  # lfsp_wt <- lfs_dataset_list_adj[[dta_nm]] %>% 
  #   mutate(london_worker=case_when(london_worker=="London" ~ london_worker,
  #                                  TRUE ~ "Not London")) %>% 
  #   left_join(london_wt_dta,by=c("london_worker","ILODEFR")) %>% 
  #   left_join(uk_wt_dta,by=c("ILODEFR")) %>% 
  #   mutate(weight_val_ldn = weight_val * uprate_weight_ldn,
  #          weight_val_uk = weight_val * uprate_weight_uk,
  #          dta_year = dta_year)
  
  # Only interested in quarter_response=="Yes" & ILODEFR==1, but keep all for data checking
  lfsp_sum <- lfsp_wt %>% 
    group_by(quarter_response,ILODEFR,london_worker,nte_worker,dta_year,uprate_weight_ldn,weight_var) %>% 
    summarise(wt_pop=sum(weight_val_ldn),
              unwt_pop=n()) %>% 
    group_by(quarter_response,ILODEFR,london_worker,dta_year,uprate_weight_ldn,weight_var) %>% 
    mutate(share_wt_pop = wt_pop/sum(wt_pop),
           share_unwt_pop = unwt_pop/sum(unwt_pop)) %>% 
    ungroup()
  
  assign(paste0(dta_nm,"_wt"),lfsp_wt)
  assign(paste0(dta_nm,"_sum"),lfsp_sum)
  
  # Add to full table
  lfsp_aj_full <- lfsp_aj_full %>% 
    bind_rows(lfsp_sum)
  
  rm(lfsp_sum,lfsp_wt)
}


# Export tables to Excel
lfsp_aj_ldn <- lfsp_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1) %>% 
  mutate(id=paste(dta_year,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

# -- This one for newer method
wb <- loadWorkbook(paste0(DATA_OUT,"/NTE data.xlsx"))
writeData(wb, sheet = "nte_workers",lfsp_aj_ldn, colNames = T)
writeData(wb, sheet = "nte_data",lfsp_aj_full, colNames = T)
saveWorkbook(wb,paste0(DATA_OUT,"/NTE data.xlsx"),overwrite = T)

# -- This one when checking against old analysis, i.e. using PWT 2017 data
# wb <- loadWorkbook(paste0(DATA_OUT,"/NTE data - consistent.xlsx"))
# writeData(wb, sheet = "nte_workers",lfsp_aj_ldn, colNames = T)
# writeData(wb, sheet = "nte_data",lfsp_aj_full, colNames = T)
# saveWorkbook(wb,paste0(DATA_OUT,"/NTE data - consistent.xlsx"),overwrite = T)
