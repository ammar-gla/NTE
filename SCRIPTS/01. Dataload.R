#_____________________________________________________________________________
### Dataload and data processing ----
#_____________________________________________________________________________


# Load the LFS datatets
dataset_nm_1014 <- c("lfsp_aj10_end_user","lfsp_aj11_end_user","lfsp_aj12_end_user","lfsp_aj13_end_user","lfsp_aj14_end_user")
dataset_ext_names <- c("lfsp_aj15_eul","lfsp_aj16_eul","lfsp_aj17_eul","lfsp_aj18_eul",
                       "lfsp_aj19_eul_pwt18","lfsp_aj20_eul_pwt22","lfsp_aj21_eul_pwt22","lfsp_aj22_eul_pwt22")
dataset_years <- c(2015:2022)

# Load the 2017 dataset from SPSS with PWT17
lfsp_aj_2017_pwt17 <-  read_sav(paste0(INPUT,"\\","lfsp_aj17_eul",".sav"))

#.............................................................................
#### Processing functions ----
#.............................................................................


# Function to adjust the data for our use, inspired by previous SPS code
## Missing variables:
## NATOX, CRYOX7
recode_dta <- function(dta=NA) {
  
  # Change data
  
  dta_adj <- dta %>% 
    mutate(london_worker = case_when(GORWKR==8 ~ "London",
                                     GORWKR==-8 ~ "No answer",
                                     GORWKR==-9 ~ "NA",
                                     TRUE ~ "Not London"),
           london_resident = case_when(GOVTOF2==8 ~ "London",
                                       TRUE ~ "Not London"),
           age_band = case_when(AGE <16 ~ "<16",
                                inrange(AGE,16,34) ~ "16-34",
                                inrange(AGE,35,54) ~ "35-54",
                                AGE>54 ~ "55+"),
           ethnicity = case_when(ETHUKEUL==1 ~ "White",
                                 ETHUKEUL %in% c(2,3,4,5,6,7,8,9) ~ "BAME",
                                 ETHUKEUL==-8 ~ "No answer",
                                 ETHUKEUL==-9 ~ "NA"),
           quarter_response = case_when(IOUTCOME %in% c(1,2) ~ "Yes",
                                        IOUTCOME == 6 ~ "No",
                                        TRUE ~ "NA"),
           nte_worker = case_when(USUWRK2==1 | USUWRK3==1 ~ "Yes",
                                  USUWRK2== 2 & USUWRK3==2 ~ "No",
                                  USUWRK2== -8 & USUWRK3==-8 ~ "No answer",
                                  USUWRK2== -9 & USUWRK3==-9 ~ "NA",
                                  TRUE ~ "NA")) 
  
  return(dta_adj)
  
}


# For adjusted weight find prop of people who were not in this quarter's survey, and uprate
## The reason we are using weights from employed people is because they are the only ones with a region of work!
new_weight <- function(dta=NA,
                       uk_tot=FALSE) {
  
  if (uk_tot==FALSE) {
    
    # First find uprating for London and non-London
    dta_new <- dta %>% 
      group_by(london_worker,ILODEFR,quarter_response) %>% 
      summarise(weight_val = sum(weight_val)) %>% 
      filter(ILODEFR==1 & !(london_worker %in% c("No answer","NA"))) %>%
      ungroup() %>% 
      pivot_wider(id_cols=london_worker,values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
      mutate(uprate_weight_ldn = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
      select(london_worker,uprate_weight_ldn)
    
  }  
  else if (uk_tot==TRUE) {
    
    dta_new <- dta %>% 
      group_by(ILODEFR,quarter_response) %>% 
      summarise(weight_val = sum(weight_val)) %>% 
      filter(ILODEFR==1) %>% 
      ungroup() %>% 
      pivot_wider(values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
      mutate(uprate_weight_uk = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
      select(uprate_weight_uk)
  }
  
  return(dta_new)
}

#.............................................................................
#### Load LFS stats, automatically clean and save ----
#.............................................................................

lfs_dataset_nm <- c()

for (y in 1:length(dataset_ext_names)) {

  temp_year <- dataset_years[y]
  temp_name <- paste0("lfsp_aj_",temp_year)
  
  temp_dta <- read.table(file = paste0(INPUT,"\\",dataset_ext_names[y],".tab"),
                         header = TRUE) 
  
  # Save relevant weight in own column 
  if (temp_year %in% c(2010:2011)) {
    temp_dta <- temp_dta %>%
      mutate(weight_val = PWT14,
             weight_var = "PWT14")
  } else  if (temp_year %in% c(2012:2019)) {
    temp_dta <- temp_dta %>%
      mutate(weight_val = PWT18,
             weight_var = "PWT18")
  } else if  (temp_year %in% c(2020:2022)) {
    temp_dta <- temp_dta %>%
      mutate(weight_val = PWT22,
             weight_var = "PWT22")
  }
  
  # # To check against previous work, load previous dataset
  # if (temp_year==2017) {
  #   temp_dta <- lfsp_aj_2017_pwt17 %>%
  #     mutate(weight_val = PWT17)
  # }
  
  # Save R data and allow loading
  saveRDS(temp_dta,
          file=paste0(RDATA,temp_name,".rds"))
  
  #temp_dta <- readRDS(file=paste0(RDATA,temp_name,".rds"))
  
  assign(temp_name,temp_dta)
  lfs_dataset_nm <- c(lfs_dataset_nm,temp_name)

  
  rm(temp_name,temp_dta,temp_year)
  
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
  
  london_wt_dta <- new_weight(lfs_dataset_list_adj[[dta_nm]])
  uk_wt_dta <- new_weight(lfs_dataset_list_adj[[dta_nm]],
                          uk_tot = TRUE)
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # Merge on the weights
  lfsp_wt <- lfs_dataset_list_adj[[dta_nm]] %>% 
    filter(ILODEFR==1 & quarter_response=="Yes") %>% 
    left_join(london_wt_dta,by="london_worker") %>% 
    merge(uk_wt_dta) %>% 
    mutate(weight_val_ldn = weight_val * uprate_weight_ldn,
           weight_val_uk = weight_val * uprate_weight_uk,
           dta_year = dta_year)
  
  lfsp_sum <- lfsp_wt %>% 
    group_by(london_worker,nte_worker,dta_year,uprate_weight_ldn,weight_var) %>% 
    summarise(wt_pop=sum(weight_val_ldn),
              unwt_pop=n()) %>% 
    group_by(london_worker,dta_year,uprate_weight_ldn,weight_var) %>% 
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


# Export tables
lfsp_aj_ldn <- lfsp_aj_full %>% 
  filter(london_worker %in% c("London","Not London")) %>% 
  mutate(id=paste(dta_year,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

wb <- loadWorkbook(paste0(DATA_OUT,"/NTE data.xlsx"))
writeData(wb, sheet = "nte_workers",lfsp_aj_ldn, colNames = T)
writeData(wb, sheet = "nte_data",lfsp_aj_full, colNames = T)
saveWorkbook(wb,paste0(DATA_OUT,"/NTE data.xlsx"),overwrite = T)
