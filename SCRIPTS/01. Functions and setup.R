#_____________________________________________________________________________
### Functions ----
#_____________________________________________________________________________

#.............................................................................
#### Processing functions ----
#.............................................................................

# Function to import tab data and manipulate. Alternatively load R datafile
import_save_dta <- function(dta_num=NA,
                            loadRDS=FALSE,
                            old_dat17=FALSE,
                            years_vector=dataset_years) {
  
  # Relevant names
  temp_year <- years_vector[dta_num]
  temp_name <- paste0("lfsp_aj_",temp_year)
  
  if (loadRDS==FALSE) {
    temp_dta <- read.table(file = paste0(INPUT,"\\",dataset_ext_names[dta_num],".tab"),
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
    
    # Save R data and allow loading
    saveRDS(temp_dta,
            file=paste0(RDATA,temp_name,".rds"))
    
  } else { #otherwise load dataset directly
    temp_dta <- readRDS(file=paste0(RDATA,temp_name,".rds"))
  }
 
  if (old_dat17==TRUE) {
    # # To check against previous work, load previous dataset
    if (temp_year==2017) {
      
      # Load the 2017 dataset from SPSS with PWT17
      lfsp_aj_2017_pwt17 <-  read_sav(paste0(INPUT,"\\","lfsp_aj17_eul",".sav"))
      
      temp_dta <- lfsp_aj_2017_pwt17 %>%
        mutate(weight_val = PWT17,
               weight_var = "PWT17")
    }
  }
  
  temp_list <- list("dta"=temp_dta,"name"=temp_name,"year"=temp_year)
  return(temp_list)
}

# Function to adjust the data for our use, inspired by previous SPS code
## Missing variables:
## NATOX, CRYOX7
recode_dta <- function(dta=NA) {
  
  # Change data
  
  dta_adj <- dta %>% 
    mutate(london_worker = case_when(GORWKR==8 ~ "London",
                                     GORWKR==-8 ~ "No answer",
                                     GORWKR==-9 ~ "NA",
                                     is.na(GORWKR) ~ "NA",
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
                       uk_tot=FALSE,
                       cons_method=FALSE) {
  
  # Old weighting method - London workers, and everyone else who works in other bucket
  ## Note: a defunct version also shown in old xlsx files also puts in people who do not work into the bin. That is not the case in published figures.
   if (cons_method==TRUE) {
     if (uk_tot==FALSE) {
       
       # Group together everyone else in not London, and do not group by employment status
       dta_new <- dta %>% 
         mutate(london_worker=case_when(london_worker=="London" ~ london_worker,
                                        TRUE ~ "Not London")) %>% 
         group_by(london_worker,ILODEFR,quarter_response) %>% # if not grouping by ILODEFR, would get the discarded higher weight of 1.2813518
         summarise(weight_val = sum(weight_val)) %>% 
         ungroup() %>% 
         pivot_wider(id_cols=c(london_worker,ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
         mutate(uprate_weight_ldn = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
         select(london_worker,ILODEFR,uprate_weight_ldn)
       
     }  
     else if (uk_tot==TRUE) {
       
       dta_new <- dta %>% 
         mutate(london_worker=case_when(london_worker=="London" ~ london_worker,
                                        TRUE ~ "Not London")) %>% 
         group_by(ILODEFR,quarter_response) %>% 
         summarise(weight_val = sum(weight_val)) %>% 
         ungroup() %>% 
         pivot_wider(id_cols=c(ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
         mutate(uprate_weight_uk = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
         select(ILODEFR,uprate_weight_uk)
     }
   }
  
  # More meaningful: separating out non-workers from both groups
  else {
    if (uk_tot==FALSE) {
      
      # First find uprating for London and non-London
      dta_new <- dta %>% 
        group_by(london_worker,ILODEFR,quarter_response) %>% 
        summarise(weight_val = sum(weight_val)) %>% 
        filter(ILODEFR==1) %>%
        ungroup() %>% 
        pivot_wider(id_cols=c(london_worker,ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
        mutate(uprate_weight_ldn = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
        select(london_worker,ILODEFR,uprate_weight_ldn)
      
    }  
    else if (uk_tot==TRUE) {
      
      dta_new <- dta %>% 
        group_by(ILODEFR,quarter_response) %>% 
        summarise(weight_val = sum(weight_val)) %>% 
        filter(ILODEFR==1) %>% 
        ungroup() %>% 
        pivot_wider(id_cols=c(ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
        mutate(uprate_weight_uk = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
        select(ILODEFR,uprate_weight_uk)
    }
  }
  
  
  return(dta_new)
}
