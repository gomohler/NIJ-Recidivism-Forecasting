
library(tidyverse)


add_features <- function(df){
  df %>% 
    #-- Convert ordered factors to integers (ignore right censoring)
    mutate(across(starts_with("Arrests_"), ~as.integer(.x) - 1, .names="num_{.col}") )%>% 
    mutate(across(starts_with("Convictions_"), ~as.integer(.x) - 1,.names="num_{.col}")) %>% 
    mutate(yrs18 = as.integer(Age_at_Release)*5 -3 ) %>%  # yrs over 18 (approx)  
    #-- Calculate Total Arrests
    rowwise() %>% 
    mutate(Total_Arrests = sum(c_across(starts_with("num_Arrests_")))) %>% 
    #-- Calculate Total Convictions
    mutate(Total_Convictions = sum(c_across(starts_with("num_Convictions_")))) %>% 
    #-- Standardize by Age
    ungroup() %>% 
    mutate(Arrests_Age = Total_Arrests / as.integer(Age_at_Release),
           Convictions_Age = Total_Convictions / as.integer(Age_at_Release))
}



col_ordinal <- function(levels=NULL){
  col_factor(levels, ordered=TRUE, include_na=TRUE)
}

col_specs = cols(
  .default = col_factor(include_na=TRUE),
  ID = col_character(),
  Gender = col_factor(include_na=TRUE),
  Race = col_factor(include_na=TRUE),
  Age_at_Release = col_ordinal(c("18-22","23-27","28-32","33-37","38-42","43-47","48 or older")), 
  Residence_PUMA = col_factor(as.character(1:25)),
  Supervision_Risk_Score_First = col_ordinal(as.character(1:10)),
  Supervision_Level_First = col_ordinal(c('Standard', 'High', 'Specialized')),
  Education_Level = col_ordinal(c("Less than HS diploma", "High School Diploma", "At least some college")),
  Dependents = col_ordinal(c(0:2, "3 or more")),
  ## Check this: did they mean Greater than 2 to 3??
  Prison_Years = col_ordinal(c("Less than 1 year","1-2 years","Greater than 2 to 3 years","More than 3 years" )),
  Prior_Arrest_Episodes_Felony = col_ordinal(c(0:9, "10 or more")),
  Prior_Arrest_Episodes_Misd = col_ordinal(c(0:5, "6 or more")),
  Prior_Arrest_Episodes_Violent = col_ordinal(c(0:2,"3 or more")),
  Prior_Arrest_Episodes_Property = col_ordinal(c(0:4,"5 or more")),
  Prior_Arrest_Episodes_Drug = col_ordinal(c(0:4,"5 or more")),
  `_v1` = col_ordinal(c(0:4,"5 or more")),
  Prior_Conviction_Episodes_Felony = col_ordinal(c(0:2,"3 or more")),
  Prior_Conviction_Episodes_Misd = col_ordinal(c(0:3,"4 or more")),
  Prior_Conviction_Episodes_Prop = col_ordinal(c(0:2,"3 or more")),
  Prior_Conviction_Episodes_Drug = col_ordinal(c(0:1,"2 or more")),
  ## Training Data (for after Year 1)
  Delinquency_Reports = col_ordinal(c(0:3,"4 or more")),
  Program_Attendances = col_ordinal(c(0:9, "10 or more")),
  Program_UnexcusedAbsences = col_ordinal(c(0:2,"3 or more")),
  Residence_Changes = col_ordinal(c(0:2,"3 or more")),
  Avg_Days_per_DrugTest = col_double(), 
  DrugTests_THC_Positive = col_double(), 
  DrugTests_Cocaine_Positive = col_double(), 
  DrugTests_Meth_Positive = col_double(),
  DrugTests_Other_Positive = col_double(),
  Percent_Days_Employed = col_double(),
  Jobs_Per_Year = col_double(), 
  Recidivism_Within_3years = col_logical(), 
  Recidivism_Arrest_Year1 = col_logical(), 
  Recidivism_Arrest_Year2 = col_logical(),
  Recidivism_Arrest_Year3 = col_logical()
)

clean_NIJ_names <- function(df) {
  df %>% rename(PUMA = Residence_PUMA)  %>% 
    rename_with(~str_replace_all(.x, 'Prior_(.+)_Episodes', '\\1s')) %>% 
    rename_with(~str_replace_all(.x, 'Prior_Revocations', 'Revos')) 
}




new_train= read_csv("train.csv",col_types = col_specs) %>%
  clean_NIJ_names() %>% 
  add_features() %>% 
  rename(
    Y1 = Recidivism_Arrest_Year1, 
  )




