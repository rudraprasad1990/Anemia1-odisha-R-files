# Load required libraries
library(lubridate)
library(epiDisplay)
library(tidyverse)
library(Gmisc)
library(moments)
library(ellmer)
library(knitr)
library(openxlsx)
library(janitor)
library(dplyr)
library(tidyr)
library(gtsummary)

# Assuming you have loaded your dataset from a CSV file exported from REDCap:
df <- data1
  
  df_cleaned <- df %>%
    
    # ==========================================
    # 1. DATE FORMATTING
    # REDCap date format 'date_dmy' needs to be converted to R Date objects
    # Calculating the age variable both from age calculated and dob field
    # ==========================================
    mutate(
      interview_date =mdy(interview_date),
      respondent_dob = mdy(respondent_dob),

      respondent_age_calculated = NA,
      respondent_age_calculated = floor((interview_date - respondent_dob)/365),

      respondent_age = NA,
      respondent_age = respondent_age_reported,
      respondent_age = ifelse(is.na(respondent_age),respondent_age_calculated ,respondent_age)
    ) %>%
    
    # ==========================================
    # 2. ENFORCING BRANCHING LOGIC (SKIP LOGIC)
    # If the parent condition isn't met, set the dependent field to NA
    # ==========================================
    mutate(
      # Beneficiary Name
      name_ben = ifelse(beneficiary_group_selected %in% c(2,3,4,5,6,7), name_ben, NA),
      
      # Age & DOB Logic
      respondent_dob = if_else(remember_dob == 1, respondent_dob, as.Date(NA)),
      respondent_age_calculated = ifelse(remember_dob == 1, respondent_age_calculated, NA),
      respondent_age_reported = ifelse(remember_dob == 0, respondent_age_reported, NA),
      
      # 'Others (Specify)' fields
      respondent_occupation_88 = ifelse(respondent_occupation == 88, respondent_occupation_88, NA),
      religion_88 = ifelse(religion == 88, religion_88, NA),
      social_category_88 = ifelse(social_category == 88, social_category_88, NA),
      family_type_88 = ifelse(family_type == 88, family_type_88, NA),
      hh_head_relation_88 = ifelse(hh_head_relation == 88, hh_head_relation_88, NA),
      hh_head_occupation_88 = ifelse(hh_head_occupation == 88, hh_head_occupation_88, NA),
      house_ownership_88 = ifelse(house_ownership == 88, house_ownership_88, NA),
      main_cooking_fuel_88 = ifelse(main_cooking_fuel == 88, main_cooking_fuel_88, NA),
      drinking_water_source_88 = ifelse(drinking_water_source == 88, drinking_water_source_88, NA),
      toilet_type_88 = ifelse(toilet_type == 88, toilet_type_88, NA),
      
      # Health Insurance Logic
      type_health_insurance = ifelse(has_health_insurance == 1, type_health_insurance, NA),
      specify_health_insurance = ifelse(has_health_insurance == 1, specify_health_insurance, NA),
      
      # Water Treatment Logic (REDCap exports checkboxes as multiple columns ending in ___X)
      water_treatment_method___1 = ifelse(water_treated_before_drink == 1, water_treatment_method___1, NA),
      water_treatment_method___2 = ifelse(water_treated_before_drink == 1, water_treatment_method___2, NA),
      water_treatment_method___3 = ifelse(water_treated_before_drink == 1, water_treatment_method___3, NA),
      water_treatment_method___4 = ifelse(water_treated_before_drink == 1, water_treatment_method___4, NA),
      water_treatment_method___5 = ifelse(water_treated_before_drink == 1, water_treatment_method___5, NA),
      water_treatment_method___6 = ifelse(water_treated_before_drink == 1, water_treatment_method___6, NA),
      water_treatment_method___88 = ifelse(water_treated_before_drink == 1, water_treatment_method___88, NA),
      water_treatment_method_88 = ifelse(water_treatment_method___88 == 1 & !is.na(water_treatment_method___88), water_treatment_method_88, NA),
      
      # Form comments
      add_comments = ifelse(comments_recorded == 1, add_comments, NA)
    ) %>%
    
    # ==========================================
    # 3. LABELING CATEGORICAL VARIABLES (FACTOR CREATION)
    # Keeping the original numeric columns but creating new factor columns (labeled with _f)
    # ==========================================
    mutate(
      beneficiary_group_selected_f = factor(beneficiary_group_selected, 
                                            levels = 2:7, 
                                            labels = c("Pregnant women", "Mother of Children 6-59m", 
                                                       "Mother of Children 5-9y", "Adolescent Girls", 
                                                       "Adolescent Boys", "WRA 20-49 yrs")),

      respondent_education_years_cat = cut(respondent_education_years,
        breaks = c(-1,0,5,7,10,12,15,20),
        labels = c("No formal schooling","Primary","Upper Primary","Secondary","Higher Secondary","Graduation","PG and above")),

      respondent_occupation_f = factor(respondent_occupation,
        levels=c(1,2,3,4,5,6,7,8,77,88), 
        labels=c("Government service","Private service","Daily wage earner","Self-employed","Farming","Housemaker","Student","Does not work","Not applicable","Others")),
      
      religion_f = factor(religion, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 88), 
                          labels = c("Hindu", "Muslim", "Sikh", "Christian", "Buddhist/Neo-Buddhist", 
                                     "Jain", "Jewish", "Parsi/Zoroastrian", "Others")),

      social_category_f = factor(social_category, 
                                 levels = c(1, 2, 3, 4, 77, 88), 
                                 labels = c("General", "Scheduled Caste", "Scheduled Tribe", 
                                            "Other Backward Classes", "Not applicable", "Others")),
      
      family_type_f = factor(family_type,
        levels=c(1,2,3,88),
        labels=c("Nuclear","Joint","Extended","Others")),
      
      respondent_marital_status_f = factor(respondent_marital_status,
        levels=c(1,2,3,4,5,6),
        labels=c("Married","Cohabiting","Widowed","Divorced","Deserted","Single")),
      
      hh_head_gender_f = factor(hh_head_gender,
        levels=c(1,2,3),
        labels=c("Male","Female","Transgender")),

      hh_head_relation_f = factor(hh_head_relation,
        levels=c(1,2,3,4,88),
        labels=c("Spouse","Father/Mother/Father-in-law/Mother-in-law","Grandfather/mother","Self","Others")),
      
      hh_head_education_years_f = cut(hh_head_education_years,
        breaks = c(-1,0,5,7,10,12,15,20),
        labels = c("No formal schooling","Primary","Upper Primary","Secondary","Higher Secondary","Graduation","PG and above")),

      hh_head_occupation_f = factor(hh_head_occupation,
        levels=c(1,2,3,4,5,6,7,8,77,88), 
        labels=c("Government service","Private service","Daily wage earner","Self-employed","Farming","Housemaker","Student","Does not work","Not applicable","Others")),

      house_ownership_f = factor(house_ownership, 
        levels = c(1,2,3,88),
        labels = c("Self-owned","Rented","Quarter: Government/ Company accommodation","Others")),
      
      owns_other_house_f = factor(owns_other_house, 
        levels=c(0,1),
        labels = c("No","Yes")),
      
      has_kitchen_f=factor(has_kitchen, 
        levels=c(0,1,2),
        labels = c("No","Yes","Cooked outside/in open")),

      main_cooking_fuel_f = factor(main_cooking_fuel,
        levels=c(1,2,3,88),
        labels=c("Clean Fuels (LPG, Electricity, Biogas, Solar energy)","Solid Fuels (Wood/Firewood, Coal/Charcoal, Dungcakes, Crop residue)","Liquid/Petroleum Fuels (Kerosene, etc)","Others")),

      owns_agri_land_f = factor(owns_agri_land,
        levels = c(0,1),
        labels = c("No","Yes")),
      
      has_bpl_card_f = factor(has_bpl_card,
        levels = c(0,1,99),
        labels = c("No","Yes","Don't Know")),
      
      has_health_insurance_f = factor(has_health_insurance, 
        levels = c(0, 1, 99), 
        labels = c("No", "Yes", "Don't know")),
          
      type_health_insurance_f = factor(type_health_insurance,
        levels = c(1,2),
        labels = c("Private","Government")),
      
      drinking_water_source_f = factor(drinking_water_source,
        levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,88),
        labels=c("Piped water into dwelling","Piped to yard/plot","Public tap/standpipe","Tube well or borehole","Protected well","Unprotected well","Water from spring (protected)","Water from spring (unprotected)","Tanker truck","Cart with small tank","Rainwater","Surface water (river/ dam/ lake/ pond/ stream/canal/ irrigation channel)","Bottled water or sachets","Water vendor","Others")),
      
      water_treated_before_drink_f = factor(water_treated_before_drink,
        levels = c(0,1,99),
        labels = c("No","Yes","Don't Know")),

      toilet_type_f = factor(toilet_type, 
        levels = c(1, 2, 3, 88), 
        labels = c("Open space", "Pit latrine", "Flush toilet", "Others")),   
      
    ) %>%
    
    # ==========================================
    # 4. CONSTRAINTS (Ensuring min/max boundaries)
    # Replaces values outside of REDCap's min/max ranges with NA
    # ==========================================
    mutate(
      respondent_education_years = ifelse(respondent_education_years < 0 | respondent_education_years > 35, NA, respondent_education_years),
      respondent_age_reported = ifelse(respondent_age_reported < 1 | respondent_age_reported > 100, NA, respondent_age_reported),
      subject_code = ifelse(subject_code < 1 | subject_code > 999, NA, subject_code)
    )
  
# Run the function on your dataset
# df_clean <- clean_household_survey(df)

# View a summary of the cleaned dataset
# summary(df_clean)


# # 1. Create the grouped summary table
# beneficiary_summary_table <- df_cleaned %>%
#   # Select the grouping variable AND all the categorical variables you want to summarize
#   select(
#     beneficiary_group_selected_f,
#     respondent_education_years_cat,
#     respondent_occupation_f,
#     religion_f,
#     social_category_f,
#     family_type_f,
#     respondent_marital_status_f,
#     hh_head_gender_f,
#     hh_head_relation_f,
#     hh_head_education_years_f,
#     hh_head_occupation_f,
#     house_ownership_f,
#     owns_other_house_f,
#     has_kitchen_f,
#     main_cooking_fuel_f,
#     owns_agri_land_f,
#     has_bpl_card_f,
#     has_health_insurance_f,
#     type_health_insurance_f,
#     drinking_water_source_f,
#     water_treated_before_drink_f,
#     toilet_type_f
#   ) %>%
#   # 2. Use tbl_summary and group by the beneficiary group
#   tbl_summary(
#     by = beneficiary_group_selected_f,        # <-- This splits the data into columns by group
#     missing = "ifany",                        # Shows NA/Missing counts if they exist
#     missing_text = "Missing",
#     statistic = all_categorical() ~ "{n} ({p}%)" # Format: Count (Percentage%)
#   ) %>%
#   add_overall() %>%                           # Adds an "Overall" total column
#   bold_labels()

# # 3. Print the table in your RStudio Viewer
# beneficiary_summary_table

# # Note: You can export this table to Word using:
# # beneficiary_summary_table %>% as_flex_table() %>% flextable::save_as_docx(path = "Beneficiary_Table.docx")


beneficiary_summary_table_all <- df_cleaned %>%
  # 1. Convert the binary 1/0 variables to "Yes/No" factors so they display nicely 
  # as counts/percentages instead of average medians
  mutate(
    across(
      c(remember_dob, comments_recorded, starts_with("asset_"), starts_with("water_treatment_method___")), 
      ~factor(., levels = c(0, 1), labels = c("No", "Yes"))
    ),
    survey_round = as.factor(survey_round) # Convert round 1-5 to categorical
  ) %>%
  
  # 2. Select ALL relevant variables from the codebook
  select(
    beneficiary_group_selected_f,
    
    # Administrative & Geography
    survey_round, state, district, block, phc, village,
    
    # Continuous / Numeric variables (gtsummary will show Median and Range)
    respondent_age, respondent_education_years,
    hh_total_members, hh_pregnant_women, hh_lactating_women, 
    hh_child_6_59m, hh_child_5_9y, hh_ado_boys, hh_ado_girls, hh_npnl_wra,
    hh_head_age, hh_head_education_years, house_rooms_count,
    
    # Categorical Variables (The _f and _cat versions you cleaned)
    respondent_education_years_cat, respondent_occupation_f, religion_f, 
    social_category_f, family_type_f, respondent_marital_status_f, 
    hh_head_gender_f, hh_head_relation_f, hh_head_education_years_f, 
    hh_head_occupation_f, house_ownership_f, owns_other_house_f, 
    has_kitchen_f, main_cooking_fuel_f, owns_agri_land_f, has_bpl_card_f,
    has_health_insurance_f, type_health_insurance_f, drinking_water_source_f, 
    water_treated_before_drink_f, toilet_type_f,
    
    # Binary Yes/No Variables
    remember_dob, comments_recorded,
    
    # Assets (Selects all 11 asset_ columns)
    starts_with("asset_"),
    
    # Water Treatment methods (Selects all 7 water_treatment_method___ columns)
    starts_with("water_treatment_method___")
  ) %>%
  
  # 3. Generate the Table
  tbl_summary(
    by = beneficiary_group_selected_f,
    missing = "ifany",
    missing_text = "Missing",
    # Specify exactly how you want the math displayed:
    statistic = list(
      all_continuous() ~ "{median} ({min} - {max})", # For age, counts, years
      all_categorical() ~ "{n} ({p}%)"              # For factors, assets, 1/0s
    )
  ) %>%
  add_overall() %>%
  bold_labels()

# Print the final massive table
beneficiary_summary_table_all

# Optional: Export it to Word
# beneficiary_summary_table_all %>% as_flex_table() %>% flextable::save_as_docx(path = "Full_Codebook_Summary.docx")
