library(tidyverse)
library(readr)

pums <- read.csv("psam_p36.csv")

# 1. Data Initial Exploration
# 1.1 Select the individuals within 5 boroughs of NYC
nyc_puma10 <- c(3701:3711, 3801:3814, 3901:3907, 4001:4019, 4101:4114)

# 1.2 Select 16-24 yrs old pop.
youth <- pums %>%
  filter(PUMA10 %in% nyc_puma10, AGEP >= 16, AGEP <= 24)

# 1.3 Select the individuals which are not enrolled in shcool(SCH == 3) & not in labor force or unemployed (ESR%in%c(6,7)) and create variable disconnected
youth <- youth %>%
  mutate(
    SCH = as.numeric(SCH),
    disconnected = if_else(SCH == 3 & ESR %in% c(6, 7), 1, 0)
  )

# 1.4 Calculate weighted disconnection rate
disconnection_summary_weighted <- youth %>%
  group_by(PUMA10) %>%
  summarise(
    total_youth = sum(PWGTP, na.rm = TRUE),
    disconnected_youth = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnection_rate = disconnected_youth / total_youth
  ) %>%
  ungroup() %>%
  mutate(
    GEOID20 = paste0("360", sprintf("%04d", PUMA10)),
    Disconnection_Rate_Percent = paste0(round(disconnection_rate * 100, 1), "%")
  )
write.csv(disconnection_summary_weighted, "nyc_disconnection_rate_weighted_percent.csv", row.names = FALSE)

# Save the file
write.csv(disconnection_summary_weighted, "nyc_disconnection_rate_weighted_percent.csv", row.names = FALSE)

summary(disconnection_summary_weighted)

# 1.4 Keep the variables I want
vars_to_keep <- c(
  "AGEP", "SEX", "RAC1P", "PINCP", "POVPIP", "NATIVITY", "SCHL", "WRK", "SSIP",
  "HINS1", "HINS2", "HINS3", "HINS4", "HINS5", "HINS6", "HINS7", "PUBCOV", "PRIVCOV",
  "DIS", "RELSHIPP", "MAR", "NOP", "YOEP", "LANX", "ENG", "POBP", "MIG", "MIGSP",
  "MIGPUMA10", "HICOV", "disconnected", "PWGTP"  
)

youth_selected <- youth %>%
  select(all_of(vars_to_keep))

write_csv(youth_selected, "cleaned_youth_data.csv")

# 2. Basic demographic characteristics
library(dplyr)
library(readr)

# 2.1 Group by age group 
youth_demo <- youth_selected %>%
  mutate(age_group = case_when(
    AGEP >= 16 & AGEP <= 17 ~ "16-17",
    AGEP >= 18 & AGEP <= 19 ~ "18-19",
    AGEP >= 20 & AGEP <= 21 ~ "20-21",
    AGEP >= 22 & AGEP <= 24 ~ "22-24"
  ))

age_summary <- youth_demo %>%
  group_by(age_group) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(age_summary, "weighted_summary_by_age.csv")

# 2.2 Group by sex 
sex_summary <- youth_selected %>%
  mutate(SEX = recode(SEX, `1` = "Male", `2` = "Female")) %>%
  group_by(SEX) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(sex_summary, "weighted_summary_by_sex.csv")


# 2.3 Group by race
race_summary <- youth_selected %>%
  group_by(RAC1P) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

race_summary_clean <- race_summary %>%
  mutate(Race = recode(
    RAC1P,
    `1` = "White",
    `2` = "Black or African American",
    `3` = "American Indian or Alaska Native",
    `4` = "Asian",
    `5` = "Native Hawaiian or Other Pacific Islander",
    `6` = "Some Other Race",
    `7` = "Two or More Races",
    .default = "Other"
  )) %>%
  select(Race, everything(), -RAC1P)

write_csv(race_summary_clean, "weighted_summary_by_race.csv")

# 2.4 Group by nativity 
nativity_summary <- youth_selected %>%
  mutate(native = if_else(NATIVITY == 1, "US-born", "Foreign-born")) %>%
  group_by(native) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(nativity_summary, "weighted_summary_by_nativity.csv")

# 2.5 Group by birthplace 
pobp_summary <- youth_selected %>%
  group_by(POBP) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

write_csv(pobp_summary, "weighted_summary_by_birthplace.csv")

sex_summary
age_summary
race_summary
nativity_summary
head(pobp_summary, 10)

# 3. Economic Factors
# 3.1 Relative Poverty
poverty_summary <- youth_selected %>%
  mutate(poverty_status = case_when(
    POVPIP < 100 ~ "Below poverty line",
    POVPIP >= 100 & POVPIP < 200 ~ "100–199% FPL",
    POVPIP >= 200 ~ "200%+ FPL",
    TRUE ~ NA_character_
  )) %>%
  group_by(poverty_status) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(poverty_summary, "summary_by_poverty_status.csv")


# 3.1.1 View observations with missing POVPIP
poverty_na <- youth_selected %>%
  filter(is.na(POVPIP))

# 3.1.2 Disconnection rate among these observations (weighted)
summary_na <- poverty_na %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

# 3.1.3 Relative analysis (non-weighted, just frequency tables)
table(poverty_na$SCHL, useNA = "always")      # Education level
table(poverty_na$ENG, useNA = "always")       # English Ability
table(poverty_na$NATIVITY, useNA = "always")  # Birthplace
table(poverty_na$MIG, useNA = "always")       # Immigration Status
table(poverty_na$RAC1P, useNA = "always")     # Race
table(poverty_na$HICOV, useNA = "always")     # Insurance
summary(poverty_na$NOP)                       # Family population


# 3.2 Individual Income
income_summary <- youth_selected %>%
  mutate(income_group = case_when(
    PINCP < 0 | PINCP == 0 ~ "No income", 
    PINCP > 0 & PINCP < 10000 ~ "0–10k",
    PINCP >= 10000 & PINCP < 25000 ~ "10k–25k",
    PINCP >= 25000 ~ "25k+",
    TRUE ~ NA_character_
  )) %>%
  group_by(income_group) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

write_csv(income_summary, "summary_by_income_group.csv")


# 3.3 SSI Receiving
ssi_summary <- youth_selected %>%
  mutate(ssi_status = if_else(SSIP > 0, "Receiving SSI", "Not receiving SSI")) %>%
  group_by(ssi_status) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(ssi_summary, "summary_by_ssi.csv")

# 4 Insurance & Disability
# 4.1 Having Insurance
hins_summary <- youth_selected %>%
  mutate(HICOV = recode(HICOV, `1` = "Insured", `2` = "Uninsured")) %>%
  group_by(HICOV) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(hins_summary, "summary_by_insurance_coverage.csv")


# 4.2 Private or Public Insurance
pub_priv_summary <- youth_selected %>%
  mutate(
    Public_Insurance = if_else(PUBCOV == 1, "Public", "No Public"),
    Private_Insurance = if_else(PRIVCOV == 1, "Private", "No Private")
  ) %>%
  group_by(Public_Insurance, Private_Insurance) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(pub_priv_summary, "summary_by_public_private_insurance.csv")


# 4.3 Disability
disability_summary <- youth_selected %>%
  mutate(disability = recode(DIS, `1` = "With Disability", `2` = "No Disability")) %>%
  group_by(disability) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(disability_summary, "summary_by_disability_status.csv")

# 5. Family Info
# 5.1 Relationship to Householder: summary & label conversion
rel_summary <- youth_selected %>%
  group_by(RELSHIPP) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  ungroup() %>%
  mutate(Household_Role = recode(
    as.character(RELSHIPP),
    `20` = "Other relative (e.g. cousin, nephew, aunt)",
    `21` = "Housemate or roommate",
    `22` = "Roomer or boarder",
    `23` = "Foster child or unrelated child <18",
    `24` = "Other nonrelative",
    `25` = "Biological child",
    `26` = "Stepchild",
    `27` = "Adopted child",
    `28` = "Sibling",
    `30` = "Parent",
    `32` = "Grandparent",
    `33` = "Parent-in-law",
    `34` = "Son/daughter-in-law",
    `35` = "Other relative",
    `36` = "Unmarried partner",
    `37` = "Housemate or roommate",
    `38` = "Other nonrelative",
    .default = "Unknown"
  ))
   
# Write to CSV
write_csv(rel_summary, "summary_by_relationship_to_householder.csv")


# 5.2 Family/Household Size
nop_summary <- youth_selected %>%
  mutate(household_size = case_when(
    NOP <= 2 ~ "1-2",
    NOP == 3 ~ "3",
    NOP == 4 ~ "4",
    NOP >= 5 ~ "5+",
    TRUE ~ NA_character_
  )) %>%
  group_by(household_size) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

write_csv(nop_summary, "summary_by_household_size.csv")


# 5.3 Marital Status
mar_summary <- youth_selected %>%
  group_by(MAR) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  ungroup() %>%
  # Recode MAR values to readable labels
  mutate(Marital_Status = recode(
    as.character(MAR),
    `1` = "Married",
    `2` = "Widowed",
    `3` = "Divorced",
    `4` = "Separated",
    `5` = "Never married/single",
    `6` = "Married, spouse absent",
    .default = "Other/Unknown"
  )) %>%
  select(Marital_Status, total, disconnected_n, disconnected_rate) %>%
  arrange(desc(disconnected_rate))

write_csv(mar_summary, "summary_by_marital_status.csv")

# 6 Migration & Education
# 6.1 Moved within the past year
mig_summary <- youth_selected %>%
  mutate(moved = recode(MIG,
                        `1` = "Same house",
                        `2` = "Same county",
                        `3` = "Different county/state")) %>%
  group_by(moved) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

write_csv(mig_summary, "summary_by_migration_status.csv")


# 6.2 English Ability
eng_summary <- youth_selected %>%
  mutate(english_level = case_when(
    ENG == 1 ~ "Very well",
    ENG == 2 ~ "Well",
    ENG == 3 ~ "Not well",
    ENG == 4 ~ "Not at all"
  )) %>%
  group_by(english_level) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

eng_df <- read_csv("summary_by_english_proficiency.csv", show_col_types = FALSE)
group_var <- names(eng_df)[1]
eng_df_clean <- eng_df %>%
  filter(!is.na(.data[[group_var]]))
write_csv(eng_df_clean, "summary_by_english_proficiency_cleaned.csv")
#To ensure accurate interpretation of disconnection rates across English proficiency levels, I excluded rows with missing (NA) values in the English Ability column. These missing values likely represent individuals for whom language proficiency was not recorded and could introduce bias or noise in visualization and modeling.

# 6.3 Main Language at Home
lanx_summary <- youth_selected %>%
  mutate(language_home = recode(LANX,
                                `1` = "English",
                                `2` = "Other")) %>%
  group_by(language_home) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  )

write_csv(lanx_summary, "summary_by_home_language.csv")


# 6.4 Highest Education
edu_summary <- youth_selected %>%
  mutate(education = case_when(
    SCHL %in% 1:15 ~ "Less than HS",
    SCHL == 16 ~ "High school grad",
    SCHL == 17 ~ "Some college",
    SCHL >= 18 ~ "Bachelor or above"
  )) %>%
  group_by(education) %>%
  summarize(
    total = sum(PWGTP, na.rm = TRUE),
    disconnected_n = sum(PWGTP[disconnected == 1], na.rm = TRUE),
    disconnected_rate = disconnected_n / total
  ) %>%
  arrange(desc(disconnected_rate))

write_csv(edu_summary, "summary_by_education_level.csv")




# Define file groups by category
demographic_files <- list(
  "Age Group" = "weighted_summary_by_age.csv",
  "Sex" = "weighted_summary_by_sex.csv",
  "Race" = "weighted_summary_by_race.csv",
  "Nativity" = "weighted_summary_by_nativity.csv",
  "Birthplace" = "weighted_summary_by_birthplace.csv"
)

economic_files <- list(
  "Income" = "summary_by_income_group.csv",
  "Poverty Level" = "summary_by_poverty_status.csv",
  "SSI Status" = "summary_by_ssi.csv"
)

insurance_files <- list(
  "Insurance Coverage" = "summary_by_insurance_coverage.csv",
  "Insurance Type" = "summary_by_public_private_insurance.csv",
  "Disability" = "summary_by_disability_status.csv"
)

family_files <- list(
  "Household Role" = "summary_by_relationship_to_householder.csv",
  "Household Size" = "summary_by_household_size.csv",
  "Marital Status" = "summary_by_marital_status.csv"
)

migration_edu_files <- list(
  "Migration" = "summary_by_migration_status.csv",
  "English Ability" = "summary_by_english_proficiency_cleaned.csv",
  "Home Language" = "summary_by_home_language.csv",
  "Education" = "summary_by_education_level.csv"
)

combine_group <- function(file_list) {
  purrr::imap_dfr(file_list, function(file, category) {
    df <- read_csv(file, show_col_types = FALSE)
    group_var <- names(df)[1]
    
    df %>%
      rename(group = all_of(group_var)) %>%
      mutate(
        group = as.character(group),  # ← force to character
        category = category
      ) %>%
      select(category, group, everything())
  })
}


# Combine each group
demographic_df <- combine_group(demographic_files)
economic_df <- combine_group(economic_files)
insurance_df <- combine_group(insurance_files)
family_df <- combine_group(family_files)
migration_edu_df <- combine_group(migration_edu_files)

# Save to CSV 
write_csv(demographic_df, "Demographic_summary.csv")
write_csv(economic_df, "Economic_summary.csv")
write_csv(insurance_df, "Insurance_Disability_summary.csv")
write_csv(family_df, "Family_Info_summary.csv")
write_csv(migration_edu_df, "Migration_Education_summary.csv")


#Combine all
library(tidyverse)

files <- list(
  "Age Group" = "weighted_summary_by_age.csv",
  "Sex" = "weighted_summary_by_sex.csv",
  "Race" = "weighted_summary_by_race.csv",
  "Nativity" = "weighted_summary_by_nativity.csv",
  "Birthplace" = "weighted_summary_by_birthplace.csv",
  "Income" = "summary_by_income_group.csv",
  "Poverty Level" = "summary_by_poverty_status.csv",
  "SSI Status" = "summary_by_ssi.csv",
  "Insurance Coverage" = "summary_by_insurance_coverage.csv",
  "Insurance Type" = "summary_by_public_private_insurance.csv",
  "Disability" = "summary_by_disability_status.csv",
  "Household Role" = "summary_by_relationship_to_householder.csv",
  "Household Size" = "summary_by_household_size.csv",
  "Marital Status" = "summary_by_marital_status.csv",
  "Migration" = "summary_by_migration_status.csv",
  "English Ability" = "summary_by_english_proficiency.csv",
  "Home Language" = "summary_by_home_language.csv",
  "Education" = "summary_by_education_level.csv"
)

combined_df <- purrr::imap_dfr(files, function(file, category) {
  df <- read_csv(file, show_col_types = FALSE)
  
  group_var <- names(df)[1]
  
  df_long <- df %>%
    rename(group = all_of(group_var)) %>%
    mutate(
      group = as.character(group),
      category = category
    ) %>%
    select(category, group, everything())
  
  return(df_long)
})

write_csv(combined_df, "combined_disconnection_summary_long.csv")




