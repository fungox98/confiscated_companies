# Load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)

# Load dataset(s) ---------------------------------------------------------

confiscated_companies <- read_excel(
  "data/confiscated_companies.xlsx",
  na = c(
    "", "N/A", "non disponibile", "nd", "n.d.", "n.d", "N.D."
    )
  )

ateco_nace_table <- read_excel("data/ateco_nace_table.xlsx")

View(confiscated_companies)
View(ateco_nace_table)

# Dataset tidying ---------------------------------------------------------

colnames(confiscated_companies)[3]  = "vat_number"
colnames(confiscated_companies)[6]  = "rea_piva_cf"
colnames(confiscated_companies)[9]  = "activity_status"
colnames(confiscated_companies)[10] = "business_name"
colnames(confiscated_companies)[11] = "legal_nature_code"
colnames(confiscated_companies)[12] = "legal_nature_description"
colnames(confiscated_companies)[13] = "ateco_nace_code"
colnames(confiscated_companies)[14] = "ateco_description"
colnames(confiscated_companies)[15] = "ateco_nace_subcode"
colnames(confiscated_companies)[16] = "ateco_subcode_description"
colnames(confiscated_companies)[17] = "registration_province"
colnames(confiscated_companies)[18] = "registration_number"
colnames(confiscated_companies)[19] = "confiscation_status"

# Merging the two province variables
confiscated_companies <- confiscated_companies |> mutate(
  province = coalesce(
    province, registration_province
    )
  )

# Add a variable with the year of the confiscation (or annulment) procedure

confiscated_companies <- confiscated_companies |>
  mutate(
    year_confiscation_procedure = str_sub(
      confiscation_status, -4, -1
      )
    )

# Standardize ateco_nace codes and description ----------------------------

# Add a point every 2 digits except for the last one
add_points <- function(code) {
  gsub("(.{2})(?!$)", "\\1.", code, perl = TRUE) 
}

confiscated_companies <- confiscated_companies |> 
  mutate(ateco_nace_subcode = sapply(ateco_nace_subcode, add_points))

# Merge Ateco 2007 - NACE rev.2 concordance table
confiscated_companies <- merge(
  x = confiscated_companies,
  y = ateco_nace_table,
  by = "ateco_nace_subcode",
  all.x = TRUE,
)

# $reordering columns
col_order <- c(
  "name", "province", "activity_status","ateco_nace_code","ateco_description", 
  "nace_rev2_code", "category", "english_nace_description", "vat_number",
  "fiscal_code", "rea_piva_cf", "n_rea", "rea", "business_name", 
  "legal_nature_code", "legal_nature_description", "ateco_subcode_description", 
  "registration_province","registration_number","confiscation_status", 
  "italian_nace_description", "ateco_nace_subcode", "notes"
    )

confiscated_companies <-  confiscated_companies[, col_order]
