# # Load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(forcats)

library(raster)
library(sf)
library(geodata)
library(giscoR)

# # Load dataset(s) ---------------------------------------------------------

confiscated_companies <- read_excel(
  "data/confiscated_companies.xlsx",
  na = c("", "N/A", "non disponibile", "nd", "n.d.", "n.d", "N.D.")
)

ateco_nace_table <- read_excel("data/ateco_nace_table.xlsx")
ateco_nace_macro_table <-
  read_excel("data/ateco_nace_macro_table.xlsx")

total_companies <-
  read_excel("data/total_limited_companies_province.xls")

total_companies$province <- toupper(total_companies$province)
total_companies$region <- toupper(total_companies$region)

# # Dataset tidying ---------------------------------------------------------

colnames(confiscated_companies)[3]  = "vat_number"
colnames(confiscated_companies)[5]  = "province_code"
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
colnames(confiscated_companies)[19] = "confiscation_status_description"


# Merging the two province variables
confiscated_companies <- confiscated_companies |> 
  mutate(province_code = coalesce(province_code, registration_province))

# Merging the province code with the name
confiscated_companies <- left_join(confiscated_companies,
                                   total_companies,
                                   by = "province_code")

# Put company name in uppercase to better check for duplicates
confiscated_companies <- confiscated_companies |>
  mutate(name = (toupper(name))) |>
  mutate(province = (toupper(province)))


# Create a variable with the date of the confiscation (or annulment) procedure
# lubridate package is used to convert dates to yyyy-mm-dd format
confiscated_companies <- confiscated_companies |>
  mutate(date_confiscation_procedure = lubridate::dmy(
    str_sub(confiscation_status_description,-10,-1))
    )

# Create a variable that takes the values: "definitive",
# "first degree", "annulled" or "other".
confiscated_companies <- confiscated_companies  |>
  mutate(
    confiscation_status = case_when(
      str_detect(confiscation_status_description,
                 pattern = "(?i)definitiva|(?i)secondo") ~ "definitive",
      str_detect(confiscation_status_description,
                 pattern = "(?i)primo") ~ "first degree",
      str_detect(confiscation_status_description,
                 pattern = "(?i)revoca") ~ "annulled",
      str_detect(confiscation_status_description,
                 pattern = "(?i)r") ~ "other"
    )
  )


# # Standardize the rea observations for querying in the ORBIS database--------


# Define a function to process each identifier
process_identifier <- function(id) {
  if (is.na(id)) {
    return(NA)
  }
  
  # Remove the hyphen
  no_hyphen <- str_replace(id, "-", "")
  
  # Extract the prefix and the numeric part of the identifier
  prefix <- str_extract(no_hyphen, "\\D+")
  numeric_part <- str_extract(no_hyphen, "\\d+")
  
  # Pad the numeric part with zeros to ensure it has at least 7 digits
  padded_numeric_part <- str_pad(numeric_part, width = 7, pad = "0")
  
  # Combine the prefix with the padded numeric part
  formatted_id <- paste0(prefix, padded_numeric_part)
  
  return(formatted_id)
}

# Apply the processing function to each element of the 'rea' column
new_rea <- sapply(confiscated_companies$rea, process_identifier)

# Create a new column in 'confiscated_companies' to store the formatted identifiers
confiscated_companies$new_rea <- new_rea



# Standardize ateco_nace codes and description ----------------------------

# Add a point every 2 digits except for the last one
add_points <- function(code) {
  gsub("(.{2})(?!$)", "\\1.", code, perl = TRUE)
}

confiscated_companies <- confiscated_companies |>
  mutate(ateco_nace_subcode = sapply(ateco_nace_subcode, add_points))

# Merge Ateco 2007 - NACE rev.2 concordance table
confiscated_companies <- merge(x = confiscated_companies,
                               y = ateco_nace_table,
                               by = "ateco_nace_subcode",
                               all.x = TRUE,)


# Merge NACE codes macro-description

confiscated_companies <- merge(x = confiscated_companies,
                               y = ateco_nace_macro_table,
                               by = "ateco_nace_code",
                               all.x = TRUE,)

# Reordering columns
col_order <- c(
  "name",
  "category",
  "province_code",
  "province",
  "region",
  "activity_status",
  "confiscation_status",
  "date_confiscation_procedure",
  "ateco_nace_code",
  "english_nace_macro_description",
  "nace_rev2_code",
  "ateco_nace_subcode",
  "english_nace_description",
  "italian_nace_description",
  "ateco_subcode_description",
  "vat_number",
  "fiscal_code",
  "rea_piva_cf",
  "new_rea",
  "rea",
  "business_name",
  "legal_nature_code",
  "legal_nature_description",
  "registration_province",
  "registration_number",
  "confiscation_status_description",
  "ateco_nace_subcode",
  "notes"
)

confiscated_companies <-  confiscated_companies[, col_order]




# Check for and delete duplicates --------------------------------------------

cols_to_check <- c("name", "vat_number")

#sort from most recent to least recent
confiscated_companies <- confiscated_companies |>
  arrange(ymd(confiscated_companies$date_confiscation_procedure))

confiscated_companies <- confiscated_companies |>
  mutate(duplicated_vat_rea = (
    duplicated(vat_number) & 
    duplicated(rea) & 
    (!is.na(vat_number) | 
    !is.na(rea))
    )
    )


confiscated_companies <- confiscated_companies[
  !duplicated(confiscated_companies[, cols_to_check]),]

confiscated_companies <- confiscated_companies |>
  filter(!duplicated_vat_rea)



# Distribution of firms by sector of activity - unfiltered ----------------

grouped_data <- confiscated_companies |>
  group_by(english_nace_macro_description) |>
  summarise(count = n()) |>
  arrange(desc(count))


plot_sector <-
  ggplot(data = subset(grouped_data, !is.na(english_nace_macro_description)),
         aes(
           x = reorder(english_nace_macro_description, count),
           y = count
         )) +
  geom_bar(
    stat = "identity",
    fill = "darkblue",
    color = "black",
    width = 0.75
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("") +
  ylab("Number of firms") +
  coord_flip() +
  theme_minimal() + # Lighter background theme
theme(
  axis.text = element_text(size = 10, vjust = 0.7),
  axis.ticks.y = element_blank(),
  panel.grid.major.x = element_blank(),
  # Remove horizontal grid lines
  axis.line.y = element_blank()
)
plot(plot_sector)
ggsave(
  "sector_of_activity_unfiltered.png",
  path = "figures/",
  dpi = 3600,
  bg = NULL
)


# Distribution of companies legal forms -----------------------------------

confiscated_companies$company_type <- ifelse(
  confiscated_companies$category %in% c("SPA", "SRL", "SAPA", "SC", "SCRL", "CONSORZIO"),
  "Limited Liability",
  "Partnership"
)

# Rename the observations  for better visualization in the plot
confiscated_companies <- confiscated_companies |>
  mutate(
    legal_nature_description = case_when(
      category == "ASSOC" ~ "Association",
      category == "Altro" ~ "Others",
      category == "CONSORZIO" ~ "Consortium",
      category == "INDIV" ~ "Sole Proprietorship",
      category == "SAPA" ~ "Limited Partnership by Shares - SAPA",
      category == "SAS" ~ "Limited Partnership - SAS",
      category == "SC" ~ "Cooperative Company - SC",
      category == "SCRL" ~ "Cooperative Company with Limited Liability - SCRL",
      category == "SNC" ~ "General Partnership - SNC",
      category == "SPA" ~ "Joint Stock Company - SPA",
      category == "SRL" ~ "Limited Liability Company - SRL",
      category == "SS" ~ "Simple Partnership - SS",
      category == "SdF" ~ "De facto corporation",
      TRUE ~ legal_nature_description
    )
  )

categories <- confiscated_companies |>
  group_by(legal_nature_description, company_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # arrange in decreasing order of count

total_count <- sum(categories$count)

categories <- categories |>
  mutate(percentage = (count / total_count) * 100)


# Create the bar plot

plot_legal_form <-
  ggplot(data = categories,
         aes(
           x = reorder(legal_nature_description, count),
           y = count,
           fill = company_type
         )) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(x = element_blank(),
       y = element_blank()) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),) +
  
  scale_fill_manual(values = c(
    "Limited Liability" = "darkred",
    "Partnership" = "navy"
  ))

plot_legal_form + geom_text(
  aes(label = sprintf("%.1f%%", percentage)),
  position = position_dodge(width = 0.9),
  hjust = -0.1,
  size = 4,
  fontface = "bold"
)




# Filtering companies -----------------------------------------------------


confiscated_companies <- confiscated_companies |>
  filter(
    confiscation_status == "definitive" |
      confiscation_status == "first degree",
    company_type == "Limited Liability"
  )


# Data visualization ---------------------------------------------------

## - Intensity map ----

###  - * Province level ----


# Get the map of Italy at the province level (year = 2021, as there is
# correspondence with the province level in the dataset)
italy_provinces <- gisco_get_nuts(
  year = "2021",
  epsg = "3035",
  resolution = "01",
  country = "italy",
  nuts_level = "3"
)

# Counting the number of limited liabilities companies per province
province_count <- confiscated_companies |>
  group_by(province) |>
  summarise(count = n())

# Convert and put in uppercase the name to ease the merge
names(italy_provinces)[names(italy_provinces) == 'NAME_LATN'] <-
  'province'

italy_provinces$province <- toupper(italy_provinces$province)

merged_data_province <-
  left_join(italy_provinces, province_count, by = "province")

merged_data_province$count[is.na(merged_data_province$count)] <- 0

merged_data_province <-
  left_join(merged_data_province, total_companies, by = "province")

# Create a variable to compute the intensity of infiltration
merged_data_province <- merged_data_province |>
  mutate(share_infiltrated = (count / total_limited_companies) * 1000)


# Add classes
merged_data_province$share_infiltrated_category <-
  cut(
    merged_data_province$share_infiltrated,
    breaks = c(-Inf, 0.1, 2, 4, 10, 20),
    labels = c('none', '0.1 - 2', '2 - 4', '4 - 10', '10 - 20')
  )

# Define custom colors
custom_colors <-
  c("white", "mistyrose", "rosybrown2", "indianred", "darkred")

# Create the choropleth map
plot_province <-
  ggplot(data = merged_data_province) +
  geom_sf(aes(fill = share_infiltrated_category)) +
  # geom_sf_text(aes(label = province_code),
  # size = 0.7, fontface="bold", check_overlap = TRUE) +
  scale_fill_manual(
    values = custom_colors,
    na.value = "grey90",
    name = "Infiltred companies every 1,000",
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank()
  )


# Display the plot and save
plot_province
ggsave(
  "infiltration_density_province.png",
  path = "figures/",
  dpi = 2400,
  bg = NULL
)


###  - * Region level ----

italy_regions <- gisco_get_nuts(
  year = "2021",
  epsg = "3035",
  resolution = "01",
  country = "italy",
  nuts_level = "2"
)

# Counting the number of limited liabilities companies per region
region_count <- confiscated_companies |>
  group_by(region) |>
  summarise(count = n())

# Convert and put in uppercase the name to ease the merge
names(italy_regions)[names(italy_regions) == 'NAME_LATN'] <-
  'region'

italy_regions$region <- toupper(italy_regions$region)

merged_data_region <-
  left_join(italy_regions, region_count, by = "region")

merged_data_region$count[is.na(merged_data_region$count)] <- 0

merged_data_region <-
  left_join(merged_data_region, total_companies, by = "region")

merged_data_region <- merged_data_region %>%
  group_by(region) %>%
  mutate(region_total_limited_companies = sum(total_limited_companies)) %>%
  ungroup()

# Create a variable to compute the intensity of infiltration
merged_data_region <- merged_data_region |>
  mutate(share_infiltrated = (count / region_total_limited_companies) *
           1000)

# Change a few observation for improved visualization in the plot
merged_data_region <- merged_data_region |>
  mutate(
    NUTS_NAME = case_when(
      NUTS_NAME == "Provincia Autonoma di Bolzano/Bozen" ~ "Bolzano",
      NUTS_NAME == "Provincia Autonoma di Trento" ~ "Trento",
      NUTS_NAME == "Valle d’Aosta/Vallée d’Aoste" ~ "Valle d'Aosta",
      TRUE ~ NUTS_NAME
    )
  )

# Custom classes
merged_data_region$share_infiltrated_category <- cut(
  merged_data_region$share_infiltrated,
  breaks = c(0, 0.5, 2, 5, 10),
  labels = c("0 - 0.5", "0.5 - 2", "2 - 5", "5 - 10"),
  include.lowest = TRUE
)

# Define custom colors (progressing from light to dark red)
custom_colors_region <-
  c("snow", "mistyrose", "indianred", "darkred")

# Create the choropleth map for regions
plot_region <-
  ggplot(data = merged_data_region) +
  geom_sf(aes(fill = share_infiltrated_category)) +
  geom_sf_text(
    aes(label = NUTS_NAME),
    size = 1.5,
    fontface = "bold",
    check_overlap = TRUE
    
  ) +
  scale_fill_manual(
    values = custom_colors_region,
    na.value = "grey90",
    name = "Infiltred companies every 1,000",
    guide = guide_legend(reverse = TRUE),
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank()
  )

# Display the plot and save
plot_region
ggsave(
  "infiltration_density_region.png",
  path = "figures/",
  dpi = 2400,
  bg = NULL
)


## - Distribution by NACE code ----
grouped_data <- confiscated_companies |>
  group_by(english_nace_macro_description) |>
  summarise(count = n()) |>
  arrange(desc(count))


plot_sector <-
  ggplot(data = subset(grouped_data, !is.na(english_nace_macro_description)),
         aes(
           x = reorder(english_nace_macro_description, count),
           y = count
         )) +
  geom_bar(
    stat = "identity",
    fill = "darkblue",
    color = "black",
    width = 0.75
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  xlab("") +
  ylab("Number of firms") +
  coord_flip() +
  theme_minimal() + # Lighter background theme
theme(
  axis.text = element_text(size = 10, vjust = 0.7),
  axis.ticks.y = element_blank(),
  panel.grid.major.x = element_blank(),
  # Remove horizontal grid lines
  axis.line.y = element_blank()
)
plot(plot_sector)
ggsave(
  "sector_of_activity.png",
  path = "figures/",
  dpi = 1200,
  bg = NULL
)

