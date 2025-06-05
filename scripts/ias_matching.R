#Female DM - Proportion Compilation
#Clearing RStudio
rm(list = ls())

#Loading and calling required libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(stringdist)
library(readr)
library(tidyr)

#Getting working directory
setwd("/Users/rishabhbijani/Desktop/female_dm_combined")

#Loading raw datasets
ias_experience_raw <- read_csv("ias-experience.csv")
ias_profile_raw    <- read_csv("ias-profile.csv")

#Cleaning raw datasets
ias_experience_cleaned <- ias_experience_raw %>%
  rename_with(tolower) %>%  # First lowercase all column names
  mutate(
    name = str_to_lower(name),
    cadre = str_to_lower(cadre),
    designation = str_to_lower(designation),
    level = str_to_lower(level),
    office = str_to_lower(office),
    organisation = str_to_lower(organisation),
    field_of_experience = str_to_lower(field_of_experience),
    category_of_experience = str_to_lower(category_of_experience),
    start_date = parse_date_time(start_date, orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY")),
    end_date   = parse_date_time(end_date, orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY")),
    name = str_trim(gsub("^(ms\\.|mr\\.|mrs\\.|shri|smt\\.|dr\\.|dr\\.\\(ms\\.\\))\\s*", "", name, ignore.case = TRUE)),
    office = str_replace_all(office, regex("allepey", ignore_case = TRUE), "alappuzha")
  )

#Filtering by cutoff dates
start_cutoff <- as.Date("2001-07-01")
end_cutoff   <- as.Date("2011-07-01")

ias_experience_filtered_base <- ias_experience_cleaned %>%
  filter((start_date >= start_cutoff & start_date <= end_cutoff) |
           (end_date   >= start_cutoff & end_date   <= end_cutoff)) %>%
  filter(!organisation %in% c("centre", "centre (foreign posting)")) %>%
  filter(!str_detect(designation, regex("administrative member|administrator|chief administrator", ignore_case = TRUE)))

#Defining DM designation list
dm_designations_list <- c(
  "district magistrate", "district collector", "additional district magistrate", "deputy commissioner",
  "spl deputy commissioner", "commissioner (land mgmt)", "collector to cm", "district officer",
  "additional collector", "district dev officer", "collector & district magistrate", "assistant collector",
  "deputy commissioner cum district magistrate", "spl collector", "magistrate", "assistant commissioner",
  "additional deputy collector", "deputy commissioner (west)", "dm", "dc & dm",
  "district collector and development commissioner", "sub collector", "deputy magistrate", "additional magistrate",
  "d.m.", "additional collector & ceo", "deputy commissioner / d m", "additional collector (ulc)",
  "district magistrate-cum-deputy commissioner", "deputy commissioner (south west)", "deputy collector",
  "executive magistrate", "dm and md", "assistant collector and md", "assistant commissioner (ut)",
  "assistant collector & assistant magistrate", "first l a collector", "joint magistrate", "assistant magistrate",
  "assistant commissioner ( dev)", "assistant collector (trg)", "assistant collector & a d m",
  "additional district magistrate & collector", "d.c.", "commissioner-cum-director",
  "assistant collector & executive magistrate (under training)", "deputy commissioner (south)",
  "assistant commissioner (dev) cum b d o", "jt colletor & additional. districtt. magistrate",
  "deputy. coll. & sdm", "deputy commissioner / sdm", "deputy. coll. / sdm", "ac (ut)with dc"
)

designation_pattern <- regex(paste0(dm_designations_list, collapse = "|"), ignore_case = TRUE)

#Defining target categories
target_categories <- c(
  "land revenue mgmt & district admn",
  "personnel and general administration",
  "local self govt",
  "rural dev",
  "public administration"
)

#Regular Expressions - Matching
ias_experience_regex <- ias_experience_filtered_base %>%
  filter(str_detect(designation, designation_pattern)) %>%
  filter(category_of_experience %in% target_categories)

#Fuzzy Matching
get_min_dist <- function(desig, reference_list) {
  min(stringdist::stringdist(desig, reference_list, method = "jw"))
}

ias_experience_fuzzy <- ias_experience_filtered_base %>%
  mutate(min_dist = sapply(designation, get_min_dist, reference_list = dm_designations_list)) %>%
  filter(min_dist <= 0.15) %>%
  filter(category_of_experience %in% target_categories) %>%
  select(-min_dist)

#Excluding Non-DM designations
excluded_designations <- c(
  "land commissioner", "member (judicial)", "additional deputy commissioner", "member (admin)",
  "additional collector", "magistrate", "assistant commissioner", "additional deputy collector",
  "director (administration)", "commissioner (land mgmt)", "deputy magistrate", "additional magistrate",
  "additional collector (dev)", "additional collector & ceo", "sub collector", "jt educational adviser",
  "additional collector (ulc)", "deputy collector", "assistant collector",
  "additional collector development and project officer", "assistant commissioner (ut)", "admn officer",
  "sdo/sdm", "assistant collector & assistant magistrate", "joint magistrate", "assistant magistrate",
  "assistant commissioner ( dev)", "supernumerary assistant collector", "sr assistant commissioner",
  "additional district magistrate & collector", "jt colletor & additional. districtt. magistrate",
  "assistant collector (trg)", "deputy. coll. & sdm", "assistant collector & a d m",
  "assistant commissioner (dev) & b d o", "assistant collector & executive magistrate (under training)",
  "deputy. coll. / sdm"
)

exclusion_pattern <- regex(paste0(excluded_designations, collapse = "|"), ignore_case = TRUE)

ias_experience_regex <- ias_experience_regex %>%
  filter(!str_detect(designation, exclusion_pattern))

ias_experience_fuzzy <- ias_experience_fuzzy %>%
  filter(!str_detect(designation, exclusion_pattern))

ias_experience_regex <- ias_experience_regex %>%
  mutate(office = str_replace_all(
    office,
    regex("alle?pp?e?y", ignore_case = TRUE),
    "alappuzha"))

ias_experience_fuzzy <- ias_experience_fuzzy %>%
  mutate(office = str_replace_all(
    office,
    regex("alle?pp?e?y", ignore_case = TRUE),
    "alappuzha"))

#Building time series for kerela
ias_experience_kerala_regex <- ias_experience_regex %>%
  filter(cadre == "kerala", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

ias_experience_kerala_fuzzy <- ias_experience_fuzzy %>%
  filter(cadre == "kerala", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

#Summarizing total days fro which service records are available
service_records_kerala_regex <- ias_experience_kerala_regex %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_regex = sum(days_in_office, na.rm = TRUE), .groups = "drop")

service_records_kerala_fuzzy <- ias_experience_kerala_fuzzy %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_fuzzy = sum(days_in_office, na.rm = TRUE), .groups = "drop")

#Arranging service records by number of days
service_records_kerala_regex <- service_records_kerala_regex %>%
  arrange(desc(no_of_days_service_records_regex))

service_records_kerala_fuzzy <- service_records_kerala_fuzzy %>%
  arrange(desc(no_of_days_service_records_fuzzy))

kerala_districts_90 <- service_records_kerala_regex %>%
  filter(no_of_days_service_records_regex >= 3286.8)
kerala_districts_80 <- service_records_kerala_regex %>%
  filter(no_of_days_service_records_regex >= 2921.6)
kerala_dsitricts_70 <- service_records_kerala_regex %>%
  filter(no_of_days_service_records_regex >= 2556.4)

#Building time series for Andhra Pradesh
ias_experience_andhra_pradesh_regex <- ias_experience_regex %>%
  filter(cadre == "andhra pradesh", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

ias_experience_andhra_pradesh_fuzzy <- ias_experience_fuzzy %>%
  filter(cadre == "andhra pradesh", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

#Summarizing total days for which service records are available
service_records_andhra_pradesh_regex <- ias_experience_andhra_pradesh_regex %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_regex = sum(days_in_office, na.rm = TRUE), .groups = "drop")

service_records_andhra_pradesh_fuzzy <- ias_experience_andhra_pradesh_fuzzy %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_fuzzy = sum(days_in_office, na.rm = TRUE), .groups = "drop")

#Arranging service records by number of days
service_records_andhra_pradesh_regex <- service_records_andhra_pradesh_regex %>%
  arrange(desc(no_of_days_service_records_regex))

service_records_andhra_pradesh_fuzzy <- service_records_andhra_pradesh_fuzzy %>%
  arrange(desc(no_of_days_service_records_fuzzy))

andhra_pradesh_districts_90 <- service_records_andhra_pradesh_fuzzy %>%
  filter(no_of_days_service_records_fuzzy >= 3286.8)
andhra_pradesh_districts_80 <- service_records_andhra_pradesh_fuzzy %>%
  filter(no_of_days_service_records_fuzzy >= 2921.6)
andhra_pradesh_districts_70 <- service_records_andhra_pradesh_fuzzy %>%
  filter(no_of_days_service_records_fuzzy >= 2556.4)

#Generating service records for odisha
ias_experience_odisha_regex <- ias_experience_regex %>%
  filter(cadre == "odisha", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

ias_experience_odisha_fuzzy <- ias_experience_fuzzy %>%
  filter(cadre == "odisha", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

#Summarizing total days f0r which service records are available
service_records_odisha_regex <- ias_experience_odisha_regex %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_regex = sum(days_in_office, na.rm = TRUE), .groups = "drop")

service_records_odisha_fuzzy <- ias_experience_odisha_fuzzy %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_fuzzy = sum(days_in_office, na.rm = TRUE), .groups = "drop")

#Arranging service records by number of days
service_records_odisha_regex <- service_records_odisha_regex %>%
  arrange(desc(no_of_days_service_records_regex))

service_records_odisha_fuzzy <- service_records_odisha_fuzzy %>%
  arrange(desc(no_of_days_service_records_fuzzy))

#Generating service records for Goa (by REGEX)

ias_experience_goa_regex <- ias_experience_regex %>%
  filter(
    cadre == "a g m u t",
    office %in% c("north goa", "south goa"),
    !is.na(start_date),
    !is.na(end_date)
  ) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

ias_experience_goa_fuzzy <- ias_experience_fuzzy %>%
  filter(
    cadre == "a g m u t",
    office %in% c("north goa", "south goa"),
    !is.na(start_date),
    !is.na(end_date)
  ) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

# Summarizing total days of service (REGEX)

service_records_goa_regex <- ias_experience_goa_regex %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_regex = sum(days_in_office, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(no_of_days_service_records_regex))

service_records_goa_fuzzy <- ias_experience_goa_fuzzy %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_fuzzy = sum(days_in_office, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(no_of_days_service_records_fuzzy))

#Generating service records for Maharashtra
ias_experience_maharashtra_regex <- ias_experience_regex %>%
  filter(cadre == "maharashtra", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

ias_experience_maharashtra_fuzzy <- ias_experience_fuzzy %>%
  filter(cadre == "maharashtra", !is.na(office), !is.na(start_date), !is.na(end_date)) %>%
  mutate(
    days_in_office = as.numeric(difftime(end_date, start_date, units = "days"))
  )

#Summarizing total days for which service records are available
service_records_maharashtra_regex <- ias_experience_maharashtra_regex %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_regex = sum(days_in_office, na.rm = TRUE), .groups = "drop")

service_records_maharashtra_fuzzy <- ias_experience_maharashtra_fuzzy %>%
  group_by(office) %>%
  summarize(no_of_days_service_records_fuzzy = sum(days_in_office, na.rm = TRUE), .groups = "drop")

#Arranging service records by number of days
service_records_maharashtra_regex <- service_records_maharashtra_regex %>%
  arrange(desc(no_of_days_service_records_regex))

service_records_maharashtra_fuzzy <- service_records_maharashtra_fuzzy %>%
  arrange(desc(no_of_days_service_records_fuzzy))

maharashtra_districts_90 <- service_records_maharashtra_fuzzy %>%
  filter(no_of_days_service_records_fuzzy >= 3286.8)
maharashtra_districts_80 <- service_records_maharashtra_fuzzy %>%
  filter(no_of_days_service_records_fuzzy >= 2921.6)
maharashtra_districts_70 <- service_records_maharashtra_fuzzy %>%
  filter(no_of_days_service_records_fuzzy >= 2556.4)

#Cleaning service records
maharashtra_districts_90 <- maharashtra_districts_90 %>%
  filter(!(office %in% c(
    "land revenue & disaster mgmt deptt",
    "land revenue deptt"
  )))
print(unique(maharashtra_districts_80$office))
maharashtra_districts_80 <- maharashtra_districts_80 %>%
  filter(!(office %in% c(
    "land revenue & disaster mgmt deptt",
    "land revenue deptt",
    "land acquisition deptt"
  )))
print(unique(maharashtra_districts_70$office))
maharashtra_districts_70 <- maharashtra_districts_70 %>%
  filter(!(office %in% c(
    "land revenue & disaster mgmt deptt",
    "land revenue deptt",
    "land acquisition deptt"
  )))

print(unique(andhra_pradesh_districts_90$office))
print(unique(andhra_pradesh_districts_80$office))
andhra_pradesh_districts_80 <- andhra_pradesh_districts_80 %>%
  filter(!(office %in% c(
    "panchayat & rural dev deptt",
    "rural dev deptt"
  )))
print(unique(andhra_pradesh_districts_70$office))
andhra_pradesh_districts_70 <- andhra_pradesh_districts_70 %>%
  filter(!(office %in% c(
    "panchayat & rural dev deptt",
    "rural dev deptt",
    "land admin deptt"
  )))
print(unique(kerala_districts_90$office))
print(unique(kerala_districts_80$office))
print(unique(kerala_dsitricts_70$office))

#Cleaning IAS Profile
ias_profile_raw <- ias_profile_raw %>%
  select("ID",
         "Name",
         "Cadre",
         "Gender",
         "Source_of_Recruitment",
         "Place_of_Domicile",
         "Mother_Tongue",
         "Languages_Known")
str(ias_profile_raw)
ias_profile_cleaned <- ias_profile_raw %>%
  rename_with(tolower) %>%
  mutate(
    name = str_to_lower(name),
    cadre = str_to_lower(cadre),
    gender = str_to_lower(gender),
    source_of_recruitment = str_to_lower(source_of_recruitment),
    place_of_domicile = str_to_lower(place_of_domicile),
    mother_tongue = str_to_lower(mother_tongue),
    languages_known = str_to_lower(languages_known),
    name = str_trim(gsub(
      "^(ms\\.|mr\\.|mrs\\.|shri|smt\\.|dr\\.|dr\\.\\(ms\\.\\))\\s*",
      "", name, ignore.case = TRUE
    ))
  )
#Filtering ias_experience regex
ias_experience_regex_analysis_70 <- ias_experience_regex %>%
  filter(office %in% c(maharashtra_districts_70$office,
                       kerala_dsitricts_70$office,
                       andhra_pradesh_districts_70$office))
ias_analysis_merged_70 <-left_join(ias_experience_regex_analysis_70, ias_profile_cleaned,
                                   by = "id")

ias_analysis_merged_70 <- ias_analysis_merged_70 %>%
  mutate(
    days_of_service = as.numeric(difftime(end_date, start_date, units = "days"))
  ) %>%
  group_by(office) %>%
  summarize(
    total_recorded_days = sum(days_of_service, na.rm = TRUE),
    total_days_of_service_female = sum(ifelse(gender == "female", days_of_service, 0), na.rm = TRUE),
    total_days_of_service_embedded = sum(ifelse(cadre.x == place_of_domicile, days_of_service, 0), na.rm = TRUE),
    prop_service_female = total_days_of_service_female / total_recorded_days,
    prop_service_embedded = total_days_of_service_embedded / total_recorded_days,
    .groups = "drop"
  )
write_csv(ias_analysis_merged_70, "ias_analysed_merged_70.csv")

ias_analysis_merged_names_70 <- ias_analysis_merged_70$office
ias_analysis_merged_names_df_70 <- data.frame(ias_analysis_merged_names_70)
write_csv(ias_analysis_merged_names_df_70, "ias_analysed_merged_names_70.csv")

#Creating 80% and 90% match districts as well
ias_experience_regex_analysis_80 <- ias_experience_regex %>%
  filter(office %in% c(maharashtra_districts_80$office,
                       kerala_districts_80$office,
                       andhra_pradesh_districts_80$office))
ias_analysis_merged_80 <-left_join(ias_experience_regex_analysis_80, ias_profile_cleaned,
                                   by = "id")

ias_analysis_merged_80 <- ias_analysis_merged_80 %>%
  mutate(
    days_of_service = as.numeric(difftime(end_date, start_date, units = "days"))
  ) %>%
  group_by(office) %>%
  summarize(
    total_recorded_days = sum(days_of_service, na.rm = TRUE),
    total_days_of_service_female = sum(ifelse(gender == "female", days_of_service, 0), na.rm = TRUE),
    total_days_of_service_embedded = sum(ifelse(cadre.x == place_of_domicile, days_of_service, 0), na.rm = TRUE),
    prop_service_female = total_days_of_service_female / total_recorded_days,
    prop_service_embedded = total_days_of_service_embedded / total_recorded_days,
    .groups = "drop"
  )
write_csv(ias_analysis_merged_80, "ias_analysed_merged_80.csv")

#Creating 90% match districts as well
ias_experience_regex_analysis_90 <- ias_experience_regex %>%
  filter(office %in% c(maharashtra_districts_90$office,
                       kerala_districts_90$office,
                       andhra_pradesh_districts_90$office))
ias_analysis_merged_90 <-left_join(ias_experience_regex_analysis_90, ias_profile_cleaned,
                                   by = "id")

ias_analysis_merged_90 <- ias_analysis_merged_90 %>%
  mutate(
    days_of_service = as.numeric(difftime(end_date, start_date, units = "days"))
  ) %>%
  group_by(office) %>%
  summarize(
    total_recorded_days = sum(days_of_service, na.rm = TRUE),
    total_days_of_service_female = sum(ifelse(gender == "female", days_of_service, 0), na.rm = TRUE),
    total_days_of_service_embedded = sum(ifelse(cadre.x == place_of_domicile, days_of_service, 0), na.rm = TRUE),
    prop_service_female = total_days_of_service_female / total_recorded_days,
    prop_service_embedded = total_days_of_service_embedded / total_recorded_days,
    .groups = "drop"
  )
write_csv(ias_analysis_merged_90, "ias_analysed_merged_90.csv")
