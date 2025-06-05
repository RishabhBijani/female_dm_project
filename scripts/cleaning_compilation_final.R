#Female DM Project - Cleaning and Compilation of District Panels

#Clearing RStudio
rm(list=ls())

#Calling libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(rvest)
library(stringr)

#Setting working directory
getwd()
setwd("/Users/rishabhbijani/Desktop/female_dm_combined")

#Loading SHRUG datasets and keys
village_directory <- read_csv("pc11_vd_clean_shrid.csv")
location_names <- read_csv("shrid_loc_names.csv")
pca <- read_csv("pc11_pca_clean_shrid.csv")
district_keys <- read_csv("dist_key.csv")
state_keys <- read_csv("state_key.csv")
rural_keys <- read_csv("pc11r_shrid_key.csv")
updated_coi_11 <- read_csv("updated_coi_wide_1.csv")

#Cleaning and filtering rural keys
str(rural_keys)

#Converting characters to numbers
rural_keys$pc11_state_id <- as.numeric(rural_keys$pc11_state_id)
rural_keys$pc11_district_id <- as.numeric(rural_keys$pc11_district_id)
rural_keys$pc11_subdistrict_id <- as.numeric(rural_keys$pc11_subdistrict_id)
rural_keys$pc11_village_id <- as.numeric(rural_keys$pc11_village_id)

#Creating master key by merging rural keys with location name
master_key <- left_join(rural_keys, location_names, by = "shrid2")
#Filtering out master key
master_key <- master_key %>%
  filter(pc11_state_id %in% c(21,27,28,30,32))

#Filtering village directory to include rows in master key
master_key_shrid2 <- master_key$shrid2
village_directory <- village_directory %>%
  filter(shrid2 %in% master_key_shrid2)
str(village_directory)
village_directory <- village_directory %>%
  select(shrid2,
         pc11_vd_p_sch_gov,
         pc11_vd_p_sch_priv,
         pc11_vd_m_sch_gov,
         pc11_vd_m_sch_priv, 
         pc11_vd_s_sch_gov,
         pc11_vd_s_sch_priv,
         pc11_vd_ch_cntr,
         pc11_vd_ph_cntr,
         pc11_vd_all_hosp,
         pc11_vd_disp,
         pc11_vd_t_p,
         pc11_vd_t_m,
         pc11_vd_t_f,
         pc11_vd_sc_p,
         pc11_vd_sc_m,
         pc11_vd_sc_f,
         pc11_vd_st_p,
         pc11_vd_st_m,
         pc11_vd_st_f,
         pc11_vd_mcw_cntr,
         pc11_vd_area)

#Merging village directory with master_key
master_dataset <- left_join(village_directory, master_key, by = "shrid2")

#Creating indicator variables in master_dataset
indicator_vars <- c(
  "pc11_vd_p_sch_gov", "pc11_vd_m_sch_gov", "pc11_vd_s_sch_gov",
  "pc11_vd_ch_cntr", "pc11_vd_ph_cntr", "pc11_vd_all_hosp", "pc11_vd_disp",
  "pc11_vd_t_p", "pc11_vd_t_m", "pc11_vd_t_f",
  "pc11_vd_sc_p", "pc11_vd_sc_m", "pc11_vd_sc_f",
  "pc11_vd_st_p", "pc11_vd_st_m", "pc11_vd_st_f",
  "pc11_vd_mcw_cntr", "pc11_vd_area", "pc11_vd_p_sch_priv", "pc11_vd_m_sch_priv", "pc11_vd_s_sch_priv")
master_dataset <- master_dataset %>%
  mutate(across(
    all_of(indicator_vars),
    ~ as.numeric(. != 0),
    .names = "ind_{.col}"))
str(master_dataset)

#Aggregating at the district level
master_dataset_district_level_11 <- master_dataset %>%
  mutate(
    ind_pc11_vd_p_sch_any = if_else(pc11_vd_p_sch_gov > 0 | pc11_vd_p_sch_priv > 0, 1, 0),
    ind_pc11_vd_m_sch_any = if_else(pc11_vd_m_sch_gov > 0 | pc11_vd_m_sch_priv > 0, 1, 0),
    ind_pc11_vd_s_sch_any = if_else(pc11_vd_s_sch_gov > 0 | pc11_vd_s_sch_priv > 0, 1, 0)
  ) %>%
  group_by(pc11_district_id, district_name, state_name) %>%
  summarize(
    no_vill_p_sch_11 = sum(ind_pc11_vd_p_sch_any),
    no_vill_m_sch_11 = sum(ind_pc11_vd_m_sch_any),
    no_vill_s_sch_11 = sum(ind_pc11_vd_s_sch_any),
    no_vill_ch_cntr_11   = sum(ind_pc11_vd_ch_cntr),
    no_vill_ph_cntr_11   = sum(ind_pc11_vd_ph_cntr),
    no_vill_all_hosp_11  = sum(ind_pc11_vd_all_hosp),
    no_vill_disp_11      = sum(ind_pc11_vd_disp),
    no_vill_mcw_cntr_11  = sum(ind_pc11_vd_mcw_cntr),
    vill_t_p_11          = sum(pc11_vd_t_p, na.rm = TRUE),
    vill_t_m_11          = sum(pc11_vd_t_m, na.rm = TRUE),
    vill_t_f_11          = sum(pc11_vd_t_f, na.rm = TRUE),
    vill_sc_p_11         = sum(pc11_vd_sc_p, na.rm = TRUE),
    vill_sc_m_11         = sum(pc11_vd_sc_m, na.rm = TRUE),
    vill_sc_f_11         = sum(pc11_vd_sc_f, na.rm = TRUE),
    vill_st_p_11         = sum(pc11_vd_st_p, na.rm = TRUE),
    vill_st_m_11         = sum(pc11_vd_st_m, na.rm = TRUE),
    vill_st_f_11         = sum(pc11_vd_st_f, na.rm = TRUE),
    tot_pc11_land_area   = sum(pc11_land_area, na.rm = TRUE),
    .groups = "drop"
  )


districts_master_dataset <- master_dataset_district_level_11$pc11_district_id

#Merging pca with rural keys to assign IDs
pca_merged <- left_join(pca, rural_keys, by = "shrid2")

#Keeping only relevant variables in pca_merged
pca_merged <- pca_merged %>%
  select(shrid2,
         pc11_pca_tot_p.x,
         pc11_pca_p_sc,
         pc11_pca_p_st,
         pc11_pca_tot_m,
         pc11_pca_tot_f,
         pc11_pca_m_sc,
         pc11_pca_f_sc,
         pc11_pca_m_st,
         pc11_pca_f_st,
         pc11_state_id,
         pc11_district_id,
         pc11_land_area)
pca_merged <- pca_merged %>%
  rename(pc11_pca_tot_p = pc11_pca_tot_p.x)

#Filtering PCA
pca_merged <- pca_merged %>%
  filter(pc11_district_id %in% districts_master_dataset)
pca_merged_district_11 <- pca_merged %>%
  group_by(pc11_district_id) %>%
  summarize(pc11_pca_dist_tot_p = sum(pc11_pca_tot_p),
            pc11_pca_dist_p_sc = sum(pc11_pca_p_sc),
            pc11_pca_dist_p_st = sum(pc11_pca_p_st),
            pc11_pca_dist_tot_m = sum(pc11_pca_tot_m),
            pc11_pca_dist_tot_f = sum(pc11_pca_tot_f),
            pc11_pca_dist_m_sc = sum(pc11_pca_m_sc),
            pc11_pca_dist_f_sc = sum(pc11_pca_f_sc),
            pc11_pca_dist_m_st = sum(pc11_pca_m_st),
            pc11_pca_dist_f_st = sum(pc11_pca_f_st))

#Creating super dataset by merging pca_merged and master_dataset
super_dataset_11 <- left_join(master_dataset_district_level_11, pca_merged_district_11 , by = "pc11_district_id")

#Adding column of number of villages per district
villages_per_district <- rural_keys %>%
  count(pc11_district_id, name = "no_of_villages")
super_dataset_11 <- left_join(super_dataset_11, villages_per_district, by = "pc11_district_id")
super_dataset_11 <- super_dataset_11 %>%
  rename(no_of_villages_11 = no_of_villages)
str(super_dataset_11)

#Inspecting CoI table
coi_variables_of_interest_11 <- updated_coi_11 %>%
  select(dist_code,
         no_vill_inhabited_total_coi_11,
         area_total_coi_11,
         area_rural_coi_11,
         pop_p_rural_coi_11,
         pop_p_total_coi_11)
coi_variables_of_interest_11 <- coi_variables_of_interest_11 %>%
  rename(pc11_district_id = dist_code)

#Merging coi_variables of interest and super_dataset
super_dataset_11 <- left_join(super_dataset_11, coi_variables_of_interest_11, by = "pc11_district_id")
str(super_dataset_11)

dataset_for_analysis <- super_dataset_11 %>%
  mutate(
    prop_vill_p_sch_11 = no_vill_p_sch_11 / no_of_villages_11,
    prop_vill_m_sch_11 = no_vill_m_sch_11 / no_of_villages_11,
    prop_vill_s_sch_11 = no_vill_s_sch_11 / no_of_villages_11,
    prop_vill_ch_cntr_11   = no_vill_ch_cntr_11 / no_of_villages_11,
    prop_vill_ph_cntr_11   = no_vill_ph_cntr_11 / no_of_villages_11,
    prop_vill_all_hosp_11  = no_vill_all_hosp_11 / no_of_villages_11,
    prop_vill_disp_11      = no_vill_disp_11 / no_of_villages_11,
    prop_vill_mcw_cntr_11  = no_vill_mcw_cntr_11 / no_of_villages_11,
    avg_vill_t_p_11        = vill_t_p_11 / no_of_villages_11,
    prop_rural_pop_11      = pc11_pca_dist_tot_p / pop_p_total_coi_11,
    prop_rural_area_11     = area_rural_coi_11 / area_total_coi_11,
    prop_vill_sc_pop_11    = vill_sc_p_11 / vill_t_p_11,
    prop_vill_st_pop_11    = vill_st_p_11 / vill_t_p_11
  ) %>%
  select(pc11_district_id, district_name, state_name,
         starts_with("prop_"),
         avg_vill_t_p_11, 
         area_rural_coi_11, 
         area_rural_coi_11, 
         no_of_villages_11)

write_csv(dataset_for_analysis, "dataset_for_analysis_11_new.csv")

#Female 2001 DM Project

#Loading SHRUG datasets and keys
village_directory_01 <- read_csv("pc01_vd_clean_shrid.csv")
location_names_01 <- read_csv("shrid_loc_names.csv")
pca_01 <- read_csv("pc01_pca_clean_shrid.csv")
rural_keys_01 <- read_csv("pc01r_shrid_key.csv")
coi_01 <- read_csv("coi_01_table_01.csv")

#Cleaning and filtering rural keys
str(rural_keys_01)

rural_keys_01 <- rural_keys_01 %>%
  mutate(
    pc01_state_id = as.numeric(pc01_state_id),
    pc01_district_id = as.numeric(pc01_district_id),
    pc01_subdistrict_id = as.numeric(pc01_subdistrict_id),
    pc01_village_id = as.numeric(pc01_village_id))
master_key_01 <- left_join(rural_keys_01, location_names_01, by = "shrid2")

#Filtering out master keys
master_key_01 <- master_key_01 %>%
  filter(state_name %in% c("odisha",
                           "maharashtra",
                           "andhra pradesh",
                           "goa",
                           "kerala"))

#Filtering out village directory to include rows in master key
master_key_01_shrid2 <- master_key_01$shrid2
village_directory_01 <- village_directory_01 %>%
  filter(shrid2 %in% master_key_01_shrid2)
str(village_directory_01)

village_directory_01 <- village_directory_01 %>%
  select(shrid2,
         pc01_vd_p_sch,
         pc01_vd_m_sch,
         pc01_vd_s_sch,
         pc01_vd_ph_cntr,
         pc01_vd_all_hosp,
         pc01_vd_disp_cntr,
         pc01_vd_t_p,
         pc01_vd_t_m,
         pc01_vd_t_f,
         pc01_vd_sc_p,
         pc01_vd_sc_m,
         pc01_vd_sc_f,
         pc01_vd_st_p,
         pc01_vd_st_m,
         pc01_vd_st_f,
         pc01_vd_mcw_cntr,
         pc01_vd_area)

#Merging village directory with master key
master_dataset_01 <- left_join(village_directory_01, master_key_01, by = "shrid2")

#Creating indicator variables in the dataset
master_dataset_01 <- master_dataset_01 %>%
  mutate(across(
    .cols = c(
      pc01_vd_p_sch, pc01_vd_m_sch, pc01_vd_s_sch,
      pc01_vd_ph_cntr, pc01_vd_all_hosp, pc01_vd_disp_cntr,
      pc01_vd_t_p, pc01_vd_t_m, pc01_vd_t_f,
      pc01_vd_sc_p, pc01_vd_sc_m, pc01_vd_sc_f,
      pc01_vd_st_p, pc01_vd_st_m, pc01_vd_st_f,
      pc01_vd_mcw_cntr
    ),
    .fns = ~ ifelse(. > 0, 1, 0),
    .names = "{.col}_indicator"
  ))

#Checking uniqueness of district IDs
master_dataset_01 %>%
  distinct(pc01_state_id, pc01_district_id) %>%
  count(pc01_district_id, name = "n_states") %>%
  arrange(desc(n_states))

#Creating unique district IDs
master_dataset_01 <- master_dataset_01 %>%
  mutate(
    pc01_district_id_unique = paste0(
      str_pad(pc01_state_id, 2, pad = "0"), "-",
      str_pad(pc01_district_id, 3, pad = "0")
    )
  )
str(master_dataset_01)

#Aggregating to the district level
master_dataset_district_level_01 <- master_dataset_01 %>%
  group_by(pc01_district_id_unique) %>%
  summarize(no_vill_p_sch_01 = sum(pc01_vd_p_sch_indicator),
            no_vill_m_sch_01 = sum(pc01_vd_m_sch_indicator),
            no_vill_s_sch_01 = sum(pc01_vd_s_sch_indicator),
            no_vill_ph_cntr_01 = sum(pc01_vd_ph_cntr_indicator),
            no_vill_all_hosp_01 = sum(pc01_vd_all_hosp_indicator),
            no_vill_disp_01 = sum(pc01_vd_disp_cntr_indicator),
            no_vill_mcw_01 = sum(pc01_vd_mcw_cntr_indicator),
            vill_t_p_01 = sum(pc01_vd_t_p),
            vill_t_m_01 = sum(pc01_vd_t_m),
            vill_t_f_01 = sum(pc01_vd_t_f),
            vill_sc_p_01 = sum(pc01_vd_sc_p),
            vill_sc_m_01 = sum(pc01_vd_sc_m),
            vill_st_p_01 = sum(pc01_vd_st_p),
            vill_st_m_01 = sum(pc01_vd_st_m),
            vill_st_f_01 = sum(pc01_vd_st_f),
            tot_pc01_land_area = sum(pc01_land_area))

district_master_dataset_01 <- master_dataset_district_level_01$pc01_district_id_unique

#Merging pca with rural keys to assign IDs
pca_merged_01 <- left_join(pca_01, rural_keys_01, by = "shrid2")

#Keeping only relevant variables in pca_merged_01
pca_merged_01 <- pca_merged_01 %>%
  select(shrid2,
         pc01_pca_tot_p.x,
         pc01_pca_p_sc,
         pc01_pca_p_st,
         pc01_pca_tot_m,
         pc01_pca_tot_f,
         pc01_pca_m_sc,
         pc01_pca_f_sc,
         pc01_pca_m_st,
         pc01_pca_f_st,
         pc01_district_id,
         pc01_state_id)
pca_merged_01 <- pca_merged_01 %>%
  rename(pc01_pca_tot_p = pc01_pca_tot_p.x)


#Filtering PCA
str(pca_merged_01)
#Creating unique district ID in pca_merged_01
pca_merged_01 <- pca_merged_01 %>%
  mutate(
    pc01_district_id_unique = paste0(
      str_pad(pc01_state_id, 2, pad = "0"), "-",
      str_pad(pc01_district_id, 3, pad = "0")))
str(pca_merged_01)

pca_merged_01 <- pca_merged_01 %>%
  filter(pc01_district_id_unique %in% district_master_dataset_01)

pca_merged_district_01 <- pca_merged_01 %>%
  group_by(pc01_district_id_unique) %>%
  summarize(pc01_pca_dist_tot_p = sum(pc01_pca_tot_p),
            pc01_pca_dist_p_sc = sum(pc01_pca_p_sc),
            pc01_pca_dist_p_st = sum(pc01_pca_p_st),
            pc01_pca_dist_tot_m = sum(pc01_pca_tot_m),
            pc01_pca_dist_tot_f = sum(pc01_pca_tot_f),
            pc01_pca_dist_m_sc = sum(pc01_pca_m_sc),
            pc01_pca_dist_f_sc = sum(pc01_pca_f_sc),
            pc01_pca_dist_m_st = sum(pc01_pca_m_st),
            pc01_pca_dist_f_st = sum(pc01_pca_f_st))  

#Creating superdataset by merging pca_merged and master dataset
super_dataset_01 <- left_join(master_dataset_district_level_01, pca_merged_district_01,
                              by = "pc01_district_id_unique")

#Inspecting CoI Table
str(coi_01)
coi_01 <- coi_01 %>%
  mutate(name = str_to_lower(name))
str(coi_01)
coi_01 <- coi_01 %>%
  filter(!name %in% c("orissa", "maharashtra", "andhra pradesh", "goa", "kerala"))

#Keeping only variables of interest
str(coi_01)
coi_01 <- coi_01 %>%
  select(name,
         no_villages_inhabited_rural_01,
         pop_total_total_01,
         pop_total_rural_01,
         area_total_01,
         area_rural_01,
         pc01_state_id,
         pc01_district_id)
str(coi_01)

#Creating unique district ID in coi_01
coi_01 <- coi_01 %>%
  mutate(pc01_district_id = as.numeric(pc01_district_id))
coi_01 <- coi_01 %>%
  mutate(
    pc01_district_id_unique = paste0(
      str_pad(pc01_state_id, 2, pad = "0"), "-",
      str_pad(pc01_district_id, 3, pad = "0")))

#Creating datatset for analysis
super_dataset_01_new <- left_join(super_dataset_01, coi_01 %>%
                                    select(pc01_district_id_unique, name, everything()),
                                  by = "pc01_district_id_unique")
str(super_dataset_01_new)

dataset_for_analysis_01 <- super_dataset_01_new %>%
  mutate(prop_vill_p_sch_01 = no_vill_p_sch_01/no_villages_inhabited_rural_01,
         prop_vill_m_sch_01 = no_vill_m_sch_01/no_villages_inhabited_rural_01,
         prop_vill_s_sch_01 = no_vill_s_sch_01/no_villages_inhabited_rural_01,
         prop_vill_ph_cntr_01 = no_vill_ph_cntr_01/no_villages_inhabited_rural_01,
         prop_vill_all_hosp_01 = no_vill_all_hosp_01/no_villages_inhabited_rural_01,
         prop_vill_disp_01 = no_vill_disp_01/no_villages_inhabited_rural_01,
         prop_vill_mcw_cntr_01 = no_vill_mcw_01/ no_villages_inhabited_rural_01,
         avg_vill_t_p_01 = vill_t_p_01/no_villages_inhabited_rural_01,
         prop_rural_pop_01 = pc01_pca_dist_tot_p/pop_total_total_01,
         prop_rural_area_01 = area_rural_01/ area_total_01,
         prop_vill_sc_pop_01 = vill_sc_p_01/vill_t_p_01,
         prop_vill_st_pop_01 = vill_st_p_01/vill_t_p_01) %>%
  select(
    pc01_district_id_unique,
    name,
    prop_vill_p_sch_01,
    prop_vill_m_sch_01,
    prop_vill_s_sch_01,
    prop_vill_ph_cntr_01,
    prop_vill_all_hosp_01,
    prop_vill_disp_01,
    prop_vill_mcw_cntr_01,
    avg_vill_t_p_01,
    prop_rural_pop_01,
    prop_rural_area_01,
    prop_vill_sc_pop_01,
    prop_vill_st_pop_01, 
    area_total_01, 
    area_rural_01, 
    no_villages_inhabited_rural_01)
write_csv(dataset_for_analysis_01, "dataset_for_analysis_01_new.csv")

#Data Analysis _ District-Level Panels

#Clearing RStudio
rm(list=ls())

#Loading files
dataset_for_analysis_01 <- read_csv("dataset_for_analysis_01_new.csv")
dataset_for_analysis_11 <- read_csv("dataset_for_analysis_11_new.csv")

#Filtering districts in 2011 panel
districts_vector_01 <- dataset_for_analysis_01$name
dataset_for_analysis_11 <- dataset_for_analysis_11 %>%
  filter(district_name %in% districts_vector_01)

districts_vector_11 <- dataset_for_analysis_11$district_name
dataset_for_analysis_01 <- dataset_for_analysis_01 %>%
  filter(name %in% districts_vector_11)

#Checking for similarity of panels
print(colnames(dataset_for_analysis_01))
print(colnames(dataset_for_analysis_11))

dataset_for_analysis_11 <- dataset_for_analysis_11 %>% 
  select(-c(prop_vill_ch_cntr_11, state_name))

write_csv(dataset_for_analysis_11, "2011_panel_clean.csv")
write_csv(dataset_for_analysis_01, "2001_panel_clean.csv")
