# Female DM Project - Panel Construction (with wide panel)

# Clear environment
rm(list = ls())

# Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Load raw files
panel_2011 <- read_csv("2011_panel_clean.csv")
panel_2001 <- read_csv("2001_panel_clean.csv")
ias_analysed_70 <- read_csv("ias_analysed_merged_70.csv")
ias_analysed_80 <- read_csv("ias_analysed_merged_80.csv")
ias_analysed_90 <- read_csv("ias_analysed_merged_90.csv")

# Preparing wide panel with IAS measures
panel_2001 <- panel_2001 %>% 
  rename(district_name = name)

long_panel_merged <- left_join(panel_2001, panel_2011, 
                               by = "district_name")

#Correcting spelling mismatches in IAS_70 dataset 
ias_analysed_70 <- ias_analysed_70 %>%
  mutate(office = case_when(
    office == "beed" ~ "bid",
    office == "gondia" ~ "gondiya",
    office == "kasargod" ~ "kasaragod",
    office == "palakkad/palghat" ~ "palakkad",
    office == "raigad / raigarh (mh)" ~ "raigarh",
    office == "thiruvananthapuram / trivandrum" ~ "thiruvananthapuram",
    office == "thrissoor" ~ "thrissur",
    TRUE ~ office
  )) 

ias_analysed_70 <- ias_analysed_70 %>% 
  rename(district_name = office)

panel_with_ias_70 <- left_join(long_panel_merged, ias_analysed_70, 
                               by = "district_name")
panel_with_ias_70 <- panel_with_ias_70 %>% 
  drop_na()

#Correcting spelling mistakes with IAS_80 dataset 

ias_analysed_80 <- ias_analysed_80 %>% 
  mutate(office = case_when(
    office == "beed" ~ "bid",
    office == "gondia" ~ "gondiya",
    office == "kasargod" ~ "kasaragod",
    office == "palakkad/palghat" ~ "palakkad",
    office == "raigad / raigarh (mh)" ~ "raigarh",
    office == "thiruvananthapuram / trivandrum" ~ "thiruvananthapuram",
    office == "thrissoor" ~ "thrissur",
    TRUE ~ office
  )) 

ias_analysed_80 <- ias_analysed_80 %>%
  rename(district_name = office)

panel_with_ias_80 <- left_join(long_panel_merged, ias_analysed_80, 
                                by = "district_name")
panel_with_ias_80 <- panel_with_ias_80 %>% 
  drop_na()

#Correcting spelling mistakes in IAS_90 dataset 

ias_analysed_90 <- ias_analysed_90 %>% 
  mutate(office = case_when(
    office == "beed" ~ "bid",
    office == "gondia" ~ "gondiya",
    office == "kasargod" ~ "kasaragod",
    office == "palakkad/palghat" ~ "palakkad",
    office == "raigad / raigarh (mh)" ~ "raigarh",
    office == "thiruvananthapuram / trivandrum" ~ "thiruvananthapuram",
    office == "thrissoor" ~ "thrissur",
    TRUE ~ office
  )) 
ias_analysed_90 <- ias_analysed_90 %>% 
  rename(district_name = office)

panel_with_ias_90 <- left_join(long_panel_merged, ias_analysed_90, 
                               by = "district_name")
panel_with_ias_90 <- panel_with_ias_90 %>% 
  drop_na()

#Exporting datasets
write_csv(panel_with_ias_70, "ias_panel_wide_70.csv")
write_csv(panel_with_ias_80, "ias_panel_wide_80.csv")
write_csv(panel_with_ias_90, "ias_panel_wide_90.csv")






