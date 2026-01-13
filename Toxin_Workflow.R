
################################################

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)


main_toxin <- ("~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025/Pena_2025.xlsx")

## exclude extra samples 
exclude_site_patterns <- c("^BKS_UB_NCOVE_")  

filter_extras <- function(df) {
  df %>%
    filter(!if_any(site_id, ~ str_detect(.x, exclude_site_patterns)))
}

#  derived site fields, this function will ensure the site IDs are correlated with the correct lakes, site type, and depth)
add_site_fields <- function(df) {
  df %>%
    mutate(
      lake = case_when(
        str_detect(site_id, "^BKS_BL_") ~ "brooks",
        str_detect(site_id, "^BKS_UB_") ~ "upper brooks",
        str_detect(site_id, "^BKS_RN_") ~ "rainbow",
        str_detect(site_id, "^BKS_LJ_") ~ "lower jade",
        str_detect(site_id, "^BYS_")    ~ "boysen",
        str_detect(site_id, "^GRAB_")   ~ "blank",
        str_detect(site_id, "^SPATT_")   ~ "blank",
        TRUE ~ NA_character_
      ),
      site_type = case_when(
        str_detect(site_id, "_SH_") ~ "shore",
        str_detect(site_id, "_BU_") ~ "buoy",
        TRUE ~ NA_character_
      ),
      depth_category = case_when(
        str_detect(site_id, "_SS$") ~ "surface",
        str_detect(site_id, "_DD$") ~ "bottom",
        TRUE ~ NA_character_
      )
    )
}


# ---- Sheet 1: Whole water results ----
grab_result <- read_excel(main_toxin, sheet = "WholeWater") %>%
  clean_names() %>%
  mutate(sample_number = str_pad(as.character(sample_number), width = 3, pad = "0")) %>%
  rename(date = sample_date) %>%            
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    method = "grab"
  ) %>%
  filter_extras() %>%    
  add_site_fields()

# ---- Sheet 2: SPATT results (only recovered, analyzed + shipped bags will be here) ----
spatt_result <- read_excel(main_toxin, sheet = "SPATT") %>%
  clean_names() %>%
  mutate(sample_number = str_pad(as.character(sample_number), width = 3, pad = "0")) %>%
  rename(date = sample_date) %>%            
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    method = "spatt"
  ) %>%
  filter_extras() %>%    
  add_site_fields()

#------- Sheet 3: All sample status( samples could either be: collected, shipped, lost)
sample_status <- read_excel(main_toxin, sheet = "Sample Status") %>%
  clean_names() %>%
  mutate(sample_number = str_pad(as.character(sample_number), width = 3, pad = "0")) %>%
  rename(date = sample_date) %>%
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    status = str_to_lower(status),
    method = str_to_lower(method)
  ) %>%
  filter_extras() %>%    
  add_site_fields()


#######################################################################################
############### Metadata for samples: building a master sheet##############
######################################################################################

#----- A) 
sample_status <- read_excel(main_toxin, sheet = "Sample Status") %>%
clean_names() %>%
  mutate(sample_number = str_pad(as.character(sample_number), width = 3, pad = "0")) %>%
  rename(date = sample_date) %>%
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    status = str_to_lower(status),
    method = str_to_lower(method)
  ) %>%
  filter_extras() %>%    
  add_site_fields()

# ---- B) Joining the results "keys", this doesnt take the toxin result, it only creates "keys" for if the sample was analyzed
grab_keys <- grab_result %>%
  distinct(sample_number, site_id, date, sample_type, method) %>%
  mutate(has_grab_result = TRUE)

spatt_keys <- spatt_result %>%
  distinct(sample_number, site_id, date, sample_type, method) %>%
  mutate(has_spatt_result = TRUE)

# ---- C) apply rules, this will help us stay organized and understand which samples (analyzed overrides; shipped + no results -> missing)  ----
sample_status_Final <- sample_status %>%
  left_join(grab_keys,  by = c("sample_number", "site_id", "date", "sample_type", "method")) %>%
  left_join(spatt_keys, by = c("sample_number", "site_id", "date", "sample_type", "method")) %>%
  mutate(
    has_grab_result  = coalesce(has_grab_result, FALSE),
    has_spatt_result = coalesce(has_spatt_result, FALSE),
    status_final = case_when(
      method == "grab"  & has_grab_result  ~ "analyzed",
      method == "spatt" & has_spatt_result ~ "analyzed",
      status == "shipped" & method == "grab"  & !has_grab_result  ~ "missing",
      status == "shipped" & method == "spatt" & !has_spatt_result ~ "missing",
      TRUE ~ status
    )
  )


## Summary of what we have 
status_summary_key <- sample_status_Final %>%
  mutate(
    method = str_to_lower(method),
    status_final = str_to_lower(status_final)
  ) %>%
  filter(status_final %in% c("missing", "lost", "analyzed")) %>%
  count(method, status_final) %>%
  pivot_wider(names_from = status_final, values_from = n, values_fill = 0)

status_summary_key
## I dont know why one grab sample didnt transfer over correctly, but it was analyzed?..


## save it!
saveRDS(sample_status_Final,
        "~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025/master_sample_tracking.rds")

write_csv(sample_status_Final,
          "~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025/master_sample_tracking.rds")


#### Toxin results table wide format- all (including blanks and dups)#####
toxin_results_wide_all <- bind_rows(grab_result, spatt_result) %>%
  mutate(
    method = str_to_lower(method),
    sample_type = str_to_lower(sample_type)
  )


#Save it 
write_csv(toxin_results_wide_all,
          "~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025/toxin_results_wide_all.csv")

saveRDS(toxin_results_wide_all,
         "~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025/toxin_results_wide_all.rds")

####################################################################################################
##  create tables with toxin results- Toxin results long (regular samples only, no dups or blanks)

# identify toxin columns from the combined wide table
meta_cols <- c("sample_number","site_id","date","sample_type","method",
               "lake","site_type","depth_category")

toxin_cols <- toxin_results_wide_all %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(meta_cols)

toxin_results_long_regular <- toxin_results_wide_all %>%
  filter(sample_type == "regular", !lake %in% c("blank", "duplicate"), !is.na(lake)) %>%
  pivot_longer(
    cols = all_of(toxin_cols),
    names_to = "congener",
    values_to = "value"
  )
#save
write_csv(toxin_results_long_regular,
          "~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025toxin_results_long_regular.csv")
saveRDS(toxin_results_long_regular,
        "~/Desktop/Project/Brooks_lake_2025/Cyanotoxins_2025/toxin_results_long_regular.rds")

## plotting the tables

# 1) Identify toxin (numeric) columns from wide table
meta_cols <- c("sample_number","site_id","date","sample_type","method",
               "lake","site_type","depth_category")

toxin_cols <- toxin_results_wide_all %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(meta_cols)

# 2) Filter to real samples and pivot long
toxin_results_long_regular <- toxin_results_wide_all %>%
  mutate(
    method = str_to_lower(method),
    sample_type = str_squish(str_to_lower(sample_type)),
    lake = str_to_lower(lake),
    site_type = str_to_lower(site_type)
  ) %>%
  filter(
    sample_type == "regular",
    !is.na(lake), lake != "blank"
  ) %>%
  pivot_longer(
    cols = all_of(toxin_cols),
    names_to = "congener",
    values_to = "value"
  ) %>%
  mutate(
    congener = str_to_lower(congener)
  )
## 2.) create toxin- class key + join it

toxin_key <- tribble(
  ~congener, ~toxin_class,
  "dm_lr", "microcystin",
  "la",   "microcystin",
  "lf",   "microcystin",
  "lr",   "microcystin",
  "ly",   "microcystin",
  "rr",   "microcystin",
  "wr",   "microcystin",
  "yr",   "microcystin",
  
  "atx",  "anatoxin",
  "hatx", "anatoxin",
  
  "cyl",  "cylindrospermopsin",
  "nod",  "nodularin"
)

toxin_plot <- toxin_results_long_regular %>%
  left_join(toxin_key, by = "congener")

toxin_plot %>% filter(is.na(toxin_class)) %>% distinct(congener)

# GRAB totals by toxin class 
grab_class_totals <- toxin_plot %>%
  filter(method == "grab") %>%
  group_by(
    sample_number, site_id, date, lake,
    site_type, depth_category, method, toxin_class
  ) %>%
  summarise(
    total = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

## SPATT presence/absence by toxin class 
spatt_presence <- toxin_plot %>%
  filter(method == "spatt") %>%
  group_by(
    sample_number, site_id, date, lake,
    site_type, depth_category, method, toxin_class
  ) %>%
  summarise(
    detected = any(value > 0, na.rm = TRUE),
    .groups = "drop"
  )

 ######### PLOTS ###########################
###################################

# plot 1: GRAB: shore vs buoy within each toxin class (faceted by lake)
grab_class_totals_focus <- grab_class_totals %>%
  filter(toxin_class %in% c("microcystin", "anatoxin"))


ggplot(grab_class_totals,
       aes(x = site_type, y = total)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  facet_grid(toxin_class ~ lake, scales = "free_y") +
  scale_y_continuous(trans = "log1p") +
  labs(
    x = "Site type",
    y = "Total toxin (sum of congeners; log1p)",
    title = "GRAB samples: shore vs buoy by toxin class"
  ) +
  theme(legend.position = "none")

toxin_plot %>%
  summarise(
    n_neg = sum(value < 0, na.rm = TRUE),
    min_val = min(value, na.rm = TRUE)
  )

toxin_plot %>%
  filter(value < 0) %>%
  select(sample_number, site_id, date, method, congener, value) %>%
  arrange(value) %>%
  head(30)
toxin_cols


#core questions are:
# Shore vs buoy — are toxin concentrations different?
 #  Surface vs bottom (buoy) — is there vertical structure?
 #  How does this vary by lake?
 #  How do toxin classes behave differently?
  # How does GRAB compare to SPATT? 


## 




  


   