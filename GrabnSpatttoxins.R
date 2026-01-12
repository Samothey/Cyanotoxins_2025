
################################################

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)

main_toxin <- "~/Desktop/Project/Brooks_lake_2025/DATA/Toxins/Pena_2025.xlsx"


# ---- helper: add derived site fields ----
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

# ---- 1) Whole water results ----
grab_result <- read_excel(main_toxin, sheet = "WholeWater") %>%
  clean_names() %>%
  rename(date = sample_date) %>%            # adjust if your date column is named differently
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    method = "grab",
    event_id = paste(site_id, date, sep = "_")
  ) %>%
  add_site_fields()

# ---- 2) SPATT results (only recovered, analyzed + shipped bags will be here) ----
spatt_results <- read_excel(main_toxin, sheet = "SPATT") %>%
  clean_names() %>%
  rename(date = sample_date) %>%            # adjust if your date column is named differently
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    method = "spatt",
    event_id = paste(site_id, date, sep = "_")
  ) %>%
  add_site_fields()


sample_status <- read_excel(main_toxin, sheet = "Sample Status") %>%
  clean_names() %>%
  rename(date = sample_date) %>%
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    status = str_to_lower(status),
    method = str_to_lower(method),
    event_id = paste(site_id, date, sep = "_")
  ) %>%
  add_site_fields()


####
# ---- A) sample_status (from your sheet) ----
sample_status <- read_excel(main_toxin, sheet = "Sample Status") %>%
  clean_names() %>%
  rename(date = sample_date) %>%
  mutate(
    date = as.Date(date),
    sample_type = str_to_lower(sample_type),
    status = str_to_lower(status),
    method = str_to_lower(method),
    event_id = paste(site_id, date, sep = "_")
  ) %>%
  add_site_fields()

# ---- B) build "has results" keys ----
grab_keys <- grab_result %>%
  distinct(site_id, date, sample_type, method) %>%
  mutate(has_grab_results = TRUE)

spatt_keys <- spatt_results %>%
  distinct(site_id, date, sample_type, method) %>%
  mutate(has_spatt_results = TRUE)

# ---- C) apply rules (analyzed overrides; shipped + no results -> missing) ----
sample_status_ruled <- sample_status %>%
  left_join(grab_keys,  by = c("site_id", "date", "sample_type", "method")) %>%
  left_join(spatt_keys, by = c("site_id", "date", "sample_type", "method")) %>%
  mutate(
    has_grab_results  = coalesce(has_grab_results, FALSE),
    has_spatt_results = coalesce(has_spatt_results, FALSE),
    status_final = case_when(
      method == "grab"  & has_grab_results  ~ "analyzed",
      method == "spatt" & has_spatt_results ~ "analyzed",
      status == "shipped" & method == "grab"  & !has_grab_results  ~ "missing",
      status == "shipped" & method == "spatt" & !has_spatt_results ~ "missing",
      TRUE ~ status
    )
  )

# ---- D) Make 1-row-per-key results tables (prevents many-to-many) ----
grab_one <- grab_result %>%
  group_by(site_id, date, sample_type, method) %>%
  summarise(across(everything(), dplyr::first), .groups = "drop")

spatt_one <- spatt_results %>%
  group_by(site_id, date, sample_type, method) %>%
  summarise(across(everything(), dplyr::first), .groups = "drop")

results_all <- bind_rows(grab_one, spatt_one)

# ---- E) Join toxin values onto status table ----
sample_with_toxins <- sample_status_ruled %>%
  left_join(
    results_all,
    by = c("site_id", "date", "sample_type", "method"),
    suffix = c("", "_results")
  )

# ---- F) Identify toxin columns and blank them out unless analyzed ----
meta_cols <- c(
  "sample_number", "site_id", "date", "sample_type", "status", "status_final",
  "method", "event_id", "lake", "site_type", "depth_category",
  "has_grab_results", "has_spatt_results"
)

toxin_cols <- sample_with_toxins %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(meta_cols)



# Optional: inspect which columns are being treated as toxins
toxin_cols

sample_with_toxins <- sample_with_toxins %>%
  mutate(
    across(all_of(toxin_cols),
           ~ if_else(status_final == "analyzed", .x, NA_real_))
  )


# status summary
sample_with_toxins %>% count(method, status_final, sort = TRUE)

# make sure analyzed rows actually have toxin values
sample_with_toxins %>%
  summarise(
    n_analyzed = sum(status_final == "analyzed", na.rm = TRUE),
    n_analyzed_with_any_value = sum(status_final == "analyzed" &
                                      if_any(all_of(toxin_cols), ~ !is.na(.x)), na.rm = TRUE)
  )



## --- Prep results for plotting ---
results_plot <- results_all %>%
  add_site_fields() %>%
  mutate(
    sample_type = str_to_lower(sample_type),
    lake = str_to_lower(lake),
    site_type = str_to_lower(site_type)
  ) %>%
  filter(
    sample_type == "regular",          # remove blanks + duplicates
    !is.na(lake), lake != "blank",
    !is.na(site_type)
  )

## --- Identify toxin columns robustly ---
meta_cols <- c(
  "sample_number", "site_id", "date", "sample_type", "method",
  "event_id", "lake", "site_type", "depth_category"
)

toxin_cols <- results_plot %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(meta_cols)

# optional sanity check
toxin_cols

## --- Long format for ggplot ---
toxins_long <- results_plot %>%
  pivot_longer(
    cols = all_of(toxin_cols),
    names_to = "congener",
    values_to = "value"
  )

## --- Plot: Shore vs Buoy by lake ---
ggplot(toxins_long,
       aes(x = date, y = value, color = site_type)) +
  geom_point() +
  geom_line(aes(group = site_id), alpha = 0.4) +
  facet_grid(congener ~ lake, scales = "free_y") +
  labs(x = "Date", y = "Concentration", title = "Shore vs buoy toxin concentrations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


grab_plot <- results_all %>%
  add_site_fields() %>%
  mutate(
    sample_type = str_to_lower(sample_type),
    lake = str_to_lower(lake),
    site_type = str_to_lower(site_type)
  ) %>%
  filter(
    method == "grab",
    sample_type == "regular",
    !is.na(lake), lake != "blank",
    !is.na(site_type)
  )

meta_cols <- c("sample_number","site_id","date","sample_type","method",
               "event_id","lake","site_type","depth_category")

toxin_cols <- grab_plot %>%
  select(where(is.numeric)) %>%
  names() %>%
  setdiff(meta_cols)

grab_long <- grab_plot %>%
  pivot_longer(
    cols = all_of(toxin_cols),
    names_to = "congener",
    values_to = "value"
  )


lake_to_plot <- "rainbow"   # change to "upper brooks", "rainbow", "lower jade", "boysen"

ggplot(grab_long %>% filter(lake == lake_to_plot),
       aes(x = date, y = value, color = site_type)) +
  geom_point() +
  geom_line(aes(group = site_id), alpha = 0.4) +
  facet_wrap(~ congener, scales = "free_y") +
  labs(
    x = "Date", y = "Concentration",
    title = paste("GRAB samples —", str_to_title(lake_to_plot), ": shore vs buoy")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(grab_long %>% filter(lake == lake_to_plot, site_type == "buoy"),
       aes(x = date, y = value, color = depth_category)) +
  geom_point() +
  geom_line(aes(group = site_id), alpha = 0.4) +
  facet_wrap(~ congener, scales = "free_y") +
  labs(
    x = "Date", y = "Concentration",
    title = paste("GRAB buoy samples —", str_to_title(lake_to_plot), ": surface vs bottom")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



