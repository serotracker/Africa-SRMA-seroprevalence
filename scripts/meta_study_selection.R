# meta study selection
remotes::install_github("chrismerkord/epical")
global_seroprev_adj <- dataset1 %>%
  left_join(dataset1_adj %>% select(study_name, denominator_value, adj_prevalence)) %>%
  #filter(!is.na(adj_prevalence)) %>%
  mutate(serum_pos_prevalence=if_else(!is.na(adj_prevalence), adj_prevalence, serum_pos_prevalence),
         denominator_value = as.integer(denominator_value),
         numerator_value = as.integer(round(serum_pos_prevalence*denominator_value,0))) %>%
  mutate(week=sampling_mid_date)
global_seroprev_wk <- dataset1 %>%
  mutate(denominator_value = as.integer(denominator_value),
         numerator_value = as.integer(round(serum_pos_prevalence*denominator_value,0))) %>%
  mutate(week=sampling_mid_date)

sero_complete <- epical::add_epi_week(global_seroprev_wk, "week", system = "cdc") %>%
  mutate(week = epical::epi_week_date(epi_week, epi_year, offset = 0, system = "cdc"))
sero_adj <- epical::add_epi_week(global_seroprev_adj, "week", system = "cdc") %>%
  mutate(week = epical::epi_week_date(epi_week, epi_year, offset = 0, system = "cdc"))
sero_nat <- sero_complete

# seroprevalence adjustment
sero_inf <- sero_nat %>% 
  group_by(study_name) %>%
  mutate(serum_pos_prevalence = max((serum_pos_prevalence-adults_vaccinated_per_hundred14/100)/
                                    (1-adults_vaccinated_per_hundred14/100),
                                          0),
         numerator_value = round(serum_pos_prevalence*denominator_value,0)) %>%
  ungroup()
saveRDS(sero_inf, "data/analysis/final/sero_inf.rds")

sero_nat <- filter(sero_nat, sampling_mid_date<=as.Date("2021-10-01"))
sero_inf <- filter(sero_inf, sampling_mid_date<=as.Date("2021-10-01"))
sero_adj <- filter(sero_adj, sampling_mid_date<=as.Date("2021-10-01"))


