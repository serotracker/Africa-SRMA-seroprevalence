library(tidyverse)
library(zoo)
library(broom)
library(gridExtra)
library(countrycode)
library(metafor)
library(meta)
devtools::install_github("chrismerkord/epical")
library(epical)
library(lubridate)
library(cowplot)
library(kableExtra)
devtools::install_github("gavinsimpson/schoenberg")
library("schoenberg")
library(mgcv)

script_path = "scripts/"
fig_path = "figures/"

date_var="week"
random_est_nat <- meta_analysis(sero=sero_nat,model="sero_nat",window_span=28,date_var=date_var)
random_est_adj <- meta_analysis(sero=sero_adj,model="sero_adj",window_span=28,date_var=date_var)
random_est_inf <- meta_analysis(sero=sero_inf,model="sero_inf",window_span=28, date_var=date_var)

random_est <- bind_rows(
  random_est_adj,
  random_est_nat,
  random_est_inf
)

saveRDS(random_est, glue::glue(data_path,"ma over time/random_est_raw.rds"))

random_est <- readRDS(glue::glue(data_path,"ma over time/random_est_raw.rds"))
vaccination <- readRDS("data/epi/vaccination_numbers.rds") %>%
  select(-total) %>%
  group_by(country_iso3) %>%
  arrange(country_iso3, date) %>%
  fill(people_vaccinated) %>%
  ungroup() %>%
  filter(date<=as.Date("2021-07-18")) %>%
  mutate_at(vars(people_vaccinated), ~replace(., is.na(.), 0))

global_population_counts <- readRDS("data/pop/global_population_counts.rds")
ts_global_cases <- who_dths_cases %>% left_join(global_population_counts%>%filter(agegrp=="all")) %>%
  filter(country_iso3!="TKM") %>% # drop Turkmenistan never has any recorded cases?!
  # create new WHO regions by income class
  mutate(who_region = factor(who_region, 
                             levels = c("Europe: Low and middle income", "Europe: High income",
                                        "Africa", "Eastern Mediterranean",
                                        "South-East Asia", "Western Pacific", 
                                        "Americas: Low and middle income", "Americas: High income"))) %>%
  add_epi_week("date", system = "cdc") %>%
  group_by(who_region, country_iso3, epi_year, epi_week) %>%
  summarise(across(c(total_cases_per_hundred9,total_cases9,total_deaths, total), ~last(na.omit(.x)))) %>%
  ungroup()

ts_global_cases$date =epical::epi_week_date(ts_global_cases$epi_week, ts_global_cases$epi_year, offset = 0, system = "cdc")

afr_regions <- read.csv("data/tables/afr_regions.csv")
ts_global_cases <- left_join(ts_global_cases, vaccination, by=c("date", "country_iso3")) %>%
  left_join(afr_regions)

region_cases <- ts_global_cases %>%
  filter(!is.na(who_region), who_region!="None") %>%
  # drop countries without population data
  filter(!is.na(total)) %>% 
  group_by(who_region, subregion, date, epi_year, epi_week) %>%
  summarise(total_cases_per_hundred9=sum(total_cases_per_hundred9),total = sum(total), 
            total_cases9=sum(total_cases9), total_cases_pct = sum(total_cases9)/sum(total),
            people_vacc = sum(people_vaccinated, na.rm = T), 
            people_vacc_pct = sum(people_vaccinated, na.rm = T)/sum(total))
saveRDS(region_cases,"data/epi/region_cases.rds")

fig3counts <- sero_nat %>%
  group_by(subregion) %>%
  summarise(n_country=n_distinct(country),
            n_study=n_distinct(study_name))
