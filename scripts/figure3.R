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
data_path = "data/"

# continent-wide cases and population

population_counts <- readRDS("data/pop/population_counts.rds")
cases_africa <- who_dths_cases %>%
  filter(who_region=="Africa" | 
           country_iso3 %in% c("DZA","EGY","LBY","MAR","SDN","TUN","SOM"))%>%
  group_by(date) %>%
  summarise(total_cases=sum(total_cases), total_deaths=sum(total_deaths)) %>%
  mutate(date = as.character(zoo::as.yearqtr(date))) %>%
  group_by(date) %>%
  summarise(total_cases=last(total_cases), total_deaths=last(total_deaths)) %>%
  mutate(total=sum(population_counts$total))

# continent-wide seroprevalence

metan_nat <- sero_nat %>% arrange(sampling_mid_qtr) %>%
  metaprop(event = numerator_value, n = denominator_value, studlab = study_name, subgroup=sampling_mid_qtr)
metan_inf <- sero_inf %>% arrange(sampling_mid_qtr) %>%
  metaprop(event = numerator_value, n = denominator_value, studlab = study_name, subgroup=sampling_mid_qtr)
metan_africa <- rbind(data.frame(model = "sero_nat",
                        subregion = "Africa",
                        date = c("2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                                 "2021 Q1", "2021 Q2", "2021 Q3"),
                        k = metan_nat$k.w,
                        # apply logit transformation to proportion
                        prop = plogis(metan_nat$TE.random.w),
                        # se on logit scale
                        se = metan_nat$seTE.random.w,
                        seP=metan_nat$seTE.predict.w,
                        I2=metan_nat$I2.w,
                        ci_l=plogis(metan_nat$lower.random.w),
                        ci_u=plogis(metan_nat$upper.random.w)),
             data.frame(model = "sero_inf",
                        subregion = "Africa",
                        date = c("2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
                                 "2021 Q1", "2021 Q2", "2021 Q3"),
                        k = metan_inf$k.w,
                        # apply logit transformation to proportion
                        prop = plogis(metan_inf$TE.random.w),
                        # se on logit scale
                        se = metan_inf$seTE.random.w,
                        seP=metan_inf$seTE.predict.w,
                        I2=metan_inf$I2.w,
                        ci_l=plogis(metan_inf$lower.random.w),
                        ci_u=plogis(metan_inf$upper.random.w))
) %>%
  left_join(cases_africa) %>%
  mutate(sero_case_ratio = total * prop / total_cases,
         sero_case_ratio_l = total * ci_l / total_cases,
         sero_case_ratio_u = total * ci_u / total_cases)

# sub-regional seroprevalence

date_var="sampling_mid_qtr"
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
