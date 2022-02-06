library(gtsummary)
library(flextable)
library(tidyverse)
library(serosurvr)
library(countrycode)
library(meta)
library(metafor)
library(gtsummary)
library(MuMIn)
library(broom.mixed)

script_path = "scripts/"
data_path = "data/"

pop_dens <- read.csv("data/tables/pop_density.csv") %>% select(study_name, pop_dens_cat)
health_index <- read.csv("data/tables/health_index.csv") %>%
  janitor::clean_names() %>%
  mutate(country_iso3=countrycode::countrycode(country, origin='country.name',destination='iso3c')) %>%
  select(-country)

regnat_data <- dataset0_to_plot%>%
  filter(study_name %in% dataset1$study_name) %>%
  left_join(dataset1_adj%>%select(study_name, denominator_value, adj_prevalence))%>%
  #filter(!is.na(adj_prevalence)) %>%
  #mutate(serum_pos_prevalence=if_else(!is.na(adj_prevalence),adj_prevalence,serum_pos_prevalence))%>%
  left_join(pop_dens)%>%
  left_join(health_index) %>%
  mutate(meta_numerator = as.integer(serum_pos_prevalence*denominator_value),
         meta_denominator = as.integer(denominator_value),
         rdt = test_type=="LFIA") %>%
  relocate(c(pct_2020, pct_city_group), .after=capital) %>%
  mutate(estimate_grade = as.character(estimate_grade),
         estimate_grade = if_else(estimate_grade!="National",
                                  "Local/subnational", "National"),
         pop_dens_cat = if_else(pop_dens_cat == "Low density", "Low density area", "High density area"),
         test_type = as.character(test_type),
         test_type = case_when(grepl("ELISA",test_type)~"ELISA",
                               grepl("Other",test_type)~"Other",
                               TRUE~test_type),
         wave = factor(wave, levels=c("After peak of 2nd wave","After peak of 1st wave","Before peak of 1st wave"))) %>%
  select(study_name, first_author, source_type, country, country_iso3, state, income_class,
         subregion, hrp_class, population_group, pop_group_old, 
         sampling_start_date, sampling_mid_date, total_cases_per_hundred9, ends_with("ave30"),
         sampling_end_date, estimate_grade, antibody_target, rdt, test_type, index,access, 
         meta_numerator, meta_denominator, serum_pos_prevalence, pop_adj, pop_dens_cat,
         sampling_mid_qtr, source_name,total_cases9,
         wave, people_vaccinated_per_hundred14
  ) %>%
  filter(people_vaccinated_per_hundred14<5)%>%
  mutate(across(c(global_index_ave30), ~.x/10))%>%
  dplyr::mutate(dplyr::across(c(hrp_class,
                                income_class,
                                test_type,
                                subregion,
                                estimate_grade,
                                pop_group_old,
                                pop_dens_cat),
                              forcats::as_factor)) %>%
  dplyr::mutate(hrp_class = relevel(hrp_class, 'non-HRP country'),
                estimate_grade = relevel(estimate_grade, 'National'),
                test_type = relevel(test_type, 'CLIA'),
                pop_group_old = relevel(pop_group_old, 'Household and community samples'),
                subregion = relevel(subregion, 'Southern Africa'),
                income_class=relevel(income_class,"Low income country"),
                wave = relevel(wave, 'Before peak of 1st wave'),
                pop_dens_cat = relevel(pop_dens_cat,"Low density area")) %>%
  distinct()

genpop_metareg_na <- na.omit(regnat_data%>%filter(pop_group_old!="Pregnant or parturient women") %>%
                               select(subregion,
                                      hrp_class,
                                                  country,
                                                  estimate_grade,
                                                  total_cases_per_hundred9,
                                                  pop_group_old,
                                                  test_type,
                                                  pop_dens_cat,
                                                  study_name,
                                                  meta_numerator, 
                                                  meta_denominator)) %>%
  filter(meta_numerator>0)
  
# meta-regression
# poisson
res <- glmer(meta_numerator ~ offset(log(meta_denominator)) +
        pop_dens_cat +
        pop_group_old +
        total_cases_per_hundred9 +
        test_type +
          subregion +
          estimate_grade +
          (1 | study_name), 
        family=poisson(link='log'), data=genpop_metareg_na, nAGQ = 7,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
drop1(res)

format_ci <- function(point, lower, upper, digits = 2) {
  paste(format(point, digits = digits, scientific = F), ' [',
        format(lower, digits = digits, scientific = F), '-',
        format(upper, digits = digits, scientific = F), '] ',
        sep = '')
}

output_reg <- function(mod) {
metareg_output_all <-tidy(mod,conf.int=TRUE,exponentiate=TRUE,effects="fixed")%>%
    rename(factor=term,coefficient=estimate,ci_lower=conf.low,
           ci_upper=conf.high, pval=p.value)%>%
    filter(factor!="(Intercept)")
  

metareg_output_all <- metareg_output_all %>%
  bind_rows(bind_cols(factor = c("tau2", "R2", "k","AIC"),
                      coefficient = c(tau2=as.numeric(attr(summary(mod)$varcor$study_name, "stddev")),
                                      R2=r.squaredGLMM(mod)[1,1],
                                      k = as.numeric(summary(mod)$ngrps), AIC = AIC(mod))))

metareg_output_formatted <- 
  metareg_output_all %>%
  mutate(exp_coeff = case_when(factor %in% c("tau2","AIC") ~ 
                                 as.character(format(coefficient, digits = 2,
                                                     scientific = FALSE)),
                               factor == "k" ~ 
                                 as.character(format(coefficient, digits = 0,
                                                     scientific = FALSE)),
                               factor %in% c("R2") ~
                                 as.character(scales::percent(coefficient)),
                               TRUE ~ format_ci(.$coefficient,
                                                .$ci_lower,
                                                .$ci_upper,
                                                digits = 2)
  ),
  pval = as.character(format(pval, digits = 2)),
  pval = if_else(pval<0.0001, "<0.0001", as.character(pval))
  ) %>%
  select(factor, coefficient, ci_lower, ci_upper, exp_coeff, pval)
return(metareg_output_formatted)
}

saveRDS(output_reg(res), "data/metareg_est.rds")
write.csv(output_reg(res), glue::glue("outputs/metareg_output_formatted_res.csv"), row.names=FALSE)

metareg_forestplot <- function(model){
  metareg_output_all <-readRDS("data/metareg_est.rds")
  
  metareg_output_forestplot <-
    metareg_output_all%>% 
    dplyr::filter(!(factor %in% c('intrcpt', 'tau2','AIC','k','R2','I2'))) %>%
    dplyr::mutate(group = case_when(
      grepl('pop_dens_cat', factor) ~ 'Population density',
      grepl('pop_group_old', factor) ~ 'Sampling frame',
      grepl('subregion', factor) ~ 'Subregion',
      grepl('total_cases_per_hundred9', factor) ~ 'Cases',
      grepl('estimate_grade', factor) ~ 'Scope',
      grepl('test_type', factor) ~ 'Test type',
      grepl('global_index_ave30', factor) ~ 'PHSM'
    )) %>%
    dplyr::mutate(
      factor = stringr::str_remove(factor, 'subregion'),
      factor = stringr::str_remove(factor, "hrp_class"),
      factor = stringr::str_remove(factor, 'pop_group_old'),
      factor = stringr::str_remove(factor, "pop_dens_cat"),
      factor = stringr::str_remove(factor, 'test_type'),
      factor = stringr::str_remove(factor, 'subregion'),
      factor = stringr::str_remove(factor, 'estimate_grade'),
      factor = stringr::str_replace(factor, 'total_cases_per_hundred9', 'Cumulative incidence'),
      factor = stringr::str_replace(factor, "pop_density", "Population density"),
      factor = stringr::str_remove(factor, 'wave'),
      factor = stringr::str_replace(factor, 'global_index_ave30', 'PHSM severity (0-10)'),
      factor = stringr::str_remove(factor, 'country'),
      factor = stringr::str_remove(factor, 'or parturient '),
      factor = factor(factor),
      ref=0
    ) %>%
    filter(grepl("ELISA|LFIA|Other|Subnational|Capital|Local|Cumulative|Blood|sera|Pregnant|Africa|High|Moderate|HRP|wave",factor))%>%
    mutate(ref=0)%>%
    add_row(group='Subregion', factor='Southern Africa (ref)',coefficient=1, 
            exp_coeff="1",ref=1) %>%
    add_row(group='Population density', factor='Low density area (ref)',coefficient=1, 
            exp_coeff="1",ref=1) %>%
    add_row(group='Sampling frame', factor='Households (ref)',coefficient=1,
            exp_coeff='1',ref=1)%>%
    add_row(group='Scope', factor='National (ref)',coefficient=1,
            exp_coeff='1',ref=1)%>%
    add_row(group='Test type', factor='CLIA (ref)',coefficient=1,
            exp_coeff='1',ref=1)%>%
    mutate(exp_coeff = if_else(ref==1, str_glue('PR: 1'),str_glue("PR: {str_trim(exp_coeff)}"))) %>%
    arrange(group, ref) %>%
    mutate(row=row_number())
  
  
  metareg_forestplot <- 
    ggplot(data = metareg_output_forestplot, 
           aes(x = row, 
               y = coefficient, 
               ymin = ci_lower,
               ymax = ci_upper)) +
    geom_pointrange(size=0.25) + 
    geom_vline(xintercept=c(1.5,3.5,6.5,8.5,12.5), lty = 2)+
    geom_hline(yintercept = 1, lty = 2) + 
    scale_x_continuous(breaks = metareg_output_forestplot$row,
                       labels = metareg_output_forestplot$factor,
                       sec.axis =sec_axis(~ ., breaks = 
                                            metareg_output_forestplot$row,  
                                          labels=metareg_output_forestplot$exp_coeff))+
    scale_y_log10() +
    xlab(NULL) +
    ylab('Prevalence Ratio (95% CI)') +
    ggtitle('Multivariate analysis')+
    coord_flip() + 
    theme_bw()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          plot.title=element_text(size=10))
  
  return(metareg_forestplot)
}
meta_reg <- metareg_forestplot(model=res)


