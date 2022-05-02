library(gtsummary)
library(flextable)
library(tidyverse)
library(serosurvr)
library(countrycode)
library(meta)
library(metafor)
library(gtsummary)
library(pander)

script_path = "scripts/"
data_path = "data/"
fig_path = "figures/"

analysis_sub_tbl <- age_sex_data %>%
  mutate(meta_numerator = as.integer(numerator_value),
         meta_denominator = as.integer(denominator_value)) %>%
  select(study_name, first_author, source_type, country, country_iso3, state,
         subregion, income_class, hrp_class, population_group,
         sampling_start_date, sampling_end_date,jbi_a_outputs_v5,
         estimate_grade, antibody_target, symptoms_numerator_value,
         symptoms_serum_pos_prevalence,
         meta_numerator, meta_denominator, serum_pos_prevalence, 
         age_analysis_category, age_specific_category, sex_specific_category,
         subgroup_specific_category, subgroup_var,who_region6
  )

reshape_age_data <- function(group1_value, group2_value){
  return(
    analysis_sub_tbl %>%
      filter(age_analysis_category %in% c(group1_value, group2_value)) %>%
      mutate(age_analysis_category = if_else(age_analysis_category==group1_value, "group1", "group2")) %>%
      group_by(income_class,study_name, age_analysis_category) %>%
      summarise(n_estimates = n(), meta_numerator = sum(meta_numerator), meta_denominator = sum(meta_denominator)) %>%
      mutate(meta_prev = meta_numerator/meta_denominator) %>%
      filter(!is.nan(meta_prev))%>%
      filter(n()==2) %>%
      pivot_wider(id_cols=c(study_name,income_class), names_from = age_analysis_category, values_from = c(meta_numerator,
                                                                                                          meta_denominator,
                                                                                                          meta_prev,
                                                                                                          n_estimates))
  )
}

age0 <- analysis_sub_tbl %>% filter(age_analysis_category == "20-29")
age10_19 <- reshape_age_data("20-29","10-19")
age30_39 <- reshape_age_data("20-29", "30-39")
age0_9 <- reshape_age_data("20-29", "0-9")
age40_49 <- reshape_age_data("20-29", "40-49")
age50_59 <- reshape_age_data("20-29", "50-59")
age60_69 <- reshape_age_data("20-29", "60+")

reshape_sex_data <- function(group1_value="Female", group2_value="Male"){
  return(
    analysis_sub_tbl %>%
      filter(sex_specific_category %in% c(group1_value, group2_value)) %>%
      mutate(sex_specific_category = if_else(sex_specific_category==group1_value, "group1", "group2")) %>%
      group_by(source_type,study_name, sex_specific_category) %>%
      summarise(n_estimates = n(), meta_numerator = sum(meta_numerator), meta_denominator = sum(meta_denominator)) %>%
      mutate(meta_prev = meta_numerator/meta_denominator) %>%
      filter(n()==2) %>%
      pivot_wider(id_cols=c(study_name,source_type), names_from = sex_specific_category, values_from = c(meta_numerator,
                                                                                                         meta_denominator,
                                                                                                         meta_prev,
                                                                                                         n_estimates))
  )
}
Male <- reshape_sex_data()
female <- analysis_sub_tbl %>% filter(sex_specific_category == 'Female')

reshape_geo_data <- function(group1_value="Urban", group2_value="Rural"){
  return(
    geo_data %>%
      filter(subgroup_specific_category %in% c(group1_value, group2_value)) %>%
      mutate(subgroup_specific_category = if_else(subgroup_specific_category==group1_value, "group1", "group2")) %>%
      group_by(study_name, subgroup_specific_category) %>%
      summarise(n_estimates = n(), meta_numerator = sum(meta_numerator), meta_denominator = sum(meta_denominator)) %>%
      mutate(meta_prev = meta_numerator/meta_denominator) %>%
      filter(n()==2) %>%
      pivot_wider(id_cols=c(study_name), names_from = subgroup_specific_category, values_from = c(meta_numerator,
                                                                                                         meta_denominator,
                                                                                                         meta_prev,
                                                                                                         n_estimates))
  )
}
Rural <- reshape_geo_data()
Urban <- geo_data %>% filter(sex_specific_category == 'Urban')

format_ci <- function(point, lower, upper, digits = 2) {
  paste(format(point, digits = digits, scientific = F), ' [',
        format(lower, digits = digits, scientific = F), '-',
        format(upper, digits = digits, scientific = F), '] ',
        sep = '')
}

get_riskratio_metan <- function (data, dataname, n_comparisons) {
  
  rma_object <- 
    data %>%
    metafor::escalc(measure = "RR",
                    ai = meta_numerator_group2,
                    n1i = meta_denominator_group2,
                    ci = meta_numerator_group1, 
                    n2i = meta_denominator_group1, 
                    data = ., 
                    append = TRUE, 
                    slab = study_name) %>%
    metafor::rma(yi, vi, data = .) 
  
  predicted <- predict(rma_object, transf = exp, digits = 2)
  
  return (list(coefficient = predicted$pred,
               ci_lower = predicted$ci.lb,
               ci_upper = predicted$ci.ub,
               riskratio = format_ci(predicted$pred, predicted$ci.lb, predicted$ci.ub, 3),
               I2 = format(rma_object$I2, digits = 2),
               data = dataname)
          %>% as_tibble()
  )
  
}

comparisons_summary <- data.frame(
  subgrouping_stratifier = c(rep("Age", 7),rep("Sex",2),rep("Geography",2)),
  group1_value = c(rep("20-29", 7),rep("female",2),rep("Urban",2)),
  group2_value = c("20-29","0-9","10-19","30-39","40-49","50-59","60+", "female","Male","Urban","Rural"),
  n_comparisons = c(nrow(age0),nrow(age0_9),nrow(age10_19), nrow(age30_39), nrow(age40_49), nrow(age50_59),
                    nrow(age60_69),  nrow(female),nrow(Male),nrow(Urban),nrow(Rural)),
  data = c("age0","age0_9","age10_19","age30_39","age40_49","age50_59","age60_69","female","Male","Urban","Rural")
)

metan_results <- bind_rows(
  get_riskratio_metan(age10_19,'age10_19'),
  get_riskratio_metan(age30_39,'age30_39'),
  get_riskratio_metan(age40_49,'age40_49'),
  get_riskratio_metan(age50_59,'age50_59'),
  get_riskratio_metan(age60_69,'age60_69'),
  get_riskratio_metan(age0_9,'age0_9'),
  get_riskratio_metan(Male,'Male'),
  get_riskratio_metan(Rural,"Rural")
)

summary_table <- 
  dplyr::left_join(comparisons_summary, metan_results, by = 'data') %>% 
  dplyr::select(!data) %>%
  mutate(coefficient = if_else(group1_value==group2_value,1,coefficient),
         riskratio = if_else(group1_value==group2_value,'1',riskratio))

# code adapted from https://stackoverflow.com/questions/38062650/forest-plot-for-a-beginner-simple-example-using-ggplot2-edited
# https://www.selfmindsociety.com/post/a-forest-plot-in-ggplot2
metan_forestplot_input <- 
  summary_table %>%
  dplyr::mutate(
    group = subgrouping_stratifier,
    row = row_number(),
    factor = stringr::str_glue("{group2_value}"),
    factor = if_else(is.na(I2),stringr::str_glue("{factor} (ref)"),
                     stringr::str_glue("{factor} (n = {n_comparisons},\nI2 = {I2})")),
    riskratio = if_else(is.na(I2),stringr::str_glue("PR: {round(coefficient,2)}"),
                        stringr::str_glue("PR: {round(coefficient,2)} [{round(ci_lower,2)}-{round(ci_upper,2)}]")),
    group = case_when(group == 'Age' ~ 'Age',
                      group == 'Sex' ~ 'Sex',
                      group == 'Geography' ~ 'Geography',
                      TRUE ~ group
    )
  ) %>%
  dplyr::arrange(-row) %>%
  mutate(row = row_number()) %>%
  dplyr::select(group,factor,row, riskratio,
                coefficient, ci_lower, ci_upper)

metan_forestplot <- function(subgroup,limits,ylab) {
  subgroup_input <- metan_forestplot_input%>%filter(group==subgroup)
 
  return( 
    ggplot(data = subgroup_input, 
           aes(x = row, 
               y = coefficient, 
               ymin = ci_lower,
               ymax = ci_upper)) +
      geom_pointrange(size=0.25) + 
      geom_hline(yintercept = 1, lty = 2) + 
      scale_x_continuous(breaks = subgroup_input$row,
                         labels = subgroup_input$factor,
                         sec.axis =sec_axis(~ ., breaks = subgroup_input$row, labels=subgroup_input$riskratio))+
      xlab('') +
      ylab(ylab) +
      ggtitle(str_glue("Subgroups: {subgroup}"))+
      coord_flip(ylim = c(0.4, 1.1),xlim=limits) + 
      geom_vline(xintercept=seq(min(subgroup_input$row)+0.5,max(subgroup_input$row)-0.5,1))+
      theme_bw()+
      theme(axis.ticks.x=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.title=element_text(size=10))
  )
}
metan_forestplot1 <- metan_forestplot("Age",NULL,'Prevalence Ratio (95% CI)')

metan_forestplot2 <- metan_forestplot("Sex",c(2.5,4.5),NULL)

metan_forestplot3 <- metan_forestplot("Geography",c(0.5,2.5),'Prevalence Ratio (95% CI)')

library(cowplot)
subgroup_analysis <- plot_grid(metan_forestplot1, 
                               plot_grid(metan_forestplot2, metan_forestplot3, align='v',nrow=2),
                           ncol=2, rel_widths = c(1,1))

