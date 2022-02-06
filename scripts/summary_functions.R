
# summary statistics
summary_stats <- function(df){
  n_total <- nrow(df)
  n_countries <- length(unique(df$country_iso3))
  n_regional <- df %>% filter(estimate_grade=="Subnational") %>% nrow()
  n_national <- df %>% filter(estimate_grade=="National") %>% nrow()
  n_local <- df %>% filter(estimate_grade=="Local") %>% nrow()
  n_sources <- length(unique(df$source_name))
  min_prev = scales::percent(as.numeric(df %>%
                                          summarise(min(serum_pos_prevalence, na.rm = TRUE))),
                             1L)
  max_prev = scales::percent(as.numeric(df %>%
                                          summarise(max(serum_pos_prevalence, na.rm = TRUE))),
                             1L)
  
  study_counts <-
    df %>%
    count(who_region6) %>%
    mutate(pct = scales::percent(prop.table(n), 1L)) %>%
    data.table::data.table()
  
  data.table::setkey(study_counts, who_region6)
  
  income_counts <-
    df %>%
    count(income_class) %>%
    mutate(pct = scales::percent(prop.table(n), 1L)) %>%
    data.table::data.table()
  
  data.table::setkey(income_counts, income_class)
  
  hrp_counts <-
    df %>%
    count(hrp_class) %>%
    mutate(pct = scales::percent(prop.table(n), 1L)) %>%
    data.table::data.table()
  
  data.table::setkey(hrp_counts, hrp_class)
  
  country_counts <-
    df %>%
    select(who_region6, country_iso3) %>%
    distinct() %>%
    count(who_region6) %>%
    data.table::data.table()
  
  data.table::setkey(country_counts, who_region6)
  
  hrp_country_counts <-
    df %>%
    select(country_iso3, hrp_class) %>%
    distinct() %>%
    count(hrp_class) %>%
    data.table::data.table()
  
  data.table::setkey(hrp_country_counts, hrp_class)
  
  income_country_counts <-
    df %>%
    select(country_iso3, income_class) %>%
    distinct() %>%
    count(income_class) %>%
    data.table::data.table()
  
  data.table::setkey(income_country_counts, income_class)
  
  rob_counts <- 
    df %>% select(study_name, jbi_a_outputs_v5) %>% distinct() %>%
    count(jbi_a_outputs_v5) %>%
    mutate(pct = scales::percent(prop.table(n), 1L)) %>%
    data.table::data.table()
  
  data.table::setkey(rob_counts, jbi_a_outputs_v5)
  
  source_counts <- 
    df %>% select(source_name, source_type) %>% distinct() %>%
    count(source_type) %>%
    mutate(pct = scales::percent(prop.table(n), 1L)) %>%
    data.table::data.table()
  
  data.table::setkey(source_counts, source_type)
  
  most_studies <- df %>%
    filter(who_region!="None")%>%
    group_by(who_region,country) %>%
    count() %>%
    ungroup()%>%
    group_by(who_region) %>%
    arrange(who_region, -n)%>%
    slice_head(n=1)%>%
    ungroup()%>%
    data.table::data.table()
  
  data.table::setkey(most_studies, who_region)
  
  return(list(n_total=n_total, n_countries=n_countries, n_regional=n_regional, n_national=n_national, n_local = n_local,
              n_sources=n_sources, min_prev = min_prev, max_prev = max_prev,
              study_counts=study_counts, country_counts=country_counts, 
              rob_counts=rob_counts, most_studies=most_studies, hrp_country_counts=hrp_country_counts,
              income_country_counts=income_country_counts,
              source_counts=source_counts, income_counts=income_counts, hrp_counts=hrp_counts))
}

sero_case_table <- function(data = df) {
  sero_case_table <-
    data %>%
    mutate(who_region=as.character(who_region))%>%
    select(who_region,
           qtr,
           sero_case_ratio) %>%
    tbl_2way_summary(row = who_region, col = qtr, con = sero_case_ratio,
                     # label = c(who_region ~ "WHO region",
                     #           qtr ~ "Average seroprevalence to case ratio (range)"),
                     statistic = list(all_continuous() ~ "{mean} ({min}-{max}")) %>%
    bold_labels() %>%
    modify_footnote(update = everything() ~ NA)
  
  return(sero_case_table)
}

sero_table <- function(data = global_tbl, rowvar, label) {
  sero_table <-
    data %>%
    mutate(all="Median and IQR (%)", serum_pos_prevalence = serum_pos_prevalence * 100) %>%
    select(rowvar,
           sampling_mid_qtr,
           serum_pos_prevalence) %>%
    tbl_2way_summary(row = {{ rowvar }}, col = sampling_mid_qtr, con = serum_pos_prevalence,
                     label = c({{ rowvar }} ~ label,
                               sampling_mid_qtr ~ "Sampling midpoint date",
                          serum_pos_prevalence ~ "Seroprevalence")) %>%
    bold_labels() %>%
    modify_footnote(update = everything() ~ NA)

  return(sero_table)
}

sero_table_stack <- function(data = unity_studies){
  sero_table_stack <- 
    tbl_stack(tbls = list(sero_table(data, "population_group", "Sampling frame"),
                          sero_table(data, "access_grp", "Access to health care index"),
                          sero_table(data, "subregion", "Sub-region"),
                          sero_table(data, "hrp_class", "HRP")
                          #sero_table(data, "test_type", "Test type")
                          #sero_table(data, "hrp_class", "HRP class"),
                          #sero_table(data, "jbi_a_outputs_v5", "Risk of bias")
                          #sero_table(data, "who_region6", "WHO region"),
                          #sero_table(data%>%filter(estimate_grade=="National"), "sampling_end_qtr", "Sampling end quarter (National studies)"),
                          #sero_table(data, "wave", "Transmission phase at study mid-point")
                          )
              )
  
  return(sero_table_stack)
}

study_table <- function(regions, data = unity_studies) {
  study_table <-
    data %>%
    filter(who_region6 %in% regions) %>%
    select(income_class,
           hrp_class,
           subregion,
           source_type,
           pct_city_group,
           population_group,
           #m_mfr,
           sampling_method,
           study_type,
           test_type,
           jbi_a_outputs_v5,
           people_vaccinated_group)%>%
    tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)"),
                label = c(income_class ~ "World Bank income level",
                          hrp_class ~ "Humanitarian response plan (HRP)",
                          subregion ~ "UN sub-region",
                          source_type ~ "Source type",
                          pct_city_group ~ "Geographic scope",
                          population_group ~ "Study population",
                          #jbi_a_outputs_v4_mfr ~ "Risk of bias",
                          sampling_method ~ "Sampling method",
                          study_type ~ "Study design",
                          test_type ~ "Test type",
                          jbi_a_outputs_v5 ~ "Overall risk of bias",
                          #wave ~ "Transmission phase at study mid-point",
                          #testing_policy ~ "Testing policy",
                          people_vaccinated_group ~ "Percent vaccinated at study mid-point"
                          ),
                          
                          #jbi_a_outputs_v4_mfr ~ "Risk of bias",
                          
                          
                missing = "no") %>%
    bold_labels()
    #modify_header(update = all_stat_cols() ~ "**{level}** \n(N = {n})") %>%
    #modify_footnote(update = everything() ~ NA)
  
  return(study_table)
}

geo_table <- function(region, data = unity_studies) {
  geo_table <-
    data %>%
    filter(who_region6 %in% region) %>%
    select(who_region6,
           income_class,
           hrp_class,
           #isotypes_used,
           #sampling_end_qtr,
           wave,
           #testing_policy,
           people_vaccinated_group) %>%
    tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)"),
                label = c(who_region6 ~ "WHO region",
                          #jbi_a_outputs_v4_mfr ~ "Risk of bias",
                          income_class ~ "World Bank income level",
                          hrp_class ~ "Humanitarian response plan (HRP)",
                          #isotypes_used ~ "Isotypes used",
                          #sampling_end_qtr ~ "Sampling end quarter",
                          wave ~ "Transmission phase at study mid-point",
                          #testing_policy ~ "Testing policy",
                          people_vaccinated_group ~ "Percent vaccinated at study mid-point"),
                missing = "no") %>%
    bold_labels()
  #modify_header(update = all_stat_cols() ~ "**{level}** \n(N = {n})") %>%
  #modify_footnote(update = everything() ~ NA)
  
  return(geo_table)
}
rob_table <- function(region, data = unity_studies) {
  rob_table <-
    data %>%
    filter(who_region6 %in% region) %>%
    select(
           population_group,
           pop_adj,
           jbi_a_outputs_v5) %>%
    tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)"),
                label = c(
                          population_group ~ "Study population",
                          pop_adj ~ "Population adjustment",
                          jbi_a_outputs_v5 ~ "Risk of bias"),
                missing = "no") %>%
    bold_labels()
  #modify_header(update = all_stat_cols() ~ "**{level}** \n(N = {n})") %>%
  #modify_footnote(update = everything() ~ NA)
  
  return(rob_table)
}

region_sub_table <- function(region, data = unity_estimates, var = age_analysis_category) {
  region_sub_table <-
    data %>%
    filter(who_region6 %in% region
           #, age_analysis_category %in% c("0-19", "60+","20-39", "40-59","7-7","19-59")
           ) %>%
    select({{ var }}) %>%
    tbl_summary(statistic = list(all_categorical() ~ "{n}"),
                label = c({{ var }} ~ "Age category"),
                missing = "no") %>%
    bold_labels() %>%
    modify_footnote(update = everything() ~ NA)
  
  return(region_sub_table)
}

merge_tables <- function() {
  regions <- c("Africa region", "Eastern Mediterranean region", "South-East Asia region", 
               "Americas region", "Western Pacific region",
               "Europe region")
  region_table1 <-
    tbl_merge(
      tbls = list(study_table(regions,data=dataset0),
                  study_table(regions,data=dataset1),
                  study_table(regions,data=dataset2)
      ),
      tab_spanner = c("Used in descriptive analysis","Used in meta-analysis and meta-regression", "Used to estimate ascertainment")
    )

  
  return(region_table1)
}

merge_sub_tables <- function(sub_data = unity_estimates, var = "age_analysis_category") {
  regions <- c("Africa", "Eastern Mediterranean", "South-East Asia", 
               "Americas","Western Pacific",
               "Europe")
  
  region_table2a <-
    tbl_merge(
      tbls = list(region_sub_table(regions[1], sub_data, var),
                  region_sub_table(regions[2], sub_data, var),
                  region_sub_table(regions[3], sub_data, var)
      ),
      tab_spanner = regions[1:3]
    )
  
  region_table2b<-
    tbl_merge(
      tbls = list(region_sub_table(regions[4], sub_data, var),
                  region_sub_table(regions[5], sub_data, var),
                  region_sub_table(regions[6], sub_data, var)
      ),
      tab_spanner = regions[4:6]
    )
  
  return(list(region_table2a, region_table2b))
}

# source: https://stackoverflow.com/a/57193998
FitFlextableToPage <- function(ft){
  
  ft_out <- ft %>%
    autofit() %>%
    padding(padding = 0, part = "all") %>%
    line_spacing(space = 1.15, part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    #set_caption(caption) %>%
    border_inner(part = "all") %>%
    border_outer(part = "all") %>%
    bold(part = "header")
    #font(fontname = "Calibri", part = "all") %>%
    #theme_zebra()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths * 7 / (flextable_dim(ft_out)$widths))
  return(ft_out)
}




