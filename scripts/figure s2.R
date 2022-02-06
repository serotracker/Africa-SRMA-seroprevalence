symptoms <- dataset1 %>%
  filter(source_type=="Early results from UNITY study collaborators", 
         !is.na(symptoms_numerator_value)) %>%
  mutate(meta_numerator = as.integer(numerator_value))%>%
  filter(meta_numerator>5) %>%
  mutate(total_seropositive = meta_numerator) %>%
  select(study_name, country, who_region6, meta_denominator_group1 = total_seropositive, meta_denominator_group2 = total_seropositive, symptoms_numerator_value 
  ) %>%
  mutate(meta_numerator_group1 = as.integer(symptoms_numerator_value),
         meta_numerator_group2 = as.integer(meta_denominator_group1 - meta_numerator_group1)) %>%
  mutate(meta_prev_group1 = meta_numerator_group1/meta_denominator_group1,
         meta_prev_group2 = meta_numerator_group2/meta_denominator_group2)

symptoms%>%
  summarise(median=median(meta_prev_group2),IQR_low=quantile(meta_prev_group2, probs=0.25),IQR_high=quantile(meta_prev_group2,probs=0.75))%>%
  pander()

symptoms_age <- age_sex_data %>%
  filter(source_type=="Unity submission", 
         !is.na(symptoms_numerator_value),!is.na(age_analysis_category)) %>%
  mutate(meta_numerator = as.integer(numerator_value))%>%
  filter(meta_numerator>5) %>%
  mutate(total_seropositive = meta_numerator) %>%
  select(study_name, age_analysis_category, country, who_region6, meta_denominator_group1 = total_seropositive, meta_denominator_group2 = total_seropositive, symptoms_numerator_value 
  ) %>%
  mutate(meta_numerator_group1 = as.integer(symptoms_numerator_value),
         meta_numerator_group2 = as.integer(meta_denominator_group1 - meta_numerator_group1)) %>%
  mutate(meta_prev_group1 = meta_numerator_group1/meta_denominator_group1,
         meta_prev_group2 = meta_numerator_group2/meta_denominator_group2)

symptoms_sex <- age_sex_data %>%
  filter(source_type=="Unity submission",
         !is.na(symptoms_numerator_value),!is.na(sex_specific_category)) %>%
  mutate(meta_numerator = as.integer(numerator_value))%>%
  filter(meta_numerator>5) %>%
  mutate(total_seropositive = meta_numerator) %>%
  select(study_name, sex_specific_category, country, who_region6, meta_denominator_group1 = total_seropositive, meta_denominator_group2 = total_seropositive, symptoms_numerator_value 
  ) %>%
  mutate(meta_numerator_group1 = as.integer(symptoms_numerator_value),
         meta_numerator_group2 = as.integer(meta_denominator_group1 - meta_numerator_group1)) %>%
  mutate(meta_prev_group1 = meta_numerator_group1/meta_denominator_group1,
         meta_prev_group2 = meta_numerator_group2/meta_denominator_group2)

# figure s3

age_plot<-ggplot(data=symptoms_age, aes(age_analysis_category, meta_prev_group2))+geom_jitter(size=1.5,width=0.2,col='#CC6600')+geom_boxplot(outlier.shape=NA,alpha = 0.25,fatten=1,lwd=0.8)+
  labs(x='Age',y='Asymptomatic',title=NULL)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0)),limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  theme_bw()+
  theme(text=element_text(size=12),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
sex_plot<-ggplot(data=symptoms_sex, aes(sex_specific_category, meta_prev_group2))+geom_jitter(size=1.5,width=0.2,col='#CC6600')+geom_boxplot(outlier.shape=NA,alpha = 0.25,fatten=1,lwd=0.8)+
  labs(x='Sex',y=NULL)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0)),limits=c(0,1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  theme_bw()+
  theme(text=element_text(size=12),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

symptoms_age%>%group_by(age_analysis_category)%>%
  summarise(median=median(meta_prev_group2),IQR_low=quantile(meta_prev_group2, probs=0.25),IQR_high=quantile(meta_prev_group2,probs=0.75))%>%
  pander()

symptoms_sex%>%group_by(sex_specific_category)%>%
  summarise(median=median(meta_prev_group2),IQR_low=quantile(meta_prev_group2, probs=0.25),IQR_high=quantile(meta_prev_group2,probs=0.75))%>%
  pander()

kruskal.test(meta_prev_group2 ~ age_analysis_category, data = symptoms_age)

kruskal.test(meta_prev_group2 ~ sex_specific_category, data = symptoms_sex)

