meta_analysis<-function(sero = sero_complete, model= "sero_45", window_span = 56, date_var = "week") { # 3-month rolling window

# meta-analysis function -- will return error if no observations in a given window
metan <- function(event, n, studlab) {
  tryCatch(
    {
      mtprop_reg_or_nat <- metaprop(event = event, 
                                    n = n, 
                                    studlab = studlab, 
                                    fixed = FALSE,
                                    method = "GLMM",
                                    sm = "PLOGIT")
      return(mtprop_reg_or_nat)
    },
    error = function(error_message){
      message(error_message)
      return(NA)
    }
  )
}

# empty dataframe to save results from meta-analysis
res<-data.frame(subregion=character(),
                                  date=as.Date(character(0)),
                                  k=double(),
                                  prop=double(),
                                  se=double(),
                n=integer(),
                seP=double(),
                I2=double(),
                ci_l=double(),
                ci_u=double())
# Loop over countries
for(j in unique(sero$subregion)){
print(j)
  r <- sero %>% filter(subregion == j) %>% select(who_region) %>% distinct() %>% pull()
  data_region <- sero %>% filter(who_region == r)
  data_reg_or_nat <- sero %>% filter(subregion == j)
  date <- as.Date(c('2020-05-17','2020-08-16','2020-11-15', '2021-02-14','2021-05-16','2021-08-15'))
    # seq(min(data_region %>% select(date_var) %>% pull()),
    #           max(data_region %>% select(date_var) %>% pull()), 
    #           28)
  # Loop over dates and run random-effects meta-analysis
  for(i in 1:length(date)){
      
    data_for_metan <- data_reg_or_nat[data_reg_or_nat[[date_var]]>= date[i]-45 & data_reg_or_nat[[date_var]]<= date[i]+45,]
    data_for_metan <- data_for_metan %>%
                      #filter(if (n_distinct(estimate_grade) > 1) 
                      #estimate_grade=="National" else estimate_grade %in% c("Subnational", "National")) %>%
                      select(denominator_value, numerator_value, study_name) %>%
                      distinct()

    mtprop_reg_or_nat <- metan(event = data_for_metan$numerator_value, 
                               n = data_for_metan$denominator_value, 
                               studlab = data_for_metan$study_name)
    
    if (!is.na(mtprop_reg_or_nat)) # if no error
    {
    # bind results from meta-analysis
    res <- rbind(res,data.frame(model = model,
                                #country = j,
                                subregion =j,
                                date = date[i],
                                k = mtprop_reg_or_nat$k.study,
                                # apply logit transformation to proportion
                                prop = plogis(mtprop_reg_or_nat$TE.random),
                                # se on logit scale
                                se = mtprop_reg_or_nat$seTE.random,
                                n=sum(data_for_metan$denominator_value),
                                seP=mtprop_reg_or_nat$seTE.predict,
                                I2=mtprop_reg_or_nat$I2,
                                ci_l=plogis(mtprop_reg_or_nat$lower.random),
                                ci_u=plogis(mtprop_reg_or_nat$upper.random))
    )
    }
  }
}

return(res)
}

