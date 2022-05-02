time_plot_function <- function(region) {

  region_subtitle <- fig3counts %>% filter(subregion==region) %>%
    mutate(subtitle=if_else(n_country==1,
      str_glue("{n_country} country; {n_study} studies"),
      str_glue("{n_country} countries; {n_study} studies")))
 
  vacc_5percent<- region_cases %>% group_by(subregion) %>%
    filter(people_vacc_pct >= 0.05) %>%
    arrange(date)%>%
    filter(date==first(date))%>%
    ungroup()
  
  fig3data <- random_est %>% mutate(est_ci = str_glue("{round(prop*100,1)}%\n[{round(ci_l*100,1)}-{round(ci_u*100,1)}%]\nn={k}"),
                                  sampling_mid_qtr=date,
                                  date=case_when(sampling_mid_qtr=="2020 Q2"~as.Date('2020-05-16'),
                                                 sampling_mid_qtr=="2020 Q3"~as.Date('2020-08-15'),
                                                 sampling_mid_qtr=="2020 Q4"~as.Date('2020-11-16'),
                                                 sampling_mid_qtr=="2021 Q1"~as.Date('2021-02-15'),
                                                 sampling_mid_qtr=="2021 Q2"~as.Date('2021-05-16'),
                                                 sampling_mid_qtr=="2021 Q3"~as.Date('2021-08-15'))
  )
  k_sero <- fig3data%>%filter(model=="sero_nat"|is.na(model))%>%filter(subregion==region)%>%
    group_by(date)%>% summarise(k=k)
  
  figure6_sero<-ggplot()+
    #geom_line(data=sp_cis,aes(x,est,col='Time trend'))+
    geom_point(data=fig3data%>%ungroup()%>%
                 filter(model=="sero_nat"|is.na(model))%>%filter(subregion==region),
               aes(date,prop,col='Unadjusted'),fill='black',
               size=0.8)+
    geom_errorbar(data=fig3data%>%ungroup()%>%
                    filter(model=="sero_nat"|is.na(model))%>%filter(subregion==region),
                  aes(date,ymin=ci_l,ymax=ci_u),col='black', width=20, size=0.5)+
    # geom_point(data=fig3data%>%ungroup()%>%
    #              filter(model=="sero_adj"|is.na(model))%>%filter(subregion==region),
    #            aes(date+14,prop,col='Adjusted'),fill='red',
    #            size=0.8)+
    # geom_errorbar(data=fig3data%>%ungroup()%>%
    #                 filter(model=="sero_adj"|is.na(model))%>%filter(subregion==region),
    #               aes(date+14,ymin=ci_l,ymax=ci_u),col='red', width=20, size=0.5)+
    #annotate("text",x=as.Date("2020-08-01"),y=.7,label="Est. and 95% CI (random-effects model)",size=3.5)+
    scale_x_date(breaks=as.Date(c('2020-05-16','2020-08-15','2020-11-16', '2021-02-15','2021-05-16','2021-08-15')),
                 labels = c("20 Q2", "20 Q3", "20 Q4", "21 Q1","21 Q2", "21 Q3"))+
    coord_cartesian(xlim=c(as.Date("2020-07-01"),as.Date("2021-10-01")))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max_sero[2]),expand = expansion(mult = c(0, 0)))+
    scale_color_manual(name="",breaks=c(#'Time trend',
      'Unadjusted', 'Adjusted'),
                       #labels=function(x) str_wrap(x, width = 20),
                       values = c(#"black",
                         'black', 'red'),
                       guide=guide_legend(nrow = 1,byrow=T, override.aes = list(linetype = c(NA), shape = c(16) ) ) )+
    labs(x=NULL, y=NULL,title=str_glue('{region}'),subtitle=region_subtitle$subtitle)+
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = 12),
          strip.text.x = element_text(size = 12),
          axis.text=element_text(size=10),
          legend.text = element_text(size=10),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0, 0, 0, 0),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0,
                               1, -1, 0.5), "lines"), #t, r, b, l
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  
  asc_plot <- ggplot() +
    geom_text(data=fig3data%>%ungroup()%>%
                filter(model=="sero_nat"|is.na(model))%>%filter(subregion==region),
              aes(date, y=0, label = est_ci),size=3.5,vjust=0.5)+
    theme_bw() +
    scale_x_date(expand = expansion(mult = c(0, 0)))+
    scale_y_continuous(limits=c(-0.005,0.01))+
    coord_cartesian(xlim=c(as.Date("2020-07-01"),as.Date("2021-10-01")))+
    theme(text = element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
          legend.position = "none",
          panel.border = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(), plot.margin = unit(c(-0.5,
                                                             1, -1, 0.5), "lines"),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    xlab(NULL)+
    ylab(NULL)
  
  return(plot_grid(figure6_sero,asc_plot,align = "v", axis = 'lr',nrow =2, rel_heights = c(1,0.4)))
  #return(plot_grid(figure6_sero))
  
}
