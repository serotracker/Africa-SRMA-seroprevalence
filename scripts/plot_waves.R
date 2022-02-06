# identify waves by country and plot epi curves
library(gridExtra)
library(ggpubr)
library(countrycode)
library(cowplot)

gisaid <- readRDS("data/epi/gisaid_data.rds")
afr_regions <- read.csv("data/tables/afr_regions.csv")
who_dths_cases <- readRDS("data/epi/who_dths_cases.rds") %>%
  left_join(afr_regions)
who_epi_curves <- readRDS("data/epi/who_epi_curves.rds")
vaccination_numbers <- readRDS("data/epi/vaccination_numbers.rds")

who_sero_data <- bind_rows(who_epi_curves%>%
                            mutate(panel='a'), 
                            dataset0_to_plot%>%
                            filter(study_name %in% dataset1$study_name) %>%
                            select(-wave, -adults_vaccinated_per_hundred14) %>%
                            rename(date=sampling_mid_date)%>%
                            mutate(panel='b'),
                            who_dths_cases%>%
                             left_join(vaccination_numbers) %>%
                            mutate(panel='c'))

# plot function
plot_waves <- function(r){
  iso_codes <- who_sero_data%>%filter(subregion==r)%>%group_by(country_iso3) %>%
    ungroup()%>% arrange(urban_pop) %>% filter(panel=='b') %>% distinct(country_iso3)%>%pull()
  plots <- list()
  death_plots <- list()
  wave_plots <- list()
  sero_plots <- list()
  gisaid_plots <- list()
  for(c in iso_codes){
  country <- who_sero_data%>%filter(country_iso3==c,panel=="b")%>%select(country)%>%distinct()%>%pull()
  urban_pop <- who_sero_data%>%filter(country_iso3==c,panel=="b")%>%select(urban_pop)%>%distinct()%>%pull()
  
  death_plots[[c]] <- 
    ggplot(who_sero_data%>%filter(country_iso3==c, panel=="c"))+
    geom_line(aes(as.Date(date), total_cases_per_hundred))+
    #geom_line(aes(as.Date(date), people_vaccinated_per_hundred14/10), col="#A65628")+
    scale_y_continuous(name="Cum. incidence (%)",expand = expansion(mult = c(0, 0.1)), limits = c(0,5))+
  #,sec.axis = sec_axis(~.*10, name="Vaccination (%)"))+
    scale_x_date(date_breaks = "3 months",date_labels = "%b %y", expand = expansion(mult = c(0, 0)),
                 limits=c(as.Date("2020-03-01"), as.Date("2021-10-01")))+
    labs(title=NULL, x=NULL)+
    theme_bw()+
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 10),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-5,-10),
          strip.text.x = element_text(size = 12),
          #axis.title.y.right = element_text(color="#A65628"),
          plot.caption = element_text(size=8)
          ) 
  
  ## Select columns, normalize, and overlay
  
  value_cols  <- who_sero_data%>%filter(country_iso3==c, panel=="a", date>=as.Date("2020-02-15")) %>% 
    dplyr::select("new_cases_per_million") # select values columns
  
  tb_GMCovid_scaled <-  bind_cols(date=who_sero_data%>%filter(country_iso3==c, panel=="a", date>=as.Date("2020-02-15")) %>% select(date,ends_with("baseline")),
                                  data.frame( value_cols)) %>%
    mutate(cases1=max(new_cases_per_million, na.rm=T)<35,
           cases2=max(new_cases_per_million, na.rm=T)>=35 & max(new_cases_per_million, na.rm=T)<100,
           cases3=max(new_cases_per_million, na.rm=T)>=100)
 
  gisaid_c <- gisaid %>%filter(country_iso3==c)
  if (c=="BFA") {gisaid_c$pct <- NA_real_}
  tb_GMCovid_scaled_c <- tb_GMCovid_scaled
  if (tb_GMCovid_scaled$cases3) {
    gisaid_plots[[c]]<-
      gisaid_plot_fill(multiplier=0.003, country=country, gisaid_c, tb_GMCovid_scaled_c)
  }
  
  else if (tb_GMCovid_scaled$cases2) {
    gisaid_plots[[c]]<-
      gisaid_plot_fill(multiplier=0.01, country=country, gisaid_c, tb_GMCovid_scaled_c)
  }
  
  else {
    gisaid_plots[[c]]<-
      gisaid_plot_fill(multiplier=0.03, country=country, gisaid_c, tb_GMCovid_scaled_c)
  }
  sero_iso_codes <- who_sero_data%>%filter(panel=='b', subregion==r)%>%distinct(country_iso3)%>%pull()
  if (c %in% sero_iso_codes) {
  max_prop <- as.numeric(who_sero_data%>%filter(subregion==r)%>%summarise(max(serum_pos_prevalence,na.rm=T)))*100
  
  sero_plots[[c]]<-
    ggplot(who_sero_data%>%filter(country_iso3==c, panel=="b")%>%mutate(first_author=str_glue("{first_author} et al.")))+
    geom_point(aes(as.Date(date), serum_pos_prevalence*100, fill = estimate_grade, shape=first_author), size = 2)+
    #geom_linerange(aes(as.Date(date), ymin=seroprev_95_ci_lower*100, ymax=seroprev_95_ci_upper*100), col='black') +
    geom_rect(aes(xmin = sampling_start_date, xmax = sampling_end_date, ymin = 0, ymax = Inf, fill = estimate_grade), 
              alpha = 0.3)+
    #geom_smooth(data=tb_GMCovid_scaled, aes(as.Date(date), new_cases_per_million*4), col='#666666', size=0.5, span=0.1, se=F, method='loess')+
    #geom_text(aes(as.Date(date), new_cases_smoothed,
    #              label=label), vjust=-1)+
    scale_y_continuous(name="Seroprev (%)", expand = expansion(mult = c(0, 0.1)), limits=c(0, max_prop))+
    scale_x_date(date_breaks = "3 months",date_labels = "%b %y", expand = expansion(mult = c(0, 0)),
                 limits=c(as.Date("2020-03-01"), as.Date("2021-10-01")))+
    scale_fill_manual(name="", labels=c("Capital","Local","Sub-ntnl", "Ntnl"),
                      breaks=c("Capital","Local","Subnational", "National"), values=c("#FF7F00", "#FFFF33", "#E41A1C", "#377EB8"))+
    scale_shape_manual(name="",values=c(21,22,23,24,25,1)) +
    labs(x=NULL, y=NULL)+
    theme_bw()+
    theme(legend.position='bottom',
          #legend.box="vertical",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 10),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-5,-10),
          strip.text.x = element_text(size = 12),
          plot.caption = element_text(size=8)) +
    guides(fill = guide_legend(nrow = 2, override.aes = list(shape=NA, alpha=0.5)),
           shape = guide_legend(nrow = 2))
  
  vacc_5percent<- vaccination_numbers %>% group_by(country_iso3) %>%
    filter(people_vaccinated_per_hundred >= 5, date<as.Date("2021-09-01")) %>%
    arrange(date)%>%
    filter(date==first(date))%>%
    ungroup()
  
  plots[[c]] <- plot_grid(gisaid_plots[[c]],sero_plots[[c]],death_plots[[c]], nrow=3,
                          align='v',rel_heights=c(1,1.1,0.9))
  }

  }
  
  print(ggarrange(plotlist = plots, ncol = 2))
}
