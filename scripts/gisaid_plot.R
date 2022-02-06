gisaid_plot_fill <- function(multiplier, country, gisaid_c, tb_GMCovid_scaled_c){
return(
  ggplot() + 
  geom_area(data=gisaid_c, aes(x=date, y=pct, fill=VOC), alpha=0.8)+
  #geom_point(data=gisaid_c, aes(x=date, y=pct, fill=VOC),position = "stack") +
  #geom_line(data=gisaid_c, aes(x=date, y=pct, fill=VOC),position = "stack") +
  geom_smooth(data=tb_GMCovid_scaled_c, aes(as.Date(date), new_cases_per_million*100*multiplier), col='black', size=0.5, span=0.1, se=F, method='loess')+
  scale_y_continuous(name="VOC (%, shaded)", expand = expansion(mult = c(0, 0)), 
                     sec.axis = sec_axis(~./(100*multiplier), name="New cases/mil"))+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %y", expand = expansion(mult = c(0, 0)), 
               limits=c(as.Date("2020-03-01"), as.Date("2021-10-01")))+
  labs(title=str_wrap(str_glue("{country}"), 32), x=NULL, y=NULL)+
  theme_bw()+
  scale_fill_manual(name="",values=c("#4DAF4A","#984EA3","#F781BF", "#999999"))+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-5,-10),
        strip.text.x = element_text(size = 12),
        plot.caption = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 1))
)
}

