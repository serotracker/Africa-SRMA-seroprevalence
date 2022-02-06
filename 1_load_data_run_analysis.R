library(rmarkdown)
library(tidyverse)
library(openxlsx)
library(lme4)
library(pander)
library(Hmisc)
library(countrycode)
library(cowplot)

#paths
script_path = "scripts/"
data_path <- "data/analysis/final/"

# helper functions
source(glue::glue(script_path,"tbl_2way_summary.R"))
source(glue::glue(script_path,"summary_functions.R"))
source(glue::glue(script_path,"meta_analysis_function.R"))
source(glue::glue(script_path,"time_plot_function.R"))
source(glue::glue(script_path,"gisaid_plot.R"))

# load data
dataset0 <- readRDS(glue::glue(data_path,"dataset0.rds"))
dataset0_to_plot <- readRDS(glue::glue(data_path,"dataset0_to_plot.rds"))
dataset1 <- readRDS(glue::glue(data_path,"dataset1.rds"))
dataset1_adj <- readRDS(glue::glue(data_path,"dataset1_adj.rds"))
dataset2 <- readRDS(glue::glue(data_path,"dataset2.rds"))
#age_sex_data <- readRDS(glue::glue(data_path,"age_sex_data.rds"))
geo_data <- readRDS(glue::glue(data_path,"geo_data.rds"))

# Figure 2
rmarkdown::render("scripts/figure 2.Rmd", output_file="figures/Figure 2", output_format = "word_document")

# Table 2
rmarkdown::render("scripts/table s3.Rmd", output_file="figures/Table S3",output_format="word_document")

# Figure 3
source(glue::glue(script_path,"meta_study_selection.R"))
source(glue::glue(script_path,"figure3.R"))

# Table 2
max_sero<-c(0.75,0.85)
max_cases<-0.02
p1<-time_plot_function("Eastern Africa",dataset="sero_nat_pred2")
p4<-time_plot_function("Western Africa",dataset="sero_nat_pred2")
max_cases<-0.05
p3<-time_plot_function("Southern Africa",dataset="sero_nat_pred2")
p2<-time_plot_function("Middle Africa",dataset="sero_nat_pred2")
pdf("figures/Figure 3.pdf",height=6.5,width=10.5)
plot_grid(p1, p3, p4, p2, ncol=2, nrow=2)
ggsave("figures/Figure 3.png",height=6.5,width=10.5)

dev.off()

# Figure 4, Figure S3
source(glue::glue(script_path,"meta_regression.R"))
#source(glue::glue(script_path,"subgroup_analysis.R"))
source(glue::glue(script_path,"figure s2.R"))

pdf("figures/Figure 4.pdf",width=7,height=8)
plot_grid(#subgroup_analysis, 
          meta_reg, nrow=2, rel_heights=c(0.9,1.1))
ggsave("figures/Figure 4.png",width=7,height=6)
dev.off()

# pdf("figures/Figure S2.pdf", height=2.5,width=8)
# plot_grid(age_plot,sex_plot,ncol=2,align='h')
# ggsave("figures/Figure S2.png")
# dev.off()

