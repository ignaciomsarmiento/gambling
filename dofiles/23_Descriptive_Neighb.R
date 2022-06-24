##########################################################
# author: Ignacio Sarmiento-Barbieri

##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list( "gsynth","haven","panelView","dplyr","lfe")
lapply(pkg, require, character.only=T)
rm(pkg)

#Wd
setwd("~/Dropbox/Phd\ Illinois/Research/gambling/")



dta<-read_dta("data/prepared_data_allchicago.dta")


crimes_year<-dta %>% group_by(year) %>% 
  summarize(violent=sum(violent),
            property=sum(property), .groups ="drop")

dta_pre<- dta %>% filter(year<2012)
table(dta_pre$year)

sum_dta_city<-dta_pre %>% 
  summarize(violent=sum(violent),
            property=sum(property))

sum_dta<-dta_pre %>% group_by(comarea_name) %>% 
          summarize(violent=sum(violent)/200342,
                 property=sum(property)/755192, .groups ="drop")

sum_dta_side<-dta_pre %>% group_by(sidename) %>% 
  summarize(violent=sum(violent)/200342,
            property=sum(property)/755192, .groups ="drop")


                 
