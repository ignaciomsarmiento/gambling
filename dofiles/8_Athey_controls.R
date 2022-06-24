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
#setwd("~/Mount_Ragnar/Dropbox/Phd Illinois/Research/gambling/")


#devtools::install_github('xuyiqing/panelView')  
#install.packages("gsynth")


#from https://yiqingxu.org/software/gsynth/gsynth_examples.html
dta<-read_dta("data/prepared_data.dta")


dta <- dta %>% mutate(violent=asinh(violent),
                      property=asinh(property))
#panelView(violent ~ within2, data = dta,  index = c("id","time"), pre.post = TRUE) 
#panelView(violent ~ within2, data = dta,  index = c("id","time"), type = "outcome") 

dta<- dta %>% mutate(population_2000_time=population_2000*time,
                     black_2000_time=black_2000*time,
                     hispanic_2000_time=hispanic_2000*time,
                     pop_15_35_2000_time=pop_15_35_2000*time,
                     hh_vacant_2000_time=hh_vacant_2000*time
)


x<-felm(violent~within2
            +mindistrb+mindistrb2 
            +population_2000_time+black_2000_time+hispanic_2000_time+pop_15_35_2000_time+hh_vacant_2000_time | factor(id) + factor(time)  , dta)
#stargazer::stargazer(x,type="text")
summary(x)


dta<-data.frame(dta)
out_viol <- gsynth(violent ~ within2 +mindistrb+mindistrb2 
                   +population_2000_time+black_2000_time+hispanic_2000_time+pop_15_35_2000_time+hh_vacant_2000_time, data = dta, estimator = "mc", index = c("id","time"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "nonparametric", nboots = 500, parallel = TRUE, cores = 124)
print(out_viol)
out_viol$att.avg/mean(dta$violent)
#plot(out_viol, theme.bw = TRUE) 

out_prop <- gsynth(property ~ within2+mindistrb+mindistrb2 
                   +population_2000_time+black_2000_time+hispanic_2000_time+pop_15_35_2000_time+hh_vacant_2000_time, data = dta, estimator = "mc", index = c("id","time"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "nonparametric", nboots = 500, parallel = TRUE, cores = 124)
print(out_prop)
out_prop$att.avg/mean(dta$property)

#plot(out_viol, theme.bw = TRUE) 
#plot(out_viol, type = "counterfactual", raw = "none", main="")

save.image("data/Athey_controls.rda")

