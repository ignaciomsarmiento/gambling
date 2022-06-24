##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo





#Load Packages
pkg<-c("dplyr","doMC","lfe","haven","ggplot2","ggthemes","stringr","tidyr","gsynth")
lapply(pkg, require, character.only=T)
rm(pkg)

setwd("~/Dropbox/Phd Illinois/Research/gambling/")
#setwd("~/Dropbox/Research/gambling/")


load("data/Athey_controls.rda")

plot(out_viol, type = "gap", xlim = c(-48, 36), ylim=c(-.15,.15))
plot(out_prop, type = "gap", xlim = c(-48, 36), ylim=c(-.15,.15))

plot(out_viol, type = "counterfactual", raw = "all")
plot(out_prop, type = "counterfactual", raw = "all")

#treated
tr<-data.frame(out_viol$Y.tr)
tr$time<-rownames(tr)
tr$time<-factor(tr$time)
tr_long <-gather(tr, id, violent, X405:X2189, factor_key=TRUE)
tr_long$treated<-1
#control
ct<-data.frame(out_viol$Y.ct)
ct$time<-rownames(ct)
ct$time<-factor(ct$time)
colnames(ct)
ct_long <-gather(ct, id, violent,  X405:X2189, factor_key=TRUE)
ct_long$treated<-0

ct_long$violent<-floor(ct_long$violent)
ct_long$violent[ct_long$violent<=0]<-0

dta_counterfactual<-rbind(tr_long,ct_long)

lfe::felm(violent~treated | id + factor(time),dta_counterfactual)


print(out_viol)
plot(out_viol, type='counterfactual')




d_outviol<-data.frame(time=names(out_viol[['att']]),att=out_viol[['att']])
d_outviol$time<-as.numeric(as.character(d_outviol$time))
d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(0:12),1,-4))
d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(13,24),2,year))
d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(25,26),3,year))
#d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(37,49),4,year))
d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(-2,-12),-1,year))
d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(-13,-24),-2,year))
d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(-25,-36),-3,year))
#d_outviol<-d_outviol %>% mutate(year=ifelse(time%in%seq(-37,-48),-4,year))
d_outviol<-d_outviol %>% mutate(year=ifelse(time==-1,0,year))

s_outviol<-d_outviol %>% group_by(year) %>% summarize(att=mean(att))
                       
s_outviol
