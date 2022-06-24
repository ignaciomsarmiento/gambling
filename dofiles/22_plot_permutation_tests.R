##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo





#Load Packages
pkg<-c("dplyr","doMC","lfe","haven","ggplot2","ggthemes","stringr")
lapply(pkg, require, character.only=T)
rm(pkg)

setwd("~/Dropbox/Phd Illinois/Research/gambling/")
#setwd("~/Dropbox/Research/gambling/")



load("data/panel_fisher_1000.Rdata")

results<-data.frame(violent=beta_violent, property=beta_property)
true_violent<-0.0385
true_property<-0.0602


name_output<-"violent"
ht<-4
wd<-6

colors <- tibble::deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
base_size = 4
base_family = "sans"


#bs<-diff(hist(results$violent)$breaks)

mean(beta_violent>true_violent)

ggplot() +
  geom_histogram(data=results,aes(x=violent),colour="black", fill=colors["Medium Gray"],binwidth =0.005) +
  geom_vline(aes(xintercept=true_violent), colour="royalblue4", linetype="twodash") +  
  xlab("Coeficient Violent Crime Within 2 Blocks (=1)") +
  ylab("Counts") +
  theme_bw() +
  theme_fivethirtyeight() + scale_color_fivethirtyeight("cyl") +
  theme(legend.title= element_blank() ,
        legend.position="none",
        legend.justification=c(1,1),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.box.just = c("top"),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0),
        rect = element_rect(colour = "transparent", fill = "white"))#,
        #axis.title = element_text(), plot.margin = unit(c(2,2,1,1), "lines"))
ggsave(filename=paste0("~/Dropbox/Apps/ShareLaTeX/Gambling/material/Figures/ihs/","fisher_violent",".png"), height = ht, width = wd)



mean(beta_property>true_property)
ggplot() +
  geom_histogram(data=results,aes(x=property),colour="black", fill=colors["Medium Gray"],binwidth =0.005) +
  geom_vline(aes(xintercept=true_property), colour="royalblue4", linetype="twodash") +  
  xlab("Coeficient Property Crime Within 2 Blocks (=1)") +
  ylab("Counts") +
  theme_bw() +
  theme_fivethirtyeight() + scale_color_fivethirtyeight("cyl") +
  theme(legend.title= element_blank() ,
        legend.position="none",
        legend.justification=c(1,1),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.box.just = c("top"),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0),
        rect = element_rect(colour = "transparent", fill = "white"))#,
#axis.title = element_text(), plot.margin = unit(c(2,2,1,1), "lines"))
ggsave(filename=paste0("~/Dropbox/Apps/ShareLaTeX/Gambling/material/Figures/ihs/","fisher_property",".png"), height = ht, width = wd)

