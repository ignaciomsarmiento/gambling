##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo





require("stringr")
require("dplyr")
require("lfe")
require("doMC")



setwd("~/Dropbox/Phd Illinois/Research/gambling/")
#setwd("~/Dropbox/Research/gambling/")


dta<-haven::read_dta("data/prepared_data.dta")


dta <- dta %>% mutate(violent=asinh(violent),
                      property=asinh(property))

dta<- dta %>% mutate(population_2000_time=population_2000*time,
                     pop_density_2000_time=pop_density_2000*time,
                     med_age_2000_time=med_age_2000*time,
                     black_2000_time=black_2000*time,
                     hispanic_2000_time=hispanic_2000*time,
                     pop_15_35_2000_time=pop_15_35_2000*time,
                     hh_vacant_2000_time=hh_vacant_2000*time,
                     renter_p_time=renter_p*time
)

                              
                              

true <- felm(property ~ within2 + mindistrb + mindistrb2 + population_2000_time + med_age_2000_time + black_2000_time + hispanic_2000_time + hh_vacant_2000_time + renter_p_time | factor(id) + factor(time) + factor(comarea):time |0| id ,data=dta)

stargazer::stargazer(true,type="text")

true$estimate[1]


treated_blgid<-unique(dta$id[dta$within2==1])
control_blgid<-unique(dta$id[dta$within2==0])
n1<-length(treated_blgid)
n2<-length(control_blgid)
n1+n2
s<-unique(c(treated_blgid,control_blgid))
length(s)
length(unique(dta$blgid))


set.seed(1001)


blgid<-c(treated_blgid,control_blgid)


#test_dta<-dta[,c("id","within2","property","time")]
test_dta<-dta
test_dta<- test_dta %>% mutate(treated=ifelse(id%in%treated_blgid,1,0),
                               post=within2)

#stuck here, I think I need to shufle the treatment status, and keep the post period equal
#Something more random it would be to assing treatment randombly by month, keeping the number of treated equal to the original, but with the extra restriction that once it turns on keeps on going


nsim<-1000

beta_property<-rep(NA,nsim)
beta_violent<-rep(NA,nsim)

for(i in 1:nsim){
  print(paste("n simul", i,sep=" "))
  
  test_dta<- test_dta %>% group_by(id) %>% dplyr::mutate(post=ifelse(time>=sample(1:127,1),1,0)) %>% ungroup()

  
  test_dta<- test_dta %>% mutate(interaction=treated*post)
  
  beta_property[i]<- felm(property ~ interaction + mindistrb + mindistrb2 + population_2000_time + med_age_2000_time + black_2000_time + hispanic_2000_time + hh_vacant_2000_time + renter_p_time | factor(id) + factor(time) + factor(comarea):time |0| id ,data=test_dta)$beta[1]
  
  beta_violent[i]<- felm(violent ~ interaction + mindistrb + mindistrb2 + population_2000_time + med_age_2000_time + black_2000_time + hispanic_2000_time + hh_vacant_2000_time + renter_p_time | factor(id) + factor(time) + factor(comarea):time |0| id ,data=test_dta)$beta[1]
  
  test_dta$interaction<-NULL
  
  #print(paste("n simul", i,"Beta",beta ,sep=" "))
  
}

# beta_property
# beta_violent

save.image("data/panel_fisher_1000.Rdata")
#load("adjacency/Data/panel_fisher.Rdata")
