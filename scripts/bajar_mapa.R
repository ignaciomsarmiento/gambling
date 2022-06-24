##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



pkg<-list("tidyverse","sf","RSocrata","lubridate")
lapply(pkg, require,  character.only=T)
rm(pkg)


#poner fecha de bajada #esto nos da los blocks en chicago
map <- read.socrata(
  "https://data.cityofchicago.org/resource/bt9m-d2mf.json",
  app_token = "L7sBjYbx4MH0hUCYCU5BESLIF",
  email     = "srmntbr2@illinois.edu",
  password  = ":Gvjj[obeP6a"
)


#bajar de tidycensus todos los blocks de Cook County

#Crear un archivo que haga el subset de los block de cook county con los de chicago (aca quedaria un mapa de chicago)