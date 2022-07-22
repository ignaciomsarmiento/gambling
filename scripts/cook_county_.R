##############################################################
#                                                            #
# Authors: Nicolas Bottan, Ignacio Sarmiento and Andres Ham  #
# Paper: Cant stop the One-Armed Bandits                    #
#         The effects of access to Gambling in Crime         #
#                                                            #
##############################################################

rm(list=ls())

# Directory

setwd("C:/Users/bpvel/Desktop/Analysis")

# Packages required

  # install.packages("rgdal")
  # install.packages("RSocrata")
  # install.packages("acs")
  # install.packages("viridis")
  # install.packages("magrittr")
  # install.packages("tidycensus")
  # install.packages("readxl")


require("tidycensus", quietly = TRUE)
require("sf", quietly = TRUE)
require("dplyr", quietly = TRUE)
require("ggplot2", quietly = TRUE)


library(foreign)
library(rgdal)
library(sf)
library(dplyr)



# Reading Chicago data with RSocrata

  # chicago <- RSocrata::read.socrata(
    # "https://data.cityofchicago.org/resource/bt9m-d2mf.json",
    # app_token = "L7sBjYbx4MH0hUCYCU5BESLIF",
    # email = "srmntbr2@illinois.edu",
    #password = ":Gvjj[obeP6a"
    #)

  # saveRDS(object = chicago, file = "chicago.rds")

chicago <- readRDS("chicago.rds")

# Charging tidycensus

  # tidycensus_ <- tidycensus::census_api_key("8337396f6389748bdff0624f2dcd6f1ce10e3113", install =T, overwrite = T)

  # saveRDS(object = tidycensus_, file = "tidy_api_key.rds")

tidycensus <- readRDS("tidy_api_key.rds")

# Getting blockgroups information

load_variables(year=2010, dataset = "sf1", cache = T)

blockgroups <- tidycensus::get_decennial(geography="block group", variables="H001001", state="IL",
                                         county="Cook County", year=2010, geometry=T)

# Map of chicago + blockgroups

cook_bg <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = chicago, ggplot2::aes(geometry = chicago$the_geom.coordinates), fill = NA)+
  ggplot2::geom_sf(data = blockgroups, ggplot2::aes(geometry = blockgroups$geometry), show.legend = F)

plot(cook_bg)

# Dataframe with community areas and sides

  #setwd("/Users/iggy/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Ignacio-Sarmiento/Gambling/")

sides <- readxl::read_excel("Data/community_areas_sides.xlsx")


# Boundaries - Census Blocks - 2010 - Downloaded on 02/06/22 from:
  # https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Blocks-2010/mfzt-js4n

blocks_shp <- read_sf("Data/Boundaries - Census Blocks - 2010/
                      geo_export_fb4ce0e7-7a9a-4f5d-9b88-961cb798be53.shp")


# Boundaries - Community Areas - 2010 - Downloaded on 30/06/22 from: 
  # https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6

community_areas_shp <- read_sf("Data/Boundaries - Community Areas/
                               geo_export_3646c425-a3eb-42a1-964a-87e800c0e605.shp")

#---------------------------------------------------------------------------------------------------------------#

# Community areas + sides

community_areas_sides <- left_join(community_areas_shp, sides, by = c("community"="community"))

community_areas_sides$community[is.na(community_areas_sides$side)]


# st_join blocks_shp + community_areas_sides

sf::sf_use_s2(FALSE)

join_map <- st_join(blocks_shp, community_areas_sides, join = st_intersects) 

# st_join join_map + blockgroups

st_crs(join_map) <- 4269

final_join <- st_join(join_map, blockgroups, join = st_intersects)


#---------------------------------------------------------------------------------------------------------------#

# Counties bordering Cook County 

  # Lake County

  # McHenry County

  # Kane County 

  # DuPage County

  # Will County

#---------------------------------------------------------------------------------------------------------------#

# Map with ggplot chicago + blockgroups

ggplot2::ggplot() +
  ggplot2::geom_sf(data = chicago, ggplot2::aes(geometry = chicago$the_geom.coordinates), fill = NA)+
  ggplot2::geom_polygon(data = blocks_shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

#---------------------------------------------------------------------------------------------------------------#

# Crimes 

crime_2010 <- readRDS("Data/crime_data/crimes_2010_download_07_Jan_2022_09.15.Rds")

