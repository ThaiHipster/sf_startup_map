# Gobal File: Final Project
#https://raw.githubusercontent.com/taubergm/unicorns/master/unicorn_investors.csv

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)

#Potential Libraries
### Notes adapted from those found at https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html

library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(mapproj)
library(broom)
library(rgeos)
library(cartogram)
library(maptools)
library(janitor)
library(readxl)


#### 1. User Choices -------------------------------
location_options <- c("SF" ) #, Global, Austin, New York, DC add in later  add in later 
color_options <- c("Industry","Value")
data_options <- c("Startup","VC","y-combinator") # "Angel" add in later
size_options <- c("Yes","No")

#### 2. Data Import  --------------------------------------------

SF_map <- readOGR(dsn = ".",  layer="geo_export_4c3f9bb2-c4b8-4fd5-8c2f-db8d32fc6c34")

SF_startup <- read_csv("silicon-valley-companies.csv")
SF_startup <- filter(SF_startup, Longitude != 0)
SF_startup <- filter(SF_startup, Latitude != 0)
SF_startup <- SF_startup[,-11]
names(SF_startup)[8] <- "lat"
names(SF_startup)[9] <- "long"

SF_vc <- read_csv("venture-capital.csv")
SF_vc <- filter(SF_vc, Longitude != 0)
SF_vc <- filter(SF_vc, Latitude != 0)
SF_vc <- SF_vc[,-11]
names(SF_vc)[8] <- "lat"
names(SF_vc)[9] <- "long"

SF_ycomb <- read_csv("y-combinator-companies.csv")
SF_ycomb <- filter(SF_ycomb, City != "New York")
SF_ycomb <- SF_ycomb[,-11]
names(SF_ycomb)[8] <- "lat"
names(SF_ycomb)[9] <- "long"

startup_funding_data <- read_excel("startup_funding_2.xlsx",sheet = "Companies")%>%
  janitor::clean_names()
colnames(startup_funding_data)[2] <- "Company Name"

sp_500_data <- read_csv("sp_500_data.csv")%>%
  janitor::clean_names()
colnames(sp_500_data)[2] <- "Company Name"


#### 3. Data Wrangle  --------------------------------------------


# 3.1 Filtering out VCs from Startups

SF_startup <- anti_join(SF_startup,SF_vc)

# 3.2 Adding in type values to datasets

SF_startup <- SF_startup %>%
  mutate( type = "Startup")

SF_vc <- SF_vc %>%
  mutate(type = "VC")

SF_ycomb <- SF_ycomb %>%
  mutate( type = "y-combinator")


# 3.4 Combining Datasets
full_data <- rbind(SF_startup,SF_vc)
full_data <- rbind(full_data,SF_ycomb)

# 3.3 Adding in Funding Amount

joined_data <- left_join(full_data,startup_funding_data, by = c("Company Name" = "Company Name"))
joined_data <- left_join(joined_data, sp_500_data, by = c("Company Name" = "Company Name"))

head(joined_data)

joined_data <- joined_data %>%
  select(`Employbl Company ID`,`Company Name`, Website, `Address 1`, City,  State, Zip, lat, long, `Company Description`, type, funding_total_usd, market_cap)

joined_data$market_cap <- ifelse(is.na(joined_data$market_cap),joined_data$funding_total_usd, joined_data$market_cap)

joined_data <- joined_data %>%
  select(-market_cap)

full_data <- joined_data






