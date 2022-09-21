library("dplyr")
library("tidyverse")
library("ggplot2")
library("gganimate")
library("ggrepel")
library("rworldmap")
library("rgeos")
library("RColorBrewer")
library("knitr")
library("plotly")
library("htmlwidgets")
library("GGally")
library("stats")
library("rstudioapi")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#data source: https://ec.europa.eu/eurostat/databrowser/view/demo_ndivind/default/table?lang=en
divorces <- read.csv("data/divorces_crudo.csv", 
                      stringsAsFactors = F)

#data source: https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en
GDP <- read.csv("data/GDP_EURO_per_capita.csv", 
                stringsAsFactors = F)

#data source: https://gist.github.com/stevewithington/20a69c0b6d2ff846ea5d35e5fc47f26c#file-country-and-continent-codes-list-csv-csv)%20)
continents <- read.csv("data/continents.csv",
                       stringsAsFactors = F)
colnames(continents)[5] <- "Country.Code"

#changes names of columns 
colnames(GDP)[6] <- "Country.Code"
colnames(GDP)[8] <- "gdp_percapita"
colnames(divorces)[5] <- "Country.Code"
colnames(divorces)[7] <- "divorces_crudo"

#changes country code from 2 letters to three letters
GDP$Country.Code[GDP$Country.Code == "AT"] <- "AUT"
GDP$Country.Code[GDP$Country.Code == "AL"] <- "ALB"
GDP$Country.Code[GDP$Country.Code == "BE"] <- "BEL"
GDP$Country.Code[GDP$Country.Code == "BG"] <- "BGR"
GDP$Country.Code[GDP$Country.Code == "CH"] <- "CHE"
GDP$Country.Code[GDP$Country.Code == "CY"] <- "CYP"
GDP$Country.Code[GDP$Country.Code == "CZ"] <- "CZE"
GDP$Country.Code[GDP$Country.Code == "DE"] <- "DEU"
GDP$Country.Code[GDP$Country.Code == "DK"] <- "DNK"
GDP$Country.Code[GDP$Country.Code == "EE"] <- "EST"
GDP$Country.Code[GDP$Country.Code == "EL"] <- "GRC"
GDP$Country.Code[GDP$Country.Code == "ES"] <- "ESP"
GDP$Country.Code[GDP$Country.Code == "FI"] <- "FIN"
GDP$Country.Code[GDP$Country.Code == "FR"] <- "FRA"
GDP$Country.Code[GDP$Country.Code == "HR"] <- "HRV"
GDP$Country.Code[GDP$Country.Code == "HU"] <- "HUN"
GDP$Country.Code[GDP$Country.Code == "IE"] <- "IRL"
GDP$Country.Code[GDP$Country.Code == "IS"] <- "ISL"
GDP$Country.Code[GDP$Country.Code == "IT"] <- "ITA"
GDP$Country.Code[GDP$Country.Code == "LI"] <- "LIE"
GDP$Country.Code[GDP$Country.Code == "LT"] <- "LTU"
GDP$Country.Code[GDP$Country.Code == "LU"] <- "LUX"
GDP$Country.Code[GDP$Country.Code == "LV"] <- "LVA"
GDP$Country.Code[GDP$Country.Code == "ME"] <- "MNE"
GDP$Country.Code[GDP$Country.Code == "MK"] <- "MKD"
GDP$Country.Code[GDP$Country.Code == "MT"] <- "MLT"
GDP$Country.Code[GDP$Country.Code == "NL"] <- "NLD"
GDP$Country.Code[GDP$Country.Code == "NO"] <- "NOR"
GDP$Country.Code[GDP$Country.Code == "PL"] <- "POL"
GDP$Country.Code[GDP$Country.Code == "PT"] <- "PRT"
GDP$Country.Code[GDP$Country.Code == "RO"] <- "ROU"
GDP$Country.Code[GDP$Country.Code == "RS"] <- "SRB"
GDP$Country.Code[GDP$Country.Code == "SE"] <- "SWE"
GDP$Country.Code[GDP$Country.Code == "SI"] <- "SVN"
GDP$Country.Code[GDP$Country.Code == "SK"] <- "SVK"
GDP$Country.Code[GDP$Country.Code == "TR"] <- "TUR"
GDP$Country.Code[GDP$Country.Code == "UK"] <- "GBR"

divorces$Country.Code[divorces$Country.Code == "AT"] <- "AUT"
divorces$Country.Code[divorces$Country.Code == "AM"] <- "ARM"
divorces$Country.Code[divorces$Country.Code == "AL"] <- "ALB"
divorces$Country.Code[divorces$Country.Code == "AZ"] <- "AZE"
divorces$Country.Code[divorces$Country.Code == "BA"] <- "BIH"
divorces$Country.Code[divorces$Country.Code == "BE"] <- "BEL"
divorces$Country.Code[divorces$Country.Code == "BG"] <- "BGR"
divorces$Country.Code[divorces$Country.Code == "BY"] <- "BLR"
divorces$Country.Code[divorces$Country.Code == "CH"] <- "CHE"
divorces$Country.Code[divorces$Country.Code == "CY"] <- "CYP"
divorces$Country.Code[divorces$Country.Code == "CZ"] <- "CZE"
divorces$Country.Code[divorces$Country.Code == "DE"] <- "DEU"
divorces$Country.Code[divorces$Country.Code == "DK"] <- "DNK"
divorces$Country.Code[divorces$Country.Code == "EE"] <- "EST"
divorces$Country.Code[divorces$Country.Code == "EL"] <- "GRC"
divorces$Country.Code[divorces$Country.Code == "ES"] <- "ESP"
divorces$Country.Code[divorces$Country.Code == "GE"] <- "GEO"
divorces$Country.Code[divorces$Country.Code == "FI"] <- "FIN"
divorces$Country.Code[divorces$Country.Code == "FR"] <- "FRA"
divorces$Country.Code[divorces$Country.Code == "HR"] <- "HRV"
divorces$Country.Code[divorces$Country.Code == "HU"] <- "HUN"
divorces$Country.Code[divorces$Country.Code == "IE"] <- "IRL"
divorces$Country.Code[divorces$Country.Code == "IS"] <- "ISL"
divorces$Country.Code[divorces$Country.Code == "IT"] <- "ITA"
divorces$Country.Code[divorces$Country.Code == "LI"] <- "LIE"
divorces$Country.Code[divorces$Country.Code == "LT"] <- "LTU"
divorces$Country.Code[divorces$Country.Code == "LU"] <- "LUX"
divorces$Country.Code[divorces$Country.Code == "LV"] <- "LVA"
divorces$Country.Code[divorces$Country.Code == "MD"] <- "MDA"
divorces$Country.Code[divorces$Country.Code == "ME"] <- "MNE"
divorces$Country.Code[divorces$Country.Code == "MK"] <- "MKD"
divorces$Country.Code[divorces$Country.Code == "MT"] <- "MLT"
divorces$Country.Code[divorces$Country.Code == "NL"] <- "NLD"
divorces$Country.Code[divorces$Country.Code == "NO"] <- "NOR"
divorces$Country.Code[divorces$Country.Code == "PL"] <- "POL"
divorces$Country.Code[divorces$Country.Code == "PT"] <- "PRT"
divorces$Country.Code[divorces$Country.Code == "RO"] <- "ROU"
divorces$Country.Code[divorces$Country.Code == "RS"] <- "SRB"
divorces$Country.Code[divorces$Country.Code == "RU"] <- "RUS"
divorces$Country.Code[divorces$Country.Code == "SE"] <- "SWE"
divorces$Country.Code[divorces$Country.Code == "SI"] <- "SVN"
divorces$Country.Code[divorces$Country.Code == "SK"] <- "SVK"
divorces$Country.Code[divorces$Country.Code == "SM"] <- "SMR"
divorces$Country.Code[divorces$Country.Code == "TR"] <- "TUR"
divorces$Country.Code[divorces$Country.Code == "UA"] <- "UKR"
divorces$Country.Code[divorces$Country.Code == "UK"] <- "GBR"

#The Highest GDP in the Europe in 2021 $ in 2020
GDP2021 <- GDP %>%
  filter(TIME_PERIOD == 2021)

HighGDP2021<- GDP2021 %>%
  filter(gdp_percapita == max(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code, gdp_percapita) 

GDP2020 <- GDP %>%
  filter(TIME_PERIOD == 2020)

HighGDP2020<- GDP2020 %>%
  filter(gdp_percapita == max(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code, gdp_percapita) 

#The Lowest GDP in the Europe in 2021 $ in 2020

LowGDP2021<- GDP2021 %>%
  filter(gdp_percapita == min(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code,gdp_percapita) 

LowGDP2020<- GDP2020 %>%
  filter(gdp_percapita == min(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code,gdp_percapita) 


#The highest divorce rate in the Europe in 2020
divorces2020 <- divorces %>%
  filter(TIME_PERIOD == 2020)

HighDiv2020<- divorces2020 %>%
  filter(divorces_crudo == max(divorces_crudo, na.rm = T)) %>%
  dplyr::select(Country.Code,divorces_crudo) 

#The lowest divorce rate in the Europe in 2020
LowDiv2020<- divorces2020 %>%
  filter(divorces_crudo == min(divorces_crudo, na.rm = T)) %>%
  dplyr::select(Country.Code,divorces_crudo) 

#correlation between GDP and divorce rate
comb_data <- left_join(GDP2020, divorces2020, by = "Country.Code")
cor(comb_data$gdp_percapita, comb_data$divorces_crudo, use = "complete.obs")
