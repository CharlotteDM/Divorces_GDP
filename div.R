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

#changes names of columns "geo" to "Country.Code"
colnames(GDP)[6] <- "Country.Code"
colnames(divorces)[5] <- "Country.Code"
