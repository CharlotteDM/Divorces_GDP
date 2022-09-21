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

