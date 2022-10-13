library("dplyr")
library("tidyverse")
library("ggplot2")
library("gganimate")
library("ggrepel")
library("rworldmap")
library("rgeos")
library("RColorBrewer")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("plotly")
library("GGally")
library("stats")
library("rstudioapi")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#data source: https://ec.europa.eu/eurostat/databrowser/view/demo_ndivind/default/table?lang=en
divorcesper100 <- read.csv("data/divorces_per100.csv", 
                           stringsAsFactors = F)


#data source: https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en
GDP <- read.csv("data/GDP_EURO_per_capita.csv", 
                stringsAsFactors = F)

GDP_PPS <- read.csv("data/GDP_PPS.csv", 
                  stringsAsFactors = F )

GNI <- read.csv("data/GNI.csv", 
                stringsAsFactors = F )

#data source: https://gist.github.com/stevewithington/20a69c0b6d2ff846ea5d35e5fc47f26c#file-country-and-continent-codes-list-csv-csv)%20)
continents <- read.csv("data/continents.csv",
                       stringsAsFactors = F)
colnames(continents)[5] <- "Country.Code"




#removes some unnecessary rows in data frames
GDP <- GDP[-c(198:219, 286:327), ]
divorces <- divorces[-c(101:110, 121:132, 143:145, 163:180), ]


#changes names of columns 
colnames(GDP)[6] <- "Country.Code"
colnames(GDP)[8] <- "gdp_percapita"
colnames(divorcesper100)[5] <- "Country.Code"
colnames(divorcesper100)[7] <- "per100"
colnames(GDP_PPS)[6] <- "Country.Code"
colnames(GDP_PPS)[8] <- "gdp_pps"
colnames(GNI)[5] <- "Country.Code"
colnames(GNI)[7] <- "gni"

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

GDP_PPS$Country.Code[GDP_PPS$Country.Code == "AT"] <- "AUT"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "AL"] <- "ALB"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "BE"] <- "BEL"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "BG"] <- "BGR"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "CH"] <- "CHE"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "CY"] <- "CYP"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "CZ"] <- "CZE"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "DE"] <- "DEU"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "DK"] <- "DNK"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "EE"] <- "EST"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "EL"] <- "GRC"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "ES"] <- "ESP"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "FI"] <- "FIN"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "FR"] <- "FRA"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "HR"] <- "HRV"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "HU"] <- "HUN"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "IE"] <- "IRL"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "IS"] <- "ISL"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "IT"] <- "ITA"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "LI"] <- "LIE"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "LT"] <- "LTU"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "LU"] <- "LUX"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "LV"] <- "LVA"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "ME"] <- "MNE"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "MK"] <- "MKD"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "MT"] <- "MLT"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "NL"] <- "NLD"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "NO"] <- "NOR"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "PL"] <- "POL"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "PT"] <- "PRT"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "RO"] <- "ROU"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "RS"] <- "SRB"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "SE"] <- "SWE"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "SI"] <- "SVN"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "SK"] <- "SVK"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "TR"] <- "TUR"
GDP_PPS$Country.Code[GDP_PPS$Country.Code == "UK"] <- "GBR"

GNI$Country.Code[GNI$Country.Code == "AT"] <- "AUT"
GNI$Country.Code[GNI$Country.Code  == "AL"] <- "ALB"
GNI$Country.Code[GNI$Country.Code  == "BE"] <- "BEL"
GNI$Country.Code[GNI$Country.Code  == "BG"] <- "BGR"
GNI$Country.Code[GNI$Country.Code  == "CH"] <- "CHE"
GNI$Country.Code[GNI$Country.Code  == "CY"] <- "CYP"
GNI$Country.Code[GNI$Country.Code  == "CZ"] <- "CZE"
GNI$Country.Code[GNI$Country.Code  == "DE"] <- "DEU"
GNI$Country.Code[GNI$Country.Code  == "DK"] <- "DNK"
GNI$Country.Code[GNI$Country.Code  == "EE"] <- "EST"
GNI$Country.Code[GNI$Country.Code  == "EL"] <- "GRC"
GNI$Country.Code[GNI$Country.Code  == "ES"] <- "ESP"
GNI$Country.Code[GNI$Country.Code  == "FI"] <- "FIN"
GNI$Country.Code[GNI$Country.Code  == "FR"] <- "FRA"
GNI$Country.Code[GNI$Country.Code  == "HR"] <- "HRV"
GNI$Country.Code[GNI$Country.Code  == "HU"] <- "HUN"
GNI$Country.Code[GNI$Country.Code  == "IE"] <- "IRL"
GNI$Country.Code[GNI$Country.Code  == "IS"] <- "ISL"
GNI$Country.Code[GNI$Country.Code  == "IT"] <- "ITA"
GNI$Country.Code[GNI$Country.Code  == "LI"] <- "LIE"
GNI$Country.Code[GNI$Country.Code  == "LT"] <- "LTU"
GNI$Country.Code[GNI$Country.Code  == "LU"] <- "LUX"
GNI$Country.Code[GNI$Country.Code  == "LV"] <- "LVA"
GNI$Country.Code[GNI$Country.Code  == "ME"] <- "MNE"
GNI$Country.Code[GNI$Country.Code  == "MK"] <- "MKD"
GNI$Country.Code[GNI$Country.Code  == "MT"] <- "MLT"
GNI$Country.Code[GNI$Country.Code == "NL"] <- "NLD"
GNI$Country.Code[GNI$Country.Code  == "NO"] <- "NOR"
GNI$Country.Code[GNI$Country.Code  == "PL"] <- "POL"
GNI$Country.Code[GNI$Country.Code  == "PT"] <- "PRT"
GNI$Country.Code[GNI$Country.Code  == "RO"] <- "ROU"
GNI$Country.Code[GNI$Country.Code  == "RS"] <- "SRB"
GNI$Country.Code[GNI$Country.Code  == "SE"] <- "SWE"
GNI$Country.Code[GNI$Country.Code == "SI"] <- "SVN"
GNI$Country.Code[GNI$Country.Code == "SK"] <- "SVK"
GNI$Country.Code[GNI$Country.Code == "TR"] <- "TUR"
GNI$Country.Code[GNI$Country.Code == "UK"] <- "GBR"





divorcesper100$Country.Code[divorcesper100$Country.Code == "AT"] <- "AUT"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "AM"] <- "ARM"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "AL"] <- "ALB"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "AZ"] <- "AZE"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "BA"] <- "BIH"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "BE"] <- "BEL"
divorcesper100$Country.Code[divorcesper100$Country.Code == "BG"] <- "BGR"
divorcesper100$Country.Code[divorcesper100$Country.Code == "BY"] <- "BLR"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "CH"] <- "CHE"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "CY"] <- "CYP"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "CZ"] <- "CZE"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "DE"] <- "DEU"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "DK"] <- "DNK"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "EE"] <- "EST"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "EL"] <- "GRC"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "ES"] <- "ESP"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "GE"] <- "GEO"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "FI"] <- "FIN"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "FR"] <- "FRA"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "HR"] <- "HRV"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "HU"] <- "HUN"
divorcesper100$Country.Code[divorcesper100$Country.Code == "IE"] <- "IRL"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "IS"] <- "ISL"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "IT"] <- "ITA"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "LI"] <- "LIE"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "LT"] <- "LTU"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "LU"] <- "LUX"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "LV"] <- "LVA"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "MD"] <- "MDA"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "ME"] <- "MNE"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "MK"] <- "MKD"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "MT"] <- "MLT"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "NL"] <- "NLD"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "NO"] <- "NOR"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "PL"] <- "POL"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "PT"] <- "PRT"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "RO"] <- "ROU"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "RS"] <- "SRB"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "RU"] <- "RUS"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "SE"] <- "SWE"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "SI"] <- "SVN"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "SK"] <- "SVK"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "SM"] <- "SMR"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "TR"] <- "TUR"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "UA"] <- "UKR"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "UK"] <- "GBR"
divorcesper100$Country.Code[divorcesper100$Country.Code  == "XK"] <- "KOS"





#The Highest GDP in the Europe in 2021 $ in 2020
GDP2021 <- GDP %>%
  filter(TIME_PERIOD == 2021)

HighGDP2021<- GDP2021 %>%
  filter(gdp_percapita == max(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code, gdp_percapita) 

GDP2020 <- GDP %>%
  filter(TIME_PERIOD == 2020)

GDP_PPS2020 <- GDP_PPS %>%
  filter(TIME_PERIOD == 2020)

GNI2020 <- GNI %>%
  filter(TIME_PERIOD == 2020)

HighGDP2020<- GDP2020 %>%
  filter(gdp_percapita == max(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code, gdp_percapita) 

HighGDP_PPS2020<- GDP_PPS2020 %>%
  filter(gdp_pps == max(gdp_pps, na.rm = T)) %>%
  dplyr::select(Country.Code, gdp_pps) 

HighGNI2020<- GNI2020 %>%
  filter(gni == max(gni, na.rm = T)) %>%
  dplyr::select(Country.Code, gni) 


#The Lowest GDP in the Europe in 2021 $ in 2020

LowGDP2021<- GDP2021 %>%
  filter(gdp_percapita == min(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code,gdp_percapita) 

LowGDP2020<- GDP2020 %>%
  filter(gdp_percapita == min(gdp_percapita, na.rm = T)) %>%
  dplyr::select(Country.Code,gdp_percapita) 



#correlation between GDP  and divorce rate per 100 marriages in 2020
comb_data2 <- left_join(GDP2020, divorcesper100_2020, by = "Country.Code")
cor(comb_data2$gdp_percapita, comb_data2$per100, use = "complete.obs")

#correlation between GDP_PPS  and divorce rate per 100 marriages in 2020
comb_data3 <- left_join(GDP_PPS2020, divorcesper100_2020, by = "Country.Code")
cor(comb_data3$gdp_pps, comb_data3$per100, use = "complete.obs")

#correlation between GNI  and divorce rate per 100 marriages in 2020
comb_data4 <- left_join(GNI2020, divorcesper100_2020, by = "Country.Code")
cor(comb_data4$gni, comb_data3$per100, use = "complete.obs")


#chart: European countries & divorce rate in 2020

chart_div_2020_2 <- ggplot(data = divorcesper100_2020) + geom_col(aes(x = reorder(Country.Code, per100), y = per100, fill = per100)) + 
  scale_fill_gradient(low="lightblue", high="blue") +
  coord_flip() +
  theme_light() +
  labs(
    title = "Divorce in Europe in 2020",
    caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/demo_ndivind/default/table?lang=en,
    https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en)",
    x = "European Countries",
    y = "Number of Divorces per 100 marriages") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.y = element_text(color="royalblue4", size=14, face="bold"),
    legend.position = "none") 




#map: Divorces in Europe in 2020
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]



comb_data_div_map2 <- left_join(divorcesper100_2020, Europe, by = c("Country.Code" = "brk_a3"))



map_divorces_2020_2 <- ggplot(comb_data_div_map2) +
  geom_sf(mapping = aes(geometry = geometry, fill = per100)) +
  scale_fill_gradient(name = "divorces per 100", low = "#FFFFCC", high = "#FF3300", na.value = "grey50") +
  coord_sf(xlim = c(-25,43), ylim = c(32,75), expand = FALSE) +
  labs(title = "Divorces in Europe in 2020",
       subtitle = "per 100 marriages",
       caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/demo_ndivind/default/table?lang=en,
    https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en )") +
  theme(plot.title = element_text(color="red4", size=16, face="bold", hjust = 0.5),
        plot.subtitle = element_text(color="red4", size=12, face="bold", hjust = 0.5),
        legend.background = element_rect(fill="wheat", 
                                         size=0.5, linetype="solid"),
        plot.caption = element_text(face = "italic", hjust = 0.5)
  ) 





# GDP per capita & divorce rate in Europe in 2020 - chart


gg_GDP_divorces_per100 <- ggplot(data = comb_data2) +
  geom_text(mapping = aes(x = gdp_percapita, y = per100, label = Country.Code)) +
  theme_light() +
  labs(
    title = "GDP per capita & Number of Divorces per 100 marriages in Europe in 2020",
    caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/demo_ndivind/default/table?lang=en
    https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en)",
    x = "GDP",
    y = "Number of Divorces per 100 marriages") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
    plot.subtitle = element_text(color="slateblue", size=8, face="italic"),
    plot.caption = element_text(color="deeppink", size=7),
    plot.caption.position = "plot",
    axis.title.x = element_text(color="darkmagenta", size=10, face = "bold"),
    axis.title.y = element_text(color="darkmagenta", size=10, face = "bold")
  ) 


gg_GNI_divorces_per100 <- ggplot(data = comb_data4) +
  geom_text(mapping = aes(x = gni, y = per100, label = Country.Code)) +
  theme_light() +
  labs(
    title = "GNI & Number of Divorces per 100 marriages in Europe in 2020",
    caption = "(based on data from: https://ec.europa.eu/eurostat/databrowser/view/demo_ndivind/default/table?lang=en
    https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en)",
    x = "GNI",
    y = "Number of Divorces per 100 marriages") +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
    plot.subtitle = element_text(color="slateblue", size=8, face="italic"),
    plot.caption = element_text(color="deeppink", size=7),
    plot.caption.position = "plot",
    axis.title.x = element_text(color="darkmagenta", size=10, face = "bold"),
    axis.title.y = element_text(color="darkmagenta", size=10, face = "bold")
  ) 
