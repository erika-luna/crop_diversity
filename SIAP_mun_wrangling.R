##### LOAD PACKAGES #####
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

##### LOAD DATA #####
SIAP <- read.csv("/Users/erikaluna/R\ Studio/crop_diversity/SIAP.csv") 

##### DATA WRANGLING #####
SIAP <- SIAP %>% 
  filter(type == "food") # Only interested in food crops

# Add regions 
north_west <- c("baja california", "baja california sur", "nayarit","sinaloa", "sonora")
north_east <- c("chihuahua", "coahuila", "durango","nuevo leon","san luis potosi","tamaulipas", "zacatecas")
central_west <- c("aguascalientes","colima","guanajuato","jalisco","michoacan","queretaro")
central <- c("distrito federal","hidalgo","guerrero","morelos","mexico","puebla","tlaxcala","veracruz")
south <- c("campeche","chiapas","oaxaca","tabasco", "quintana roo","yucatan")

SIAP <- SIAP %>% 
  mutate(region = case_when(
    state %in% north_west  ~ "north_west",
    state %in% north_east  ~ "north_east",
    state %in% central_west  ~ "central_west",
    state %in% central  ~ "central",
    state %in% south  ~ "south"
  ))

SIAP$region <- as.factor(SIAP$region)

# Municipal level
SIAP_mun <- SIAP %>%
  filter(year > 2002) # data is recorded at the mun level after 2003

SIAP_mun <- SIAP_mun %>% # we create a COV_ID column because mun_code is not id
  mutate(COV_ID = group_indices(., state_code, mun_code)) # COV_ID is determined by the state code and mun code
# Look at the municipal boundaries shapefile to understand this (2463 municipalities total)
# For state_code = 1 (aguas), mun_code = 1 (aguas), COV_ID = 1 
# For state_code = 2 (bc), mun_code = 1 (ensenada), COV_ID = 2

SIAP_codes <- SIAP_mun %>% # we save the COV_ID into a data frame 
  select(c("state","mun","state_code", "mun_code","COV_ID")) %>% # we keep only these columns (names and codes)
  distinct()
# SIAP data contains 2436 municipalities. This is, 27 municipalities do not grow anything. 
# SIAP_codes contains 2576 observations (~140 muncipalities repeat; human error like recording the name as "llano el"
# and as "el llano". Therefore, this municipality has the same ID and should not be repeated. 

SIAP$mun[SIAP$mun == "llano el"] <- "el llano"  
SIAP$mun[SIAP$mun == "paz la"] <- "la paz"  
SIAP$mun[SIAP$mun == "cabos los"] <- "los cabos"
SIAP$mun_code[SIAP$mun == "loreto"] <- 9
# Chiapas
SIAP$mun[SIAP$mun == "monte cristo de gro"] <- "montecristo de guerrero"
SIAP$mun[SIAP$mun == "libertad la"] <- "la libertad"
SIAP$mun[SIAP$mun == "independencia la"] <- "la independencia"
SIAP$mun[SIAP$mun == "concordia la"] <- "la concordia"
SIAP$mun[SIAP$mun == "margaritas las"] <- "las margaritas"
SIAP$mun[SIAP$mun == "bosque el"] <- "el bosque"
SIAP$mun[SIAP$mun == "cintalapa de figueroa"] <- "cintalapa"
SIAP$mun[SIAP$mun == "grandeza la"] <- "la grandeza"
SIAP$mun[SIAP$mun == "metapa de dominguez"] <- "metapa"
SIAP$mun[SIAP$mun == "porvenir de velazco suarez el"] <- "el porvenir"
SIAP$mun[SIAP$mun == "simojovel de allende"] <- "simojovel"
SIAP$mun[SIAP$mun == "villa flores"] <- "villaflores"
SIAP$mun[SIAP$mun == "motozintla de mendoza"] <- "motozintla"
SIAP$mun[SIAP$mun == "rosas las"] <- "las rosas"
SIAP$mun[SIAP$mun == "trinitaria la"] <- "la trinitaria"
SIAP$mun[SIAP$mun == "villa flores"] <- "villaflores"
SIAP$mun[SIAP$mun == "villa flores"] <- "villaflores"
# Chihuahua
SIAP$mun[SIAP$mun == "dr. belisario dominguez"] <- "doctor belisario dominguez"
SIAP$mun[SIAP$mun == "cruz la"] <- "la cruz"
SIAP$mun[SIAP$mun == "temosachi"] <- "temosachic"
SIAP$mun[SIAP$mun == "tule el"] <- "el tule"
SIAP$mun[SIAP$mun == "coyame del sotol"] <- "coyame"
# Coahuila
SIAP$mun_code[SIAP$mun == "viesca"] <- 36
SIAP$mun[SIAP$mun == "fco. i. madero"] <- "francisco i. madero"
# Distrito federal
SIAP$mun[SIAP$mun == "cuajimalpa de morelos/cuajimal"] <- "cuajimalpa de morelos"
SIAP$mun[SIAP$mun == "magdalena contreras la"] <- "la magdalena contreras"
# Durango
SIAP$mun[SIAP$mun == "villa hidalgo"] <- "hidalgo"
SIAP$mun[SIAP$mun == "villa ocampo"] <- "ocampo"
SIAP$mun[SIAP$mun == "oro el"] <- "el oro"
# Gto
SIAP$mun[SIAP$mun == "san miguel de allende"] <- "allende"
SIAP$mun[SIAP$mun == "dolores hidalgo cuna de la ind"] <- "dolores hidalgo cuna de la independencia nacional"
SIAP$mun[SIAP$mun == "silao de la victoria"] <- "silao"
# Guerrero
SIAP$mun[SIAP$mun == "cuautepec"] <- "cuauhtepec"
SIAP$mun[SIAP$mun == "apaxtla de castrejon"] <- "apaxtla"
# Hidalgo
SIAP$mun[SIAP$mun == "arenal el"] <- "el arenal"
SIAP$mun[SIAP$mun == "san agustin metzquititlan"] <- "metzquititlan"
SIAP$mun[SIAP$mun == "molango de escamilla"] <- "molango"
SIAP$mun[SIAP$mun == "santiago tulantepec de lugo guerrero"] <- "santiago tulantepec"
SIAP$mun[SIAP$mun == "tulancingo de bravo"] <- "tulancingo"
# Jalisco
SIAP$mun[SIAP$mun == "zocoalco de torres"] <- "zacoalco de torres"
SIAP$mun[SIAP$mun == "san pedro tlaquepaque"] <- "tlaquepaque"
SIAP$mun[SIAP$mun == "salto el"] <- "el salto"
SIAP$mun[SIAP$mun == "la manzanilla de la paz"] <- "manzanilla de la paz la"
SIAP$mun[SIAP$mun == "manuel m. dieguez"] <- "santa maria del oro"
SIAP$mun[SIAP$mun == "limon el"] <- "el limon"
SIAP$mun[SIAP$mun == "huerta la"] <- "la huerta"
SIAP$mun[SIAP$mun == "grullo el"] <- "el grullo"
SIAP$mun[SIAP$mun == "zapotlan el grande (ciudad guzman)"] <- "zapotlan el grande"
SIAP$mun[SIAP$mun == "barca la"] <- "la barca"
SIAP$mun[SIAP$mun == "arenal el"] <- "el arenal"
SIAP$mun[SIAP$mun == "antonio escobedo"] <- "san juanito de escobedo"
# Mexico
SIAP$mun[SIAP$mun == "acambay de ruiz castaneda"] <- "acambay"
SIAP$mun[SIAP$mun == "coacalco de berriozabal"] <- "coacalco"
SIAP$mun[SIAP$mun == "ecatepec de morelos"] <- "ecatepec"
SIAP$mun[SIAP$mun == "mexicalcingo"] <- "mexicaltzingo"
SIAP$mun[SIAP$mun == "oro el"] <- "el oro"
SIAP$mun[SIAP$mun == "paz la"] <- "la paz"
SIAP$mun[SIAP$mun == "valle de chalco solidaridad"] <- "valle de chalco"
# Michoac??n
SIAP$mun[SIAP$mun == "ting????indin"] <- "tinguindin"
SIAP$mun[SIAP$mun == "reyes los"] <- "los reyes"
SIAP$mun[SIAP$mun == "piedad la"] <- "la piedad"
SIAP$mun[SIAP$mun == "huacana la"] <- "la huacana"
# Morelos
SIAP$mun[SIAP$mun == "jojutla de juarez"] <- "jojutla"
SIAP$mun[SIAP$mun == "tlaltizapan de zapata"] <- "tlaltizapan"
SIAP$mun[SIAP$mun == "zacatepec de hidalgo"] <- "zacatepec"
SIAP$mun[SIAP$mun == "zacualpan de amilpas"] <- "zacualpan"
# Nayarit
SIAP$mun[SIAP$mun == "yesca la"] <- "la yesca"
SIAP$mun[SIAP$mun == "nayar el"] <- "del nayar"
# Nuevo le??n
SIAP$mun[SIAP$mun == "aldamas los"] <- "los aldamas"
SIAP$mun[SIAP$mun == "carmen"] <- "el carmen"
SIAP$mun[SIAP$mun == "herrera los"] <- "los herreras"
SIAP$mun[SIAP$mun == "ramones los"] <- "los ramones"
# 
SIAP$mun[SIAP$mun == ""] <- ""

SIAP$mun[SIAP$mun == ""] <- ""


write.csv(SIAP_codes, file = "SIAP_codes.csv")  