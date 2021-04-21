SIAP <- read.csv("/Users/erikaluna/Sync/UBC/Research/thesis/Data/Agt_cierre_80_16_equivalenciesFAO-SIAP.csv")
str(SIAP)

SIAP$Cosechada <- sapply(SIAP$Cosechada, as.character)
SIAP[is.na(SIAP$Cosechada)] <- " "
SIAP


SIAP$Cosechada <- as.numeric(as.character(SIAP$Cosechada))
SIAP$Produccion <- as.numeric(as.character(SIAP$Produccion))
SIAP$Rendimiento <- as.numeric(as.character(SIAP$Rendimiento))

SIAP <- SIAP %>% 
  mutate(area = Produccion/Rendimiento)




data <- SIAP %>% 
  #group_by(Estado, Modalidad, YearAgricola) %>% 
  group_by(YearAgricola) %>% 
drop_na() %>% 
  summarise(ag_harv = sum(area))

data <- SIAP2 %>% 
  group_by(year) %>% 
  mutate(n = sum(harvested))

colnames(data) <- c("state", "water", "year", "ag_harv")

data <- data %>% 
  mutate(region = case_when(
    state %in% north_west  ~ "north_west",
    state %in% north_east  ~ "north_east",
    state %in% central_west  ~ "central_west",
    state %in% central  ~ "central",
    state %in% south  ~ "south"
  ))

data <- data %>% 
  #group_by(region, water, year) %>% 
  group_by(region, year) %>% 
  summarise(ag_harv = sum(ag_harv))

perc_area <- data %>% 
  group_by(year) %>% 
  mutate(percent = ag_harv/sum(ag_harv)*100)

perc_area %>% 
  ggplot(aes(year, ag_harv, fill = region)) +
  geom_col()


data %>% 
  ggplot(aes(year, ag_harv)) +
  geom_line() 
  

trece <- SIAP %>% 
  filter(YearAgricola == 2013) 

write.csv(trece, file = "trece.csv")
  
thirteen <- SIAP2 %>% 
  filter(year == 2013) %>% 
  summarise(sum(planted))


