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


#########slope extraction from a linear model in R##########
#e.g. if you were trying to estimate a linear effect of temp on yields for each state,
#holding year constant
##########################################################
##load some data
library(ggplot2)
data(data)
diamonds$color <-factor(diamonds$color, order=F) #reset to factor var
##full mod
#for your data replace price =yield, color=admin unit, and table= year and carat=temperature
#we set intercept to -1 for convenience
model.all<-lm(price~color*carat+color*table-1, data=data)
model.all <- lm(value~year*water+region*year-1, data=data)
##subset model
#e.g.  the data for diamond colour E (e.g. or in your case a particular state).
sub.e<-base::subset(data, region=="central")
model.E<-lm(value~water, table, data=sub.e)
##you can use both approaches, eg. either running the model on each colour (state)
#or running the complete model, 
#note the coefficients from the two approaches
coef(model.E)
coef(model.all)
##to reconstruct the coefficents for effect of carat (temp) on price (yield), for colour D
#do this...because D is the base value
#note R uses Helmert contrasts by default and is alphabetically ordered
coef(model.all)['carat']
#so for color E you would do this
coef(model.all)['carat']+coef(model.all)['colorE:carat']
#And colour F this..
coef(model.all)['carat']+coef(model.all)['colorF:carat']
#this is tedious... 
#you can do this all in one go with the effects package.
library(effects)
ef.mod.all<-effect('region:water', model.all)
summary(ef.mod.all)
#because the effect is linear it is simple
#for your application just change the index for the columns (4,3)
#to those representing columns with unit changes in your predictor
summary(ef.mod.all)$effect[,4]-summary(ef.mod.all)$effect[,3]
#which is the same as our manual estimate from the full
coef(model.all)['carat']+coef(model.all)['colorE:carat']
#and individual model for E
coef(model.E)[2]


