##### LOAD PACKAGES #####
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(DT)
library(psych)
library(stringr)
library(scales)

##### LOAD DATA #####
SIAP <- read.csv("/Users/erikaluna/R\ Studio/msc_thesis/SIAP.csv") 

##### Data Wrangling #####


#### Graphing diversity ####

one_crop <- SIAP %>% 
  #filter(type == "food", cycle == "perennial") %>% 
  #filter(type == "food", year > 1994) %>% 
  #filter(crop == "maiz", str_detect(crop_var,'grano')) %>% 
  #filter(crop == "frijol", water == "rainfed") %>% 
  #filter(crop == "sorgo", str_detect(crop_var,'grano'), water == "irrigated") %>% 
  #group_by(state, year, crop) %>% 
  #group_by(state, year) %>% 
  #group_by(year) %>% 
  summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            HAR = round(sum(harvested)/sum(planted), digits = 2))

p1 <- ggplot(one_crop, aes(x = state, fill = crop)) +
  geom_bar(position = "stack") + 
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(legend.position = "none")          
p1

diversity <- one_crop %>% 
  group_by(state, year) %>% 
  count()

ggplot(diversity, aes(year, n, group = state, colour = state)) +
  geom_line() +
  #geom_tile() #+
  #scale_x_discrete(guide = guide_axis(angle = 45))
  theme(legend.position = "none")

ggplot(one_crop, aes(year, ag_planted, group = state, colour = state)) +
#ggplot(one_crop, aes(year, ag_planted)) +
  geom_line() 

ggplot(one_crop, aes(x=year)) + 
  geom_line(aes(y = ag_planted), color = "darkred") + 
  geom_line(aes(y = ag_harv), color="steelblue") +
  scale_y_continuous(labels = comma) +
  ylab("Area (ha)") +
  ggtitle("Beans rainfed")

one_crop <- drop_na(one_crop)
yields_south <- drop_na(one_crop)

m0 <- lm(n~state*year-1, data = diversity)
#m0 <- lm(HAR~state*year-1, data = one_crop)
#m0 <- lm(HAR~state*year-1, data = yields_south)
predict(m0)
diversity$simplemodel <- predict(m0)
#one_crop$simplemodel <- predict(m0)
#yields_south$simplemodel <- predict(m0)


#model.all<-lm(n~state*year-1, data = diversity)
model.all<-lm(HAR~state*year-1, data = one_crop)
summary(model.all)


ggplot(diversity, aes(year, simplemodel, group = state, colour = state)) +
  #ggplot(one_crop, aes(year, simplemodel, group = state, colour = state)) +
  #ggplot(yields_south, aes(year, simplemodel, group = state, colour = state)) +
  geom_line() +
  theme(legend.position = "none")  


diversity %>% 
  filter(state == "coahuila") %>% 
  ggplot(aes(year, simplemodel)) +
  geom_line()




#### Diversity indices ####

crop <- SIAP %>% 
  filter(crop == "maiz") #%>% 
  #filter(str_detect(crop_var,'grano'))

crop_div <- SIAP %>% 
  group_by(state, year, crop) %>% 
  count()

unique(crop$crop_var)



crop_div <- crop_div %>% 
  group_by(state, year) %>% 
  count() #%>% 
  ggplot(aes(year, n, group = state, colour = state)) +
  geom_line() +
  theme(legend.position = "none")
  
  
aguas <- SIAP %>% 
  filter(state == "aguascalientes") %>% 
  group_by(year) %>% 
  summarise(ag_harv = sum(harvested),
            ag_plan = sum(planted))

state <- SIAP %>% 
  filter(state == "chiapas") %>% 
  group_by(year) %>% 
  summarise(ag_harv = sum(harvested),
            ag_plan = sum(planted))


aguas %>% 
  ggplot(aes(year, ag_harv)) +
  geom_point() +
  geom_smooth(method = "lm")


library(ggpubr)
theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)

ggplot(state, aes(year, value)) +
geom_point(aes(color = area)) +
  #geom_rug(aes(color =area)) +
  geom_smooth(aes(color = area), method = lm, 
              se = FALSE, fullrange = TRUE)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))#+
  ggpubr::stat_cor(aes(color = area), label.x = 3)


data <- SIAP %>% 
    group_by(state, year) %>% 
    summarise(ag_harv = sum(harvested),
              ag_plan = sum(planted))
  
data <- gather(data, "area", "value", 3:4)  
  
  
  cities = unique(data$state)
  city_plots = list()
  for(city_ in cities) {
    city_plots[[city_]] = ggplot(data %>% filter(state == city_), aes(x=year, y=value)) +
      geom_point(aes(color = area)) +
      #geom_rug(aes(color =area)) +
      geom_smooth(aes(color = area), method = lm, 
                  se = FALSE, fullrange = TRUE)+
      scale_color_manual(values = c("#00AFBB", "#E7B800"))#+
    
    print(city_plots[[city_]])
    ggsave(city_plots[[city_]], file=paste0("plot_", city_,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
  }
  
  
  
  
  
#### Measuring taxonomic crop alpha diversity ####
library(vegan)

aguas <- SIAP %>% 
  filter(state == "aguascalientes") %>% 
  group_by(state, year, crop) %>% 
  summarise(ag_harv = sum(harvested))

# Matrix
aguas <- aguas %>%  
  spread(crop, ag_harv)

aguas[is.na(aguas)] <- 0 # NA values to cero 

aguas <- as.data.frame(aguas)

tmp <- c("state", "year") # columns to remove
 
aguas <- aguas %>%  # remove columns from the matrix
  select(-one_of(tmp))
  
aguas <- mapply(aguas, FUN=as.integer)

# Indices
H <- diversity(aguas)
simp <- diversity(aguas, "simpson")
invsimp <- diversity(aguas, "inv")
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(aguas, 2) - 1
## Fisher alpha
alpha <- fisher.alpha(aguas)
## Plot all
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(aguas) ## rowSums(BCI > 0) does the same...
J <- H/log(S)

period <- c(1980:2016)

SR_time <- as.data.frame(cbind(period, H)) 
SD_time <- as.data.frame(cbind(period, simp))

plot(SR_time)
plot(SD_time)

ggplot(SR_time, aes(period, H)) +
  geom_point() +
  geom_smooth(method = "lm")


## beta diversity defined as gamma/alpha - 1:
data(dune)
data(dune.env)
alpha <- with(dune.env, tapply(specnumber(dune), Management, mean))
gamma <- with(dune.env, specnumber(dune, Management))
gamma/alpha - 1

 
  


#########slope extraction from a linear model in R##########
#e.g. if you were trying to estimate a linear effect of temp on yields for each state,
#holding year constant
data(one_crop)
diamonds$color <-factor(diamonds$color, order=F) #reset to factor var
##full mod
#for your data replace price =yield, color=admin unit, and table= year and carat=temperature
#we set intercept to -1 for convenience
#model.all<-lm(n~state*year-1, data=diversity)
model.all<-lm(ag_harv~state*year-1, data=one_crop)
##subset model
#e.g.  the data for diamond colour E (e.g. or in your case a particular state).
sub.e<-base::subset(diversity, state=="aguascalientes")
model.E<-lm(n~ year, data=sub.e)
##you can use both approaches, eg. either running the model on each colour (state)
#or running the complete model, 
#note the coefficients from the two approaches
coef(model.E)
coef(model.all)
##to reconstruct the coefficents for effect of carat (temp) on price (yield), for colour D
#do this...because D is the base value
#note R uses Helmert contrasts by default and is alphabetically ordered
coef(model.all)['year']
#so for color E you would do this
coef(model.all)['carat']+coef(model.all)['colorE:carat']
#And colour F this..
coef(model.all)['carat']+coef(model.all)['colorF:carat']
#this is tedious... 
#you can do this all in one go with the effects package.
library(effects)
ef.mod.all<-effect('state:year', model.all)
summary(ef.mod.all)
#because the effect is linear it is simple
#for your application just change the index for the columns (4,3)
#to those representing columns with unit changes in your predictor
summary(ef.mod.all)$effect[,4]-summary(ef.mod.all)$effect[,3]
#which is the same as our manual estimate from the full
coef(model.all)['carat']+coef(model.all)['colorE:carat']
#and individual model for E
coef(model.E)[2]

library(broom)
tmp <- tidy(m0)
augment(m0)
glimpse(m0)

print(m0$coefficients)

results3 <- broom::tidy(m0)

glimpse(results3)

tmp <- filter(tmp, str_detect(term, "year"))
write.csv(tmp, "ag_harv_coeff.csv")


one_crop <- SIAP %>% 
  #filter(crop == "maiz", water == "irrigated",str_detect(crop_var,'grano'), state %in% maize_states) %>% 
  #filter(crop == "trigo", water == "rainfed",str_detect(crop_var,'grano'), state %in% maize_states) %>% 
  #filter(crop == "soya", water == "rainfed", state %in% maize_states) %>% 
  #filter(crop == "sorgo", water == "irrigated",str_detect(crop_var,'grano'), state %in% maize_states) %>% 
  group_by(year, state) %>% 
  summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            HAR = round(sum(harvested)/sum(planted), digits = 2) )


#### One particular crop ####

one_crop %>% 
  DT::datatable()

#### Panel data ####
period <- tibble(rep(c(1980:2016), times = 32)) #32 states report maize production
colnames(period) <- c("year") 
states <- tibble(rep(c("aguascalientes","baja california","baja california sur",
                       "campeche", "coahuila", "colima","chiapas","chihuahua",
                       "distrito federal","durango",
                       "guanajuato", "guerrero", "hidalgo", "jalisco", 
                       "mexico", "michoacan", "morelos", "nayarit","nuevo leon", 
                       "oaxaca", "puebla", "queretaro", "quintana roo",
                       "san luis potosi", "sinaloa", "sonora", "tabasco", 
                       "tamaulipas", "tlaxcala","veracruz", "yucatan", "zacatecas"), times = 37))
cov_id <- tibble(rep(c(1, 2, 3,
                       4, 5, 6, 7,8,
                       9, 10, 
                       11, 12, 13, 14, 
                       15, 16, 17, 18, 19,
                       20, 21, 22, 23,
                       24, 25, 26, 27,
                       28, 29, 30, 31, 32), times = 37))

colnames(states) <- c("state") 
states <- states %>% 
  arrange(state) 

colnames(cov_id) <- c("state_code")
cov_id <- cov_id %>% 
  arrange(state_code)

states_period <- cbind(cov_id, states, period)

##### Data frame for one particular crop ####
maize <- left_join(states_period, one_crop, by=c("state", "year"))
maize <- maize %>%  
  transform(i=as.numeric(factor(state))) %>% 
  transform(t=as.numeric(factor(year))) %>% 
  group_by(year) %>% 
  arrange(state) 

maize %>% 
  DT::datatable()

#### REGIONS #####
mex_south <- c("chiapas", "guanajuato", "jalisco", "mexico", "michoacan") 
mex_north <- c("sinaloa", "tamaulipas") # maize states
maize_states <- c(mex_south, mex_north)

mex_south <- c("chiapas", "guanajuato", "jalisco", "mexico", "michoacan") # maize states

yields_south <- maize %>%
  #filter(state %in% mex_south) %>% # filter by state names 
  #group_by(year) %>% 
  group_by(state, year) %>% 
  summarise(ag_yield = round(sum(ag_prod)/sum(ag_harv), digits = 2),
            HAR = round(sum(ag_harv)/sum(ag_planted), digits = 2)) # just calculate yields

yields_south %>% 
  DT::datatable()

str(yields_south)

n <- diff(log(yields_south$ag_yield))
fd_yield <- c(0, n)
yields_south$fd_yield <- fd_yield

e <- diff(log(yields_south$HAR))
fd_HAR <- c(0, e)
yields_south$fd_HAR <- fd_HAR



yields_south %>% 
  filter(state == "puebla") %>% 
  ggplot(aes(x=year, y=HAR)) +
  geom_line() #+
#geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

ggplot(yields_south, aes(x=year, y=fd_HAR)) +
  geom_line() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)


















