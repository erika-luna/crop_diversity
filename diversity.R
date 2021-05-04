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
library(gridExtra)
library(broom)

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

# Add crop groups
#Tud??

# Municipal level
SIAP_mun <- SIAP %>%
  filter(year > 2002) # data is recorded at the mun level after 2003

#### Graphing diversity ####

one_crop <- SIAP %>% 
  #filter(crop == "maiz", str_detect(crop_var,'grano')) %>% 
  group_by(state, year, crop) %>% 
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

ggplot(one_crop, aes(x=year)) + 
  geom_line(aes(y = ag_planted), color = "darkred") + 
  geom_line(aes(y = ag_harv), color="steelblue") +
  scale_y_continuous(labels = comma) +
  ylab("Area (ha)") +
  ggtitle("Beans rainfed")

one_crop <- drop_na(one_crop)

m0 <- lm(n~state*year-1, data = diversity)
predict(m0)
diversity$simplemodel <- predict(m0)

tmp <- tidy(m0)
augment(m0)
glimpse(m0)

print(m0$coefficients)

results3 <- broom::tidy(m0)

glimpse(results3)


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







#### Species richness ####

crop_div <- crop_div %>% 
  group_by(region, crop, year) %>% 
  count()

crop_div %>% 
  ggplot(aes(year, n, group = region, colour = region)) +
  geom_line() 
  
one_state <- SIAP %>% 
  filter(state == "veracruz") %>% 
  group_by(year, crop) %>% 
  summarise(ag_harv = sum(harvested),
            ag_plan = sum(planted))

one_state %>% 
  ggplot(aes(year, ag_harv)) +
  geom_point() +
  geom_smooth(method = "lm")

one_state <- one_state %>% 
  gather(area, ha, 3:4) %>% 
  filter(year == 1983)

ggplot(one_state, aes(year, ha)) +
geom_point(aes(color = crop)) #+
  #geom_rug(aes(color =area)) +
  #geom_smooth(aes(color = crop), method = lm, 
            #  se = FALSE, fullrange = TRUE)+
  #scale_color_manual(values = c("#00AFBB", "#E7B800"))#+
  #ggpubr::stat_cor(aes(color = area), label.x = 3)


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
    #ggsave(city_plots[[city_]], file=paste0("plot_", city_,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
  }
  
  
  

#### Area #####
  
data <- SIAP %>% 
  group_by(region, water, year) %>% 
  summarise(ag_harv = sum(harvested),
            ag_plan = sum(planted))
  
data <- gather(data, "area", "value", 4:5) 

cities = unique(data$region)
city_plots = list()
  for(city_ in cities) {
    city_plots[[city_]] = ggplot(data %>% filter(region == city_), aes(x=year, y=value)) +
      #geom_point(aes(color = area, linetype = water)) +
      geom_smooth(aes(color = area, linetype = water), method = lm, 
                  se = FALSE, fullrange = TRUE)+
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      scale_y_continuous(limits = c(0,4000000), labels = comma) +
      ggtitle(city_)
    
    print(city_plots[[city_]])
    #ggsave(city_plots[[city_]], file=paste0("plot_", city_,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
  }
  
  do.call(grid.arrange,city_plots)
  
##### Total area #####
data <- SIAP %>% 
  group_by(region, year) %>% 
  summarise(ag_harv = sum(harvested))

perc_area <- data %>% 
group_by(year) %>% 
  mutate(percent = ag_harv/sum(ag_harv)*100)

perc_area %>% 
  ggplot(aes(year, percent, fill = region)) +
  geom_area()
  

##### Diversity Indices #####
# Measuring taxonomic crop alpha diversity 
library(vegan)

data <- SIAP %>% 
  group_by(state, crop, year) %>% # modify for region, state or municipal level
  summarise(ag_harv = sum(harvested))

# Matrix
data <- data %>%
  spread(crop, ag_harv)

data[is.na(data)] <- 0

data <- as.data.frame(data)

# indices with vegan
abundance <- data %>%
  group_by(state, year) %>% # modify for region, state or municipal level
  group_modify(~ broom::tidy(diversity(.x))) # this is H index (Shannon - species abundance)
colnames(abundance) <- c("state", "year", "abundance") # modify for region, state or municipal level

simpson <- data %>% 
  group_by(state, year) %>%
  group_modify(~ broom::tidy(diversity(.x, "simpson"))) # this is D index (Simpson - species abundance)
colnames(simpson) <- c("state", "year", "simpson")

richness <- data %>% 
  group_by(state, year) %>% 
  group_modify(~ broom::tidy(specnumber(.x))) # this is S index (Species richness)
colnames(richness) <- c("state", "year", "richness")

# Indices data frame
indices <- left_join(abundance, simpson, by = c("state", "year")) %>% 
              left_join(., richness, by=c("state", "year")) 

indices <- indices %>% 
  mutate(evenness = abundance/log(richness)) # this is J index (Pielou's evenness)

indices <- indices %>% 
  mutate(encs = exp(-abundance)) # this is ENCS (Effective Number of Crop Species)

#write.csv(indices, file = "indices_state.csv")
  
# Plots
H_plot <- indices %>% 
  ggplot(aes(year, abundance, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Shannon index (H) - crop abundance", x = "year", y = "H") 

D_plot <- indices %>% 
  ggplot(aes(year, simpson, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Simpson index (D) - crop abundance", x = "year", y = "D") 

S_plot <- indices %>% 
  ggplot(aes(year, richness, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Richness index (S) - crop richness", x = "year", y = "n") 

J_plot <- indices %>% 
  ggplot(aes(year, evenness, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Pielou's index (J) - crop evenness", x = "year", y = "J")

ENCS_plot <- indices %>% 
  ggplot(aes(year, encs, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "ENCS", x = "year", y = "ENCS")

ggarrange(H_plot, D_plot, S_plot, J_plot,nrow = 1, common.legend = TRUE, legend="bottom")

# Slope extraction
library(lme4)
fits <- lmList(encs ~ year | state, data=indices)
fits
coefs <- coef(fits)

write.csv(coefs, file = "coefs_state.csv")



m0 <- lm(encs~region*year-1, data=indices)

library(broom)
tmp <- tidy(m0)
augment(m0)
glimpse(m0)
summary(m0)

j <- summ(m0, digits = 3)


## beta diversity defined as gamma/alpha - 1: 
# Tud??
data(dune)
data(dune.env)
alpha <- with(dune.env, tapply(specnumber(dune), Management, mean))
gamma <- with(dune.env, specnumber(dune, Management))
gamma/alpha - 1


fitted_models = indices %>% group_by(region) %>% do(model = lm(encs ~ year, data = .))
summary(fitted_models)


##########slope extraction from a linear model in R##########
#e.g. if you were trying to estimate a linear effect of temp on yields for each state,
#holding year constant
##########################################################
##load some data
library(ggplot2)
data(diamonds)
diamonds$color <-factor(diamonds$color, order=F) #reset to factor var
##full mod
#for your data replace price =yield, color=admin unit, and table= year and carat=temperature
#we set intercept to -1 for convenience
#model.all<-lm(price~color*carat+color*table-1, data=diamonds)
model.all<-lm(encs~year*region, data=indices)
##subset model
#e.g.  the data for diamond colour E (e.g. or in your case a particular state).
sub.e<-base::subset(diamonds, color=="E")
model.E<-lm(price~carat+ table, data=sub.e)
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
ef.mod.all<-effect('color:carat', model.all)
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


m0$coefficients



#### Just one crop ####
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




##### Archive ######

one_region <- data %>% 
  filter(region == "central") %>% # Modify for each region, delete after writing for loop 
  group_by(year, crop) %>% 
  summarise(ag_harv = sum(ag_harv))

# Matrix
one_region <- one_region %>%  
  spread(crop, ag_harv)

one_region[is.na(one_region)] <- 0 # NA values to cero 

one_region <- as.data.frame(one_region)

#tmp <- c("state", "year") # columns to remove

one_region <- one_region %>%  # remove columns from the matrix
  #select(-one_of(tmp))
  select(-year)

one_region <- mapply(one_region, FUN=as.integer)


# Indices
H <- diversity(one_region)
simp <- diversity(one_region, "simpson")
invsimp <- diversity(one_region, "inv")
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(one_region, 2) - 1
## Fisher alpha
alpha <- fisher.alpha(one_region)
## Plot all
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(one_region) ## rowSums(BCI > 0) does the same...
J <- H/log(S)

ENCS <- exp(-H)

period <- c(1980:2016)


indices_time <- as.data.frame(cbind(period, H, simp, S, J, ENCS))


J_plot <- ggplot(indices_time, aes(period, J)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Pielou's evenness (J)", x = "year", y = "H") +
  scale_y_continuous(limits = c(0, 1))

D_plot <- ggplot(indices_time, aes(period, simp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Simpson index (D)", x = "year", y = "D") +
  scale_y_continuous(limits = c(0, 1))

ggarrange(J_plot, D_plot,nrow = 2)


## beta diversity defined as gamma/alpha - 1:
data(dune)
data(dune.env)
alpha <- with(dune.env, tapply(specnumber(dune), Management, mean))
gamma <- with(dune.env, specnumber(dune, Management))
gamma/alpha - 1



