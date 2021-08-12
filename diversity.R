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

# Municipal level
SIAP_mun <- read.csv("SIAP_mun.csv")

# Frequently used objects
regions <- c("Central", "Central West", "North East", "North West", "South")

##### DATA WRANGLING #####
# Move this to the SIAP_mun_wrangling script
#SIAP <- SIAP %>% 
  #filter(type == "food") # Only interested in food crops

# Add regions 
#north_west <- c("baja california", "baja california sur", "nayarit","sinaloa", "sonora")
#north_east <- c("chihuahua", "coahuila", "durango","nuevo leon","san luis potosi","tamaulipas", "zacatecas")
#central_west <- c("aguascalientes","colima","guanajuato","jalisco","michoacan","queretaro")
#central <- c("distrito federal","hidalgo","guerrero","morelos","mexico","puebla","tlaxcala","veracruz")
#south <- c("campeche","chiapas","oaxaca","tabasco", "quintana roo","yucatan")

#SIAP <- SIAP %>% 
  #mutate(region = case_when(
    #state %in% north_west  ~ "north_west",
    #state %in% north_east  ~ "north_east",
    #state %in% central_west  ~ "central_west",
    #state %in% central  ~ "central",
    #state %in% south  ~ "south"
  #))

#SIAP$region <- as.factor(SIAP$region)

# Add crop groups
#Tudu



#### Graphing diversity ####

cane <- SIAP %>%
  filter(year == 2015, FAO_crop == "Sugar cane") %>% 
  group_by(state)
  summarise(ag_harv = sum(harvested))
  
SIAP_v1 <- read.csv("/Users/erikaluna/R\ Studio/crop_diversity/archive/SIAP_v1.csv")
cane <- SIAP_v1 %>%
  filter(crop == "cana de azucar") %>% 
  group_by(year) %>% 
  summarise(ag_harv = sum(harvested))

cane %>% 
  ggplot(aes(year, ag_harv)) +
  geom_line()

levels(SIAP_v1$type)

animal_feed <- SIAP_v1 %>% 
  filter(type == "food", crop != "pastos forrajeros") %>%
  group_by(crop, year) %>% 
  summarise(ag_harv = sum(harvested)) 


animal_feed %>% 
ggplot(aes(year, ag_harv, colour = crop))+
  geom_line() +
  scale_y_continuous(labels = comma) +
  directlabels::geom_dl(aes(label = crop), method = "smart.grid")
  


cane_lm <- lm(ag_harv ~ year, data = cane)
summary(cane_lm)

region_crop_group <- SIAP %>% 
  group_by(year, crop_group, region) %>% 
  summarise(ag_harv = sum(harvested))

region_crop_group %>% 
  ggplot(aes(year, ag_harv, colour = crop_group)) +
  geom_smooth(method = "lm") +
  #geom_line()+
  scale_y_continuous(labels = comma, trans='log10') +
  facet_grid(~region)
  
m0 <- lm(ag_harv ~ year*region + region*crop_group, data = region_crop_group)
summary(m0)

lm_region_crop_group <- region_crop_group %>% 
  group_by(region, crop_group) %>%
  do(m1 = tidy(lm(ag_harv ~ year, data = .))) %>% # change the name of diversity index,
  unnest(m1)

lm_region_crop_group_sig <- lm_region_crop_group %>%
  filter(term == "year") %>% 
  mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                             p.value >= 0.05  ~ "non significant"))


SIAP <- read.csv("/Users/erikaluna/R\ Studio/crop_diversity/archive/SIAP_v2.csv")

FAO_crops <- Equivalencies.FAO.SIAP

colnames(FAO_crops) <- c("crop", "FAO_code", "FAO_crop", "crop_group", "crop_importance")

SIAP_FAO <- left_join(SIAP, FAO_crops, by="crop")

trad <- SIAP_FAO %>% 
  filter(crop_importance == "traditional") %>% 
  group_by(year, region) %>% 
  summarise(ag_harv = sum(harvested))

trad %>% 
  ggplot(aes(year, ag_harv, colour = region)) +
  #geom_smooth(method = "lm") #+
  geom_line()+
  scale_y_continuous(labels = comma, trans='log10')

m1 <- lm(ag_harv ~ region*year, data = trad)
summary(m1)

central <- trad %>% 
  filter(region == "central")

m2 <- lm(ag_harv ~ year, data = central)
summary(m2)




one_crop <- SIAP %>% 
  #filter(crop == "maiz", str_detect(crop_var,'grano')) %>% 
  #filter(year == 1980) %>% 
  group_by(region, state, year, crop) %>% 
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
  group_by(region, year) %>% 
  count()

ggplot(diversity, aes(year, n, group = region, colour = region)) +
  geom_line() +
  #geom_tile() #+
  #scale_x_discrete(guide = guide_axis(angle = 45))
  theme(legend.position = "none") +
  labs(x = "Year", y = "Number of crops")

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
      geom_point(aes(color = area, linetype = water)) +
      #geom_smooth(aes(color = area, linetype = water), method = lm, 
       #           se = FALSE, fullrange = TRUE)+
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      scale_y_continuous(limits = c(0,4000000), labels = comma) +
      ggtitle(city_)
    
    print(city_plots[[city_]])
    #ggsave(city_plots[[city_]], file=paste0("plot_", city_,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
  }
  
  do.call(grid.arrange,city_plots)
  
##### Total area #####
# National
area_nat <- SIAP %>% 
  group_by(year) %>% 
  summarise(ag_harv = sum(harvested))

  area_nat %>% 
    ggplot(aes(year, ag_harv)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Harvested Area (ha)")  
  
  a <- lm(ag_harv ~ year, area_nat)
  summary(a)
  
# Regional
area_reg <- SIAP %>% 
    group_by(region, year) %>% 
    summarise(ag_harv = sum(harvested))

  area_reg %>% 
    ggplot(aes(year, ag_harv, colour = region)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Harvested Area (ha)") +
    scale_color_discrete(name = "Region",labels = regions)
  
  a <- lm(ag_harv ~ year*region, area_reg)
  summary(a)

  lm_area <- area_reg %>% 
    group_by(region) %>%
    do(m1 = tidy(lm(ag_harv ~ year, data = .))) %>% # change the name of diversity index,
    unnest(m1)
  
  lm_area_sig <- lm_area %>%
    filter(term == "year") %>% 
    mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                               p.value >= 0.05  ~ "non significant"))
  
# State
area_sta <- SIAP %>% 
  group_by(state, year) %>% 
  summarise(ag_harv = sum(harvested))

# Municipal  
area_mun <- SIAP_mun %>% 
  group_by(mun, year) %>% 
  summarise(ag_harv = sum(harvested))

data %>% 
  ggplot(aes(year, ag_harv, colour = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Harvested Area (ha)") +
  scale_color_discrete(name = "Region",labels = c("Central", "Central West", "North East", "North West", "South"))

lm_area <- data %>% 
  group_by(region) %>%
  do(m1 = tidy(lm(ag_harv ~ year, data = .))) %>% # change the name of diversity index,
  unnest(m1)

lm_area_sig <- lm_area %>%
  filter(term == "year") %>% 
  mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                             p.value >= 0.05  ~ "non significant"))

lm_area_sig <- right_join(data, lm_area_sig, by = "region") 

lm_area_sig %>% 
  ggplot(aes(year, ag_harv, group = region, color = p.value)) +
  scale_color_manual(values=c("gray40", "indianred1")) +
  geom_smooth(method="lm", se = T, show.legend = T) +
  directlabels::geom_dl(aes(label = region), method = "smart.grid") #+
  #labs(title = "Effective Number of Crop Species", x = "year", y = "ENCS") +
  #facet_grid(cols = vars(region), labeller = as_labeller(region))



lm_state_sig <- lm_state %>%
  filter(term == "year") %>% 
  mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                             p.value >= 0.05  ~ "non significant"))
  
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
  #filter(year > 1993) %>% 
#data <- SIAP_mun %>% 
  #group_by(region, crop, year) %>% # modify for region, state or municipal level
  #group_by(COV_ID, crop, year) %>% # modify for region, state or municipal level
  group_by(region, crop_group, year) %>% # modify for region, state or municipal level
  summarise(ag_harv = sum(harvested))

# Matrix
data <- data %>%
  #spread(crop, ag_harv)
  spread(crop_group, ag_harv)

data[is.na(data)] <- 0

data <- as.data.frame(data)

# indices with vegan
abundance <- data %>%
  group_by(region, year) %>% # modify for region, state or municipal level
  group_modify(~ broom::tidy(diversity(.x))) # this is H index (Shannon - species abundance)
colnames(abundance) <- c("region", "year", "abundance") # modify for region, state or municipal level
#colnames(abundance) <- c("region", "state","year", "abundance")

simpson <- data %>% 
  group_by(region, year) %>%
  group_modify(~ broom::tidy(diversity(.x, "simpson"))) # this is D index (Simpson - species abundance)
colnames(simpson) <- c("region", "year", "simpson")
#colnames(simpson) <- c("region", "state","year", "simpson")

richness <- data %>% 
  group_by(region, year) %>% 
  group_modify(~ broom::tidy(specnumber(.x))) # this is S index (Species richness)
colnames(richness) <- c("region","year", "richness")
#colnames(richness) <- c("region","state", "year", "richness")

# Indices data frame
indices <- left_join(abundance, simpson, by = c("region","year")) %>% 
              left_join(., richness, by=c("region","year")) 

indices <- indices %>% 
  mutate(evenness = abundance/log(richness)) # this is J index (Pielou's evenness)

indices <- indices %>% 
  mutate(encs = exp(abundance)) # this is ENCS (Effective Number of Crop Species)

write.csv(indices, file = "indices_region_37.csv")
indices <- read.csv("indices_region_37.csv")
  
# Plots
H_plot <- indices %>% 
  ggplot(aes(year, abundance, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Shannon index (H)", x = "year", y = "H") 

D_plot <- indices %>% 
  ggplot(aes(year, simpson, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Simpson index (D)", x = "year", y = "D") 

S_plot <- indices %>% 
  ggplot(aes(year, richness, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Richness index (S)", x = "year", y = "n") 

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
ENCS_plot
ggarrange(H_plot, D_plot, S_plot, J_plot,nrow = 1, common.legend = TRUE, legend="bottom")

# Slope extraction
library(lme4)

#indices$mun <- as.factor(indices$mun)
#str(indices)

fits_encs <- lmList(encs ~ year | COV_ID, data=indices)
coefs_encs <- coef(fits_encs)
colnames(coefs_encs) <- c("int_encs", "slope_encs")

fits_abundance <- lmList(abundance ~ year | COV_ID, data=indices)
coefs_abundance <- coef(fits_abundance)
colnames(coefs_abundance) <- c("int_abundance", "slope_abundance")

fits_simpson <- lmList(simpson ~ year | COV_ID, data=indices)
coefs_simpson <- coef(fits_simpson)
colnames(coefs_simpson) <- c("int_simpson", "slope_simpson")

fits_richness <- lmList(richness ~ year | COV_ID, data=indices)
coefs_richness <- coef(fits_richness)
colnames(coefs_richness) <- c("int_richness", "slope_richness")

fits_evenness <- lmList(evenness ~ year | COV_ID, data=indices)
coefs_evenness <- coef(fits_evenness)
colnames(coefs_evenness) <- c("int_evenness", "slope_evenness")

COV_ID <- c(1:2436)
state_code <- c(1:32)

coefs <- cbind(coefs_encs, coefs_abundance, coefs_simpson, coefs_richness, coefs_evenness)
#coefs <- left_join(coefs, SIAP_codes, by = "COV_ID")
write.csv(coefs, file = "coefs_COV_ID.csv")

coefs %>% 
  ggplot(aes(state, slope_evenness)) +
  geom_col()

snap_2003 <- indices %>% 
  filter(year == 2003)

snap_2010 <- indices %>% 
  filter(year == 2010)

snap_2016 <- indices %>% 
  filter(year == 2016)

write.csv(snap_2003, file = "snap_2003.csv")
write.csv(snap_2010, file = "snap_2010.csv")
write.csv(snap_2016, file = "snap_2016.csv")



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

alpha <- with(data, tapply(specnumber(dune), Management, mean))
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


#### ANOVA ####
# Summary statistics

indices %>% 
group_by(region) %>%
  summarise(
    count = n(),
    mean = mean(encs, na.rm = TRUE),
    sd = sd(encs, na.rm = TRUE)
  )

# Pairwise comparisons
library(rstatix)
pwc <- indices %>% 
  pairwise_t_test(
    encs ~ region
  )
pwc


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(indices, x = "region", y = "evenness", 
          color = "region",
          legend = "none",
          #rotate_x_text(angle = 90, hjust = NULL, vjust = NULL),
          ylab = "evenness", xlab = "Region")

summary(aov(encs~year*region, data=indices))


# Compute the analysis of variance
res.aov <- aov(encs ~ region*year, data = indices)
# Summary of the analysis
summary(res.aov)

##### Breakpoint analysis #####
library(segmented)
library(strucchange)
bp <- breakpoints(richness ~ year, h=,data = indices)

plot(richness ~ region*year, pch = 19, data = indices)


lines(fitted(bp, breaks = 1) ~ y, col = 4, lwd = 1.5)
lines(fitted(bp, breaks = 2) ~ y, col = 2, lwd = 1.5)


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



