#### Graphing diversity ####

one_crop <- SIAP %>% 
  #filter(type == "food", cycle == "perennial") %>% 
  #filter(type == "food", year > 1994) %>% 
  #filter(crop == "maiz", str_detect(crop_var,'grano')) %>% 
  filter(crop == "frijol", water == "rainfed") %>% 
  #filter(crop == "sorgo", str_detect(crop_var,'grano'), water == "irrigated") %>% 
  #group_by(state, year, crop) %>% 
  group_by(year) %>% 
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

#ggplot(one_crop, aes(year, ag_planted, group = state, colour = state)) +
ggplot(one_crop, aes(year, ag_planted)) +
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


#########slope extraction from a linear model in R##########
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
