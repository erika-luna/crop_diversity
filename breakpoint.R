#### LOAD PACKAGES ####
library(ggplot2)
library(tidyverse)
library(segmented)
library(ggpubr)
library(purrr)
library(broom)
library(plotly)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(lme4)
library(gridExtra)

##### LOAD DATA ####
indices <- read.csv("indices_region_37.csv") # This csv has diversity indices for each region for the whole 37 year period. 
indices <- read.csv("indices_states_37.csv")

#### EXPLORE THE DATA ####
### Diversity density plots
ggplot(df, aes(x = richness, fill = state)) +
  geom_density(alpha = .3)

ggplot(df, aes(x = abundance, fill = state)) +
  geom_density(alpha = .3)

ggplot(df, aes(x = evenness, fill = state)) +
  geom_density(alpha = .3)

ggplot(df, aes(x = encs, fill = state)) +
  geom_density(alpha = .3)

### Linear trends 
H_plot <- indices %>% 
  ggplot(aes(year, abundance, color = state)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Shannon index (H) - crop abundance", x = "year", y = "H") 
H_plot + scale_y_continuous(expand = c(0, 0))

D_plot <- indices %>% 
  ggplot(aes(year, simpson, color = state)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Simpson index (D) - crop abundance", x = "year", y = "D") 

S_plot <- indices %>% 
  ggplot(aes(year, richness, color = state)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Richness index (S) - crop richness", x = "year", y = "n") 

J_plot <- indices %>% 
  ggplot(aes(year, evenness, color = state)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Pielou's index (J) - crop evenness", x = "year", y = "J")

ENCS_plot <- indices %>% 
  ggplot(aes(year, encs, color = state)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "ENCS", x = "year", y = "ENCS")
ENCS_plot
ggarrange(H_plot, D_plot, S_plot, J_plot,nrow = 1, common.legend = TRUE, legend="bottom")

#### all states ####

regions <- c(
  `central` = "Central",
  `central_west` = "Central West",
  `north_east` = "North East",
  `north_west` = "North West",
  `south` = "South"
)

# ENCS
encs_regions_plot <- indices %>% 
  ggplot(aes(year, encs, color = state)) +
  #ggplot(aes(year, encs, group = 1, color = state)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method="lm", se = T) +
  directlabels::geom_dl(aes(label = state), method = "smart.grid") +
  labs(title = "Effective Number of Crop Species", x = "year", y = "ENCS") +
  theme(legend.position = "none")+
    facet_grid(cols = vars(region), labeller = as_labeller(regions))
encs_regions_plot
#ggsave(encs_regions_plot, file=paste0("plots/indices/encs_regions_lm_plot.png"), width = 44.45, height = 27.78, units = "cm", dpi=300)

# Evenness 
evenness_regions_plot <- indices %>% 
  #ggplot(aes(year, evenness, color = state)) +
  ggplot(aes(year, evenness, group = 1, color = state)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method="lm", se = T) +
  directlabels::geom_dl(aes(label = state), method = "smart.grid") +
  labs(title = "Pielou's index (J) - crop evenness", x = "year", y = "evenness") +
  theme(legend.position = "none")+
  facet_grid(cols = vars(region), labeller = as_labeller(regions))
evenness_regions_plot
#ggsave(evenness_regions_plot, file=paste0("plots/indices/evenness_regions_lm_plot.png"), width = 44.45, height = 27.78, units = "cm", dpi=300)

# Richness
richness_regions_plot <- indices %>% 
  ggplot(aes(year, richness, color = state)) +
  #ggplot(aes(year, richness, group = 1, color = state)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method="lm", se = F) +
  directlabels::geom_dl(aes(label = state), method = "smart.grid") +
  labs(title = "Richness index (S) - crop richness", x = "year", y = "richness") +
  theme(legend.position = "none")+
  facet_grid(cols = vars(region), labeller = as_labeller(regions))
richness_regions_plot
ggsave(richness_regions_plot, file=paste0("plots/indices/richness_regions_plot.png"), width = 44.45, height = 27.78, units = "cm", dpi=300)

# Abundance
abundance_regions_plot <- indices %>% 
  #ggplot(aes(year, abundance, color = state)) +
  ggplot(aes(year, abundance, group = 1, color = state)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method="lm", se = T) +
  directlabels::geom_dl(aes(label = state), method = "smart.grid") +
  labs(title = "Shannon index (H) - crop abundance", x = "year", y = "abundance") +
  theme(legend.position = "none")+
  facet_grid(cols = vars(region), labeller = as_labeller(regions))
abundance_regions_plot
ggsave(abundance_regions_plot, file=paste0("plots/indices/abundance_regions_lm_plot.png"), width = 44.45, height = 27.78, units = "cm", dpi=300)


##### lm for every state and for every diversity index #####
indices <- indices %>% 
  gather("index", "value", 5:9)

lm_state <- indices %>% 
  group_by(region, state, index) %>%
  do(m1 = tidy(lm(value ~ year, data = .))) %>% # change the name of diversity index,
  unnest(m1)

lm_state_sig <- lm_state %>%
  filter(term == "year") %>% 
  mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                        p.value >= 0.05  ~ "non significant"))

lm_state_sig <- right_join(indices, lm_state_sig, by = c("state", "index")) 

# Graph showing where lm is significant and non significant 
lm_state_sig_encs_plot <- lm_state_sig %>% 
  ggplot(aes(year, encs, group = state, color = p.value)) +
  scale_color_manual(values=c("gray40", "indianred1")) +
  geom_smooth(method="lm", se = T, show.legend = T) +
  directlabels::geom_dl(aes(label = state), method = "smart.grid") +
  labs(title = "Effective Number of Crop Species", x = "year", y = "ENCS") +
  facet_grid(cols = vars(region), labeller = as_labeller(regions))
lm_state_sig_encs_plot
ggsave(lm_state_sig_encs_plot, file=paste0("plots/indices/lm_state_sig_encs_plot.png"), width = 44.45, height = 27.78, units = "cm", dpi=300)

cities = unique(lm_state_sig$index)
city_plots = list()
for(city_ in cities) {
  city_plots[[city_]] = ggplot(lm_state_sig %>% filter(index == city_), aes(x=year, y=value)) +
    geom_smooth(aes(group = state, color = p.value), method = lm, 
               se = FALSE, fullrange = TRUE)+
    directlabels::geom_dl(aes(label = state, color=p.value), method = "smart.grid") +
    scale_color_manual(values=c("gray40", "indianred1")) +
    facet_grid(cols = vars(region.x), labeller = as_labeller(regions))+
    ggtitle(city_)
  
  print(city_plots[[city_]])
  ggsave(city_plots[[city_]], file=paste0("plots/indices/lm_state_sig_plot_", city_,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
}

do.call(grid.arrange,city_plots)

#### lm for every region and for every diversity index ####

#indices <- read.csv("indices_region_37.csv")  ### Fixed Effects
#indices <- indices %>% 
 # gather("index", "value", 4:8)

indices <- read.csv("indices_states_37.csv")  ### Random Effects
indices <- indices %>% 
  gather("index", "value", 5:9)

lm_regions <- indices %>% 
  group_by(region, index) %>%
  do(m1 = tidy(lm(value ~ year, data = .))) %>%
  unnest(m1)

lm_regions_sig <- lm_regions %>%
  filter(term == "year") %>% 
  mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                             p.value >= 0.05  ~ "non significant"))

lm_regions_sig <- right_join(indices, lm_regions_sig, by = c("region", "index")) 

#write.csv(lm_regions_sig, file = "csv/lm_regions_sig_states.csv")

###############################
##### Breakpoint Analysis #####
###############################

# The following analysis is divided into random and fixed effects

##### FIXED EFFECTS ####
# i.e. indices at the regional (5 regions) level for 37 years (185 obs)
indices <- read.csv("indices_region_37.csv")
indices <- indices %>% 
  gather("index", "value", 4:8)

# create a figure to get an idea of the data
p <- ggplot(indices, aes(x = year, y = value, color = region)) + 
  geom_line() +
  facet_grid(rows = vars(index), scales = "free")
p

# We first fit a linear model
lm_regions <- indices %>% 
  group_by(region, index) %>%
  do(m1 = tidy(lm(value ~ year, data = .))) %>%
  unnest(m1)

# a linear model with data for the part after 1994
#my.lm2 <- lm(evenness ~ year, data = df[df$year > 1993, ])
#summary(my.lm2)

# Extract te coefficients from the overall model
slopes <- lm_regions %>% 
  filter(term == "year") %>% 
  select(region, estimate)

intercepts <- lm_regions %>% 
  filter(term == "(Intercept)") %>% 
  select(region, estimate)

p <- ggplot(indices, aes(x = year, y = value, color = region)) + 
  geom_abline(intercept = intercepts$estimate, 
              slope = slopes$estimate)+
  # aes(color = "royalblue")) +
  facet_grid(rows = vars(index), scales = "free")
p









#####  Fixed effects one by one (regions) #### 
indices <- read.csv("indices_region_37.csv")
indices <- indices %>% 
  filter(region == "south")

# initial look at the datao
p <- ggplot(indices, aes(x = year, y = richness)) + 
  geom_line()
p

my.lm <- lm(richness ~ year, data = indices)
summary(my.lm)
my.coef <- coef(my.lm)


# add the regression line to the graph
p <- p + geom_abline(intercept = my.coef[1], 
                     slope = my.coef[2], 
                     aes(color = "royalblue")) ### fix this, is not using the color argument
p 


### TRY 1 (with segmented)
# https://rpubs.com/MarkusLoew/12164

# have to provide estimates for breakpoints.
# after looking a the data, 
my.seg <- segmented(my.lm, 
                    seg.Z = ~ year)
                    #seg.Z = ~ year,
                    #psi = list(year = c(1990,2000,2010)))

#test for the 2nd breakpoint in the variable z
pscore.test(my.seg, seg.Z = ~year, more.break=T)
pscore.test(my.seg)

# When not providing estimates for the breakpoints "psi = NA" can be used.
# The number of breakpoints that will show up is not defined
#my.seg <- segmented(my.lm, 
#                    seg.Z = ~ year, 
#                    psi = NA)

# display the summary
summary(my.seg)
# get the breakpoints
my.seg$psi
# get the slopes
slope(my.seg)
# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(year = indices$year, richness = my.fitted)
# plot the fitted model
ggplot(my.model, aes(x = year, y = richness)) + geom_line()
# add the fitted data to the exisiting plot
p <- p + geom_line(data = my.model, aes(x = year, y = richness), colour = "maroon")
# add vertical lines to indicate the break locations
# second row of the psi-matrix
my.lines <- my.seg$psi[, 2]
p<- p + geom_vline(xintercept = my.lines, linetype = "dashed")
p + labs(title = "North East") 
p

AIC(my.lm)
AIC(my.seg)













##### RANDOM EFFECTS one by one (regions) ######
indices <- read.csv("indices_states_37.csv")
# South 
indices <- indices %>% 
  filter(region == "south")

# initial look at the data
p <- ggplot(indices, aes(x = year, y = evenness)) + 
  geom_point()
p

my.lm <- lm(evenness ~ year, data = indices)
summary(my.lm)
my.coef <- coef(my.lm)


# add the regression line to the graph
p <- p + geom_abline(intercept = my.coef[1], 
                     slope = my.coef[2], 
                     aes(color = "royalblue")) ### fix this, is not using the color argument
p 

my.seg <- segmented(my.lm, 
                    seg.Z = ~ year)
#seg.Z = ~ year,
#psi = list(year = c(1990,2000,2010)))

#test for the 2nd breakpoint in the variable z
pscore.test(my.seg, seg.Z = ~year, more.break=T)
pscore.test(my.seg)

# When not providing estimates for the breakpoints "psi = NA" can be used.
# The number of breakpoints that will show up is not defined
#my.seg <- segmented(my.lm, 
#                    seg.Z = ~ year, 
#                    psi = NA)

# display the summary
summary(my.seg)
# get the breakpoints
my.seg$psi
# get the slopes
slope(my.seg)
# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(year = indices$year, evenness = my.fitted)
# plot the fitted model
ggplot(my.model, aes(x = year, y = evenness)) + geom_line()
# add the fitted data to the exisiting plot
p <- p + geom_line(data = my.model, aes(x = year, y = evenness), colour = "maroon")
# add vertical lines to indicate the break locations
# second row of the psi-matrix
my.lines <- my.seg$psi[, 2]
p<- p + geom_vline(xintercept = my.lines, linetype = "dashed")
p + labs(title = "North East") 
p

AIC(my.lm)
AIC(my.seg)









### to get all plots in one

fits <- lmList(evenness ~ year | region, data=indices)
fits$central


fitted_models = indices %>% 
  group_by(region) %>% 
  do(model = lm(evenness ~ year, data = .))

tmp <- broom::tidy(tmp$model)

tmp <- tibble::glimpse(fitted_models)

rowwise(fitted_models) %>% tidy(model)



# TRY 2 (with segmented too)
# https://stackoverflow.com/questions/8758646/piecewise-regression-with-r-plotting-the-segments

#o <- segmented(my.lm, seg.Z = ~year, psi = list(year = c(1990,2000)),
o <- segmented(my.lm, seg.Z = ~year,
               control = seg.control(display = FALSE))
dat2 = data.frame(x = c(1980:2016), y = broken.line(o)$fit)

ggplot(dat2, aes(x = x, y = y)) +
  #geom_point() #+
  geom_line(color = 'blue')

# TRY 3
# Unraveling Spline Regression in R
# https://towardsdatascience.com/unraveling-spline-regression-in-r-937626bc3d96

# Look at the data
plot_ly(df,x=~year,
        y=~evenness,
        type="scatter"
)

# Linear model
fit <- lm(evenness ~ year, data=df)
summary(fit)

plot_ly(df,x=~year,
        y=~evenness,
        type="scatter") %>% 
  add_lines(x =  ~year, y = fitted(fit))

# Degree 2 Polynomial 
fit2 <- lm(evenness ~ poly(year,2) + year, data=df)
summary(fit2)

plot_ly(df,x=~year,
        y=~evenness,
        type="scatter") %>% add_lines(x =  ~year, y = fitted(fit2))

# Xbar here is called the Knot value.
df$Xbar <- ifelse(df$year>1994,1,0)
df$diff <- df$year - 1994
df$X <- df$diff*df$Xbar

df

reg <- lm(evenness ~ year + X, data = df)

plot_ly(df,x=~year,
        y=~evenness,
        type="scatter") %>% add_lines(x =  ~year, y = fitted(reg))

summary(reg)

# Same as above but now with segmented 
fit_seg <- segmented(fit, seg.Z = ~year, psi = list(year=c(1994, 2010)))

plot_ly(df,x=~year,
        y=~evenness,
        type="scatter") %>% add_lines(x =  ~year, y = fitted(fit_seg))

#### Summaries of models into tables ####
write.csv(tidy(my.lm) , "coefs.csv") # tidy() from broom package
write.csv(glance(my.lm) , "an.csv") # glance() from broom package
write.csv(tidy(my.seg) , "my.seg.csv") # tidy() from broom package


# Or ... 
fitted_models <- df %>% 
  group_by(region) %>% 
  do(model = lm(evenness ~ year, data = .))

summary(fitted_models)

fitted_models %>% tidy(model)
fitted_models %>% glance(model)

# Or ...
library(lme4)
fits <- lmList(evenness ~ year | region, data=df)
glance(fits$central)

##### Linear Models #####
# Evenness ~ year by region
lm_regions <- indices %>% 
  group_by(region) %>%
  do(my.lm = lm(evenness ~ year, data = .)) %>% 
  ungroup %>% 
  transmute(region, RegionsCoef = map(my.lm, tidy)) %>% 
  unnest(RegionsCoef)


segmented.mod <- segmented(my.lm, seg.Z=~year)
fit <- numeric(length(df$year)) * NA
fit[complete.cases(rowSums(cbind(ChH, CL)))]
  
  
tmp <- broken.line(segmented.mod)$fit

data1 <- data.frame(CL = CL, ChH = ChH, fit = fit)

ggplot(data1, aes(x = CL, y = ChH)) + 
  geom_point() +
  geom_line(aes(x = CL, y = fit), color = 'blue')

str(my.seg)

my.seg$coefficients
plot(tmp)
names(summary(my.lm))
names(summary(my.seg))

sink("hola.csv")
print(summary(my.seg))
sink()

library(xtable)
print(xtable(summary(my.seg)), file = "myfile.csv")


tidy.mlm(my.seg)
glance(my.seg)




lapply(names(df)[2:ncol(df)], function(x)run_mod(x, df))


breakpoints <- as.data.frame(my.seg$psi)
coefs <- as.data.frame(my.seg$coefficients)
jaja <- as.data.frame(left_join(breakpoints, coefs))


# get the slopes
slope(my.seg)


##### SIAP ####
# Load data
SIAP_mun <- read.csv("SIAP_mun.csv")
SIAP <- read.csv("SIAP.csv")

# Aggregate data at the state level
SIAP_state <- SIAP %>% 
  group_by(region, state_code, year) %>%
  summarise(ag_harv = sum(harvested))

# Descriptive statistics
SIAP_state %>% 
  group_by(region) %>% 
  summarise(max_harv = max(ag_harv),
            min_harv = min(ag_harv),
            range_harv = max(ag_harv)-min(ag_harv),
            sd_harv = sd(ag_harv),
            mean_harv = mean(ag_harv),
            median_harv = median(ag_harv)) 

ggboxplot(SIAP_state, x = "region", y = "ag_harv", 
          color = "region",
          legend = "none",
          ylab = "Harvested area", xlab = "Region")

summary(aov(ag_harv~year*region, data=SIAP_state))

# Read attribute table from states boundaries shapefile
states_area <- read.csv("csv/states_area.csv")
colnames(states_area) <- c("cve_ent", "state", "capital", "area", "perimeter", "COV_", "state_code")

area_covered <- left_join(SIAP_state, states_area, by = "state_code")
area_covered$perc_covered <- area_covered$ag_harv*100/area_covered$area

ggplot(area_covered, aes(region, perc_covered, colour = region)) +
  geom_boxplot()

ggplot(area_covered, aes(x = perc_covered, fill = region)) +
  geom_density(alpha = .3)

plot_ly(area_covered, x=~region, 
        y=~perc_covered,
        type="box")


###################
##### ARCHIVE #####
###################


# get the slopes manually - excercise!!
my.slopes <- coef(my.seg)

# first line: 
#y = b0 + b1*x
#y = intercept1 + slope1 * x

# second line:
#y = c0 + c1*x
#y = intercept2 + slope2 * x

# third line
#y = d0 + d1 *x
#y = intercept3 + slope3 * x

# At the breakpoint (break1), the segments b and c intersect

#b0 + b1*x = c0 + c1*x

b0 <- coef(my.seg)[[1]]
b1 <- coef(my.seg)[[2]]

# Important:
# the coefficients are the differences in slope in comparison to the previous slope
c1 <- coef(my.seg)[[2]] + coef(my.seg)[[3]]
break1 <- my.seg$psi[[3]]

#Solve for c0 (intercept of second segment):
c0 <- b0 + b1 * break1 - c1 * break1


# At the breakpoint (break2), the two lines are the same again:
# the coefficients are the differences in slope in comparison to the previous slope
d1 <- coef(my.seg)[[4]] + c1
break2 <- my.seg$psi[[4]]

#Solve for d0 (intercept of third segment):
d0 <- c0 + c1 * break2 - d1 * break2

# adding lines to the graph

# line before first breakpoint
p <- p + geom_abline(intercept = b0, slope = b1, 
                     aes(colour = "first part"), show_guide = TRUE)
p

# DENSITY PLOTS
# Kernel Density Plot
d <- density(df$abundance) # returns the density data
plot(d) # plots the results



fits_encs <- lmList(encs ~ year | state, data=indices)
summary(fits_encs)
summary(fits_encs$aguascalientes)
coefs_encs <- coef(fits_encs)

fits_encs <- lmList(encs ~ year | region, data=indices)
summary(fits_encs)
fits_encs$central

fit <- fits_encs %>%
  filter(term == "year") %>% 
  mutate(p.value = case_when(p.value < 0.05 ~ "significant",
                             p.value >= 0.05  ~ "non significant"))

coefs_encs <- coef(fits_encs)

df = group_by(indices, region) %>%
  do(m1 = lm(encs ~ year, data = .)) # change the name of diversity index, there MUST be a way to do this at once i.e. for loop, lapply

lm_region <- tidy.lm(df, m1)   # gives coefficients, SE, p-value, etc.
lm_region_sig <- lm_region %>%
  filter(term == "year", p.value < 0.05)

write.csv(lm_region_sig, file = "encs_lm_region_sig.csv")

lm_regions <- df %>% 
  group_by(region) %>%
  do(my.lm = lm(evenness ~ year, data = .)) %>% 
  ungroup %>% 
  transmute(region, RegionsCoef = map(my.lm, tidy)) %>% 
  unnest(RegionsCoef)

glance(fits_encs)
xs <- split(indices,f = indices$region)
# ENCS
p1 <- ggplot(xs$central,aes(x = year,y = encs,group = 1,colour = state)) + 
  geom_jitter() + 
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(limits = c(0, 16))+
  theme(legend.title = element_blank(), legend.position=c(0.1, 0.9)) +
  facet_wrap(~region, ncol=1)
p1
p2 <- p1 %+% xs$south
p3 <- p1 %+% xs$central_west
p4 <- p1 %+% xs$north_east
p5 <- p1 %+% xs$north_west

grid.arrange(p1,p2,p3,p4,p5, ncol = 5)

# Evenness
p1 <- ggplot(xs$central,aes(x = year,y = evenness,group = 1,colour = state)) + 
  geom_jitter(size=0.5) + 
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(limits = c(0, 1))+
  theme(legend.title = element_blank(), legend.position=c(0.1, 0.9)) +
  facet_wrap(~region, ncol=1)
p1
p2 <- p1 %+% xs$south
p3 <- p1 %+% xs$central_west
p4 <- p1 %+% xs$north_east
p5 <- p1 %+% xs$north_west

grid.arrange(p1,p2,p3,p4,p5, ncol = 5)

p1 <- ggplot(xs$central,aes(x = year,y = encs,group = 1,colour = state)) + 
  geom_jitter(size=0.5) + 
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(limits = c(0, 16))+
  theme(legend.title = element_blank(), legend.position=c(0.1, 0.9)) +
  facet_wrap(~region, ncol=1)
p1
p2 <- p1 %+% xs$south
p3 <- p1 %+% xs$central_west
p4 <- p1 %+% xs$north_east
p5 <- p1 %+% xs$north_west

grid.arrange(p1,p2,p3,p4,p5, ncol = 5)

p1 <- ggplot(xs$central,aes(x = year,y = encs,group = 1,colour = state)) + 
  geom_jitter(size=0.5) + 
  geom_smooth(method="lm", se=T) +
  scale_y_continuous(limits = c(0, 16))+
  theme(legend.title = element_blank(), legend.position=c(0.1, 0.9)) +
  facet_wrap(~region, ncol=1)
p1
p2 <- p1 %+% xs$south
p3 <- p1 %+% xs$central_west
p4 <- p1 %+% xs$north_east
p5 <- p1 %+% xs$north_west

grid.arrange(p1,p2,p3,p4,p5, ncol = 5)
