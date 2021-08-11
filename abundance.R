library(ggsci)
library(broom)

crop_ag_region <- SIAP %>% 
  group_by(region, year, crop) %>% 
  summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            HAR = round(sum(harvested)/sum(planted), digits = 2))

crop_ag_region <- crop_ag_region %>% 
  select(c("region", "year", "crop", "ag_harv")) %>% 
  spread(year, ag_harv)

write.csv(crop_ag_region, file = "racebar_regions.csv")

crop_group_ag_region <- SIAP %>%
  group_by(region, year, crop_group, FAO_crop) %>% 
  summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            HAR = round(sum(harvested)/sum(planted), digits = 2))

crop_group_ag_region <- crop_group_ag_region %>% 
  select(c("region", "year", "crop_group", "FAO_crop","ag_harv")) %>% 
  spread(year, ag_harv)

write.csv(crop_group_ag_region, file = "racebar_regions_crop_group.csv")

SIAP_ag <- SIAP %>% 
  group_by(year, crop_group, FAO_crop) %>% 
  summarise(ag_harv = sum(harvested)) #%>% 
  filter(crop_group == "Cereals")  %>%  
  ggplot(aes(year, ag_harv, colour = FAO_crop)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Harvested Area (ha)") +
  ggtitle("Cereals")

groups = unique(SIAP_ag$crop_group)
group_plots = list()
for(group in groups) {
    group_plots[[group]] = ggplot(SIAP_ag %>% filter(crop_group == group), aes(x=year, y=ag_harv)) +
    geom_line(aes(color = FAO_crop)) +
    scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Harvested Area (ha)") +
    ggtitle(group)  
  print(group_plots[[group]])
  ggsave(group_plots[[group]], file=paste0("plot_", group,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
}

# Country level
data <- SIAP %>% 
  #group_by(crop_group, year) %>% 
  filter(crop_group == "Sugar crops and sweetners") %>% 
  group_by(crop, year) %>% 
  summarise(ag_harv = sum(harvested)) #%>% 
  #filter(year == 1980 | year == 2016) # Initial and final year

data %>% 
  ggplot(aes(year, ag_harv, color = crop_group)) +
  geom_point() +
  geom_smooth(method = "lm")

#slopes <- lm(ag_harv~year*crop_group, data=data)
slopes <- lm(ag_harv~year*crop, data=data)


slopes <- tidy(slopes)

changes <- data %>% 
  #group_by(crop_group) %>% 
  group_by(crop) %>% 
  do(tidy(lm(ag_harv ~ year, data = .))) %>% 
  #select(crop_group = crop_group, term, slope = estimate) %>% 
  select(crop = crop, term, slope = estimate) %>% 
  filter(term == "year")

changes %>% 
  #ggplot(aes(reorder(crop_group, -slope), slope)) +
  ggplot(aes(reorder(crop, -slope), slope)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 45))

perc_change <- function(x, y) { return(x/y*100-100) }

change <- data %>%
  group_by(crop_group) %>% 
  mutate(perc_change = round(perc_change(ag_harv, lag(ag_harv)), 2))

perc_area <- data %>% 
  group_by(year) %>% 
  mutate(percent = round(ag_harv/sum(ag_harv)*100, 2)) %>% 
  filter(year == 1980 | year == 2016)





perc_area %>% 
  ggplot(aes(year, percent, fill = crop_group)) +
  geom_area() +
  #scale_fill_brewer(palette = "Set3") +
  labs(x = "Year", y = "Proportion of Harvested Area") +
  ggtitle("Country level")



# Regional level
data <- SIAP %>% 
  group_by(region, crop_group, year) %>% 
  summarise(ag_harv = sum(harvested)) %>% 
  filter(year == 1980 | year == 2016)
  
perc_area <- data %>% 
  group_by(region, year) %>% 
  mutate(perc = ag_harv/sum(ag_harv)) %>% 
  filter(year == 1980 | year == 2016)

colnames(perc_area) <- c("regions", "crop_group", "year", "ag_harv", "percent")

regions = unique(SIAP$region)
region_plots = list()
for(region in regions) {
  region_plots[[region]] = ggplot(perc_area %>% filter(regions == region), aes(x=year, y=percent)) +
    geom_area(aes(fill = crop_group)) +
    labs(x = "Year", y = "Proportion of Harvested Area (ha)") +
    ggtitle(region)  
  print(group_plots[[group]])
  #ggsave(group_plots[[group]], file=paste0("plot_", group,".png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
}

do.call(grid.arrange,region_plots)

ggarrange(region_plots$central, 
          region_plots$central_west,
          region_plots$north_east,
          region_plots$north_west,
          region_plots$south,
          common.legend = TRUE, legend="right")

# Transition Matrix
library(markovchain)

state1 <- data %>% 
  group_by(year) %>% 
  mutate(percent = round(ag_harv/sum(ag_harv)*100, digits = 3)) %>% 
  filter(year==1980)

state2 <- data %>% 
  group_by(year) %>% 
  mutate(percent = round(ag_harv/sum(ag_harv)*100, digits = 3)) %>%
  filter(year==2016)

states <- right_join(state1, state2, by = "crop_group")

write.csv(states, "states.csv")

state1 <- data %>%
  filter(year==1980) %>% 
  summarise(prop = ag_harv/sum(ag_harv))

sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", 
              "b", "a", "a", "b", "b", "b", "a")
mcFit <- markovchainFit(data=sequence)

energyStates <- c("sigma", "sigma_star")
byRow <- TRUE
gen <- matrix(data = c(-3, 3, 1, -1), nrow = 2,
              byrow = byRow, dimnames = list(energyStates, energyStates))
generatorToTransitionMatrix(gen)

statesNames <- c("a", "b", "c")
markovB <- new("markovchain", states = statesNames, transitionMatrix =
                 matrix(c(0.2, 0.5, 0.3, 0, 1, 0, 0.1, 0.8, 0.1), nrow = 3,
                        byrow = TRUE, dimnames=list(statesNames,statesNames)),
               name = "A markovchain Object"
)
transitionProbability(markovB,"b", "c")

state1 <- data %>% 
  filter(year == 1980) %>% 
  group_by(crop_group) %>% 
  summarise(prop = ag_harv/sum(ag_harv))# %>% 
  

state2 <- SIAP %>% 
  group_by(year, crop) %>% 
  summarise(ag_harv = sum(harvested)) %>% 
  filter(year == 2016)

state2 <- state2 %>% 
  mutate(proportion == ag_harv/sum(ag_harv))



df <- merge(state1, state2, by = "crop", all.y = T)
transition = table(df$crop, df$ag_harv.y)
print(transition)

statesNames <- levels(df$crop)
markovB <- new("markovchain", states = statesNames, transitionMatrix =
                 matrix(c(0.2, 0.5, 0.3, 0, 1, 0, 0.1, 0.8, 0.1), nrow = 3,
                        byrow = TRUE, dimnames=list(statesNames,statesNames)),
               name = "A markovchain Object" 
)    
transitionProbability(markovB,"b", "c")




myStates <- sort(unique(c(state1$crop, state2$crop)))
lenSt <- length(statesNames)

currState <- match(state1$crop, statesNames)
nextState <- match(state2$crop, statesNames)
transMat <- matrix(0L, lenSt, lenSt)

transMat[cbind(currState, nextState)] <- 1L
transMat <- transMat/rowSums(transMat)
transMat[is.na(transMat)] <- 0


v1 <- c(0.282051282, 0.384615385, 0.333333333)
v2 <- c(0.301282,  0.339744, 0.358974)
v1/v2

v2/v1

v1%*%v2
v1%o%v2
crossprod(v2)

AUB <- 0.282051282*0.301282
AB <- AUB/0.301282 

tmp <- 0.282051282 + 0.301282 - AUB

statesNames <- c("hola", "adios", "jaja")


markovB <- new("markovchain", states = statesNames, transitionMatrix =
                 matrix(c(0.282051282, 0.384615385, 0.333333333,
                          0.301282, 0.339744, 0.358974, 
                          0.1, 0.8, 0.1), nrow = 3,
                        byrow = TRUE, dimnames=list(statesNames,statesNames)),
               name = "A markovchain Object" )
transitionProbability(markovB,"hola", "jaja")


trans <- read.csv("/Users/erikaluna/Sync/UBC/Research/thesis/trans_v1.csv")
trans <- trans %>% 
  gather("crops", "perc", 2:13)
write.csv(trans, file = "trans.csv")



