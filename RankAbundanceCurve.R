library(vegan)
data(dune.env)
data(dune)

data <- SIAP %>% 
  #filter(crop_group == "Fruits") %>% 
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

data <- data %>% 
  #filter(year == 1990)
  filter(region == "north_east")

data_1980 <- data %>% 
  #filter(year == 1990) %>% 
  filter(region == "north_east") %>% 
  select(-region, -year)

RankAbun.1 <- BiodiversityR::rankabundance(data_1980)
RankAbun.1
rankabunplot(RankAbun.1, scale='abundance', addit=FALSE, specnames=c(1,2,3))

rankabunplot(RankAbun.1, scale='logabun', addit=FALSE, specnames=c(1:30), 
             srt=45, ylim=c(1,100))
rankabuncomp(dune, y=dune.env, factor='Management', 
             scale='proportion', legend=FALSE)


rankabuncomp(data_1980, y=data, factor='region', 
             scale='proportion', legend=FALSE)

data$year <- as.factor(data$year)
rankabuncomp(data_1980, y=data, factor='year', 
             scale='proportion', legend=FALSE)

## CLICK IN THE GRAPH TO INDICATE WHERE THE LEGEND NEEDS TO BE PLACED
## IF YOU OPT FOR LEGEND=TRUE.

## Not run: 
# ggplot2 plotting method

# Only label the two most abundant species
RA.data <- rankabuncomp(dune, y=dune.env, factor='Management', 
                        return.data=TRUE, specnames=c(1:2), legend=FALSE)

library(ggplot2)
library(ggrepel)

# possibly need for extrafont::loadfonts(device="win") to have Arial
# as alternative, use library(ggThemeAssist)
BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12, family="Arial"),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

plotgg1 <- ggplot(data=RA.data, aes(x = rank, y = abundance)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(aes(colour=Grouping, shape=Grouping), size=5, alpha=0.7) +
  geom_text_repel(data=subset(RA.data, labelit == TRUE), 
                  aes(colour=Grouping, label=species), 
                  angle=45, nudge_x=1, nudge_y=1, show.legend=FALSE) +
  BioR.theme +
  scale_color_brewer(palette = "Set1") +
  labs(x = "rank", y = "abundance", colour = "Management", shape = "Management")

plotgg1

# use different facets
# now label first 10 species
RA.data <- rankabuncomp(dune, y=dune.env, factor='Management', 
                        return.data=TRUE, specnames=c(1:10), legend=FALSE)

plotgg2 <- ggplot(data=RA.data, aes(x = rank, y = abundance)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(aes(colour=Grouping), size=5, alpha=0.7) +
  geom_text_repel(data=subset(RA.data, labelit == TRUE), 
                  aes(label=species), 
                  angle=45, nudge_x=1, nudge_y=1, show.legend=FALSE) +
  BioR.theme +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ Grouping) +
  labs(x = "rank", y = "abundance", colour = "Management")

plotgg2







