# NMDS

#### Load packages #####
library(vegan)
library(tidyverse)

#### Frequently used objects ####
regions <- c("Central", "Central West", "North East", "North West", "South")
period <- 1980:2016
decades <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2016)

regions_rep <- factor(c(rep(1,37), rep(2,37), rep(3,37), rep(4,37), rep(5,37)),  labels = regions)
regions_rep <- factor(rep(regions, 3))
period <- rep(1980:2016, 5)
decades_rep <- rep(decadas, 5)

#### Load and wrangle data  ####
data <- SIAP %>% 
  #filter(crop_group == "Fruits") %>% 
  #group_by(region, crop, year) %>% # modify for region, state or municipal level
  #group_by(COV_ID, crop, year) %>% # modify for region, state or municipal level
  #group_by(region, crop_group, year) %>% # modify for region, state or municipal level
  group_by(region, crop, year) %>% # modify for region, state or municipal level
  #group_by(state, crop_group, year) %>% # modify for region, state or municipal level
  summarise(ag_harv = sum(harvested))

data <- data %>%
  spread(crop, ag_harv)
  #spread(crop_group, ag_harv)

data[is.na(data)] <- 0

data <- as.data.frame(data)

data <- data %>% 
  filter(year %in% decades) %>% 
  #filter(region == "north_east") %>% 
  select(-region, -year)
  #select(-state, -year)



## Bray-Curtis distances between samples
#dis <- vegdist(varespec)
dis <- vegdist(data)
## First 16 sites grazed, remaining 8 sites ungrazed
#groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))

## Calculate multivariate dispersions
mod <- betadisper(dis, regions_rep)
mod

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 99)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## `scores` and `eigenvals` also work
scrs <- scores(mod)
str(scrs)
head(scores(mod, 1:4, display = "sites"))
# group centroids/medians 
scores(mod, 1:4, display = "centroids")
# eigenvalues from the underlying principal coordinates analysis
eigenvals(mod) 

## try out bias correction; compare with mod3
(mod3B <- betadisper(dis, groups, type = "median", bias.adjust=TRUE))
anova(mod3B)
permutest(mod3B, permutations = 99)

## should always work for a single group
group <- factor(rep("grazed", NROW(varespec)))
(tmp <- betadisper(dis, group, type = "median"))
(tmp <- betadisper(dis, group, type = "centroid"))

## simulate missing values in 'd' and 'group'
## using spatial medians
groups[c(2,20)] <- NA
dis[c(2, 20)] <- NA
mod2 <- betadisper(dis, groups) ## messages
mod2
permutest(mod2, permutations = 99)
anova(mod2)
plot(mod2)
boxplot(mod2)
plot(TukeyHSD(mod2))

## Using group centroids
mod3 <- betadisper(dis, regions_rep, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))



ord <- metaMDS(data)
ord$stress
stressplot(ord)
ordiplot(ord)
ordihull(ord, period)
ordiellipse(ord, period)



data(data)
attach(data)
plot(ord, disp="sites", type="n")
ordihull(ord, groups=groups, draw = "polygon", col="grey90")
ordiellipse(ord, decadas, col=1:37, kind = "ehull", lwd=3)
ordiellipse(ord, period, col=1:4, draw="polygon")
ordispider(ord, decadas, col=1:37, label = TRUE)
points(ord,groups,disp="sites", pch=21, col="red",cex=1.3)



## Raw data and plotting
data(sipoo)
m <- betadiver(data)
plot(m)
## The indices
betadiver(help=TRUE)
## The basic Whittaker index
d <- betadiver(data, "w")
## This should be equal to Sorensen index (binary Bray-Curtis in
## vegan)
range(d - vegdist(data, binary=TRUE))


data("varespec")
euc_dij <- vegdist(data, method = "euclidian")
bc_dij <- vegdist(data)


# Principal Coordinates Analysis
pco1 <- wcmdscale(vegdist(data), eig = T)
round(eigenvals(pco1), 3) # from this we get negative eigenvalues
# Correct eigenvalues (to get all positives)
pco2 <- wcmdscale(vegdist(data), eig = T, add = "lingoes")
round(eigenvals(pco2),3)


pco <- wcmdscale(vegdist(data), eig = T)

plot(pco)

scrs <- scores(pco, choices = 1:2)

spp_scrs <- wascores(scrs, data, expand = F)

# add
points(spp_scrs, col = "red", pch = 19)

# NMDS
sol <- metaMDS(data, trace = T)
plot(sol, main = "NMDS plot")
stressplot(sol, main = "Shepard plot")

k_vec <- 1:10
stress <- numeric(length(k_vec))
dune_dij <- metaMDSdist(data, trace = F)
set.seed(25)
for(i in seq_along(k_vec)){
  sol <- metaMDSiter(dune_dij, k = i, 
                     trace = F)
  stress[i] <- sol$stress
}

plot(k_vec, stress, type = "b", ylab = "Stress",
     xlab = "Dimensions")

g <- goodness(sol)

sum(g^2)

sol$stress^2

data.scores = as.data.frame(scores(sol))

#add columns to data frame 
#data.scores$Sample = data$Sample

data <- SIAP %>% 
  group_by(region, crop, year) %>% # modify for region, state or municipal level
  #group_by(state, crop_group, year) %>% # modify for region, state or municipal level
  summarise(ag_harv = sum(harvested))

data <- data %>%
  spread(crop, ag_harv)
#spread(crop_group, ag_harv)

data[is.na(data)] <- 0

data <- as.data.frame(data)

data <- data %>% 
  filter(year %in% decades)

str(data$year)
data$year <- as.factor(data$year)

data.scores$year = data$year
data.scores$region = data$region

head(data.scores)
str(data.scores)
library(ggplot2)

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape = year, colour = region))#+ 
  #theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
   #     axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
    #    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
     #   legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
      ## legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        #panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        #legend.key=element_blank()) + 
 # labs(x = "NMDS1", colour = "Time", y = "NMDS2", shape = "Type")  + 
#  scale_colour_manual(values = c("#009E73", "#E69F00")) 

xx






