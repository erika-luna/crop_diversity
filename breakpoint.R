library(ggplot2)

df <- read.csv("indices_region_37.csv")
df <- df %>% 
  filter(region == "south")

# create a figure to get an idea of the data
p <- ggplot(df, aes(x = year, y = evenness)) + geom_line()
p

my.lm <- lm(evenness ~ year, data = df)
summary(my.lm)

# a linear model with data for the part after 16 km
#my.lm2 <- lm(evenness ~ year, data = df[df$year > 1993, ])
#summary(my.lm2)

# Extract te coefficients from the overall model
my.coef <- coef(my.lm)

# add the regression line to the graph
# setting the aesthetics to a constant - this provides a name that we can reference later when we add additional layers
p <- p + geom_abline(intercept = my.coef[1], 
                     slope = my.coef[2], 
                     aes(colour = "overall"))
p

# -------------------
# analyse breakpoints
# -------------------
# http://cran.r-project.org/doc/Rnews/Rnews_2008-1.pdf
library(segmented)

my.lm <- lm(evenness ~ year, data = df)
summary(my.lm)

# have to provide estimates for breakpoints.
# after looking a the data, 
my.seg <- segmented(my.lm, 
                    seg.Z = ~ year, 
                    psi = list(year = c(1990,2000)))

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
my.model <- data.frame(year = df$year, evenness = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = year, y = evenness)) + geom_line()

# add the fitted data to the exisiting plot
p <- p + geom_line(data = my.model, aes(x = year, y = evenness), colour = "tomato")

# add vertical lines to indicate the break locations
# second row of the psi-matrix
my.lines <- my.seg$psi[, 2]

p<- p + geom_vline(xintercept = my.lines, linetype = "dashed")
p


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

######
library(segmented)
set.seed(12)
xx <- 1:100
zz <- runif(100)
yy <- 2 + 1.5*pmax(xx - 35, 0) - 1.5*pmax(xx - 70, 0) + 15*pmax(zz - .5, 0) + 
  rnorm(100,0,2)
dati <- data.frame(x = xx, y = yy, z = zz)
out.lm <- lm(evenness ~ year, data = df)
o <- segmented(my.lm, seg.Z = ~year, psi = list(year = c(1990,2000,2010)),
               control = seg.control(display = FALSE))
dat2 = data.frame(x = c(1980:2016), y = broken.line(o)$fit)

library(ggplot2)
ggplot(dat2, aes(x = x, y = y)) +
  #geom_point() #+
  geom_line(color = 'blue')


#####
# Fit segmented model
exp10 <- function(x)10^x

sfit <- segmented(my.lm, seg.Z = ~ year)
exp10(sfit$psi)
summary(sfit)

bpoints <- 10^(sfit$psi)[, 2]
newx <- c(0.2, bpoints, 50)
newy <- predict(sfit, data.frame(year = log10(newx)))

ggplot(aes(y = x, x = y), data = df) + 
  geom_path(data = data.frame(Distance = newx, Speed = newy), colour = "grey 50", size = 2) +
  #geom_point(aes(colour = Standard.Event), size = 3) + 
  #geom_text(data = labels1, aes(label = Text), hjust = -0.2, vjust = -0.2) +
  #geom_text(data = labels2, aes(label = Text), hjust = 1.2, vjust = -0) +
  scale_x_log10() + 
  theme_bw(16) + 
  ggtitle("World record speed for mens running events")
