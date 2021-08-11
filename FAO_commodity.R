# Load packages
library(tidyverse)

# Load data
SIAP <- read.csv("/Users/erikaluna/R\ Studio/crop_diversity/archive/SIAP_v2.csv")
FAO_crops <- read.csv("csv/Equivalencies-FAO-SIAP.csv")
colnames(FAO_crops) <- c("crop", "FAO_code", "FAO_crop", "crop_group")

SIAP_FAO <- left_join(SIAP, FAO_crops, by="crop")

# Save new SIAP (master) file
write.csv(SIAP_FAO, "SIAP.csv")



