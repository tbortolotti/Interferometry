setwd("/Users/teresabortolotti/Documents/R/Interferometry")

library(RColorBrewer)
library(ggplot2)
library(lattice)
library(lubridate)
library(viridis)

rm(list=ls())
graphics.off()
cat("\014")

#' Estimation of the stratification effect
#' 

## LOAD DATA
# Sequence of displacement
load("DATA/filtered_sequence_array.RData")

# Temporal information
load("DATA/tva_date.RData")

# DTM
load("DATA/dtm.RData")

# Velocity
load("DATA/velocity.RData")

## Preliminar visualizations -----------------------------------------------------
save.dir <- paste0(getwd(), "/images/stratification")
dir.create(save.dir)

# 


c(tva_date[1],tva_date[392])
# Time window goes from 2015-03-27 to 2023-02-22
# Let us consider one year:
year <- tva_date[1:27]
c(year[1], year[27])

#' First plot
#' Transform matrix into vectors
#' 

j <- 4

dtm_vec <- as.vector(dtm)
seq_vec <- as.vector(-my_seq_arr[,,j]*5.5/(4*pi))
x11()
plot(dtm_vec, seq_vec, pch=16, main=paste0("Date: ", tva_date[j]))

for(date in year)
{
  plot()
}

## Linear regression -------------------------------------------------------------



## Monotonic smoothing -----------------------------------------------------------