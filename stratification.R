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

## LOAD DATA ---------------------------------------------------------------------
# Sequence of displacement
load("DATA/final_temporal_sequence_array.RData")

# Temporal information
load("DATA/tva_date.RData")

# DTM
load("DATA/dtm.RData")

# Velocity
load("DATA/velocity_filtered.RData")

## Preliminar visualizations -----------------------------------------------------
save.dir <- paste0(getwd(), "/images/stratification")
dir.create(save.dir)

# Plot velocity in the considered area
range(velocity[!is.na(velocity)])
vel.min <- -3
vel.max <- 3
## Disclaimer: these thresholds on velocity for visualization purposes are commonly
##             employed by engineers working on interferometry.

vel_corr <- velocity
vel_corr[which(velocity > vel.max)] <- vel.max
vel_corr[which(velocity < vel.min)] <- vel.min

xx <- rep(1:dim(vel_corr)[1], dim(vel_corr)[2])
yy <- rep(1:dim(vel_corr)[2], each=dim(vel_corr)[1])
vel.df <- data.frame(x=xx,y=yy,z=as.vector(vel_corr))

x11()
ggplot(data=vel.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis",na.value="white",
                        limits=c(vel.min,vel.max)) +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent pixels",
       x="", y="")

## Consider only the pixels with velocity in (-0.3,0.3)
idxs_low <- which(velocity>=-0.05 & velocity <=0.05)
{
  low_vel <- array(NA, dim=c(601,601))
  low_vel[idxs_low] = velocity[idxs_low]
  
  xx <- rep(1:dim(low_vel)[1], dim(low_vel)[2])
  yy <- rep(1:dim(low_vel)[2], each=dim(low_vel)[1])
  low_vel.df <- data.frame(x=xx,y=yy,z=as.vector(low_vel))
  
  x11()
  ggplot(data=low_vel.df,aes(x=x,y=y,fill=z)) +
    scale_fill_continuous(type="viridis",
                          na.value="white",
                          limits=c(vel.min,vel.max)) +
    geom_tile() + 
    labs(fill= "Mean velocity",
         title= "Mean velocity in coherent and slowly moving pixels",
         x="", y="")
}




c(tva_date[1],tva_date[392])
# Time window goes from 2015-03-27 to 2023-02-22
# Let us consider one year:
year <- tva_date[1:27]
c(year[1], year[27])

#' First plot
#' Transform matrix into vectors
#' 

j <- 2

dtm_vec <- as.vector(dtm[idxs_low])
seq_vec <- as.vector(seq_temp[,,j])
seq_vec_low <- seq_vec[idxs_low]


x11()
plot(dtm_vec, seq_vec_low, pch=16, main=paste0("Date: ", tva_date[j]))

for(date in year)
{
  plot()
}

## Linear regression -------------------------------------------------------------



## Monotonic smoothing -----------------------------------------------------------