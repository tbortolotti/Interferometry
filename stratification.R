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
c(tva_date[1],tva_date[392])
# Time window goes from 2015-03-27 to 2023-02-22
# Let us consider one year:
year <- tva_date[1:27]
c(year[1], year[27])

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

## Consider only the pixels with velocity in (-0.5,0.5)
idxs_low <- which(velocity>=-0.5 & velocity <=0.5)
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

# Work with the image of the second day
j <- 2
day2 <- tva_date[2][[1]]

daily_image <- seq_temp[,,j]
low_dtm <- array(NA, dim=c(601,601))
low_dtm[idxs_low] <- dtm[idxs_low]
low_dimage <- array(NA, dim=c(601,601))
low_dimage[idxs_low] <- daily_image[idxs_low]

low_dtm_vec <- as.vector(low_dtm)
low_seq_vec <- as.vector(low_dimage)

# For plots that follow
range(low_dtm_vec[!is.na(low_dtm_vec)])
x.min <- 40
x.max <- 460

range(low_seq_vec[!is.na(low_seq_vec)])
y.min <- -2.5
y.max <- 2.5

#' First plot

x11()
plot(low_dtm_vec, low_seq_vec, pch=16, main=paste0("Date: ", tva_date[j]))

## Moving window -----------------------------------------------------------------
window_width <- 100

for(i in 1:6){
  for(j in 1:6){
    
    rectangle.x <- ((i-1)*window_width+1):(window_width*i)
    rectangle.y <- ((j-1)*window_width+1):(window_width*j)
    
    seq_ij <- low_dimage[rectangle.x, rectangle.y]
    dtm_ij <- low_dtm[rectangle.x, rectangle.y]
    
    seqij_vec <- as.vector(seq_ij)
    dtmij_vec <- as.vector(dtm_ij)
    
    pdf(file = paste0(save.dir,"/displ_vs_dtm_i",i,"_j",j,".pdf"), width = 8, height = 6)
    par(mar = c(5, 4, 4, 6))
    plot(dtmij_vec, seqij_vec, pch=16, main=paste0(day2, " in window: i=",i, " j=",j ),
         xlim=c(x.min, x.max), ylim=c(y.min,y.max))
    dev.off()
    
  }
}

## Linear regression -------------------------------------------------------------



## Monotonic smoothing -----------------------------------------------------------