setwd("/Users/teresabortolotti/Documents/R/Interferometry")

library(RColorBrewer)
library(ggplot2)
library(lattice)
library(lubridate)
library(viridis)

rm(list=ls())
graphics.off()
cat("\014")

#' First plots, Selection of the Window of Interest, Visualizations of temporal sequence
#'
#'

# save.dir <- "/Users/teresabortolotti/Desktop/StatInf-project/images"

save.dir <- paste0(getwd(), "/images")
dir.create(save.dir)

## VELOCITY -----------------------------------------------------------------------
con = file("DATA/vel_finale.dat", "rb")
bindata = readBin(con, "numeric", n = 2550*2450, size=4)
vel = array(bindata, dim=c(2550,2450))
close(con)

con = file("DATA/coh_temporal.dat", "rb")
bindata = readBin(con, "numeric", n=2550*2450, size=4)
coh = array(bindata, dim=c(2550,2450))
close(con)

mask = array(NA, dim=c(2550,2450))
mask[coh>=0.8] = 1

vel_mask = vel*mask

# velocity <- vel_mask[600:1200, 1000:1600]
# save(velocity, file="DATA/velocity_filtered.RData")

#### Display velocity ----------------------------------------------------------------
vel.min <- -3
vel.max <- 3
## Disclaimer: these thresholds on velocity for visualization purposes are commonly
##             employed by engineers working on interferometry.

vel_mask_corrected <- vel_mask
vel_mask_corrected[which(vel_mask > 3)] <- 3
vel_mask_corrected[which(vel_mask < -3)] <- -3

# rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "Lab")
# pdf(file = paste0(save.dir,"/mean_vel.pdf"), width = 8, height = 6)
# par(mar = c(5, 4, 4, 6))
# levelplot(vel_mask, col.regions=rgb.palette(120), at=seq(vel.min, vel.max, length.out=120),
#           main="Mean velocity in coherent pixels")
# dev.off()

# Convert matrix to dataframe
xx <- rep(1:dim(vel_mask)[1], dim(vel_mask)[2])
yy <- rep(1:dim(vel_mask)[2], each=dim(vel_mask)[1])
vel_mask.df <- data.frame(x=xx,y=yy,z=as.vector(vel_mask_corrected))

# ggplot(data=vel_mask.df,aes(x=x,y=y,fill=z)) +
#   scale_fill_continuous(low="blue", high="red", 
#                         guide="colorbar",na.value="white",
#                         limits=c(vel.min,vel.max)) +
#   geom_tile() + 
#   labs(fill= "Mean velocity",
#        title= "Mean velocity in coherent pixels",
#        x="", y="")

## Changing palette because I realized the first on wasn't the best option
ggplot(data=vel_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis",na.value="white",
                        limits=c(vel.min,vel.max)) +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent pixels",
       x="", y="")

ggsave(filename = "mean_vel_3.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()

#### Display pixels with low velocity ---------------------------------------------
idxs_low <- which(vel>=-0.5 & vel <=0.5)
low_vel <- array(NA, dim=c(2550,2450))
low_vel[idxs_low] = vel[idxs_low]

low_vel_mask = low_vel*mask

## GGPLOT
# Convert matrix to dataframe
xx <- rep(1:dim(low_vel_mask)[1], dim(low_vel_mask)[2])
yy <- rep(1:dim(low_vel_mask)[2], each=dim(low_vel_mask)[1])
low_vel_mask.df <- data.frame(x=xx,y=yy,z=as.vector(low_vel_mask))

# Scala colori originale

# ggplot(data=low_vel_mask.df,aes(x=x,y=y,fill=z)) +
#   scale_fill_continuous(low="blue", high="yellow", 
#                         guide="colorbar",na.value="white",
#                         limits=c(vel.min,vel.max)) +
#   geom_tile() + 
#   labs(fill= "Mean velocity",
#        title= "Mean velocity in coherent and slowly moving pixels",
#        x="", y="")

ggplot(data=low_vel_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis",
                        na.value="white",
                        limits=c(vel.min,vel.max)) +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent and slowly moving pixels",
       x="", y="")

ggsave(filename = "mean_low_vel3.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()

# Scala colori definita dai valori di velocità dei pixel a bassa velocità
ggplot(data=low_vel_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis", 
                        na.value="white") +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent and slowly moving pixels",
       x="", y="")

ggsave(filename = "mean_low_vel_newscale.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()

#### WOI on original map ----------------------------------------------
## FULL VELOCITY MAP
# Convert matrix to dataframe
xx <- rep(1:dim(vel_mask)[1], dim(vel_mask)[2])
yy <- rep(1:dim(vel_mask)[2], each=dim(vel_mask)[1])
vel_mask.df <- data.frame(x=xx,y=yy,z=as.vector(vel_mask_corrected))

# plot with geom_tiles
ggplot(data=vel_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis",
                        na.value="white",
                        limits=c(vel.min,vel.max)) +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent pixels",
       x="", y="") +
  annotate("rect", xmin = 600, xmax = 1200, ymin = 1000, ymax = 1600,
           colour="black",
           fill=NA)

ggsave(filename = "mean_vel3_rect.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()

## LOW VELOCITY MAP
# Convert matrix to dataframe
xx <- rep(1:dim(low_vel_mask)[1], dim(low_vel_mask)[2])
yy <- rep(1:dim(low_vel_mask)[2], each=dim(low_vel_mask)[1])
low_vel_mask.df <- data.frame(x=xx,y=yy,z=as.vector(low_vel_mask))

# Original scale
ggplot(data=low_vel_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis",
                        na.value="white",
                        limits=c(vel.min, vel.max)) +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent and slowly moving pixels",
       x="", y="") +
  annotate("rect", xmin = 600, xmax = 1200, ymin = 1000, ymax = 1600,
           colour="black",
           fill=NA)

ggsave(filename = "mean_low_vel3_rect.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()

# Low velocity scale
ggplot(data=low_vel_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(type="viridis",
                        na.value="white") +
  geom_tile() + 
  labs(fill= "Mean velocity",
       title= "Mean velocity in coherent and slowly moving pixels",
       x="", y="") +
  annotate("rect", xmin = 600, xmax = 1200, ymin = 1000, ymax = 1600,
           colour="black",
           fill=NA)

ggsave(filename = "mean_low_vel_newscale_rect.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()


#### WOI - Isolated -------------------------------------------------------
# vel_wnd <- vel_mask_corrected[600:1200, 1000:1600]
# rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "Lab")
# pdf(file = paste0(save.dir,"/sWOI3.pdf"), width = 8, height = 6)
# par(mar = c(5, 4, 4, 6))
# levelplot(vel_wnd, col.regions=rgb.palette(120), at=seq(vel.min, vel.max, length.out=120),
#           main="Mean velocity in the smallest window of interest")
# dev.off()

vel_wnd <- vel_mask_corrected[600:1200, 1000:1600]
pdf(file = paste0(save.dir,"/sWOI3.pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(vel_wnd, col.regions=viridis(60), at=seq(vel.min, vel.max, length.out=60),
          main="Mean velocity in the smallest window of interest")
dev.off()

# Cambio scala colori
vel_wnd_corrected <- vel_mask_corrected[600:1200, 1000:1600]
vel_wnd_corrected[which(vel_wnd_corrected > 0.5)] <- 0.5
vel_wnd_corrected[which(vel_wnd_corrected < -0.5)] <- -0.5
pdf(file = paste0(save.dir,"/sWOI_newscale.pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(vel_wnd_corrected, col.regions=viridis(60), at=seq(-0.5, 0.5, length.out=60),
          main="Mean velocity in the smallest window of interest")
dev.off()

## DTM ------------------------------------------------------------------------
con = file("DATA/Quota.dat", "rb")
bindata = readBin(con, "numeric", n = 2450*2550, size=4)
quota = array(bindata, dim=c(2450,2550))
close(con)

quota <- t(quota)

con = file("DATA/coh_temporal.dat", "rb")
bindata = readBin(con, "numeric", n=2550*2450, size=4)
coh = array(bindata, dim=c(2550,2450))
close(con)

mask = array(NA, dim=c(2550,2450))
mask[coh>=0.8] = 1

quota_mask = quota*mask

# Plot
xx <- rep(1:dim(quota_mask)[1], dim(quota_mask)[2])
yy <- rep(1:dim(quota_mask)[2], each=dim(quota_mask)[1])
quota_mask.df <- data.frame(x=xx,y=yy,z=as.vector(quota_mask))

# plot with geom_tiles
ggplot(data=quota_mask.df,aes(x=x,y=y,fill=z)) +
  scale_fill_continuous(low="blue", high="yellow", 
                        guide="colorbar",na.value="white") +
  geom_tile() + 
  labs(fill= "DTM",
       title= "DTM in coherent pixels",
       x="", y="")

ggsave(filename = "dtm.png",
       plot = last_plot(),
       device = NULL,
       path = save.dir,
       scale = 1,
       width = 8,
       height = 6,
       units = "in",
       dpi = 320,
       limitsize = TRUE,
       bg = NULL,)
dev.off()

# dtm <- quota_mask[600:1200, 1000:1600]
# save(dtm, file="DATA/dtm.RData")


#### DTM in the area of interest -------------------------------------------------
quota_wnd <- quota_mask[600:1200, 1000:1600]
q.min <- min(quota_wnd[!is.na(quota_wnd)])
q.max <- max(quota_wnd[!is.na(quota_wnd)])
rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "Lab")
pdf(file = paste0(save.dir,"/WOI_dtm.pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(quota_wnd, col.regions=rgb.palette(120), at=seq(q.min, q.max, length.out=120),
          main="DTM in the window of interest")
dev.off()

## TEMPORAL INFORMATION -----------------------------------------------------------
con = file("DATA/tva_392.dat", "rb")
tva = readBin(con, "double", n=392)
close(con)

convert_decimal_date <- function(decimal_year) {
  year <- floor(decimal_year)
  remainder <- decimal_year - year
  
  # Convert decimal remainder to number of days
  days_in_year <- ifelse(leap_year(year), 366, 365)
  days <- as.integer(remainder * days_in_year)
  
  # Create a date object for the given year and days
  date <- as.Date(paste0(year, "-01-01"), "%Y-%m-%d") + days
  
  # Extract year, month, and day from the date
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))
  
  date_as_string <- paste(year, sprintf("%02d", month), sprintf("%02d", day), sep = "-")
  
  return(date_as_string)
}

tva_converted <- list()
for(t in 1:length(tva))
{
  tva_converted[[t]] <- convert_decimal_date(tva[t]) 
}
tva_date <- tva_converted
save(tva_date, file="DATA/tva_date.RData")

## TEMPORAL SEQUENCE ---------------------------------------------------------------
load("DATA/filtered_sequence_array.RData")

d.min <- quantile(my_seq_arr[!is.na(my_seq_arr)], probs=0.1)
d.min <- as.numeric(d.min)
d.max <- quantile(my_seq_arr[!is.na(my_seq_arr)], probs=0.9)
d.max <- as.numeric(d.max)
t_seq <- c(2,3,4,5,6,7)

dir.create(paste0(save.dir,"/sequence"))


## 2
t <- t_seq[1]
image_t <- my_seq_arr[,,t]
image_t[which(image_t>d.max)] <- d.max
image_t[which(image_t<d.min)] <- d.min
pdf(file = paste0(save.dir,"/sequence/WOI_seq_",t,".pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(image_t, col.regions=viridis(60), at=seq(d.min, d.max, length.out=60),
          main=paste0("Displacement in the window of interest at ", tva_converted[[t]]))
dev.off()

## 3
t <- t_seq[2]
image_t <- my_seq_arr[,,t]
image_t[which(image_t>d.max)] <- d.max
image_t[which(image_t<d.min)] <- d.min
pdf(file = paste0(save.dir,"/sequence/WOI_seq_",t,".pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(image_t, col.regions=viridis(60), at=seq(d.min, d.max, length.out=60),
          main=paste0("Displacement in the window of interest at ", tva_converted[[t]]))
dev.off()

## 4
t <- t_seq[3]
image_t <- my_seq_arr[,,t]
image_t[which(image_t>d.max)] <- d.max
image_t[which(image_t<d.min)] <- d.min
pdf(file = paste0(save.dir,"/sequence/WOI_seq_",t,".pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(image_t, col.regions=viridis(60), at=seq(d.min, d.max, length.out=60),
          main=paste0("Displacement in the window of interest at ", tva_converted[[t]]))
dev.off()

## 5
t <- t_seq[4]
image_t <- my_seq_arr[,,t]
image_t[which(image_t>d.max)] <- d.max
image_t[which(image_t<d.min)] <- d.min
pdf(file = paste0(save.dir,"/sequence/WOI_seq_",t,".pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(image_t, col.regions=viridis(60), at=seq(d.min, d.max, length.out=60),
          main=paste0("Displacement in the window of interest at ", tva_converted[[t]]))
dev.off()

## 6
t <- t_seq[5]
image_t <- my_seq_arr[,,t]
image_t[which(image_t>d.max)] <- d.max
image_t[which(image_t<d.min)] <- d.min
pdf(file = paste0(save.dir,"/sequence/WOI_seq_",t,".pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(image_t, col.regions=viridis(60), at=seq(d.min, d.max, length.out=60),
          main=paste0("Displacement in the window of interest at ", tva_converted[[t]]))
dev.off()

## 7
t <- t_seq[6]
image_t <- my_seq_arr[,,t]
image_t[which(image_t>d.max)] <- d.max
image_t[which(image_t<d.min)] <- d.min
pdf(file = paste0(save.dir,"/sequence/WOI_seq_",t,".pdf"), width = 8, height = 6)
par(mar = c(5, 4, 4, 6))
levelplot(image_t, col.regions=viridis(60), at=seq(d.min, d.max, length.out=60),
          main=paste0("Displacement in the window of interest at ", tva_converted[[t]]))
dev.off()



