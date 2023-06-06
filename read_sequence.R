setwd("/Users/teresabortolotti/Documents/R/Interferometry")

rm(list=ls())
graphics.off()
cat("\014")

# ## Read first 100 rows of a very large csv file
# prova <- read.csv("export/seq_2550x2450x392.dat_TopoInterf_CAL_rampe_presenti.csv", skip=0,
#                   nrow=100, header=FALSE)
# #read.csv(skip=100+1, nrow=100, header=FALSE,)
# View(prova)


my_seq <- read.csv("DATA/filtered_sequence.csv",sep=',', header=FALSE)
my_seq <- my_seq[,-c(1,2)]

# Load mask
con = file("DATA/coh_temporal.dat", "rb")
bindata = readBin(con, "numeric", n=2550*2450, size=4)
coh = array(bindata, dim=c(2550,2450))
close(con)
mask = array(NA, dim=c(2550,2450))
mask[coh>=0.8] = 1

mask <- mask[600:1200, 1000:1600]

# Create and save masked array
my_seq_arr <- array(NA, dim=c(601,601,392))
for(t in 1:392)
{
  my_seq_arr[,,t] <- array(my_seq[,t], dim=c(601,601))*mask
}

save(my_seq_arr, file="DATA/filtered_sequence_array.RData")

# load("DATA/filtered_sequence_array.RData")
# lambda <- 5.54658
# seq_temp <- - my_seq_arr*lambda/(4*pi)
# save(seq_temp, file="DATA/final_temporal_sequence_array.RData")


## UTILITIES: read CSV sequentially -----------------------------------------------
# Create an array and save the time sequence in the selected small window
# my_seq <- array(NA, dim=c(601,601,392))
# 
# B <- 601
# (Start.Time <- Sys.time())
# pb <- progress_bar$new(total=B)
# for(b in 1:B){
#   j <- 1000 + (b-1)
#   nrows <- 2550
#   skip_par <- (j-1)*nrows
#   seq_csv <- read.csv("DATA/seq_2550x2450x392.dat_TopoInterf_CAL_rampe_presenti.csv",
#                       skip=skip_par,
#                       nrow=nrows, header=FALSE)
#   my_seq[,j,] <- seq_csv[600:1200,3:394]
#   
#   pb$tick()
# }
# End.Time <- Sys.time()
# round(End.Time - Start.Time, 2)
