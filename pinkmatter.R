#### Making a bunch of spectrograms at once using seewave ####

# set working directory to folder with all WTSP recordings. For practice, I'm using my chickadee data
setwd("C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Remy (Rm-RR_HZCH_SPPUA)/chopped")

library(seewave)
library(tuneR)


# make a function that will read in a file and make a spectrogram that can be used in lapply later
SpectroMaker<-function(x) {
  a<-readWave(x)
  spectro(a, wl = 512, ovlp = 95, collevels = seq(-42,0,6), 
          tlim = c(0, 1.259),
          flim = c(0, 10),
          osc = F, scale = F, colgrid = "gray", cexlab = 0.8, cexaxis = 0.7)
}

# applying the function over all files
lapply(list.files(), SpectroMaker)

# problem: files that are shorter than the tlim parameter set are bypassed
# need to know length of all recordings and get minimum. tlim parameter needs to be < the shortest recording

allwaves<-sapply(list.files(), readWave)

alltimes<-as.data.frame(sapply(allwaves, duration))

min(alltimes) #1.259125

# exporting plots from the temporary directory. It seems that only 100 plots can be stored here--not ideal, but I'm pretty sure there are < 100 WTSP recordings. We are gonna want to change some of the parameters in spectro() for the final project
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/Shelby Palmer/Desktop/CHICKADEES/winter 2022 song analyses/spectrogram practice")


# okay, now all of the spectrograms are made...
## problem 1: part of the signal is cut off in some 
###is there a way to standardize time across wav files?
## problem 2: the function also puts a bunch of shit in the console that we don't need, but that's not really a big deal

# is tlim() a required argument in spectro()?
