#### Making a bunch of spectrograms at once using seewave ####

# set working directory to folder with all WTSP recordings. For practice, I'm using my chickadee data
setwd("C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Remy (Rm-RR_HZCH_SPPUA)/chopped")

library(seewave)
library(tuneR)

# h 

filt_resamp <- function(x) {
setwd("/Users/zapata/Documents/WT sparrow")
wav <- readWave(x)
wav_fir <- fir(wav, from = 2000, to=6000, output="Wave")
wav_fir_rs <- resamp(wav_fir, g=22050, output="Wave")
wav_final <- normalize(wav_fir_rs, unit=c("16"))
setwd("/Users/zapata/Documents/WT sparrow")
writeWave(wav_final, filename=paste("Modified", x, sep="_"), extensible=F)
}

lapply(list.files(), filt_resamp)

# make a function that will read in a file and make a spectrogram that can be used in lapply later
SpectroMaker<-function(x) {
  a<-readWave(x)
  spectro(a, wl = 512, ovlp = 95, collevels = seq(-42,0,6),
          flim = c(0, 7),
          osc = F, scale = F)
  par(new = T)
  timer(a, threshold=3.6, envt="hil", msmooth=c(120, 98), plot=T)
  dev.off()

}

SpectroMaker<-function(x) {
  a<-readWave(x)
  spectro(a, wl = 512, ovlp = 95, collevels = seq(-42,0,6),
          flim = c(0, 7),
          osc = F, scale = F)
  par(new = T)
  timer(a, threshold=3.6, envt="hil", msmooth=c(120, 98), plot=T)
  plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
  plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
  file.copy(from=plots.png.paths, to="~/Documents/WT sparrow/wts/wts5")
}


# applying the function over all files
lapply(list.files(), SpectroMaker)

# exporting plots from the temporary directory. It seems that only 100 plots can be stored here--not ideal, but I'm pretty sure there are < 100 WTSP recordings. We are gonna want to change some of the parameters in spectro() for the final project
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="~/Documents/WT sparrow/wts/wts5")


# okay, now all of the spectrograms are made...
## problem 1: part of the signal is cut off in some 
###is there a way to standardize time across wav files?
## problem 2: the function also puts a bunch of shit in the console that we don't need, but that's not really a big deal

# is tlim() a required argument in spectro()?


a<-readWave(x)
spectro(x, wl = 512, ovlp = 95, collevels = seq(-42,0,6), 
        tlim = c(0, 1.259),
        flim = c(0, 10),
        osc = F, scale = F, colgrid = "gray", cexlab = 0.8, cexaxis = 0.7)
par(new = T)
timer(x, threshold=4.6, envt="hil", msmooth=c(120, 98), plot=T)
