#### Step 1: visual quality-check of timer() output using spectrograms ####

setwd("C:/Users/Shelby Palmer/Desktop/WTSP")

library(seewave)
library(tuneR)

# First, we need to remove the recordings that obviously have undetectable signal periods

# make a new folder in the working directory named "figures"

onespec<-function(x) {
  a<-readWave(x)
  # if sampling rate is not 48000, resample to 48000
  if (a@samp.rate!=48000) {
    resamp(a,
           g=48000,
           output="Wave")
  }
  b<-fir(a,
         from=2000,
         to=6000,
         bandpass=T,
         output="Wave")
  png(filename = paste("figures/", x, ".png", sep = ""))
  c<-spectro(b,
             wl = 512,
             ovlp = 95,
             collevels = seq(-42,0,6),
             #flim = c(0, 10),
             osc = F,
             scale = F,
             colgrid = "gray",
             cexlab = 0.8,
             cexaxis = 0.7)
  dev.off()
}

lapply(list.files(pattern = ".wav"), onespec)

# Can we determine an entropy threshold above which recordings are definitely unuseable?

entropyDF<-data.frame(list.files())
entropyDF$entropy<-rep(NA, length(entropyDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  if (a@samp.rate!=48000) {
    resamp(a,
           g=48000,
           output="Wave")
  }
  b<-fir(a,
         from=3000,
         to=4200,
         bandpass=T,
         output="Wave")
  entropyDF$entropy[i]<-H(b)
}

mean(entropyDF$entropy)
# [1] 0.3302272
max(entropyDF$entropy)
# [1] 0.6614621
min(entropyDF$entropy)
# [1] 0.2131076


##########

# write a function that resamples all files to 48000 Hz, bandpass filters each file, and makes a spectrogram with timer() intervals overlaid. lapply applies the function over all wav files in the working directory

# make a new folder in the working directory named "figures2"
timespec<-function(x) {
  a<-readWave(x)
  # if sampling rate is not 48000, resample to 48000
  if (a@samp.rate!=48000) {
    resamp(a,
          g=48000,
          output="Wave")
  }
  a1<-fir(a,
          from=2000,
          to=6000,
          bandpass=T,
          output="Wave")
  b<-fir(a1,
         from=(mean(dfreq(hm2, plot=F)[,2])*1000)-500,
         to=(mean(dfreq(hm2, plot=F)[,2])*1000)+500,
         bandpass=T,
         output="Wave")
  png(filename = paste("figures2/", x, ".png", sep = ""))
  c<-spectro(b,
             wl = 512,
             ovlp = 95,
             collevels = seq(-42,0,6),
             #flim = c(0, 10),
             osc = F,
             scale = F,
             colgrid = "gray",
             cexlab = 0.8,
             cexaxis = 0.7)
  par(new = T)
  d<-timer(b,
           dmin = 0.02,
           envt = "hil",
           msmooth=c(512, 90),
           threshold = 10)
  # if timer returns an error, skip this file
  dev.off()
}

lapply(list.files(pattern = ".wav"), timespec)

# noisy recordings keep fucking up the code

hm<-readWave("XC147689_terminal_strophes.wav")
hm2<-fir(hm,
    from=2000,
    to=6000,
    bandpass=T,
    output="Wave")
spectro(hm2,
        wl = 512,
        ovlp = 95,
        collevels = seq(-42,0,6),
        #flim = c(0, 10),
        osc = F,
        scale = F,
        colgrid = "gray",
        cexlab = 0.8,
        cexaxis = 0.7)
par(new=T)
timer(hm2,
      #dmin = 0.02,
      envt = "hil",
      msmooth=c(512, 90),
      threshold = 10) # can add plot = FALSE when done checking

# average dominant frequeny of the wave in Hz
mean(dfreq(hm, plot=F)[,2])*1000
