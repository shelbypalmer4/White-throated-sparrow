#### Step 1: visual quality-check of timer() output using spectrograms ####

#setwd("C:/Users/Shelby Palmer/Desktop/WTSP")
setwd("/Users/Shared/WTSP/")

#source("timer_hack.R")

library(seewave)
library(tuneR)

# First, we need to remove the recordings that obviously have undetectable signal periods

# make a new folder in the working directory named "figures"

# onespec<-function(x) {
#   a<-readWave(x)
#   # if sampling rate is not 48000, resample to 48000
#   if (a@samp.rate!=48000) {
#     resamp(a,
#            g=48000,
#            output="Wave")
#   }
#   b<-fir(a,
#          from=2000,
#          to=6000,
#          bandpass=T,
#          output="Wave")
#   png(filename = paste("figures/", x, ".png", sep = ""))
#   c<-spectro(b,
#              wl = 512,
#              ovlp = 95,
#              collevels = seq(-42,0,6),
#              #flim = c(0, 10),
#              osc = F,
#              scale = F,
#              colgrid = "gray",
#              cexlab = 0.8,
#              cexaxis = 0.7)
#   dev.off()
# }
# 
# lapply(list.files(pattern = ".wav"), onespec)




##########

# write a function that resamples all files to 48000 Hz, bandpass filters each file, and makes a spectrogram with timer() intervals overlaid. lapply applies the function over all wav files in the working directory

# make a new folder in the working directory named "figures2"
timespec<-function(x) {
  a<-readWave(x)
  # if sampling rate is not 48000, resample to 48000
  if (a@samp.rate!=48000) {
    a<-resamp(a,
              g=48000,
              output="Wave")
  }
  a1<-fir(a,
          from=2000,
          to=6000,
          bandpass=T,
          output="Wave") # initial filter
  b<-fir(a1,
         from=(mean(dfreq(a1, plot=F)[,2])*1000)-500,
         to=(mean(dfreq(a1, plot=F)[,2])*1000)+500,
         bandpass=T,
         output="Wave") # customized filter
  png(filename = paste("/Users/Shared/WTSP/resamp_25_specs/", x, ".png", sep = ""))
  c<-spectro(b,
             wl = 512,
             ovlp = 95,
             collevels = seq(-42,0,6),
             flim = c(0, 7),
             osc = F,
             scale = F,
             colgrid = "gray",
             cexlab = 0.8,
             cexaxis = 0.7)
  par(new = T)
  try(expr=timer(b,
                 dmin = 0.02,
                 envt = "hil",
                 msmooth=c(512, 90),
                 threshold = 25), 
      silent=F)
  dev.off()
}
lapply(list.files(pattern = ".wav"), timespec)


#
#
#
#
### messing around with just 1 recording
hm <- readWave("WTSP13.wav")
#hm <- readWave("ML34904071_terminal_strophes.wav")
#hm <- readWave("XC147689_terminal_strophes.wav")
hm2<-fir(hm,
         from=(mean(dfreq(hm, plot=F)[,2])*1000)-500,
         to=(mean(dfreq(hm, plot=F)[,2])*1000)+500,
         bandpass=T,
         output="Wave")
j <- spectro(hm2,
             wl = 512,
             ovlp = 95,
             collevels = seq(-42,0,6),
             #flim = c(0, 10),
             osc = F,
             scale = F,
             colgrid = "gray",
             cexlab = 0.8,
             cexaxis = 0.7,
             flim = c(0,7))
par(new=T)
k <- timer(hm2,
           #dmin = 0.02,
           envt = "hil",
           msmooth=c(512, 90),
           threshold = 10) # can add plot = FALSE when done checking

l <- cutw(wave = hm2, from = k$s.start[1], to = k$s.end[1], output = "Wave")
wave1 <- env(wave = l, msmooth = c(1024,90), envt = "hil", norm = TRUE, 
             plot = TRUE)

localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(Inf, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
times_at_minima <- localMinima(wave1)
amps_at_minima <- c(wave1[times_at_minima[2:(length(times_at_minima)-1)]])
min(amps_at_minima)

#### looping over a working directory to find local minima ####

# writing new Waves of the first note only
setwd("/Users/Shared/WTSP")
specs<-read.csv("WTSP spectrogram usability.csv") # Caleb's scoring sheet
usables <- specs$file.name[which(specs$good. == "yes")]
#dir.create("firstnote")
setwd("/Users/Shared/WTSP/recordings")

for (i in 1:length(usables)) {
  a<-readWave(usables[i])
  if (a@samp.rate!=48000) {
    resamp(a,
           g=48000,
           output="Wave")
  }
  a1<-fir(a,
          from=2000,
          to=6000,
          bandpass=T,
          output="Wave") # initial filter
  a2<-fir(a1,
          from=(mean(dfreq(a1, plot=F)[,2])*1000)-500,
          to=(mean(dfreq(a1, plot=F)[,2])*1000)+500,
          bandpass=T,
          output="Wave")
  a3<-normalize(a2,
                unit=c("16"))
  b<-timer(a3,
           envt="hil",
           msmooth=c(512,90),
           threshold=25)
  d<-cutw(a3,
          from=b$s.start[1],
          to=b$s.end[1],
          output="Wave")
  e <- paste(usables[i], "firstnote.wav", sep="_") 
  writeWave(d, filename=paste("/Users/Shared/WTSP/firstnote/", e, sep=""))
}

###################
setwd("/Users/Shared/WTSP/firstnote/")

ampmins<-as.numeric(rep(NA, length=length(list.files())))
names(ampmins) <- list.files()

for (i in 1:length(list.files())) {
  hm2 <- readWave(list.files()[i])
  png(filename = paste("/Users/Shared/WTSP/firstnote_env/", list.files()[i], ".png", sep = ""))
  wave1 <- env(wave = hm2, msmooth = c(1024,90), envt = "hil", norm = TRUE, 
               plot = TRUE)
  dev.off()
  times_at_minima <- localMinima(wave1)
  amps_at_minima <- c(wave1[times_at_minima[2:(length(times_at_minima)-1)]])
  ampmins[i] <- min(amps_at_minima)
}

wave1 <- env(wave = hm2, msmooth = c(1024,90), envt = "hil", norm = TRUE, 
             plot = TRUE)
hm2

first<-readWave("ML100883_terminal_strophes.wav_firstnote.wav")
first
