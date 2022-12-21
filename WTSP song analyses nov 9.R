#### Step 1: visual quality-check of timer() output using spectrograms ####

#setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow")
setwd("/Users/Shared/WTSP/")

source("timer_hack.R")

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
# setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow/terminal strophe recordings")
# hm <- readWave("WTSP13.wav")
# #hm <- readWave("ML34904071_terminal_strophes.wav")
# #hm <- readWave("XC147689_terminal_strophes.wav")
# hm2<-fir(hm,
#          from=(mean(dfreq(hm, plot=F)[,2])*1000)-500,
#          to=(mean(dfreq(hm, plot=F)[,2])*1000)+500,
#          bandpass=T,
#          output="Wave")
# j <- spectro(hm2,
#              wl = 512,
#              ovlp = 95,
#              collevels = seq(-42,0,6),
#              #flim = c(0, 10),
#              osc = F,
#              scale = F,
#              colgrid = "gray",
#              cexlab = 0.8,
#              cexaxis = 0.7,
#              flim = c(0,7))
# par(new=T)
# k <- timer(hm2,
#            #dmin = 0.02,
#            envt = "hil",
#            msmooth=c(512, 90),
#            threshold = 10) # can add plot = FALSE when done checking

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
# times_at_minima <- localMinima(wave1)
# amps_at_minima <- c(wave1[times_at_minima[2:(length(times_at_minima)-1)]])
# min(amps_at_minima)

#### looping over a working directory to find local minima ####

# writing new Waves of the first note only
setwd("/Users/Shared/WTSP")
#setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow")
specs<-read.csv("WTSP_spectrogram_usability_25.csv") # Caleb's scoring sheet
usables <- specs$file.name[which(specs$X25_resamp == "yes")]
#dir.create("firstnote")
setwd("/Users/Shared/WTSP/recordings")
#setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow/terminal strophe recordings")

# getting first notes only
for (i in 1:length(usables)) {
  a<-readWave(usables[i])
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
  #writeWave(d, filename=paste("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow/first_note", e, sep=""))
}

###################
setwd("/Users/Shared/WTSP/firstnote/")

ampmins<-as.numeric(rep(NA, length=length(list.files())))
names(ampmins) <- list.files()

for (i in 1:length(list.files())) {
  hm2 <- readWave(list.files()[i])
  #png(filename = paste("/Users/Shared/WTSP/firstnote_env/", list.files()[i], ".png", sep = ""))
  wave1 <- env(wave = hm2, msmooth = c(1024,90), envt = "hil", norm = TRUE, 
               plot = FALSE)
  #dev.off()
  times_at_minima <- localMinima(wave1)
  amps_at_minima <- c(wave1[times_at_minima[2:(length(times_at_minima)-1)]])
  ampmins[i] <- min(amps_at_minima)
}
View(ampmins)

setwd("/Users/Shared/WTSP/recordings")

note_starts <- list()
#for (i in 1:1){
for (i in 1:length(usables)) {
  a<-readWave(usables[i])
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
         output="Wave")
  k <- timer(b,
             dmin = 0.02,
             envt = "hil",
             msmooth=c(512, 90),
             threshold = 25,
             main=usables[i])
  note_starts[[i]] <- k$s.start
}

odd_intervals <- list()
for (i in 1:length(note_starts)){
  if(length(note_starts[[i]]) %% 2 == 0){
    odd_notes <- seq(from = 1, to = length(note_starts[[i]]), by = 2)
    diffs <- c()
    for (j in 1:length(odd_notes)){
      diffs <- append(diffs, note_starts[[i]][odd_notes[j]+1]-note_starts[[i]][odd_notes[j]])
    }
    odd_intervals[[i]] <- diffs
  }
  else{
    odd_notes <- seq(from = 1, to = length(note_starts[[i]])-1, by = 2)
    diffs <- c()
    for (j in 1:length(odd_notes)){
      diffs <- append(diffs, note_starts[[i]][odd_notes[j]+1]-note_starts[[i]][odd_notes[j]])
    }
    odd_intervals[[i]] <- diffs
  }
}

even_intervals <- list()
for (i in 1:length(note_starts)){
  if(length(note_starts[[i]]) %% 2 == 0){
    even_notes <- seq(from = 2, to = length(note_starts[[i]])-1, by = 2)
    diffs <- c()
    for (j in 1:length(even_notes)){
      diffs <- append(diffs, note_starts[[i]][even_notes[j]+1]-note_starts[[i]][even_notes[j]])
    }
    even_intervals[[i]] <- diffs
  }
  else{
    even_notes <- seq(from = 2, to = length(note_starts[[i]]), by = 2)
    diffs <- c()
    for (j in 1:length(even_notes)){
      diffs <- append(diffs, note_starts[[i]][even_notes[j]+1]-note_starts[[i]][even_notes[j]])
    }
    even_intervals[[i]] <- diffs
  }
}

max_mean_dur <- rep(NA, length.out = length(odd_intervals))
min_mean_dur <- rep(NA, length.out = length(odd_intervals))

for(i in 1:length(max_mean_dur)){
  max_mean_dur[i] <- max(mean(odd_intervals[[i]]), mean(even_intervals[[i]]))
  min_mean_dur[i] <- min(mean(odd_intervals[[i]]), mean(even_intervals[[i]]))
}

# seeing if our use/trim cases work
setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow")
adjust<-read.csv("WTSP_spectrogram_usability_adjusted.csv")
View(adjust)
colnames(adjust)[2:4]<-c("threshold_30", 
                         "threshold_25", 
                         "salvageable_30")

#### USE ####
# adjusting the thresholds and looking at amplitude envelopes
setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow/terminal strophe recordings")
for (i in 1:length(adjust$file.name)) {
  a<-readWave(adjust$file.name[i])
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
         output="Wave")
  if(adjust$threshold_25[i]=="use"){
    try(timer(b,
          dmin = 0.02,
          envt = "hil",
          msmooth=c(512, 90),
          threshold = as.numeric(adjust$new_threshold[i]),
          main=adjust$file.name[i]))
  }

}


#### TRIM ####
## cutw needs numerical values to cut from...
adjust$trim_before[is.na(adjust$trim_before)]<-0

#...and to
for (i in 1:length(adjust$file.name)) {
  a<-readWave(adjust$file.name[i])
  ifelse(is.na(adjust$trim_after[i]),
         adjust$trim_after[i]<-duration(a),
         NA)
}

# cutting and looking at them
for (i in 1:length(adjust$file.name)) {
  a<-readWave(adjust$file.name[i])
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
         output="Wave")
  if(adjust$threshold_25[i]=="trim"){
    b1<-cutw(b,
         from=adjust$trim_before[i],
         to=adjust$trim_after[i],
         output="Wave")
    timer(b1,
          dmin = 0.02,
          envt = "hil",
          msmooth=c(512, 90),
          threshold=30,
          main=adjust$file.name[i])
  }
}

