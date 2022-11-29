#### White-throated Sparrow song measurements for doublet/triplet classification ####

# set working directory to folder with all WTSP recordings
setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-throated-sparrow/songs")

library(seewave)
library(tuneR)

# filter and re-sample recordings
# filt_resamp <- function(x) {
#   wav <- readWave(x)
#   wav_fir <- fir(wav, 
#                  from = 2000, 
#                  to=6000, 
#                  bandpass = TRUE,
#                  output="Wave")
#   wav_fir_rs <- resamp(wav_fir, 
#                        g=22050, 
#                        output="Wave")
#   wav_final <- normalize(wav_fir_rs, 
#                          unit=c("16"))
#   writeWave(wav_final, 
#             filename=paste("Modified", x, sep="_"), 
#             extensible=F)
# }
# lapply(list.files(), filt_resamp)
# I then moved altered recordings manually to a different folder
# In this new folder, I made an empty folder called "figures" in which to deposit the images made by allspec()


# filter recordings and make spectrograms with timer() intervals overlaid to check for quality of recordings for note-level analyses 
allspec<-function(x) {
  a<-readWave(x)
  fir(a, 
        from = 200, 
        to = 6000, 
        bandpass = TRUE,
        output="Wave")
  normalize(a, 
            unit = c("16"))
  resamp(a,
         g=48000,
         output="Wave")
  png(filename = paste("figures/", x, ".png", sep = ""))
  spectro(a, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 10),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7)
  par(new = T)
  timer(a, 
        # dmin = 0.05,
        envt = "hil",
        msmooth=c(512, 95),
        threshold = 7)
  dev.off()
}
lapply(list.files(pattern = ".wav"), allspec)

# what are all the sampling rates?
waves<-as.array(lapply(list.files(pattern=".wav"), readWave))

SR<-data.frame(list.files(pattern=".wav"))
SR$sampling_rates<-rep(NA, length(SR[,1]))
SR$bit<-rep(NA, length(SR[,1]))
for (i in 1:length(waves)) {
  SR$sampling_rates[i]<-waves[[i]]@samp.rate
  SR$bit[i]<-waves[[i]]@bit
}
unique(SR$sampling_rates)
# [1]  96000  44100  48000 192000

which(SR$sampling_rates==96000)
which(SR$sampling_rates==192000)
which(SR$sampling_rates==44100)
which(SR$sampling_rates==48000)
# need to resample everything to 48000 Hz
which(SR$sampling_rates!=48000)
SR2<-subset(SR, SR$sampling_rates!=48000)
SR2
SR3<-lapply(SR2$list.files.pattern.....wav.., readWave)
for (i in 1:length(SR3)) {
  writeWave(SR3[[i]], filename=paste(i, ".wav", sep=""), extensible = F)
}
# move them to a separate folder
# then read  in and resample
SR4<-lapply(list.files(), readWave)
SR4<-as.array(SR4)

resume<-function(x) {
  resamp(x, g=48000, output="Wave")
}
lapply(SR4, resume)


