library(seewave)
library(tuneR)
# seeing if our use/trim cases work (20 Dec. 2022)
#setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow")
setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow")
# adjust<-read.csv("WTSP_spectrogram_usability_adjusted.csv")
# View(adjust)
# colnames(adjust)[2:4]<-c("threshold_30", 
#                          "threshold_25", 
#                          "salvageable_30")
# adjust$new_threshold[35]<-45
# adjust$new_threshold<-as.numeric(adjust$new_threshold)

adjust<-read.csv("WTSP_params_16feb23.csv")

#### USE (change threshold) ####
# adjusting the thresholds and looking at amplitude envelopes
#setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow/terminal strophe recordings")
setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/terminal strophe recordings")
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
    png(filename = paste("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/try/", adjust$file.name[i], ".png", sep = ""))
    try(timer(b,
              dmin = 0.02,
              envt = "hil",
              msmooth=c(512, 90),
              threshold = adjust$new_threshold[i],
              main=adjust$file.name[i]))
    dev.off()
  }
  
}

# 2 Feb 2023: figuring out which files were passed over by try()
use_works <- as.character(c())
setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/try/")
for(i in 1:length(list.files())){
  use_works <- append(use_works, unlist(strsplit(list.files()[i], split = ".png")))  
}

failures <- c()
for (i in 1:length(adjust$file.name[which(adjust$threshold_25 == "use")])){
  failures[i] <- !adjust$file.name[which(adjust$threshold_25 == "use")][i] %in% use_works
}

use_candidates <- adjust$file.name[which(adjust$threshold_25 == "use")]
losers <- use_candidates[which(failures == TRUE)]


# back to 20 dec 23
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
          threshold=25,
          main=adjust$file.name[i])
  }
}

# use/trim
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
  if(adjust$threshold_25[i]=="use/trim"){
    b1<-cutw(b,
             from=adjust$trim_before[i],
             to=adjust$trim_after[i],
             output="Wave")
    timer(b1,
          dmin = 0.02,
          envt = "hil",
          msmooth=c(512, 90),
          threshold=as.numeric(adjust$new_threshold[i]),
          main=adjust$file.name[i])
  }
}

#### YES, USE, and TRIM ####
remix<-adjust[-c(which(adjust$threshold_25=="no")),]
View(remix)

# making amp env figures with everything usable--to review together
for (i in 1:length(remix$file.name)) {
  a<-readWave(remix$file.name[i])
  if (a@samp.rate!=48000) {
    a<-resamp(a,
              g=48000,
              output="Wave")
  }
  a1<-fir(a,
          from=2000,
          to=6000,
          bandpass=T,
          output="Wave") 
  b<-fir(a1,
         from=(mean(dfreq(a1, plot=F)[,2])*1000)-500,
         to=(mean(dfreq(a1, plot=F)[,2])*1000)+500,
         bandpass=T,
         output="Wave")
  if(remix$threshold_25[i]=="yes") {
    timer(b,
          dmin = 0.02,
          envt = "hil",
          msmooth=c(512, 90),
          threshold = 25,
          main=usables[i])
  }
  if(remix$threshold_25[i]=="use"){
    try(timer(b,
              dmin = 0.02,
              envt = "hil",
              msmooth=c(512, 90),
              threshold = as.numeric(remix$new_threshold[i]),
              main=remix$file.name[i]))
  }
  if(remix$threshold_25[i]=="trim"|remix$threshold_25[i]=="Trim"){
    b1<-cutw(b,
             from=remix$trim_before[i],
             to=remix$trim_after[i],
             output="Wave")
    timer(b1,
          dmin = 0.02,
          envt = "hil",
          msmooth=c(512, 90),
          threshold=30,
          main=remix$file.name[i])
  }
  if(remix$threshold_25[i]=="use/trim"){
    b1<-cutw(b,
             from=remix$trim_before[i],
             to=remix$trim_after[i],
             output="Wave")
    timer(b1,
          dmin = 0.02,
          envt = "hil",
          msmooth=c(512, 90),
          threshold=as.numeric(remix$new_threshold[i]),
          main=remix$file.name[i])
  }
}

write.csv(adjust, "C:/Users/Shelby Palmer/Desktop/The House Always Wins/White-Throated-Sparrow/WTSP_params_20dec22.csv")
#=======
setwd("/Users/Shared/WTSP/")
m_dec14<-read.csv("WTSP_spectrogram_usability_adjusted.csv")
table(m_dec14$X25_resamp)
m_dec14[which(m_dec14$X25_resamp=="trim?"),]

################## writing new wav files for "trim"/"use/trim" #########################
library(seewave)
library(tuneR)
setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/terminal strophe recordings")
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
if(adjust$threshold_25[i] %in% c("trim", "use/trim")){
  b1<-normalize(cutw(b,
                      from=adjust$trim_before[i],
                      to=adjust$trim_after[i],
                      output="Wave"),
                unit="16")
  writeWave(b1,
            filename=adjust$file.name[i])
  }
}

