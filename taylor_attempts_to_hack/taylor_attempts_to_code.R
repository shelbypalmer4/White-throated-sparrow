#triplety_fun <- subset(the_truth, Terminal.Strophe.type == "Triplet")

#otters_folly <- triplety_fun[which(triplety_fun$min_max_ratio<0.7),]




#adjust<-read.csv("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/WTSP_params_16feb23.csv")
#adjust$new_threshold[which(is.na(adjust$new_threshold))] <- 25

#adjust <- adjust[which(adjust$threshold_25!="no"),]

setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/terminal strophe recordings")

library(seewave)
library(tuneR)

hey <- "ML104970781_terminal_strophes.wav"

a<-readWave(hey)
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
png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/taylor_attempts_to_hack/ML104970781_terminal_strophes.png")
k <- timer(b,
           dmin = 0.02,
           envt = "hil",
           msmooth=c(512, 90),
           threshold = 25,
           plot=T)
dev.off()




the_truth_dactyl <- the_truth[which(the_truth$PC1 < -1 & the_truth$PC2 > 0),]

png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/taylor_attempts_to_hack/pc_plot",
    width = 7, height = 7, units = "in", res = 300)
ggbiplot(rhythm_pca,
         groups = the_truth$Terminal.Strophe.type,
         varname.size = 0)
dev.off()


names(rhythm_pca[["center"]])[1] <- "Max Note Duration"
names(rhythm_pca[["center"]])[2] <- "Max Onset Interval Duration"
names(rhythm_pca[["center"]])[3] <- "Min:Max Ratio"
names(rhythm_pca[["center"]])[4] <- "Mid:Long Ratio"

rownames(rhythm_pca[["rotation"]])[1] <- "Max Note Duration"
rownames(rhythm_pca[["rotation"]])[2] <- "Max Onset Interval Duration"
rownames(rhythm_pca[["rotation"]])[3] <- "Min:Max Ratio"
rownames(rhythm_pca[["rotation"]])[4] <- "Mid:Long Ratio"




png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/min_max_ratio_by_Otter_score_plus_mid_long_ratio_color.png", width = 7, height = 7, units = "in", res = 300)
ggplot(data = subset(the_truth, !is.na(Terminal.Strophe.type)), aes(x=Terminal.Strophe.type, y=min_max_ratio, color = mid_to_long_ratio)) + 
  geom_jitter(position=position_jitter(0.1)) +
  xlab("Published observer score") +
  ylab("min:max ratio") +
  theme_cowplot() +
  labs(color='mid:long ratio') 
dev.off()

the_truth_borderline <- the_truth[which(the_truth$min_max_ratio > 0.5 & the_truth$min_max_ratio < 0.75),]










##looking at pre-adjustment thresholds
adjust<-read.csv("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/WTSP_params_16feb23.csv")

adjust <- adjust[which(adjust$threshold_25!="no"),]
#n = 268 (11/8)

setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/terminal strophe recordings/")
note_starts <- list()
note_durations <- list()
#for (i in 1:length(usables)) {
for (i in 1:length(adjust$file.name)) {
  a<-readWave(adjust$file.name[i])
  # if sampling rate is not 48000, resample to 48000
  if (a@samp.rate!=48000) {
    a<-resamp(a,
              g=48000,
              output="Wave")
  }
  a <- cutw(a,
            from = adjust$trim_before[i],
            to = adjust$trim_after[i],
            output = "Wave")
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
png(filename = paste("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/amplitude_prof_preadjust/", adjust$file.name[i], "preadjust.png", sep = ""))
  k <- timer(b,
             dmin = 0.02,
             envt = "hil",
             msmooth=c(512, 90),
             threshold = 25,
             plot=T)
  dev.off()
  note_starts[[i]] <- k$s.start
  note_durations[[i]] <- k$s
  if(k$first == "signal"){
    print(paste("uh oh", adjust$file.name[i]))
  }
}

                                 