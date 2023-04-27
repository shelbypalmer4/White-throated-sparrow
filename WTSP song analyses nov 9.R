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
# timespec<-function(x) {
#   a<-readWave(x)
#   # if sampling rate is not 48000, resample to 48000
#   if (a@samp.rate!=48000) {
#     a<-resamp(a,
#               g=48000,
#               output="Wave")
#   }
#   a1<-fir(a,
#           from=2000,
#           to=6000,
#           bandpass=T,
#           output="Wave") # initial filter
#   b<-fir(a1,
#          from=(mean(dfreq(a1, plot=F)[,2])*1000)-500,
#          to=(mean(dfreq(a1, plot=F)[,2])*1000)+500,
#          bandpass=T,
#          output="Wave") # customized filter
#   png(filename = paste("/Users/Shared/WTSP/resamp_25_specs/", x, ".png", sep = ""))
#   c<-spectro(b,
#              wl = 512,
#              ovlp = 95,
#              collevels = seq(-42,0,6),
#              flim = c(0, 7),
#              osc = F,
#              scale = F,
#              colgrid = "gray",
#              cexlab = 0.8,
#              cexaxis = 0.7)
#   par(new = T)
#   try(expr=timer(b,
#                  dmin = 0.02,
#                  envt = "hil",
#                  msmooth=c(512, 90),
#                  threshold = 25), 
#       silent=F)
#   dev.off()
# }
# lapply(list.files(pattern = ".wav"), timespec)


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

# l <- cutw(wave = hm2, from = k$s.start[1], to = k$s.end[1], output = "Wave")
# wave1 <- env(wave = l, msmooth = c(1024,90), envt = "hil", norm = TRUE, 
#              plot = TRUE)

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
#specs<-read.csv("WTSP_spectrogram_usability_25.csv") # Caleb's scoring sheet
#usables <- specs$file.name[which(specs$X25_resamp == "yes")]
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



############## Scoring rhythms #################
adjust<-read.csv("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/WTSP_params_16feb23.csv")
adjust$new_threshold[which(is.na(adjust$new_threshold))] <- 25

adjust <- adjust[which(adjust$threshold_25!="no"),]

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
             threshold = as.numeric(adjust$new_threshold[i]),
             plot=F)
  note_starts[[i]] <- k$s.start
  note_durations[[i]] <- k$s
  if(k$first == "signal"){
    print(paste("uh oh", adjust$file.name[i]))
  }
}

note_number <- rep(NA, length.out = length(note_starts))
for(i in 1:length(note_number)){
  note_number[i] <- length(note_starts[[i]])
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

three_set_note_durations_1 <- list()
three_set_note_durations_2 <- list()
three_set_note_durations_3 <- list()
for(i in 1:length(note_durations)){
  first_positions <- seq(from = 1, to = length(note_durations[[i]]), by = 3)
  second_positions <- seq(from = 2, to = length(note_durations[[i]]), by = 3)
  third_positions <- seq(from = 3, to = length(note_durations[[i]]), by = 3)
  three_set_note_durations_1[[i]] <- note_durations[[i]][first_positions]
  three_set_note_durations_2[[i]] <- note_durations[[i]][second_positions]
  three_set_note_durations_3[[i]] <- note_durations[[i]][third_positions]
}


##max_mean_dur and min_mean_dur are labeled dur but they are onset interval durations, not note durations
max_mean_dur <- rep(NA, length.out = length(odd_intervals))
min_mean_dur <- rep(NA, length.out = length(odd_intervals))
long_note_durs <-  rep(NA, length.out = length(odd_intervals))
med_note_durs <-  rep(NA, length.out = length(odd_intervals))
short_note_durs <- rep(NA, length.out = length(odd_intervals))
##To add in: maximum note duration, maximum onset interval duration
max_note_dur <- rep(NA, length.out = length(odd_intervals))
max_onset_interval_dur <- rep(NA, length.out = length(odd_intervals))

for(i in 1:length(max_mean_dur)){
  max_mean_dur[i] <- max(mean(odd_intervals[[i]]), mean(even_intervals[[i]]))
  min_mean_dur[i] <- min(mean(odd_intervals[[i]]), mean(even_intervals[[i]]))
  long_note_durs[i] <-  max(mean(three_set_note_durations_1[[i]]), mean(three_set_note_durations_2[[i]]), mean(three_set_note_durations_3[[i]]))
  med_note_durs[i] <- sort(c(mean(three_set_note_durations_1[[i]]), mean(three_set_note_durations_2[[i]]), mean(three_set_note_durations_3[[i]])))[2]
  short_note_durs[i] <-  min(mean(three_set_note_durations_1[[i]]), mean(three_set_note_durations_2[[i]]), mean(three_set_note_durations_3[[i]]))
  max_note_dur[i] <- max(note_durations[[i]])
  max_onset_interval_dur[i] <- max(max(odd_intervals[[i]], max(even_intervals[[i]])))
}

durs <- data.frame(max_mean_dur, min_mean_dur, long_note_durs, med_note_durs, short_note_durs, max_note_dur, max_onset_interval_dur)

library(ggplot2)
library(cowplot)
png(filename = "min duration by max duration.png", )
ggplot(durs, aes(x=max_mean_dur, y=min_mean_dur)) + 
  geom_point(size = 2) +
  theme_cowplot() +
  xlab("Greater onset interval mean") +
  ylab("Lesser onset interval mean")
dev.off()
# dist_point_line <- function(a, slope, intercept) {
#   b = c(1, intercept+slope)
#   c = c(-intercept/slope,0)       
#   v1 <- b - c
#   v2 <- a - b
#   m <- cbind(v1,v2)
#   return(abs(det(m))/sqrt(sum(v1*v1)))
# }
# 
# dists_from_line <- as.numeric(c())
# for (i in 1:length(max_mean_dur)){
#   pt <- c(max_mean_dur[i], min_mean_dur[i])
#   dists_from_line <- append(dists_from_line, dist_point_line(pt, 1, 0))
# }
# hist(dists_from_line, breaks = 15)


max_min_ratio <- max_mean_dur/min_mean_dur
log_max_min_ratio <- log(max_min_ratio)
min_max_ratio <- min_mean_dur/max_mean_dur

mid_to_long_ratio <- med_note_durs/long_note_durs
plot(min_max_ratio, mid_to_long_ratio)

durs <- data.frame(durs, min_max_ratio, mid_to_long_ratio)



trochee_scores <- data.frame(adjust$file.name, max_min_ratio, log_max_min_ratio, min_max_ratio, note_number, mid_to_long_ratio, max_mean_dur, min_mean_dur, med_note_durs, long_note_durs, short_note_durs, max_note_dur, max_onset_interval_dur)
colnames(trochee_scores) <- c("file.name", "max_min_ratio", "log_max_min_ratio", "min_max_ratio", "note_number", "mid_to_long_ratio", "max_mean_dur", "min_mean_dur", "med_note_durs", "long_note_durs", "short_note_durs", "max_note_dur", "max_onset_interval_dur")

write.csv(trochee_scores, "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/trochee_scores.csv")

otters <- read.csv("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/Otter_et_al_list_of_all_recordings.csv")
colnames(otters)[2] <- "recording"

trochee_scores$recording.name <- rep(NA)
for (i in 1:length(trochee_scores$recording.name)){ 
  trochee_scores$recording.name[i] <- unlist(strsplit(trochee_scores$file.name[i], split = "_"))[1]
}
for (i in 1:length(trochee_scores$recording.name)){ 
  trochee_scores$recording.name[i] <- unlist(strsplit(trochee_scores$recording.name[i], split = "[.]"))[1]
}

lab_scores <- read.csv("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/lab_rhythm_scores.csv")

for (i in 1:length(trochee_scores$recording.name)){
  if(trochee_scores$recording.name[i] %in% lab_scores$Name_for_scoring){
    trochee_scores$recording.name[i] <- lab_scores$Recording.ID..if.noted.[which(lab_scores$Name_for_scoring == trochee_scores$recording.name[i])]
  }
}
##Need to check for duplicate names for birds with both score types
## ML152956471, ML39355611, ML31612451 were all scored both doublet and triplet by Otter et al, and are represented 
## by multiple scores in our data set. Taylor deliberately pulled two songs from each recording, one that seemed
## doublety and one triplety

the_truth <- merge(otters, trochee_scores, by.x = "recording", by.y = "recording.name", all.x = FALSE)
##Remove problematic duplicates
the_truth <- the_truth[-which(the_truth$file.name == "WTSP82.wav" & the_truth$Terminal.Strophe.type == "Doublet"),]
the_truth <- the_truth[-which(the_truth$file.name == "WTSP75.wav" & the_truth$Terminal.Strophe.type == "Triplet"),]
the_truth <- the_truth[-which(the_truth$file.name == "ML31612451_terminal_strophes_triplet.wav" & the_truth$Terminal.Strophe.type == "Doublet"),]
the_truth <- the_truth[-which(the_truth$file.name == "WTSP59.wav" & the_truth$Terminal.Strophe.type == "Doublet"),]
the_truth <- the_truth[-which(the_truth$file.name == "ML39355611_terminal_strophes_doublet.wav" & the_truth$Terminal.Strophe.type == "Triplet"),]
the_truth <- the_truth[-which(the_truth$file.name == "ML94149261_terminal_strophes.wav" & the_truth$Introductory.Notes..if.noted. == "Ascending"),]
the_truth <- the_truth[-which(the_truth$file.name == "WTSP66.wav" & the_truth$Introductory.Notes..if.noted. == "Ascending"),]
the_truth <- the_truth[-which(the_truth$file.name == "WTSP71.wav" & the_truth$Introductory.Notes..if.noted. == "Ascending"),]
fucking_duplicated_rows <- the_truth[which(the_truth$recording =="ML105954241"),]
the_truth <- the_truth[-which(the_truth$recording =="ML105954241"),]
the_truth <- rbind(the_truth, fucking_duplicated_rows[1,])
the_truth <- the_truth[-which(the_truth$file.name == "ML150818621_terminal_strophes.wav" & the_truth$Introductory.Notes..if.noted. == "Ascending"),]
the_truth <- the_truth[-which(the_truth$file.name == "ML154043581_terminal_strophes.wav" & the_truth$Introductory.Notes..if.noted. == "Ascending"),]
the_truth <- the_truth[-which(the_truth$file.name == "ML169021_terminal_strophes.wav" & the_truth$Longitude == -73.88525),]

the_truth$remainder <- the_truth$note_number %% 3


hist(the_truth$log_max_min_ratio, breaks = 15)
hist(the_truth$max_min_ratio, breaks = 15)

# remove what turned out to be outliers in rhythm PCAs
the_truth <- the_truth[-which(the_truth$file.name %in% c("XC141294_terminal_strophes.wav", "XC33226_terminal_strophes.wav")),]

pca_measures <- data.frame(scale(the_truth$max_note_dur), scale(the_truth$max_onset_interval_dur), scale(the_truth$min_max_ratio), scale(the_truth$mid_to_long_ratio))
rhythm_pca <- prcomp(pca_measures)

the_truth$PC1 <- rhythm_pca$x[,1]
the_truth$PC2 <- rhythm_pca$x[,2]
the_truth$PC3 <- rhythm_pca$x[,3]
the_truth$PC4 <- rhythm_pca$x[,4]

####Testing hypotheses for score discrepancies

library(ggplot2)
library(cowplot)
ggplot(the_truth, aes(x=Terminal.Strophe.type, y=max_min_ratio)) + 
  geom_jitter(position=position_jitter(0.1), aes(color = Longitude)) +
  theme_cowplot()

ggplot(the_truth, aes(x=Terminal.Strophe.type, y=max_min_ratio)) + 
  geom_jitter(position=position_jitter(0.1), aes(color = Year)) +
  theme_cowplot()

ggplot(the_truth, aes(x=Terminal.Strophe.type, y=max_min_ratio)) + 
  geom_jitter(position=position_jitter(0.1), aes(color = remainder)) +
  theme_cowplot()

ggplot(the_truth, aes(x=Terminal.Strophe.type, y=max_min_ratio)) + 
  geom_jitter(position=position_jitter(0.1), aes(color = Longitude)) +
  theme_cowplot()

ggplot(the_truth,  aes(x=Terminal.Strophe.type, y=min_max_ratio)) +
  geom_jitter(position=position_jitter(0.1), aes(color = mid_to_long_ratio)) +
  theme_cowplot()

png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/the_truth_and_nothing_but_the_truth.png", width = 7, height = 7, units = "in", res = 300)
ggplot(the_truth, aes(x=Terminal.Strophe.type, y=max_min_ratio)) + 
  geom_jitter(position=position_jitter(0.1)) +
  xlab("Published observer score") +
  ylab("Onset interval ratio") +
  theme_cowplot() 
dev.off()

###PC plots
ggplot(the_truth, aes(x=PC1, y=PC2, color = Terminal.Strophe.type)) + 
  geom_point() +
  theme_cowplot() 

ggplot(the_truth, aes(x=PC1, y=PC3, color = Terminal.Strophe.type)) + 
  geom_point() +
  theme_cowplot() 

ggplot(the_truth, aes(x=PC2, y=PC3, color = Terminal.Strophe.type)) + 
  geom_point() +
  theme_cowplot() 

ggbiplot(rhythm_pca,
         groups = the_truth$Terminal.Strophe.type,
         varname.size = 3,
         varname.adjust = 1)

ggbiplot(rhythm_pca,
         groups = the_truth$Terminal.Strophe.type,
         choices = 2:3,
         varname.size = 3,
         varname.adjust = 1)

ggbiplot(rhythm_pca,
         groups = the_truth$Terminal.Strophe.type,
         choices = c(1,3),
         varname.size = 3,
         varname.adjust = 1)

the_truth[which(the_truth$PC2< -2.9),]

##Check very low scoring doublets
low_doublets <- the_truth[which(the_truth$Terminal.Strophe.type == "Doublet" & the_truth$max_min_ratio < 1.2),]
write.csv(low_doublets, file = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/doublet_problems.csv")

# 20 April 2023: PCA with only triplety-scoring recordings to check for clustering
# which(durs$min_max_ratio>0.70)
# durs_triplety <- durs[which(durs$min_max_ratio>0.70),]
# 
# pca_measures_triplety <- data.frame(scale(durs_triplety$max_note_dur), scale(durs_triplety$max_onset_interval_dur), scale(durs_triplety$min_max_ratio), scale(durs_triplety$mid_to_long_ratio))
# rhythm_pca_triplety <- prcomp(pca_measures_triplety)

the_truth_triplety <- the_truth[which(the_truth$min_max_ratio>0.70),]

pca_measures_triplety <- data.frame(scale(the_truth_triplety$max_note_dur), scale(the_truth_triplety$max_onset_interval_dur), scale(the_truth_triplety$min_max_ratio), scale(the_truth_triplety$mid_to_long_ratio))
rhythm_pca_triplety <- prcomp(pca_measures_triplety)

pca_scores_triplety <- cbind(rhythm_pca_triplety$x[,1],
                            rhythm_pca_triplety$x[,2],
                            rhythm_pca_triplety$x[,3],
                            rhythm_pca_triplety$x[,4])
colnames(pca_scores_triplety) <- c("PC1_trip", "PC2_trip", "PC3_trip", "PC4_trip")

the_truth_triplety <- cbind(the_truth_triplety,
                            pca_scores_triplety)

ggplot(the_truth_triplety, aes(x=PC1_trip, y=PC2_trip, color = Terminal.Strophe.type)) + 
  geom_point() +
  theme_cowplot() 

ggplot(the_truth_triplety, aes(x=PC2_trip, y=PC3_trip, color = Terminal.Strophe.type)) + 
  geom_point() +
  theme_cowplot() 

ggplot(the_truth_triplety, aes(x=PC1_trip, y=PC3_trip, color = Terminal.Strophe.type)) + 
  geom_point() +
  theme_cowplot()

rhythm_pca_triplety

the_truth_triplety[which(the_truth_triplety$PC2_trip< -3),]
hist(the_truth_triplety$mid_to_long_ratio)

library(ggbiplot)
ggbiplot(rhythm_pca_triplety,
         groups = the_truth_triplety$Terminal.Strophe.type,
         varname.size = 3,
         varname.adjust = 1)

ggbiplot(rhythm_pca_triplety,
         choices = 2:3,
         groups = the_truth_triplety$Terminal.Strophe.type,
         varname.size = 3,
         varname.adjust = 1)

ggbiplot(rhythm_pca_triplety,
         choices = c(1,3),
         groups = the_truth_triplety$Terminal.Strophe.type,
         varname.size = 3,
         varname.adjust = 1)

the_truth[which(the_truth$PC2< -1 & the_truth$PC1< -1),]
