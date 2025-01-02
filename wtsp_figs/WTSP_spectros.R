setwd("/Users/jaymcentee/Documents/GitHub/White-throated-sparrow/wtsp_figs")

library(seewave)
library(tuneR)

#### triplet ####
a<-readWave("ML98816_cretic_1point5s.wav")
# new wave object with bandpass filter applied:
a0<-fir(a,
        from=2000,
        to=5000,
        bandpass=T,
        output="Wave") # initial filter
a1<-fir(a0,
       from=(mean(dfreq(a0, plot=F)[,2])*1000)-500,
       to=(mean(dfreq(a0, plot=F)[,2])*1000)+500,
       bandpass=T,
       output="Wave")
# b1<-fir(a,
#        from = 2000,
#        to = 5000,
#        bandpass = TRUE,
#        output="Wave")

#### doublet ####
d<-readWave("ML146267_trochee_1point5s.wav")
e0<-fir(d,
        from=2000,
        to=5000,
        bandpass=T,
        output="Wave") # initial filter
e1<-fir(e0,
        from=(mean(dfreq(e0, plot=F)[,2])*1000)-500,
        to=(mean(dfreq(e0, plot=F)[,2])*1000)+500,
        bandpass=T,
        output="Wave")
# e1<-fir(d, 
#         from = 1000, 
#         to = 4000, 
#         bandpass = TRUE,
#         output="Wave")

#### dactyl ####
#setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/terminal_strophe_recordings_16bit")
g<-readWave("XC190052_dactyl_1point5s.wav")
h0<-fir(g,
        from=2000,
        to=5000,
        bandpass=T,
        output="Wave") # initial filter
h1<-fir(h0,
        from=(mean(dfreq(h0, plot=F)[,2])*1000)-500,
        to=(mean(dfreq(h0, plot=F)[,2])*1000)+500,
        bandpass=T,
        output="Wave")
# h1<-fir(g, 
#         from = 3500, 
#         to = 5000, 
#         bandpass = TRUE,
#         output="Wave")

#setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures")
# spectrograms 

png(filename = "/Users/jaymcentee/Documents/GitHub/White-throated-sparrow/wtsp_figs/WTSP_spectro_manuscript.png", width = 27, height = 9, res = 300, units = "in")
par(mfrow = c(1,3),
    oma = c(2,1.5,1.5,0), 
    mar = c(4,3,2,2),
    mgp = c(3,2,0))
c1<-spectro(a1, 
            wl = 512,
            ovlp = 95,
            collevels = c(-30,-14,-12,-10,-8,-6,-4,-2,0), 
            flim = c(0, 5),
            osc = F, 
            scale = F, 
            grid = F, 
            cexlab = 1, 
            cexaxis = 3,
            palette = reverse.heat.colors,
            tlab = NULL,
            flab = NULL,
            norm = TRUE,
            bty = "l")
f1<-spectro(e1, 
            wl = 512, 
            ovlp = 95,
            collevels = c(-30,-14,-12,-10,-8,-6,-4,-2,0),
            flim = c(0, 5),
            osc = F, 
            scale = F, 
            grid = F, 
            cexlab = 1, 
            cexaxis = 3,
            palette = reverse.heat.colors,
            tlab = NULL,
            flab = NULL,
            norm = TRUE,
            bty = "l",
            yaxt = "n")
i1<-spectro(h1, 
            wl = 512, 
            ovlp = 95,
            collevels = c(-30,-14,-12,-10,-8,-6,-4,-2,0),
            flim = c(0, 5),
            osc = F, 
            scale = F, 
            grid = F, 
            cexlab = 1, 
            cexaxis = 3,
            palette = reverse.heat.colors,
            tlab = NULL,
            flab = NULL,
            norm = TRUE,
            bty = "l")
# par(las = 0)
# mtext(text = "Frequency (kHz)", side = 2, outer = TRUE, line = 0.3, padj = 1, cex = 1)
# mtext(text = "Time (s)", side = 1, outer = TRUE, line = 0, padj = 1, cex = 1)
dev.off()


png(filename = "/Users/jaymcentee/Documents/GitHub/White-throated-sparrow/wtsp_figs/WTSP_amp_timerless.png", width = 27, height = 9, res = 300, units = "in")
par(mfrow = c(1,3),
    oma = c(2,1.5,1.5,0), 
    mar = c(4,3,2,2),
    mgp = c(3,2,0))
timer_a1 <- timer(a1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 99.9,
                  plotthreshold	= FALSE,
                  colval = "white",
                  cex.axis = 2.5,
                  cex = 0.1,
                  xlab = "",
                  bty = "l",
                  xaxp = c(0, 1.5, 3))
timer_e1 <- timer(e1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 99.9,
                  plotthreshold	= FALSE,
                  colval = "white",
                  cex.axis = 2.5,
                  cex = 0.1,
                  xlab = "",
                  bty = "l",
                  xaxp = c(0, 1.5, 3))
timer_h1 <- timer(h1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 99.9,
                  plotthreshold	= FALSE,
                  colval = "white",
                  cex.axis = 2.5,
                  cex = 0.1,
                  xlab = "",
                  bty = "l",
                  xaxp = c(0, 1.5, 3))
dev.off()

##Edit timer plotting so that timer graphs do not show labels on durations, and that the threshold label is larger
trace(timer, edit = TRUE)
##Comment out lines 118-121?
##Change cex of threshold label to 1.5 in line 98

png(filename = "/Users/jaymcentee/Documents/GitHub/White-throated-sparrow/wtsp_figs/trochee_scoring_figure.png", width = 18, height = 9, res = 300, units = "in")
par(mfrow = c(1,2), 
    mar = c(3,3,2,2),
    bty = "l")
timer_e1 <- timer(e1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 35,
                  cex.axis = 1.5)
timer_a1 <- timer(a1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 35,
                  cex.axis = 1.5)
dev.off()

png(filename = "/Users/jaymcentee/Documents/GitHub/White-throated-sparrow/wtsp_figs/mid_long_scoring_figure.png", width = 18, height = 9, res = 300, units = "in")
par(mfrow = c(1,2), 
    mar = c(3,3,2,2),
    bty = "l")
timer_b1 <- timer(b1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 35,
                  cex.axis = 1.5)
timer_h1 <- timer(h1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 35,
                  cex.axis = 1.5)
dev.off()
