setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures")

library(seewave)
library(tuneR)

#### triplet ####
a<-readWave("ML98816_terminal_strophes_padded_resamp.wav")
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
d<-readWave("ML146267_terminal_strophes_resamp_figure.wav")
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
setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/terminal_strophe_recordings_16bit")
g<-readWave("XC190052_terminal_strophes.wav")
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

setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures")
# spectrograms 

png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures/WTSP_spectro_manuscript.png", width = 27, height = 9, res = 300, units = "in")
par(mfrow = c(1,3),
    oma = c(2,1.5,0,0), 
    mar = c(3,3,2,2))
c1<-spectro(a1, 
            wl = 512,
            ovlp = 95,
            collevels = c(-30,-14,-12,-10,-8,-6,-4,-2,0), 
            flim = c(0, 5),
            osc = F, 
            scale = F, 
            grid = F, 
            cexlab = 0.8, 
            cexaxis = 2.5,
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
            cexlab = 0.8, 
            cexaxis = 2.5,
            palette = reverse.heat.colors,
            tlab = NULL,
            flab = NULL,
            norm = TRUE,
            bty = "l")
i1<-spectro(h1, 
            wl = 512, 
            ovlp = 95,
            collevels = c(-30,-14,-12,-10,-8,-6,-4,-2,0),
            flim = c(0, 5),
            osc = F, 
            scale = F, 
            grid = F, 
            cexlab = 0.8, 
            cexaxis = 2.5,
            palette = reverse.heat.colors,
            tlab = NULL,
            flab = NULL,
            norm = TRUE,
            bty = "l")
# par(las = 0)
# mtext(text = "Frequency (kHz)", side = 2, outer = TRUE, line = 0.3, padj = 1, cex = 1)
# mtext(text = "Time (s)", side = 1, outer = TRUE, line = 0, padj = 1, cex = 1)
dev.off()


png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures/WTSP_amp_timerless.png", width = 27, height = 9, res = 300, units = "in")
par(mfrow = c(1,3), 
    mar = c(3,3,2,2),
    bty = "l")
timer_b1 <- timer(b1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 99.9,
                  plotthreshold	= FALSE,
                  colval = "white",
                  cex.axis = 2.5,
                  cex = 0.1)
                  #ps = 0.1,
                  #ann = FALSE
                  #)
timer_e1 <- timer(e1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 99.9,
                  plotthreshold	= FALSE,
                  colval = "white",
                  cex.axis = 2.5,
                  cex = 0.1)
                  #ps = 0.1,
                  #ann = FALSE
                  #)
timer_h1 <- timer(h1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 99.9,
                  plotthreshold	= FALSE,
                  colval = "white",
                  cex.axis = 2.5,
                  cex = 0.1)
                  #ps = 0.1,
                  #ann = FALSE
                  #)
dev.off()


png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures/trochee_scoring_figure.png", width = 18, height = 9, res = 300, units = "in")
par(mfrow = c(1,2), 
    mar = c(3,3,2,2),
    bty = "l")
timer_e1 <- timer(e1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 35,
                  cex.axis = 1.5)
timer_b1 <- timer(b1,
                  #dmin = 0.02,
                  envt = "hil",
                  msmooth=c(512, 90),
                  threshold = 35,
                  cex.axis = 1.5)
dev.off()

png(filename = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures/mid_long_scoring_figure.png", width = 18, height = 9, res = 300, units = "in")
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
