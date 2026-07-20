setwd("/Users/taylorhiers/Documents/WTSP")

library(ggplot2)
library(cowplot)

######Increasing color contrast on Fig 3######
the_truth <- read.csv("./the_truth.csv")
only_doub_and_trip <- the_truth$Terminal.Strophe.type == "Doublet" | the_truth$Terminal.Strophe.type == "Triplet"
only_doub <- the_truth$Terminal.Strophe.type == "Doublet"

png(filename = "/Users/taylorhiers/Documents/WTSP/fig3alt.png", width = 7, height = 7, units = "in", res = 300)
ggplot(data = subset(the_truth, only_doub_and_trip), aes(x=Terminal.Strophe.type, y=min_max_dur_ratio, color = mid_to_long_ratio)) + 
  geom_jitter(position=position_jitter(0.1)) +
  xlab("Published observer score") +
  ylab("trochee score") +
  theme_cowplot() +
  labs(color='mid:long ratio') +
  scale_color_gradient(low="blue", high="red")
dev.off()

######Adding a legend to Fig 4######
library(mclust)
# mclust_measures <- data.frame(the_truth$min_max_ratio, the_truth$mid_to_long_ratio, the_truth$min_max_dur_ratio)
# BIC <- mclustBIC(mclust_measures)
mclust_measures <- data.frame(asin(sqrt(the_truth$min_max_dur_ratio)), asin(sqrt(the_truth$mid_to_long_ratio)))
BIC <- mclustBIC(mclust_measures)

mod1 <- Mclust(mclust_measures, x = BIC)
mclust.options("classPlotSymbols" = c(16, 16, 16, 16, 16))

the_truth$poetry <- mod1$classification
##In classification, 1 = cretic, 2 = trochaic, 3 = dactyl
the_truth$prob_assignment <- rep(NA, length(the_truth[,1]))
for(i in 1:length(the_truth$prob_assignment)){
  the_truth$prob_assignment[i] <- max(mod1$z[i,])
}

trace(mclust2Dplot, edit=TRUE)
##Edit mclust2Dplot by defining u as 1-u, and altering bubble() function as follows
##u <- (1 - u)^2
##b <- bubble(u, cex = cex * c(0.3, 1.5), alpha = c(1,1))
clusters <- read.csv(file = "clustering_for_maps.csv")

wrap_title <- strwrap("Clustering assignment", width = 15)

png(filename = "/Users/taylorhiers/Documents/WTSP/fig4.png", width = 7, height = 7, units = "in", res = 300)
plot(mod1, what = "uncertainty", xlab = "arcsin-transformed trochee score", ylab = "arcsin-transformed mid:long ratio", colors = c("#37a8b7", "#FE9929", "#AE017E"), bty = "l")
legend(0.8, 0.82, legend=c(paste("Cretic (–","\u1D17", "–)"), paste("Trochaic (–", "\u1D17",")"),paste("Dactylic (–","\u1D17","\u1D17",")")),
       col=c("#37a8b7", "#FE9929","#AE017E"), cex=0.9, pch=16,title="Rhythm type cluster",pt.cex=1.2)
dev.off()

##upside down circle unicode: "\u1D17"


######Making legends for Fig 5######

png(filename = "/Users/taylorhiers/Documents/WTSP/fig5legendright.png", width = 7, height = 7, units = "in", res = 300)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend=c(paste("Cretic (–","\u1D17", "–)"), paste("Trochaic (–", "\u1D17",")"),paste("Dactylic (–","\u1D17","\u1D17",")")),
       col=c("#37a8b7", "#FE9929","#AE017E"), cex=1, pch=16,title="Rhythm type cluster",horiz=TRUE,pt.cex=1.2)
dev.off()

png(filename = "/Users/taylorhiers/Documents/WTSP/fig5legendleft.png", width = 7, height = 7, units = "in", res = 300)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend=c("Triplet", "Doublet"),
       col=c("#37a8b7", "#FE9929"), cex=1, pch=16,title="Human scores",horiz=TRUE,pt.cex=1.2)
dev.off()

######Making a legend for figure 7######
png(filename = "/Users/taylorhiers/Documents/WTSP/fig7legend.png", width = 7, height = 7, units = "in", res = 300)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend=c(paste("Cretic (–","\u1D17", "–)"), paste("Trochaic (–", "\u1D17",")"),paste("Dactylic (–","\u1D17","\u1D17",")")),
       col=c("#37a8b7", "#FE9929","#AE017E"), cex=1,title="Rhythm Type Cluster",horiz=TRUE,pt.cex=1.2,lty=1,lwd=3,seg.len=2.5)
dev.off()

######making spectrograms for entire songs######
library(seewave)
library(tuneR)
#### cretic: ML98816 ####
a<-readWave("98816_fullsong.wav")
# new wave object with bandpass filter applied:
a1<-fir(a,
        from=1000,
        to=7000,
        bandpass=T,
        output="Wave")

#### trochaic: ML146267 ####
d<-readWave("146267_fullsong.wav")
e1<-fir(d,
        from=1000,
        to=7000,
        bandpass=T,
        output="Wave") # initial filter

#### dactyl: XC190052 ####
g<-readWave("XC190052_whole_song_16bit.wav")
h1<-fir(g,
        from=1000,
        to=7000,
        bandpass=T,
        output="Wave") # initial filter

png(filename = "./wholesong_spectro.png", width = 27, height = 15, res = 300, units = "in")
par(mfrow = c(3,1),
    oma = c(2,1.5,1.5,0), 
    mar = c(4,60,2,60),
    mgp = c(3,2,0))
c1<-spectro(a1, 
            wl = 512,
            ovlp = 95,
            collevels = c(-30,-28,-25,-20,-8,-6,-4,-2,0), 
            flim = c(2, 6),
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
            collevels = c(-30,-28,-25,-20,-8,-6,-4,-2,0), 
            flim = c(2, 6),
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
            collevels = c(-30,-28,-25,-20,-8,-6,-4,-2,0), 
            flim = c(2, 6),
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