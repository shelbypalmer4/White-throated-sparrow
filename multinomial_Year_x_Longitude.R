#install.packages("RColorBrewer")
library(RColorBrewer)

setwd("/Users/taylorhiers/Documents/WTSP")

#otter_data <- read.csv("/Users/jaymcentee/Dropbox/White-throated Sparrows/otter_data.csv")

quant_data <- read.csv("./clustering_for_maps.csv")
quant_data$poetry <- as.factor(quant_data$poetry)
head(quant_data)

##THERE ARE MANY NA'S IN THIS VERSION OF THIS DATA SET

library(MuMIn)
library(nnet)
library(effects)
library(ggplot2)
library(cowplot)
library(stargazer)

#library(globaltest)
#cc_quant_data <- quant_data[complete.cases(quant_data$poetry, #quant_data$Year, quant_data$Longitude),]
#glbtest.mod <- globaltest::mlogit(as.factor(poetry) ~ as.numeric(Year)*Longitude, data = cc_quant_data)


multinom.m1 <- multinom(poetry ~ Year*Longitude, data = cc_quant_data)
##1 = cretic, 2 = trochee, 3 = dactyl
summary(multinom.m1)
stargazer(multinom.m1, type="text")

logLik(multinom.m1) 
# this is a base R function. It also includes the df.

library(pscl)
# pseudo r-squared
pscl::pR2(multinom.m1)
# pR2, from the pscl package, returns a variety of pseudo r-squared statistics.

# Chi-squared test of model fit
# This is a likelihood ratio test comparing the full model to a null model.

# First, fit the null model
null <- multinom(poetry ~ 1, data = quant_data)

# Then, calculate the test statistic
2 * (logLik(multinom.m1) - logLik(null))

install.packages("gtsummary")
library(gtsummary)
tbl_regression(multinom.m1, exponentiate = TRUE)
##Year range is 1951 - 2021
##Longitude range is -135.066 to -57.7439
##Longitude of BC population is -122.76
##Longitude of Springfield is -93.25
##Longitude of upstate New York where many historical recordings made -73.9
years_for_preds <- seq(from = 1951, to = 2021, length.out = 71)

west_longitude_for_pred <- rep(-122.76, 71)
central_longitude_for_pred <- rep(-93.25, 71)
east_longitude_for_pred <- rep(-73.9, 71)

nd1 <- data.frame(years_for_preds, west_longitude_for_pred)
colnames(nd1) <- c("Year", "Longitude")
multinom.west <- predict(multinom.m1, type = "probs", newdata = nd1)

nd2 <- data.frame(years_for_preds, central_longitude_for_pred)
colnames(nd2) <- c("Year", "Longitude")
multinom.west <- predict(multinom.m1, type = "probs", newdata = nd2)

nd3 <- data.frame(years_for_preds, east_longitude_for_pred)
colnames(nd3) <- c("Year", "Longitude")
multinom.west <- predict(multinom.m1, type = "probs", newdata = nd3)

##For quant_data$poetry, 1 = cretic, 2 = trochaic, 3 = dactyl

year_probs_cretic <- rep(NA, 71)
year_lower_prob_cretic <- rep(NA, 71)
year_upper_prob_cretic <- rep(NA, 71)
year_probs_trochaic <- rep(NA, 71)
year_lower_prob_trochaic <- rep(NA, 71)
year_upper_prob_trochaic <- rep(NA, 71)
year_probs_dactyl <- rep(NA, 71)
year_lower_prob_dactyl <- rep(NA, 71)
year_upper_prob_dactyl <- rep(NA, 71)

library(effects)

for (i in 1:length(years_for_preds)){
  fit.eff <- Effect(focal.predictors = c("Year"), mod = multinom.m1, xlevels = list(Year = c(years_for_preds[i], years_for_preds[i])), fixed.predictors = list(given.values = c("Longitude" = c(west_longitude_for_pred[1]))))
  year_probs_cretic[i] <- fit.eff$prob[1,1]
  year_lower_prob_cretic[i] <- fit.eff$lower.prob[1,1]
  year_upper_prob_cretic[i] <- fit.eff$upper.prob[1,1]
  year_probs_trochaic[i] <- fit.eff$prob[1,2]
  year_lower_prob_trochaic[i] <- fit.eff$lower.prob[1,2]
  year_upper_prob_trochaic[i] <- fit.eff$upper.prob[1,2]
  year_probs_dactyl[i] <- fit.eff$prob[1,3]
  year_lower_prob_dactyl[i] <- fit.eff$lower.prob[1,3]
  year_upper_prob_dactyl[i] <- fit.eff$upper.prob[1,3]
}

cretic_west <- splinefun(years_for_preds, year_probs_cretic)
cretic_lower_west <- splinefun(years_for_preds, year_lower_prob_cretic)
cretic_upper_west <- splinefun(years_for_preds, year_upper_prob_cretic)
trochaic_west <- splinefun(years_for_preds, year_probs_trochaic)
trochaic_lower_west <- splinefun(years_for_preds, year_lower_prob_trochaic)
trochaic_upper_west <- splinefun(years_for_preds, year_upper_prob_trochaic)
dactyl_west <- splinefun(years_for_preds, year_probs_dactyl)
dactyl_lower_west <- splinefun(years_for_preds, year_lower_prob_dactyl)
dactyl_upper_west <- splinefun(years_for_preds, year_upper_prob_dactyl)

##Try using cowplot. dashed line is linetype = 3 in stat_function
song_rhythm_probability_by_year_west <- ggplot(data.frame(x = c(1951, 2021)), aes(x)) +
  ylim(0,1) +
  ylab("") +
  xlab("Year") +
  stat_function(fun = cretic_west, colour = "#41B6C4", size = 1.5) +
  stat_function(fun = cretic_lower_west, colour = "#41B6C4", size = 1, linetype = 5) +
  stat_function(fun = cretic_upper_west, colour = "#41B6C4", size = 1, linetype = 5) +
  stat_function(fun = trochaic_west, colour = "#FE9929", size = 1.5) +
  stat_function(fun = trochaic_lower_west, colour = "#FE9929", size = 1, linetype = 5) +
  stat_function(fun = trochaic_upper_west, colour = "#FE9929", size = 1, linetype = 5) +
  stat_function(fun = dactyl_west, colour = "#AE017E", size = 1.5) +
  stat_function(fun = dactyl_lower_west, colour = "#AE017E", size = 1, linetype = 5) +
  stat_function(fun = dactyl_upper_west, colour = "#AE017E", size = 1, linetype = 5) +
  theme_cowplot(25)
ggsave(filename = "rhythms_by_year_west.png", plot = song_rhythm_probability_by_year_west)

####### Central

for (i in 1:length(years_for_preds)){
  fit.eff <- Effect(focal.predictors = c("Year"), mod = multinom.m1, xlevels = list(Year = c(years_for_preds[i], years_for_preds[i])), fixed.predictors = list(given.values = c("Longitude" = c(central_longitude_for_pred[1]))))
  year_probs_cretic[i] <- fit.eff$prob[1,1]
  year_lower_prob_cretic[i] <- fit.eff$lower.prob[1,1]
  year_upper_prob_cretic[i] <- fit.eff$upper.prob[1,1]
  year_probs_trochaic[i] <- fit.eff$prob[1,2]
  year_lower_prob_trochaic[i] <- fit.eff$lower.prob[1,2]
  year_upper_prob_trochaic[i] <- fit.eff$upper.prob[1,2]
  year_probs_dactyl[i] <- fit.eff$prob[1,3]
  year_lower_prob_dactyl[i] <- fit.eff$lower.prob[1,3]
  year_upper_prob_dactyl[i] <- fit.eff$upper.prob[1,3]
}

cretic_central <- splinefun(years_for_preds, year_probs_cretic)
cretic_lower_central <- splinefun(years_for_preds, year_lower_prob_cretic)
cretic_upper_central <- splinefun(years_for_preds, year_upper_prob_cretic)
trochaic_central <- splinefun(years_for_preds, year_probs_trochaic)
trochaic_lower_central <- splinefun(years_for_preds, year_lower_prob_trochaic)
trochaic_upper_central <- splinefun(years_for_preds, year_upper_prob_trochaic)
dactyl_central <- splinefun(years_for_preds, year_probs_dactyl)
dactyl_lower_central <- splinefun(years_for_preds, year_lower_prob_dactyl)
dactyl_upper_central <- splinefun(years_for_preds, year_upper_prob_dactyl)

##Try using cowplot. dashed line is linetype = 3 in stat_function
song_rhythm_probability_by_year_central <- ggplot(data.frame(x = c(1951, 2021)), aes(x)) +
  ylim(0,1) +
  ylab("") +
  xlab("Year") +
  stat_function(fun = cretic_central, colour = "#41B6C4", size = 1.5) +
  stat_function(fun = cretic_lower_central, colour = "#41B6C4", size = 1, linetype = 5) +
  stat_function(fun = cretic_upper_central, colour = "#41B6C4", size = 1, linetype = 5) +
  stat_function(fun = trochaic_central, colour = "#FE9929", size = 1.5) +
  stat_function(fun = trochaic_lower_central, colour = "#FE9929", size = 1, linetype = 5) +
  stat_function(fun = trochaic_upper_central, colour = "#FE9929", size = 1, linetype = 5) +
  stat_function(fun = dactyl_central, colour = "#AE017E", size = 1.5) +
  stat_function(fun = dactyl_lower_central, colour = "#AE017E", size = 1, linetype = 5) +
  stat_function(fun = dactyl_upper_central, colour = "#AE017E", size = 1, linetype = 5) +
  theme_cowplot(25)
ggsave(filename = "rhythms_by_year_central.png", plot = song_rhythm_probability_by_year_central)

####East
for (i in 1:length(years_for_preds)){
  fit.eff <- Effect(focal.predictors = c("Year"), mod = multinom.m1, xlevels = list(Year = c(years_for_preds[i], years_for_preds[i])), fixed.predictors = list(given.values = c("Longitude" = c(east_longitude_for_pred[1]))))
  year_probs_cretic[i] <- fit.eff$prob[1,1]
  year_lower_prob_cretic[i] <- fit.eff$lower.prob[1,1]
  year_upper_prob_cretic[i] <- fit.eff$upper.prob[1,1]
  year_probs_trochaic[i] <- fit.eff$prob[1,2]
  year_lower_prob_trochaic[i] <- fit.eff$lower.prob[1,2]
  year_upper_prob_trochaic[i] <- fit.eff$upper.prob[1,2]
  year_probs_dactyl[i] <- fit.eff$prob[1,3]
  year_lower_prob_dactyl[i] <- fit.eff$lower.prob[1,3]
  year_upper_prob_dactyl[i] <- fit.eff$upper.prob[1,3]
}

cretic_east <- splinefun(years_for_preds, year_probs_cretic)
cretic_lower_east <- splinefun(years_for_preds, year_lower_prob_cretic)
cretic_upper_east <- splinefun(years_for_preds, year_upper_prob_cretic)
trochaic_east <- splinefun(years_for_preds, year_probs_trochaic)
trochaic_lower_east <- splinefun(years_for_preds, year_lower_prob_trochaic)
trochaic_upper_east <- splinefun(years_for_preds, year_upper_prob_trochaic)
dactyl_east <- splinefun(years_for_preds, year_probs_dactyl)
dactyl_lower_east <- splinefun(years_for_preds, year_lower_prob_dactyl)
dactyl_upper_east <- splinefun(years_for_preds, year_upper_prob_dactyl)

##Try using cowplot. dashed line is linetype = 3 in stat_function
song_rhythm_probability_by_year_east <- ggplot(data.frame(x = c(1951, 2021)), aes(x)) +
  ylim(0,1) +
  ylab("") +
  xlab("Year") +
  stat_function(fun = cretic_east, colour = "#41B6C4", size = 1.5) +
  stat_function(fun = cretic_lower_east, colour = "#41B6C4", size = 1, linetype = 5) +
  stat_function(fun = cretic_upper_east, colour = "#41B6C4", size = 1, linetype = 5) +
  stat_function(fun = trochaic_east, colour = "#FE9929", size = 1.5) +
  stat_function(fun = trochaic_lower_east, colour = "#FE9929", size = 1, linetype = 5) +
  stat_function(fun = trochaic_upper_east, colour = "#FE9929", size = 1, linetype = 5) +
  stat_function(fun = dactyl_east, colour = "#AE017E", size = 1.5) +
  stat_function(fun = dactyl_lower_east, colour = "#AE017E", size = 1, linetype = 5) +
  stat_function(fun = dactyl_upper_east, colour = "#AE017E", size = 1, linetype = 5) +
  theme_cowplot(25)
ggsave(filename = "rhythms_by_year_east.png", plot = song_rhythm_probability_by_year_east)

par(mfrow = c(1, 2))
brewer.pal(n = 11, name ="RdPu")

palette(c("#FE9929", "#41B6C4"))

otter_data$Terminal.Strophe.type <- as.factor(otter_data$Terminal.Strophe.type)
plot(otter_data$Longitude, otter_data$Year, col = otter_data$Terminal.Strophe.type, pch = 16, cex = 1.5, bty = "l", xlab = "Longitude", ylab = "Year")
legend("left", legend = c("Doublet", "Triplet"), col = c("#FE9929", "#41B6C4"), bty = "o", pch = 16)

palette(c("#41B6C4", "#FE9929", "#AE017E"))
plot(quant_data$Longitude, quant_data$Year, col = quant_data$poetry, pch = 16, cex = 1.5, bty = "l", xlab = "Longitude", ylab = "Year")
legend("left", legend = c("Cretic", "Trochaic", "Dactyl"), col = c("#41B6C4", "#FE9929", "#AE017E"), bty = "o", pch = 16)