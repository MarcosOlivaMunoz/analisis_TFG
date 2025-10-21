library(tidyverse)
library(ltm)
library(psych)
library(eRm)
library(difR)
library(mirt)
library(ggmirt)

source("scripts/limpieza_datos_bin.R")

# ---=== Análisis exploratorio ===---
# PRETEST
# Item Difficulty
item_diff_pretest <- colMeans(pretestBIN, na.rm = TRUE)
round(item_diff_pretest,3)
# Item Discrimination
total_score_pretest <- rowSums(pretestBIN)
item_discr_pretest <- cor(pretestBIN,total_score_pretest); item_discr_pretest


# 1-parameter Rasch
# Cualitativo o Cuantitativo
par(mfrow=c(1,3))
item_weights_pretest <- rasch(pretestBIN[,1:3], na.action = NULL)
plot(item_weights_pretest,type="ICC")
item_weights_test1 <- rasch(test1BIN[,3:5], na.action = NULL)
plot(item_weights_test1,type="ICC")
item_weights_test2 <- rasch(test2BIN[,3:4], na.action = NULL)
plot(item_weights_test2,type="ICC")


par(mfrow=c(1,2))
test2_si <- test2BIN[test2BIN$Participación=="Sí",]
test2_no <- test2BIN[test2BIN$Participación=="No",]
item_weights_test2si <- rasch(test2_si[,3:4], na.action = NULL)
plot(item_weights_test2si,type="ICC")
item_weights_test2no <- rasch(test2_no[,3:4], na.action = NULL)
plot(item_weights_test2no,type="ICC")

data_pretest <- RM(pretestBIN)
summary(data_pretest)
data_test1 <- RM(test1BIN)
summary(data_test1)
data_test2 <- RM(test2BIN[-1])
summary(data_test2)

calcular_rasch <- function(b){
  p=1/(1+exp(b))
  return(p)
}

calcular_rasch(c(-1.165,-0.069))
calcular_rasch(c(-0.911,0.2))
calcular_rasch(c(-0.79,0.283))

# 2-parameter
# Cualitativo o Cuantitativo
par(mfrow=c(1,1))
pretest_2p <- ltm(pretestBIN ~ z1)
plot(pretest_2p, item=c(1,2,3,5))


# 3-parameter
# Cualitativo o Cuantitativo

# Fitting the model
unimodel <- 'F1 = 1-6'
fit3PL <- mirt(data = pretestBIN, 
              model = unimodel,
              itemtype = "3PL",
              verbose = FALSE)
summary(fit3PL)
# IRT pararmeter
params3PL <- coef(fit3PL, IRTparms = TRUE, simplify = TRUE)
round(params3PL$items, 2)
# Model, person & item fit
M2(fit3PL)
itemfit(fit3PL)
itemfit(fit3PL, fit_stats = "infit")
itemfitPlot(fit3PL)
itempersonMap(fit3PL)

tracePlot(fit3PL)
tracePlot(fit3PL, facet = F, legend = T)
itemInfoPlot(fit3PL, legend = T) + scale_color_brewer(palette = "Set3")
