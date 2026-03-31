###### Pakete laden
library(boot)   #Funtkion fuer Cross validation
library(lmtest) #Breusch-Pagan-test -> "Tests zur Pruefung von Heteroskedastizitaet" 
library(zoo)

###### Daten einlesen
hainich.data <- read.csv("hainich.csv", sep=";")

###### Modelle

#temp.15, litter.d
lm1 <- lm(log(soil.res) ~ I(temp.15^2) + I(litter.d^2), data = hainich.data)

# temp.15 und lmoi
lm2 <- lm(log(soil.res) ~ I(temp.15^2) + I(lmoi^2), data = hainich.data)

# litter.d und soiln0
lm3 <- lm(log(soil.res) ~ I(litter.d^2) + I(soiln0^2), data = hainich.data)

# litter.d, soiln0, temp.15 und lmoi
lm4 <- lm(log(soil.res) ~ I(litter.d^2) + I(lmoi^2) + I(temp.15^2) + I(soiln0^2), data = hainich.data)

###### Diagnostikplots des Modells

plot(lm1)
plot(lm2)
plot(lm3)
plot(lm4)

###### Modelluebersicht
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

###### speichern des Plots
# PNG device
png("residuals.png")

# Code
plot(lm1, 1)

# Close device
dev.off()

###### BIC
BIC(lm1)
BIC(lm2)
BIC(lm3)
BIC(lm4)

####### LOOCV
# Modell wird n-mal geschaetzt, jeweils eine Beobachtung weggelassen
# je kleiner, desto besser
cv_rmse_loocv <- function(formula, data){
  glm.fit <- glm(formula, data = data)
  cv.out <- cv.glm(data, glm.fit)
  sqrt(cv.out$delta[1])
}

#LOOCV-RMSE fuer beide Modelle
rmse_loocv_lm1 <- cv_rmse_loocv(formula(lm1), hainich.data)
rmse_loocv_lm2  <- cv_rmse_loocv(formula(lm2), hainich.data)
rmse_loocv_lm3  <- cv_rmse_loocv(formula(lm3), hainich.data)
rmse_loocv_lm4  <- cv_rmse_loocv(formula(lm4), hainich.data)

####### k-fold Cross Validation:
# Datensatz wird in k Teile aufgeteilt
# ebenso je kleiner, desto besser
cv_rmse_kfold <- function(formula, data, K = 10){
  glm.fit <- glm(formula, data = data)
  cv.out <- cv.glm(data, glm.fit, K = K)
  sqrt(cv.out$delta[1])
}

#10-fold-CV-RMSE fuer beide Modelle
rmse_kfold_lm1 <- cv_rmse_kfold(formula(lm1), hainich.data, K = 10)
rmse_kfold_lm2  <- cv_rmse_kfold(formula(lm2), hainich.data, K = 10)
rmse_kfold_lm3  <- cv_rmse_kfold(formula(lm3), hainich.data, K = 10)
rmse_kfold_lm4  <- cv_rmse_kfold(formula(lm4), hainich.data, K = 10)

###### Shapiro Wilk Test
#Test auf Normalverteilung Residuen
# W sollte nahe 1 sein, für Normalverteilung
print(shapiro.test(residuals(lm1)))
print(shapiro.test(residuals(lm2)))
print(shapiro.test(residuals(lm3)))
print(shapiro.test(residuals(lm4)))

###### Breusch Pagan Test
#test auf konstante Varianz residuen
#Tests zur Pruefung von Heteroskedastizitaet
#wenn p-Wert signifikant ist das Hinweis für Heterokedastizität
print(bptest(lm1))
print(bptest(lm2))
print(bptest(lm3))
print(bptest(lm4))

###### Cook's Distance
#beobachten (wichtige)
#misst Einfluss einzelner Beobachtungen

cook_distance <- function(model) {
  cook <- cooks.distance(model)
  #typische Schwelle
  cook.limit <- 4 / nrow(hainich.data)
  print(which(cook > cook.limit))
  cook.limit
  #-> plots
  #cooks distanz plot
  plot(cook, type = "h",
      main = "Cook's Distance",
      ylab = "Cook's D")
  abline(h = cook.limit, col = 2, lty = 2)
}

cook_distance(lm1)
cook_distance(lm2)
cook_distance(lm3)
cook_distance(lm4)



