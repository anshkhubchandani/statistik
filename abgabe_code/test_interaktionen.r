###### Pakete laden
library(boot)   #Funtkion fuer Cross validation
library(lmtest) #Breusch-Pagan-test -> "Tests zur Pruefung von Heteroskedastizitaet" 
library(zoo)

###### Daten einlesen
hainich.data <- read.csv("hainich.csv", sep=";")

###### Modelle

## andere Modelle mit Interaktionen

# temp.15 und lmoi
lm2 <- lm(log(soil.res) ~ I(temp.15^2) * I(lmoi^2), data = hainich.data)

# litter.d und soiln0
lm3 <- lm(log(soil.res) ~ I(litter.d^2) * I(soiln0^2), data = hainich.data)

# litter.d, soiln0, temp.15 und lmoi
lm41 <- lm(log(soil.res) ~ I(litter.d^2) * I(lmoi^2) + I(temp.15^2) + I(soiln0^2), data = hainich.data)
lm42 <- lm(log(soil.res) ~ I(litter.d^2) + I(lmoi^2) * I(temp.15^2) + I(soiln0^2), data = hainich.data)
lm43 <- lm(log(soil.res) ~ I(litter.d^2) + I(lmoi^2) + I(temp.15^2) * I(soiln0^2), data = hainich.data)
lm44 <- lm(log(soil.res) ~ I(litter.d^2) * I(temp.15^2) + I(lmoi^2) + I(soiln0^2), data = hainich.data)
lm45 <- lm(log(soil.res) ~ I(temp.15^2) + I(lmoi^2) + I(litter.d^2) * I(soiln0^2), data = hainich.data)

###### Diagnostikplots des Modells

plot(lm2)
plot(lm3)
plot(lm41)
plot(lm42)
plot(lm43)
plot(lm44)
plot(lm45)

###### Modelluebersicht
summary(lm2)
summary(lm3)
summary(lm41)
summary(lm42)
summary(lm43)
summary(lm44)
summary(lm45)

###### BIC
BIC(lm2)
BIC(lm3)
BIC(lm41)
BIC(lm42)
BIC(lm43)
BIC(lm44)
BIC(lm45)

####### LOOCV
# Modell wird n-mal geschaetzt, jeweils eine Beobachtung weggelassen
# je kleiner, desto besser
cv_rmse_loocv <- function(formula, data){
  glm.fit <- glm(formula, data = data)
  cv.out <- cv.glm(data, glm.fit)
  sqrt(cv.out$delta[1])
}

#LOOCV-RMSE fuer beide Modelle
rmse_loocv_lm2 <- cv_rmse_loocv(formula(lm2), hainich.data)
rmse_loocv_lm3  <- cv_rmse_loocv(formula(lm3), hainich.data)
rmse_loocv_lm41  <- cv_rmse_loocv(formula(lm41), hainich.data)
rmse_loocv_lm42 <- cv_rmse_loocv(formula(lm42), hainich.data)
rmse_loocv_lm43 <- cv_rmse_loocv(formula(lm43), hainich.data)
rmse_loocv_lm44 <- cv_rmse_loocv(formula(lm44), hainich.data)
rmse_loocv_lm45 <- cv_rmse_loocv(formula(lm45), hainich.data)


####### k-fold Cross Validation:
# Datensatz wird in k Teile aufgeteilt
# ebenso je kleiner, desto besser
cv_rmse_kfold <- function(formula, data, K = 10){
  glm.fit <- glm(formula, data = data)
  cv.out <- cv.glm(data, glm.fit, K = K)
  sqrt(cv.out$delta[1])
}

#10-fold-CV-RMSE fuer beide Modelle
rmse_kfold_lm2 <- cv_rmse_kfold(formula(lm2), hainich.data, K = 10)
rmse_kfold_lm3  <- cv_rmse_kfold(formula(lm3), hainich.data, K = 10)
rmse_kfold_lm41  <- cv_rmse_kfold(formula(lm41), hainich.data, K = 10)
rmse_kfold_lm42 <- cv_rmse_kfold(formula(lm42), hainich.data, K = 10)
rmse_kfold_lm43 <- cv_rmse_kfold(formula(lm43), hainich.data, K = 10)
rmse_kfold_lm44 <- cv_rmse_kfold(formula(lm44), hainich.data, K = 10)
rmse_kfold_lm45 <- cv_rmse_kfold(formula(lm45), hainich.data, K = 10)


###### Shapiro Wilk Test
#Test auf Normalverteilung Residuen
# W sollte nahe 1 sein, für Normalverteilung
print(shapiro.test(residuals(lm2)))
print(shapiro.test(residuals(lm3)))
print(shapiro.test(residuals(lm41)))
print(shapiro.test(residuals(lm42)))
print(shapiro.test(residuals(lm43)))
print(shapiro.test(residuals(lm44)))
print(shapiro.test(residuals(lm45)))

###### Breusch Pagan Test
#test auf konstante Varianz residuen
#Tests zur Pruefung von Heteroskedastizitaet
#wenn p-Wert signifikant ist das Hinweis für Heterokedastizität
print(bptest(lm2))
print(bptest(lm3))
print(bptest(lm41))
print(bptest(lm42))
print(bptest(lm43))
print(bptest(lm44))
print(bptest(lm45))

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

cook_distance(lm2)
cook_distance(lm3)
cook_distance(lm41)
cook_distance(lm42)
cook_distance(lm43)
cook_distance(lm44)
cook_distance(lm45)