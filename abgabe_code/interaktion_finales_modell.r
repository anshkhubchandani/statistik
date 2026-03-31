###### Pakete laden
library(boot)   #Funtkion fuer Cross validation
library(lmtest) #Breusch-Pagan-test -> "Tests zur Pruefung von Heteroskedastizitaet" 
library(zoo)

###### Daten einlesen
hainich.data <- read.csv("hainich.csv", sep=";")

###### Modelle

## finales Modell
#temp.15, litter.d
lm1 <- lm(log(soil.res) ~ I(temp.15^2) + I(litter.d^2), data = hainich.data)

# mit Interaktion
lmI <- lm(log(soil.res) ~ I(temp.15^2) * I(litter.d^2), data = hainich.data)

# mit linearen Termen
lml <- lm(log(soil.res) ~ temp.15 + I(temp.15^2) + litter.d + I(litter.d^2), data = hainich.data)

# mit Interaktion und linearen Termen
lmlI <- lm(log(soil.res) ~ temp.15 + I(temp.15^2) * litter.d + I(litter.d^2), data = hainich.data)

###### Diagnostikplots des Modells

plot(lm1)
plot(lmI)
plot(lml)
plot(lmlI)

###### Modelluebersicht
summary(lm1)
summary(lmI)
summary(lml)
summary(lmlI)

###### BIC
BIC(lm1)
BIC(lmI)
BIC(lml)
BIC(lmlI)

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
rmse_loocv_lm2  <- cv_rmse_loocv(formula(lmI), hainich.data)
rmse_loocv_lm3  <- cv_rmse_loocv(formula(lml), hainich.data)
rmse_loocv_lm4  <- cv_rmse_loocv(formula(lmlI), hainich.data)

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
rmse_kfold_lm2  <- cv_rmse_kfold(formula(lmI), hainich.data, K = 10)
rmse_kfold_lm3  <- cv_rmse_kfold(formula(lml), hainich.data, K = 10)
rmse_kfold_lm4  <- cv_rmse_kfold(formula(lmlI), hainich.data, K = 10)

###### Shapiro Wilk Test
#Test auf Normalverteilung Residuen
# W sollte nahe 1 sein, für Normalverteilung
print(shapiro.test(residuals(lm1)))
print(shapiro.test(residuals(lmI)))
print(shapiro.test(residuals(lml)))
print(shapiro.test(residuals(lmlI)))

###### Breusch Pagan Test
#test auf konstante Varianz residuen
#Tests zur Pruefung von Heteroskedastizitaet
#wenn p-Wert signifikant ist das Hinweis für Heterokedastizität
print(bptest(lm1))
print(bptest(lmI))
print(bptest(lml))
print(bptest(lmlI))

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
cook_distance(lmI)
cook_distance(lml)
cook_distance(lmlI)

