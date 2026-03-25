# Projekt 10 – Bodenatmung
# Modellvalidierung

# Pakete laden
library(boot)   #Funtkion fuer Cross validation
library(lmtest) #Breusch-Pagan-test -> "Tests zur Pruefung von Heteroskedastizitaet" 

# Daten einlesen
hainich.data <- read.csv("hainich.csv", sep=";")

#best-Modell (Modell aus Teil 1) wird validiert
#evtl. aendern, falls modell nochmals angepasst wird

best_model <- lm(log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) + soilc0 + I(soilc0^2) + I(litdoc^2)
                 + rootdw0 + I(rootdw0^2) + rootc5, data = hainich.data)

model_big <- lm(log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) +
                  soiln5 + I(soiln5^2) + soilc0 + I(soilc0^2) + soilc5 + I(soilc5^2) +
                  I(sno35^2) + I(litdoc^2) + rootdw0 + I(rootdw0^2) + rootdw5 +
                  I(rootdw5^2) + temp.0 + I(temp.0^2) + temp.15 + I(temp.15^2) +
                  rootc5,
                data = hainich.data)

#Zur Uebersicht (dem allgemeinen Verstaendnis)
# Modelluebersicht
#summary(best_model)
#summary(model_big)

# Diagnostikplots des Modells
plot(best_model)
plot(model_big)

#Vergleich der finalen Modelle ueber BIC
bic_best <- BIC(best_model)
bic_big  <- BIC(model_big)

#LOOCV: Modell wird n-mal geschaetzt, jeweils eine Beobachtung weggelassen
cv_rmse_loocv <- function(formula, data){
  glm.fit <- glm(formula, data = data)
  cv.out <- cv.glm(data, glm.fit)
  sqrt(cv.out$delta[1])
}

#k-fold Cross Validation: Datensatz wird in k Teile aufgeteilt
cv_rmse_kfold <- function(formula, data, K = 10){
  glm.fit <- glm(formula, data = data)
  cv.out <- cv.glm(data, glm.fit, K = K)
  sqrt(cv.out$delta[1])
}

#LOOCV-RMSE fuer beide Modelle
rmse_loocv_best <- cv_rmse_loocv(formula(best_model), hainich.data)
rmse_loocv_big  <- cv_rmse_loocv(formula(model_big), hainich.data)

#10-fold-CV-RMSE fuer beide Modelle
rmse_kfold_best <- cv_rmse_kfold(formula(best_model), hainich.data, K = 10)
rmse_kfold_big  <- cv_rmse_kfold(formula(model_big), hainich.data, K = 10)


#Ausgabe der Ergebnisse
cat("BIC (best_model):", bic_best, "\n")
cat("BIC (model_big):", bic_big, "\n\n")

cat("LOOCV RMSE (best_model):", rmse_loocv_best, "\n")
cat("LOOCV RMSE (model_big):", rmse_loocv_big, "\n\n")

cat("10-fold CV RMSE (best_model):", rmse_kfold_best, "\n")
cat("10-fold CV RMSE (model_big):", rmse_kfold_big, "\n\n")

cat(
  "- Trainings RMSE: 0.15; LOOCV RMSE: 0.23; 10-fold RMSE: 0.24 -> Fehler fuer neue Daten ist groesser als im Training\n",
  "- Deutlich groesserer Vorhersagefehler fuer grosses Modell -> Overfitting\n\n"
)

#Test auf Normalverteilung Residuen
#mit shapiro wilk test
#reduziertes mdoel
print(shapiro.test(residuals(best_model)))

#test auf konstante Varianz residuen
#breusch pagan test: Tests zur Pruefung von Heteroskedastizitaet.
#reduziertes modell
print(bptest(best_model))

#model_big: shapiro
print(shapiro.test(residuals(model_big)))
#model_big:breusch
print(bptest(model_big))



cat(
  "- Shapiro: gibt keinen statistischen Hinweis, dass die Residuen nicht normalverteilt sind -> Normalitaetsannahme plausibel erfuellt\n",
  "-> passt typischerweise auch zum QQ-Plot, wenn die Punkte ungefaehr auf der Linie liegen\n",
  "- Breusch-Pagan-Test: keine Hinweise auf Heteroskedastizitaet -> Residuen haben ungefaehr konstante Varianz\n"
)

#beobachten (wichtige)
#cook's Distance misst Einfluss einzelner Beobachtungen
cook <- cooks.distance(best_model)

#typische Schwelle
cook.limit <- 4 / nrow(hainich.data)

print(which(cook > cook.limit))
cook.limit

cat(
  "- beobachtungen mit Cook's Distance >4/n gelten als potenziell einflussreich\n",
  "-> Beobachtungen 4, 6, 8, 33, 37, 38 ueberschritten Schwelle\n",
  "-> Punkte koennen einen relativ starken Einfluss auf Modellschaetzung haben\n"
)

#-> plots
#cooks distanz plot
plot(cook, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's D")
abline(h = cook.limit, col = 2, lty = 2)

