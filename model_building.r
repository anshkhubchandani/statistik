# erstellen aller Modelle brute force

# Prädikatoren
# smoi^2, lmoi, litter.d, soiln0, soiln5, soilc0, soilc5, sno35, litdoc, sdoc5, rootdw0, rootdw5
# jeweils linear und quadratisch

pred <-c("smoi", "lmoi", "litter.d", "soiln0", "soiln5", "soilc0", "soilc5", "sno35", "litdoc", "sdoc5", "rootdw0", "rootdw5")

# erstellen des quadratischen Prädikator
create_quad_pred <- function(pred){
    paste0("I(", pred, "^2)")
}

# Teilmengen berechnen
# wenn ein quadratischer term eingefügt wird, muss auch der lineare vorkommen
# lm(y ~ lmoi + smoi + I(smoi^2) + rootdw0 + litdoc + soiln0 + sno35, data = hainich.data)
# y = log(soil.res)
# ausgabe liste aller modelle

powerset_linear <- function(pred) {
  # Start mit Liste, die nur die leere Menge enthält
  P <- list(c())
  
  # Für jedes Element der Menge
  for (x in pred) {
    # Neue Teilmengen: zu jeder bisherigen Teilmenge x hinzufügen
    new_subsets <- lapply(P, function(sub) c(sub, x))
    # Alte und neue Teilmengen zusammenführen
    P <- c(P, new_subsets)
  }
  
  return(P)
}

#linear_models <- powerset_linear(pred) #4096 Teilmengen
#length(linear_models)

# quadratische + lineare Modelle
# 3^12=531.441 Modelle -1 der komplett leeren Teilmenge

powerset_quads <- function(pred) {
  lin_sets <- powerset_linear(pred)
  all_models <- list()

  for (S in lin_sets) {
    # immer das reine lineare Modell hinzufügen
    all_models[[length(all_models) + 1]] <- S

    if (length(S) == 0) next  # zur leeren Menge gibt es keine Quadrate

    # alle Teilmengen der Variablen in S bestimmen (für Quadratterme)
    quad_lin_sets <- powerset_linear(S)
    # erste ist die leere Menge -> nur lineares Modell, haben wir oben schon
    if (length(quad_lin_sets) > 1) {
      for (Q in quad_lin_sets[-1]) {
        quad_terms <- create_quad_pred(Q)
        all_models[[length(all_models) + 1]] <- c(S, quad_terms)
      }
    }
}

    return(all_models)
}

#models <- powerset_quads(pred)
#length(models)
#models[1:5]

# Funktion regsubsets()
# macht globale Suche, also brute force
# Variablen: smoi^2, lmoi, litter.d, soiln0, soiln5, soilc0, soilc5, sno35, litdoc, rootdw0, rootdw5, temp.0, temp.15, rootc5

library(leaps)

hainich.data <- read.csv("hainich.csv", sep=";")

fit <- regsubsets(log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) +litter.d + I(litter.d^2)+ soiln0+ I(soiln0^2) + soiln5 + 
                  + I(soiln5^2) + soilc0 + I(soilc0^2) + soilc5 + I(soilc5^2) + sno35 + I(sno35^2) + litdoc + I(litdoc^2)
                  + rootdw0 + I(rootdw0^2) + rootdw5 + I(rootdw5^2) + temp.0 + I(temp.0^2) + temp.15 + I(temp.15^2) + rootc5 + I(rootc5^2), 
                  data = hainich.data, 
                  nvmax = 30)  # max 12 Prädiktoren + Intercept

# Übersichtstabelle
sum_fit <- summary(fit)
sum_fit$which    # TRUE/FALSE-Matrix: welche Variable in welchem Modell

# Kriterien anschauen
cbind(
  Rsq = sum_fit$rsq,
  AdjRsq = sum_fit$adjr2, 
  Cp = sum_fit$cp,
  BIC = sum_fit$bic
)

#           Rsq    AdjRsq        Cp        BIC
' [1,] 0.2270764 0.2056063 27.944478  -2.512680
 [2,] 0.4323087 0.3998692 13.496531 -10.601984
 [3,] 0.4949564 0.4503938 10.475746 -11.407858
 [4,] 0.5639081 0.5110484  6.949751 -13.348352
 [5,] 0.6065118 0.5450293  5.535356 -13.617242
 [6,] 0.6431643 0.5740993  4.597915 -13.695126
 [7,] 0.6752408 0.5994637  4.027199 -13.636825
 [8,] 0.7002528 0.6175640  4.022661 -13.044730
 [9,] 0.7277954 0.6403011  3.815311 -13.069793
[10,] 0.7476471 0.6541830  4.224341 -12.309765
[11,] 0.7704721 0.6733641  4.395073 -12.274731
[12,] 0.7887441 0.6873413  4.930695 -11.789420
[13,] 0.8052483 0.6997579  5.607997 -11.242937
[14,] 0.8243232 0.7173895  6.079278 -11.522366
[15,] 0.8390781 0.7293587  6.896774 -11.218400
[16,] 0.8480888 0.7323470  8.174628  -9.770491
[17,] 0.8541529 0.7301828  9.688637  -7.680911
[18,] 0.8622459 0.7317420 11.040037  -6.212698
[19,] 0.8650317 0.7225651 12.816778  -3.351451
[20,] 0.8734998 0.7246761 14.138115  -2.176127
[21,] 0.8767289 0.7149357 15.879323   0.478852
[22,] 0.8808466 0.7060882 17.549323   2.825437
[23,] 0.8826712 0.6899168 19.403089   5.876607
[24,] 0.8845333 0.6713640 21.253858   8.906280
[25,] 0.8871069 0.6519129 23.047602  11.687318
[26,] 0.8875835 0.6218719 25.009402  15.164124
[27,] 0.8876355 0.5842512 27.005241  18.784154
[28,] 0.8877008 0.5383257 29.000000  22.399619'
# Modell 21 sieht gut aus, hohes r quadrat aber kleines BIC

# Modell 21 auswählen (Position 21 in der Liste)
coef(fit, 21)  # Zeigt Koeffizienten

# Namen der Variablen extrahieren
vars_21 <- names(coef(fit, 21))[-1]  # ohne Intercept
formel_21 <- as.formula(paste("log(soil.res) ~", paste(vars_21, collapse = " + ")))

# Anzeigen
formel_21
'log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) + soiln5 + 
    I(soiln5^2) + soilc0 + I(soilc0^2) + soilc5 + I(soilc5^2) + 
    I(sno35^2) + I(litdoc^2) + rootdw0 + I(rootdw0^2) + rootdw5 + 
    I(rootdw5^2) + temp.0 + I(temp.0^2) + temp.15 + I(temp.15^2) + 
    rootc5'

best_model <- lm(log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) + soiln5 + 
    I(soiln5^2) + soilc0 + I(soilc0^2) + soilc5 + I(soilc5^2) + 
    I(sno35^2) + I(litdoc^2) + rootdw0 + I(rootdw0^2) + rootdw5 + 
    I(rootdw5^2) + temp.0 + I(temp.0^2) + temp.15 + I(temp.15^2) + 
    rootc5, data = hainich.data)

summary(best_model)
plot(best_model)

# alle Variablen, größer Korrelation 0.2 da kommt nur noch sdoc5 dazu
fit2 <- regsubsets(log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) +litter.d + I(litter.d^2)+ soiln0+ I(soiln0^2) + soiln5 + 
                  + I(soiln5^2) + soilc0 + I(soilc0^2) + soilc5 + I(soilc5^2) + sno35 + I(sno35^2) + litdoc + I(litdoc^2) + sdoc5 + I(sdoc5^2)
                  + rootdw0 + I(rootdw0^2) + rootdw5 + I(rootdw5^2) + temp.0 + I(temp.0^2) + temp.15 + I(temp.15^2) + rootc5 + I(rootc5^2), 
                  data = hainich.data, 
                  nvmax = 32)  # max 12 Prädiktoren + Intercept

# Übersichtstabelle
sum_fit2 <- summary(fit2)
sum_fit2$which    # TRUE/FALSE-Matrix: welche Variable in welchem Modell

# Kriterien anschauen
cbind(
  Rsq = sum_fit2$rsq,
  AdjRsq = sum_fit2$adjr2, 
  Cp = sum_fit2$cp,
  BIC = sum_fit2$bic
)

# Modell 21 auswählen (Position 21 in der Liste)
coef(fit, 21)  # Zeigt Koeffizienten

# Namen der Variablen extrahieren
vars_21 <- names(coef(fit, 21))[-1]  # ohne Intercept
formel_21 <- as.formula(paste("log(soil.res) ~", paste(vars_21, collapse = " + ")))

# Anzeigen
formel_21

'log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) + soiln5 + 
    I(soiln5^2) + soilc0 + I(soilc0^2) + soilc5 + I(soilc5^2) + 
    I(sno35^2) + I(litdoc^2) + rootdw0 + I(rootdw0^2) + rootdw5 + 
    I(rootdw5^2) + temp.0 + I(temp.0^2) + temp.15 + I(temp.15^2) + 
    rootc5'

#keine Änderung in der Formel
#ich habe mir die summary angeschaut und alle Variablen die nicht signifikant waren aus dem Modell genommen
#damit ist dieses Modell entstanden
#der code ist nochmal im notebook a separate notebook damit man die Plots besser anschauen kann

best_model <- lm(log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) + soilc0 + I(soilc0^2) + I(litdoc^2)
                 + rootdw0 + I(rootdw0^2) + rootc5, data = hainich.data)

summary(best_model)
plot(best_model)

Call:
lm(formula = log(soil.res) ~ smoi + I(smoi^2) + lmoi + I(lmoi^2) + 
    soilc0 + I(soilc0^2) + I(litdoc^2) + rootdw0 + I(rootdw0^2) + 
    rootc5, data = hainich.data)

'Residuals:
      Min        1Q    Median        3Q       Max 
-0.234111 -0.118037 -0.006095  0.117311  0.297293 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)   
(Intercept)   9.242e+00  3.962e+00   2.333  0.02736 * 
smoi         -2.331e-01  1.079e-01  -2.161  0.03973 * 
I(smoi^2)     3.175e-03  1.480e-03   2.146  0.04101 * 
lmoi         -1.835e+01  9.741e+00  -1.884  0.07038 . 
I(lmoi^2)     1.741e+01  8.287e+00   2.101  0.04511 * 
soilc0        5.322e-02  2.061e-02   2.583  0.01555 * 
I(soilc0^2)  -3.164e-04  1.479e-04  -2.139  0.04166 * 
I(litdoc^2)   5.001e-01  1.585e-01   3.156  0.00391 **
rootdw0      -8.557e+00  3.197e+00  -2.677  0.01248 * 
I(rootdw0^2)  5.727e+01  1.764e+01   3.246  0.00312 **
rootc5       -3.409e-02  1.396e-02  -2.443  0.02140 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1814 on 27 degrees of freedom
Multiple R-squared:  0.7272,	Adjusted R-squared:  0.6262 
F-statistic: 7.199 on 10 and 27 DF,  p-value: 2.03e-05'