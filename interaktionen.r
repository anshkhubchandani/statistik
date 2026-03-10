# Interaktionen, so wenig wie möglich, das gilt auch für die Variablen, da wir auch wenig Messpunkte
# haben und damit schnell Overfitting passiert

# Werte: Anova, signifikanz, AIC

# variablen im Model: lmoi + litter.d + soilc0 + rootdw0 + temp.15
# relative Feuchte der Streuschicht
# Dicke der Streuschicht
# Kohlenstoffgehalt des Bodens in 0-5 cm Tiefe
# Trockenmasse der Wurzeln in 0-5 cm Tiefe
# Bodentemperatur in 15 cm Tiefe

hainich.data <- read.csv("hainich.csv", sep=";")

# Feuchte hängt von dicke der Streuschicht ab, dünne trocknen schneller aus und können weniger Wasser speichern
best_model <- lm(log(soil.res) ~ I(lmoi^2) + I(litter.d^2) + soilc0 + I(soilc0^2) + 
    I(rootdw0^2) + I(temp.15^2), data = hainich.data)

best_model_interaktion <- lm(log(soil.res) ~ I(lmoi^2) * I(litter.d^2) + soilc0 + I(soilc0^2) + 
    I(rootdw0^2) + I(temp.15^2), data = hainich.data)
summary(best_model_interaktion)
# Interaktion nicht signifikant

AIC(best_model, best_model_interaktion)
# AIC auch nicht besser

######################################
# Interaktion zwischen Temperatur und Feuchte.

# Mikroorganismen drosseln bei extremer Trockenheit trotz hoher Temperaturen ihre Aktivität
# In trockenen Perioden (wie im August 2001 beobachtet) bricht die normale
# Temperatur-Respirations-Kurve ein. Das Modell für Bodenatmung wird signifikant besser,
# wenn sowohl Temperatur als auch Feuchte einbezogen werden

best_model_interaktion2 <- lm(log(soil.res) ~ I(lmoi^2) *  I(temp.15^2) + soilc0 + I(soilc0^2) + 
    I(rootdw0^2) + I(litter.d^2), data = hainich.data)
summary(best_model_interaktion2)
# Interaktion nicht signifikant

AIC(best_model, best_model_interaktion2)
# AIC auch nicht besser

######################################
# Interaktion zwischen Bodentemperatur und Trockenmasse der Wurzeln

# Die Temperatur-Sensitivität (Q10​) unterscheidet sich zwischen der autotrophen Respiration (Wurzeln) und der heterotrophen Respiration (Mikroorganismen)
# Wurzelrespiration macht oft mehr als die Hälfte der gesamten Bodenatmung aus. Da Wurzeln und
# Mikroorganismen unterschiedliche physiologische Optima haben, hängt die Gesamtreaktion des Bodens auf
# Erwärmung davon ab, wie viel Wurzelbiomasse vorhanden ist

best_model_interaktion3 <- lm(log(soil.res) ~ I(lmoi^2) + I(temp.15^2) * I(rootdw0^2) + soilc0 +
    I(soilc0^2) + I(litter.d^2), data = hainich.data)
summary(best_model_interaktion3)
# Interaktion nicht signifikant

AIC(best_model, best_model_interaktion3)
# AIC auch nicht besser, schlechteste Interaktion bisher

######################################
# Interaktion Kohlenstoffgehalt und Wurzelmasse
# Räumliche Muster der Bodenatmung werden oft gemeinsam durch die Verteilung von Pflanzen (Wurzeln) und Bodenressourcen (C) erklärt
# Das „beste Modell“ in der Publikation kombiniert explizit Wurzelbiomasse, Boden-C und Feuchte.
# Diese Faktoren korrelieren oft räumlich miteinander, da Bäume über den Wurzeleintrag den Bodenkohlenstoff beeinflussen

best_model_interaktion4 <- lm(log(soil.res) ~ I(lmoi^2) + I(temp.15^2) + I(rootdw0^2) * I(soilc0^2) +
    soilc0 + I(litter.d^2), data = hainich.data)
summary(best_model_interaktion4)
# Interaktion nicht signifikant

AIC(best_model, best_model_interaktion4)
# AIC auch nicht besser

##############################################

# Interaktion Feuchte und Kohlenstoffgehalt
# Die Verfügbarkeit von Substrat (Kohlenstoff) für den Abbau wird durch die Feuchtigkeit limitiert oder gefördert.
# Ein sehr hoher Wassergehalt (besonders in tonreichen Böden wie im Hainich) kann die
# Sauerstoffdiffusion hemmen und so die mikrobielle Aktivität trotz vorhandenen Kohlenstoffs
# einschränken. Zudem sind Streufeuchte (lmoi) und Streudicke (litter.d) zentrale Parameter für die
# räumliche Variabilität

best_model_interaktion5 <- lm(log(soil.res) ~ I(lmoi^2) * soilc0 +
    I(soilc0^2) + I(temp.15^2) + I(rootdw0^2) + I(litter.d^2), data = hainich.data)
summary(best_model_interaktion5)
# Interaktion nicht signifikant

AIC(best_model, best_model_interaktion5)
# Modell besser
'                        df      AIC
best_model               8 -8.691496
best_model_interaktion5  9 -8.951633'

BIC(best_model, best_model_interaktion5)
# BIC ist schlechter

anova(best_model, best_model_interaktion5)
# Interaktion liefert keine signifikante Verbesserung