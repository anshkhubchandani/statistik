r	Bedeutung
0	kein linearer Zusammenhang
0.1–0.3	schwach
0.3–0.5	moderat
0.5–0.7	relativ stark
> 0.7	stark
_______________________________________

## Bodenfeuchte smoi -> wichtig
cor(y, hainich.data$smoi)
-0.2590309745763 -> schwacher Zusammenhang, nicht lineare beziehung -> durch eine parabel nach oben beschrieben U I(smoi^2)

	zu trocken → wenig Aktivität
	•	optimal → viel Aktivität
	•	zu nass → wenig Aktivit

## litter Streufeuchte lmoi -> wichtigste
cor(y, hainich.data$lmoi)
0.47415944921147 -> moderat

Höhere Streufeuchte → höhere Bodenatmung.

-------------------------

## Dicke der Sreuschicht -> wichtig
cor(y, hainich.data$litter.d)
0.306588646363252 -> leicht positiver trend
	•	dickere Streuschicht → tendenziell mehr Bodenatmung

## Stickstoff in Streu litter.n
cor(y, hainich.data$litter.n) #neg korrelation
-0.0177449928915829 -> kein lin zusammenhang


## Kohlenstoff in Streu litter.c
cor(y, hainich.data$litter.c)
0.138910847972551 -> sehr schwach, kaum zusammenhangFast alle Streu enthält viel Kohlenstoff.→ C ist selten limitierend.


# ----------PH Wert---------------------------
## ph.litter
cor(y, hainich.data$ph.litter) #negative korrelation
-0.0456583095568331 -> kein zusammenhang
Der ph der Streuschicht erklärt die Bodenatmung nicht.

## ph soil 0 cm  
cor(y, hainich.data$ph.soil0) # pos
0.151659292437412 -> schwach positiver Zusammenhang
	•	etwas höherer pH → leicht höhere Bodenatmung
	•	aber Effekt ist sehr klein 

## ph soil 5 cm tiefe
cor(y, hainich.data$ph.soil5) #negative korrelation
-0.182685383508701 -> schwach negativer zusammenhang
p-wert =  0.2723 -> nicht signifikant

Mikroorganismen reagieren stark auf pH nur wenn der pH extrem ist.
ph wert ist wichtig aber variiert hier im Datensatz zu wenig, um einen Effekt zu zeigen.


# -----Würzeln-----------------------

## Trockenmasse der Wurzeln in 0-5 cm
cor(y, hainich.data$rootdw0)
0.28532376568652 -> schwach bis moderat positiv
- mehr Wurzelbiomasse → etwas höhere Bodenatmung
(feine wurzel hier wahrscheinlich)korreliert besser als bei 5.10 cm (1)

## Trockenmasse der Wurzeln in 5-10 cm
cor(y, hainich.data$rootdw5)
0.246419362524827 -> schwach positiv aber schäche als wurzel an der oberfläche
Die meisten feinen Wurzeln liegen in den oberen Bodenschichten. (1)

------
## Stickstoffgehalt der Wurzeln in 0-5 cm Tiefe 
cor(y, hainich.data$rootn0) 
-0.146904693668116 -> kein klarer zusammenhang 
plot zeigt leicht fallende linie

## Stickstoffgehalt der Wurzeln in 5-10 cm Tiefe
cor(y, hainich.data$rootn5) #pos, höher
0.0387472496903867 -> praktisch kein zusammenhang

-> Stickstoffgehalt der Wurzeln beeinflusst hier die Bodenatmung kaum. (2)
------

## Kohlenstoffgehalt der Wurzeln in 0-5 cm Tiefe
cor(y, hainich.data$rootc0)
0.133312884879414

## Kohlenstoffgehalt der Wurzeln in 5-10 cm Tiefe
cor(y, hainich.data$rootc5) #neg
-0.23002382865889





(1)https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.70326#:~:text=Approximately%2040%25%20of%20fine-root,soil%20layers%20through%20mineral%20weathering.
(2) nachschauen ob das mit dem Stickstoff stimmt
(3)




"Dominanz der Kohlenstoffmenge: Studien zeigen, dass der gesamte unterirdische Kohlenstoffvorrat (in Wurzeln und Oberboden) der stärkste Treiber 
für die Bodenatmung ist. Die Menge an "Futter" für Mikroben ist entscheidender als dessen Nährstoffqualität." nachschauen