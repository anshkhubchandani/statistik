r	Bedeutung
0	kein linearer Zusammenhang
0.1–0.3	schwach
0.3–0.5	moderat
0.5–0.7	relativ stark
>0.7	stark
______________________________

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
# Sreuschicht Nährstoffe

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

# Boden Nährstoffe

## Stickstoffgehalt des Bodens in 0-5 cm Tiefe"
cor(y, hainich.data$soiln0)
0.326089433400278 ->  schwach bis moderat
höherer Stickstoffgehalt → etwas höhere Bodenatmung
Im Plot sieht man:
	•	steigende Regressionslinie
	•	moderate Streuung


## Stickstoffgehalt des Bodens in 5-10 cm Tiefe
cor(y, hainich.data$soiln5)
0.269114779377351 -> etwas schwächer als bei 0.5 cm

-> Mehr Stickstoff kann bedeuten:
	•	bessere Nährstoffversorgung der Mikroben
	•	schnellere Zersetzung organischer Substanz
	•	mehr CO2-Produktion

## Kohlenstoffgehalt des Bodens in 0-5 cm Tiefe
cor(y, hainich.data$soilc0)
0.229798870427361 -> schwach 
mehr Kohlenstoff → etwas mehr Bodenatmung
theorie sagt: Kohlenstoff ist Energiequelle für Mikroben.


## Kohlenstoffgehalt des Bodens in 5-10 cm Tiefe
cor(y, hainich.data$soilc5)
0.21385157685631 -> leicht schwächer als bei 0-5 cm

hier Stickstoff im Oberboden zeigt den stärksten Zusammenhang.
		mehr C → mehr Energie
    	mehr N → schnelleres Wachstum
-> mehr CO₂-Freisetzung.
Ich würde beide fürs modell nutzen

---------------------------------------------------------------------


# Nitrat , Amonium , DOC( dissolved organic carbon) der Streuschicht bzw. des Bodens 

## Nitratgehalt der Streuschicht in 0-5 cm Tiefe
cor(y, hainich.data$litno3)
0.162947191275231 ->  schwach 
		mehr Nitrat → eeetwas höhere Bodenatmung

## Nitratgehalt des Bodens in 0-5 cm bzw. Tiefe
cor(y, hainich.data$sno30)
-0.00118291180005637 -> kein zsmhang - plot zeigt auch fast horizontale linie


## Nitratgehalt des Bodens in 5-10 cm Tiefe
cor(y, hainich.data$sno35)
-0.268060994648253 -> schwach neg
Wenn viel Nitrat vorhanden ist, kann das bedeuten:
	weniger aktive mikrobielle Immobilisierung
	weniger Zersetzung ?
-> Nitrat -> schwacher effekr

----- 
## Ammoniumgehalt der Streuschicht in 0-5 cm Tiefe
cor(y, hainich.data$litnh4)
0.192512251509065 -> schwach 
Mehr Ammonium kann bedeuten:
	höhere mikrobielle Aktivität

## Ammoniumgehalt des Bodens in 0-5 cm bzw. Tiefe
cor(y, hainich.data$snh40)
0.0715219572819145 -> kein zshm


## Ammoniumgehalt des Bodens in 5-10 cm Tiefe
cor(y, hainich.data$snh45)
-0.00171113429793457 > kein zshm fast horizontale linie

-> 
---
## DOC der Streuschicht in 0-5 cm Tiefe wichtigste
cor(y, hainich.data$litdoc)
0.275734616548858 -> schwach bis moderat (plot zeigt steigende linie)
mehr DOC → mehr Bodenatmung

## DOC des Bodens in 0-5 cm bzw. Tiefe
cor(y, hainich.data$sdoc0)
0.155185168823793 -> schwach


## DOC  des Bodens in 5-10 cm Tiefe
cor(y, hainich.data$sdoc5)
0.202014792609833 -> schwach, aber besser al bei 0-5 cm


-> DOC ist sehr wichtig biologisch, weil es direkt verfügbarer Kohlenstoff für Mikroben ist.

-> litdoc zeigt das stärkste effekt mitnehemn





#----------PH Wert---------------------------
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


#-----Würzeln-----------------------

##Trockenmasse der Wurzeln in 0-5 cm
cor(y, hainich.data$rootdw0)
0.28532376568652 -> schwach bis moderat positiv
- mehr Wurzelbiomasse → etwas höhere Bodenatmung
(feine wurzel hier wahrscheinlich)korreliert besser als bei 5.10 cm (1)

##Trockenmasse der Wurzeln in 5-10 cm
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
0.133312884879414 -> sehr schwach, kaum effekt

## Kohlenstoffgehalt der Wurzeln in 5-10 cm Tiefe
cor(y, hainich.data$rootc5) #neg
-0.23002382865889 -> schwacher negativ effekr

-> Also: mehr Wurzelbiomasse → mehr Bodenatmung
N-C-Konzentration spielt kaum Rolle
Wurzelbiomasse (vor allem an der oberfläche) ist der wichtigste Faktor 


#nn – Entfernung der Probestelle zum nächsten Baum (nearest neighbour) 
cor(y, hainich.data$nn)
-0.151853113442641 -> schwach neg

-> Distanz zum Baum erklärt nur einen kleinen Teil der Variation der Bodenatmung.


(1)https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.70326#:~:text=Approximately%2040%25%20of%20fine-root,soil%20layers%20through%20mineral%20weathering.