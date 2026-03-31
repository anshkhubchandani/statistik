# Statistik Projekt: Bodenatmung

Dieses Repository enthält den vollständigen Quellcode, die Daten sowie das LaTeX-Dokument zur Projektarbeit im Modul Statistik.

## Überblick

Im Rahmen dieser Arbeit wurde die Bodenatmung anhand gegebener Datensätze statistisch analysiert. Ziel war es, geeignete Modelle zu entwickeln, zu vergleichen und die Ergebnisse zu interpretieren.

Die Analyse umfasst:

- Datenaufbereitung
- Modellbildung und -selektion
- Untersuchung von Interaktionen
- Simulationen
- Visualisierung der Ergebnisse

---

##  Repository-Struktur
```
.
├── abgabe_code/        # Finale R-Skripte für Analyse und Modelle
├── latex/              # LaTeX-Dokument der Arbeit
│   ├── Text/           # Kapitel der Arbeit
│   ├── pictures/       # Abbildungen
│   └── Script.tex      # Hauptdokument
├── literatur/          # Literatur und Notizen
├── *.ipynb             # Jupyter Notebooks zur Exploration
├── *.r                 # Weitere Analyse-Skripte
├── hainich.csv         # Datensatz
└── Script.pdf          # Finale Abgabe (PDF)
```
---

## Verwendete wurde
- **R** (statistische Analyse, Modellierung)
- **LaTeX** (Dokumentation der Arbeit)
- **Jupyter Notebook** (explorative Analyse)
---

## Verwendung
### R-Skripte ausführen
Die zentralen Analysen befinden sich im Ordner: 
abgabe_code/

Diese können direkt in R oder RStudio ausgeführt werden.

---

### LaTeX-Dokument kompilieren
```bash
pdflatex Script.tex
biber Script
pdflatex Script.tex
pdflatex Script.tex
```
---

## Ergebnisse

Die finalen Ergebnisse der Arbeit sind im PDF-Dokument enthalten:
`Script.pdf`

---

## Autoren
- Team member  
- Anshul Khubchandani  
- Team member  
- Team member  

Friedrich-Schiller-Universität Jena

---

## Hinweis

- Temporäre LaTeX-Dateien (`.aux`, `.log`, `.out`, etc.) können ignoriert werden.
- Das Repository dient der Reproduzierbarkeit der Ergebnisse.

Dieses Repository wurde im Rahmen einer universitären Projektarbeit erstellt und dient ausschließlich zu wissenschaftlichen Zwecken.