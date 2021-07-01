##### PREAMBLE -----

## ENVIRONMENT LEEREN / WORKING DIRECTORY SETZEN

#rm(list = ls())

#setwd("/Volumes/GoogleDrive/Meine Ablage/+++ Universität/Psych/PsyBSc7_Statistik II/Quiz")



## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "multcomp", "tidyr", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny", "gvlma", "emmeans", "ez")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))


## DATEN LADEN
load(url("https://pandar.netlify.app/post/alc.rda"))




##### Aufgabe 1 -----

# Daten ergänzen
alc$alcuse.17 <- alc$alcuse.16*0.95

# Übersicht über die Variablen
head(alc)

# Umwandlung in Long-Format
alc_long <- reshape(alc, varying = 5:8, timevar = "age", direction = "long")

# Age-Variable zu Faktor umwandeln
alc_long$age <- as.factor(alc_long$age)

# Ausgabe des gesuchten Werts
round(alc_long[17, 3], 3)




##### Aufgabe 2 -----

# Graphische Darstellung des Mittelwerteverlaufs
ezPlot(alc_long, dv = alcuse, wid = id, within = age, x = age)

# Deskriptivstatistiken zum Mittelwerteverlauf
ezStats(alc_long, dv = alcuse, wid = id, within = age)

# Ausgabe der gesuchten Werte
round(ezStats(alc_long, dv = alcuse, wid = id, within = age)[4, c("Mean", "SD")], 3)




##### Aufgabe 3 -----

# Vollständige Within-Subjects-ANOVA
ezANOVA(alc_long, dv = alcuse, wid = id, within = age)




##### Aufgabe 4 -----

# Within-Subjects-ANOVA mit aov-Funktion
wdh_aov <- aov(alcuse ~ age + Error(id/age), 
               data = alc_long)

# Anwendung von emmeans
em <- emmeans(wdh_aov, ~ age)

# Verlaufsprüfung mittels Kontrasten
contrast(em, interaction = 'poly', p.adjust = 'bonferroni')

# Interpretation: Es liegt ein bedeutsam quadratischer Trend vor.




##### Aufgabe 4 -----

# Ausgabe eines Plots orientiert an der Umsetzung im Pandar-Skript
ezPlot(alc_long, dv = alcuse, wid = id, within = age, x = age) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE,
              formula = y ~ x + I(x^2), color = 'red') +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE,
              formula = y ~ x + I(x^3), color = 'yellow')


##### Aufgabe 5  -----

# Datensatz-Ergänzung
alc$treat <- c(0,1)
alc$alcuse.18 <- with(alc, ifelse(treat == 0, alcuse.17*1, alcuse.17*0.90))

# Datensatz auf relevante Messzeitpunkte verkleinern
alc_small <- subset(alc, select = c("id", "male", "treat", "alcuse.17", "alcuse.18"))

# Erneute Umwandlung in Long-Format
alc_small_long <- reshape(alc_small, varying = 4:5, timevar = "age", direction = "long")

# Age-/Treatment-Variable zu Faktor umwandeln
alc_small_long$age <- as.factor(alc_small_long$age)
alc_small_long$treat <- as.factor(alc_small_long$treat)

# Ausgabe eines Split-Plots
ezPlot(alc_small_long, dv = alcuse, wid = id, within = age, x = age, between = treat, split = treat)

# Durchführung der Mixed-Design-ANOVA
ezANOVA(alc_small_long, dv = alcuse, wid = id, within = age, between = treat)



