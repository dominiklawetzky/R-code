##### Preamble -----

# Environment leeren

#rm(list = ls())

#setwd("/Volumes/GoogleDrive/Meine Ablage/+++ Universität/Psych/PsyBSc7_Statistik II/Quiz")

# Paket-Namen speichern
packages <- c("ggplot2", "readxl", "MASS", "dplyr", "multcomp", "tidyr", "lm.beta", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "interactions", "rococo", "shiny", "ez")

# Nicht-installierte Pakete installieren
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Pakete versteckt laden
invisible(lapply(packages, library, character.only = TRUE))

# Daten laden
load(url("https://pandar.netlify.app/post/conspiracy.rda"))




##### ANOVA I -----

## AUFGABE 1.1

conspiracy$id <- as.factor(1:nrow(conspiracy))

anova <- ezANOVA(conspiracy, 
                 wid = id, 
                 dv = CI, 
                 between = edu, 
                 detailed = TRUE, 
                 return_aov = TRUE)

residuals <- residuals(anova$aov)
qqPlot(residuals) # Hinweis: Auf x-Achse achten



## AUFGABE 1.2

shapiro.test(residuals) # signifikante Abweichung von Normalverteilungsannahme



## AUFGABE 1.3

# Wichtiger als die Voraussetzung der Normalverteilung der Residuen sowie deren Homoskedastizität
# ist die Unabhängigkeit der Stichprobe; dies wird durch randomisierte Zuweisung sichergestellt.



## AUFGABE 1.4

# Möglichkeit 1 (siehe Z. 33)
ezANOVA(conspiracy, wid = id, dv = CI, between = edu, detailed = TRUE, return_aov = TRUE)

# Möglichkeit 2
leveneTest(conspiracy$CI ~ conspiracy$edu)

# p-Wert = 0.001



## AUFGABE 1.5

ezANOVA(conspiracy, 
        wid = id, 
        dv = CI, 
        between = edu, 
        detailed = TRUE, 
        return_aov = TRUE, 
        white.adjust = TRUE)$ANOVA

# F-Statistik = 20.2560



## AUFGABE 1.6

ezANOVA(conspiracy, 
        wid = id, 
        dv = CI, 
        between = edu, 
        detailed = TRUE, 
        return_aov = TRUE, 
        white.adjust = FALSE)$ANOVA

# Es liegt ein kleiner Effekt (~ 0.016) vor, der allerdings signifikant ist



# AUFGABE 1.6

TukeyHSD(aov(CI ~ edu, data = conspiracy), conf.level = 0.95)


##### ANOVA II -----

## VORBEREITUNG

load(url("https://pandar.netlify.app/post/nature.rda"))
str(nature)

nature$ID <- as.factor(1:nrow(nature)) # Personen-ID hinzufügen

nature$NV <- apply(nature[, 1:6], 1, mean) # Skalenwerte bilden



## AUFGABE 1

ezStats(nature, dv = NV, wid = ID, between = c(urban, continent))

ezANOVA(nature, dv = NV, wid = ID, between = c(urban, continent))

# Zwei unabhängige Variablen: urban / continent
# Drei Effekte: zwei Haupteffekte und ein Interaktionseffekt
# Insgesamt sechs Gruppen: 3*2=6
# Mittelwerte lassen bedeutsame Unterschiede vermuten
# Design ist nicht balanciert



## AUFGABE 2

anova1 <- ezANOVA(nature, 
                  dv = NV, 
                  wid = ID, 
                  between = c(urban, continent), 
                  detailed = TRUE,
                  return_aov = TRUE)

# Determinationkoeffizient kann aus ges abgelesen werden oder manuell:

R_squared <- 5.165871 / (5.165871 + 396.1437)
R_squared

anova2 <- ezANOVA(nature, 
                  dv = NV, 
                  wid = ID, 
                  between = c(urban, continent), 
                  detailed = TRUE,
                  type = 3,
                  return_aov = TRUE)



## AUFGABE 3

library(emmeans)

anova3 <- ezANOVA(nature, 
                  dv = NV, 
                  wid = ID, 
                  between = c(urban, continent), 
                  detailed = TRUE,
                  type = 2,
                  return_aov = TRUE)


TukeyHSD(anova3$aov)

emm1 <- emmeans(anova3$aov, ~ urban * continent)

cont1 <- c(-.5, .25, .25, -.5, .25, .25)
contrast(emm1, list(cont1), adjust = 'bonferroni')

cont2 <- c(0, 0, 1, 0, 0, -1)
contrast(emm1, list(cont1, cont2), adjust = 'bonferroni')
