##### Info -----
# Dominik Lawetzky
# Bearbeitet am 03.06.2021
# Alle Ansätze und Umsetzungen ohne Gewähr ;)

##### Preamble -----

## ENVIRONMENT LEEREN / WORKING DIRECTORY SETZEN

# rm(list = ls())

# setwd("/Users/dominiklawetzky/Desktop")



## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "multcomp", "tidyr", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny", "gvlma")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))

library(report)



## DATENSATZ LADEN
load(url("https://pandar.netlify.app/post/Interaction.rda"))

##### TEIL 1 - Aufgabe 1 -----

head(Interaction)

## TEIL I

mod <- lm(Y ~ X1 + X2 + X1:X2,
          data = Interaction)

summary(mod)

# Interaktion ist signifikant


## TEIL II

mod_quad <- lm(Y ~ poly(X1, 2) + poly(X2, 2) + X1:X2,
          data = Interaction)

summary(mod_quad)

# Interaktion nach Aufnahme der quadratischen Terme nicht mehr signifikant


## TEIL III

int <- Interaction$X1 * Interaction$X2
X2_quad <- Interaction$X2^2

cor.test(int, X2_quad)

# Korrelation hochsignifkant
# r = 0.7123


## TEIL IV

cor.test(int, Interaction$X2)

# Korrelation hochsignifikant
# r = 0.6295


##### TEIL 1 - Aufgabe 2 -----

## TEIL I

Interaction_scale <- data.frame(scale(Interaction))

head(Interaction)

mod_scale <- lm(Y ~ X1 + X2 + X1:X2,
                data = Interaction_scale)

summary(mod_scale)

# Interation ist signifikant


## TEIL II

mod_scale_quad <- lm(Y ~ poly(X1, 2) + poly(X2, 2) + X1:X2,
                     data = Interaction_scale)

summary(mod_scale_quad)

# Interaktion nach Aufnahme der quadratischen Terme nicht mehr signifikant


# TEIL III

int_scale <- Interaction_scale$X1 * Interaction_scale$X2

X2_scale_quad <- Interaction_scale$X2^2

cor.test(int_scale, X2_scale_quad)

# Korrelation hochsignifikant
# r = 0.6658
# Deutung: Das Polynom 2. Grades vom Regressor X2 ist 
# bedeutsamer Teil des quadratischen Interaktionsterms.
# Dementsprechend ist hier immer von einer hochsignifikanten 
# Korrelation auszugehen.


## TEIL IV

cor.test(int_scale, Interaction_scale$X2)

# Korrelation keineswegs signifikant
# r = -0.0184
# Deutung: kein lin. Zshg. zw. quad. und lin. Term

##### TEIL 1 - Aufgabe 3 -----

# Korrelationen verändern sich nicht durch Standardisierung
# Erklärung: bei Standardisierung wird SD = 1, 
# damit ist der Zähler beim Korrelationskoeffizienten 1,
# eine erneute Division durch die SD hat also keinen Effekt

# Durch Aufnahme des quadratischen Terms wird die Interaktion nie
# signifikant, da die quadratischen Terme mit der Interaktion 
# hochsingifikant korrelieren -> Multikollinearität
# Lösung: Zentrierung zum Mittelwert (siehe Aufgabe 2, TEIL III)

# Drastische Unterschiede bei linearen Effekten zw. Interaktionsterm
# und vollem Modell nur bei UNZENTRIERTEN Daten





##### TEIL 2 - Aufgabe 0 -----

## DATEN LADEN
confirmed_raw = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths_raw = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
missings = which(apply(confirmed_raw[,1:4],1,function(x) any(is.na(x)))) # find out, if any meta data are not complete
confirmed = confirmed_raw[-missings,] # remove incomplete data
confirmed_long = reshape(confirmed,
                         varying = names(confirmed)[-c(1:4)],
                         v.names = 'Confirmed',
                         timevar = 'Day',
                         idvar = names(confirmed)[1:4],
                         direction = 'long', ids = 1:prod(dim(confirmed)))

missings = which(apply(deaths_raw[,1:4],1,function(x) any(is.na(x)))) # find out, if any meta data are not complete
deaths = deaths_raw[-missings,] # remove incomplete data
deaths_long = reshape(deaths,
                      varying = names(deaths)[-c(1:4)],
                      v.names = 'Deaths',
                      timevar = 'Day',
                      idvar = names(deaths)[1:4],
                      direction = 'long')

long = merge(confirmed_long, deaths_long,
             by = c('Province.State', 'Country.Region', 'Lat', 'Long', 'Day'))

covid = aggregate(cbind(Confirmed, Deaths) ~ Country.Region + Day, data = long, FUN = 'sum')

covid_full = covid
covid = covid[covid$Day < 80, ]

covid_sel = covid[covid$Country.Region %in% c('France', 'Germany', 'Italy', 'Spain', 'United Kingdom'), ]

## TEIL I

head(covid)

names(confirmed)
# 79+4 Spalte beinhaltet die Daten für den 79. Tag

colnames(confirmed)[83]
# Achtung: Formatierung des Datums in TT.MM.JJJJ beachten


## TEIL II

# Ablesen aus Zeile 182 ODER:
nlevels(as.factor(covid_sel$Country.Region))


## TEIL III

covid_germany <- subset(covid_sel, Country.Region == "Germany", Confirmed)

# Letzer Wert entspricht dem 79. Tag (siehe Zeile 180)
covid_germany[79,]


##### TEIL 2 - Aufgabe 1a -----

covid_germany$Day <- 1:79

ggplot(covid_germany, aes(y = Confirmed, x = Day)) +
  geom_point()

# Bei Vergleich die y-Achse beachten, endet hier bei 120000

##### TEIL 2 - Aufgabe 1b -----

covid_germany$log_Confirmed <- log(covid_germany$Confirmed)
covid_germany$log_Confirmed[covid_germany$log_Confirmed == -Inf] <- NA

ggplot(covid_germany, aes(y = log_Confirmed, x = Day)) +
  geom_line(size = 2)

# Tipp 1: Hier ist die Form der Funktion entscheidend
# Tipp 2: Skalierung der y-Achsen ebenfalls zum Ausschluss geeignet



##### TEIL 2 - Aufgabe 2a -----

covid_germany$Confirmed[covid_germany$Confirmed == 0] <- NA

covid_germany <- na.omit(covid_germany)



## TEIL I

mod_covid <- lm(Confirmed ~ Day,
                data = covid_germany)
summary(mod_covid)

# Determinationskoeffizient = 0.6083 bzw. 60.83 %


mod_covid_quad <- lm(Confirmed ~ poly(Day, 2),
                     data = covid_germany)
summary(mod_covid_quad)

# Determinationskoeffizient = 0.9356 bzw. 90.56 %



## TEIL II

anova(mod_covid, mod_covid_quad)

# Varianzinkrement ist statistisch bedeutsam
# F-Wert = 360.54



## TEIL III

covid_germany_pred <- predict(mod_covid_quad)

ggplot(covid_germany, aes(x = Day, y = Confirmed)) +
  geom_point(stat = "identity") +
  geom_line(aes(y = covid_germany_pred))

# Quadratisches Modell bildet Geschehn nicht sinnvoll ab 
# Denn: negative Fälle zw. ca. Tag 17 und Tag 45
# Alternative: exponentieller Verlauf


##### TEIL 2 - Aufgabe 2b -----

## LOGARITHMISCHES MODELL

mod_covid_log <- lm(log_Confirmed ~ Day,
                    data = covid_germany)
summary(mod_covid_log)

# Determinationskoeffizient = 0.9449 bzw. 94.49 %
# Dies ist knapp 4,5 % mehr als das quadratische Modell.



## MODELLVERGLEICH

anova(mod_covid_quad, mod_covid_log)

# Signifikanzprüfung NICHT möglich, da die AV unterschiedlich sind



## GRAPHISCHE ÜBERPRÜFUNG

covid_germany_log_pred <- predict(mod_covid_log)

ggplot(covid_germany, aes(x = Day, y = log_Confirmed)) +
  geom_point(stat = "identity") +
  geom_line(aes(y = covid_germany_log_pred), color = "blue")


##### TEIL 2 - Aufgabe 3 -----

## QUADRATISCH-EXPONENTIELLES MODELL

mod_covid_log_quad <- lm(log_Confirmed ~ Day + poly(Day, 2),
                          data = covid_germany)
summary(mod_covid_log_quad)

# Koeffizienten deuten auf beschleunigt exponentielles Wachstum hin

summary(mod_covid_log_quad)$r.squared - summary(mod_covid_log)$r.squared

# Minimaler Inkrement beim Determinationskoeffizient von ca. 1 %

anova(mod_covid_log, mod_covid_log_quad)

# Signifikanter Erklärungsgewinn durch quadratischen Trend



## GRAPHISCHE ÜBERPRÜFUNG
covid_germany_log_quad_pred <- predict(mod_covid_log_quad)

ggplot(covid_germany, aes(x = Day, y = log_Confirmed)) +
  geom_point(stat = "identity") +
  geom_line(aes(y = covid_germany_log_pred), color = "blue") +
  geom_line(aes(y = covid_germany_log_quad_pred), color = "red")

# Zshg. immer noch NICHT linear, nur stellenweise gutes Modell



## VORAUSSETZUNGEN ÜBERPRÜFEN

residualPlots(mod_covid_log_quad)

gvlma(mod_covid_log)
plot(gvlma(mod_covid_log))

# Voraussetzungen nicht hinreichend erfüllt:
# Residuen sind NICHT normalverteilt
# Linearität NICHT gegeben

var(residuals(mod_covid_log_quad))
boxplot(studres(mod_covid_log_quad))

# Residualvarianz ist klein (aber Aureißer vorhanden) 

##### TEIL 2 - Aufgabe 4 -----

## DATEN FÜR SPANIEN AUFBEREITEN
covid_spain <- subset(covid_sel, Country.Region == "Spain", Confirmed)
covid_spain$Day <- 1:79

covid_spain$log_Confirmed <- log(covid_spain$Confirmed)
covid_spain$log_Confirmed[covid_spain$log_Confirmed == -Inf] <- NA

covid_spain <- na.omit(covid_spain)


## EXPONENTIELLES MODELL FÜR SPANIEN

mod_covid_spain_log <- lm(log_Confirmed ~ Day,
                          data = covid_spain)

summary(mod_covid_spain_log)

# Steigungskoeffizient für Spanien = 0.224455

summary(mod_covid_log)

# Deutsches Modell ist noch aus Aufgabe 2b vorhanden
# Steigungskoeffizient für Deutschland = 0.168688



## KONFIDENZINTERVALLE

confint(mod_covid_log)

confint(mod_covid_spain_log)


