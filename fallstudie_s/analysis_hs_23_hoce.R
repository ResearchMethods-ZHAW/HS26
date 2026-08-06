# .################################################################################################
# Einfluss von COVID19 auf die Besucherzahlen im WPZ waehrend der dunklen Tageszeit ####
# Modul Research Methods, HS22. Adrian Hochreutener ####
# .################################################################################################

# .################################################################################################
# METADATA UND DEFINITIONEN ####
# .################################################################################################

# save and load workspace

# ____________________________________________________________

save.image(file = "fallstudie_s/results/my_work_space.RData")

# ____________________________________________________________

load(file = "fallstudie_s/results/my_work_space.RData")

# ____________________________________________________________


# Datenherkunft ####
# Saemtliche verwendeten Zaehdaten sind Eigentum des Wildnispark Zuerich und duerfen nur im Rahmen
# des Moduls verwendet werden. Sie sind vertraulich zu behandeln.
# Die Meteodaten sind Eigentum von MeteoSchweiz.
# Verwendete Meteodaten
# Lufttemperatur 2 m UEber Boden; Tagmaximum (6 UTC blis 18 UTC), tre200jx [°C ]
# Niederschlag; Halbtagessumme 6 UTC - 18 UTC, rre150j0 [mm]


# Ordnerstruktur ####
# Im Ordner in dem das R-Projekt abgelegt ist muessen folgende Unterordner bestehen:
# - data (Rohdaten hier ablegen)
# - results
# - scripts

# Benoetigte Bibliotheken ####
library("readr") # read data into r
library("ggplot2") # plot nice graphs
library("dplyr") # select data
library("lubridate") # Arbeiten mit Datumsformaten
library("suncalc") # berechne Tageszeiten abhaengig vom Sonnenstand
library("ggpubr") # to arrange multiple plots in one graph
library("PerformanceAnalytics") # Plotte Korrelationsmatrix
library("MuMIn") # Multi-Model Inference
library("AICcmodavg") # Modellaverageing
library("fitdistrplus") # Prueft die Verteilung in Daten
library("lme4") # Multivariate Modelle
library("DHARMa") # Modeldiagnostik
library("blmeco") # Bayesian data analysis using linear models
library("sjPlot") # Plotten von Modellergebnissen (tab_model)
library("lattice") # einfaches plotten von Zusammenhängen zwischen Variablen
library("glmmTMB")# zero-inflated model




# definiere ein farbset zur wiedervewendung
mycolors <- c("orangered", "gold", "mediumvioletred", "darkblue")

# Start und Ende ####
# Untersuchungszeitraum, ich waehle hier das Jahr 2019 bis und mit Sommer 2021
depo_start <- as.Date("2017-01-01")
depo_end <- as.Date("2022-7-31")

# Start und Ende Lockdown
# definieren, wichtig fuer die spaeteren Auswertungen
lock_1_start <- as.Date("2020-03-16")
lock_1_end <- as.Date("2020-05-11")

lock_2_start <- as.Date("2020-12-22")
lock_2_end <- as.Date("2021-03-01")

# Ebenfalls muessen die erste und letzte Kalenderwoche der Untersuchungsfrist definiert werden
# Diese werden bei Wochenweisen Analysen ebenfalls ausgeklammert da sie i.d.R. unvollstaendig sind
KW_start <- isoweek(depo_start)
KW_end <- isoweek(depo_end)

# Erster und letzter Tag der Ferien
# je nach Untersuchungsdauer muessen hier weitere oder andere Ferienzeiten ergaenzt werden
# (https://www.schulferien.org/schweiz/ferien/2020/)

# Rule of thumb: Sobald man viele (>5) Objekte mit sehr ähnlichen Namen erstellt, sollte man besser mit Listen oder DataFrames arbeiten
schulferien <- read_delim("datasets/fallstudie_s/ferien.csv", ",")


# .################################################################################################
# 1. DATENIMPORT #####
# .################################################################################################

# Beim Daten einlesen koennen sogleich die Datentypen und erste Bereinigungen vorgenommen werden

# 1.1 Zaehldaten ####
# Die Zaehldaten des Wildnispark wurden vorgaengig bereinigt. z.B. wurden Stundenwerte
# entfernt, an denen am Zaehler Wartungsarbeiten stattgefunden haben.

# lese die Daten ein
# Je nach Bedarf muss der Speicherort sowie der Dateiname angepasst werden
depo <- read_delim("datasets/fallstudie_s/WPZ/211_sihlwaldstrasse_2017_2022.csv", ";")

# Hinweis zu den Daten:
# In hourly analysis format, the data at 11:00 am corresponds to the counts saved between
# 11:00 am and 12:00 am.

# erstes Sichten und Anpassen der Datentypen
str(depo)

depo <- depo |>
  mutate(
    Datetime = as.POSIXct(DatumUhrzeit, format = "%d.%m.%Y %H:%M", tz = "CET"),
    Datum = as.Date(Datetime)
  )

# In dieser Auswertung werden nur Personen zu Fuss betrachtet!
depo <- depo |>
  # mit select werden spalten ausgewaehlt oder eben fallengelassen
  # (velos interessieren uns in dieser Auswertung nicht und Zeit soll in R immer zusammen mit Datum gespeichert werden)
  dplyr::select(-c(Velo_IN, Velo_OUT, Zeit, DatumUhrzeit)) |>
  # Berechnen des Totals, da dieses in den Daten nicht vorhanden ist
  mutate(Total = Fuss_IN + Fuss_OUT)

# Entferne die NA's in dem df.
depo <- na.omit(depo)

# .################################################################################################

# 1.2 Meteodaten ####
# Einlesen
meteo <- read_delim("datasets/fallstudie_s/WPZ/order_105742_data.txt", ";")

# Datentypen setzen
# Das Datum wird als Integer erkannt. Zuerst muss es in Text umgewaldelt werden aus dem dann
# das eigentliche Datum herausgelesen werden kann
meteo <- mutate(meteo, time = as.Date(as.character(time), "%Y%m%d"))

# Zeitangaben in UTC: 
#  00:40 UTC = 02:40 Sommerzeit = 01:40 Winterzeit
# Beispiel: 13 = beinhaltet Messperiode von 12:01 bis 13:00
# --> da wir mit Tageshöchstwerten oder -summen rechnen, können wir zum Glück ignorieren, dass das nicht 
# mit den Zähldaten übereinstimmt.


# Die eigentlichen Messwerte sind alle nummerisch
meteo <- meteo |>
  mutate(
    tre200nx = as.numeric(tre200nx),
    tre200jx = as.numeric(tre200jx),
    rre150n0 = as.numeric(rre150n0),
    rre150j0 = as.numeric(rre150j0),
    sremaxdv = as.numeric(sremaxdv)
  ) |>
  filter(time >= depo_start, time <= depo_end) # schneide dann auf Untersuchungsdauer

# Was ist eigentlich Niederschlag:
# https://www.meteoschweiz.admin.ch/home/wetter/wetterbegriffe/niederschlag.html

# Filtere Werte mit NA
meteo <- na.omit(meteo)

# Pruefe ob alles funktioniert hat
str(meteo)
sum(is.na(meteo)) # zeigt die Anzahl NA's im data.frame an


# .################################################################################################
# 2. VORBEREITUNG DER DATEN #####
# .################################################################################################

# 2.1 Convenience Variablen ####


# Wir gruppieren die Meteodaten noch nach Kalenderwoche und Werktag / Wochenende
# Dafür brauchen wir zuerst diese als Convenience Variablen
meteo <- meteo |>
  # wday sortiert die Wochentage automatisch in der richtigen Reihenfolge
  mutate(
    Wochentag = wday(time, week_start = 1),
    Wochentag = factor(Wochentag),
    # Werktag oder Wochenende hinzufuegen
    Wochenende = ifelse(Wochentag %in% c(6, 7), "Wochenende", "Werktag"),
    Wochenende = as.factor(Wochenende),
    # Kalenderwoche hinzufuegen
    KW = isoweek(time),
    KW = factor(KW),
    # monat und Jahr
    Monat = month(time),
    Monat = factor(Monat),
    Jahr = year(time),
    Jahr = factor(Jahr))

# und nun gruppieren und mean berechnen
meteo_day <- meteo |>
  group_by(Jahr, Monat, KW, Wochenende) |>
  summarise(
    tre200nx = mean(tre200nx),
    tre200jx = mean(tre200jx),
    rre150n0 = mean(rre150n0),
    rre150j0 = mean(rre150j0),
    sremaxdv= mean(sremaxdv))


depo <- depo |>
  # wday sortiert die Wochentage automatisch in der richtigen Reihenfolge
  mutate(
    Wochentag = wday(Datetime, week_start = 1),
    Wochentag = factor(Wochentag),
    # Werktag oder Wochenende hinzufuegen
    Wochenende = ifelse(Wochentag %in% c(6, 7), "Wochenende", "Werktag"),
    Wochenende = as.factor(Wochenende),
    # Kalenderwoche hinzufuegen
    KW = isoweek(Datetime),
    KW = factor(KW),
    # monat und Jahr
    Monat = month(Datetime),
    Monat = factor(Monat),
    Jahr = year(Datetime),
    Jahr = factor(Jahr))

# Lockdown
# Hinweis: ich mache das nachgelagert, da ich die Erfahrung hatte, dass zu viele
# Operationen in einem Schritt auch schon mal durcheinander erzeugen koennen.
# Hinweis II: Wir packen alle Phasen (normal, die beiden Lockdowns und Covid aber ohne Lockdown)
# in eine Spalte --> long ist schoener als wide
depo <- depo |>
  mutate(Phase = case_when(
    Datetime < lock_1_start ~ "Pre",
    Datetime >= lock_1_start & Datetime <= lock_1_end ~ "Lockdown_1",
    Datetime > lock_1_end & Datetime < lock_2_start ~ "Inter",
    Datetime >= lock_2_start & Datetime <= lock_2_end ~ "Lockdown_2",
    Datetime > lock_2_end ~ "Post"
  ))

# hat das gepklappt?!
unique(depo$Phase)

# in welchen KW war der Lockdown?
KW_lock_1_start <- isoweek(min(depo$Datum[depo$Phase == "Lockdown_1"]))
KW_lock_1_ende <- isoweek(max(depo$Datum[depo$Phase == "Lockdown_1"]))

depo <- depo |>
  # mit factor() koennen die levels direkt einfach selbst definiert werden.
  # wichtig: speizfizieren, dass aus R base, ansonsten kommt es zu einem
  # mix-up mit anderen packages
  mutate(Phase = base::factor(Phase, levels = c("Pre", "Lockdown_1", "Inter", "Lockdown_2", "Post")))

str(depo)

#Schulferien
# schreibe nun eine Funktion zur zuweisung Ferien. WENN groesser als start UND kleiner als
# ende, DANN schreibe ein 1
for (i in 1:nrow(schulferien)) {
  depo$Ferien[depo$Datum >= schulferien[i, "Start"] & depo$Datum <= schulferien[i, "Ende"]] <- 1
}
depo$Ferien[is.na(depo$Ferien)] <- 0

# als faktor speichern
depo$Ferien <- factor(depo$Ferien)


# Fuer einige Auswertungen muss auf die Stunden als nummerischer Wert zurueckgegriffen werden
depo$Stunde <- hour(depo$Datetime)
# hour gibt uns den integer
typeof(depo$Stunde)

# Die Daten wurden kalibriert. Wir runden sie fuer unserer Analysen auf Ganzzahlen
depo$Total <- round(depo$Total, digits = 0)
depo$Fuss_IN <- round(depo$Fuss_IN, digits = 0)
depo$Fuss_OUT <- round(depo$Fuss_OUT, digits = 0)

# 2.2 Tageszeit hinzufuegen ####

# Einteilung Standort Zuerich
Latitude <- 47.38598
Longitude <- 8.50806

# Start und das Ende der Sommerzeit:
# https://www.schulferien.org/schweiz/zeit/zeitumstellung/


# Welche Zeitzone haben wir eigentlich?
# Switzerland uses Central European Time (CET) during the winter as standard time,
# which is one hour ahead of Coordinated Universal Time (UTC+01:00), and
# Central European Summer Time (CEST) during the summer as daylight saving time,
# which is two hours ahead of Coordinated Universal Time (UTC+02:00).
# https://en.wikipedia.org/wiki/Time_in_Switzerland

# Was sind Astronomische Dämmerung und Golden Hour ueberhaupt?
# https://sunrisesunset.de/sonne/schweiz/zurich-kreis-1-city/
# https://www.rdocumentation.org/packages/suncalc/versions/0.5.0/topics/getSunlightTimes

# Wir arbeiten mit folgenden Variablen:
# "nightEnd" : night ends (morning astronomical twilight starts)
# "goldenHourEnd" : morning golden hour (soft light, best time for photography) ends
# "goldenHour" : evening golden hour starts
# "night" : night starts (dark enough for astronomical observations)

lumidata <-
  getSunlightTimes(
    date = seq.Date(depo_start, depo_end, by = 1),
    keep = c("nightEnd", "goldenHourEnd", "goldenHour", "night"),
    lat = Latitude,
    lon = Longitude,
    tz = "CET"
  ) |>
  as_tibble()

# jetzt haben wir alle noetigen Angaben zu Sonnenaufgang, Tageslaenge usw.
# diese Angaben koennen wir nun mit unseren Zaehldaten verbinden:
depo <- depo |>
  left_join(lumidata, by = c(Datum = "date"))

# im naechsten Schritt weise ich den Stunden die Tageszeiten Morgen, Tag, Abend und Nacht zu.
# diese Zuweisung basiert auf der Einteilung gem. suncalc und eigener Definition.
depo <- depo |>
  mutate(Tageszeit = case_when(
    Datetime >= nightEnd & Datetime <= goldenHourEnd ~ "Morgen",
    Datetime > goldenHourEnd & Datetime < goldenHour ~ "Tag",
    Datetime >= goldenHour & Datetime <= night ~ "Abend",
    .default = "Nacht"
  )) |>
  mutate(Tageszeit = factor(Tageszeit, levels = c("Morgen", "Tag", "Abend", "Nacht"), ordered = TRUE))


# behalte die relevanten Var
depo <- depo |> dplyr::select(-nightEnd, -goldenHourEnd, -goldenHour, -night, -lat, -lon)

# Plotte zum pruefn ob das funktioniert hat
ggplot(depo, aes(y = Datetime, color = Tageszeit, x = Stunde)) +
  geom_jitter() +
  scale_color_manual(values = mycolors)

sum(is.na(depo))

# bei mir hat der Zusatz der Tageszeit noch zu einigen NA-Wertren gefueht.
# Diese loesche ich einfach:
depo <- na.omit(depo)
# hat das funktioniert?
sum(is.na(depo))


# 2.4 Aggregierung der Stundendaten zu ganzen Tagen ####
# Zur Berechnung von Kennwerten ist es hilfreich, wenn neben den Stundendaten auch auf Ganztagesdaten
# zurueckgegriffen werden kann
# hier werden also pro Nutzergruppe und Richtung die Stundenwerte pro Tag aufsummiert
depo_d <- depo |>
  group_by(Datum, Wochentag, Wochenende, KW, Monat, Jahr, Phase) |>
  summarise(
    Total = sum(Fuss_IN + Fuss_OUT),
    Fuss_IN = sum(Fuss_IN),
    Fuss_OUT = sum(Fuss_OUT)
  )
# Wenn man die Convinience Variablen als grouping variable einspeisst, dann werden sie in
# das neue df uebernommen und muessen nicht nochmals hinzugefuegt werden
# pruefe das df
head(depo_d)

# nun gruppieren wir nicht nach Tag sondern v.a. nach Tageszeit
# Das Datum schliessen wir aus dieser Gruppierung aus, denn wenn wir es drin hätten,
# würde dieselbe "Nacht" jeweils zwei Einträge generieren (da sie über zwei Daten geht).
# Mit dieser Gruppierung haben wir jeweils eine Summe pro Tageszeit für alle Werktage einer Woche zusammen
# und beide Wochenendtage
depo_daytime <- depo |>
  group_by(Jahr, Monat, KW, Phase, Ferien, Wochenende, Tageszeit) |>
  summarise(
    Total = sum(Fuss_IN + Fuss_OUT),
    Fuss_IN = sum(Fuss_IN),
    Fuss_OUT = sum(Fuss_OUT))

# mean besser Vergleichbar, da Zeitreihen unterschiedlich lange
mean_phase_d <- depo_daytime |>
  group_by(Phase, Tageszeit) |>
  summarise(
    Total = mean(Total),
    IN = mean(Fuss_IN),
    OUT = mean(Fuss_OUT))


# Gruppiere die Werte nach Monat
depo_m <- depo |>
  group_by(Jahr, Monat) |>
  summarise(Total = sum(Total))

depo_m <- depo_m |>
  mutate(
    Ym = paste(Jahr, Monat), # und mache eine neue Spalte, in der Jahr und
    Ym = lubridate::ym(Ym)
  ) # formatiere als Datum


# Gruppiere die Werte nach Monat und TAGESZEIT
depo_m_daytime <- depo |>
  group_by(Jahr, Monat, Tageszeit) |>
  summarise(Total = sum(Total))
# sortiere das df aufsteigend (nur das es sicher stimmt)
depo_m_daytime <- depo_m_daytime |>
  mutate(
    Ym = paste(Jahr, Monat), # und mache eine neue Spalte, in der Jahr und
    Ym = lubridate::ym(Ym)
  ) # formatiere als Datum

# .################################################################################################
# 3. DESKRIPTIVE ANALYSE UND VISUALISIERUNG #####
# .################################################################################################

# 3.1 Verlauf der Besuchszahlen / m ####

# Monatliche Summen am Standort als Verlauf
# Plotte
ggplot(depo_m, mapping = aes(Ym, Total, group = 1)) + # group = 1 braucht R, dass aus den Einzelpunkten ein Zusammenhang hergestellt wird
  # zeichne Lockdown 1
  geom_rect(
    mapping = aes(
      xmin = ym("2020-3"), xmax = ym("2020-5"),
      ymin = 0, ymax = max(Total + (Total / 100 * 10))),
    fill = "lightskyblue", alpha = 0.2, colour = NA) +
  # zeichne Lockdown 2
  geom_rect(
    mapping = aes(
      xmin = ym("2020-12"), xmax = ym("2021-3"),
      ymin = 0, ymax = max(Total + (Total / 100 * 10))),
    fill = "darkolivegreen2", alpha = 0.2, colour = NA) +
  geom_line(alpha = 0.6, linewidth = 1) +
  scale_x_date(date_labels = "%b%y", date_breaks = "6 months") +
  labs(title = "", y = "Fussgänger:innen pro Monat", x = "Jahr") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave("Entwicklung_Zaehlstelle.png",
  width = 20, height = 10, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/")


# Monatliche Summen am Standort übereinander gelagert
ggplot(depo_m, aes(Monat, Total, group = Jahr, color = Jahr, linetype = Jahr)) +
  geom_line(size = 2) +
  geom_point() +
  scale_colour_viridis_d() +
  scale_linetype_manual(values = c(rep("solid", 3), "twodash", "twodash", "solid")) +
  scale_x_discrete(breaks = c(seq(0, 12, by = 1))) +
  geom_vline(xintercept = c(seq(1, 12, by = 1)), linetype = "dashed", color = "gray") +
  labs(title = "", y = "Fussgänger:innen pro Monat", x = "Monat") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# mache einen prozentuellen areaplot
ggplot(depo_m_daytime, aes(Ym, Total, fill = Tageszeit)) +
  geom_area(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = mycolors) +
  scale_x_date(date_labels = "%b%y", date_breaks = "6 months", 
               limits = c(min(depo_m_daytime$Ym), max = max(depo_m_daytime$Ym)), 
               # force the first and last tick marks to correspond to the actual limits specified in scale_x_date
               expand = c(0, 0)) +
  geom_vline(xintercept = seq(as.Date(min(depo_m_daytime$Ym)), as.Date(max(depo_m_daytime$Ym)), 
                              by = "6 months"), linetype = "dashed", color = "black")+
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = "bottom") +
  labs(title = "", y = "Verteilung Fussgänger:innen / Monat [%]", x = "Jahr")

ggsave("Proz_Entwicklung_Zaehlstelle.png",
       width = 20, height = 10, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/")



# .################################################################################################


# # ctree ####
# # berechne Regression für die Besuchszahlen in den Tageszeiten
# summary(lm(Total~Jahr + Tageszeit, data = depo))
#
# # berechne ctree
# library(partykit)
#
# ct <- ctree(Total ~ Jahr + Phase + Tageszeit + Stunde,
#             data = depo, maxdepth = 5)
#
# # we can inspect the results via a print method
# ct
# st <- as.simpleparty(ct)
#
# # https://stackoverflow.com/questions/13751962/how-to-plot-a-large-ctree-to-avoid-overlapping-nodes
# # ?plot.party
#
# # WICHTIG: um ueberlappungen zu vermeiden, plotte Bild, oeffne im separaten Fenster, amche Screenshot und speichere unter ctrees.png ab.
#
# plot(st, gp = gpar(fontsize = 10),
#      inner_panel=node_inner,
#      ep_args = list(justmin = 5),
#      ip_args=list(
#        abbreviate = FALSE,
#        id = FALSE))
#
# ## KW SUBSET
# # Berechne veränderung gegenüber Lockdown immer nur mit denselben KW
# depo_KW_lock <- depo |>
#   filter(KW_num>=KW_lock_1_start & KW_num <= KW_lock_1_ende)
#
# ct <- ctree(Total ~ Jahr + KW + Phase + Tageszeit + Stunde,
#            data = depo_KW_lock, maxdepth = 4)
#
# ct
# st <- as.simpleparty(ct)
#
# plot(st, gp = gpar(fontsize = 10),
#      inner_panel=node_inner,
#      ep_args = list(justmin = 5),
#      ip_args=list(
#        abbreviate = FALSE,
#        id = FALSE))
#
# # wenn man nur die KW während des lockdown 1 (12-20) miteinander vergleicht, zeigt sich, dass die phase
# # im ctree nur am tag relevant ist. am morgen, abend und nacht ist die phase mit 4 ebenen
# # nicht relevant. erst mit 5 ebenen ist siw während der dunkelheit auf der untersten ebene relevant.
#
# # calendar heat chart ####
#
# # to comapre, get day of the year instead date
# depo$day_of_y <- yday(depo$Datum)
#
# library(viridis)
#
# # ggplot(depo, aes(day_of_y, Stunde, fill=Total, color = Total))+
# #   geom_tile(size=0.6)
# #   scale_fill_viridis(name="Passagen",option ="C")+
# #   scale_color_viridis(name="Passagen",option ="C")+
# #   facet_grid(rows = vars(Jahr), scales = "free", space = "free")+
# #   scale_y_continuous(trans = "reverse", breaks = unique(depo$Stunde)) +
# #   theme_minimal(base_size = 15) +
# #   labs(title= "", x="Datum", y="Uhrzeit [h]")+
# #   theme(axis.text.x = element_blank(),
# #         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
#
# p <-   ggplot(depo)+
#     geom_tile(aes(day_of_y, Stunde, fill=Total, color = Total), size=0.6)+
#     geom_line(aes(day_of_y, nightEnd), color = "white")+
#     geom_line(aes(day_of_y, goldenHour), color = "white")+
#     scale_fill_viridis(name="Passagen",option ="C")+
#     scale_color_viridis(name="Passagen",option ="C")+
#     facet_grid(rows = vars(Jahr), scales = "free", space = "free")+
#     scale_y_continuous(trans = "reverse", breaks = unique(depo$Stunde)) +
#     theme_minimal(base_size = 15) +
#     labs(title= "", x="Datum", y="Uhrzeit [h]")+
#     theme(axis.text.x = element_blank(),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
# p
#
# # plotly::ggplotly(p)
#

# .################################################################################################



# 3.2 Wochengang ####
ggplot(data = depo, aes(x = Wochentag, y = Total, fill = Tageszeit)) +
  geom_violin() +
  labs(title = "", y = "Fussgänger:innen pro Tag") +
  facet_grid(cols = vars(Tageszeit), rows = vars(Phase))+
  scale_y_log10()+
  scale_fill_manual(values = mycolors) +
  theme_classic(base_size = 15) +
  theme(
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.title = element_blank(), 
    legend.position = "none")


# gruppiere nur nach Tageszeit und Phasen für Kennwerte
mean_phase <- depo_daytime |>
  group_by(Phase, Tageszeit) |>
  summarise(
    Mean = mean(Total))

ggplot(mean_phase, aes(Tageszeit, Mean, fill = Phase))+
  geom_col(position = "dodge", color = "black") +
  scale_fill_viridis_d() +
  labs(title = "", y = "Durchschnitt Fussgänger:innen pro Tag") +
  # scale_fill_manual(values = mycolors) +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom")




# 3.3 Tagesgang ####
# Bei diesen Berechnungen wird jeweils der Mittelwert pro Stunde berechnet.
# wiederum nutzen wir dafuer "pipes"
Mean_h <- depo |>
  group_by(Wochentag, Stunde, Phase) |>
  summarise(Total = mean(Total))

# Plotte den Tagesgang, unterteilt nach Wochentagen
ggplot(Mean_h, aes(x = Stunde, y = Total, colour = Wochentag, linetype = Wochentag)) +
  geom_line(size = 1) +
  scale_colour_viridis_d() +
  scale_linetype_manual(values = c(rep("solid", 5), "twodash", "twodash")) +
  scale_x_continuous(breaks = c(seq(0, 23, by = 1)), labels = c(seq(0, 23, by = 1))) +
  facet_grid(rows = vars(Phase)) +
  labs(x = "Uhrzeit [h]", y = "Durchscnnitt Fussganger_Innen / h", title = "") +
  lims(y = c(0, 25)) +
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave("Tagesgang.png",
  width = 25, height = 25, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/")



# plotte die Verteilung der Fussgänger nach Tageszeit abhängig von der Phase
ggplot(mean_phase_d, mapping = aes(Phase, Total, fill = Tageszeit)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = mycolors) +
  labs(title = "", y = "Verteilung Fussgänger:innen nach Tageszeit [%]", x = "Phase")+
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom") 

ggsave("Proz_Entwicklung_Zaehlstelle_Phase.png",
  width = 20, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/")






















# .################################################################################################
# 4. MULTIFAKTORIELLE ANALYSE UND VISUALISIERUNG #####
# .################################################################################################

# 4.1 Einflussfaktoren Besucherzahl ####
# Erstelle ein df indem die Zaehldaten / Tageszeit und Meteodaten vereint sind
umwelt <- inner_join(depo_daytime, meteo_day, by = c("Jahr", "Monat", "KW", "Wochenende"))


# Faktor und integer
# Im GLMM wird das Jahr, die KW oder auch anderes als random factor definiert. Dazu müssen sie als
# Faktor vorliegen. 
# prüfe:
str(umwelt)
# es darf keine NA's im datensatz haben
sum(is.na(umwelt))
# umwelt <- na.omit(umwelt)
summary(umwelt)


# Unser Modell kann nur mit ganzen Zahlen umgehen. Zum Glueck habe wir die Zaehldaten
# bereits gerundet.

# unser Datensatz muss ein df sein, damit scale funktioniert
umwelt <- as.data.frame(umwelt)

#  Variablen skalieren
# Skalieren der Variablen, damit ihr Einfluss vergleichbar wird
# (Problem verschiedene Skalen der Variablen (bspw. Temperatur in Grad Celsius,
# Niederschlag in Millimeter und Sonnenscheindauer in Minuten)
umwelt <- umwelt |>
  mutate(
    tre200jx_scaled = scale(tre200jx),
    tre200nx_scaled = scale(tre200nx),
    rre150j0_scaled = scale(rre150j0),
    rre150n0_scaled = scale(rre150n0),
    sremaxdv_scaled = scale(sremaxdv)
  )

# 4.2 Variablenselektion ####
# Korrelierende Variablen koennen das Modelergebnis verfaelschen. Daher muss vor der
# Modelldefinition auf Korrelation getestet werden.

# Erklaerende Variablen definieren
# Hier wird die Korrelation zwischen den (nummerischen) erklaerenden Variablen berechnet
cor <- cor(subset(umwelt, select = c(tre200nx: sremaxdv)))

# Mit dem folgenden Code kann eine simple Korrelationsmatrix aufgebaut werden
# hier kann auch die Schwelle für die Korrelation gesetzt werden,
# 0.7 ist liberal / 0.5 konservativ
# https://researchbasics.education.uconn.edu/r_critical_value_table/
cor[abs(cor) < 0.7] <- 0 # Setzt alle Werte kleiner 0.7 auf 0 (diese sind dann ok, alles groesser ist problematisch!)
cor

# Korrelationsmatrix erstellen
# Zur Visualisierung kann ein einfacher Plot erstellt werden:
chart.Correlation(subset(umwelt, select = c(tre200nx: sremaxdv)), 
                  histogram = TRUE, pch = 19)

# die Temperatur bei Nacht und diejenige bei tag korrelieren. ich packe also niemals beide variablen in ein modell



# Automatisierte Variablenselektion (achtung, RECHENINTENSIV)
# fuehre die dredge-Funktion und ein Modelaveraging durch
# Hier wird die Formel für die dredge-Funktion vorbereitet
# f <- Total ~ Wochentag + Ferien + Phase + Monat +
#   tre200jx_scaled + rre150j0_scaled + rre150n0_scaled +
#   sremaxdv_scaled
# # Jetzt kommt der Random-Factor hinzu und es wird eine Formel daraus gemacht
# f_dredge <- paste(c(f, "+ (1|Jahr)"), collapse = " ") |>
#   as.formula()
# # Das Modell mit dieser Formel ausführen
# m <- glmer.nb(f_dredge, data = umwelt, na.action = "na.fail")
# # Das Modell in die dredge-Funktion einfügen (siehe auch ?dredge)
# all_m <- dredge(m)
# # suche das beste Modell
# print(all_m)
# # Importance values der Variablen
# # hier wird die wichtigkeit der Variablen in den verschiedenen Modellen abgelesen
# MuMIn::sw(all_m)

# Schliesslich wird ein Modelaverage durchgeführt
# Schwellenwert für das delta-AIC = 2
# avgmodel <- model.avg(all_m, rank = "AICc", subset = delta < 2)
# summary(avgmodel)


# Unterteile in TAG DÄMMERUNG UND NACHT
umwelt_day <- umwelt |> 
  filter(Tageszeit == "Tag")

umwelt_duskdawn <- umwelt |> 
  filter(Tageszeit == "Morgen" | Tageszeit == "Abend" )

umwelt_night <- umwelt |> 
  filter(Tageszeit == "Nacht")


# 4.3 Pruefe Verteilung ####
# Tag

f1 <- fitdist(umwelt_day$Total, "norm") # Normalverteilung
f1_1 <- fitdist((umwelt_day$Total + 1), "lnorm") # log-Normalvert (beachte, dass ich +1 rechne.
# log muss positiv sein; allerdings kann man die
# Verteilungen dann nicht mehr miteinander vergleichen).
f2 <- fitdist(umwelt_day$Total, "pois") # Poisson
f3 <- fitdist(umwelt_day$Total, "nbinom") # negativ binomial
f4 <- fitdist(umwelt_day$Total, "exp") # exponentiell
# f5<-fitdist(umwelt_day$Total,"gamma")  # gamma (berechnung mit meinen Daten nicht möglich)
f6 <- fitdist(umwelt_day$Total, "logis") # logistisch
f7 <- fitdist(umwelt_day$Total, "geom") # geometrisch
# f8<-fitdist(umwelt_day$Total,"weibull")  # Weibull (berechnung mit meinen Daten nicht möglich)

gofstat(list(f1, f2, f3, f4, f6, f7),
  fitnames = c(
    "Normalverteilung", "Poisson",
    "negativ binomial", "exponentiell", "logistisch",
    "geometrisch"))

# die 2 besten (gemaess Akaike's Information Criterion) als Plot + normalverteilt,
plot.legend <- c("Normalverteilung", "exponentiell", "negativ binomial")
# vergleicht mehrere theoretische Verteilungen mit den empirischen Daten
cdfcomp(list(f1, f4, f3), legendtext = plot.legend)

# --> Verteilung ist gemäss AICc  negativ binomial
# --> ich entscheide mich für diese und NOrmalverteilung, als "baseline".




# 4.4 Berechne verschiedene Modelle ####

# TAG ####

# Hinweise zu GLMM: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# wie kommt man von log mean estimates zu den eigentlich werten?
# https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/

# Ich verwende hier die Funktion glmer aus der Bibliothek lme4.
# Die Totale Besucheranzahl soll durch verschiedene Parameter erklaert werden.
# Die verschiedenen Jahre sollen hierbei nicht beachtet werden,
# sie wird als random Faktor bestimmt --> Wir betrachten jedes Jahr für sich und nicht
# den allgemeinen Trend




# Einfacher Start
# Auch wenn wir gerade herausgefunden haben, dass die Verteilung negativ binomial ist,
# berechne ich für den Vergleich zuerst ein einfaches Modell der Familie poisson.
poisson_model <- glmer(Total ~ Monat + Ferien + Phase + Wochenende +
                       tre200jx_scaled + rre150j0_scaled + rre150n0_scaled +
                         sremaxdv_scaled +
                         (1 | Jahr), family = poisson, data = umwelt_day)

summary(poisson_model)



# Modeldiagnostik ####

# Model testing for over/underdispersion, zeroinflation and spatial autocorrelation following the DHARMa package.
# --> unbedingt die Vignette des DHARMa-Package konsultieren:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

# Residuals werden über eine Simulation auf eine Standard-Skala transformiert und
# können anschliessend getestet werden. Dabei kann die Anzahl Simulationen eingestellt
# werden (dauert je nach dem sehr lange)

simulationOutput <- simulateResiduals(fittedModel = poisson_model, n = 1000)

# plotting and testing scaled residuals

plot(simulationOutput)

testResiduals(simulationOutput)

testUniformity(simulationOutput)

# The most common concern for GLMMs is overdispersion, underdispersion and
# zero-inflation.

# separate test for dispersion

testDispersion(simulationOutput)

# test for Zeroinflation

testZeroInflation(simulationOutput)

# Testen auf Multicollinearität (dh zu starke Korrelationen im finalen Modell, zB falls
# auf Grund der ökologischen Plausibilität stark korrelierte Variablen im Modell)
# use VIF values: if values less then 5 is ok (sometimes > 10), if mean of VIF values
# not substantially greater than 1 (say 5), no need to worry.

car::vif(poisson_model)
mean(car::vif(poisson_model))





# TAG: Berechne ein negativ binomiales Modell
# gemäss AICc die beste Verteilung
nb_model_day <- glmer.nb(Total ~ Monat + Ferien + Phase + Wochenende +
                             tre200jx_scaled + rre150j0_scaled +
                             sremaxdv_scaled  +
                             (1 | Jahr), data = umwelt_day)

summary(nb_model_day)


# Residuals werden über eine Simulation auf eine Standard-Skala transformiert und
# können anschliessend getestet werden. Dabei kann die Anzahl Simulationen eingestellt
# werden (dauert je nach dem sehr lange)
simulationOutput <- simulateResiduals(fittedModel = nb_model_day, n = 1000)
# plotting and testing scaled residuals
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
# The most common concern for GLMMs is overdispersion, underdispersion and
# zero-inflation.
# separate test for dispersion
testDispersion(simulationOutput)
# test for Zeroinflation
testZeroInflation(simulationOutput)
# Testen auf Multicollinearität (dh zu starke Korrelationen im finalen Modell, zB falls
# auf Grund der ökologischen Plausibilität stark korrelierte Variablen im Modell)
# use VIF values: if values less then 5 is ok (sometimes > 10), if mean of VIF values
# not substantially greater than 1 (say 5), no need to worry.
car::vif(nb_model_day)
mean(car::vif(nb_model_day))



# ich verwende das package glmmTMB. Es ist wahnsinnig schnell und erlaubt viele spezifikationen
# https://glmmtmb.github.io/glmmTMB/articles/glmmTMB.pdf

# auf quadratischen Term testen ("es gehen weniger Leute in den Wald, wenn es zu heiss ist")
nb_quad_model_day <- glmmTMB(Total ~ Monat + Ferien + Phase + Wochenende +
                                  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled +
                                  sremaxdv_scaled +
                                  (1 | Jahr), family =nbinom1,
                               data = umwelt_day)

summary(nb_quad_model_day)

simulationOutput <- simulateResiduals(fittedModel = nb_quad_model_day, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_model_day)
mean(car::vif(nb_model_day))


# Interaktion testen, da Ferien und / oder Wochentage einen Einfluss auf
# die Besuchszahlen waehrend des Lockown haben koennen!
# (Achtung: Rechenintensiv!)
nb_quad_int_model_day <- glmmTMB(Total ~  Ferien + Phase + Wochenende +
                                   Monat * rre150j0_scaled+ tre200jx_scaled + I(tre200jx_scaled^2)  +
                                   sremaxdv_scaled +
                                  (1|Jahr), data = umwelt_day)

summary(nb_quad_int_model_day)
# nicht signifikant, darum vernachlaessige



# Vergleich der Modellguete mittels AICc

# cand.models <- list()
# cand.models[[1]] <- nb_model_day
# cand.models[[2]] <- nb_quad_model_day
# 
# 
# Modnames <- c(
#   "nb_model_day", "nb_quad_model_day"
# )
# aictab(cand.set = cand.models, modnames = Modnames)

# K = Anzahl geschaetzter Parameter (2 Funktionsparameter und die Varianz)
# Delta_AICc <2 = Statistisch gleichwertig
# AICcWt =  Akaike weight in %

# --> Ich entscheide mich bei diesen drei Modellen für das nb_quad_model_day
# Warum: statistisch das beste und ich denke die Quadratur macht Sinn!
# zudem wissen wir gem. Test der Verteilungen, dass negativ binomial Sinn macht.
# PROBLEM: alle drei Modelle erfüllen gem. der Modelldiagnostik die Vorausetzungen
# nicht komplett.

# Berechne ein Modell mit exponentieller Verteilung:
# gemäss AICc der Verteilung die zweitbeste
# https://stats.stackexchange.com/questions/240455/fitting-exponential-regression-model-by-mle
exp_model_day <- glmmTMB((Total + 1) ~ Monat + Ferien + Phase + Wochenende +
                             tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled +
                             sremaxdv_scaled +
                           (1 | Jahr), 
                         family = Gamma(link = "log"), data = umwelt_day)

summary(exp_model_day, dispersion = 1)
simulationOutput <- simulateResiduals(fittedModel = nb_quad_model_day, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
# car::vif(exp_model_day)
# mean(car::vif(exp_model_day))

# --> Die zweitbeste Verteilung (exp) führt auch nicht dazu, dass die Modellvoraussetzungen deutlich besser
# erfüllt werden



# zero-inflated model
# Dies weiss ich aus dem testZeroInflation und testResiduals

# "Ferien" not sig, therefore exclude
nb_model_day_zi <- glmmTMB(Total ~ Monat + Phase + Wochenende +
                             tre200jx_scaled + rre150j0_scaled +
                             sremaxdv_scaled  +
                             (1 | Jahr), data = umwelt_day, 
                           # The basic glmmTMB fit — a zero-inflated Poisson model with a single zero-
                           # inflation parameter applying to all observations (ziformula~1)
                           ziformula=~1,
                             family = nbinom1)

summary(nb_model_day_zi)

# Residuals werden über eine Simulation auf eine Standard-Skala transformiert und
# können anschliessend getestet werden. Dabei kann die Anzahl Simulationen eingestellt
# werden (dauert je nach dem sehr lange)
simulationOutput <- simulateResiduals(fittedModel = nb_model_day_zi, n = 1000)
# plotting and testing scaled residuals
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
# The most common concern for GLMMs is overdispersion, underdispersion and
# zero-inflation.
# separate test for dispersion
testDispersion(simulationOutput)
# test for Zeroinflation
testZeroInflation(simulationOutput)
# Testen auf Multicollinearität (dh zu starke Korrelationen im finalen Modell, zB falls
# auf Grund der ökologischen Plausibilität stark korrelierte Variablen im Modell)
# use VIF values: if values less then 5 is ok (sometimes > 10), if mean of VIF values
# not substantially greater than 1 (say 5), no need to worry.
car::vif(nb_model_day_zi)
mean(car::vif(nb_model_day_zi))


#. BEST MODEL DAY ####
# interaktion zero inflation
# "Ferien" not sig, therefore exclude
nb_int_model_day_zi <- glmmTMB(Total ~ Phase + Wochenende +
                             Monat * rre150j0_scaled + tre200jx_scaled + I(tre200jx_scaled^2) +
                             sremaxdv_scaled +
                             (1 | Jahr), data = umwelt_day, 
                           # The basic glmmTMB fit — a zero-inflated Poisson model with a single zero-
                           # inflation parameter applying to all observations (ziformula~1)
                           ziformula=~1,
                           family = nbinom1)

summary(nb_int_model_day_zi)

# Residuals werden über eine Simulation auf eine Standard-Skala transformiert und
# können anschliessend getestet werden. Dabei kann die Anzahl Simulationen eingestellt
# werden (dauert je nach dem sehr lange)
simulationOutput <- simulateResiduals(fittedModel = nb_int_model_day_zi, n = 1000)
# plotting and testing scaled residuals
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
# The most common concern for GLMMs is overdispersion, underdispersion and
# zero-inflation.
# separate test for dispersion
testDispersion(simulationOutput)
# test for Zeroinflation
testZeroInflation(simulationOutput)
# Testen auf Multicollinearität (dh zu starke Korrelationen im finalen Modell, zB falls
# auf Grund der ökologischen Plausibilität stark korrelierte Variablen im Modell)
# use VIF values: if values less then 5 is ok (sometimes > 10), if mean of VIF values
# not substantially greater than 1 (say 5), no need to worry.
car::vif(nb_model_day_zi)
mean(car::vif(nb_model_day_zi))
# erklaerte varianz
# The marginal R squared values are those associated with your fixed effects,
# the conditional ones are those of your fixed effects plus the random effects.
# Usually we will be interested in the marginal effects.
performance::r2(nb_int_model_day_zi)




# NACHT ####


# NACHT: Berechne ein negativ binomiales Modell
nb_model_night <- glmer.nb(Total ~ Monat + Ferien + Phase + Wochenende +
                           tre200nx_scaled + rre150n0_scaled +
                           (1 | Jahr), data = umwelt_night)

#.BEST MODEL NIGHT ####
# not sig:tre200nx_scaled + rre150n0_scaled +
nb_red_model_night <- glmer.nb(Total ~ Monat + Ferien + Phase + Wochenende +
                             (1 | Jahr), 
                             data = umwelt_night)
summary(nb_red_model_night)

simulationOutput <- simulateResiduals(fittedModel = nb_red_model_night, n = 1000)
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_red_model_night)
mean(car::vif(nb_red_model_night))
performance::r2(nb_red_model_night)



# auf quadratischen Term testen ("es gehen weniger Leute in den Wald, wenn es zu heiss ist")
nb_quad_model_night <- glmmTMB(Total ~ Monat + Ferien + Phase + Wochenende +
                               tre200nx_scaled + I(tre200nx_scaled^2) + rre150n0_scaled +
                               (1 | Jahr), family =nbinom1,
                             data = umwelt_night)

summary(nb_quad_model_night)

simulationOutput <- simulateResiduals(fittedModel = nb_quad_model_night, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_quad_model_night)
mean(car::vif(nb_quad_model_night))
performance::check_singularity(nb_quad_model_night)


# reduziertes modell
# not sig, therefore excl: Phase + tre200nx_scaled + I(tre200nx_scaled^2) + rre150n0_scaled +
nb_red_model_night <- glmmTMB(Total ~ Monat + Ferien +  + Wochenende +
                                 (1 | Jahr), family =nbinom1,
                               data = umwelt_night)

summary(nb_red_model_night)

simulationOutput <- simulateResiduals(fittedModel = nb_red_model_night, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_red_model_night)
mean(car::vif(nb_red_model_night))
performance::r2(nb_red_model_night)
performance::check_singularity(nb_red_model_night)


# Interaktion testen, da Ferien und / oder Wochentage einen Einfluss auf
# die Besuchszahlen waehrend des Lockown haben koennen!
nb_quad_int_model_night <- glmmTMB(Total ~  Ferien + Phase + Wochenende +
                                   Monat * rre150n0_scaled+ tre200nx_scaled + I(tre200nx_scaled^2)  +
                                   (1|Jahr), data = umwelt_night)

summary(nb_quad_int_model_night)
# nicht signifikant, darum vernachlaessige


# Berechne ein Modell mit exponentieller Verteilung:
exp_model_night <- glmmTMB((Total + 1) ~ Monat + Ferien + Phase + Wochenende +
                           tre200nx_scaled + I(tre200nx_scaled^2) + rre150n0_scaled +
                          (1 | Jahr), 
                         family = Gamma(link = "log"), data = umwelt_night)

summary(exp_model_night, dispersion = 1)
simulationOutput <- simulateResiduals(fittedModel = exp_model_night, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(exp_model_day)
mean(car::vif(exp_model_day))

# -->sehr schlecht


# zero-inflated model
# Dies weiss ich aus dem testZeroInflation und testResiduals
nb_model_night_zi <- glmmTMB(Total ~ Ferien +Monat + Phase + Wochenende +
                             tre200nx_scaled + rre150n0_scaled +
                             (1 | Jahr), data = umwelt_night, 
                           # The basic glmmTMB fit — a zero-inflated Poisson model with a single zero-
                           # inflation parameter applying to all observations (ziformula~1)
                           ziformula=~1,
                           family = nbinom1)

summary(nb_model_night_zi)
simulationOutput <- simulateResiduals(fittedModel = nb_model_night_zi, n = 1000)
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_model_night_zi)
mean(car::vif(nb_model_night_zi))
performance::r2(nb_model_night_zi)
performance::check_singularity(nb_model_night_zi)


# interaktion zero inflation
nb_int_model_night_zi <- glmmTMB(Total ~ Ferien + Phase + Wochenende +
                                 Monat * rre150n0_scaled + tre200nx_scaled + I(tre200nx_scaled^2) +
                                 (1 | Jahr), data = umwelt_night, 
                               # The basic glmmTMB fit — a zero-inflated Poisson model with a single zero-
                               # inflation parameter applying to all observations (ziformula~1)
                               ziformula=~1,
                               family = nbinom1)

summary(nb_int_model_night_zi)
simulationOutput <- simulateResiduals(fittedModel = nb_int_model_day_zi, n = 1000)
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_model_day_zi)
mean(car::vif(nb_model_day_zi))
performance::r2(nb_int_model_night_zi)
# == TRUE, problem
performance::check_singularity(nb_int_model_night_zi)
# A “singular” model fit means that some dimensions of the variance-covariance matrix have been estimated as exactly zero. This often occurs for mixed models with overly complex random effects structures.








# ---> in der nacht noch kein gutes modell gefunden


























# DUSKDAWN ####
# TAG: Berechne ein negativ binomiales Modell
# gemäss AICc die beste Verteilung
nb_model_duskdawn <- glmmTMB(Total ~ Monat + Ferien + Phase + Wochenende +
                               tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled +
                               sremaxdv_scaled +
                               (1 | Jahr), family =nbinom1,
                             data = umwelt_duskdawn)
summary(nb_model_duskdawn)
simulationOutput <- simulateResiduals(fittedModel = nb_model_duskdawn, n = 1000)
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_model_duskdawn)
mean(car::vif(nb_model_duskdawn))


# auf quadratischen Term testen ("es gehen weniger Leute in den Wald, wenn es zu heiss ist")
nb_quad_model_duskdawn <- glmmTMB(Total ~ Monat + Ferien + Phase + Wochenende +
                               tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled +
                               sremaxdv_scaled +
                               (1 | Jahr), family =nbinom1,
                             data = umwelt_duskdawn)

summary(nb_quad_model_duskdawn)

simulationOutput <- simulateResiduals(fittedModel = nb_quad_model_duskdawn, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_quad_model_duskdawn)
mean(car::vif(nb_quad_model_duskdawn))


# Interaktion testen, da Ferien und / oder Wochentage einen Einfluss auf
# die Besuchszahlen waehrend des Lockown haben koennen!
# (Achtung: Rechenintensiv!)
nb_quad_int_model_duskdawn <- glmmTMB(Total ~  Ferien + Phase + Wochenende +
                                   Monat * rre150j0_scaled+ tre200jx_scaled + I(tre200jx_scaled^2)  +
                                   sremaxdv_scaled +
                                   (1|Jahr), data = umwelt_duskdawn)

summary(nb_quad_int_model_duskdawn)
# nicht signifikant, darum vernachlaessige



# Berechne ein Modell mit exponentieller Verteilung:
# gemäss AICc der Verteilung die zweitbeste
# https://stats.stackexchange.com/questions/240455/fitting-exponential-regression-model-by-mle
exp_model_duskdawn <- glmmTMB((Total + 1) ~ Monat + Ferien + Phase + Wochenende +
                           tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled +
                           sremaxdv_scaled +
                           (1 | Jahr), 
                         family = Gamma(link = "log"), data = umwelt_duskdawn)

summary(exp_model_duskdawn, dispersion = 1)
simulationOutput <- simulateResiduals(fittedModel = nb_quad_model_day, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(exp_model_duskdawn)
mean(car::vif(exp_model_duskdawn))

# --> Die zweitbeste Verteilung (exp) führt dazu, dass die Modellvoraussetzungen besser
# erfüllt werden


#. BEST MODEL DUSKDAWN ####

# zero-inflated model
# Dies weiss ich aus dem testZeroInflation und testResiduals

# "Ferien" , tre200jx_scaled, not sig, sremaxdv_scaled, therefore exclude
nb_model_duskdawn_zi <- glmmTMB(Total ~ Monat + Phase + Wochenende +
                              rre150j0_scaled +
                              (1 | Jahr), data = umwelt_duskdawn, 
                           # The basic glmmTMB fit — a zero-inflated Poisson model with a single zero-
                           # inflation parameter applying to all observations (ziformula~1)
                           ziformula=~1,
                           family = nbinom1)

summary(nb_model_duskdawn_zi)

simulationOutput <- simulateResiduals(fittedModel = nb_model_duskdawn_zi, n = 1000)
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_model_duskdawn_zi)
mean(car::vif(nb_model_duskdawn_zi))
performance::r2(nb_model_duskdawn_zi)
performance::check_singularity(nb_model_duskdawn_zi)




# interaktion zero inflation
# "Ferien" not sig, therefore exclude
nb_int_model_duskdawn_zi <- glmmTMB(Total ~ Ferien + Phase + Wochenende +
                                 Monat * rre150j0_scaled + tre200jx_scaled + I(tre200jx_scaled^2) +
                                 sremaxdv_scaled +
                                 (1 | Jahr), data = umwelt_duskdawn, 
                               # The basic glmmTMB fit — a zero-inflated Poisson model with a single zero-
                               # inflation parameter applying to all observations (ziformula~1)
                               ziformula=~1,
                               family = nbinom1)

summary(nb_int_model_duskdawn_zi)

simulationOutput <- simulateResiduals(fittedModel = nb_int_model_duskdawn_zi, n = 1000)
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
car::vif(nb_int_model_duskdawn_zi)
mean(car::vif(nb_int_model_duskdawn_zi))
performance::r2(nb_int_model_duskdawn_zi)
performance::check_singularity(nb_int_model_duskdawn_zi)











# 4.5 Transformationen ####
# Die Modellvoraussetzungen waren überall mehr oder weniger verletzt.
# Das ist ein Problem, allerdings auch nicht ein so grosses.
# (man sollte es aber trotzdem ernst nehmen)
# Schielzeth et al. Robustness of linear mixed‐effects models to violations of distributional assumptions
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434
# Lo and Andrews, To transform or not to transform: using generalized linear mixed models to analyse reaction time data
# https://www.frontiersin.org/articles/10.3389/fpsyg.2015.01171/full

# die Lösung ist nun, die Daten zu transformieren:
# mehr unter: https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

# berechne skewness coefficient
library(moments)
skewness(umwelt$Total)
# A positive value means the distribution is positively skewed (rechtsschief).
# The most frequent values are low; tail is toward the high values (on the right-hand side)

# log 10, da stark rechtsschief
Tages_Model_quad_Jahr_log10 <- lmer(log10(Total + 1) ~ Wochentag + Ferien + Phase + Monat +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled + (1 | Jahr), data = umwelt)
summary(Tages_Model_quad_Jahr_log10)
plot(Tages_Model_quad_Jahr_log10, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_log10)
dispersion_glmer(Tages_Model_quad_Jahr_log10)
r.squaredGLMM(Tages_Model_quad_Jahr_log10)
car::vif(Tages_Model_nb_quad)
# lmer zeigt keine p-Werte, da diese schwer zu berechnen sind. Alternative Packages berechnen diese
# anhand der Teststatistik. Achtung: die Werte sind wahrscheinlich nicht präzise!
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html
tab_model(Tages_Model_quad_Jahr_log10, transform = NULL, show.se = TRUE)


# natural log, da stark rechtsschief
Tages_Model_quad_Jahr_ln <- lmer(log(Total + 1) ~ Wochentag + Ferien + Phase + Monat +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled + (1 | Jahr), data = umwelt)
summary(Tages_Model_quad_Jahr_ln)
plot(Tages_Model_quad_Jahr_ln, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_ln)
dispersion_glmer(Tages_Model_quad_Jahr_ln)
r.squaredGLMM(Tages_Model_quad_Jahr_ln)
car::vif(Tages_Model_nb_quad)

# --> Die Modellvoraussetzungen sind nicht deutlich besser erfüllt jetzt wo wir Transformationen
# benutzt haben. log10 und ln performen beide etwa gleich.

# Zusatz: ACHTUNG - Ruecktransformierte Regressionskoeffizienten zu erlangen (fuer die Interpretation, das Plotten),
# ist zudem nicht moeglich (Regressionskoeffizienten sind nur im transformierten Raum linear).
# Ein ruecktransformierter Regressionskoeffiziente haette eine nicht-lineare Beziehung mit der
# abhaengigen Variable.









# 4.6 Exportiere die Modellresultate ####
# (des besten Modells)
tab_model(nb_int_model_day_zi, transform = NULL, show.se = TRUE)
tab_model(nb_red_model_night, transform = NULL, show.se = TRUE)
tab_model(nb_model_duskdawn_zi, transform = NULL, show.se = TRUE)


# The marginal R squared values are those associated with your fixed effects,
# the conditional ones are those of your fixed effects plus the random effects.
# Usually we will be interested in the marginal effects.















# 4.7 Visualisiere Modellresultate ####

# Hintergrundinfo interaction plot:
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html

## definiere eine Funktion zum Plotten der Modellergebnisse
# Credits function to Sabrina Harsch


# schreibe fun fuer continuierliche var
rescale_plot_num_day <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "orangered")
  labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks + 1) * x_scaling, x_nk)
  
  custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var) - min(scaled_var)) / num_breaks))
  custom_limits <- c(min(scaled_var), max(scaled_var))
  
  plot_id <- plot_id +
    scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x = x_lab)) +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 170)) +
    theme_classic(base_size = 20)
  
  return(plot_id)
}

rescale_plot_num_night <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "darkblue")
  labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks + 1) * x_scaling, x_nk)
  
  custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var) - min(scaled_var)) / num_breaks))
  custom_limits <- c(min(scaled_var), max(scaled_var))
  
  plot_id <- plot_id +
    scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x = x_lab)) +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 20)) +
    theme_classic(base_size = 20)
  
  return(plot_id)
}

rescale_plot_num_duskdawn <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "mediumvioletred")
  labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks + 1) * x_scaling, x_nk)
  
  custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var) - min(scaled_var)) / num_breaks))
  custom_limits <- c(min(scaled_var), max(scaled_var))
  
  plot_id <- plot_id +
    scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x = x_lab)) +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 15)) +
    theme_classic(base_size = 20)
  
  return(plot_id)
}

# schreibe fun fuer diskrete var
rescale_plot_fac_day <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "orangered")
  
  plot_id <- plot_id +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 170)) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  return(plot_id)
}

rescale_plot_fac_night <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "darkblue")
  
  plot_id <- plot_id +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 20)) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  return(plot_id)
}

rescale_plot_fac_duskdawn <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "mediumvioletred")
  
  plot_id <- plot_id +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 15)) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  return(plot_id)
}

# Plotte

# Temperatur ####

## Tagesmaximaltemperatur
input_df <- nb_int_model_day_zi
input_term <- "tre200jx_scaled [all]"
unscaled_var <- umwelt_day$tre200jx
scaled_var <- umwelt_day$tre200jx_scaled
num_breaks <- 10
x_lab <- "Temperatur [°C]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_temp <- rescale_plot_num_day(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_temp

ggsave("day_temp.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)



# Sonnenscheindauer ####

input_df <- nb_int_model_day_zi
input_term <- "sremaxdv_scaled [all]"
unscaled_var <- umwelt_day$sremaxdv
scaled_var <- umwelt_day$sremaxdv_scaled
num_breaks <- 10
x_lab <- "Sonnenscheindauer [%]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_sonn <- rescale_plot_num_day(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_sonn

ggsave("day_sonne.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)

# Niederschlag ####

## regen tag
input_df <- nb_int_model_day_zi
input_term <- "rre150j0_scaled [all]"
unscaled_var <- umwelt_duskdawn$rre150j0
scaled_var <- umwelt_duskdawn$rre150j0_scaled
num_breaks <- 10
x_lab <- "Regensumme 6 - 18 Uhr UTC [mm]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_reg <- rescale_plot_num_day(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_reg

ggsave("day_regen.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


## regen duskdawn
input_df <- nb_model_duskdawn_zi
input_term <- "rre150j0_scaled [all]"
unscaled_var <- umwelt_day$rre150j0
scaled_var <- umwelt_day$rre150j0_scaled
num_breaks <- 10
x_lab <- "Regensumme 6 - 18 Uhr UTC [mm]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_reg <- rescale_plot_num_duskdawn(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_reg

ggsave("dusk_regen.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


# Wochentag ####

## Wochentag tag
input_df <- nb_int_model_day_zi
input_term <- "Wochenende [all]"
unscaled_var <- umwelt_day$Wochenende
scaled_var <- umwelt_day$Wochenende
num_breaks <- 10
x_lab <- "Wochentag"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_wd <- rescale_plot_fac_day(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk)
p_wd

ggsave("day_wd.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)

## Wochentag nacht
input_df <- nb_red_model_night
input_term <- "Wochenende [all]"
unscaled_var <- umwelt_night$Wochenende
scaled_var <- umwelt_night$Wochenende
num_breaks <- 10
x_lab <- "Wochentag"
y_lab <- "Fussgänger:innen pro Nacht"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_wd <- rescale_plot_fac_night(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk)
p_wd

ggsave("night_wd.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


## Wochentag duskdawn
input_df <- nb_model_duskdawn_zi
input_term <- "Wochenende [all]"
unscaled_var <- umwelt_duskdawn$Wochenende
scaled_var <- umwelt_duskdawn$Wochenende
num_breaks <- 10
x_lab <- "Wochentag"
y_lab <- "Fussgänger:innen in der Dämmerung"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_wd <- rescale_plot_fac_duskdawn(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk)
p_wd

ggsave("duskdawn_wd.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)

# Ferien ####

# nacht
input_df     <-  nb_red_model_night
input_term   <- "Ferien [all]"
unscaled_var <- umwelt_night$Ferien
scaled_var   <- umwelt_night$Ferien
num_breaks   <- 10
x_lab        <- "Ferien"
y_lab        <- "Fussgänger:innen pro Nacht"
x_scaling    <- 1 # in prozent
x_nk         <- 0   # x round nachkommastellen

p_feri <- rescale_plot_fac_night(input_df, input_term, unscaled_var, scaled_var, num_breaks,
                         x_lab, y_lab, x_scaling, x_nk)
p_feri

ggsave("night_ferien.png", width=15, height=15, units="cm", dpi=1000,
       path = "fallstudie_s/results/")


# Phase ####

# tag
input_df <- nb_int_model_day_zi
input_term <- "Phase [all]"
unscaled_var <- umwelt_day$Phase
scaled_var <- umwelt_day$Phase
num_breaks <- 10
x_lab <- "Phase"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_phase <- rescale_plot_fac_day(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_phase

ggsave("day_phase.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


# nacht
input_df <- nb_red_model_night
input_term <- "Phase [all]"
unscaled_var <- umwelt_night$Phase
scaled_var <- umwelt_night$Phase
num_breaks <- 10
x_lab <- "Phase"
y_lab <- "Fussgänger:innen pro Nacht"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_phase <- rescale_plot_fac_night(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_phase

ggsave("night_phase.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)

# duskdawn
input_df <- nb_model_duskdawn_zi
input_term <- "Phase [all]"
unscaled_var <- umwelt_duskdawn$Phase
scaled_var <- umwelt_duskdawn$Phase
num_breaks <- 10
x_lab <- "Phase"
y_lab <- "Fussgänger:innen in der Dämmerung"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_phase <- rescale_plot_fac_duskdawn(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_phase

ggsave("duskdawn_phase.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)

# Monat ####
# tag
input_df <- nb_int_model_day_zi
input_term <- "Monat [all]"
unscaled_var <- umwelt_day$Monat
scaled_var <- umwelt_day$Monat
num_breaks <- 10
x_lab <- "Monat"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_Monat <- rescale_plot_fac_day(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_Monat

ggsave("day_Monat.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


# nacht
input_df <- nb_red_model_night
input_term <- "Monat [all]"
unscaled_var <- umwelt_night$Monat
scaled_var <- umwelt_night$Monat
num_breaks <- 10
x_lab <- "Monat"
y_lab <- "Fussgänger:innen pro Nacht"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_Monat <- rescale_plot_fac_night(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_Monat

ggsave("night_Monat.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)

# duskdawn
input_df <- nb_model_duskdawn_zi
input_term <- "Monat [all]"
unscaled_var <- umwelt_duskdawn$Monat
scaled_var <- umwelt_duskdawn$Monat
num_breaks <- 10
x_lab <- "Monat"
y_lab <- "Fussgänger:innen in der Dämmerung"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_Monat <- rescale_plot_fac_duskdawn(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_Monat

ggsave("duskdawn_Monat.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)















# ZUSATZ: Wir haben die Wetterparameter skaliert.
# Fuer die Plots muss das beruecksichtigt werden: wir stellen nicht die wirklichen Werte
# dar sondern die skalierten. Mit folgendem Befehl kann man die Skalierung nachvollziehen:
# attributes(umwelt$tre200jx_scaled)
# Die Skalierung kann rueckgaengig gemacht werden, indem man die Skalierten werte mit
# dem scaling factor multipliziert und dann den Durchschnitt addiert:
# Bsp.: d$s.x * attr(d$s.x, 'scaled:scale') + attr(d$s.x, 'scaled:center')
# mehr dazu: https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
# --> wir bleiben aber bei den skalierten Werten, leben damit und sind uns dessen bewusst.

# Auch beim Plotten der Modellresultate gilt:
# visualisiere nur die Parameter welche nach der Modellselektion uebig bleiben
# und signifikant sind!
# plot_model / type = "pred" sagt die Werte "voraus"
# achte auf gleiche Skalierung der y-Achse (Vergleichbarkeit)

#
# # Temperatur
# t <- plot_model(Tages_Model_quad_Jahr_log10, type = "pred", terms =
#                   "tre200jx_scaled [all]", # [all] = Model contains polynomial or cubic /
#                 #quadratic terms. Consider using `terms="tre200jx_scaled [all]"`
#                 # to get smooth plots. See also package-vignette
#                 # 'Marginal Effects at Specific Values'.
#                 title = "", axis.title = c("Tagesmaximaltemperatur [°C]",
#                                            "Fussgaenger:innen pro Tag [log]"))
# # fuege die Achsenbeschriftung hinzu. Hier wird auf die unskalierten Werte zugegriffen.
# labels <- round(seq(floor(min(umwelt$tre200jx)), ceiling(max(umwelt$tre200jx)),
#                     # length.out = ___ --> Anpassen gemaess breaks auf dem Plot
#                     length.out = 5), 0)
# (Tempplot <- t +
#     scale_x_continuous(breaks = c(-2,-1,0,1,2),
#                        labels = c(labels))+
#     # fuege die y- Achsenbeschriftung hinzu. Hier transformieren wir die Werte zurueck
#     scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
#                        labels = round(c(10^0, 10^0.5, 10^1, 10^1.5, 10^2),0),
#                        limits = c(0, 2))+
#     theme_classic(base_size = 20))



















#.#####################################################
#.#####################################################
#.#####################################################

# ARCHIV HS22####




# 4.1 Einflussfaktoren Besucherzahl ####
# Erstelle ein df indem die taeglichen Zaehldaten und Meteodaten vereint sind
umwelt <- inner_join(depo_daytime, meteo_day, by = c("Jahr", "Monat", "KW", "Wochenende"))

# Wir muessen unserem Daten noch zuweisen, ob Ferienzeit oder nicht. Das machen wir mit einer Funktion
# erstelle zuerst ein dataframe zur Zuweisung der Ferien # credits Melina Grether

Start <- c(
  Winterferien_2016_start, Fruehlingsferien_2017_start, Sommerferien_2017_start, Herbstferien_2017_start,
  Winterferien_2017_start, Fruehlingsferien_2018_start, Sommerferien_2018_start, Herbstferien_2018_start,
  Winterferien_2019_start, Fruehlingsferien_2019_start, Sommerferien_2019_start, Herbstferien_2019_start,
  Winterferien_2020_start, Fruehlingsferien_2020_start, Sommerferien_2020_start, Herbstferien_2020_start,
  Winterferien_2021_start, Fruehlingsferien_2021_start, Sommerferien_2021_start, Herbstferien_2021_start,
  Winterferien_2022_start, Fruehlingsferien_2022_start, Sommerferien_2022_start, Herbstferien_2022_start
)
End <- c(
  Winterferien_2016_ende, Fruehlingsferien_2017_ende, Sommerferien_2017_ende, Herbstferien_2017_ende,
  Winterferien_2017_ende, Fruehlingsferien_2018_ende, Sommerferien_2018_ende, Herbstferien_2018_ende,
  Winterferien_2019_ende, Fruehlingsferien_2019_ende, Sommerferien_2019_ende, Herbstferien_2019_ende,
  Winterferien_2020_ende, Fruehlingsferien_2020_ende, Sommerferien_2020_ende, Herbstferien_2020_ende,
  Winterferien_2021_ende, Fruehlingsferien_2021_ende, Sommerferien_2021_ende, Herbstferien_2021_ende,
  Winterferien_2022_ende, Fruehlingsferien_2022_ende, Sommerferien_2022_ende, Herbstferien_2022_ende
)

# verbinde das zu einem df
ferien <- data.frame(Start, End)

# schreibe nun eine Funktion zur zuweisung Ferien. WENN groesser als start UND kleiner als
# ende, DANN schreibe ein 1
for (i in 1:nrow(ferien)) {
  umwelt$Ferien[umwelt$Datum >= ferien[i, "Start"] & umwelt$Datum <= ferien[i, "End"]] <- 1
}
umwelt$Ferien[is.na(umwelt$Ferien)] <- 0

# hat das funktioniert? zaehle die anzahl Ferientage
sum(umwelt$Ferien)

# Faktor und integer
# Im GLMM wird das Jahr als random factor definiert. Dazu muss es als
# Faktor vorliegen. Monat und KW koennen die Besuchszahlen auch erklaeren.
# auch sie muessen faktoren sein


# DAS SOLLTE EIG SCHON GEMACHT SEIN VON WEITER OBEN. PRÜFE DAS


umwelt <- umwelt |>
  mutate(Jahr = as.factor(Jahr)) |>
  mutate(KW = as.factor(KW)) |>
  mutate(Monat = as.factor(Monat)) |>
  mutate(Ferien = as.factor(Ferien)) |>
  # zudem muessen die die nummerischen Wetterdaten auch als solche abgespeichert sein
  mutate(tre200nx = as.numeric(tre200nx)) |>
  mutate(tre200jx = as.numeric(tre200jx)) |>
  mutate(rre150j0 = as.numeric(rre150j0)) |>
  mutate(rre150n0 = as.numeric(rre150n0)) |>
  mutate(sremaxdv = as.numeric(sremaxdv))

# falls das noch zu NA's gefuehrt hat, muessen diese entfernt werden
sum(is.na(umwelt))
umwelt <- na.omit(umwelt)
summary(umwelt)
str(umwelt)

# Unser Modell kann nur mit ganzen Zahlen umgehen. Zum Glueck habe wir die Zaehldaten
# bereits gerundet.

# unser Datensatz muss ein df sein, damit scale funktioniert
umwelt <- as.data.frame(umwelt)

#  Variablen skalieren
# Skalieren der Variablen, damit ihr Einfluss vergleichbar wird
# (Problem verschiedene Skalen der Variablen (bspw. Temperatur in Grad Celsius,
# Niederschlag in Millimeter und Sonnenscheindauer in Minuten)
umwelt <- umwelt |>
  mutate(
    tre200jx_scaled = scale(tre200jx),
    tre200nx_scaled = scale(tre200nx),
    rre150j0_scaled = scale(rre150j0),
    rre150n0_scaled = scale(rre150n0),
    sremaxdv_scaled = scale(sremaxdv)
  )

# 4.2 Variablenselektion ####
# Korrelierende Variablen koennen das Modelergebnis verfaelschen. Daher muss vor der
# Modelldefinition auf Korrelation getestet werden.

# Erklaerende Variablen definieren
# Hier wird die Korrelation zwischen den (nummerischen) erklaerenden Variablen berechnet
cor <- cor(umwelt[, 12:16]) # in den [] waehle ich die skalierten Spalten.
# Mit dem folgenden Code kann eine simple Korrelationsmatrix aufgebaut werden
# hier kann auch die Schwelle für die Korrelation gesetzt werden,
# 0.7 ist liberal / 0.5 konservativ
# https://researchbasics.education.uconn.edu/r_critical_value_table/
cor[abs(cor) < 0.7] <- 0 # Setzt alle Werte kleiner 0.7 auf 0 (diese sind dann ok, alles groesser ist problematisch!)
cor

# Korrelationsmatrix erstellen
# Zur Visualisierung kann ein einfacher Plot erstellt werden:
chart.Correlation(umwelt[, 12:16], histogram = TRUE, pch = 19)

# ich schliesse die Temperatur bei Nacht in den Modellen aufgrund der Korelation aus,
# da ich davon ausgehe, dass die Temperatur bei Tag das Besuchsaufkommen besser erklaert

# Automatisierte Variablenselektion (achtung, RECHENINTENSIV)
# fuehre die dredge-Funktion und ein Modelaveraging durch
# Hier wird die Formel für die dredge-Funktion vorbereitet
# f <- Total ~ Wochentag + Ferien + Phase + Monat +
#   tre200jx_scaled + rre150j0_scaled + rre150n0_scaled +
#   sremaxdv_scaled
# # Jetzt kommt der Random-Factor hinzu und es wird eine Formel daraus gemacht
# f_dredge <- paste(c(f, "+ (1|Jahr)"), collapse = " ") |>
#   as.formula()
# # Das Modell mit dieser Formel ausführen
# m <- glmer.nb(f_dredge, data = umwelt, na.action = "na.fail")
# # Das Modell in die dredge-Funktion einfügen (siehe auch ?dredge)
# all_m <- dredge(m)
# # suche das beste Modell
# print(all_m)
# # Importance values der Variablen
# # hier wird die wichtigkeit der Variablen in den verschiedenen Modellen abgelesen
# MuMIn::sw(all_m)

# Schliesslich wird ein Modelaverage durchgeführt
# Schwellenwert für das delta-AIC = 2
# avgmodel <- model.avg(all_m, rank = "AICc", subset = delta < 2)
# summary(avgmodel)

# 4.3 Pruefe Verteilung ####
# pruefe zuerst nochmals, ob wir NA im df haben:
sum(is.na(umwelt$Total))

f1 <- fitdist(umwelt$Total, "norm") # Normalverteilung
f1_1 <- fitdist((umwelt$Total + 1), "lnorm") # log-Normalvert (beachte, dass ich +1 rechne.
# log muss positiv sein; allerdings kann man die
# Verteilungen dann nicht mehr miteinander vergleichen).
f2 <- fitdist(umwelt$Total, "pois") # Poisson
f3 <- fitdist(umwelt$Total, "nbinom") # negativ binomial
f4 <- fitdist(umwelt$Total, "exp") # exponentiell
# f5<-fitdist(umwelt$Total,"gamma")  # gamma (berechnung mit meinen Daten nicht möglich)
f6 <- fitdist(umwelt$Total, "logis") # logistisch
f7 <- fitdist(umwelt$Total, "geom") # geometrisch
# f8<-fitdist(umwelt$Total,"weibull")  # Weibull (berechnung mit meinen Daten nicht möglich)

gofstat(list(f1, f2, f3, f4, f6, f7),
        fitnames = c(
          "Normalverteilung", "Poisson",
          "negativ binomial", "exponentiell", "logistisch",
          "geometrisch"
        )
)

# die 2 besten (gemaess Akaike's Information Criterion) als Plot + normalverteilt,
plot.legend <- c("Normalverteilung", "exponentiell", "negativ binomial")
# vergleicht mehrere theoretische Verteilungen mit den empirischen Daten
cdfcomp(list(f1, f4, f3), legendtext = plot.legend)

# --> Verteilung ist gemäss AICc exponentiell. negativ binomial ist auch nicht schlecht.
# --> ich entscheide mich für diese beide und probiere mit beiden Modelle aus.





########### EINPFLEGEN FÜR HS23

# Model testing for over/underdispersion, zeroinflation and spatial autocorrelation following the DHARMa package.
# unbedingt die Vignette des DHARMa-Package konsultieren:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

# Residuals werden über eine Simulation auf eine Standard-Skala transformiert und
# können anschliessend getestet werden. Dabei kann die Anzahl Simulationen eingestellt
# werden (dauert je nach dem sehr lange)
#
# simulationOutput <- simulateResiduals(fittedModel = m_day, n = 10000)
#
# # plotting and testing scaled residuals
#
# plot(simulationOutput)
#
# testResiduals(simulationOutput)
#
# testUniformity(simulationOutput)
#
# # The most common concern for GLMMs is overdispersion, underdispersion and
# # zero-inflation.
#
# # separate test for dispersion
#
# testDispersion(simulationOutput)
#
# # test for Zeroinflation
#
# testZeroInflation(simulationOutput)
#
# # test for spatial Autocorrelation
#
# # calculating x, y positions per group
# groupLocations = aggregate(DF_mod_day[, 3:4], list(DF_mod_day$x, DF_mod_day$y), mean)
# groupLocations$group <- paste(groupLocations$Group.1,groupLocations$Group.2)
#
# # calculating residuals per group
# res2 = recalculateResiduals(simulationOutput, group = groupLocations$group)
#
# # running the spatial test on grouped residuals
# testSpatialAutocorrelation(res2, groupLocations$x, groupLocations$y, plot = F)
#
# # Testen auf Multicollinearität (dh zu starke Korrelationen im finalen Modell, zB falls
# # auf Grund der ökologischen Plausibilität stark korrelierte Variablen im Modell)
# # use VIF values: if values less then 5 is ok (sometimes > 10), if mean of VIF values
# # not substantially greater than 1 (say 5), no need to worry.
#
# car::vif(m_day)
# mean(car::vif(m_day))







# 4.4 Berechne verschiedene Modelle ####

# Hinweise zu GLMM: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# wie kommt man von log mean estimates zu den eigentlich werten?
# https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/

# Ich verwende hier die Funktion glmer aus der Bibliothek lme4.
# Die Totale Besucheranzahl soll durch verschiedene Parameter erklaert werden.
# Die verschiedenen Jahre sollen hierbei nicht beachtet werden,
# sie wird als random Faktor bestimmt --> Wir betrachten jedes Jahr für sich und nicht
# den allgemeinen Trend







#######################################
# KALENDERWOCHE NICHT DRIN
# Monat ist aber drin und damit die saisonalitaet
#######################################






# Einfacher Start
# Auch wenn wir gerade herausgefunden haben, dass die Verteilung negativ binomial ist,
# berechne ich für den Vergleich zuerst ein einfaches Modell der Familie poisson.
Tages_Model <- glmer(Total ~ Wochentag + Ferien + Phase + Monat +
                       tre200jx_scaled + rre150j0_scaled + rre150n0_scaled +
                       sremaxdv_scaled +
                       (1 | Jahr), family = poisson, data = umwelt)

summary(Tages_Model)
# Inspektionsplots
plot(Tages_Model, type = c("p", "smooth"))
qqmath(Tages_Model)
# pruefe auf Overdispersion
dispersion_glmer(Tages_Model) # it shouldn't be over 1.4
# wir gut erklaert das Modell?
r.squaredGLMM(Tages_Model)
# check for multicollinearity
# https://rforpoliticalscience.com/2020/08/03/check-for-multicollinearity-with-the-car-package-in-r/
car::vif(Tages_Model) # VIF für beide predictors = 1, d.h. voneinander unabhängig (kritisch wird es ab einem Wert von >4-5)

# Berechne ein negativ binomiales Modell
# gemäss AICc die zweitbeste Verteilung
Tages_Model_nb <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Monat +
                             tre200jx_scaled + rre150j0_scaled + rre150n0_scaled +
                             sremaxdv_scaled +
                             (1 | Jahr), data = umwelt)

summary(Tages_Model_nb)
plot(Tages_Model_nb, type = c("p", "smooth"))
qqmath(Tages_Model_nb)
dispersion_glmer(Tages_Model_nb)
r.squaredGLMM(Tages_Model_nb)
car::vif(Tages_Model_nb)

# auf quadratischen Term testen ("es gehen weniger Leute in den Wald, wenn es zu heiss ist")
Tages_Model_nb_quad <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Monat +
                                  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
                                  sremaxdv_scaled +
                                  (1 | Jahr), data = umwelt)

summary(Tages_Model_nb_quad)
plot(Tages_Model_nb_quad, type = c("p", "smooth"))
qqmath(Tages_Model_nb_quad)
dispersion_glmer(Tages_Model_nb_quad)
r.squaredGLMM(Tages_Model_nb_quad)
car::vif(Tages_Model_nb_quad)

# Interaktion testen, da Ferien und / oder Wochentage einen Einfluss auf
# die Besuchszahlen waehrend des Lockown haben koennen!
# (Achtung: Rechenintensiv!)
# Tages_Model_nb_int <- glmer.nb(Anzahl_Total ~  Wochentag  * Ferien + Phase +
#                                  tre200jx_scaled + I(tre200jx_scaled^2) *
#                                  rre150j0_scaled + sremaxdv_scaled +
#                                  (1|KW) + (1|Jahr), data = umwelt)
#
# summary(Tages_Model_nb_int)
# plot(Tages_Model_nb_int, type = c("p", "smooth"))
# qqmath(Tages_Model_nb_int)
# dispersion_glmer(Tages_Model_nb_int)
# r.squaredGLMM(Tages_Model_nb_int)


# Vergleich der Modellguete mittels AICc
cand.models <- list()
cand.models[[1]] <- Tages_Model
cand.models[[2]] <- Tages_Model_nb
cand.models[[3]] <- Tages_Model_nb_quad

Modnames <- c(
  "Tages_Model", "Tages_Model_nb",
  "Tages_Model_nb_quad"
)
aictab(cand.set = cand.models, modnames = Modnames)
# K = Anzahl geschaetzter Parameter (2 Funktionsparameter und die Varianz)
# Delta_AICc <2 = Statistisch gleichwertig
# AICcWt =  Akaike weight in %

# --> Ich entscheide mich bei diesen drei Modellen für das Tages_Model_nb_quad
# Warum: statistisch das beste und ich denke die Quadratur macht Sinn!
# zudem wissen wir gem. Test der Verteilungen, dass negativ binomial Sinn macht.
# PROBLEM: alle drei Modelle erfüllen gem. der Modelldiagnostik die VOrausetzungen
# nicht komplett.

# Berechne ein Modell mit exponentieller Verteilung:
# gemäss AICc der Verteilung die zweitbeste
# https://stats.stackexchange.com/questions/240455/fitting-exponential-regression-model-by-mle
Tages_Model_exp <- glmer((Total + 1) ~ Wochentag + Ferien + Phase + Monat +
                           tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
                           sremaxdv_scaled + (1 | Jahr), family = Gamma(link = "log"), data = umwelt)

summary(Tages_Model_exp, dispersion = 1)
plot(Tages_Model_exp, type = c("p", "smooth"))
qqmath(Tages_Model_exp)
dispersion_glmer(Tages_Model_exp) # it shouldn't be over 1.4
r.squaredGLMM(Tages_Model_exp)
car::vif(Tages_Model_nb_quad)

# --> Die zweitbeste Verteilung (exp) führt auch nicht dazu, dass die Modellvoraussetzungen besser
# erfüllt werden


# 4.5 Transformationen ####
# Die Modellvoraussetzungen waren überall mehr oder weniger verletzt.
# Das ist ein Problem, allerdings auch nicht ein so grosses.
# (man sollte es aber trotzdem ernst nehmen)
# Schielzeth et al. Robustness of linear mixed‐effects models to violations of distributional assumptions
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434
# Lo and Andrews, To transform or not to transform: using generalized linear mixed models to analyse reaction time data
# https://www.frontiersin.org/articles/10.3389/fpsyg.2015.01171/full

# die Lösung ist nun, die Daten zu transformieren:
# mehr unter: https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

# berechne skewness coefficient
library(moments)
skewness(umwelt$Total)
# A positive value means the distribution is positively skewed (rechtsschief).
# The most frequent values are low; tail is toward the high values (on the right-hand side)

# log 10, da stark rechtsschief
Tages_Model_quad_Jahr_log10 <- lmer(log10(Total + 1) ~ Wochentag + Ferien + Phase + Monat +
                                      tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
                                      sremaxdv_scaled + (1 | Jahr), data = umwelt)
summary(Tages_Model_quad_Jahr_log10)
plot(Tages_Model_quad_Jahr_log10, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_log10)
dispersion_glmer(Tages_Model_quad_Jahr_log10)
r.squaredGLMM(Tages_Model_quad_Jahr_log10)
car::vif(Tages_Model_nb_quad)
# lmer zeigt keine p-Werte, da diese schwer zu berechnen sind. Alternative Packages berechnen diese
# anhand der Teststatistik. Achtung: die Werte sind wahrscheinlich nicht präzise!
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html
tab_model(Tages_Model_quad_Jahr_log10, transform = NULL, show.se = TRUE)


# natural log, da stark rechtsschief
Tages_Model_quad_Jahr_ln <- lmer(log(Total + 1) ~ Wochentag + Ferien + Phase + Monat +
                                   tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
                                   sremaxdv_scaled + (1 | Jahr), data = umwelt)
summary(Tages_Model_quad_Jahr_ln)
plot(Tages_Model_quad_Jahr_ln, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_ln)
dispersion_glmer(Tages_Model_quad_Jahr_ln)
r.squaredGLMM(Tages_Model_quad_Jahr_ln)
car::vif(Tages_Model_nb_quad)

# --> Die Modellvoraussetzungen sind nicht deutlich besser erfüllt jetzt wo wir Transformationen
# benutzt haben. log10 und ln performen beide etwa gleich.

# Zusatz: ACHTUNG - Ruecktransformierte Regressionskoeffizienten zu erlangen (fuer die Interpretation, das Plotten),
# ist zudem nicht moeglich (Regressionskoeffizienten sind nur im transformierten Raum linear).
# Ein ruecktransformierter Regressionskoeffiziente haette eine nicht-lineare Beziehung mit der
# abhaengigen Variable.


# 4.6 Exportiere die Modellresultate ####
# (des besten Modells)
tab_model(Tages_Model_nb_quad, transform = NULL, show.se = TRUE)
# The marginal R squared values are those associated with your fixed effects,
# the conditional ones are those of your fixed effects plus the random effects.
# Usually we will be interested in the marginal effects.


# 4.7 Visualisiere Modellresultate ####

# Hintergrundinfo interaction plot:
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html

## definiere eine Funktion zum Plotten der Modellergebnisse
# Credits function to Sabrina Harsch

# original
# rescale_plot <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, x_scaling, x_nk) {
#
#   plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title="")
#   labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks+1)*x_scaling, x_nk)
#
#   custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var)-min(scaled_var))/num_breaks))
#   custom_limits <- c(min(scaled_var), max(scaled_var))
#
#   plot_id <- plot_id +
#     scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x=x_lab)) +
#     scale_y_continuous(name=NULL, labels = scales::percent_format(accuracy = 5L), limits = c(0,1),position = "left") +
#     theme_classic()
#
#   return(plot_id)
# }

# schreibe fun fuer continuierliche var
rescale_plot_num <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "", color = "blue")
  labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks + 1) * x_scaling, x_nk)
  
  custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var) - min(scaled_var)) / num_breaks))
  custom_limits <- c(min(scaled_var), max(scaled_var))
  
  plot_id <- plot_id +
    scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x = x_lab)) +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 65)) +
    theme_classic(base_size = 20)
  
  return(plot_id)
}

# schreibe fun fuer diskrete var
rescale_plot_fac <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "")
  
  plot_id <- plot_id +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 65)) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  return(plot_id)
}


## Tagesmaximaltemperatur
input_df <- nb_model_day_zi
input_term <- "tre200jx_scaled [all]"
unscaled_var <- umwelt_day$tre200jx
scaled_var <- umwelt_day$tre200jx_scaled
num_breaks <- 10
x_lab <- "Temperatur [°C]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_temp <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_temp

ggsave("temp.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)






## NACHTmaximaltemperatur
input_df <- nb_model_night_zi
input_term <- "tre200nx_scaled [all]"
unscaled_var <- umwelt_night$tre200jx
scaled_var <- umwelt_night$tre200jx_scaled
num_breaks <- 10
x_lab <- "Temperatur [°C]"
y_lab <- "Fussgänger:innen pro Nacht"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_temp <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_temp

ggsave("temp.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)










## sonnenscheindauer
input_df <- Tages_Model_nb_quad
input_term <- "sremaxdv_scaled [all]"
unscaled_var <- umwelt$sremaxdv
scaled_var <- umwelt$sremaxdv_scaled
num_breaks <- 10
x_lab <- "Sonnenscheindauer [%]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_sonn <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_sonn

ggsave("sonne.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


## regen tag
input_df <- Tages_Model_nb_quad
input_term <- "rre150j0_scaled [all]"
unscaled_var <- umwelt$rre150j0
scaled_var <- umwelt$rre150j0_scaled
num_breaks <- 10
x_lab <- "Regensumme 6 - 18 Uhr [mm]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_reg <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_reg

ggsave("regen.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


## Wochentag
input_df <- Tages_Model_nb_quad
input_term <- "Wochentag [all]"
unscaled_var <- umwelt$Wochentag
scaled_var <- umwelt$Wochentag
num_breaks <- 10
x_lab <- "Wochentag"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_wd <- rescale_plot_fac(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_wd

ggsave("wd.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


## Ferien
# input_df     <-  Tages_Model_nb_quad
# input_term   <- "Ferien [all]"
# unscaled_var <- umwelt$Ferien
# scaled_var   <- umwelt$Ferien
# num_breaks   <- 10
# x_lab        <- "Ferien"
# y_lab        <- "Fussgänger:innen pro Tag"
# x_scaling    <- 1 # in prozent
# x_nk         <- 0   # x round nachkommastellen
#
#
# p_feri <- rescale_plot_fac(input_df, input_term, unscaled_var, scaled_var, num_breaks,
#                          x_lab, y_lab, x_scaling, x_nk)
# p_feri
#
# ggsave("ferien.png", width=15, height=15, units="cm", dpi=1000,
#        path = "fallstudie_s/results/")


## Phase
input_df <- Tages_Model_nb_quad
input_term <- "Phase [all]"
unscaled_var <- umwelt$Phase
scaled_var <- umwelt$Phase
num_breaks <- 10
x_lab <- "Phase"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_phase <- rescale_plot_fac(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_phase

ggsave("phase.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)


## Monat
input_df <- Tages_Model_nb_quad
input_term <- "Monat [all]"
unscaled_var <- umwelt$Monat
scaled_var <- umwelt$Monat
num_breaks <- 10
x_lab <- "Monat"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_monat <- rescale_plot_fac(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_monat

ggsave("monat.png",
       width = 15, height = 15, units = "cm", dpi = 1000,
       path = "fallstudie_s/results/"
)






# ZUSATZ: Wir haben die Wetterparameter skaliert.
# Fuer die Plots muss das beruecksichtigt werden: wir stellen nicht die wirklichen Werte
# dar sondern die skalierten. Mit folgendem Befehl kann man die Skalierung nachvollziehen:
# attributes(umwelt$tre200jx_scaled)
# Die Skalierung kann rueckgaengig gemacht werden, indem man die Skalierten werte mit
# dem scaling factor multipliziert und dann den Durchschnitt addiert:
# Bsp.: d$s.x * attr(d$s.x, 'scaled:scale') + attr(d$s.x, 'scaled:center')
# mehr dazu: https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
# --> wir bleiben aber bei den skalierten Werten, leben damit und sind uns dessen bewusst.

# Auch beim Plotten der Modellresultate gilt:
# visualisiere nur die Parameter welche nach der Modellselektion uebig bleiben
# und signifikant sind!
# plot_model / type = "pred" sagt die Werte "voraus"
# achte auf gleiche Skalierung der y-Achse (Vergleichbarkeit)

#
# # Temperatur
# t <- plot_model(Tages_Model_quad_Jahr_log10, type = "pred", terms =
#                   "tre200jx_scaled [all]", # [all] = Model contains polynomial or cubic /
#                 #quadratic terms. Consider using `terms="tre200jx_scaled [all]"`
#                 # to get smooth plots. See also package-vignette
#                 # 'Marginal Effects at Specific Values'.
#                 title = "", axis.title = c("Tagesmaximaltemperatur [°C]",
#                                            "Fussgaenger:innen pro Tag [log]"))
# # fuege die Achsenbeschriftung hinzu. Hier wird auf die unskalierten Werte zugegriffen.
# labels <- round(seq(floor(min(umwelt$tre200jx)), ceiling(max(umwelt$tre200jx)),
#                     # length.out = ___ --> Anpassen gemaess breaks auf dem Plot
#                     length.out = 5), 0)
# (Tempplot <- t +
#     scale_x_continuous(breaks = c(-2,-1,0,1,2),
#                        labels = c(labels))+
#     # fuege die y- Achsenbeschriftung hinzu. Hier transformieren wir die Werte zurueck
#     scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
#                        labels = round(c(10^0, 10^0.5, 10^1, 10^1.5, 10^2),0),
#                        limits = c(0, 2))+
#     theme_classic(base_size = 20))
































# .################################################################################################
# 5. MULTIF. STUNDE #####
# .################################################################################################

# 4.1 Einflussfaktoren Besucherzahl ####
# Erstelle ein df indem die taeglichen Zaehldaten und Meteodaten vereint sind
umwelt_h <- inner_join(depo, meteo, by = c("Datum" = "time"))


# schreibe nun eine Funktion zur zuweisung Ferien. WENN groesser als start UND kleiner als
# ende, DANN schreibe ein 1
for (i in 1:nrow(ferien)) {
  umwelt_h$Ferien[umwelt_h$Datum >= ferien[i, "Start"] & umwelt_h$Datum <= ferien[i, "End"]] <- 1
}
umwelt_h$Ferien[is.na(umwelt_h$Ferien)] <- 0

# hat das funktioniert? zaehle die anzahl Ferientage
sum(umwelt_h$Ferien)

# Faktor und integer
# Im GLMM wird das Jahr als random factor definiert. Dazu muss es als
# Faktor vorliegen. Monat und KW koennen die Besuchszahlen auch erklaeren.
# auch sie muessen faktoren sein





# DAS SOLLTE EIG SCHON GEMACHT SEIN VON WEITER OBEN. PRÜFE DAS




umwelt_h <- umwelt_h |>
  mutate(Jahr = as.factor(Jahr)) |>
  mutate(KW = as.factor(KW)) |>
  mutate(Monat = as.factor(Monat)) |>
  mutate(Ferien = as.factor(Ferien)) |>
  mutate(Stunde = as.factor(Stunde)) |>
  # zudem muessen die die nummerischen Wetterdaten auch als solche abgespeichert sein
  mutate(tre200nx = as.numeric(tre200nx)) |>
  mutate(tre200jx = as.numeric(tre200jx)) |>
  mutate(rre150j0 = as.numeric(rre150j0)) |>
  mutate(rre150n0 = as.numeric(rre150n0)) |>
  mutate(sremaxdv = as.numeric(sremaxdv))

# falls das noch zu NA's gefuehrt hat, muessen diese entfernt werden
sum(is.na(umwelt_h))
umwelt_h <- na.omit(umwelt_h)
summary(umwelt_h)
str(umwelt_h)

# Unser Modell kann nur mit ganzen Zahlen umgehen. Zum Glueck habe wir die Zaehldaten
# bereits gerundet.

# unser Datensatz muss ein df sein, damit scale funktioniert
umwelt_h <- as.data.frame(umwelt_h)

#  Variablen skalieren
# Skalieren der Variablen, damit ihr Einfluss vergleichbar wird
# (Problem verschiedene Skalen der Variablen (bspw. Temperatur in Grad Celsius,
# Niederschlag in Millimeter und Sonnenscheindauer in Minuten)
umwelt_h <- umwelt_h |>
  mutate(
    tre200jx_scaled = scale(tre200jx),
    tre200nx_scaled = scale(tre200nx),
    rre150j0_scaled = scale(rre150j0),
    rre150n0_scaled = scale(rre150n0),
    sremaxdv_scaled = scale(sremaxdv)
  )

# 4.2 Variablenselektion ####
# Korrelierende Variablen koennen das Modelergebnis verfaelschen. Daher muss vor der
# Modelldefinition auf Korrelation getestet werden.

# Erklaerende Variablen definieren
# Hier wird die Korrelation zwischen den (nummerischen) erklaerenden Variablen berechnet
cor <- cor(umwelt_h[, 21:24]) # in den [] waehle ich die skalierten Spalten.
# Mit dem folgenden Code kann eine simple Korrelationsmatrix aufgebaut werden
# hier kann auch die Schwelle für die Korrelation gesetzt werden,
# 0.7 ist liberal / 0.5 konservativ
# https://researchbasics.education.uconn.edu/r_critical_value_table/
cor[abs(cor) < 0.7] <- 0 # Setzt alle Werte kleiner 0.7 auf 0 (diese sind dann ok, alles groesser ist problematisch!)
cor


# 4.3 Pruefe Verteilung ####
# pruefe zuerst nochmals, ob wir NA im df haben:
sum(is.na(umwelt_h$Total))

f1 <- fitdist(umwelt_h$Total, "norm") # Normalverteilung
f1_1 <- fitdist((umwelt_h$Total + 1), "lnorm") # log-Normalvert (beachte, dass ich +1 rechne.
# log muss positiv sein; allerdings kann man die
# Verteilungen dann nicht mehr miteinander vergleichen).
f2 <- fitdist(umwelt_h$Total, "pois") # Poisson
f3 <- fitdist(umwelt_h$Total, "nbinom") # negativ binomial
f4 <- fitdist(umwelt_h$Total, "exp") # exponentiell
# f5<-fitdist(umwelt_h$Total,"gamma")  # gamma (berechnung mit meinen Daten nicht möglich)
f6 <- fitdist(umwelt_h$Total, "logis") # logistisch
f7 <- fitdist(umwelt_h$Total, "geom") # geometrisch
# f8<-fitdist(umwelt_h$Total,"weibull")  # Weibull (berechnung mit meinen Daten nicht möglich)

gofstat(list(f1, f2, f3, f4, f6, f7),
  fitnames = c(
    "Normalverteilung", "Poisson",
    "negativ binomial", "exponentiell", "logistisch",
    "geometrisch"
  )
)

# die 2 besten (gemaess Akaike's Information Criterion) als Plot + normalverteilt,
plot.legend <- c("Normalverteilung", "exponentiell", "negativ binomial")
# vergleicht mehrere theoretische Verteilungen mit den empirischen Daten
cdfcomp(list(f1, f4, f3), legendtext = plot.legend)

# --> Verteilung ist gemäss AICc exponentiell. negativ binomial ist auch nicht schlecht.
# --> ich entscheide mich für diese beide und probiere mit beiden Modelle aus.


# 4.4 Berechne verschiedene Modelle ####




# BERECHNE FÜR JEDE TAGESZEIT EIN EIGENES MODELL
# BEGINNE AM TAG, MACHE DANN NACHT. IN DER NACHT ANDERE WETTERVAR ALS AM TAG


# nehme monat anstatt kw im modelle auf














# Berechne ein negativ binomiales Modell
# gemäss AICc die zweitbeste Verteilung
Tages_Model_nb <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Tageszeit +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled +
  (1 | Jahr) + (1 | KW), data = umwelt_h)

summary(Tages_Model_nb)
plot(Tages_Model_nb, type = c("p", "smooth"))
qqmath(Tages_Model_nb)
dispersion_glmer(Tages_Model_nb)
r.squaredGLMM(Tages_Model_nb)
car::vif(Tages_Model_nb)




Tages_Model_nb_h <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Tageszeit +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled + Stunde +
  (1 | Jahr) + (1 | KW), data = umwelt_h)

summary(Tages_Model_nb_h)
plot(Tages_Model_nb_h, type = c("p", "smooth"))
qqmath(Tages_Model_nb_h)
dispersion_glmer(Tages_Model_nb_h)
r.squaredGLMM(Tages_Model_nb_h)
car::vif(Tages_Model_nb_h)

tab_model(Tages_Model_nb_h, transform = NULL, show.se = TRUE)






# Model testing for over/underdispersion, zeroinflation and spatial autocorrelation following the DHARMa package.
# unbedingt die Vignette des DHARMa-Package konsultieren:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

# Residuals werden über eine Simulation auf eine Standard-Skala transformiert und
# können anschliessend getestet werden. Dabei kann die Anzahl Simulationen eingestellt
# werden (dauert je nach dem sehr lange)
#
# simulationOutput <- simulateResiduals(fittedModel = m_day, n = 10000)
#
# # plotting and testing scaled residuals
#
# plot(simulationOutput)
#
# testResiduals(simulationOutput)
#
# testUniformity(simulationOutput)
#
# # The most common concern for GLMMs is overdispersion, underdispersion and
# # zero-inflation.
#
# # separate test for dispersion
#
# testDispersion(simulationOutput)
#
# # test for Zeroinflation
#
# testZeroInflation(simulationOutput)
#
# # test for spatial Autocorrelation
#
# # calculating x, y positions per group
# groupLocations = aggregate(DF_mod_day[, 3:4], list(DF_mod_day$x, DF_mod_day$y), mean)
# groupLocations$group <- paste(groupLocations$Group.1,groupLocations$Group.2)
#
# # calculating residuals per group
# res2 = recalculateResiduals(simulationOutput, group = groupLocations$group)
#
# # running the spatial test on grouped residuals
# testSpatialAutocorrelation(res2, groupLocations$x, groupLocations$y, plot = F)
#
# # Testen auf Multicollinearität (dh zu starke Korrelationen im finalen Modell, zB falls
# # auf Grund der ökologischen Plausibilität stark korrelierte Variablen im Modell)
# # use VIF values: if values less then 5 is ok (sometimes > 10), if mean of VIF values
# # not substantially greater than 1 (say 5), no need to worry.
#
# car::vif(m_day)
# mean(car::vif(m_day))











# auf quadratischen Term testen ("es gehen weniger Leute in den Wald, wenn es zu heiss ist")
Tages_Model_nb_quad <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Tageszeit +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled +
  (1 | Jahr) + (1 | KW), data = umwelt_h)

summary(Tages_Model_nb_quad)
plot(Tages_Model_nb_quad, type = c("p", "smooth"))
qqmath(Tages_Model_nb_quad)
dispersion_glmer(Tages_Model_nb_quad)
r.squaredGLMM(Tages_Model_nb_quad)
car::vif(Tages_Model_nb_quad)



# Interaktion testen, da Ferien und / oder Wochentage einen Einfluss auf
# die Besuchszahlen waehrend des Lockown haben koennen!
# (Achtung: Rechenintensiv!)
# Tages_Model_nb_int <- glmer.nb(Anzahl_Total ~  Wochentag  * Ferien + Phase +
#                                  tre200jx_scaled + I(tre200jx_scaled^2) *
#                                  rre150j0_scaled + sremaxdv_scaled +
#                                  (1|KW) + (1|Jahr), data = umwelt_h)
#
# summary(Tages_Model_nb_int)
# plot(Tages_Model_nb_int, type = c("p", "smooth"))
# qqmath(Tages_Model_nb_int)
# dispersion_glmer(Tages_Model_nb_int)
# r.squaredGLMM(Tages_Model_nb_int)


# Vergleich der Modellguete mittels AICc
cand.models <- list()
cand.models[[1]] <- Tages_Model_nb_h
cand.models[[2]] <- Tages_Model_nb

Modnames <- c("Tages_Model_nb_h", "Tages_Model_nb")
aictab(cand.set = cand.models, modnames = Modnames)
# K = Anzahl geschaetzter Parameter (2 Funktionsparameter und die Varianz)
# Delta_AICc <2 = Statistisch gleichwertig
# AICcWt =  Akaike weight in %

# --> Ich entscheide mich bei diesen drei Modellen für das Tages_Model_nb_quad
# Warum: statistisch das beste und ich denke die Quadratur macht Sinn!
# zudem wissen wir gem. Test der Verteilungen, dass negativ binomial Sinn macht.
# PROBLEM: alle drei Modelle erfüllen gem. der Modelldiagnostik die VOrausetzungen
# nicht komplett.

# Berechne ein Modell mit exponentieller Verteilung:
# gemäss AICc der Verteilung die zweitbeste
# https://stats.stackexchange.com/questions/240455/fitting-exponential-regression-model-by-mle
Tages_Model_exp <- glmer((Total + 1) ~ Wochentag + Ferien + Phase + Monat +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled + (1 | Jahr), family = Gamma(link = "log"), data = umwelt_h)

summary(Tages_Model_exp, dispersion = 1)
plot(Tages_Model_exp, type = c("p", "smooth"))
qqmath(Tages_Model_exp)
dispersion_glmer(Tages_Model_exp) # it shouldn't be over 1.4
r.squaredGLMM(Tages_Model_exp)
car::vif(Tages_Model_nb_quad)

# --> Die zweitbeste Verteilung (exp) führt auch nicht dazu, dass die Modellvoraussetzungen besser
# erfüllt werden


# 4.5 Transformationen ####
# Die Modellvoraussetzungen waren überall mehr oder weniger verletzt.
# Das ist ein Problem, allerdings auch nicht ein so grosses.
# (man sollte es aber trotzdem ernst nehmen)
# Schielzeth et al. Robustness of linear mixed‐effects models to violations of distributional assumptions
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434
# Lo and Andrews, To transform or not to transform: using generalized linear mixed models to analyse reaction time data
# https://www.frontiersin.org/articles/10.3389/fpsyg.2015.01171/full

# die Lösung ist nun, die Daten zu transformieren:
# mehr unter: https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

# berechne skewness coefficient
library(moments)
skewness(umwelt_h$Total)
# A positive value means the distribution is positively skewed (rechtsschief).
# The most frequent values are low; tail is toward the high values (on the right-hand side)

# log 10, da stark rechtsschief
Tages_Model_quad_Jahr_log10 <- lmer(log10(Total + 1) ~ Wochentag + Ferien + Phase + Monat +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled + (1 | Jahr), data = umwelt_h)
summary(Tages_Model_quad_Jahr_log10)
plot(Tages_Model_quad_Jahr_log10, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_log10)
dispersion_glmer(Tages_Model_quad_Jahr_log10)
r.squaredGLMM(Tages_Model_quad_Jahr_log10)
car::vif(Tages_Model_nb_quad)
# lmer zeigt keine p-Werte, da diese schwer zu berechnen sind. Alternative Packages berechnen diese
# anhand der Teststatistik. Achtung: die Werte sind wahrscheinlich nicht präzise!
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html
tab_model(Tages_Model_quad_Jahr_log10, transform = NULL, show.se = TRUE)


# natural log, da stark rechtsschief
Tages_Model_quad_Jahr_ln <- lmer(log(Total + 1) ~ Wochentag + Ferien + Phase + Monat +
  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled +
  sremaxdv_scaled + (1 | Jahr), data = umwelt_h)
summary(Tages_Model_quad_Jahr_ln)
plot(Tages_Model_quad_Jahr_ln, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_ln)
dispersion_glmer(Tages_Model_quad_Jahr_ln)
r.squaredGLMM(Tages_Model_quad_Jahr_ln)
car::vif(Tages_Model_nb_quad)

# --> Die Modellvoraussetzungen sind nicht deutlich besser erfüllt jetzt wo wir Transformationen
# benutzt haben. log10 und ln performen beide etwa gleich.

# Zusatz: ACHTUNG - Ruecktransformierte Regressionskoeffizienten zu erlangen (fuer die Interpretation, das Plotten),
# ist zudem nicht moeglich (Regressionskoeffizienten sind nur im transformierten Raum linear).
# Ein ruecktransformierter Regressionskoeffiziente haette eine nicht-lineare Beziehung mit der
# abhaengigen Variable.


# 4.6 Exportiere die Modellresultate ####
# (des besten Modells)
tab_model(Tages_Model_nb_quad, transform = NULL, show.se = TRUE)
# The marginal R squared values are those associated with your fixed effects,
# the conditional ones are those of your fixed effects plus the random effects.
# Usually we will be interested in the marginal effects.


# 4.7 Visualisiere Modellresultate ####

# Hintergrundinfo interaction plot:
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html

## definiere eine Funktion zum Plotten der Modellergebnisse
# Credits function to Sabrina Harsch

# original
# rescale_plot <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, x_scaling, x_nk) {
#
#   plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title="")
#   labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks+1)*x_scaling, x_nk)
#
#   custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var)-min(scaled_var))/num_breaks))
#   custom_limits <- c(min(scaled_var), max(scaled_var))
#
#   plot_id <- plot_id +
#     scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x=x_lab)) +
#     scale_y_continuous(name=NULL, labels = scales::percent_format(accuracy = 5L), limits = c(0,1),position = "left") +
#     theme_classic()
#
#   return(plot_id)
# }

# schreibe fun fuer continuierliche var
rescale_plot_num <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "")
  labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks + 1) * x_scaling, x_nk)

  custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var) - min(scaled_var)) / num_breaks))
  custom_limits <- c(min(scaled_var), max(scaled_var))

  plot_id <- plot_id +
    scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x = x_lab)) +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 65)) +
    theme_classic(base_size = 20)

  return(plot_id)
}

# schreibe fun fuer diskrete var
rescale_plot_fac <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title = "")

  plot_id <- plot_id +
    scale_y_continuous(labs(y = y_lab), limits = c(0, 65)) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  return(plot_id)
}


## Tagesmaximaltemperatur
input_df <- Tages_Model_nb_quad
input_term <- "tre200jx_scaled [all]"
unscaled_var <- umwelt_h$tre200jx
scaled_var <- umwelt_h$tre200jx_scaled
num_breaks <- 10
x_lab <- "Temperatur [°C]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_temp <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_temp

ggsave("temp.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


## sonnenscheindauer
input_df <- Tages_Model_nb_quad
input_term <- "sremaxdv_scaled [all]"
unscaled_var <- umwelt_h$sremaxdv
scaled_var <- umwelt_h$sremaxdv_scaled
num_breaks <- 10
x_lab <- "Sonnenscheindauer [%]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_sonn <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_sonn

ggsave("sonne.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


## regen tag
input_df <- Tages_Model_nb_quad
input_term <- "rre150j0_scaled [all]"
unscaled_var <- umwelt_h$rre150j0
scaled_var <- umwelt_h$rre150j0_scaled
num_breaks <- 10
x_lab <- "Regensumme 6 - 18 Uhr [mm]"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_reg <- rescale_plot_num(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_reg

ggsave("regen.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


## Wochentag
input_df <- Tages_Model_nb_quad
input_term <- "Wochentag [all]"
unscaled_var <- umwelt_h$Wochentag
scaled_var <- umwelt_h$Wochentag
num_breaks <- 10
x_lab <- "Wochentag"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_wd <- rescale_plot_fac(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_wd

ggsave("wd.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


## Ferien
# input_df     <-  Tages_Model_nb_quad
# input_term   <- "Ferien [all]"
# unscaled_var <- umwelt_h$Ferien
# scaled_var   <- umwelt_h$Ferien
# num_breaks   <- 10
# x_lab        <- "Ferien"
# y_lab        <- "Fussgänger:innen pro Tag"
# x_scaling    <- 1 # in prozent
# x_nk         <- 0   # x round nachkommastellen
#
#
# p_feri <- rescale_plot_fac(input_df, input_term, unscaled_var, scaled_var, num_breaks,
#                          x_lab, y_lab, x_scaling, x_nk)
# p_feri
#
# ggsave("ferien.png", width=15, height=15, units="cm", dpi=1000,
#        path = "fallstudie_s/results/")


## Phase
input_df <- Tages_Model_nb_quad
input_term <- "Phase [all]"
unscaled_var <- umwelt_h$Phase
scaled_var <- umwelt_h$Phase
num_breaks <- 10
x_lab <- "Phase"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_phase <- rescale_plot_fac(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_phase

ggsave("phase.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)


## Monat
input_df <- Tages_Model_nb_quad
input_term <- "Monat [all]"
unscaled_var <- umwelt_h$Monat
scaled_var <- umwelt_h$Monat
num_breaks <- 10
x_lab <- "Monat"
y_lab <- "Fussgänger:innen pro Tag"
x_scaling <- 1 # in prozent
x_nk <- 0 # x round nachkommastellen


p_monat <- rescale_plot_fac(
  input_df, input_term, unscaled_var, scaled_var, num_breaks,
  x_lab, y_lab, x_scaling, x_nk
)
p_monat

ggsave("monat.png",
  width = 15, height = 15, units = "cm", dpi = 1000,
  path = "fallstudie_s/results/"
)
