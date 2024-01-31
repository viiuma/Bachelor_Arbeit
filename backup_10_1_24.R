library(dplyr)
library(tidyverse)
library(ggpmisc)
library(splines)
library(lubridate)


# Daten einlesen und bereinigen ####
setwd("~/Desktop/ETH/BA_VILMA/R_Code") 

dat23 <- read.csv("WW_traits_PhenomEn_2023.csv", sep = ";")
dat22 <- read.csv("WW_traits_PhenomEn_2022.csv", sep = ";")

# Site und Parcel in Faktoren umwandeln
dat23$SiteName <- factor(dat23$SiteName)
dat23$ParcelName <- factor(dat23$ParcelName)
str(dat23)

dat22$SiteName <- factor(dat22$SiteName)
dat22$ParcelName <- factor(dat22$ParcelName)
str(dat22)

# Spalten umbenennen für einfacheres Arbeiten
names(dat23)
names(dat23) <- c("Time", "SiteName", "ParcelName", "PointID", "GreenCanopyCover", "X", "Y", 
                  "BBCH", "Date", "GLAI", "DryBiomass", "DAS", "GDD")
names(dat23)

names(dat22) <- c("Time", "SiteName", "ParcelName", "PointID", "GreenCanopyCover", "GDD", "X", "Y", "BBCH", "GLAI", "DryBiomass", "DAS")
names(dat22)

# Duplikate entfernen
dat22 <- dat22 %>% distinct()
head(dat22)

# Spalten Date (2022) und Year hinzufügen, Biomasse in kg/ha umrechnen bei 2023
dat22$Date <- NA
dat22$Year <- 2022
dat23$Year <- 2023
dat23$DryBiomass <- dat23$DryBiomass*10000

# Spalte Datum hinzufügen bei Daten von 2022 durch splitten von String "Time"
split <- strsplit(dat22$Time, " ")
for (i in (1:319)) {
  dat22$Date[i] <- split[[i]][1]
}

# Datensätze zusammenführen zu einem
dat <- rbind(dat22, dat23)
dat$Year <- factor(dat$Year)
str(dat)

# Entfernen von obersten und untersten 5%
l <- quantile(dat$DryBiomass, probs = 0.05, na.rm = TRUE)
u <- quantile(dat$DryBiomass, probs = 0.95, na.rm = TRUE)

for (i in (1:nrow(dat))) {
  if (is.na(dat$DryBiomass[i]) == FALSE){
    if (dat$DryBiomass[i] < l | dat$DryBiomass[i] > u){
      dat$DryBiomass[i] <- NA
    }
  }
}

l <- quantile(dat$GLAI, probs = 0.05, na.rm = TRUE)
u <- quantile(dat$GLAI, probs = 0.95, na.rm = TRUE)

for (i in (1:nrow(dat))) {
  if (is.na(dat$GLAI[i]) == FALSE){
    if (dat$GLAI[i] < l | dat$GLAI[i] > u){
      dat$GLAI[i] <- NA
    }
  }
}

l <- quantile(dat$GreenCanopyCover, probs = 0.05, na.rm = TRUE)
u <- quantile(dat$GreenCanopyCover, probs = 0.95, na.rm = TRUE)

for (i in (1:nrow(dat))) {
  if (is.na(dat$GreenCanopyCover[i]) == FALSE){
    if (dat$GreenCanopyCover[i] < l | dat$GreenCanopyCover[i] > u){
      dat$GreenCanopyCover[i] <- NA
    }
  }
}


# Entfernen von BBCH kleiner als 20 oder grösser als 59 
x <- which(with(dat, (BBCH > 59 | BBCH < 20 | (BBCH >= 40 & BBCH < 50))))
dat <- dat[-x,]



# Makrostadien zum Datensatz hinzufügen ####
for (i in (1:nrow(dat))) {
  if (dat$BBCH[i] < 30 && dat$BBCH[i] >= 20) {
    dat$Makro[i] <- "20 - 29"
  }
}


for (i in (1:nrow(dat))) {
  if (dat$BBCH[i] < 40 && dat$BBCH[i] >= 30) {
    dat$Makro[i] <- "30 - 39"
  }
}

for (i in (1:nrow(dat))) {
  if (dat$BBCH[i] < 60 && dat$BBCH[i] >= 50) {
    dat$Makro[i] <- "50 - 59"
  }
}

# Als Faktor definieren
dat$Makro <- factor(dat$Makro)



# Meteodaten zu Datensatz hinzufügen ####
# 2022
a_prec <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_arenenberg_daily_precipitation_sum_meteoswiss.csv")
a_temp <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_arenenberg_daily_mean_temperature_meteoswiss.csv")
SH_prec22 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_strickhof_daily_precipitation_sum_agrometeo.csv")
SH_temp22 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_strickhof_daily_mean_temperature_agrometeo.csv")
SFF_prec22 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_swissfuturefarm_daily_precipitation_sum_meteoswiss.csv")
SFF_temp22 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_swissfuturefarm_daily_mean_temperature_meteoswiss.csv")
w_prec <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_witzwil_daily_precipitation_sum_meteoswiss.csv")
w_temp <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_22/phenomen_witzwil_daily_mean_temperature_meteoswiss.csv")
# 2023
SH_temp23 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_23/phenomen_strickhof_daily_mean_temperature_agrometeo.csv")
SH_prec23 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_23/phenomen_strickhof_daily_precipitation_sum_agrometeo.csv")
SFF_temp23 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_23/phenomen_swissfuturefarm_daily_mean_temperature_meteoswiss.csv")
SFF_prec23 <- read.csv("~/Desktop/ETH/BA_VILMA/R_Code/meteo_23/phenomen_swissfuturefarm_daily_precipitation_sum_meteoswiss.csv")

names(SH_temp22)[1] <- "Date"
names(SH_temp23)[1] <- "Date"
names(SH_prec22)[1] <- "Date"
names(SH_prec23)[1] <- "Date"
names(SFF_temp22)[1] <- "Date"
names(SFF_temp23)[1] <- "Date"
names(SFF_prec22)[1] <- "Date"
names(SFF_prec23)[1] <- "Date"
names(a_temp)[1] <- "Date"
names(a_prec)[1] <- "Date"
names(w_temp)[1] <- "Date"
names(w_prec)[1] <- "Date"

# Spalten Temperatur und Niederschlag hinzufügen
dat$Temp <- NA
dat$Prec <- NA


# Datum anpassen damit beide Datensätze gleiches Format im Datum haben 
# Strickhof Temp + Perc
SH_temp22$Date <- format(as.Date(SH_temp22$Date, format = "%d.%m.%Y"), "%d.%m.%y")
SH_temp23$Date <- format(as.Date(SH_temp23$Date, format = "%d.%m.%Y"), "%d.%m.%y")

SH_prec22$Date <- format(as.Date(SH_prec22$Date, format = "%d.%m.%Y"), "%d.%m.%y")
SH_prec23$Date <- format(as.Date(SH_prec23$Date, format = "%d.%m.%Y"), "%d.%m.%y")

# Swiss Future Farm Temp + Perc
SFF_temp22$Date <- ymd(SFF_temp22$Date)              
SFF_temp22$Date <- format(as.Date(SFF_temp22$Date, format = "%Y-%m-%d"), "%d.%m.%y")
SFF_temp23$Date <- ymd(SFF_temp23$Date)
SFF_temp23$Date <- format(as.Date(SFF_temp23$Date, format = "%Y-%m-%d"), "%d.%m.%y")

SFF_prec22$Date <- ymd(SFF_prec22$Date)              
SFF_prec22$Date <- format(as.Date(SFF_prec22$Date, format = "%Y-%m-%d"), "%d.%m.%y")
SFF_prec23$Date <- ymd(SFF_prec23$Date)
SFF_prec23$Date <- format(as.Date(SFF_prec23$Date, format = "%Y-%m-%d"), "%d.%m.%y")

# Arenenberg Temp + Perc
a_temp$Date <- ymd(a_temp$Date)
a_temp$Date <- format(as.Date(a_temp$Date, format = "%Y-%m-%d"), "%d.%m.%y")
a_prec$Date <- ymd(a_prec$Date)
a_prec$Date <- format(as.Date(a_prec$Date, format = "%Y-%m-%d"), "%d.%m.%y")

# Witzwil
w_temp$Date <- ymd(w_temp$Date)
w_temp$Date <- format(as.Date(w_temp$Date, format = "%Y-%m-%d"), "%d.%m.%y")
w_prec$Date <- ymd(w_prec$Date)
w_prec$Date <- format(as.Date(w_prec$Date, format = "%Y-%m-%d"), "%d.%m.%y")
w_prec$precip_tot <- as.numeric(w_prec$precip_tot)


# For-Schleifen um Temperaturen zu übertragen in dat, je eine Schleife pro Standort und Jahr
# Strickhof
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SH_temp22))) {
    if (dat$SiteName[i] == "Strickhof" & dat$Date[i] == SH_temp22$Date[j]) {
      dat$Temp[i] <- SH_temp22$T_mean[j]
    }
  }
}

for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SH_temp23))) {
    if (dat$SiteName[i] == "Strickhof" & dat$Date[i] == SH_temp23$Date[j]) {
      dat$Temp[i] <- SH_temp23$T_mean[j]
    }
  }
}

# Swiss Future Farm
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SFF_temp22))) {
    if (dat$SiteName[i] == "SwissFutureFarm" & dat$Date[i] == SFF_temp22$Date[j]) {
      dat$Temp[i] <- SFF_temp22$T_mean[j]
    }
  }
}

for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SFF_temp23))) {
    if (dat$SiteName[i] == "SwissFutureFarm" & dat$Date[i] == SFF_temp23$Date[j]) {
      dat$Temp[i] <- SFF_temp23$T_mean[j]
    }
  }
}

# Arenenberg
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(a_temp))) {
    if (dat$SiteName[i] == "Arenenberg" & dat$Date[i] == a_temp$Date[j]) {
      dat$Temp[i] <- a_temp$T_mean[j]
    }
  }
}

# Witzwil
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(w_temp))) {
    if (dat$SiteName[i] == "Witzwil" & dat$Date[i] == w_temp$Date[j]) {
      dat$Temp[i] <- w_temp$T_mean[j]
    }
  }
}

# For Schleifen für Niederschlag -> Error "replacement has length zero" ??
# Strickhof
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SH_prec22))) {
    if (dat$SiteName[i] == "Strickhof" & dat$Date[i] == SH_prec22$Date[j]) {
      dat$Prec[i] <- SH_prec22$precip_tot[j]
    }
  }
}

for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SH_prec23))) {
    if (dat$SiteName[i] == "Strickhof" & dat$Date[i] == SH_prec23$Date[j]) {
      dat$Prec[i] <- SH_prec23$precip_tot[j]
    }
  }
}

# Swiss Future Farm
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SFF_prec22))) {
    if (dat$SiteName[i] == "SwissFutureFarm" & dat$Date[i] == SFF_prec22$Date[j]) {
      dat$Prec[i] <- SFF_prec22$precip_tot[j]
    }
  }
}

for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SFF_prec23))) {
    if (dat$SiteName[i] == "SwissFutureFarm" & dat$Date[i] == SFF_prec23$Date[j]) {
      dat$Temp[i] <- SFF_prec23$precip_tot[j]
    }
  }
}
# Arenenberg
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(a_prec))) {
    if (dat$SiteName[i] == "Arenenberg" & dat$Date[i] == a_prec$Date[j]) {
      dat$Prec[i] <- a_prec$precip_tot[j]
    }
  }
}
# Witzwil
for (i in (1:nrow(dat))) {
  for (j in (1:nrow(SFF_prec22))) {
    if (dat$SiteName[i] == "Witzwil" & dat$Date[i] == SFF_prec22$Date[j]) {
      dat$Prec[i] <- SFF_prec22$precip_tot[j]
    }
  }
}

# Streudiagramme der drei Traits Canopy Cover, LAI und Biomasse, beide Jahre und nach Jahr unterteilt ####
pairs(~ GLAI + GreenCanopyCover + DryBiomass, data = dat, labels = c("GLAI", "Green Canopy Cover", 
                                                                       "Dry Biomass"), main = "2022 + 2023")

# 2022
pairs(~ GLAI + GreenCanopyCover + DryBiomass, data = subset(dat, Year == 2022), labels = c("GLAI", "Green Canopy Cover", 
                                                                       "Dry Biomass"), main = "2022")

# 2023
pairs(~ GLAI + GreenCanopyCover + DryBiomass, data = subset(dat, Year == 2023), labels = c("GLAI", "Green Canopy Cover", 
                                                                       "Dry Biomass"), main = "2023")


# Anzahl Messungen ####
dat %>%
  ggplot() +
  aes(x = Year) +
  geom_bar() +
  labs(y = "Anzahl Messungen", x = "Jahr", title = "Anzahl Messungen pro Jahr")+
  theme(plot.title = element_text(hjust = 0.5))

# Anzahl Messungen pro Standort
dat %>%
  ggplot() +
  aes(x = SiteName, fill = Year) +
  geom_bar() +
  labs(y = "Anzahl Messungen", title = "Anzahl Messungen pro Standort", x = "Standort", fill = "Jahr")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  ylim(0,300)+
  annotate(geom="text", x=1, y=35, label="n = 20",
           color="black")+
  annotate(geom="text", x=2, y=253, label="n = 238",
           color="black")+
  annotate(geom="text", x=3, y=179, label="n = 164",
           color="black")+
  annotate(geom="text", x=4, y=40, label="n = 25",
           color="black")

# Anzahl Messungen pro BBCH
dat %>%
  ggplot() +
  aes(x = BBCH, fill = Year) +
  geom_bar() +
  labs(y = "Anzahl Messungen")

# Anzahl Messungen pro BBCH Makrostadium
dat %>%
  ggplot() +
  geom_bar(aes(x = Makro, fill = Year)) +
  labs(y = "Anzahl Messungen", x = "BBCH", title = "Anzahl Messungen pro BBCH-Makrostadium", fill = "Jahr")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  annotate(geom="text", x=3, y=66, label="n = 51",
           color="black")+
  annotate(geom="text", x=2, y=286, label="n = 271",
           color="black")+
  annotate(geom="text", x=1, y=140, label="n = 125",
           color="black")+
  ylim(0,300)

# Übersicht des Datensatzes mit summary
summary(dat)
write.csv(summary(dat), "summary.csv")
# Boxplot mit Daten nach Jahr unterteilt (Streuung von GLAI, Biomasse, Canopy Cover) ####
# Biomasse
dat %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = DryBiomass))+
  labs(x = "Jahr", y = "Biomasse [kg/ha]", title = "Streuung von Biomasse")+
  theme(plot.title = element_text(hjust = 0.5))

# GLAI
dat %>% 
  ggplot()+
  geom_boxplot(aes(x = Year, y = GLAI))+
  labs(x = "Jahr", y = "GLAI", title = "Streuung von GLAI")+
  theme(plot.title = element_text(hjust = 0.5))

# Canopy Cover
dat %>%
  ggplot()+
  geom_boxplot(aes(x=Year, y = GreenCanopyCover))+
  labs(x= "Jahr", y = "Green Canopy Cover (%)", title = "Streuung von Green Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5))


# Plots nach Standort ####
# Subset mit Standorten Strickhof und Swiss Future Farm (nur von diesen beiden Standorten Daten für beide Jahre vorhanden)
site <- subset(dat, SiteName == "Strickhof" | SiteName == "SwissFutureFarm")
site <- droplevels(site)
str(site)

# GLAI
site %>%
  ggplot()+
  geom_boxplot(aes(x = SiteName, y = GLAI))+
  labs(x = "Standort", title = "GLAI nach Standort")+
  theme(plot.title = element_text(hjust = 0.5))

site %>%
  ggplot()+
  geom_boxplot(aes(x = SiteName, y = GLAI))+
  facet_wrap(~Year)+
  ylab(bquote('GLAI [m'^2~'/ m'^2~']'))+
  labs(x = "Standort")

# Biomasse
site %>%
  ggplot()+
  geom_boxplot(aes(x = SiteName, y = DryBiomass))+
  labs(x = "Standort", y = "Biomasse (kg/ha)", title = "Biomasse nach Standort")+
  theme(plot.title = element_text(hjust = 0.5))

site %>%
  ggplot()+
  geom_boxplot(aes(x = SiteName, y = DryBiomass))+
  facet_wrap(~Year)+
  labs(x = "Standort", y = "Biomasse [kg/ha]")

# Canopy Cover
site %>%
  ggplot()+
  geom_boxplot(aes(x = SiteName, y = GreenCanopyCover))+
  labs(x = "Standort", y = "Green Canpy Cover (%)", title = "Canopy Cover nach Standort")+
  theme(plot.title = element_text(hjust = 0.5))

site %>%
  ggplot()+
  geom_boxplot(aes(x = SiteName, y = GreenCanopyCover))+
  facet_wrap(~Year)+
  labs(x = "Standort", y = "GreenCanopyCover [%]")

# Plots der Traits gegenüber BBCH ####
# LAI
dat %>%
  ggplot(aes(x = BBCH, y = GLAI)) +
  geom_point(aes(colour = Year))

boxplot(dat$GLAI ~ dat$BBCH)
# Biomasse
dat %>%
  ggplot(aes(x = BBCH, y = DryBiomass))+
  geom_point(aes(colour = Year))+
  labs(y = "Biomasse (kg/ha)")

boxplot(dat$DryBiomass ~ dat$BBCH)
# Canopy Cover  
dat %>%
  ggplot(aes(x = BBCH, y = GreenCanopyCover))+
  geom_point(aes(colour = Year))+
  labs(y = "Green Canopy Cover (%)")

# Boxplots nach Makrostadien BBCH nach Jahren und Standort ####
# GLAI
jpeg("glai_bbch.jpeg", width=2500, height=1675, res=300)
dat %>%
  ggplot(aes(x = Makro, y = GLAI))+
  geom_boxplot()+
  facet_wrap(~Year)+
  ylab(bquote('GLAI [ m'^2~'/m'^2~']'))+
  labs(x = "BBCH")
dev.off()

site %>%
  ggplot(aes(x = Makro, y = GLAI)) +
  geom_boxplot() +
  facet_wrap(~Year+SiteName) +
  labs(x = "BBCH")

# Biomasse
jpeg("biomasse_bbch.jpeg", width=2500, height=1675, res=300)
dat %>%
  ggplot(aes(x = Makro, y = DryBiomass)) +
  geom_boxplot() + 
  facet_wrap(~Year) +
  labs(x = "BBCH", y = "Biomasse [kg/ha]")
dev.off()

site %>%
  ggplot(aes(x = Makro, y = DryBiomass))+
  geom_boxplot()+
  facet_wrap(~Year + SiteName)+
  labs(x = "BBCH", y = "Biomasse (kg/ha)")

# Canopy Cover
jpeg("cc_bbch.jpeg", width=2500, height=1675, res=300)
dat %>%
  ggplot(aes(x = Makro, y = GreenCanopyCover))+
  geom_boxplot()+
  facet_wrap(~Year)+
  labs(x = "BBCH", y = "Green Canopy Cover (%)")
dev.off()

site %>%
  ggplot(aes(x = Makro, y = GreenCanopyCover))+
  geom_boxplot()+
  facet_wrap(~Year + SiteName)+
  labs(x = "BBCH", y = "Green Canopy Cover (%)")


# LAI und Biomasse im Verlauf der "Zeit" mit GDD####
dat %>% 
  ggplot(aes(x = GDD, y = GLAI))+
  geom_point(aes(colour = Year))

dat %>%
  ggplot(aes(x = GDD, y = GLAI))+
  geom_point()+
  facet_wrap(~Year)

# Biomasse im zeitlichen Verlauf mit GDD
dat %>%
  ggplot(aes(x = GDD, y = DryBiomass))+
  geom_point(aes(colour = Year))+
  labs(y = "Biomasse (kg/ha)", colour = " ")
  

dat %>%
  ggplot(aes(x = GDD, y = DryBiomass))+
  geom_point()+
  facet_wrap(~Year)+
  labs(y = "Biomasse (kg/ha)")

# CC
dat %>%
  ggplot(aes(x = GDD, y = GreenCanopyCover))+
  geom_point(aes(colour = SiteName))+
  labs(y = "Green Canopy Cover (%)")

# GDD in Zsh mit BBCH?
dat %>%
  ggplot(aes(x = BBCH, y = GDD))+
  geom_point()

dat %>%
  ggplot(aes(x = BBCH, y = GDD))+
  geom_point()+
  facet_wrap(~Year)

dat %>%
  ggplot(aes(x = Makro, y = GDD))+
  geom_boxplot()+
  facet_wrap(~Year)+
  labs(x = "BBCH")



# Biomasse vs. GLAI beide Jahre ####
# GLAI vs. Biomasse
dat %>%
  ggplot(aes(x = DryBiomass, y = GLAI))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE)+
  labs(x = "Biomasse (kg/ha)", y = "GLAI", title = "GLAI vs. Biomasse 2022 / 2023", colour = "Makrostadium")+
  theme(plot.title = element_text(face='bold', hjust = 0.5))


# Biomasse vs. GLAI
dat %>%
  ggplot(aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(col= Makro))+
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse (kg/ha)", title = "Biomasse vs. GLAI 2022 / 2023", colour = "Makrostadium")+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))
  # Plot mit Formel weiter unten (nach fit)

# log(Biomasse) vs. GLAI
jpeg("biomasse_log_glai.jpeg", width=2500, height=1675, res=300)
dat %>%  
  ggplot(aes(x = GLAI, y = log(DryBiomass))) +
  stat_poly_line(colour = "black")+
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point(aes(colour = Makro))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlab(bquote('GLAI [ m'^2~'/m'^2~']'))+
  labs(y = "ln(Biomasse [kg / ha])", title = "Biomasse (logarithmiert) vs. GLAI", colour = "Makrostadium", subtitle = "n = 181")+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
dev.off()

# LM log(Dry Biomass) ~ GLAI alle Daten
fit <- lm(log(DryBiomass)~GLAI, data = dat)
summary(fit)
# Regressionsformel: log(y) = 6.671217 + 0.39844 * x

# Residuen
qqnorm(rstandard(fit))
qqline(rstandard(fit))
hist(rstandard(fit))

# Plot von oben (Biomasse vs. GLAI) mit Formel aus fit abgeleitet
# Koeffizienten für Formel
a <- round(summary(fit)$coefficients[1,1], 2)
b <- round(summary(fit)$coefficients[2,1], 2)
r.squared = 0.79

eq <- paste0('y~`=`~e^{', a , '+', b , '*x}*", "*R^2~`=`~', r.squared)

# Biomasse vs. GLAI mit Formel
jpeg("biomasse_glai.jpeg", width=2500, height=1675, res=300)
dat %>%
  ggplot(aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(col= Makro))+
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), color = "black")+
  labs(y = "Biomasse [kg / ha]", x = "GLAI", title = "Biomasse vs. GLAI 2022 / 2023", 
       colour = "Makrostadium", subtitle = "n = 181")+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlab(bquote('GLAI [m'^2~'/ m'^2~']'))+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  annotate("text", x=1.5, y=12000, label = eq, parse =TRUE)
dev.off()
# Biomasse vs. GLAI 2022 und 2023 einzeln ####
# Biomasse vs. GLAI
ggplot(data = dat, aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  labs(y = "Biomasse (kg/ha)", title = " Biomasse vs. GLAI")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)

# GLAI vs. Biomasse
ggplot(data = dat, aes(x = DryBiomass, y = GLAI))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "lm", formula = y ~ log(x))+
  labs(x = "Biomasse (kg/ha)", title = "GLAI vs. Biomasse")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  facet_wrap(~Year)

# log(Biomasse) vs. GLAI
jpeg("biomasse_log_glai_22_23.jpeg", width=2500, height=1675, res=300)
ggplot(data = dat, aes(x = GLAI, y = log(DryBiomass))) +
  stat_poly_line(colour = "black")+
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point(aes(colour = Makro))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  labs(y = "ln(Biomasse [kg/ha])", title = "Biomasse (logarithmiert) vs. GLAI", colour = "Makrostadien")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  xlab(bquote('GLAI [ m'^2~'/m'^2~']'))+
  facet_wrap(~Year)
dev.off()


# LM log(Dry Biomass) ~ GLAI 2022
fit.22 <- lm(log(DryBiomass)~GLAI, data = subset(dat, Year == 2022))
summary(fit.22)
# Regressionsformel: log(y) = 6.74915 + 0.43510 * x

# Residuen
qqnorm(rstandard(fit.22))
qqline(rstandard(fit.22))
hist(rstandard(fit.22))

# LM log(Dry Biomass) ~ GLAI 2023
fit.23 <- lm(log(DryBiomass)~GLAI, data = subset(dat, Year == 2023))
summary(fit.23)
# Regressionsformel: log(y) = 6.62324 + 0.38305 * x

# Residuen
qqnorm(rstandard(fit.23))
qqline(rstandard(fit.23))
hist(rstandard(fit.23))


# Biomasse vs. GLAI beide Jahre einzeln, optimierte Plots mit Formeln
# 2022
a.22 <- round(summary(fit.22)$coefficients[1,1], 2)
b.22 <- round(summary(fit.22)$coefficients[2,1], 2)
r22.squared = 0.80

eq.22 <- paste0('y~`=`~e^{', a.22 , '+', b.22 , '*x}*", "*R^2~`=`~', r22.squared)

ggplot(data = subset(dat, Year == 2022), aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse (kg/ha)", title = " Biomasse vs. GLAI 2022", subtitle = "n = 89", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))+
  annotate("text", x=1.5, y=13500, label = eq.22, parse =TRUE)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlim(0,7)+
  ylim(0, 15000)

# 2023
a.23 <- round(summary(fit.23)$coefficients[1,1], 2)
b.23 <- round(summary(fit.23)$coefficients[2,1], 2)
r23.squared = 0.89

eq.23 <- paste0('y~`=`~e^{', a.23 , '+', b.23 , '*x}*", "*R^2~`=`~', r23.squared)

ggplot(data = subset(dat, Year == 2023), aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse (kg/ha)", title = " Biomasse vs. GLAI 2023", subtitle = "n = 92", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))+
  annotate("text", x=1.5, y=13500, label = eq.23, parse =TRUE)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlim(0,7)+
  ylim(0,15000)


# Biomasse vs. GLAI nach Standort ####
ggplot(data = site, aes(x = GLAI, y = DryBiomass))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse (kg/ha)", title = "Biomasse vs. GLAI")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year+SiteName)

# Strickhof
# Biomasse vs. GLAI
ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse [kg/ha]", title = "Biomasse vs. GLAI Strickhof")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
  
# Swiss Future Farm
# Biomasse vs. GLAI
ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse (kg/ha)", title = "Biomasse vs. GLAI, Swiss Future Farm")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# LM Strikchof
fit1.SH <- lm(log(DryBiomass)~GLAI, data = subset(dat, SiteName == "Strickhof"))
summary(fit1.SH)

# Residuen
qqnorm(rstandard(fit1.SH))
qqline(rstandard(fit1.SH))
hist(rstandard(fit1.SH))

# LM Swiss Future Farm
fit1.SFF <- lm(log(DryBiomass)~GLAI, data = subset(dat, SiteName == "SwissFutureFarm"))
summary(fit1.SFF)

# Residuen
qqnorm(rstandard(fit1.SFF))
qqline(rstandard(fit1.SFF))
hist(rstandard(fit1.SFF))
# Residuen normalverteilt ?

# Optimierte Plots Standort Biomasse vs. GLAI
# Strickhof
a.SH <- round(summary(fit1.SH)$coefficients[1,1], 2)
b.SH <- round(summary(fit1.SH)$coefficients[2,1], 2)
rSH.squared = 0.74

eq.SH <- paste0('y~`=`~e^{', a.SH , '+', b.SH , '*x}*", "*R^2~`=`~', rSH.squared)

ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse [kg / ha]", title = "Biomasse vs. GLAI, Strickhof", subtitle = "n = 111", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))+
  annotate("text", x=1.5, y=13500, label = eq.SH, parse =TRUE)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlab(bquote('GLAI [m'^2~'/ m'^2~']'))+
  ylim(0,15000)+
  xlim(0,7)

# log(biomasse)
jpeg("biomasse_log_glai_strickhof.jpeg", width=2500, height=1675, res=300)
ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GLAI, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  stat_poly_line(colour = "black")+
  stat_poly_eq(use_label(c("eq", "R2")))+
  labs(y = "ln(Biomasse [kg / ha])", title = "Biomasse (logarithmiert) vs. GLAI, Strickhof", colour = "Makrostadium", subtitle = "n = 111")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlab(bquote('GLAI [m'^2~'/ m'^2~']'))+
  xlim(0,7)+
  ylim(6,10)
dev.off()

# Swiss Future Farm
a.SFF <- round(summary(fit1.SFF)$coefficients[1,1], 2)
b.SFF <- round(summary(fit1.SFF)$coefficients[2,1], 2)
rSFF.squared = 0.78

eq.SFF <- paste0('y~`=`~e^{', a.SFF , '+', b.SFF , '*x}*", "*R^2~`=`~', rSFF.squared)

ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GLAI, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')), colour = "black")+
  labs(y = "Biomasse [kg / ha]", title = "Biomasse vs. GLAI, Swiss Future Farm", colour = "Makrostadium", subtitle = "n = 51")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))+
  annotate("text", x=1.5, y=13500, label = eq.SFF, parse =TRUE)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlab(bquote('GLAI [m'^2~'/ m'^2~']'))+
  ylim(0,15000)+
  xlim(0,7)

# log(biomasse)
jpeg("biomasse_log_glai_SFF.jpeg", width = 2500, height = 1675, res = 300)
ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GLAI, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  stat_poly_line(colour = "black")+
  stat_poly_eq(use_label(c("eq", "R2")))+
  labs(y = "ln(Biomasse [kg / ha])", title = "Biomasse (logarithmiert) vs. GLAI, Swiss Future Farm", colour = "Makrostadium", subtitle = "n = 51")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  xlab(bquote('GLAI [m'^2~'/ m'^2~']'))+
  xlim(0,7)+
  ylim(6,10)
dev.off()


# Biomasse vs. GLAI Anwendung eines Jahres aufs andere ####
# Daten von 2023 in Formel von 2022 einsetzen
x <- dat$GLAI[dat$Year == 2023]
a <- summary(fit.22)$coefficients[1,1]
b <- summary(fit.22)$coefficients[2,1]

estimated <- exp(a) * (exp(b))^x
ggplot(data = subset(dat, Year == 2023), aes(x = DryBiomass, y = estimated))+
  geom_point()+
  stat_smooth(method="lm", se = FALSE, aes(colour = "Lineare Regression"), size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0, colour = "1:1"), show.legend = FALSE)+ # wie Legende anpassen? (y = x, Regressionslinie)
  scale_colour_manual(values=c("1:1"="blue", "Lineare Regression"="red"))+
  labs(x = "Gemessene Werte Biomasse [kg/ha] 2023", y = "Erwartete Werte für Biomasse [kg/ha]", title = "Erwartete vs. reale Werte für Biomasse 2023", colour = " ")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), legend.position = c(.15, .9), legend.background = element_rect(fill = 'transparent'))+
  xlim(0, 15000)+
  ylim(0, 15000)


# Residuen
qqnorm(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2023])))
qqline(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2023])))
hist(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2023])))

# Daten von 2022 in Formel von 2023
x <- dat$GLAI[dat$Year == 2022]
a <- summary(fit.23)$coefficients[1,1]
b <- summary(fit.23)$coefficients[2,1]

estimated <- exp(a) * (exp(b))^x
ggplot(data = subset(dat, Year == 2022), aes(x = DryBiomass, y = estimated))+
  geom_point()+
  geom_abline(aes(colour ="1:1", intercept = 0, slope = 1), show.legend = FALSE)+
  stat_smooth(method="lm", se = FALSE, aes(colour = "Lineare Regression"), size = 0.5)+
  scale_colour_manual(values=c("1:1"="blue", "Lineare Regression"="red"))+
  labs(x = "Gemessene Werte Biomasse [kg/ha] 2022", y = "Erwartete Werte für Biomasse [kg/ha]", title = "Erwartete vs. reale Werte für Biomasse 2022", colour = " ")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), legend.position = c(.15, .90), legend.background = element_rect(fill='transparent'))+
  xlim(0, 15000)+
  ylim(0, 15000)


qqnorm(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2022])))
qqline(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2022])))
hist(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2022])))

# Biomasse vs. Canopy Cover beide Jahre ####
# Canopy Cover vs. Biomasse
dat %>%
  ggplot(aes(x = DryBiomass, y = GreenCanopyCover))+
  geom_point(aes(colour = Makro))+
  #geom_smooth(method = "lm", formula = y ~ log(x))+
  geom_smooth(span = 0.99, se = FALSE)+
  labs(x = "Biomasse (kg/ha)", y = "Green Canopy Cover (%)", title = "Canopy Cover vs. Biomasse 2022 / 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# Abnahmevon CC ab BBCH 50 - 59?

# Biomasse vs. Canopy Cover
dat %>%
  ggplot(aes(y = DryBiomass, x = GreenCanopyCover))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = gaussian(link = 'log')))+
  labs(y = "Biomasse (kg/ha)", x = "Green Canopy Cover (%)", title = "Biomasse vs. Canopy Cover 2022 / 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))


# LOESS
dat %>%
  ggplot(aes(y = DryBiomass, x = GreenCanopyCover))+
  geom_point(aes(colour = Makro))+
  geom_smooth(se = FALSE)+
  labs(y = "Biomasse (kg/ha)", x = "Green Canopy Cover (%)", title = "Biomasse vs. Canopy Cover 2022 / 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# log(Biomasse) vs. Canopy Cover
jpeg("biomasse_log_cc.jpeg", width=2500, height=1675, res=300)
dat %>%
  ggplot(aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  stat_poly_line(colour = "black")+
  stat_poly_eq(use_label(c("eq", "R2")))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  labs(x = "Green Canopy Cover [%]", y = "ln(Biomasse [kg/ha])", 
       title = "Biomasse (logarithmiert) vs. Canopy Cover 2022 / 2023", subtitle = "n = 285", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face ='bold'), plot.subtitle = element_text(hjust = 0.5))
dev.off()

#Loess
dat %>%
  ggplot(aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  geom_smooth(span = 0.8, se = FALSE)
  labs(x = "Green Canopy Cover", y = "log(Biomasse)", title = "Biomasse (logarithmiert) vs. Canopy Cover 2022 / 2023")+
  theme(plot.title = element_text(hjust = 0.5, face ='bold'))
  # BBCH 20 -29 und 30 - 39 erkennbar in Regression
  
# LM log(Biomasse) ~ Canopy Cover
fit2 <- lm(log(DryBiomass) ~ GreenCanopyCover, data = dat)
summary(fit2)
hist(rstandard(fit2))
qqnorm(rstandard(fit2))
qqline(rstandard(fit2))
# Regressionsformel: log(y) = 6.647486 + 0.024147*x, umgeformt y = 770.844*1.024^x



# Biomasse vs. Canopy Cover 2022 und 2023 einzeln ####
ggplot(data = dat, aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  labs(x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)", title = "Biomasse vs. Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  geom_smooth(method="glm", method.args = list(family = gaussian(link = 'log')))+
  facet_wrap(~Year)
# Loess
ggplot(data = dat, aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  labs(x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)", title = "Biomasse vs. Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  geom_smooth()+
  facet_wrap(~Year)
# log(Biomasse)
jpeg("biomasse_log_cc_22_23.jpeg", width=2500, height=1675, res=300)
ggplot(data = dat, aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq", "R2")))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  labs(x = "Green Canopy Cover [%]", y = "log(Biomasse [kg/ha])", title = "Biomasse (logarithmiert) vs. Canopy Cover", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)
dev.off()
#loess
ggplot(data = dat, aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  geom_smooth(span = 0.9)+
  labs(x = "Green Canopy Cover (%)", y = "log(Biomasse)", title = "Biomasse (logarithmiert) vs. Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)

# LM log(Biomasse) ~ Canopy Cover 2022
fit2.22 <- lm(log(DryBiomass) ~ GreenCanopyCover, data = subset(dat, Year == 2022))
summary(fit2.22)
hist(rstandard(fit2.22))
qqnorm(rstandard(fit2.22))
qqline(rstandard(fit2.22))
# Regressionsformel: log(y) = 5.873306 + 0.036599*x
# Passt Modell? 


# LM log(Biomasse) ~ Canopy Cover 2023
fit2.23 <- lm(log(DryBiomass) ~ GreenCanopyCover, data = subset(dat, Year == 2023))
summary(fit2.23)
hist(rstandard(fit2.23))
qqnorm(rstandard(fit2.23))
qqline(rstandard(fit2.23))


# Biomasse vs. Canopy Cover nach Standort ####
# Strickhof
ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", formula = y~ x, method.args = list(family = gaussian(link = 'log')))+
  labs(title = "Biomasse vs. Canopy Cover Strickhof", x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# loess
ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth()+
  labs(title = "Biomasse vs. Canopy Cover Strickhof", x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# log(Biomasse)
jpeg("biomasse_log_cc_strickhof.jpeg", width=2500, height=1675, res=300)
ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "lm", formula = y~ x)+
  stat_poly_eq(use_label(c("eq", "R2")))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  ylim(6,10)+
  labs(title = "Biomasse (logarithmiert) vs. Canopy Cover, Strickhof", x = "Green Canopy Cover [%]", y = "log(Biomasse [kg/ha])", colour = "Makrostadium", subtitle = "n = 152")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))
dev.off()
# loess
ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  geom_smooth()+
  labs(title = "Biomasse (logarithmiert) vs. Canopy Cover Strickhof", x = "Green Canopy Cover (%)", y = "log(Biomasse)")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))




# Swiss Future Farm
jpeg("biomasse_log_cc_SFF.jpeg", width=2500, height=1675, res=300)
ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "lm", formula = y~ x)+
  stat_poly_eq(use_label(c("eq", "R2")))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  ylim(6,10)+
  labs(title = "Biomasse (logarithmiert) vs. Canopy Cover, Swiss Future Farm", x = "Green Canopy Cover [%]", y = "log(Biomasse [kg/ha])", colour = "Makrostadium", subtitle = "n = 114")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))
dev.off()

ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))+
  labs(x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)", title = "Biomasse vs. Canopy Cover Swiss Future Farm")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))+
  labs(x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)", title = "Biomasse vs. Canopy Cover Swiss Future Farm")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)

ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point(aes(colour = Makro))+
  geom_smooth()+
  stat_poly_eq(use_label(c("eq", "r2")))+
  labs(x = "Green Canopy Cover (%)", y = "Biomasse (kg/ha)", title = "Biomasse vs. Canopy Cover Swiss Future Farm")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)

# LM Strickhof
fit2.SH <- lm(log(DryBiomass)~GreenCanopyCover, data = subset(dat, SiteName == "Strickhof"))
summary(fit2.SH)
qqnorm(rstandard(fit2.SH))
qqline(rstandard(fit2.SH))
hist(rstandard(fit2.SH))

# LM Swiss Future Farm
fit2.SFF <- lm(log(DryBiomass)~GreenCanopyCover, data = subset(dat, SiteName == "SwissFutureFarm"))
summary(fit2.SFF)
qqnorm(rstandard(fit2.SFF))
qqline(rstandard(fit2.SFF))
hist(rstandard(fit2.SFF))

# Biomasse vs. Canopy Cover Anwendung eines Jahres aufs andere ####
# Daten von 2023 in Formel von 2022 einsetzen
x <- dat$GreenCanopyCover[dat$Year == 2023]
a <- summary(fit2.22)$coefficients[1,1]
b <- summary(fit2.22)$coefficients[2,1]

estimated <- exp(a) * (exp(b))^x
ggplot(data = subset(dat, Year == 2023), aes(x = DryBiomass, y = estimated))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, 
              colour="blue")+
  labs(x = "Gemessene Werte für Biomasse (kg/ha) 2023", y = "Erwartete Werte für Biomasse (kg/ha)", title = "Erwartete vs. reale Werte für Biomasse 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Residuen
qqnorm(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2023])))
qqline(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2023])))
hist(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2023])))

# Daten von 2022 in Formel von 2023 (passt nicht)
x <- dat$GreenCanopyCover[dat$Year == 2022]
a <- summary(fit2.23)$coefficients[1,1]
b <- summary(fit2.23)$coefficients[2,1]
summary(fit2.23)
estimated <- exp(a) * (exp(b))^x
ggplot(data = subset(dat, Year == 2022), aes(x = DryBiomass, y = estimated))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, 
              colour="blue")+
  labs(x = "Biomasse (kg/ha) 2022", y = "Erwartete Werte für Biomasse (kg/ha)", title = "Erwartete vs. reale Werte für Biomasse 2022")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Residuen
qqnorm(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2022])))
qqline(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2022])))
hist(rstandard(lm(estimated ~ dat$DryBiomass[dat$Year == 2022])))

# GLAI vs. Canopy Cover beide Jahre####
dat %>% 
  ggplot(aes(x = GreenCanopyCover, y = GLAI))+
  geom_point(aes(colour = Makro))+
  stat_poly_eq(use_label(c("eq", "R2")))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  geom_smooth(method = "lm", formula = y ~ x, colour = "black")+
  labs(x = "Green Canopy Cover (%)", title = "GLAI vs. Canopy Cover 2022 / 2023", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# LOESS
dat %>% 
  ggplot(aes(x = GreenCanopyCover, y = GLAI))+
  geom_point(aes(colour = Makro))+
  geom_smooth(span = .9, se = FALSE)+
  labs(x = "Green Canopy Cover (%)", title = "GLAI vs. Canopy Cover 2022 / 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# Änderung bei BBCH 50 - 59 ?

# LM GLAI vs. Canopy Cover
fit4 <- lm(GLAI ~ GreenCanopyCover, data = dat)
summary(fit4)
qqnorm(rstandard(fit4))
qqline(rstandard(fit4))
hist(rstandard(fit4))

# log(GLAI) vs. Canopy Cover
jpeg("glai_log_cc.jpeg", width=3000, height=2000, res=300)
dat %>% 
  ggplot(aes(x = GreenCanopyCover, y = log(GLAI)))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "lm", colour = "black")+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  stat_poly_eq(use_label(c("eq", "R2")))+
  labs(x = "Green Canopy Cover (%)", title = "GLAI (logarithmiert) vs. Canopy Cover 2022 / 2023", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

# LOESS
dat %>% 
  ggplot(aes(x = GreenCanopyCover, y = log(GLAI)))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "loess", span = 0.9, se = FALSE)+
  labs(x = "Green Canopy Cover (%)", title = "GLAI (logarithmiert) vs. Canopy Cover 2022 / 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# LM log(GLAI) vs. Canopy Cover
fit5 <- lm(log(GLAI) ~ GreenCanopyCover, data = dat)
summary(fit5)
qqnorm(rstandard(fit5))
qqline(rstandard(fit5))
hist(rstandard(fit5))


# GLAI vs. Canopy Cover 2022 und 2023 einzeln ####
ggplot(data = dat, aes(y = GreenCanopyCover, x = GLAI))+
  geom_point(aes(colour = Makro))+
  #geom_smooth(method = "lm", formula = y ~ log(x))+
  geom_smooth(span = 0.9)+
  labs(y = "Green Canopy Cover (%)", title = "Canopy Cover vs. GLAI")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)

ggplot(data = dat, aes(x = GreenCanopyCover, y = GLAI))+
  geom_point(aes(colour = Makro))+
  geom_smooth(span = 0.9)+
  labs(x = "Green Canopy Cover (%)", title = "GLAI vs. Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)

ggplot(data = dat, aes(x = GreenCanopyCover, y = log(GLAI)))+
  geom_point(aes(colour = Makro))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq", "R2")))+
  labs(x = "Green Canopy Cover (%)", title = "GLAI (logarithmiert) vs. Canopy Cover")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  facet_wrap(~Year)


# LM GLAI vs. Canopy Cover 2022
fit4.22 <- lm(GLAI ~ GreenCanopyCover, data = subset(dat, Year == 2022))
summary(fit4.22)
qqnorm(rstandard(fit4.22))
qqline(rstandard(fit4.22))
hist(rstandard(fit4.22))


# LM log(GLAI) vs. Canopy Cover 2022
fit5.22 <- lm(log(GLAI) ~ GreenCanopyCover, data = subset(dat, Year == 2022))
summary(fit5.22)
qqnorm(rstandard(fit5.22))
qqline(rstandard(fit5.22))
hist(rstandard(fit5.22))
# Welches Modell besser?


# LM GLAI vs. Canopy Cover 2023
fit4.23 <- lm(GLAI ~ GreenCanopyCover, data = subset(dat, Year == 2023))
summary(fit4.23)
qqnorm(rstandard(fit4.23))
qqline(rstandard(fit4.23))
hist(rstandard(fit4.23))
# unpassend


# LM log(GLAI) vs. Canopy Cover 2023
fit5.23 <- lm(log(GLAI) ~ GreenCanopyCover, data = subset(dat, Year == 2023))
summary(fit5.23)
qqnorm(rstandard(fit5.23))
qqline(rstandard(fit5.23))
hist(rstandard(fit5.23))
# Modell passt nicht, evtl Zsh mit Werten von Canopy Cover die nicht ganz passen -> vgl oben Übersichtsplots


# GLAI vs. Canopy Cover nach Standort ####
# Strickhof
ggplot(data = subset(dat, SiteName== "Strickhof"), aes(x = GreenCanopyCover, y = GLAI))+
  geom_point(aes(colour = Makro))+
  geom_smooth(span = 0.9)+
  labs(x = "Green Canopy Cover (%)", y = "GLAI", title = "GLAI vs. Canopy Cover Strickhof")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
  #geom_smooth(method = "lm")

ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GLAI, y = GreenCanopyCover))+
  geom_point(aes(colour = Makro))+
  #geom_smooth(method = "lm", formula = y~x)+
  geom_smooth(span = 0.9)+
  labs(y = "Greeen Canopy Cover (%)", title = "Canopy Cover vs. GLAI Strickhof")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

ggplot(data = subset(dat, SiteName == "Strickhof"), aes(x = GreenCanopyCover, y = GLAI))+
  geom_point(aes(colour = Makro))+
  facet_wrap(~Year)+
  geom_smooth(span = 0.9)
  #geom_smooth(method = "glm", method.args = list(family = gaussian(link = 'log')))
  

# Swiss Future Farm
ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GreenCanopyCover, y = GLAI))+
  geom_point(aes(colour = Makro))+
  geom_smooth(span = 0.9)+
  labs(x = "Green Canopy Cover (%)", title = "GLAI vs. Canopy Cover Swiss Future Farm")+
  theme(plot.title = element_text(hjust=0.5, face = 'bold'))

ggplot(data = subset(dat, SiteName == "SwissFutureFarm"), aes(x = GLAI, y = GreenCanopyCover))+
  geom_point(aes(colour = Makro))+
  geom_smooth(method = "lm", formula = y ~ log(x))+
  facet_wrap(~Year)
  
# LM Strickhof
fit3.SH <- lm(GLAI~ GreenCanopyCover, data = subset(dat, SiteName == "Strickhof"))
summary(fit3.SH)
qqnorm(rstandard(fit3.SH))
qqline(rstandard(fit3.SH))
hist(rstandard(fit3.SH))
# Residuen nicht normalverteilt

# LM Swiss Future Farm
fit3.SFF <- lm(GLAI ~ GreenCanopyCover, data = subset(dat, SiteName == "SwissFutureFarm"))
summary(fit3.SFF)
qqnorm(rstandard(fit3.SFF))
qqline(rstandard(fit3.SFF))
hist(rstandard(fit3.SFF))
# unpassend, welcher Zsh? Zu wenig Daten?


# GLAI vs. Canopy Cover Anwendung eines Jahres aufs andere ####
# Daten von 2023 in Formel von 2022 einsetzen (beide Modelle passen nicht wirklich)
x <- dat$GreenCanopyCover[dat$Year == 2023]
a <- summary(fit4.22)$coefficients[1,1]
b <- summary(fit4.22)$coefficients[2,1]

estimated <- a + b*x
ggplot(data = subset(dat, Year == 2023), aes(x = GLAI, y = estimated))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, 
              colour="blue")+
  labs(x = "Gemessene Werte GLAI 2023", y = "Erwartete Werte für GLAI", title = "Erwartete vs. reale Werte für GLAI 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Residuen
qqnorm(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2023])))
qqline(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2023])))
hist(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2023])))

# Gleiches mit fit5.22
x <- dat$GreenCanopyCover[dat$Year == 2023]
a <- summary(fit5.22)$coefficients[1,1]
b <- summary(fit5.22)$coefficients[2,1]

estimated <- exp(a) * (exp(b))^x
ggplot(data = subset(dat, Year == 2023), aes(x = GLAI, y = estimated))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, 
              colour="blue")+
  labs(x = "Gemessene Werte GLAI 2023", y = "Erwartete Werte für GLAI", title = "Erwartete vs. reale Werte für GLAI 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Residuen
qqnorm(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2023])))
qqline(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2023])))
hist(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2023])))

# Daten von 2022 in Formel von 2023
x <- dat$GreenCanopyCover[dat$Year == 2022]
a <- summary(fit4.23)$coefficients[1,1]
b <- summary(fit4.23)$coefficients[2,1]

estimated <- a + b*x
ggplot(data = subset(dat, Year == 2022), aes(x = GLAI, y = estimated))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, 
              colour="blue")+
  labs(x = "gemessene Werte für GLAI", y = "Erwartete Werte für GLAI", title = "Erwartete vs. reale Werte für Biomasse 2022")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Residuen
qqnorm(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2022])))
qqline(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2022])))
hist(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2022])))
# Gleiches mit fit5.23 
x <- dat$GreenCanopyCover[dat$Year == 2022]
a <- summary(fit5.23)$coefficients[1,1]
b <- summary(fit5.23)$coefficients[2,1]

estimated <- exp(a) * (exp(b))^x
ggplot(data = subset(dat, Year == 2022), aes(x = GLAI, y = estimated))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, 
              colour="blue")+
  labs(x = "gemessene Werte für GLAI", y = "Erwartete Werte für GLAI", title = "Erwartete vs. reale Werte für Biomasse 2022")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Residuen
qqnorm(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2022])))
qqline(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2022])))
hist(rstandard(lm(estimated ~ dat$GLAI[dat$Year == 2022])))

# Regressionen für BBCH Abschnitte ####
# log(Biomasse) vs. GLAI
ggplot(dat = dat, aes(x = GLAI, y = log(DryBiomass)))+
  geom_point()+
  facet_wrap(~Makro)+
  geom_smooth(method = "lm")+
  stat_poly_eq(use_label(c("eq", "r2")))
ggplot(dat = dat, aes(x = GLAI, y = log(DryBiomass)))+
  geom_point()+
  facet_wrap(~Year+Makro)+
  geom_smooth(method = "lm")+
  stat_poly_eq(use_label(c("eq", "r2")))
# -> vor allem für Makrostadium 30 - 39 Zusammenhang erekennbar

# Biomasse vs. Canopy Cover
ggplot(dat = dat, aes(x = GreenCanopyCover, y = DryBiomass))+
  geom_point()+
  facet_wrap(~Makro)+
  #geom_smooth(method = "lm")+
  geom_smooth(span = 0.9)+
  stat_poly_eq(use_label(c("eq", "r2")))+
  labs(title = "Biomasse vs. Green Canopy Cover")

ggplot(dat = dat, aes(x = GreenCanopyCover, y = log(DryBiomass)))+
  geom_point()+
  facet_wrap(~Makro)+
  #geom_smooth()+
  geom_smooth(method = "lm")+
  stat_poly_eq(use_label(c("eq", "r2")))+
  labs(title = "log(Biomasse) vs. Green Canopy Cover")

# GLAI vs. Canopy Cover
ggplot(dat = dat, aes(x = GreenCanopyCover, y = GLAI))+
  geom_point()+
  facet_wrap(~Makro)+
  geom_smooth(span = 0.9)

ggplot(dat = dat, aes(x = GreenCanopyCover, y = log(GLAI)))+
  geom_point()+
  facet_wrap(~Makro)+
  geom_smooth(method = "lm")+
  stat_poly_eq(use_label(c("eq", "r2")))
  




# Plots mit Temperatur ####
SH_temp22$Date <- as.Date(SH_temp22$Date, format = "%d.%m.%y")
SFF_temp22$Date <- as.Date(SFF_temp22$Date, format = "%d.%m.%y")
a_temp$Date <- as.Date(a_temp$Date, format = "%d.%m.%y")
w_temp$Date <- as.Date(w_temp$Date, format = "%d.%m.%y")
SH_temp23$Date <- as.Date(SH_temp23$Date, format = "%d.%m.%y")
SFF_temp23$Date <- as.Date(SFF_temp23$Date, format = "%d.%m.%y")
dat$Date <- as.Date(dat$Date, format = "%d.%m.%y")

# Temperaturverlauf 2022
ggplot() +
  geom_line(data = subset(SH_temp22, Date <= "2022-07-31" & Date > "2022-03-01"), aes(x = Date, y = T_mean, colour = "Strickhof"))+
  geom_line(data = subset(SFF_temp22, Date <= "2022-07-31" & Date > "2022-03-01"), aes(x = Date, y = T_mean, colour = "Swiss Future Farm"))+
  #geom_line(data = a_temp[93:304,], aes(x = Date, y = T_mean, colour = "Arenenberg"))+
  #geom_line(data = w_temp[93:304,], aes(x = Date, y = T_mean, colour = "Witzwil"))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Temperaturverlauf 2022", y = "Mittlere Temperatur in °C")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Temperaturverlauf 2023
ggplot()+
  geom_line(data = subset(SH_temp23, Date <= "2023-07-31" & Date >= "2023-03-01"), aes(x = Date, y = T_mean, colour = "Strickhof"))+
  geom_line(data = subset(SFF_temp23, Date <= "2023-07-31" & Date >= "2023-03-01"), aes(x = Date, y = T_mean, colour = "Swiss Future Farm"))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Temperaturverlauf 2023", y = "Mittlere Temperatur in °C")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Plots mit kumulierten Temperaturen ####
# Datensatz erstellen mit kumulierten Temperaturen (> 0 Grad) für Standorte und Wachstumssaison (März - Juli (Erntedatum))
SH_tempsum22 <- data.frame(Date = SH_temp22$Date[SH_temp22$Date <= "2022-07-25" & SH_temp22$Date >= "2022-03-01"],
                           T_mean = SH_temp22$T_mean[SH_temp22$Date <= "2022-07-25" & SH_temp22$Date >= "2022-03-01"], T_sum = NA)
SH_tempsum23 <- data.frame(Date = SH_temp23$Date[SH_temp23$Date <= "2023-07-20" & SH_temp23$Date >= "2023-03-01"],
                           T_mean = SH_temp23$T_mean[SH_temp23$Date <= "2023-07-20" & SH_temp23$Date >= "2023-03-01"], T_sum = NA)
SFF_tempsum22 <- data.frame(Date = SFF_temp22$Date[SFF_temp22$Date <= "2022-07-27" & SFF_temp22$Date >= "2022-03-01"],
                           T_mean = SFF_temp22$T_mean[SFF_temp22$Date <= "2022-07-27" & SFF_temp22$Date >= "2022-03-01"], T_sum = NA)
SFF_tempsum23 <- data.frame(Date = SFF_temp23$Date[SFF_temp23$Date <= "2023-07-20" & SFF_temp23$Date >= "2023-03-01"],
                           T_mean = SFF_temp23$T_mean[SFF_temp23$Date <= "2023-07-20" & SFF_temp23$Date >= "2023-03-01"], T_sum = NA)
# Erste Einträge manuell einfüllen
SH_tempsum22$T_sum[1] <- SH_tempsum22$T_mean[1]
SH_tempsum23$T_sum[1] <- 0
SFF_tempsum22$T_sum[1] <- 0
SFF_tempsum23$T_sum[1] <- SFF_tempsum23$T_mean[1]

# Schleifen für restliche Temperatursummen
# Strickhof 22
for (i in 2:nrow(SH_tempsum22)){
  if (SH_tempsum22$T_mean[i] > 0){
    SH_tempsum22$T_sum[i] <- SH_tempsum22$T_sum[i-1]+SH_tempsum22$T_mean[i]
  }
  else {
    SH_tempsum22$T_sum[i] <- SH_tempsum22$T_sum[i-1]
  }
}

#Strickhof 23
for (i in 2:nrow(SH_tempsum23)){
  if (SH_tempsum23$T_mean[i] > 0){
    SH_tempsum23$T_sum[i] <- SH_tempsum23$T_sum[i-1]+SH_tempsum23$T_mean[i]
  }
  else {
    SH_tempsum23$T_sum[i] <- SH_tempsum23$T_sum[i-1]
  }
}

# Swiss Future Farm 22
for (i in 2:nrow(SFF_tempsum22)){
  if (SFF_tempsum22$T_mean[i] > 0){
    SFF_tempsum22$T_sum[i] <- SFF_tempsum22$T_sum[i-1]+SFF_tempsum22$T_mean[i]
  }
  else {
    SFF_tempsum22$T_sum[i] <- SFF_tempsum22$T_sum[i-1]
  }
}

# Swiss Future Farm 23
for (i in 2:nrow(SFF_tempsum23)){
  if (SFF_tempsum23$T_mean[i] > 0){
    SFF_tempsum23$T_sum[i] <- SFF_tempsum23$T_sum[i-1]+SFF_tempsum23$T_mean[i]
  }
  else {
    SFF_tempsum23$T_sum[i] <- SFF_tempsum23$T_sum[i-1]
  }
}


# Plots mit kumulierter Temperatur
# Strickhof 22
ggplot()+
  geom_line(data = SH_tempsum22, aes(x = Date, y = T_sum))+
  labs(x = "Zeit", y = "Temperatursumme in °C", title = "Temperatursumme über die Zeit Strickhof 2022")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
#Strickhof 23
ggplot()+
  geom_line(data = SH_tempsum23, aes(x = Date, y = T_sum))+
  labs(x = "Zeit", y = "Temperatursumme in °C", title = "Temperatursumme über die Zeit Strickhof 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# Swiss Future Farm 22
ggplot()+
  geom_line(data = SFF_tempsum22, aes(x = Date, y = T_sum))+
  labs(x = "Zeit", y = "Temperatursumme in °C", title = "Temperatursumme über die Zeit Swiss Future Farm 2022")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# Swiss Future Farm 23
ggplot()+
  geom_line(data = SFF_tempsum23, aes(x = Date, y = T_sum))+
  labs(x = "Zeit", y = "Temperatursumme in °C", title = "Temperatursumme über die Zeit Swiss Future Farm 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Kombinierte Plots
# 2022
ggplot()+
  geom_line(data = SH_tempsum22, aes(x = Date, y = T_sum, colour = "Strickhof"))+
  geom_line(data = SFF_tempsum22, aes(x = Date, y = T_sum, colour = "Swiss Future Farm"))+
  labs(x = "Zeit", y = "Temperatursumme in °C", title = "Temperatursumme über die Zeit 2022")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# 2023
ggplot()+
  geom_line(data = SH_tempsum23, aes(x = Date, y = T_sum, colour = "Strickhof"))+
  geom_line(data = SFF_tempsum23, aes(x = Date, y = T_sum, colour = "Swiss Future Farm"))+
  labs(x = "Zeit", y = "Temperatursumme in °C", title = "Temperatursumme über die Zeit 2023")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Spalten für Datum hinzufügen um beide Jahre in einem plot zu haben
SFF_tempsum22$Date_d_m <- format(SFF_tempsum22$Date,"%m-%d")
SFF_tempsum22$Date_d_m <- as.Date(SFF_tempsum22$Date_d_m, "%m-%d")
SH_tempsum22$Date_d_m <- format(SH_tempsum22$Date, "%m-%d")
SH_tempsum22$Date_d_m <- as.Date(SH_tempsum22$Date_d_m, "%m-%d")
SFF_tempsum23$Date_d_m <- format(SFF_tempsum23$Date,"%m-%d")
SFF_tempsum23$Date_d_m <- as.Date(SFF_tempsum23$Date_d_m, "%m-%d")
SH_tempsum23$Date_d_m <- format(SH_tempsum23$Date, "%m-%d")
SH_tempsum23$Date_d_m <- as.Date(SH_tempsum23$Date_d_m, "%m-%d")

ggplot()+
  geom_line(data = SFF_tempsum22, aes(x = Date_d_m, y = T_sum, colour = "Swiss Future Farm 2022"))+
  geom_line(data = SFF_tempsum23, aes(x = Date_d_m, y = T_sum, colour = "Swiss Future Farm 2023"))+
  geom_line(data = SH_tempsum22, aes(x = Date_d_m, y = T_sum, colour = "Strickhof 2022"))+
  geom_line(data = SH_tempsum23, aes(x = Date_d_m, y = T_sum, colour = "Strickhof 2023"))+
  labs(x = "Zeit", y = "Temperatursumme [°C]", title = "Temperatursummen pro Standort und Jahr", colour = " ")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))



# Plots mit Niederschlag ####
# Date als Datum definieren
SH_prec22$Date <- as.Date(SH_prec22$Date, format = "%d.%m.%y")
SFF_prec22$Date <- as.Date(SFF_prec22$Date, format = "%d.%m.%y")
SH_prec23$Date <- as.Date(SH_prec23$Date, format = "%d.%m.%y")
SFF_prec23$Date <- as.Date(SFF_prec23$Date, format = "%d.%m.%y")

# Niederschalgsverlauf
# 2022
ggplot() +
  geom_line(data = subset(SH_prec22, Date <= "2022-07-31" & Date > "2022-03-01"), aes(x = Date, y = precip_tot, colour = "Strickhof"))+
  geom_line(data = subset(SFF_prec22, Date <= "2022-07-31" & Date > "2022-03-01"), aes(x = Date, y = precip_tot, colour = "Swiss Future Farm"))+
  labs(title = "Niederschlag 2022", y = "Niederschlag in mm")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  ylim(0,60)

# 2023
ggplot()+
  geom_line(data = subset(SH_prec23, Date <= "2023-07-31" & Date >= "2023-03-01"), aes(x = Date, y = precip_tot, colour = "Strickhof"))+
  geom_line(data = subset(SFF_prec23, Date <= "2023-07-31" & Date >= "2023-03-01"), aes(x = Date, y = precip_tot, colour = "Swiss Future Farm"))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Niederschlag 2023", y = "Niederschlag in mm")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  ylim(0, 60)

# Kumulierte Neiderschläge ####
# Datensätze erstellen
SH_precsum22 <- data.frame(Date = SH_prec22$Date[SH_prec22$Date <= "2022-07-25" & SH_prec22$Date >= "2022-03-01"], 
                           prec = SH_prec22$precip_tot[SH_prec22$Date <= "2022-07-25" & SH_prec22$Date >= "2022-03-01"], prec_sum = NA)
SH_precsum23 <- data.frame(Date = SH_prec23$Date[SH_prec23$Date <= "2023-07-20" & SH_prec23$Date >= "2023-03-01"],
                           prec = SH_prec23$precip_tot[SH_prec23$Date <= "2023-07-20" & SH_prec23$Date >= "2023-03-01"], prec_sum = NA)
SFF_precsum22 <- data.frame(Date = SFF_prec22$Date[SFF_prec22$Date <= "2022-07-27" & SFF_prec22$Date >= "2022-03-01"],
                            prec = SFF_prec22$precip_tot[SFF_prec22$Date <= "2022-07-27" & SFF_prec22$Date >= "2022-03-01"], prec_sum = NA)
SFF_precsum23 <- data.frame(Date = SFF_prec23$Date[SFF_prec23$Date <= "2023-07-20" & SFF_prec23$Date >= "2023-03-01"],
                            prec = SFF_prec23$precip_tot[SFF_prec23$Date <= "2023-07-20" & SFF_prec23$Date >= "2023-03-01"], prec_sum = NA)

# -> vielleicht einen Datensatz machen für weniger Code?
# Erste EInträge manuell eintragen
SH_precsum22$prec_sum[1] <- SH_precsum22$prec[1]
SH_precsum23$prec_sum[1] <- SH_precsum23$prec[1]
SFF_precsum22$prec_sum[1] <- SFF_precsum22$prec[1]
SFF_precsum23$prec_sum[1] <- SFF_precsum23$prec[1]

# Schleifen um Niederschlagssumme zu berechnen
# Strickhof 22
for (i in 2:nrow(SH_precsum22)){
  SH_precsum22$prec_sum[i] <- SH_precsum22$prec_sum[i-1]+SH_precsum22$prec[i]
}
# Strickhof 23
for (i in 2:nrow(SH_precsum23)){
  SH_precsum23$prec_sum[i] <- SH_precsum23$prec_sum[i-1]+SH_precsum23$prec[i]
}
# Swiss Future Farm 22
for (i in 2:nrow(SFF_precsum22)){
  SFF_precsum22$prec_sum[i] <- SFF_precsum22$prec_sum[i-1]+SFF_precsum22$prec[i]
}
# Swiss Future Farm 23
for (i in 2:nrow(SFF_precsum23)){
  SFF_precsum23$prec_sum[i] <- SFF_precsum23$prec_sum[i-1]+SFF_precsum23$prec[i]
}

# Plots mit summiertem Niederschlag
# Strickhof 22
ggplot()+
  geom_line(data = SH_precsum22, aes(x = Date, y = prec_sum))+
  labs(x = "Zeit", y = "Niederschlag [mm]", title = "Niederschlag Strickhof 2022")+
  theme(plot.title = element_text(hjust = 0.5, face ='bold'))
  
# Strickhof 23
ggplot()+
  geom_line(data = SH_precsum23, aes(x = Date, y = prec_sum))+
  labs(x = "Zeit", y = "Niederschlag [mm]", title = "Niederschlag Strickhof 2023")+
  theme(plot.title = element_text(hjust = 0.5, face ='bold'))

# Swiss Future Farm 22
ggplot()+
  geom_line(data = SFF_precsum22, aes(x = Date, y = prec_sum))+
  labs(x = "Zeit", y = "Niederschlag [mm]", title = "Niederschlag Swiss Future Farm 2022")+
  theme(plot.title = element_text(hjust = 0.5, face ='bold'))

# Swiss Future Farm 23
ggplot()+
  geom_line(data = SFF_precsum23, aes(x = Date, y = prec_sum))+
  labs(x = "Zeit", y = "Niederschlag [mm]", title = "Niederschlag Swiss Future Farm 2023")+
  theme(plot.title = element_text(hjust = 0.5, face ='bold'))

# Kombinierte Plots
# 2022
ggplot()+
  geom_line(data = SH_precsum22, aes(x = Date, y = prec_sum, colour = "Strickhof"))+
  geom_line(data = SFF_precsum22, aes(x = Date, y = prec_sum, colour = "Swiss Future Farm"))+
  labs(x = "Zeit", y = "Niederschlaf [mm]", title = "Niederschlag 2022", colour= "")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
# 2023
ggplot()+
  geom_line(data = SH_precsum23, aes(x = Date, y = prec_sum, colour = "Strickhof"))+
  geom_line(data = SFF_precsum23, aes(x = Date, y = prec_sum, colour = "Swiss Future Farm"))+
  labs(x = "Zeit", y = "Niederschlaf [mm]", title = "Niederschlag 2023", colour= "")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Spalten für Datum hinzufügen um beide Jahre in einem Plot zu haben
SFF_precsum22$Date_d_m <- format(SFF_precsum22$Date,"%m-%d")
SFF_precsum22$Date_d_m <- as.Date(SFF_precsum22$Date_d_m, "%m-%d")
SH_precsum22$Date_d_m <- format(SH_precsum22$Date, "%m-%d")
SH_precsum22$Date_d_m <- as.Date(SH_precsum22$Date_d_m, "%m-%d")
SFF_precsum23$Date_d_m <- format(SFF_precsum23$Date,"%m-%d")
SFF_precsum23$Date_d_m <- as.Date(SFF_precsum23$Date_d_m, "%m-%d")
SH_precsum23$Date_d_m <- format(SH_precsum23$Date, "%m-%d")
SH_precsum23$Date_d_m <- as.Date(SH_precsum23$Date_d_m, "%m-%d")

jpeg("niederschlag.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_line(data = SFF_precsum23, aes(x = Date_d_m, y = prec_sum, colour = "Swiss Future Farm 2023"))+
  geom_line(data = SH_precsum23, aes(x = Date_d_m, y = prec_sum, colour = "Strickhof 2023"))+
  geom_line(data = SFF_precsum22, aes(x = Date_d_m, y = prec_sum, colour = "Swiss Future Farm 2022"))+
  geom_line(data = SH_precsum22, aes(x = Date_d_m, y = prec_sum, colour = "Strickhof 2022"))+
  labs(x = "Zeit", y = "Niederschlag [mm]", title = "Niederschlagssummen pro Standort und Jahr", colour = " ")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

# Traits im Verlauf der Wachstumssaison -> vgl mit Wetterplots ####

# Biomasse 
# 2022
jpeg("biomasse_22.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_point(data = subset(dat, Year == 2022), aes(x = Date, y = DryBiomass, colour = Makro))+
  ylim(0,15000)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  labs(x = "Zeit", y = "Biomasse [kg/ha]", title = "2022", colour = "Makrostadium")+
  scale_x_date(limits = as.Date(c("2022-03-01","2022-06-08")))+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

#2023
jpeg("biomasse_23.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_point(data = subset(dat, Year == 2023), aes(x = Date, y = DryBiomass, colour = Makro))+
  ylim(0,15000)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  labs(x = "Zeit", y = "Biomasse [kg/ha]", title = "2023", colour = "Makrostadium")+
  scale_x_date(limits = as.Date(c("2023-03-01","2023-06-08")))+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

# GLAI
# 2022
jpeg("glai_22.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_point(data = subset(dat, Year == 2022), aes(x = Date, y = GLAI, colour = Makro))+
  ylim(0,7)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  scale_x_date(limits = as.Date(c("2022-03-01","2022-06-08")))+
  labs(x = "Zeit", title = "2022", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

#2023
jpeg("glai_23.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_point(data = subset(dat, Year == 2023), aes(x = Date, y = GLAI, colour = Makro))+
  ylim(0,7)+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+
  scale_x_date(limits = as.Date(c("2023-03-01","2023-06-08")))+
  labs(x = "Zeit", title = "2023", colour = "Makrostadium")
dev.off()

# CC
# 2022
jpeg("cc_22.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_point(data = subset(dat, Year == 2022), aes(x = Date, y = GreenCanopyCover, colour = Makro))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  ylim(0,100)+
  labs(x = "Zeit", y = "Green Canopy Cover [%]", title = "Verlauf Green Canopy Cover 2022", colour = "Makrostadium")+
  scale_x_date(limits = as.Date(c("2022-03-01","2022-06-08")))+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

# 2023 
jpeg("cc_23.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_point(data = subset(dat, Year == 2023), aes(x = Date, y = GreenCanopyCover, colour = Makro))+
  scale_colour_manual(values = c("dodgerblue4", "#54bebe", "coral1"))+
  ylim(0,100)+
  scale_x_date(limits = as.Date(c("2023-03-01","2023-06-08")))+
  labs(x = "Zeit", y = "Green Canopy Cover [%]", title = "Verlauf Green Canopy Cover 2023", colour = "Makrostadium")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()

# Globalstrahlung Strickhof ####
strahlung <- read.csv("strahlung.csv", sep = ";")
strahlung$Datum <- as.Date(strahlung$Datum, format = "%d.%m.%Y")
names(strahlung) <- c("Datum", "Globalstrahlung")

ggplot()+
  geom_line(data = subset(strahlung, Datum >= "2022-03-01" & Datum <= "2022-07-31"), aes(x = Datum, y = Globalstrahlung))+
  labs(title = "Globalstrahlung Strickhof 2022")
ggplot()+
  geom_line(data = subset(strahlung, Datum >= "2023-03-01" & Datum <= "2023-07-31"), aes(x = Datum, y = Globalstrahlung))+
  labs(title = "Globalstrahlung Strickhof 2023")

# Strahlungssummen in neuen Datensätzen
strahlung22 <- data.frame(Datum = strahlung$Datum[strahlung$Datum >= "2022-03-01" & strahlung$Datum <= "2022-07-31"], Globalstrahlung = strahlung$Globalstrahlung[strahlung$Datum >= "2022-03-01" & strahlung$Datum <= "2022-07-31"])
strahlung23 <- data.frame(Datum = strahlung$Datum[strahlung$Datum >= "2023-03-01" & strahlung$Datum <= "2023-07-31"], Globalstrahlung = strahlung$Globalstrahlung[strahlung$Datum >= "2023-03-01" & strahlung$Datum <= "2023-07-31"])
strahlung22$Summe <- NA
strahlung23$Summe <- NA

strahlung22$Summe[1] <- strahlung22$Globalstrahlung[1]
for (i in 2:nrow(strahlung22)){
  strahlung22$Summe[i] <- strahlung22$Summe[i-1] + strahlung22$Globalstrahlung[i]
  }

strahlung23$Summe[1] <- strahlung23$Globalstrahlung[1]
for (i in 2:nrow(strahlung23)){
  strahlung23$Summe[i] <- strahlung23$Summe[i-1] + strahlung23$Globalstrahlung[i]
}

# Plots mit Strahlungssummen
strahlung22$Dat <- format(strahlung22$Datum,"%m-%d")
strahlung22$Dat <- as.Date(strahlung22$Dat, "%m-%d")
strahlung23$Dat <- format(strahlung23$Datum,"%m-%d")
strahlung23$Dat <- as.Date(strahlung23$Dat, "%m-%d")

jpeg("globalstrahlung.jpeg", width=2500, height=1675, res=300)
ggplot()+
  geom_line(data = strahlung22, aes(x = Dat, y = Summe, colour = "2022"))+
  geom_line(data = strahlung23, aes(x = Dat, y = Summe, colour = "2023"))+
  labs(title = "Summierte Globalstrahlung am Strickhof", x = "Zeit", y = "Globalstrahlung Summe [W/m2]", colour = "")+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
dev.off()
# GDD im Verlafu der Zeit ####
ggplot()+
  geom_point(data = dat[dat$Year == 2022 & (dat$SiteName == "Strickhof" | dat$SiteName == "SwissFutureFarm"),], aes(x = Date, y = GDD, colour = SiteName))+
  ylim(400,1600)
ggplot()+
  geom_point(data = dat[dat$Year == 2023,], aes(x = Date, y = GDD, colour = SiteName))+
  ylim(400,1600)
# Berechnungen für Tabellen im Text ####
# Berechnungen der mittleren Temperaturen und Niederschlagssummen (für Tabelle im Text)
# Das Datum wird angepasst an Aussaatdatum bzw. Erntedatum
# Strickhof 2022 und 2023
sum(SH_prec22$precip_tot[SH_prec22$Date <= "2022-07-25" & SH_prec22$Date >= "2021-10-27"])
sum(SH_prec23$precip_tot[SH_prec23$Date <= "2023-07-20" & SH_prec23$Date >= "2022-10-18"])
mean(SH_temp22$T_mean[SH_temp22$Date <= "2022-07-25" & SH_temp22$Date >= "2021-10-27"])
mean(SH_temp23$T_mean[SH_temp23$Date <= "2023-07-20" & SH_temp23$Date >= "2022-10-18"])
# SFF 2022 und 2023
sum(SFF_prec22$precip_tot[SFF_prec22$Date <= "2022-07-27" & SFF_prec22$Date >= "2021-10-11"])
sum(SFF_prec23$precip_tot[SFF_prec23$Date <= "2023-07-20" & SFF_prec23$Date >= "2022-10-18"])
mean(SFF_temp22$T_mean[SFF_temp22$Date <= "2022-07-27" & SFF_temp22$Date >= "2021-10-11"])
mean(SFF_temp23$T_mean[SFF_temp23$Date <= "2023-07-20" & SFF_temp23$Date >= "2022-10-18"])
# Arenenberg
sum(a_prec$precip_tot[a_prec$Date <= "2022-07-15" & a_prec$Date >= "2021-10-15"])
mean(a_temp$T_mean[a_temp$Date <= "2022-07-15" & a_temp$Date >= "2021-10-15"])
# Witzwil
sum(w_prec$precip_tot[w_prec$Date <= "2022-07-25" & w_prec$Date >= "2021-10-18"], na.rm = TRUE)
mean(w_temp$T_mean[w_temp$Date <= "2022-07-25" & w_temp$Date >= "2021-10-18"])


# Quantile der Traits für Standort und Jahr
summary(dat[dat$Year == 2022 & dat$SiteName == "Strickhof",])
summary(dat[dat$Year == 2022 & dat$SiteName == "SwissFutureFarm",])
summary(dat[dat$Year == 2023 & dat$SiteName == "Strickhof",])
summary(dat[dat$Year == 2023 & dat$SiteName == "SwissFutureFarm",])

# Daten extrahieren ####
write.csv(dat, "data_cleaned_2022_2023.csv", row.names=TRUE)

