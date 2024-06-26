###### SCRIPT PARA UNIR TIROS DE FOTMOB CON EVENTING DE OPTA ######
library(dplyr)
library(stringr)


eventing_opta <- readRDS("eventing.rds")
tiros_fotmob <- readRDS("tiros_fotmob_completados.rds")

tiros_opta <- eventing_opta[which(eventing_opta$isShot == T),]


## RANKEAR LOS TIROS POR MINUTO 
tiros_opta$rank_tiro <- NA

for (partidin in unique(tiros_opta$partido)) {
  subset_match <- tiros_opta[tiros_opta$partido == partidin,]
  subset_match$rank_tiro <- row_number(subset_match$minute)
  
  tiros_opta[tiros_opta$partido == partidin, "rank_tiro"] <- subset_match$rank_tiro
}


tiros_fotmob$rank_tiro <- NA

for (partidin in unique(tiros_fotmob$partido)) {
  subset_match <- tiros_fotmob[tiros_fotmob$partido == partidin,]
  subset_match$rank_tiro <- row_number(subset_match$min)
  
  tiros_fotmob[tiros_fotmob$partido == partidin, "rank_tiro"] <- subset_match$rank_tiro
}


## DISCREPANCIAS CON NOMBRES
cambios_opta <- c("Atletico" = "Atletico Madrid",
            "Man Utd" = "Manchester United",
            "Man City" = "Manchester City",
            "Luton" = "Luton Town",
            "Sheff Utd" = "Sheffield United",
            "Verona" = "Hellas Verona",
            "Leverkusen" = "Bayer Leverkusen",
            "Bayern" = "Bayern München",
            "RBL" = "RB Leipzig",
            "Borussia M.Gladbach" = "Borussia Monchengladbach",
            "Bochum" = "VfL Bochum",
            "Mainz" = "Mainz 05",
            "Darmstadt" = "SV Darmstadt",
            "Shakhtar" = "Shakhtar Donetsk",
            "Sporting" = "Sporting CP")
            
cambios_fotmob <- c("AFC Bournemouth" = "Bournemouth",
            "Wolverhampton Wanderers" = "Wolves",
            "Brighton & Hove Albion" = "Brighton",
            "West Ham United" = "West Ham",
            "Tottenham Hotspur" = "Tottenham",
            "Newcastle United" = "Newcastle",
            "SSC Napoli" = "Napoli",
            "VfB Stuttgart" = "Stuttgart",
            "FC Augsburg" = "Augsburg",
            "SC Freiburg" = "Freiburg",
            "TSG Hoffenheim" = "Hoffenheim",
            "FC Köln" = "FC Koln",
            "Borussia Mönchengladbach" = "Borussia Monchengladbach",
            "Paris Saint-Germain" = "PSG",
            "FC Twente" = "Twente",
            "FC Porto" = "Porto")
            
reemplazar_nombres <- function(columna, vector) {
  for (nombre in names(vector)) {
    columna <- gsub(nombre, vector[nombre], columna)
  }
  return(columna)
}

tiros_opta$partido <- reemplazar_nombres(tiros_opta$partido, cambios_opta)

tiros_fotmob$teamName <- reemplazar_nombres(tiros_fotmob$teamName, cambios_fotmob)
tiros_fotmob$partido <- reemplazar_nombres(tiros_fotmob$partido, cambios_fotmob)
            


union_final <- left_join(tiros_opta, tiros_fotmob, by = c("partido", "dia" = "fecha", "rank_tiro"))



## AÑADIR INFORMACIÓN ADICIONAL DE LOS TIROS A LA TABLA DE EVENTING 
eventing_opta$shotType <- NA
eventing_opta$xG <- NA
eventing_opta$xGOT <- NA

eventing_opta[which(eventing_opta$isShot == T), "shotType"] <- union_final$shotType
eventing_opta[which(eventing_opta$isShot == T), "xG"] <- union_final$xG
eventing_opta[which(eventing_opta$isShot == T), "xGOT"] <- union_final$xGOT

saveRDS(eventing_opta, "eventingytiros_opta.rds")


