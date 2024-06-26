library(dplyr)
library(stringr)

jugadores_opta <- readRDS("jugadores.rds")

jugadores_tm <- readRDS("jugadores_transfermarkt.rds")


# unir por club y dorsal: ver que los clubes se llamen igual
unique(jugadores_opta$team)

unique(jugadores_tm$current_club)

# Limpieza inicial:
por_algo <- c("Athletic Bilbao" = "Athletic Club", "de Vigo" = "Vigo", "Manchester" = "Man", "United" = "Utd", 
              "Wolverhampton Wanderers" = "Wolves", "Sheffield" = "Sheff", "RB Leipzig" = "RBL", "RasenBallsport Leipzig" = "RBL",
              "Borussia Mönchengladbach" = "Borussia M.Gladbach", "1.FSV Mainz 05" = "Mainz", "SV Darmstadt 98" = "Darmstadt",
              "Paris Saint-Germain" = "PSG", "Stade Rennais" = "Rennes", "Stade Brestois 29" = "Brest",
              "Vitória Guimarães" = "Vitoria de Guimaraes", "GD Estoril Praia" = "Estoril", "CF Estrela Amadora SAD" = "Estrela da Amadora", 
              "Union Saint-Gilloise" = "Union St.Gilloise", "BK Häcken" = "Haecken" ,"Sparta Rotterdam" = "Sp Rotterdam")

por_vacio <- c("Castilla", "SSC", "CFC", "1.FC", "LOSC", "BSC", "HSC", "AFC", "ACF", "TSC", "KRC", "RSC", " B$", "RCD", "CF", "Hotspur",
               "FC", "UD", "CA", "de Madrid", " U21", "Town", "SS", "Calcio", "1909", "1919", "US", "Balompié", "U23",
               "Hellas", "Munich", " II", "U19", "1846", "Bayer 04", "VfL", "VfB", "SC", "TSG 1899", "SV", "Almelo", "Mestalla",
               "OGC", "Olympique de", "Olympique", "RC", "Alsace", "Stade", "AC", "63", "AS", "SL", "BC", "& Hove Albion",
               "CP", "GD", "Amsterdam", "Arnhem", "Rotterdam", "Piraeus", "SK", "Red Bull", "Donetsk", "FK")

reemplazar_nombres <- function(columna, vector) {
  for (nombre in names(vector)) {
    columna <- gsub(nombre, vector[nombre], columna)
  }
  return(columna)
}

reemplazar_vacio <- function(columna, vector) {
  for (nombre in vector) {
    columna <- gsub(nombre, "", columna)
  }
  return(columna)
}

jugadores_tm$current_club <- reemplazar_nombres(jugadores_tm$current_club, por_algo) 

jugadores_tm$current_club <- reemplazar_vacio(jugadores_tm$current_club, por_vacio)

jugadores_tm[jugadores_tm$current_club == "Sp ", "current_club"] <- "Sparta Rotterdam"
jugadores_tm[jugadores_tm$current_club == "P Eindhoven", "current_club"] <- "PSV Eindhoven"
jugadores_tm[jugadores_tm$current_club == "LK", "current_club"] <- "LASK"

jugadores_tm$current_club <- str_trim(jugadores_tm$current_club)



# 2ª Limpieza: acentos á, é, í, ó, ú y ã õ
#dput(unique(jugadores_opta$team), "equipos_opta.txt")

#unique(jugadores_tm$current_club)

cambios_fin <- c("West Ham Utd" = "West Ham", "Newcastle Utd" = "Newcastle",
                 "Inter Milan" = "Inter", "Milan" = "AC Milan", 
                 "Heidenheim" = "FC Heidenheim", "Köln" = "FC Koln",
                 "Almere City" = "Almere City FC", "Twente Enschede" = "Twente",
                 "Heerenveen" = "SC Heerenveen", "Utrecht" = "FC Utrecht",
                 "Volendam" = "FC Volendam", "Casa Pia" = "Casa Pia AC",
                 "Servette" = "Servette FC", "Backa Topola" = "TSC Backa Topola",
                 "Red Star Belgrade" = "FK Crvena Zvezda", 
                 "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ã" = "a", "õ" = "o", "ö" = "o")


jugadores_tm$current_club <- reemplazar_nombres(jugadores_tm$current_club, cambios_fin)


unique(jugadores_tm$current_club)

jugadores_tm$player_num <- as.numeric(jugadores_tm$player_num)

saveRDS(jugadores_tm, "jugadores_tm.rds")



#####
##### UNIÓN: playernum y shirtno

jugadores_tm <- readRDS("jugadores_tm.rds")

jugadores_tocho <- left_join(jugadores_opta, jugadores_tm, by = c("team" = "current_club", "shirtNo" = "player_num"))

saveRDS(jugadores_tocho, "jugadores3_opta+tm.rds")
