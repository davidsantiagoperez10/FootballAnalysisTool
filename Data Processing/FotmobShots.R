library(dplyr)
library(rvest)
library(jsonlite)
library(readxl)
library(httr)
library(purrr)
library(stringr)

ligas <- read_excel("listas_url.xlsx")
codigo <- 4537
ligas$URL <- gsub(3724,codigo,ligas$URL)


# partidos <- tibble()

partidos <- readRDS("partidos_fotmob_completados.rds")

# Partidos
convert_round_name <- function(inner_list) {
  inner_list$roundName <- as.character(inner_list$roundName)
  return(inner_list)
}

for (code in ligas$URL){
  Sys.sleep(runif(1, 0, 1))
  
  partidos_liga <- GET(url = code) %>% content(as="parsed") 
  partidos_liga <- partidos_liga[["pageProps"]][["matches"]][["allMatches"]]
  
  partidos_liga <- map(partidos_liga, convert_round_name)
  
  partidos_liga <- map_dfr(partidos_liga, as.data.frame)
  
  partidos_liga <- partidos_liga %>% select(id, roundName, home.name, home.id, 
                                            away.name, away.id, status.finished,
                                            status.started, status.cancelled, 
                                            status.scoreStr, status.utcTime, pageUrl) %>% 
                   mutate(partido = str_c(home.name %>% str_trim(), '-', away.name %>% str_trim(), sep = ' '),
                          fecha = as.POSIXct(status.utcTime, tz = "UTC") %>% format("%d/%m/%Y"),
                          tiros_sacados = F) %>% 
                   filter(status.finished == TRUE, status.cancelled == FALSE)
  
  partidos_liga <- partidos_liga %>% select(-status.utcTime)
  
  
  duplicados <- duplicated(rbind(partidos[, c("id", "home.name", "away.name")], 
                                 partidos_liga[, c("id", "home.name", "away.name")]))[(nrow(partidos)+1):(nrow(partidos) + nrow(partidos_liga))]
  
  partidos <- rbind(partidos, partidos_liga[!duplicados, ])
}


df_tiros <- readRDS("tiros_fotmob_completados.rds")


por_sacar_tiros <- partidos[partidos$tiros_sacados == F, ]



# Tiros
for (match_id in por_sacar_tiros$id) {
  Sys.sleep(runif(1, 0, 1))
  cat("Partido", which(por_sacar_tiros$id == match_id), "de", nrow(por_sacar_tiros), "\n")
  
  url<-paste('https://data.fotmob.com/matchfacts.',match_id,'.fot.gz',sep="")
  
  a <- read_html(url)
  
  tiros <- unlist(c(gregexpr("<shotmap>", a), gregexpr("</shotmap>", a)))
  
  if (tiros[1] != -1){
    # recorte para coger solo los tiros
    b <- substr(a, start = tiros[1]-9, stop = tiros[2]+1)
    
    # recorte para directamente tener el json
    b <- substr(b, start = str_locate(b, "\\{")[1], stop = str_locate(b, "\\</")[1]-1)
    
    
    df_tiros_partido <- fromJSON(b)[['shots']]
    
    df_tiros_partido <- df_tiros_partido %>% select(id, eventType, teamId, playerId, playerName,
                                                    x, y, min, minAdded, isBlocked, isOnTarget, blockedX,
                                                    blockedY, goalCrossedY, goalCrossedZ, expectedGoals,
                                                    expectedGoalsOnTarget, shotType, situation, isOwnGoal) %>% 
                                             mutate(dorsal = 0,
                                                    match_id = match_id,
                                                    xG = round(expectedGoals, 2),
                                                    xGOT = round(expectedGoalsOnTarget, 2))
    
    # para hacer el dorsal
    i <- 1
    for (player in df_tiros_partido$playerName) {
      df_tiros_partido$dorsal[i] <- substr(a %>% html_text(), str_locate(a %>% html_text(), str_c(player, ': :'))[1],
                                str_locate(a %>% html_text(), str_c(player, ': :'))[2]+2) %>% str_match('[:digit:]+') %>% as.numeric() 
      i <- i+1
    }
    
    df_tiros_partido <- df_tiros_partido %>% inner_join(por_sacar_tiros, by = c("match_id" = "id"))
    
    teamName <- ifelse(df_tiros_partido$teamId == df_tiros_partido$home.id, df_tiros_partido$home.name, df_tiros_partido$away.name)
    
    df_tiros_partido$teamName <- teamName
    
    df_tiros_partido <- df_tiros_partido %>% select(id, match_id, eventType, teamId, teamName, playerId, playerName, dorsal,
                                    x, y, min, minAdded, isBlocked, isOnTarget, blockedX,
                                    blockedY, goalCrossedY, goalCrossedZ, shotType, situation,
                                    isOwnGoal, xG, xGOT, partido, fecha)

    
    
    df_tiros <- union(df_tiros, df_tiros_partido)
    partidos$tiros_sacados[which(partidos$id == match_id)] <- T
  }
}

View(df_tiros)


## GUARDO LOS ARCHIVOS PARA DESPUÉS SOLO ACCEDER A LOS NUEVOS PARTIDOS COMPLETADOS
saveRDS(partidos, "partidos_fotmob_completados.rds")

saveRDS(df_tiros, "tiros_fotmob_completados.rds")





################################# AUXILIAR #####################################

# accedo a los tiros de un partido para probar

url<-paste('https://data.fotmob.com/matchfacts.',4205343,'.fot.gz',sep="")

a <- read_html(url)


tiros <- unlist(c(gregexpr("<shotmap>", a), gregexpr("</shotmap>", a)))

# dorsal: 
c <- substr(a %>% html_text(),  str_locate(a %>% html_text(), "Isi Palazon: :")[1],
            str_locate(a %>% html_text(), "Isi Palazon: :")[2]+2) %>% str_match('[:digit:]+') %>% as.numeric()

tiros <- unlist(c(gregexpr("<shotmap>", a), gregexpr("</shotmap>", a)))

# recorte para coger solo los tiros
a <- substr(a, start = tiros[1]-8, stop = tiros[2]+1)

# recorte para directamente tener el json
a <- substr(a, start = str_locate(a, "\\{")[1], stop = str_locate(a, "\\</")[1]-1)


df_tiros_partido <- fromJSON(a)[['shots']]

df_tiros_partido$match_id <- partidos$id[1]



