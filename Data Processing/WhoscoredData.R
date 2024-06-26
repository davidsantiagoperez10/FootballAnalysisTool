library(RSelenium)
library(wdman)
library(tidyverse)
library(netstat)
library(jsonlite)
library(tidyr)
library(rvest)

# se inicia el servidor
firefox <- rsDriver(browser = "firefox",
                    chromever = NULL,
                    verbose = F,
                    port = free_port())

# se 'crea' un cliente
cliente <- firefox$client

cliente$open()

cliente$navigate("https://es.whoscored.com")


################################################################################
################################### LINKS ######################################
################################################################################

# LIGA A LIGA: VEO LO QUE CAMBIA DE CADA URL Y HAGO UNA TABLA PARA IR CAMBIÁNDOLO 

# LaLiga: https://es.whoscored.com/Regions/206/Tournaments/4/Seasons/9682/Stages/22176/Fixtures/España-LaLiga-2023-2024
# Premier: https://es.whoscored.com/Regions/252/Tournaments/2/Seasons/9618/Stages/22076/Fixtures/Inglaterra-Premier-League-2023-2024
# Serie A: https://es.whoscored.com/Regions/108/Tournaments/5/Seasons/9659/Stages/22143/Fixtures/Italia-Serie-A-2023-2024
# Bundes: https://es.whoscored.com/Regions/81/Tournaments/3/Seasons/9649/Stages/22128/Fixtures/Alemania-Bundesliga-2023-2024
# Ligue 1: https://es.whoscored.com/Regions/74/Tournaments/22/Seasons/9635/Stages/22105/Fixtures/Francia-Ligue-1-2023-2024
# Eredivisie: https://es.whoscored.com/Regions/155/Tournaments/13/Seasons/9705/Stages/22225/Fixtures/Holanda-Eredivisie-2023-2024
# Champions: https://es.whoscored.com/Regions/250/Tournaments/12/Seasons/9664/Stages/22538/Fixtures/Europa-Champions-League-2023-2024
# Champions-Eliminatorias: https://es.whoscored.com/Regions/250/Tournaments/12/Seasons/9664/Stages/22686/Fixtures/Europa-Champions-League-2023-2024
# UEL: https://es.whoscored.com/Regions/250/Tournaments/30/Seasons/9778/Stages/22539/Fixtures/Europa-League-2023-2024
# UEL-Eliminatorias: https://es.whoscored.com/Regions/250/Tournaments/30/Seasons/9778/Stages/22687/Fixtures/Europa-League-2023-2024
# Portugal: https://es.whoscored.com/Regions/177/Tournaments/21/Seasons/9730/Stages/22254/Fixtures/Portugal-Liga-2023-2024

ligas <- c("España-LaLiga", "Inglaterra-Premier-League", "Italia-Serie-A", "Alemania-Bundesliga", "Francia-Ligue-1", "Holanda-Eredivisie",
           rep("Europa-Champions-League", 8), "Eliminatorias-UCL", rep("Europa-League", 8), "Eliminatorias-UEL", "Portugal-Liga")
region_code <- c(206, 252, 108, 81, 74, 155, rep(250, 18), 177)
tournament_code <- c(4, 2, 5, 3, 22, 13, rep(12, 9), rep(30, 9), 21)
season_code <- c(9682, 9618, 9659, 9649, 9635, 9705, rep(9664, 9), rep(9778, 9), 9730)
stage_code <- c(22176, 22076, 22143, 22128, 22105, 22225,
                seq(22489, 22496), 22686, seq(22510, 22517), 22687, 22254)

ligas_codes <- tibble(ligas, region_code, tournament_code, season_code, stage_code)


# links_partidos <- tibble(liga = as.character(), hrefs = as.character(), scrapped = F)

links_partidos <- readRDS("links_partidos.rds")

for (idx_liga in 1:nrow(ligas_codes)) {
  cadena <- "https://es.whoscored.com/Regions/region_code/Tournaments/tournament_code/Seasons/season_code/Stages/stage_code/Fixtures/league_name-2023-2024"
  
  cat("vamos por la", ligas_codes$ligas[idx_liga], "\n")
  # cambio el link para ajustarlo a la liga
  cadena <- gsub("region_code", ligas_codes$region_code[idx_liga], cadena)
  cadena <- gsub("tournament_code", ligas_codes$tournament_code[idx_liga], cadena)
  cadena <- gsub("season_code", ligas_codes$season_code[idx_liga], cadena)
  cadena <- gsub("stage_code", ligas_codes$stage_code[idx_liga], cadena)
  cadena <- gsub("league_name", ligas_codes$ligas[idx_liga], cadena)
  
  # navego a ese link
  cliente$navigate(cadena)
  
  # lo paro un tiempo porque si no puede saltar un error
  Sys.sleep(runif(1, 10, 14))
  
  # voy a los elementos que contienen los links y al botón para mandar
  # atrás los calendarios y recoger todos los partidos

  elements <- cliente$findElements(using = "css", "[class = 'Match-module_score__5Ghhj']")
  boton <- cliente$findElement(using = "css", "button#dayChangeBtn-prev")

  
  hrefs <- sapply(elements, function(element) {
    element$getElementAttribute("href")
  })
  
  hrefs <- unlist(hrefs)
  
  if (ligas_codes$ligas[idx_liga] %in% c("Eliminatorias-UCL", "Eliminatorias-UEL")){
    elements_elim <- cliente$findElements(using = "css", "[class = 'Match-module_aggregateScoreHome__LaT4t']")
  
  
    hrefs2 <- sapply(elements_elim, function(element) {
      element$getElementAttribute("href")
    })
  
    hrefs <- c(hrefs, unlist(hrefs2))
  }
  
  
  # tabla `temporal´ para ir almacenando los links de cada mes de cada liga
  current_links_partidos <- tibble(liga = ligas_codes$ligas[idx_liga], hrefs = hrefs, scrapped = F)
  
  # BUCLE: para ir añadiendo partidos de las ligas moviendo el calendario para atrás, 
  # y parando cuando ya tengamos los links de estos partidos guardados
  
  
  while (any(!current_links_partidos$hrefs %in% links_partidos$hrefs)) {
    
    duplicados <- duplicated(c(str_extract(links_partidos$hrefs, "(?<=Matches/)[0-9]+"), 
                               str_extract(current_links_partidos$hrefs, "(?<=Matches/)[0-9]+")))[(nrow(links_partidos)+1):(nrow(links_partidos) + nrow(current_links_partidos))]
    
    links_partidos <- rbind(links_partidos, current_links_partidos[!duplicados, ])
    
    if (exists("boton")) {
      # aquí iríamos al mes anterior, y repetimos lo mismo de antes
      boton$clickElement()
    }
    
    # se necesita menos tiempo para que cargue (estamos en el mismo link)
    Sys.sleep(runif(1, 5, 8))
    
    elements <- cliente$findElements(using = "css", "[class = 'Match-module_score__5Ghhj']")
    hrefs <- sapply(elements, function(element) {
      element$getElementAttribute("href")
    })
    
    if (ligas_codes$ligas[idx_liga] %in% c("Eliminatorias-UCL", "Eliminatorias-UEL")){
      elements_elim <- cliente$findElements(using = "css", "[class = 'Match-module_aggregateScoreHome__LaT4t']")
      
      
      hrefs2 <- sapply(elements_elim, function(element) {
        element$getElementAttribute("href")
      })
      
      hrefs <- c(unlist(hrefs), unlist(hrefs2))
    }
    
    # se actualiza la tabla temporal y se comprueba al inicio del bucle si ya tenemos estos links
    current_links_partidos <- tibble(liga = ligas_codes$ligas[idx_liga], hrefs = hrefs, scrapped = F)
  }
  
  # se elimina el botón porque la Champions y UEL no tienen y daría error
  rm(boton)
}

# estarían los links para luego acceder y sacar el eventing
View(links_partidos)



################################################################################
################################# EVENTING #####################################
################################################################################

links_partidos <- readRDS("links_partidos.rds")

## LUEGO VAMOS A CADA UNO DE ELLOS Y LOS SCRAPPEAMOS
#jugadores <- tibble(playerId = numeric(), name = character(), shirtNo = numeric(), team = character(), team_id = numeric())
#eventing <- tibble()

jugadores <- readRDS("jugadores.rds")
eventing <- readRDS("eventing.rds") 

copia_links <- links_partidos[links_partidos$scrapped == FALSE, ]
# copia_links$hrefs <- gsub("Show", "Live", copia_links$hrefs)



for (link in copia_links$hrefs) {
  Sys.sleep(runif(1, 3, 6))
  cat('Partido', which(copia_links$hrefs == link), 
      'de', nrow(copia_links), '\n')
  
  cliente$navigate(link)
  
  # LLEGAMOS AL EVENTING
  tryCatch({
    div_inner_html <- cliente$executeScript(paste("return document.getElementById('layout-wrapper').innerHTML;"))
    matchCentreData <- div_inner_html[[1]]
    
    # voy a coger el id del partido, y luego acotar el string para coger solo el eventing
    elem_match_id <- as.numeric(str_locate(matchCentreData, "matchId")[1])
    elem_inicio <- as.numeric(str_locate(matchCentreData, "matchCentreData")[1])
    elem_fin <- as.numeric(str_locate(matchCentreData, "matchCentreEventTypeJson")[1])
    
    match_id <- substr(matchCentreData, elem_match_id, elem_match_id+20) %>% stringr::str_match("[:digit:]+") %>% as.numeric()
    
    eventing_partido <- substr(matchCentreData, elem_inicio, elem_fin-1) %>% str_trim(side = 'both')
    eventing_partido <- substr(eventing_partido, str_locate(eventing_partido, "matchCentreData")[2]+ 2, nchar(eventing_partido) - 1)  %>% str_trim(side = 'both')
    
    aa <- fromJSON(eventing_partido, flatten = T)
    
    
    if (!is.null(aa)){
      # almacenar plantillas de jugadores
      jugadores_locales <- aa$home$players %>% select(playerId, name, shirtNo) %>% mutate(team = aa$home$name,
                                                                                          team_id = aa$home$teamId)
      jugadores_visitantes <- aa$away$players %>% select(playerId, name, shirtNo) %>% mutate(team = aa$away$name,
                                                                                             team_id = aa$away$teamId)
      plantillas <- rbind(jugadores_locales, jugadores_visitantes)
      
      # unirlas si hay nuevos jugadores
      jugadores <- union(jugadores, plantillas)
      
      
      bb <- aa$events
      
      if ("isOwnGoal" %in% colnames(bb)) {
        # Cambiar 'Goal' a 'OwnGoal' en 'type.displayName' cuando 'is.OwnGoal' es TRUE
        bb <- bb %>%
          mutate(type.displayName = ifelse(isOwnGoal & type.displayName == "Goal", "OwnGoal", type.displayName))
      }
      
      if (!("cardType.displayName") %in% colnames(bb)){
        bb <- bb %>% mutate(cardType.displayName = NA)
      }
        
      # escojo las columnas que quiero del eventing
      match_events <- bb %>% select(eventId, minute, second, teamId, x, y, playerId, endX, endY, 
                                  relatedEventId, relatedPlayerId, blockedX, blockedY, goalMouthZ,
                                  goalMouthY, isShot, period.displayName, type.displayName, 
                                  outcomeType.displayName, cardType.displayName) %>% 
                             mutate(match_id = match_id,
                                    competicion = copia_links$liga[which(copia_links$hrefs == link)],
                                    partido = str_c(aa$home$name, '-', aa$away$name, sep = ' '),
                                    resultado = str_c(unlist(str_split(aa$score, ":"))[1] %>% str_trim(), '-', unlist(str_split(aa$score, ":"))[-1] %>% str_trim(), sep = ' '),
                                    dia = unlist(str_split(aa$startTime, 'T'))[1] %>% as.Date() %>% format("%d/%m/%Y"),
                                    hora = unlist(str_split(aa$startTime, 'T'))[2] %>% strptime(format = "%H:%M:%S") %>% format("%H:%M")) %>% 
                             filter(!(type.displayName %in% c("Start", "End", "FormationSet")))
    
      eventing <- bind_rows(eventing, match_events)
      
      links_partidos$scrapped[which(links_partidos$hrefs == link)] <- T
    }
  }, error = function(e){
    cat('Partido', which(copia_links$hrefs == link), "no empezó \n")
  })
  
}


View(eventing)
View(jugadores)

saveRDS(eventing, "eventing.rds")
saveRDS(jugadores, "jugadores.rds")
saveRDS(links_partidos, "links_partidos.rds")


# links_partidos$match_id <- as.numeric(str_extract(links_partidos$hrefs, "(?<=Matches/)\\d+"))
# eventing2 <- left_join(eventing, links_partidos, by = "match_id") %>% mutate(competicion = liga) %>%  select(c(names(eventing), "competicion"))


#paramos el servidor
firefox$server$stop()


################################################################################
#################################### AUXILIAR ##################################
################################################################################

cliente$navigate("https://es.whoscored.com/Matches/1734710/Live/Espa%C3%B1a-LaLiga-Villarreal-Osasuna")

# LLEGAMOS AL EVENTING
div_inner_html <- cliente$executeScript(paste("return document.getElementById('layout-wrapper').innerHTML;"))
matchCentreData <- div_inner_html[[1]]


# matchId, matchCentreData, matchCentreEventTypeJson, formationIdNameMappings
elem_match_id <- as.numeric(str_locate(matchCentreData, "matchId")[1])
elem_inicio <- as.numeric(str_locate(matchCentreData, "matchCentreData")[1])
elem_fin <- as.numeric(str_locate(matchCentreData, "matchCentreEventTypeJson")[1])

match_id <- substr(matchCentreData, elem_match_id, elem_match_id+20) %>% stringr::str_match("[:digit:]+") %>% as.numeric()

eventing_partido <- substr(matchCentreData, elem_inicio, elem_fin-1) %>% str_trim(side = 'both')
eventing_partido <- substr(eventing_partido, str_locate(eventing_partido, "matchCentreData")[2]+ 2, nchar(eventing_partido) - 1)  %>% str_trim(side = 'both')
eventing_partido

aa <- fromJSON(eventing_partido, flatten = T)

# almacenar jugadores
jugadores_locales <- aa$home$players %>% select(playerId, name, shirtNo, height, weight, age) %>% mutate(team = aa$home$name,
                                                                                                team_id = aa$home$teamId)
jugadores_visitantes <- aa$away$players %>% select(playerId, name, shirtNo, height, weight, age) %>% mutate(team = aa$away$name,
                                                                                                   team_id = aa$away$teamId)
plantillas <- rbind(jugadores_locales, jugadores_visitantes)

# if !any plantillas$playerid in jugadores$playerid then union


# eventing: añadir nombre de los equipos !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
bb <- aa$events

if ("isOwnGoal" %in% colnames(bb)) {
  # Cambiar 'Goal' a 'OwnGoal' en 'type.displayName' cuando 'is.OwnGoal' es TRUE
  bb <- bb %>%
    mutate(type.displayName = ifelse(isOwnGoal & type.displayName == "Goal", "OwnGoal", type.displayName))
}

match_events <- bb %>% select(eventId, minute, second, teamId, x, y, playerId, endX, endY, 
                              relatedEventId, relatedPlayerId, blockedX, blockedY, goalMouthZ,
                              goalMouthY, isShot, period.displayName, type.displayName, 
                              outcomeType.displayName, cardType.displayName) %>% 
                        mutate(match_id = match_id,
                               partido = str_c(aa$home$name, '-', aa$away$name, sep = ' '),
                               resultado = str_c(unlist(str_split(aa$score, ":"))[1] %>% str_trim(), '-', unlist(str_split(aa$score, ":"))[-1] %>% str_trim(), sep = ' '),
                               dia = unlist(str_split(aa$startTime, 'T'))[1] %>% as.Date() %>% format("%d/%m/%Y"),
                               hora = unlist(str_split(aa$startTime, 'T'))[2] %>% strptime(format = "%H:%M:%S") %>% format("%H:%M")) %>% 
                        filter(!(type.displayName %in% c("Start", "End", "FormationSet")))


if ("isOwnGoal" %in% colnames(bb)) {
  # Cambiar 'Goal' a 'OwnGoal' en 'type.displayName' cuando 'is.OwnGoal' es TRUE
  bb <- bb %>%
    mutate(type.displayName = ifelse(isOwnGoal & type.displayName == "Goal", "OwnGoal", type.displayName))
}
# las dimensiones de fotmob son (105, 68) -> las custom

# plantillas con el eventing para coger equipo y dorsal
# tiros con lo resultante y ver como hacer
eventing <- readRDS("event_prueba.rds")

cc <- left_join(eventing, plantillas, by = 'playerId')
cc <- cc %>% select(names(eventing), name, shirtNo)



tiritos <- readRDS("tiros_completados.rds")



#paramos el servidor
firefox$server$stop()
