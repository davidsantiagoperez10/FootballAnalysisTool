library(dplyr)
library(stringr)


eventing <- readRDS("eventingytiros_opta.rds")
partidos <- unique(eventing$match_id)
jugadores_opta <- readRDS("jugadores.rds")

eventing <- eventing %>% left_join(unique(jugadores_opta[, c("team", "team_id")]), by = c("teamId" = "team_id"))

eventing$posesion <- NA
eventing$fasedejuego <- NA


# CLUSTERING PARA ACELERAR EL PROCESO #
library(parallel)
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(stringr)
  eventing <- readRDS("eventingytiros_opta.rds")
  jugadores_opta <- readRDS("jugadores.rds")
  eventing <- eventing %>% left_join(unique(jugadores_opta[, c("team", "team_id")]), by = c("teamId" = "team_id"))
  eventing$posesion <- NA
  eventing$fasedejuego <- NA
  partidos <- unique(eventing$match_id)
  
})

preparado_fases <- function(part_id){
  cat("Partido", which(partidos == part_id), "de", length(partidos), '\n')
  eventing_mejorado <- eventing %>% filter(match_id == part_id)  
  
  eventing_mejorado <- eventing_mejorado %>% left_join(jugadores_opta, by = "playerId", relationship = "many-to-many") %>% select(c(names(eventing_mejorado)[1:7], "name", names(eventing_mejorado)[8:28]))
  eventing_mejorado$name <- chartr("áéøíóúÁÉÍÓÚãõäöüëÖÜØ", "aeoiouAEIOUaoaoueOUO", eventing_mejorado$name)
  eventing_mejorado$name <- str_replace(eventing_mejorado$name, "Toti Gomes", "Toti")
  eventing_mejorado$name <- str_replace(eventing_mejorado$name, "Danley Jean Jacques", "Danley Jean-Jacques")
  eventing_mejorado$name <- str_replace(eventing_mejorado$name, "Eli Junior Kroupi", "Eli Kroupi")
  eventing_mejorado$name <- str_replace(eventing_mejorado$name, "Mousa Al Ta'mari", "Mousa Tamari")
  eventing_mejorado$name <- str_replace(eventing_mejorado$name, "Kornelius Normann Hansen", "Kornelius Hansen")
  eventing_mejorado$name <- str_replace(eventing_mejorado$name, "Jose Rodriguez", "Puma Rodriguez")
  
  
  eventing_mejorado <- eventing_mejorado[!duplicated(eventing_mejorado),]
  
  ## IDENTIFICAR MOMENTOS DE JUEGO: CONST. OFENSIVA - TRANS D-A / A-D O FASE DEFENSIVA
  
  # en verdad solo hay que identificar fase ofensiva o transición d-a, el resto son más sin balón
  
  ### F.O. - desde abajo, posesiones largas, más horizontales 
  
  ### TRANS D-A : con un robo o pérdida rival, primeros (10) pases llegando rápido al área contraria
  
  eventing_mejorado$posesion <- NA
  
  # pérdidas de posesión
  
  perdidas <- which((eventing_mejorado$type.displayName == "Pass" & eventing_mejorado$outcomeType.displayName == "Unsuccessful") | 
                      (eventing_mejorado$type.displayName == "MissedShots" & eventing_mejorado$outcomeType.displayName == "Successful") |
                      (eventing_mejorado$type.displayName == "Foul" & eventing_mejorado$outcomeType.displayName == "Successful") | 
                      (eventing_mejorado$type.displayName == "CornerAwarded" & lead(eventing_mejorado$type.displayName == "CornerAwarded")) | 
                      (eventing_mejorado$type.displayName == "SavedShot" & eventing_mejorado$outcomeType.displayName == "Successful") | 
                      (eventing_mejorado$type.displayName == "Goal" & eventing_mejorado$outcomeType.displayName == "Successful") | 
                      (eventing_mejorado$type.displayName == "OwnGoal" & eventing_mejorado$outcomeType.displayName == "Successful") | 
                      (eventing_mejorado$type.displayName == "OffsidePass" & eventing_mejorado$outcomeType.displayName == "Successful") | 
                      (eventing_mejorado$type.displayName == "Dispossessed" & eventing_mejorado$outcomeType.displayName == "Successful") | 
                      (eventing_mejorado$type.displayName == "BallTouch" & eventing_mejorado$outcomeType.displayName == "Unsuccessful") |
                      (eventing_mejorado$type.displayName == "TakeOn" & eventing_mejorado$outcomeType.displayName == "Unsuccessful"))
  
  intervalos <- c()
  
  for (perdida in perdidas) {
    aux <- eventing_mejorado[perdida+1:nrow(eventing_mejorado),]
    
    intervalos <- c(intervalos, which((aux$type.displayName == "Pass" | aux$type.displayName == "Interception" | aux$type.displayName == "BallRecovery" | aux$type.displayName == "KeeperPickup" | aux$type.displayName == "TakeOn" | str_detect(aux$type.displayName, "Shot")) & aux$outcomeType.displayName == "Successful")[1])
  }
  
  inicio_pos <- perdidas + intervalos
  inicio_pos <- c(na.omit(c(1, inicio_pos)))
  
  
  num_pos <- 1
  for (value in perdidas) {
    eventing_mejorado$posesion[inicio_pos[min(which(perdidas==value), length(inicio_pos))]:value] <- num_pos
    num_pos <- num_pos + 1
  }
  
  
  ## CONTRAATAQUES:
  recuperaciones <- eventing_mejorado %>% filter(lag(type.displayName) == "BallRecovery" |
                                                   lag(type.displayName) == "KeeperPickup" |
                                                   (lag(type.displayName) == "BlockedPass" & lag(teamId) == teamId & type.displayName == "Pass" & outcomeType.displayName == "Successful"))
  
  posibles_contras <- eventing_mejorado %>% filter(posesion %in% recuperaciones$posesion)
  
  
  posibles_contras <- posibles_contras %>% filter(!is.na(posesion))
  
  posesiones_contraataque_vertical <- posibles_contras %>%
    group_by(posesion) %>%
    mutate(verticales = all(diff(x[teamId == first(teamId)]) > -5) & first(x) < 50 & last(x) > 70 & first(teamId) == last(teamId),
           duracion = last(minute)*60 + last(second) - first(minute)*60 - first(second)) %>% 
    filter(verticales, duracion < 20)
  
  
  ## ABP:
  abp <- eventing_mejorado %>% filter(type.displayName == "Foul" | (type.displayName == "CornerAwarded"))
  
  posesiones_abp <- unique(abp$posesion) + 1
  
  posible_abp <- eventing_mejorado %>% filter(posesion %in% c(na.exclude(posesiones_abp))) %>% 
    group_by(posesion) %>% filter(first(x) > 70 | any(endX > 80))
  
  
  # Meter fases de juego
  eventing_mejorado$fasedejuego <- NA
  
  eventing_mejorado <- eventing_mejorado %>% mutate(fasedejuego = ifelse(posesion %in% posesiones_contraataque_vertical$posesion, "Transicion DEF-AT Vertical", 
                                                                         ifelse(posesion %in% posibles_contras$posesion, "Transicion DEF-AT",
                                                                                ifelse(posesion %in% posible_abp$posesion, "ABP", 
                                                                                       ifelse(is.na(posesion), NA, "Construccion Ofensiva"))))) %>% 
    mutate(fasedejuego = ifelse(type.displayName == "CornerAwarded", NA, fasedejuego))
  
  return(eventing_mejorado)
  #eventing[eventing$match_id == part_id, c("posesion", "fasedejuego")] <- eventing_mejorado[, c("posesion", "fasedejuego")]
  
}

eventing_bueno <- parSapply(cl, partidos, preparado_fases)


eventing_guapo <- tibble()

for (part in 1:ncol(eventing_bueno)) {
  cat("Partido", part, "\n")
  eventing_guapo <- bind_rows(eventing_guapo, as.data.frame(eventing_bueno[, part]))
}

View(eventing_guapo)


eventing_guapo$xGOT <- eventing$xGOT
eventing_guapo$team <- eventing$team

eventing_guapo$partido <- gsub("Union St.Gilloise", "Union St Gilloise", eventing_guapo$partido)
eventing_guapo$team <- gsub("Union St.Gilloise", "Union St Gilloise", eventing_guapo$team)
jugadores_opta$team <- gsub("Union St.Gilloise", "Union St Gilloise", jugadores_opta$team)

eventing_guapo$partido <- gsub("Borussia M.Gladbach", "Borussia Monchengladbach", eventing_guapo$partido)
eventing_guapo$team <- gsub("Borussia M.Gladbach", "Borussia Monchengladbach", eventing_guapo$team)
jugadores_opta$team <- gsub("Borussia M.Gladbach", "Borussia Monchengladbach", jugadores_opta$team)


saveRDS(eventing_guapo, "eventing_confases.rds")
saveRDS(jugadores_opta, "jugadores.rds")


