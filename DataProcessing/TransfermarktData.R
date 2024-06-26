library(worldfootballR)
library(dplyr)


ligas <- c("Spain", "England", "Italy", "Germany", "Portugal", "Netherlands",
           "Switzerland", "France", "Scotland", "Denmark", "Austria", "Serbia",
           "Belgium", "Ukraine", "Sweden", "Greece", "Cyprus", "Turkey", "Czech Republic"
           )


##################
links_ligas <- c()

for (liga in ligas) {
  link1 <- tm_league_team_urls(country_name = liga, start_year = 2023)
  links_ligas <- c(links_ligas, link1)
}


for (i in 1:length(links_ligas)) {
  print(links_ligas[i])
  jugadores_team <- tm_player_bio(tm_team_player_urls(links_ligas[i]))
  
  if (exists("jugadores_full")) {
    jugadores_full <- bind_rows(jugadores_full, jugadores_team)
  }
  
  else {
    jugadores_full <- jugadores_team
  }
}

View(jugadores_full)

# names(jugadores_full)
jugadores_full <- jugadores_full %>% select(player_name, height, position, foot, player_agent, current_club, joined, contract_expires, URL, on_loan_from, contract_there_expires, contract_option, date_of_birth)

for (liga in ligas) {
  nuevas_col <- tm_player_market_values(country_name = liga, start_year = 2023) %>% select(player_num, player_age, player_nationality, joined_from, player_market_value_euro)
  
  if (exists("cols_add")){
    cols_add <- rbind(cols_add, nuevas_col)
  }
  
  else {
    cols_add <- nuevas_col
  }
}

saveRDS(cols_add, "colsadd.rds")
saveRDS(jugadores_full, "jugfull.rds")




colsadd <- readRDS("colsadd.rds")
jugfull <- readRDS("jugfull.rds")


#4189 A TOMAR POR CULO
colsadd <- colsadd[-4189, ]

jugadores_ligas <- cbind(jugfull, colsadd)


jugadores_ligas$player_market_value_euro <- as.numeric(jugadores_ligas$player_market_value_euro)
options(scipen = 999)


jugadores_ligas$foto <- unlist(lapply(jugadores_ligas$URL, function(url) {
  # Lee el HTML de la URL
  src <- NA
  print(url)
  
  if (!is.na(url)){
    html <- read_html(url)
    # Extrae el src de la imagen
    src <- html %>% html_elements(".data-header__profile-image") %>% html_attr("src")
  }
  
  return(src)
}))


jugadores_ligas$name <- chartr("áéøíóúÁÉÍÓÚãõäöüëÖÜØ", "aeoiouAEIOUaoaoueOUO", jugadores_ligas$name)
jugadores_ligas$name <- str_replace(jugadores_ligas$name, "Toti Gomes", "Toti")
jugadores_ligas$name <- str_replace(jugadores_ligas$name, "Danley Jean Jacques", "Danley Jean-Jacques")
jugadores_ligas$name <- str_replace(jugadores_ligas$name, "Eli Junior Kroupi", "Eli Kroupi")
jugadores_ligas$name <- str_replace(jugadores_ligas$name, "Mousa Al Ta'mari", "Mousa Tamari")
jugadores_ligas$name <- str_replace(jugadores_ligas$name, "Kornelius Normann Hansen", "Kornelius Hansen")
jugadores_ligas$name <- str_replace(jugadores_ligas$name, "Jose Rodriguez", "Puma Rodriguez")

jugadores_ligas <- jugadores_ligas[!duplicated(jugadores_ligas[, c("name", "team")]),]


saveRDS(jugadores_ligas, file = "jugadores_transfermarkt.rds")


