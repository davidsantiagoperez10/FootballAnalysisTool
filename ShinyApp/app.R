## shinydashboard app
library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(shinyWidgets)

library(dplyr)
library(rvest)
library(jsonlite)
library(readxl)
library(httr)
library(tidyr)
library(purrr)
library(stringr)
library(plotly)
library(ggsoccer)
library(ggplot2)
library(igraph)
library(gganimate)
library(png)
library(stringi)
library(shinyjs)
library(scales)
library(htmltools)
library(ggforce)
library(magick)
library(grid)


### FUNCIONES Y CARGA DE DATOS:
eventing <- readRDS("eventing_confases.rds")
jugadores <- readRDS("jugadores3_opta+tm.rds")


partidos <- unique(eventing[, c("partido", "competicion")])


### LOGOS ------------------------
# logos:


competis <- unique(eventing$competicion)
logos <- c("LaLiga.png",
           "Premier League.png",
           "Serie A Tim.png",
           "Bundesliga.png",
           "Ligue 1 Uber Eats.png",
           "UCL.png",
           "UCL_KO.png",
           "UEFA Europa League.png",
           "UEL_KO.png",
           "Liga Portugal Betclic.png")

lista_opciones <- lapply(1:length(competis), function(i) {
  option <- competis[i]
  img_src <- paste("Escudos", competis[i], logos[i], sep = "/")
  list(label = option, value = img_src)
})


texto_opciones <- sapply(lista_opciones, function(x) x$label)
imagenes_locales <- sapply(lista_opciones, function(x) x$value)


imagenes_opciones <- c("https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEiXMAnlv_D8a97HlbLL1EbDFAROBs6RG-2u8cmoDuPSN62tRREbbJjadN--bo19X62KRntqUrDviCIy477Jyzur3EgHosMr2imRLlG4z375J6eKs35Kw5TIytTB-icWToWwgcRYrEPaSLCkGmpAv2lvtSIht_QtdC35hOKhPurxIGQigY8_jl3uC1TtR2A/s16000/LaLiga%20EA%20Sports.png",
                       "https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEinfbw332e0UzbLOV7XBjJtUA43wR1Lm7cRq1UKVMHwTpK2J49ZE3vbN6zhmGX-72OHQcHZo63GtpH-qT6CBZ409itDt9bqeTARdwkJON1vMQkDLx5MA0HhkuUxmaTrjfQwyrwl-8QSkEESg_Toj_IF2zMxyAYqcA2Buw1Dq2f_5FeKnLpnWkPXfOBD-DI/s512/Premier%20League.png",
                       "https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhwuwAIHMWYWHY8mTnrckP8eMqgzM4Ca2uAul_a6aSwEUN3nN7WSmalO6BnI5QkYbPTt12kz4POkp6l51C_v5U3ePGvZgOippOXr9ZCpDnEzqqIc4QYK9CRaI0JecX1jogGgswP5uG8lxc0Pcy-_d0PhPRE3R57uA6MY9nuCXpNGzX7x9nDjZuKQoaq/s256/Lega%20Serie%20A%20TIM256x.png",
                       "https://1.bp.blogspot.com/-890CN7NVBdM/Xmp0Mb2SqPI/AAAAAAABX_Y/syOpxjGu1HoTshxENdNdk5nND0BOGD9XwCLcBGAsYHQ/s1600/Bundesliga.png",
                       "https://1.bp.blogspot.com/-JcPz_yTeORc/X2P0vbBSBuI/AAAAAAABgJg/JkAShhgU0RcXVdRrSerH2LpGmNvJqlhnwCLcBGAsYHQ/s0/Ligue%2B1%2BUber%2BEats256x.png",
                       "https://1.bp.blogspot.com/-hcnyHLOnvdo/YNigUJVjEwI/AAAAAAABkCQ/HLVaLvjDz20rOk9Q6MURH8zNePnWY-57gCLcBGAsYHQ/s16000/UCL.png",
                       "https://i.postimg.cc/RZ057kYm/UCL-KO.png",
                       "https://1.bp.blogspot.com/-czO_p_38Lxc/Xav99TUoU2I/AAAAAAABWZ4/aICYUJ7471w9-mpDvEaT-LsV-MJvT5C_QCLcBGAsYHQ/s1600/UEFA%2BEuropa%2BLeague.png",
                       "https://i.postimg.cc/NjNtZsXL/UEL-KO.png",
                       "https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhS-kSygMGXmTNiAsAE4h4SxRYM60CBB3qFSGWmZ-p9K5oan7x_tZuvAcV6J_m82eCikjvTuFzdEIU4IgH76DV2CMNQPzjAnKa9EvCoCuxshbdDdnDOICrtOazEcK00PoQXooIb_06FLy1fzbM4aLrEPJ9k5vE9Rfdu6s09KF1l4dJDEgxCEGZ4mqslxIk/w1200-h630-p-k-no-nu/Liga%20Portugal%20Betclic.png"
                       )


logos_equipos <- lapply(1:length(competis), function(i) {
  dir(paste("Escudos", competis[i], sep = "/"))
})

logos_equipos <- unlist(logos_equipos) 



### FUNCIONES ------------------------

mostrar_escudo <- function(ruta){
  par(mar = c(0,0,0,0))
  
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  return(rasterImage(readPNG(ruta), 0, 0, 1, 1))
}


## Centros
sacar_centros <- function(datos){
  centros <- datos %>% filter(type.displayName == "Pass", (y <= 25 & between(endY, 30+y, 80)) | (y >= 75 & between(endY, 20, y-30)), endX > 85) %>% select(x, y, endX, endY, name, outcomeType.displayName, fasedejuego)
  return(centros)
}

todas_franjas <- data.frame(
  franja = c("Far Right", "Deep Right", "Far Left", "Deep Left")
)

poligonos <- data.frame(
  x = c(50, 85, 85, 50, 85, 100, 100, 85, 85, 50, 50, 85, 100, 85, 85, 100),
  y = c(0, 0, 25, 25, 0, 0, 25, 25, 75, 75, 100, 100, 75, 75, 100, 100),
  franja = rep(c("Far Right", "Deep Right", "Far Left", "Deep Left"), each = 4)
)

poligonosDef <- data.frame(
  x = c(50, 15, 15, 50, 15, 0, 0, 15, 15, 50, 50, 15, 0, 15, 15, 0),
  y = c(100, 100, 75, 75, 100, 100, 75, 75, 25, 25, 0, 0, 25, 25, 0, 0),
  franja = rep(c("Far Right", "Deep Right", "Far Left", "Deep Left"), each = 4)
)

## Secuencias de pases más repetidas
find_sequences <- function(df, k) {
  sequences <- df %>%
    group_by(posesion) %>%
    filter(n() >= k) %>%
    summarise(sequence = list(map(seq_len(n() - k + 1), 
                                  ~paste(name[.x:(.x + k - 1)], collapse = " - "))),
              .groups = "drop") %>%
    unnest(sequence) %>%
    count(sequence, sort = TRUE)
  
  return(sequences)
}
combinaciones_comunes <- function(datos, limite){
  aux <- datos %>% group_by(partido, posesion) %>% summarise(n_pases = n()) %>% filter(n_pases >= limite)
  
  tabla <- datos %>% semi_join(aux, by = c("partido", "posesion")) %>% select(name, partido, posesion)
  
  all_sequences <- list()
  for (k in limite:5) {  
    all_sequences[[paste0("length_", k)]] <- find_sequences(tabla, k)
  }
  
  combined_sequences <- bind_rows(all_sequences) %>% arrange(desc(n))
  
  return(combined_sequences[1:5,])
}

OptaMAPcampofutbol <- function(){
  
  #Creamos la plantilla del tema del gr?fico (esto es de soccermatics con algunos cambios)
  theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="#3ab54a", colour=NA),
      legend.key=element_rect(colour="#3ab54a",fill="#3ab54a"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "#3ab54a", fill = "#3ab54a", size = .5),
      panel.background=element_rect(fill="#3ab54a",colour="#3ab54a"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="#3ab54a",size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0
  xmin <- 0
  
  # Defining dimensions
  GoalWidth <- 732
  penspot <- 1100
  boxedgeW <- 4032
  boxedgeL <- 1650
  box6yardW <- 1832
  box6yardL <- 550
  
  ## dimensions calculations
  # The 18 Yard Box
  TheBoxWidth <- c(((7040 / 2) + (boxedgeW / 2)),((7040 / 2) - (boxedgeW / 2)))
  TheBoxHeight <- c(boxedgeL,10600-boxedgeL)
  GoalPosts <- c(((7040 / 2) + (GoalWidth / 2)),((7040 / 2) - (GoalWidth / 2)))
  
  # The 6 Yard Box
  box6yardWidth <- c(((7040 / 2) + (box6yardW / 2)),((7040 / 2) - (box6yardW / 2)))
  box6yardHeight <- c(box6yardL,10600-box6yardL)
  
  ## Centre circle dimensions
  centreCirle_d <- 1830
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create leftD arc ####
  Dleft <- circleFun(c((penspot),(7040/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]
  
  ## create rightD arc  ####
  Dright <- circleFun(c((10600-(penspot)),(7040/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dright <- Dright[which(Dright$x <= (10600-(boxedgeL))),]
  
  #### create center circle ####
  center_circle <- circleFun(c((10600/2),(7040/2)),centreCirle_d,npoints = 100)
  
  ## create corner flag radius ####
  TopLeftCorner <- circleFun(c(xmin,7040),200,npoints = 1000)
  TopRightCorner <- circleFun(c(10600,7040),200,npoints = 1000)
  BottomLeftCorner <- circleFun(c(xmin,ymin),200,npoints = 1000)
  BottomRightCorner <- circleFun(c(10600,ymin),200,npoints = 1000)
  
  p <- ggplot() +
    
    xlim(c(-1000,10600+1000)) + ylim(c(-10,7040+10)) +
    theme_blankPitch() +
    geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "#3ab54a", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#3ab54a", colour = "#ffffff") +
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#3ab54a", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#3ab54a", colour = "#ffffff")  +
    geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#3ab54a", colour = "#ffffff")  +
    geom_segment(aes(x = 10600/2, y = ymin, xend = 10600/2, yend = 7040),colour = "#ffffff") +
    geom_path(data=Dleft, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=Dright, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=center_circle, aes(x=x,y=y), colour = "#ffffff") +
    geom_point(aes(x = penspot , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600-(penspot)) , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600/2) , y = 7040/2), colour = "#ffffff") +
    geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "white", size = 1) +
    geom_segment(aes(x = 10600, y = GoalPosts[1], xend = 10600, yend = GoalPosts[2]),colour = "white", size = 1)+
    theme(legend.position="bottom")
  
  return(p)
  
}
segmentsDf <- function(data, shorten.start, shorten.end, offset){
  
  data$dx = data$x - data$avg_x
  data$dy = data$y - data$avg_y
  data$dist = sqrt( data$dx^2 + data$dy^2 )
  data$px = data$dx/data$dist
  data$py = data$dy/data$dist
  
  
  data$x2 = data$avg_x + data$px * shorten.start
  data$y2 = data$avg_y + data$py * shorten.start
  data$xend = data$x - data$px * shorten.end
  data$yend = data$y - data$py * shorten.end
  data$x2 = data$x2 - data$py * offset
  data$xend = data$xend - data$py * offset
  data$y2 = data$y2 + data$px * offset
  data$yend = data$yend + data$px * offset
  
  data<-data.frame(data)
  
  return(data)
}

red_pases <- function(datos, partidos, fasedj, equipo, minutos = 0:130){
  
  subset_querido <- datos %>% filter(partido %in% partidos, fasedejuego == fasedj,
                                     team == equipo, between(minute, minutos[1], minutos[length(minutos)]))
  
  para_red <- subset_querido %>% filter(type.displayName == "Pass", outcomeType.displayName == "Successful", team == equipo) %>% 
    mutate(destinatario = ifelse(lead(posesion) == posesion, lead(name), NA)) %>% 
    filter(!is.na(destinatario))
  
  para_red <- para_red %>% group_by(name) %>% mutate(x = mean(x), y = mean(y)) 
  
  para_red <- para_red %>% group_by(name, x, y, destinatario) %>% summarise(pases = n())
  
  
  # nodos
  knodes <- para_red %>% group_by(name, x, y) %>% summarise(total = sum(pases))
  
  pasestotales<-sum((para_red$pases))
  
  # Saco la matriz sin portero para calcular el convex hull y sacar profundidad y amplitud
  portero <- (jugadores %>% filter(team == equipo, position == "Goalkeeper") %>% select(name))$name
  
  player_data_gk<-para_red %>%
    filter(!(name %in% portero)) ################## CAMBIAR LÍNEA POR PORTERO A SECAS
  
  # Me quedo con duplas de 2 o mas pases  
  player_data_gk<-player_data_gk %>%
    filter(pases>=2)
  
  # valores para profundidad y amplitud  
  x_max_prof<-max(player_data_gk$x)*106
  x_min_prof<-min(player_data_gk$x)*106
  y_max_ampl<-max(player_data_gk$y)*70.4
  y_min_ampl<-min(player_data_gk$y)*70.4
  
  
  hull_coor <- player_data_gk %>%
    slice(chull(x,y)) %>%
    mutate(hull1= 1)
  
  centro_pol<-player_data_gk %>%
    summarise(xm=mean(x),ym=mean(y))
  
  grafo <- para_red[, c("name", "destinatario", "pases")]
  
  names(grafo)<- c('to','from','pases')
  
  
  grafo<-filter(grafo,pases>=1)
  
  #creamos el grafo que permite sacar metricas
  Colleague_Graph_kedges<-igraph::graph.data.frame(grafo, directed=TRUE)
  
  #simplificamos los looping (es raro que haya pero alguno he visto) y las multiplicidades
  Colleague_Graph_kedges_d<-igraph::simplify(Colleague_Graph_kedges, remove.multiple=TRUE, remove.loops=TRUE)
  

  media<-igraph::mean_distance(Colleague_Graph_kedges_d,directed=FALSE,unconnected=TRUE)
  grado=degree(Colleague_Graph_kedges_d, mode="all")
  centralidad<-closeness(Colleague_Graph_kedges_d, mode="all")
  eigenvector<-centr_eigen(Colleague_Graph_kedges_d, directed=T, normalized=T)$vector
  hs <- hub_score(Colleague_Graph_kedges_d, weights=NA)$vector
  as <- authority_score(Colleague_Graph_kedges_d, weights=NA)$vector
  
  #Calculamos la centralidad de cada jugador
  bet<-igraph::betweenness(Colleague_Graph_kedges_d,directed = TRUE,normalized = TRUE)
  bet2 <- as.data.frame(bet)
  bet3 <- cbind(Label = rownames(bet2), bet2)
  bet3[,'Label'] <- as.character(bet3[,'Label'])
  
  #unimos a los nodos el valor de la centralidad
  knodes$total<-as.numeric(knodes$total)
  knodes10 <- knodes %>%
    ungroup() %>%
    mutate(size = scales::rescale(total, c(1,5), c(min(total), max(total)))) %>%
    left_join(bet3, by = c("name"="Label"))
  
  curva<- para_red %>%
    group_by(name,destinatario) %>%
    summarise(avg_x=mean(x),avg_y=mean(y),to=sum(as.integer(pases)))
  
  curva<-curva %>%
    select(name,avg_x,avg_y,destinatario,to) %>%
    filter(to>=2) %>%
    inner_join(knodes,by=c('destinatario'='name'))  %>%
    mutate(size2 = scales::rescale(to, c(0.1, 2), c(min(to), max(to))))
  
  
  curva <- curva %>%
    segmentsDf(2,2, 1)
  
  arrow <- arrow(type = "closed", angle = 30, length = unit(0.1, "inches"))
  
  
  h <- OptaMAPcampofutbol()
  p <- h +
    geom_polygon(hull_coor, mapping=aes(x= x*106, y=y*70.4), alpha = 0.4,linetype = "dashed",size = 1,show.legend = FALSE) +
    geom_point(data = centro_pol,aes(x = xm*106,y =ym*70.4),shape = 21, colour = "black", size = 3, stroke = 5) +
    geom_segment(data = curva, aes(x = x2*106, y =(y2*70.4), xend = xend*106, yend =( yend*70.4), size = size2,color=to), arrow = arrow, alpha = 1) +
    
    
    scale_color_gradient(low = "white", high = "red")+
    geom_point(data = knodes10,aes(x = x*106,y = (y*70.4),size = size,fill=bet),color='black',shape=21,stroke = 1) +
    
    ggrepel::geom_label_repel(
      data = knodes10,
      aes(x = x*106,y = (y*70.4),label=name),
      nudge_y = 400,
      size=3) +
    
    geom_segment(aes(x = x_min_prof, y = 6900, xend = x_max_prof, yend = 6900),colour = "#000000", size = 1) +
    geom_segment(aes(x = x_min_prof, y = 7025, xend = x_min_prof, yend = 6775),colour = "#000000", size = 1) +
    geom_segment(aes(x = x_max_prof, y = 7025, xend = x_max_prof, yend = 6775),colour = "#000000", size = 1) +
    geom_text(aes(x=(x_max_prof-x_min_prof)/2+x_min_prof,y=6725,label=format((x_max_prof-x_min_prof)*(100/106)/100,digits=2,nsmall=2)), colour="black") +
    
    
    geom_segment(aes(x = 9650, y = y_min_ampl, xend = 9650, yend = y_max_ampl),colour = "#000000", size = 1) +
    geom_segment(aes(x = 9525, y = y_min_ampl, xend = 9725, yend = y_min_ampl),colour = "#000000", size = 1) +
    geom_segment(aes(x = 9525, y = y_max_ampl, xend = 9725, yend = y_max_ampl),colour = "#000000", size = 1) +
    geom_text(aes(x=9450,y=(y_min_ampl-y_max_ampl)/2+y_max_ampl,label=format((y_max_ampl-y_min_ampl)*(100/70.4)/100,digits=2,nsmall=2)), colour="black") +
    
    scale_size_identity() +
    
    # annotate("text", label = paste("Pases Totales en Matriz: ",pasestotales), x = 100, y = 200, size = 5, colour = "black",hjust = 0)+
    theme(legend.position="none")
  
  return(p)
  
}



### RENDERS
renderMarcador <- function(datos, partido, equipo) {
  renderUI({
    texto <- unique(datos$resultado[datos$partido == partido])
    texto <- unlist(str_split(texto, " - "))[equipo]
    HTML(paste0("<span style='font-size:150px; font-weight:bold;'>", texto, "</span>"))
  })
}

renderGoleadores <- function(datos, partidin, equipo) {
  renderUI({
    goles <- datos %>% filter(partido == partidin, (type.displayName == "Goal" & team == equipo) | (type.displayName == "OwnGoal" & team != equipo)) %>% mutate(name = ifelse(type.displayName == "OwnGoal", paste(name, "(OG)"), name)) %>% select(name, minute)
    texto_html <- ""
    if (nrow(goles) > 0) {
      for (scorer in 1:nrow(goles)) {
        gol <- paste0("<span style='font-size:15px;'>", goles$name[scorer], " ", goles$minute[scorer], "'", "</span> <br>")
        texto_html <- str_c(texto_html, gol, sep = " ")
      }
    }
    HTML(texto_html)
  })
}

##### FASE OFENSIVA
#####
renderHeatmap <- function(datos, partidin, fasedj, equipo) {
  renderPlot({
    datos <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful")
    ggplot(datos, aes(x, y)) +
      geom_density_2d_filled(alpha = 1, aes(fill = as.numeric(stat(level)))) +
      geom_point(alpha = .6) +
      scale_fill_gradient2(low = "#3ab54a", mid = "yellow", high = "red", midpoint = 7.5) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) + direction_label()
  })
}

renderShotmap <- function(datos, partidin, fasedj, equipo) {
  renderPlot({
    # Filtrar los datos y ajustar las coordenadas finales de los segmentos
    datos <- datos %>%
      filter(partido == partidin, team == equipo, str_detect(fasedejuego, fasedj), isShot == TRUE) %>%
      mutate(
        xend = ifelse(!is.na(blockedX), blockedX, 100),
        yend = ifelse(!is.na(blockedY), blockedY, goalMouthY),
        goal = ifelse(type.displayName == "Goal", "goal", "no_goal"),  # Marcar goles
        id = row_number()  # Identificar cada fila
      )
    
    # Crear el gráfico con ggplot
    ggplot(data = datos) +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend, id = id), 
                   arrow = arrow(length = unit(0.2, "cm"), type = "open"), 
                   show.legend = FALSE) +
      geom_point(aes(x = x, y = y, color = goal, text = name), size = 2) +
      scale_color_manual(values = c("goal" = "green", "no_goal" = "red")) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") +
      ggrepel::geom_label_repel(
        data = datos,
        aes(x = x ,y = y, label=name), alpha = 0.5,
        size=3) +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
    
  })
}

renderSeq <- function(datos, partidin, fasedj, equipo) {
  renderUI({
    df <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful")
    
    df <- combinaciones_comunes(df, 3)
    
    html_list <- list()
    
    for (i in 1:nrow(df)) {
      # Obtener la secuencia y el número de repeticiones
      sequence <- df$sequence[[i]]
      count <- df$n[i]
      
      # Obtener los nombres de los jugadores de la secuencia
      jugadores_seq <- strsplit(sequence, " - ")[[1]]
      
      image_list <- list()
      
      for (j in 1:length(jugadores_seq)) {
        # Obtener la URL de la foto del jugador
        foto_url <- jugadores %>% filter(name == jugadores_seq[j], team == equipo) %>% select(foto) %>% pull()
        # Si la foto no existe, usar un marcador de posición
        if (is.na(foto_url) || length(foto_url) == 0) {
          foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
        }
        img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
        name_tag <- tags$p(jugadores_seq[j], style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
        player_div <- tags$div(class = "player", img_tag, name_tag)
        image_list <- append(image_list, list(player_div))
        
        if (j < length(jugadores_seq)) {
          arrow_div <- tags$div(class = "arrow", style = "font-size: 23px; font-weight: bold; margin-bottom: 15px;", "→")
          image_list <- append(image_list, list(arrow_div))
        }
      
        row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                             do.call(tagList, image_list), 
                             tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(count)))
        
        # Agregar la fila al html_list
        html_list[[i]] <- row_html
      
      }
    }
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderRedPases <- function(datos, partidos, fasedj, equipo, minutos = 0:130){
  renderPlot({
    red_pases(datos, partidos, fasedj, equipo, minutos = 0:130)
  })
}

renderCentros <- function(datos, partidin, fasedj, equipo){
  datos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo)
  
  centros <- sacar_centros(datos)
  
  centros <- centros %>%
    mutate(franja = case_when(
      x >= 50 & x <= 85 & y <= 25 ~ "Far Right",
      x > 85 & y <= 25 ~ "Deep Right",
      x >= 50 & x <= 85 & y >= 75 ~ "Far Left",
      x > 85 & y >= 75 ~ "Deep Left"
    ))
  
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligoneiros <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(50, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligoneiros, aes(x = x, y = y, group = franja, fill = num_centros), alpha = 0.5, color = "black") +
      
      # Ajustar colores
      scale_fill_gradient2(low = "#3ab54a", high = "red", mid = "yellow", midpoint = max(poligoneiros$num_centros)/2) +
      
      # Añadir los centros
      geom_point(data = centros, aes(x = x, y = y), color = "black", size = 3) +
      geom_segment(data = centros, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      
      # Añadir etiquetas de porcentaje y número de centros
      geom_text(data = frecuencias, aes(x = c(67.5, 92.5, 67.5, 92.5), y = c(12.5, 12.5, 87.5, 87.5), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}


renderPasesMedios <- function(datos, partidin, fasedj, equipo){
  renderValueBox({
  datos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo)
  mediapases <- datos %>% filter(fasedejuego == "Construccion Ofensiva") %>% group_by(posesion) %>% summarise(pases = n()-1) %>% filter(pases > 1)
  media <- mean(mediapases$pases)
  
  valueBox(
    value = formatC(media, digits = 1, format = "f"),
    subtitle = "Passes per possession (mean)",
    icon = icon("arrows-spin"),
    color = "green"
  )
})
}
renderVerticalidad <- function(datos, partidin, fasedj, equipo){
  renderValueBox({
    datos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo)
    verticalidad <- datos %>% group_by(posesion) %>% filter(n() > 2) %>% summarise(vert = (last(x)-first(x))/(n()-1)) 
    vert <- mean(verticalidad$vert)*1.05
    
    valueBox(
      value = formatC(vert, digits = 1, format = "f"),
      subtitle = "Metres won per pass (mean)",
      icon = icon("angles-right"),
      color = "red"
    )
  })
}

renderFinalizadores <- function(datos, partidin, fasedj, equipo) {
  renderUI({
    tiros <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj, (str_detect(type.displayName, "Shot") | type.displayName == "Goal"))
    
    tiros <- tiros %>% group_by(name) %>% summarise(tiros = n()) %>% arrange(desc(tiros))
    
    tiros <- tiros[1:min(3, nrow(tiros)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(tiros)) {
      # Obtener la secuencia y el número de repeticiones
      player <- tiros$name[i]
      shots <- tiros$tiros[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(shots)))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
  
  
  # Devolver la lista de filas de HTML
  do.call(tagList, html_list)
  })
}

renderEnvios <- function(datos, partidin, fasedj, equipo, linea, ipt, l_v){
  renderPlot({
      
    if (linea == "Saques portero"){
      datos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo)
      envios <- datos %>% filter(type.displayName == "Pass", x <= 5, between(y, 36, 63))
    }
    
    if (linea == "Defensiva"){
      datos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo)
      datos <- datos %>% filter(type.displayName == "Pass", ((x < 5 & (y > 75 | y < 25)) | (between(x, 5.1, 35))))
     
      envios <- datos %>% filter(name %in% ipt[[paste0("jugadores_linea_def_", l_v)]])
    }
    
    envios$cuadrante_x <- cut(envios$endX, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    envios$cuadrante_y <- cut(envios$endY, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    # Contar el número de robos en cada cuadrante
    conteo_envios <- envios %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_envios = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_envios, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
      
    
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_envios), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_envios), color = "black", size = 5, na.rm = TRUE) +
      geom_segment(data = envios, aes(x = x, y = y, xend = endX, yend = endY, color = outcomeType.displayName),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      scale_color_manual(values = c("Successful" = "green", "Unsuccessful" = "red")) +
      
      
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_envios))/2) +
      annotate_pitch(colour = "white",
                     fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}

##### TRANS DEF-AT
#####
es_recuperacion_rapida <- function(datos, tiempo_recuperacion, equipo, pos) {
  # Filtrar eventos del mismo equipo en los 10 segundos anteriores
  eventos_anteriores <- datos %>% 
    filter(team == equipo, 
           posesion != pos, 
           tiempo >= (tiempo_recuperacion - 10),
           tiempo < tiempo_recuperacion,
           type.displayName %in% c("Pass"))
  
  # Retornar TRUE si hay al menos una posesión en ese intervalo
  return(nrow(eventos_anteriores) > 0)
}

renderRecuperaciones <- function(datos, partidin, fasedj, equipo){
  renderPlot({
    robos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo) %>% filter(type.displayName %in% c("BallRecovery", "Interception")) 
    
    robos$cuadrante_x <- cut(robos$x, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    robos$cuadrante_y <- cut(robos$y, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    c_datos <- datos %>% filter(partido == partidin, team == equipo)
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    robos_rapidos <- robos %>% rowwise() %>% mutate(rapida = es_recuperacion_rapida(c_datos, minute*60+second, team, posesion))
    robos_rapidos <- robos_rapidos %>% filter(rapida, type.displayName == "BallRecovery")
    
    # Contar el número de robos en cada cuadrante
    conteo_robos <- robos %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_robos = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_robos, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_robos), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_robos), color = "black", size = 5, na.rm = TRUE) +
      geom_point(data = robos_rapidos, aes(x = x, y = y), color = "cyan", size = 3) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_robos))/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}
renderRecuperadores <- function(datos, partidin, fasedj, equipo) {
  renderUI({
    robos <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj, type.displayName %in% c("BallRecovery", "Interception"))
    
    robos <- robos %>% group_by(name) %>% summarise(robos = n()) %>% arrange(desc(robos))
    
    robos <- robos[1:min(3, nrow(robos)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(robos)) {
      # Obtener la secuencia y el número de repeticiones
      player <- robos$name[i]
      recoveries <- robos$robos[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(recoveries)))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderFlechas <- function(datos, partidin, fasedj, equipo){
  renderPlot({
    contras <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj) %>% filter(type.displayName=="Pass")
    
    
    aux <- contras %>% group_by(posesion) %>%
      summarise(
        pen_x = nth(x, -2),
        pen_endX = nth(endX, -2),
        pen_endY = nth(endY, -2),
        ult_x = nth(x, -1),
        pen_y = nth(y, -2),
        ult_y = nth(y, -1)
      ) 
    
    carril <- aux %>% mutate(carril = ifelse(((ult_x > 60) & (pen_y < 35) & (ult_y < 35)) | ((pen_x > 60) & (pen_y < 35) & pen_endX > 82 & between(pen_endY, 20, 80)), "Derecho", 
                                             ifelse(((ult_x > 60) & (pen_y > 65) & (ult_y > 65)) | ((pen_x > 60) & (pen_y > 65) & pen_endX > 82 & between(pen_endY, 20, 80)), "Izquierdo", 
                                                    ifelse((ult_x > 60) & between(pen_y, 35, 65) & between(ult_y, 35, 65), "Centro", "Mixto"))))
    
    carr <- carril %>% select(posesion, carril)
    
    contras <- contras %>% left_join(carr, by = "posesion") %>% filter(carril %in% c("Izquierdo", "Centro", "Derecho"))
    
    # Contar el número de acciones en cada franja
    conteo_acciones <- contras %>%
      group_by(carril) %>%
      summarise(n_acciones = n_distinct(posesion))
    
    # Crear un dataframe para las flechas y textos
    flechas <- data.frame(
      carril = c("Derecho", "Centro", "Izquierdo"),
      x_start = 20,
      y_start = c(16.665, 50, 83.335),
      x_end = 80,
      y_end = c(16.665, 50, 83.335),
      label_x = 85,
      label_y = c(16.665, 50, 83.335)
    ) %>%
      left_join(conteo_acciones, by = "carril") %>% mutate_all(~replace(., is.na(.), 0))
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_segment(data = flechas, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = n_acciones),
                   arrow = arrow(length = unit(0.3, "inches")), size = 2) +
      geom_text(data = flechas, aes(x = label_x, y = label_y, label = n_acciones, color = n_acciones), size = 5, fontface = "bold") +
      scale_color_gradient2(low = "yellow2", high = "red", mid = "orange3", midpoint = max(flechas$n_acciones)/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
}


renderPorcVerticales <- function(datos, partidin, fasedj, equipo){
  renderValueBox({
    transiciones <- datos %>% filter(partido == partidin, team == equipo, str_detect(fasedejuego, fasedj)) %>% group_by(posesion) %>% filter(n()>2)
    n_pos <- length(unique(transiciones$posesion))
    
    porc_vert <- transiciones %>% group_by(fasedejuego) %>% summarise(veces = n_distinct(posesion)) %>% mutate(porc_fase = round(veces/n_pos, 2)*100)
    porc_vert <- porc_vert$porc_fase[porc_vert$fasedejuego == "Transicion DEF-AT Vertical"]
    
    
    valueBox(
      value = formatC(porc_vert, digits = 1, format = "f"),
      subtitle = "% Of transitions being VERTICAL",
      icon = icon("angles-right"),
      color = "blue"
    )
  })
}

renderPorcFinalizadas <- function(datos, partidin, fasedj, equipo){
  renderValueBox({
    transiciones <- datos %>% filter(partido == partidin, team == equipo, str_detect(fasedejuego, fasedj)) %>% group_by(posesion) %>% filter(n()>2)

    porc_final <- transiciones %>% group_by(fasedejuego, posesion) %>% 
      summarise(finalizadas = any(str_detect(type.displayName, "Shot") | type.displayName == "Goal")) %>% 
      group_by(fasedejuego) %>% summarise(porc_fin = round(sum(finalizadas)/n_distinct(posesion), 3)*100)
    
    porc_final <- porc_final$porc_fin[porc_final$fasedejuego == "Transicion DEF-AT Vertical"]
    
    valueBox(
      value = formatC(porc_final, digits = 1, format = "f"),
      subtitle = "% Of VERTICAL transitions ended in Shot",
      icon = icon("arrow-right-to-bracket"),
      color = "blue"
    )
  })
}

renderAsistentesTiro <- function(datos, partidin, fasedj, equipo) {
  renderUI({
    transiciones <- datos %>% filter(partido == partidin, team == equipo, str_detect(fasedejuego, fasedj)) %>% group_by(posesion) %>% filter(n()>2)
    asistentes_tiro <- transiciones %>% filter(str_detect(type.displayName, "Shot")) %>% select(relatedEventId)    
    name_asis_tiro <- transiciones[transiciones$eventId %in% asistentes_tiro$relatedEventId, "name"]
    
    
    asistentes <- name_asis_tiro %>% group_by(name) %>% summarise(pases = n()) %>% arrange(desc(pases))
    
    
    html_list <- list()
    
    if (nrow(asistentes) > 0) {
      for (i in 1:nrow(asistentes)) {
      # Obtener la secuencia y el número de repeticiones
      player <- asistentes$name[i]
      assists <- asistentes$pases[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(assists)))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderAsistentesGol <- function(datos, partidin, fasedj, equipo) {
  renderUI({
    transiciones <- datos %>% filter(partido == partidin, team == equipo, str_detect(fasedejuego, fasedj)) %>% group_by(posesion) %>% filter(n()>2)
    asistentes_gol <- transiciones %>% filter(str_detect(type.displayName, "Goal")) %>% select(relatedEventId)    
    name_asis_gol <- transiciones[transiciones$eventId %in% asistentes_gol$relatedEventId, "name"]
    
    
    asistentes <- name_asis_gol %>% group_by(name) %>% summarise(pases = n()) %>% arrange(desc(pases))
    
    
    html_list <- list()
    
    if (nrow(asistentes > 0)){
      for (i in 1:nrow(asistentes)) {
      # Obtener la secuencia y el número de repeticiones
      player <- asistentes$name[i]
      assists <- asistentes$pases[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(assists)))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderContrasVerticales <- function(datos, partidin, equipo){
  
  renderPlot({
    verticales <- datos %>% filter(partido == partidin, str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == equipo)
    
    lineas_discontinuas <- verticales %>% group_by(posesion) %>% mutate(next_x = lead(x), next_y = lead(y)) %>% select(endX, endY, next_x, next_y, posesion)
    lineas_discontinuas$posesion <- as.factor(lineas_discontinuas$posesion)
    
    shots <- verticales %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal")
    shots <- shots %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    verticales$posesion <- as.factor(verticales$posesion)
    posesion_colors <- viridis::viridis_pal(option = "D")(length(unique(verticales$posesion)))
    
    ggplot() +
      geom_segment(data = verticales, aes(x = x, y = y, xend = endX, yend = endY, color = posesion),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      geom_segment(data = lineas_discontinuas, aes(x = endX, y = endY, xend = next_x, yend = next_y, color = posesion), linetype = "dashed") +
      
      scale_color_manual(values = posesion_colors) +
      
      geom_point(data = shots, aes(x = x, y = y, fill = esgol), shape = 21, size = 3, color = "black") + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      
      ggrepel::geom_label_repel(
        data = shots,
        aes(x = x, y = y, label=name), alpha = 0.5,
        size=3) +
      
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))

  })
  
}
  

#####
##### FASE DEF
#####
renderDefHeatmap <- function(datos, partidin, fasedj, equipo) {
  renderPlot({
    datos <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful", x > 50)
    
    datos <- datos %>% mutate(x = 100-x, y = 100-y)
    
    ggplot(datos, aes(x, y)) +
      coord_cartesian(xlim = c(0, 50)) +
      geom_density_2d_filled(alpha = 1, aes(fill = as.numeric(stat(level)))) +
      geom_point(alpha = .6) +
      geom_vline(aes(xintercept = quantile(x, .25))) +
      scale_fill_gradient2(low = "#3ab54a", mid = "yellow", high = "red", midpoint = 7.5) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) + direction_label()
  })
}

renderDefShotmap <- function(datos, partidin, fasedj, equipo) {
  renderPlot({
    # Filtrar los datos y ajustar las coordenadas finales de los segmentos
    datos <- datos %>%
      filter(partido == partidin, team == equipo, str_detect(fasedejuego, fasedj), isShot == TRUE) %>%
      mutate(
        xend = ifelse(!is.na(blockedX), 100-blockedX, 0),
        yend = ifelse(!is.na(blockedY), 100-blockedY, 100-goalMouthY),
        goal = ifelse(type.displayName == "Goal", "goal", "no_goal"),  # Marcar goles
        id = row_number()  # Identificar cada fila
      ) %>% 
      mutate(x = 100-x, y = 100-y)
    
    # Crear el gráfico con ggplot
    ggplot(data = datos) +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend, id = id), 
                   arrow = arrow(length = unit(0.2, "cm"), type = "open"), 
                   show.legend = FALSE) +
      geom_point(aes(x = x, y = y, color = goal, text = name), size = 2) +
      scale_color_manual(values = c("goal" = "green", "no_goal" = "red")) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
    
  })
}

renderDefCentros <- function(datos, partidin, fasedj, equipo){
  datos <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo)
  
  centros <- sacar_centros(datos)

  centros <- centros %>% mutate(x = 100-x, y = 100-y, endX = 100-endX, endY = 100-endY)
  centros <- centros %>%
    mutate(franja = case_when(
      x <= 50 & x >= 15 & y >= 75 ~ "Far Right",
      x < 15 & y >= 75 ~ "Deep Right",
      x <= 50 & x >= 15 & y <= 25 ~ "Far Left",
      x < 15 & y <= 25 ~ "Deep Left"
    ))
  
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligoneiros <- poligonosDef %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 50)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligoneiros, aes(x = x, y = y, group = franja, fill = num_centros), alpha = 0.5, color = "black") +
      
      # Ajustar colores
      scale_fill_gradient2(low = "#3ab54a", high = "red", mid = "yellow", midpoint = max(poligoneiros$num_centros)/2) +
      
      # Añadir los centros
      geom_point(data = centros, aes(x = x, y = y), color = "black", size = 3) +
      geom_segment(data = centros, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      
      # Añadir etiquetas de porcentaje y número de centros
      geom_text(data = frecuencias, aes(x = c(32.5, 7.5, 32.5, 7.5), y = c(87.5, 87.5, 12.5, 12.5), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}

renderPPDA <- function(datos, partidin, equipo_df, equipo_at){
  datos <- datos %>% filter(partido == partidin)
  
  
  equipo_def <- datos %>% group_by(posesion) %>% filter(any(x < 67))
  
  pases_at <- equipo_def %>% filter(type.displayName %in% c("Pass", "BallTouch", "Aerial", "Interception", "TakeOn", "MissedShots", "SavedShot", "Goal", "ShotOnPost"), outcomeType.displayName == "Successful", team == equipo_at)
  
  acciones_def <- which(
      (datos$type.displayName == "Foul" & datos$outcomeType.displayName == "Unsuccessful" & datos$team == equipo_df & datos$x>33) | 
      (datos$type.displayName == "Dispossessed" & datos$outcomeType.displayName == "Successful" & datos$team == equipo_at & datos$x>33) | 
      (datos$type.displayName == "Aerial" & datos$outcomeType.displayName == "Successful" & datos$team == equipo_df & lag(datos$team) == equipo_at & datos$x>33) |
      (datos$type.displayName == "BlockedPass" & datos$outcomeType.displayName == "Successful" & datos$team == equipo_df & datos$x>33) |
      (datos$type.displayName == "Tackle" & datos$outcomeType.displayName == "Successful" & datos$team == equipo_df & datos$x>33) |
      (datos$type.displayName == "Interception" & datos$outcomeType.displayName == "Successful" & datos$team == equipo_df & datos$x>33)
    
  )
  
  ppda <- nrow(pases_at)/length(acciones_def)
  
  renderValueBox({
    valueBox(
      value = formatC(ppda, digits = 1, format = "f"),
      subtitle = "PPDA",
      icon = icon("shield"),
      color = "purple"
    )
  })
}

renderDefFlechas <- function(datos, partidin, fasedj, equipo){
  renderPlot({
    contras <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == fasedj) %>% filter(type.displayName=="Pass")
    
    
    aux <- contras %>% group_by(posesion) %>%
      summarise(
        pen_x = nth(x, -2),
        pen_endX = nth(endX, -2),
        pen_endY = nth(endY, -2),
        ult_x = nth(x, -1),
        pen_y = nth(y, -2),
        ult_y = nth(y, -1)
      ) 
    
    carril <- aux %>% mutate(carril = ifelse(((ult_x > 60) & (pen_y < 35) & (ult_y < 35)) | ((pen_x > 60) & (pen_y < 35) & pen_endX > 82 & between(pen_endY, 20, 80)), "Derecho", 
                                             ifelse(((ult_x > 60) & (pen_y > 65) & (ult_y > 65)) | ((pen_x > 60) & (pen_y > 65) & pen_endX > 82 & between(pen_endY, 20, 80)), "Izquierdo", 
                                                    ifelse((ult_x > 60) & between(pen_y, 35, 65) & between(ult_y, 35, 65), "Centro", "Mixto"))))
    
    carr <- carril %>% select(posesion, carril)
    
    contras <- contras %>% left_join(carr, by = "posesion") %>% filter(carril %in% c("Izquierdo", "Centro", "Derecho"))
    
    # Contar el número de acciones en cada franja
    conteo_acciones <- contras %>%
      group_by(carril) %>%
      summarise(n_acciones = n_distinct(posesion))
    
    # Crear un dataframe para las flechas y textos
    flechas <- data.frame(
      carril = c("Derecho", "Centro", "Izquierdo"),
      x_start = 80,
      y_start = c(83.335, 50, 16.665),
      x_end = 20,
      y_end = c(83.335, 50, 16.665),
      label_x = 85,
      label_y = c(83.335, 50, 16.665)
    ) %>%
      left_join(conteo_acciones, by = "carril") %>% mutate_all(~replace(., is.na(.), 0))
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_segment(data = flechas, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = n_acciones),
                   arrow = arrow(length = unit(0.3, "inches")), size = 2) +
      geom_text(data = flechas, aes(x = label_x, y = label_y, label = n_acciones, color = n_acciones), size = 5, fontface = "bold") +
      scale_color_gradient2(low = "yellow2", high = "red", mid = "orange3", midpoint = max(flechas$n_acciones)/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
}

renderDefContrasVerticales <- function(datos, partidin, equipo){
  
  renderPlot({
    verticales <- datos %>% filter(partido == partidin, str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == equipo)
    verticales <- verticales %>% mutate(x = 100-x, y = 100-y, endX = ifelse(is.na(endX), endX, 100-endX), endY = ifelse(is.na(endY), endY, 100-endY))
    
    
    lineas_discontinuas <- verticales %>% group_by(posesion) %>% mutate(next_x = lead(x), next_y = lead(y)) %>% select(endX, endY, next_x, next_y, posesion)
    lineas_discontinuas$posesion <- as.factor(lineas_discontinuas$posesion)
    
    
    shots <- verticales %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal")
    
    shots <- shots %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    verticales$posesion <- as.factor(verticales$posesion)
    posesion_colors <- viridis::viridis_pal(option = "D")(length(unique(verticales$posesion)))
    
    ggplot() +
      geom_segment(data = verticales, aes(x = x, y = y, xend = endX, yend = endY, color = posesion),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      geom_segment(data = lineas_discontinuas, aes(x = endX, y = endY, xend = next_x, yend = next_y, color = posesion), linetype = "dashed") +
      
      scale_color_manual(values = posesion_colors) +
      
      geom_point(data = shots, aes(x = x, y = y, fill = esgol), shape = 21, size = 3, color = "black") + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      
      ggrepel::geom_label_repel(
        data = shots,
        aes(x = x, y = y, label=name), alpha = 0.5,
        size=3) +
      
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
  })
  
}

renderDefFinalizadas3cuartos <- function(datos, partidin, equipo_at){
  datos <- datos %>% filter(partido == partidin)
  
  
  equipo_ata <- datos %>% group_by(posesion) %>% filter(any(x >= 75))
  
  posesiones_tiro <- equipo_ata %>% filter(team == equipo_at) %>% group_by(posesion) %>% summarise(fin = (any(str_detect(type.displayName, "Shot")) | type.displayName == "Goal"))
  
  porc_finalizadas <- (sum(posesiones_tiro$fin)/nrow(posesiones_tiro))*100
  
  renderValueBox({
    valueBox(
      value = formatC(porc_finalizadas, digits = 1, format = "f"),
      subtitle = "% of Attacks in 3/4 ended in Shot",
      icon = icon("shield"),
      color = "purple"
    )
  })
  
}
renderDefCortadas3cuartos <- function(datos, partidin, equipo_df, equipo_at){
  datos <- datos %>% filter(partido == partidin)
  
  
  equipo_ata <- datos %>% group_by(posesion) %>% filter(any(x >= 75))
  
  posesiones_tiro <- equipo_ata %>% filter(team == equipo_at) %>% group_by(posesion) %>% summarise(fin = (any(str_detect(type.displayName, "Shot")) | type.displayName == "Goal"))
  
  
  acciones_def <- which(
    (equipo_ata$type.displayName == "Foul" & equipo_ata$outcomeType.displayName == "Unsuccessful" & equipo_ata$team == equipo_df) | 
      (equipo_ata$type.displayName == "Dispossessed" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo_at) | 
      (equipo_ata$type.displayName == "Aerial" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo_df & lag(equipo_ata$team) == equipo_at) |
      (equipo_ata$type.displayName == "BlockedPass" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo_df) |
      (equipo_ata$type.displayName == "Tackle" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo_df) |
      (equipo_ata$type.displayName == "Clearance" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo_df) |
      (equipo_ata$type.displayName == "Interception" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo_df)
  )
  
  porc_cortadas <- (length(acciones_def)/nrow(posesiones_tiro))*100
  renderValueBox({
    valueBox(
      value = formatC(porc_cortadas, digits = 1, format = "f"),
      subtitle = "% of Attacks in 3/4 Solved by the Defense",
      icon = icon("shield"),
      color = "purple"
    )
  })
}


renderJugadoresProtas <- function(datos, partidin, fasedj, equipo, primero_segundo){
  renderUI({
    
    acciones <- datos %>% filter(partido == partidin, (fasedejuego == fasedj) | (lag(fasedejuego) == fasedj) | (lag(fasedejuego, 2) == fasedj), team == equipo, type.displayName %in% c("BallRecovery", "Aerial", "Clearance", "BlockedPass", "Interception", "Claim"), x < 40)
    

    tabla_porc <- acciones %>% group_by(name, type.displayName) %>% summarise(exito = sum(outcomeType.displayName == "Successful"),
                                                                              fracaso = sum(outcomeType.displayName == "Unsuccessful"),
                                                                              total = n(),
                                                                              porc_exito = round(exito / (total) * 100, 2)) %>% ungroup()

    # acciones exitosas
    por_jug <- tabla_porc %>% group_by(name) %>% summarise(acciones = sum(exito)) %>% arrange(desc(acciones))
    

    
    prueba <- tabla_porc[tabla_porc$name == por_jug$name[primero_segundo], ]

    nombre_tio <- prueba$name[1]

    foto_url <- jugadores %>% filter(name == nombre_tio, team == equipo) %>% select(foto) %>% pull()

    if (is.na(foto_url) || length(foto_url) == 0) {
      foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
    }
    
    
    resultados <- vector(mode = "character", length = nrow(prueba))
    for (i in 1:nrow(prueba)) {
      if (prueba$type.displayName[i] == "Aerial") {
        resultados[i] <- paste(prueba$exito[i], "/", prueba$total[i], " (", prueba$porc_exito[i], "%)", sep = "")
      } else {
        resultados[i] <- prueba$total[i]
      }
    }
    
    categorias <- paste0(prueba$type.displayName, collapse = " & ")
    resultados <- paste0(resultados, collapse = " & ")
    
    
    # Crear la tabla personalizada
    tabla_html <- htmltools::HTML(
      paste0(
        '<table class="table table-striped" style="width: 100%; border: 3px solid black;">',
        '<thead>',
        '<tr>',
        '<th colspan="5" style="text-align:center; border-bottom: 3px solid black; font-size: 20px; font-family: Arial, sans-serif;">',
        '<div style="display: flex; align-items: center; justify-content: center; margin-bottom: 10px; margin-top: 10px;">',
        '<img src="', foto_url, '" style="width:100px;height:100px; vertical-align: middle; margin-right: 40px;">',
        nombre_tio,
        '</th>',
        '</tr>',
        '</thead>',
        '<tbody>',
        '<tr>',
        '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias), '</td>',
        '</tr>',
        '<tr>',
        '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados), '</td>',
        '</tr>',
        '</tbody>',
        '</table>'
      )
    )
    
    htmltools::HTML(tabla_html)
  })
  
}


##### TRANS AT-DEF
#####
es_perdida_rapida <- function(datos, tiempo_recuperacion, equipo, pos) {
  
  eventos_anteriores <- datos %>% 
    filter(team != equipo, 
           posesion != pos, 
           tiempo >= (tiempo_recuperacion - 10),
           tiempo < tiempo_recuperacion,
           type.displayName %in% c("Pass"))
  
  
  return(nrow(eventos_anteriores) > 0)
}

renderPerdidas <- function(datos, partidin, fasedj, equipo){
  renderPlot({
    perdidas <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo) %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                                    (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                                                                    (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                                                                    (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")
                                                                                                    ) 
    
    perdidas$cuadrante_x <- cut(perdidas$x, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    perdidas$cuadrante_y <- cut(perdidas$y, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    c_datos <- datos %>% filter(partido == partidin, team != equipo)
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    perdidas_rapidas <- perdidas %>% rowwise() %>% mutate(rapida = es_perdida_rapida(c_datos, minute*60+second, team, posesion))
    perdidas_rapidas <- perdidas_rapidas %>% filter(rapida, ((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                      (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                      (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                      (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")))
    
    # Contar el número de perdidas en cada cuadrante
    conteo_perdidas <- perdidas %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_perdidas = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_perdidas, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_perdidas), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_perdidas), color = "black", size = 5, na.rm = TRUE) +
      geom_point(data = perdidas_rapidas, aes(x = x, y = y), color = "cyan", size = 3) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_perdidas))/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}

renderPerdedores <- function(datos, partidin, fasedj, equipo) {
  renderUI({
    perdidas <- datos %>% filter(partido == partidin, fasedejuego == fasedj, team == equipo) %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                                                                          (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")
                                                                                                        ) 
    
    perdidas <- perdidas %>% group_by(name) %>% summarise(perdidas = n()) %>% arrange(desc(perdidas))
    
    perdidas <- perdidas[1:min(3, nrow(perdidas)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(perdidas)) {
      # Obtener la secuencia y el número de repeticiones
      player <- perdidas$name[i]
      losses <- perdidas$perdidas[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(losses)))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}


##### ABP
#####

# - favor
renderCornersDer <- function(datos, partidin, equipo, ipt, l_v){
  
  renderPlot({
    corners_der <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP", x > 99, y < 1, name %in% ipt[[paste0("jugadores_cornerd_", l_v)]])
    
    cd <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP") %>% group_by(posesion) %>% filter(any(x > 99), any(y < 1), any(name %in% ipt[[paste0("jugadores_cornerd_", l_v)]]), type.displayName != "Aerial")
    
    disparos <- cd %>% mutate(pase_x = lag(x), pase_y = lag(y)) %>% filter((str_detect(type.displayName, "Shot") | type.displayName == "Goal"), between(x, 85, 99), between(y, 25, 75))
    disparos <- disparos %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    pases_zona <- disparos %>% filter(pase_x < 99, pase_y > 1)
    
    corners_der <- corners_der %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "1st Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "2nd Post",
        endY < 15 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_der %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 0, 0, 15, 15),
      palo = rep(c("1st Post", "Center", "2nd Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("1st Post", "Center", "2nd Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    
    ggplot() +
    annotate_pitch(colour = "white", fill = "#3ab54a") +
    theme_pitch() +
    coord_cartesian(xlim = c(75, 100)) +
    
    # Añadir los polígonos de las franjas
    geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
    geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
    
    scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
    
    # Añadir los centros
    geom_point(data = corners_der, aes(x = x, y = y), color = "black", size = 3) +
    
    geom_point(data = disparos, aes(x = x, y = y, color = esgol), size = 3) +
    scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
    geom_segment(data = pases_zona, aes(x = pase_x, y = pase_y, xend = x, yend = y), linetype = "dashed") +
      
    # Añadir etiquetas de porcentaje y número de envios
    geom_text(data = frecuencias, aes(x = c(88.75, 88.75, 88.75, 88.75), y = c(35, 50, 65, 8), 
                                      label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
              color = "black", size = 3, fontface = "bold") +
    
    geom_text(data = frecuencias[1:3,], aes(x = c(96.25, 96.25, 96.25), y = c(35, 50, 65), 
                                            label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
              color = "black", size = 3, fontface = "bold") +
      
    ggrepel::geom_label_repel(
      data = corners_der,
      aes(x = x, y = y, label=name),
      size=3) +
      
    ggrepel::geom_label_repel(
      data = disparos,
      aes(x = x, y = y, label=name),
      size=3) +
      
    
    # Configurar el tema
    theme_pitch() +
    theme_void() + theme(legend.position = "none") + guides(fill = "none") +
    theme(panel.background = element_rect(fill = "#3ab54a")) 
  
  })
  
}

renderCornersIzq <- function(datos, partidin, equipo, ipt, l_v){

  renderPlot({
    corners_izq <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP", x > 99, y > 99, name %in% ipt[[paste0("jugadores_corneri_", l_v)]])
    
    ci <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP") %>% group_by(posesion) %>% filter(any(x > 99), any(y > 99), any(name %in% ipt[[paste0("jugadores_corneri_", l_v)]]), type.displayName != "Aerial")
    
    disparos <- ci %>% mutate(pase_x = lag(x), pase_y = lag(y)) %>% filter((str_detect(type.displayName, "Shot") | type.displayName == "Goal"), between(x, 85, 99), between(y, 25, 75))
    disparos <- disparos %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    pases_zona <- disparos %>% filter(pase_x < 99, pase_y < 99)
    
    corners_izq <- corners_izq %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "2nd Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "1st Post",
        endY > 85 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_izq %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 100, 100, 85, 85),
      palo = rep(c("2nd Post", "Center", "1st Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("2nd Post", "Center", "1st Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(75, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_izq, aes(x = x, y = y), color = "black", size = 3) +
      
      geom_point(data = disparos, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +

      geom_segment(data = pases_zona, aes(x = pase_x, y = pase_y, xend = x, yend = y), linetype = "dashed") +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(88.75, 88.75, 88.75, 88.75), y = c(65, 50, 35, 92), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(96.25, 96.25, 96.25), y = c(65, 50, 35), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      ggrepel::geom_label_repel(
        data = corners_izq,
        aes(x = x, y = y, label=name),
        size=3) +
      
      ggrepel::geom_label_repel(
        data = disparos,
        aes(x = x, y = y, label=name),
        size=3) +
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderFaltasCentro <- function(datos, partidin, equipo) {
  faltas <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP") %>% 
    group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  
  # Bandas y centros
  centros_seq <- entiro %>% filter(n()>1, type.displayName == "Pass") 
  
  centros <- entiro %>% filter(n()>1, type.displayName == "Pass") %>% summarise(x = first(x), y = first(y))
  remates <- entiro %>% filter(n()>1, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  
  remates <- remates %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  centros <- centros %>%
    mutate(franja = case_when(
      between(y, 0, 20) ~ "Right",
      between(y, 80, 100) ~ "Left",
      between(y, 20, 80) ~ "Center"
    ))
  
  # Añadir frecuencias al data frame original
  todas_franjas <- data.frame(
    franja = c("Right", "Left", "Center")
  )
  
  # Calcular frecuencias
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligonos <- data.frame(
    x = c(50, 100, 100, 50, 50, 100, 100, 50, 82, 50, 50, 82),
    y = c(0, 0, 20, 20, 80, 80, 100, 100, 20, 20, 80, 80),
    franja = rep(c("Right", "Left", "Center"), each = 4)
  )
  
  poligonos <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
    annotate_pitch(colour = "white", fill = "#3ab54a") +
    theme_pitch() +
    coord_cartesian(xlim = c(50, 100)) +
    
    # centros
    geom_polygon(data = poligonos, aes(x = x, y = y, group = franja, fill = rescale(num_centros)), alpha = 0.5, color = "black") +
    scale_fill_gradient2(high = "red3", low = "yellow3", mid = "orange3", midpoint = 0.5) +
    
    
    geom_point(data = centros_seq, aes(x = x, y = y), color = "black", size = 3) +
    
      
    geom_segment(data = centros_seq, aes(x = x, y = y, xend = endX, yend = endY),
                 arrow = arrow(length = unit(0.2, "cm"),
                               type = "open")) +
    # remates
    geom_point(data = remates, aes(x = x, y = y, color = esgol), size = 3) +
    
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
    
    geom_text(data = frecuencias, aes(x = c(75, 75, 65), y = c(10, 90, 50), 
                                      label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
              color = "black", size = 4, fontface = "bold") +
    
    ggrepel::geom_label_repel(
      data = centros_seq,
      aes(x = x, y = y, label=name),
      size=3) +
      
    ggrepel::geom_label_repel(
      data = remates,
      aes(x = x, y = y, label=name),
      size=3) +
      
    theme_pitch() +
    theme_void() + theme(legend.position = "none") + guides(fill = "none") +
    theme(panel.background = element_rect(fill = "#3ab54a")) 
  
  })
  
  
}

renderLibreDirecto <- function(datos, partidin, equipo) {
  faltas <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP") %>% 
    group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  libdir <- entiro %>% filter(n()==1, x < 82, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  libdir <- libdir %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  renderPlot({
    ggplot() +
    annotate_pitch(colour = "white", fill = "#3ab54a") +
    theme_pitch() +
    coord_cartesian(xlim = c(50, 100)) +
    
    geom_point(data = libdir, aes(x = x, y = y, color = esgol), size = 3) +
    scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
    
    geom_segment(data = libdir, aes(x = x, y = y, xend = ifelse(is.na(blockedX), 100, blockedX), yend = ifelse(is.na(blockedY), goalMouthY, blockedY)),
                 arrow = arrow(length = unit(0.2, "cm"),
                               type = "open")) +
    ggrepel::geom_label_repel(
      data = libdir,
      aes(x = x, y = y, label=name),
      size=3) +
    
    theme_pitch() +
    theme_void() + theme(legend.position = "none") + guides(fill = "none") +
    theme(panel.background = element_rect(fill = "#3ab54a")) 
  
  })
  
  
}

renderPenales <- function(datos, partidin, equipo) {
  penales <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP", x == 88.5, y == 50)
  penales <- penales %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  renderPlot({
    porteria <- data.frame(
      x = c(0, 0, 10, 10),
      y = c(0, 4, 4, 0)
    )
    
    ggplot() +
      # Dibujar la portería
      geom_path(data = porteria, aes(x = x, y = y), color = "black", size = 1) +
      # Añadir los tiros
      geom_point(data = penales, aes(x = 55-goalMouthY, y = goalMouthZ/10, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      ggrepel::geom_label_repel(
        data = penales,
        aes(x = 55-goalMouthY, y = goalMouthZ/10, label=name),
        size=3) +
      
      # Configurar los límites del gráfico
      coord_fixed(ratio = 1) +
      xlim(-1, 11) +
      ylim(0, 5) +
      
      theme_void() + theme(legend.position = "none") + guides(fill = "none") 
    
  })
}

# - contra
renderDefCornersDer <- function(datos, partidin, equipo){
  
  renderPlot({
    corners_der <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP", x > 99, y < 1)
    
    corners_der <- corners_der %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "1st Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "2nd Post",
        endY < 15 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_der %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 0, 0, 15, 15),
      palo = rep(c("1st Post", "Center", "2nd Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("1st Post", "Center", "2nd Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    
    corners_der <- corners_der %>% mutate(x = 100-x, y = 100-y)
    poligonos_envios <- poligonos_envios %>% mutate(x = 100-x, y = 100-y)
    poligonos_efectividad <- poligonos_efectividad %>% mutate(x = 100-x, y = 100-y)
    
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 25)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_der, aes(x = x, y = y), color = "black", size = 3) +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(11.25, 11.25, 11.25, 11.25), y = c(65, 50, 35, 92), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(3.75, 3.75, 3.75), y = c(65, 50, 35), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderDefCornersIzq <- function(datos, partidin, equipo){
  
  renderPlot({
    corners_izq <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP", x > 99, y > 99)
    
    corners_izq <- corners_izq %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "2nd Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "1st Post",
        endY > 85 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_izq %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 100, 100, 85, 85),
      palo = rep(c("2nd Post", "Center", "1st Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("2nd Post", "Center", "1st Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    corners_izq <- corners_izq %>% mutate(x = 100-x, y = 100-y)
    poligonos_envios <- poligonos_envios %>% mutate(x = 100-x, y = 100-y)
    poligonos_efectividad <- poligonos_efectividad %>% mutate(x = 100-x, y = 100-y)
    
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 25)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_izq, aes(x = x, y = y), color = "black", size = 3) +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(11.25, 11.25, 11.25, 11.25), y = c(35, 50, 65, 8), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(3.75, 3.75, 3.75), y = c(35, 50, 65), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderDefFaltasCentro <- function(datos, partidin, equipo) {
  faltas <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP") %>% 
    group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  
  # Bandas y centros
  centros_seq <- entiro %>% filter(n()>1, type.displayName == "Pass") 
  
  centros <- entiro %>% filter(n()>1, type.displayName == "Pass") %>% summarise(x = first(x), y = first(y))
  remates <- entiro %>% filter(n()>1, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  
  remates <- remates %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  centros <- centros %>%
    mutate(franja = case_when(
      between(y, 0, 20) ~ "Right",
      between(y, 80, 100) ~ "Left",
      between(y, 20, 80) ~ "Center"
    ))
  
  # Añadir frecuencias al data frame original
  todas_franjas <- data.frame(
    franja = c("Right", "Left", "Center")
  )
  
  # Calcular frecuencias
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligonos <- data.frame(
    x = c(50, 100, 100, 50, 50, 100, 100, 50, 82, 50, 50, 82),
    y = c(0, 0, 20, 20, 80, 80, 100, 100, 20, 20, 80, 80),
    franja = rep(c("Right", "Left", "Center"), each = 4)
  )
  
  poligonos <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  poligonos <- poligonos %>% mutate(x = 100-x, y = 100-y)
  centros_seq <- centros_seq %>% mutate(x = 100-x, y = 100-y, endX = 100-endX, endY = 100-endY)
  remates <- remates %>% mutate(x = 100-x, y = 100-y)
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 50)) +
      
      # centros
      geom_polygon(data = poligonos, aes(x = x, y = y, group = franja, fill = rescale(num_centros)), alpha = 0.5, color = "black") +
      scale_fill_gradient2(high = "red3", low = "yellow3", mid = "orange3", midpoint = 0.5) +
      
      
      geom_point(data = centros_seq, aes(x = x, y = y), color = "black", size = 3) +
      
      
      geom_segment(data = centros_seq, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      # remates
      geom_point(data = remates, aes(x = x, y = y, color = esgol), size = 3) +
      
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_text(data = frecuencias, aes(x = c(25, 25, 35), y = c(90, 10, 50), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}

renderDefLibreDirecto <- function(datos, partidin, equipo) {
  faltas <- datos %>% filter(partido == partidin, team == equipo, fasedejuego == "ABP") %>% 
    group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  libdir <- entiro %>% filter(n()==1, x < 82, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  libdir <- libdir %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  libdir <- libdir %>% mutate(x = 100-x, y = 100-y)
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 50)) +
      
      geom_point(data = libdir, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_segment(data = libdir, aes(x = x, y = y, xend = ifelse(is.na(blockedX), 0, 100-blockedX), yend = ifelse(is.na(blockedY), 100-goalMouthY, 100-blockedY)),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}


################################
############ EQUIPO ############
################################

renderTirosFDA <- function(datos, fasedj, equipo) {
  renderPlot({
    tiros_fda <- datos %>% filter(team == equipo, fasedejuego == fasedj, isShot == T, (x < 82.5) | ((x > 82.5 & y < 20) | (x > 82.5 & y > 80)))
    tiros_fda <- tiros_fda %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    goles <- tiros_fda %>% filter(esgol)
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      
      stat_binhex(data = tiros_fda, aes(x = x, y = 100-y, label = ..count..), geom = "hex", bins = 15, colour = "darkgrey", alpha=0.5) +
      
      geom_point(data = goles, aes(x = x, y = 100-y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "green")) + 
      
      ggplot2::stat_binhex(data = tiros_fda, aes(x = x, y = 100-y,label = ..count..), geom = "text", bins = 15, colour = "black") +
      scale_fill_gradientn(colours =  c("yellow", "orange", "red")) +
      theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 
    
  })
  
  
  
}


renderTirosArea <- function(datos, fasedj, equipo) {
  renderPlot({
    tiros_area <- datos %>% filter(team == equipo, fasedejuego == fasedj, isShot == T, x > 83, between(y, 20, 80))
    tiros_area <- tiros_area %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    goles <- tiros_area %>% filter(esgol)
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_flip(xlim = c(82, 100), ylim = c(20, 80)) +
      
      stat_binhex(data = tiros_area, aes(x = x, y = 100-y, label = ..count..), geom = "hex", bins = 25, colour = "darkgrey", alpha=0.5) +
      
      geom_point(data = goles, aes(x = x, y = 100-y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "green")) + 
      
      ggplot2::stat_binhex(data = tiros_area, aes(x = x, y = 100-y,label = ..count..), geom = "text", bins = 25, colour = "black") +
      scale_fill_gradientn(colours =  c("yellow", "orange", "red")) +
      theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 
    
  })
}


renderIniciosPortero <- function(datos, fasedj, equipo) {
  renderPlot({
    
    datos <- datos %>% filter(fasedejuego == fasedj, team == equipo)
    envios <- datos %>% filter(type.displayName == "Pass", x <= 5, between(y, 36, 63))
    
    envios$cuadrante_x <- cut(envios$endX, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    envios$cuadrante_y <- cut(envios$endY, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    # Contar el número de robos en cada cuadrante
    conteo_envios <- envios %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_envios = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_envios, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_envios), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_envios), color = "black", size = 5, na.rm = TRUE) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_envios))/2) +
      annotate_pitch(colour = "white",
                     fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
}

renderSalidasDefensa <- function(datos, fasedj, equipo) {
  renderPlot({
    
    datos <- datos %>% filter(fasedejuego == fasedj, team == equipo)
    envios <- datos %>% filter(type.displayName == "Pass", ((x < 5 & (y > 75 | y < 25)) | (between(x, 5.1, 35))))
    
    envios$cuadrante_x <- cut(envios$endX, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    envios$cuadrante_y <- cut(envios$endY, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    # Contar el número de robos en cada cuadrante
    conteo_envios <- envios %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_envios = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_envios, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_envios), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_envios), color = "black", size = 5, na.rm = TRUE) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_envios))/2) +
      annotate_pitch(colour = "white",
                     fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderSalidasMC <- function(datos, fasedj, equipo) {
  renderPlot({
    
    datos <- datos %>% filter(fasedejuego == fasedj, team == equipo)
    envios <- datos %>% filter(type.displayName == "Pass", ((between(x, 40, 70) & (y > 85 | y < 15))))
    
    envios$cuadrante_x <- cut(envios$endX, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    envios$cuadrante_y <- cut(envios$endY, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    # Contar el número de robos en cada cuadrante
    conteo_envios <- envios %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_envios = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_envios, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_envios), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_envios), color = "black", size = 5, na.rm = TRUE) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_envios))/2) +
      annotate_pitch(colour = "white",
                     fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}


renderCentrosEquipo <- function(datos, fasedj, equipo){
  datos <- datos %>% filter(fasedejuego == fasedj, team == equipo)
  
  centros <- sacar_centros(datos)
  
  centros <- centros %>%
    mutate(franja = case_when(
      x >= 50 & x <= 85 & y <= 25 ~ "Far Right",
      x > 85 & y <= 25 ~ "Deep Right",
      x >= 50 & x <= 85 & y >= 75 ~ "Far Left",
      x > 85 & y >= 75 ~ "Deep Left"
    ))
  
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligoneiros <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(50, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligoneiros, aes(x = x, y = y, group = franja, fill = num_centros), alpha = 0.5, color = "black") +
      
      # Ajustar colores
      scale_fill_gradient2(low = "#3ab54a", high = "red", mid = "yellow", midpoint = max(poligoneiros$num_centros)/2) +
      
      # Añadir etiquetas de porcentaje y número de centros
      geom_text(data = frecuencias, aes(x = c(67.5, 92.5, 67.5, 92.5), y = c(12.5, 12.5, 87.5, 87.5), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}

renderFlechasEquipo <- function(datos, fasedj, equipo){
  renderPlot({
    contras <- datos %>% filter(team == equipo, str_detect(fasedejuego, fasedj)) %>% filter(type.displayName=="Pass")
    
    
    aux <- contras %>% group_by(posesion) %>%
      summarise(
        pen_x = nth(x, -2),
        pen_endX = nth(endX, -2),
        pen_endY = nth(endY, -2),
        ult_x = nth(x, -1),
        pen_y = nth(y, -2),
        ult_y = nth(y, -1)
      ) 
    
    carril <- aux %>% mutate(carril = ifelse(((ult_x > 60) & (pen_y < 35) & (ult_y < 35)) | ((pen_x > 60) & (pen_y < 35) & pen_endX > 82 & between(pen_endY, 20, 80)), "Derecho", 
                                             ifelse(((ult_x > 60) & (pen_y > 65) & (ult_y > 65)) | ((pen_x > 60) & (pen_y > 65) & pen_endX > 82 & between(pen_endY, 20, 80)), "Izquierdo", 
                                                    ifelse((ult_x > 60) & between(pen_y, 35, 65) & between(ult_y, 35, 65), "Centro", "Mixto"))))
    
    carr <- carril %>% select(posesion, carril)
    
    contras <- contras %>% left_join(carr, by = "posesion") %>% filter(carril %in% c("Izquierdo", "Centro", "Derecho"))
    
    # Contar el número de acciones en cada franja
    conteo_acciones <- contras %>%
      group_by(carril) %>%
      summarise(n_acciones = n_distinct(posesion)) %>% 
      mutate(porcentaje = round(n_acciones / sum(n_acciones) * 100, 2))
    
    # Crear un dataframe para las flechas y textos
    flechas <- data.frame(
      carril = c("Derecho", "Centro", "Izquierdo"),
      x_start = 20,
      y_start = c(16.665, 50, 83.335),
      x_end = 80,
      y_end = c(16.665, 50, 83.335),
      label_x = 90,
      label_y = c(16.665, 50, 83.335)
    ) %>%
      left_join(conteo_acciones, by = "carril") %>% mutate_all(~replace(., is.na(.), 0))
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_segment(data = flechas, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = n_acciones, size = n_acciones),
                   arrow = arrow(length = unit(0.3, "inches"))) +
      geom_text(data = flechas, aes(x = label_x, y = label_y, label = paste(porcentaje, "%"), color = n_acciones), size = 5, fontface = "bold") +
      scale_color_gradient2(low = "yellow2", high = "red", mid = "orange3", midpoint = max(flechas$n_acciones)/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
}

renderCombinacionesEquipoDef <- function(datos, fasedj, equipo) {
  renderUI({
    df <- datos %>% filter(team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful", between(x, 5, 35))
    
    df <- combinaciones_comunes(df, 3)
    
    html_list <- list()
    
    for (i in 1:nrow(df)) {
      # Obtener la secuencia y el número de repeticiones
      sequence <- df$sequence[[i]]
      count <- df$n[i]
      
      # Obtener los nombres de los jugadores de la secuencia
      jugadores_seq <- strsplit(sequence, " - ")[[1]]
      
      image_list <- list()
      
      for (j in 1:length(jugadores_seq)) {
        # Obtener la URL de la foto del jugador
        foto_url <- jugadores %>% filter(name == jugadores_seq[j], team == equipo) %>% select(foto) %>% pull()
        # Si la foto no existe, usar un marcador de posición
        if (is.na(foto_url) || length(foto_url) == 0) {
          foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
        }
        img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
        name_tag <- tags$p(jugadores_seq[j], style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
        player_div <- tags$div(class = "player", img_tag, name_tag)
        image_list <- append(image_list, list(player_div))
        
        if (j < length(jugadores_seq)) {
          arrow_div <- tags$div(class = "arrow", style = "font-size: 23px; font-weight: bold; margin-bottom: 15px;", "→")
          image_list <- append(image_list, list(arrow_div))
        }
        
        row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                             do.call(tagList, image_list), 
                             tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(count)))
        
        # Agregar la fila al html_list
        html_list[[i]] <- row_html
        
      }
    }
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderCombinacionesEquipoMed <- function(datos, fasedj, equipo) {
  renderUI({
    df <- datos %>% filter(team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful", between(x, 40, 70))
    
    df <- combinaciones_comunes(df, 3)
    
    html_list <- list()
    
    for (i in 1:nrow(df)) {
      # Obtener la secuencia y el número de repeticiones
      sequence <- df$sequence[[i]]
      count <- df$n[i]
      
      # Obtener los nombres de los jugadores de la secuencia
      jugadores_seq <- strsplit(sequence, " - ")[[1]]
      
      image_list <- list()
      
      for (j in 1:length(jugadores_seq)) {
        # Obtener la URL de la foto del jugador
        foto_url <- jugadores %>% filter(name == jugadores_seq[j], team == equipo) %>% select(foto) %>% pull()
        # Si la foto no existe, usar un marcador de posición
        if (is.na(foto_url) || length(foto_url) == 0) {
          foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
        }
        img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
        name_tag <- tags$p(jugadores_seq[j], style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
        player_div <- tags$div(class = "player", img_tag, name_tag)
        image_list <- append(image_list, list(player_div))
        
        if (j < length(jugadores_seq)) {
          arrow_div <- tags$div(class = "arrow", style = "font-size: 23px; font-weight: bold; margin-bottom: 15px;", "→")
          image_list <- append(image_list, list(arrow_div))
        }
        
        row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                             do.call(tagList, image_list), 
                             tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(count)))
        
        # Agregar la fila al html_list
        html_list[[i]] <- row_html
        
      }
    }
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderPasesMediosEquipo <- function(datos, fasedj, equipo){
  renderValueBox({
    datos <- datos %>% filter(fasedejuego == fasedj, team == equipo)
    mediapases <- datos %>% filter(fasedejuego == "Construccion Ofensiva") %>% group_by(partido, posesion) %>% summarise(pases = n()-1) %>% filter(pases > 1)
    media <- mean(mediapases$pases)
    
    valueBox(
      value = formatC(media, digits = 1, format = "f"),
      subtitle = "Passes per possession (mean)",
      icon = icon("arrows-spin"),
      color = "green"
    )
  })
}
renderVerticalidadEquipo <- function(datos, fasedj, equipo){
  renderValueBox({
    datos <- datos %>% filter(fasedejuego == fasedj, team == equipo)
    verticalidad <- datos %>% group_by(partido, posesion) %>% filter(n() > 2) %>% summarise(vert = (last(x)-first(x))/(n()-1)) 
    vert <- mean(verticalidad$vert)*1.05
    
    valueBox(
      value = formatC(vert, digits = 1, format = "f"),
      subtitle = "Metres won per pass (mean)",
      icon = icon("angles-right"),
      color = "red"
    )
  })
}

renderPorcGoles <- function(datos, fasedj, equipo) {
  renderValueBox({
  
    goles <- datos %>% filter(team == equipo, type.displayName == "Goal") %>% group_by(fasedejuego) %>% summarise(total = n()) %>% mutate(porc = round(total / sum(total) * 100, 2))
    porc_goles_of <- goles %>% filter(fasedejuego == fasedj)
    
    valueBox(
      value = formatC(ifelse(nrow(porc_goles_of) == 0, 0, porc_goles_of$porc), digits = 1, format = "f"),
      subtitle = "% of Goals Scored in this Phase",
      icon = icon("arrow-right"),
      color = "blue"
    )
    
  })
  
}

renderFinalizadoresEquipo <- function(datos, fasedj, equipo) {
  renderUI({
    tiros <- datos %>% filter(team == equipo, (fasedejuego == fasedj) | (lag(fasedejuego) == fasedj) | (lag(fasedejuego, 2) == fasedj), (str_detect(type.displayName, "Shot") | type.displayName == "Goal"))
    
    tiros <- tiros %>% group_by(name) %>% summarise(tiros = n()) %>% arrange(desc(tiros))
    
    tiros <- tiros[1:min(3, nrow(tiros)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(tiros)) {
      # Obtener la secuencia y el número de repeticiones
      player <- tiros$name[i]
      shots <- tiros$tiros[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(paste(shots, "Shots"))))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderCentradoresEquipo <- function(datos, fasedj, equipo) {
  renderUI({
    datos <- datos %>% filter(team == equipo, fasedejuego == fasedj)
    centros <- sacar_centros(datos)
    
    centros <- centros %>% group_by(name) %>% summarise(centros = n()) %>% arrange(desc(centros))
    
    centros <- centros[1:min(3, nrow(centros)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(centros)) {
      # Obtener la secuencia y el número de repeticiones
      player <- centros$name[i]
      crosses <- centros$centros[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(paste(crosses, "Crosses"))))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderProtasOfensivos <- function(datos, fasedj, equipo, faceta, ipt, sev_otro){
  renderUI({
    
    minutos_jugador <- datos %>%
      filter(team == equipo) %>%
      group_by(name, partido) %>%
      summarise(
        minutos = case_when(
          any(type.displayName == "SubstitutionOff") ~ min(90, max(minute[type.displayName == "SubstitutionOff"])),
          any(type.displayName == "SubstitutionOn") ~ max(0, min(90, 90 - min(minute[type.displayName == "SubstitutionOn"]))),
          TRUE ~ 90
        ),
        .groups = "drop"
      )
    
    
    minutos_por90 <- minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), por90 = sum(minutos)/90) %>% na.omit()
    
    
    
    tiros <- datos %>% filter(team == equipo, fasedejuego == fasedj, type.displayName %in% c("MissedShots", "SavedShot", "Goal", "ShotOnPost"))
    
    conteo_tiros <- tiros %>% group_by(name) %>% summarise(a_puerta = sum(type.displayName %in% c("SavedShot", "Goal", "ShotOnPost")),
                                                                     fuera = sum(type.displayName == "MissedShots"),
                                                                     total = a_puerta + fuera,
                                                                     goles = sum(type.displayName == "Goal"),
                                                                     xG = sum(na.omit(xG)),
                                                                     xGOT = sum(na.omit(xGOT)))
    # por90
    conteo_tiros <- merge(conteo_tiros, minutos_por90, by = "name")
    
    conteo_tiros <- conteo_tiros %>% mutate(goles_90 = round(goles / por90, 2),
                                            xG_90 = round(xG / por90, 2),
                                            xGOT_90 = round(xGOT / por90, 2),
                                            a_puerta_90 = round(a_puerta / por90, 2),
                                            total_90 = round(total / por90, 2))
    
    conteo_tiros <- conteo_tiros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(goles_90))
    
    # conteo_tiros <- conteo_tiros %>% arrange(desc(goles))
    
    
    
    
    regates <- datos %>% filter(team == equipo, fasedejuego == fasedj) %>% mutate(next_event = lead(type.displayName),
                                                           next_outcome = lead(outcomeType.displayName)) %>% filter(type.displayName == "TakeOn")
    
    
    conteo_regates <- regates %>% group_by(name) %>% summarise(exito = sum(outcomeType.displayName == "Successful"),
                                             fracaso = sum(outcomeType.displayName == "Unsuccessful"),
                                             continuidad = sum(ifelse(outcomeType.displayName == "Successful" & next_event %in% c("Pass", "SavedShot", "MissedShots", "ShotOnPost", "Goal", "TakeOn", "Foul") & next_outcome == "Successful", T, F)),
                                             porc_continuidad = round(continuidad/exito * 100, 2))
    
    
    
    
    # patrones 
    # pases filtrados en 3/4
    pases_filtrados <- datos %>% filter(team == equipo, fasedejuego == fasedj, type.displayName == "Pass", x > 65, endX - x >= 10, abs(endY - y) < 15, between(y, 35, 65), outcomeType.displayName == "Successful")
    conteo_filtrados <- pases_filtrados %>% group_by(name) %>% summarise(pases_filtrados = n())
    
    conteo_filtrados <- conteo_filtrados %>% arrange(desc(pases_filtrados))
    
    
    # cambios de orientación 
    porteros <- ""
    if (ipt[[paste0("excluir_portero_", sev_otro)]]) {
      porteros <- jugadores %>% filter(team == equipo, position == "Goalkeeper")
      porteros <- porteros$name
    }
    
    contactos_zona <- datos %>% filter(team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful", !(name %in% porteros),
                                            ((lag(y) < 20) | (lag(y) > 80)) & between(y, 30, 60))
    
    contactos_zona <- contactos_zona %>% group_by(name) %>% summarise(contactos = n())
    
    
    cambios_orientacion <- datos %>% filter(team == equipo, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful", !(name %in% porteros),
                                                ((lag(y) < 20) & between(y, 30, 60) & (endY > 80)) | ((lag(y) < 20) & between(y, 30, 60) & (endY < 80) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) > 80)) |
                                                ((lag(y) > 80) & between(y, 30, 60) & (endY < 20)) | ((lag(y) > 80) & between(y, 30, 60) & (endY > 20) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) < 20))) 
    
                                                
    conteo_cambios <- cambios_orientacion %>% group_by(name) %>% summarise(cambios = n())
    
    conteo_cambios <- merge(conteo_cambios, contactos_zona, by = "name")
    # conteo_cambios <- conteo_cambios %>% arrange(desc(cambios))
    
    # por90
    conteo_cambios <- merge(conteo_cambios, minutos_por90, by = "name")
    
    conteo_cambios <- conteo_cambios %>% mutate(cambios_90 = round(cambios / por90, 2),
                                                contactos_90 = round(contactos / por90, 2),
                                                porc_cambios = round(cambios_90 / contactos_90 * 100, 2))
    
    conteo_cambios <- conteo_cambios %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2, cambios > max(conteo_cambios$cambios)/2) %>% arrange(desc(porc_cambios))
    
    
    # centros
    centros <- datos %>% filter(team == equipo, fasedejuego == fasedj)
    centros <- sacar_centros(centros)
    
    centros <- centros %>% group_by(name) %>% summarise(total_centros = n(),
                                                        buenos_centros = sum(outcomeType.displayName == "Successful"),
                                                        malos_centros = sum(outcomeType.displayName == "Unuccessful"),
                                                        ratio = round(buenos_centros / total_centros * 100, 2))
    centros <- centros %>% filter(total_centros > max(centros$total_centros)/2)
    
    
    # por90
    centros <- merge(centros, minutos_por90, by = "name")
    
    centros <- centros %>% mutate(total_90 = round(total_centros / por90, 2),
                                  buenos_90 = round(buenos_centros / por90, 2),
                                  malos_90 = round(malos_centros / por90, 2))
    
    conteo_centros <- centros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(ratio))
    
    # asistencias
    asis <- datos %>% filter(team == equipo, fasedejuego == fasedj)
    asistentes_gol <- asis %>% filter(type.displayName == "Goal") %>% mutate(aux = paste(partido, relatedEventId, sep = ", "))  
    name_asis_gol <- asis %>% mutate(aux2 = paste(partido, eventId, sep = ", ")) %>%  filter(aux2 %in% asistentes_gol$aux) %>% select(name)   
    
    asistentes <- name_asis_gol %>% group_by(name) %>% summarise(asistencias = n())
    
    # por90
    asistentes <- merge(asistentes, minutos_por90, by = "name")
    
    asistentes <- asistentes %>% mutate(asistencias_90 = round(asistencias / por90, 2))
    
    asistentes <- asistentes %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(asistencias_90))
    
    
    # el mejor de cada faceta?
    if (faceta == "cambios"){
      nombre_tio <- conteo_cambios$name[1]
    } else if (faceta == "filtrados") {
      nombre_tio <- conteo_filtrados$name[1]
    } else if (faceta == "goles") {
      nombre_tio <- conteo_tiros$name[1]
    } else if (faceta == "centros") {
      nombre_tio <- conteo_centros$name[1]
    } else if (faceta == "asistencias") {
      nombre_tio <- asistentes$name[1]
    }
    
    foto_url <- jugadores %>% filter(name == nombre_tio, team == equipo) %>% select(foto) %>% pull()
    
    if (is.na(foto_url) || length(foto_url) == 0) {
      foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
    }
    
    if (faceta == "cambios"){
      categorias <- paste("Passes Changing Orientation", "% of Contacts Received from a Side Being a Change", sep = " & ")
      resultados <- paste(c(conteo_cambios$cambios[1], conteo_cambios$porc_cambios[1]), collapse = " & ")
      categorias2 <- "Passes Changing Orientation (per90)"
      resultados2 <- conteo_cambios$cambios_90[1]
    } else if (faceta == "filtrados") {
      categorias <- "Filtered Passes (near Rival Box)"
      resultados <- conteo_filtrados$pases_filtrados[1]
    } else if (faceta == "goles") {
      categorias <- paste("Goals", "xG", "xGOT", "Shots On Target / Total", sep = " & ")
      resultados <- paste(c(conteo_tiros$goles[1], conteo_tiros$xG[1], conteo_tiros$xGOT[1], paste(conteo_tiros$a_puerta[1], "/", conteo_tiros$total[1])), collapse = " & ")
      categorias2 <- paste("Goals (per90)", "xG (per90)", "xGOT (per90)", "Shots On Target / Total (per90)", sep = " & ")
      resultados2 <- paste(c(conteo_tiros$goles_90[1], conteo_tiros$xG_90[1], conteo_tiros$xGOT_90[1], paste(conteo_tiros$a_puerta_90[1], "/", conteo_tiros$total_90[1])), collapse = " & ")
    } else if (faceta == "centros") {
      categorias <- paste("Successful Crosses", "Total Crosses", "Ratio", sep = " & ")
      resultados <- paste(c(conteo_centros$buenos_centros[1], conteo_centros$total_centros[1], paste0(conteo_centros$ratio[1], "%")), collapse = " & ")
      categorias2 <- paste("Successful Crosses (per90)", "Total Crosses (per90)", sep = " & ")
      resultados2 <- paste(c(conteo_centros$buenos_90[1], conteo_centros$total_90[1]), collapse = " & ")
    } else if (faceta == "asistencias") {
      categorias <- "Assists"
      resultados <- asistentes$asistencias[1]
      categorias2 <- "Assists (per90)"
      resultados2 <- asistentes$asistencias_90[1]
    }
    
    
    # Crear la tabla personalizada
    tabla_html <- htmltools::HTML(
      paste0(
        '<table class="table table-striped" style="width: 100%; border: 3px solid black;">',
        '<thead>',
        '<tr>',
        '<th colspan="5" style="text-align:center; border-bottom: 3px solid black; font-size: 20px; font-family: Arial, sans-serif;">',
        '<div style="display: flex; align-items: center; justify-content: center; margin-bottom: 10px; margin-top: 10px;">',
        '<img src="', foto_url, '" style="width:100px;height:100px; vertical-align: middle; margin-right: 40px;">',
        nombre_tio,
        '</th>',
        '</tr>',
        '</thead>',
        '<tbody>',
        '<tr>',
        '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias), '</td>',
        '</tr>',
        '<tr>',
        '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados), '</td>',
        '</tr>',
        '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias2), '</td>',
        '</tr>',
        '<tr>',
        '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados2), '</td>',
        '</tr>',
        '</tbody>',
        '</table>'
      )
    )
    
    
    htmltools::HTML(tabla_html)
  })
  
}


renderTirosJugador <- function(datos, fasedj, equipo, player) {
  renderPlot({
    tiros_fda <- datos %>% filter(team == equipo, name == player, fasedejuego == fasedj, isShot == T)
    tiros_fda <- tiros_fda %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    goles <- tiros_fda %>% filter(esgol)
    
    
    foto_jug <- jugadores %>% filter(team == equipo, name == player) %>% select(foto) %>% pull()
    foto_jug <- rasterGrob(as.raster(image_read(foto_jug)), interpolate = TRUE)
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      
      stat_binhex(data = tiros_fda, aes(x = x, y = 100-y, label = ..count..), geom = "hex", bins = 15, colour = "darkgrey", alpha=0.5) +
      
      geom_point(data = goles, aes(x = x, y = 100-y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "green")) + 
      
      ggplot2::stat_binhex(data = tiros_fda, aes(x = x, y = 100-y,label = ..count..), geom = "text", bins = 15, colour = "black") +
      scale_fill_gradientn(colours =  c("yellow", "orange", "red")) +
      theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 
    
    
    # p + annotation_custom(foto_jug, xmin = 50, xmax = 70, ymin = 75, ymax = 100)
  })
  
  
  
}

renderCentrosJugador <- function(datos, fasedj, equipo, player) {
  datos <- datos %>% filter(fasedejuego == fasedj, team == equipo, name == player)
  
  centros <- sacar_centros(datos)
  
  centros <- centros %>%
    mutate(franja = case_when(
      x >= 50 & x <= 85 & y <= 25 ~ "Far Right",
      x > 85 & y <= 25 ~ "Deep Right",
      x >= 50 & x <= 85 & y >= 75 ~ "Far Left",
      x > 85 & y >= 75 ~ "Deep Left"
    ))
  
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligoneiros <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(50, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligoneiros, aes(x = x, y = y, group = franja, fill = num_centros), alpha = 0.5, color = "black") +
      
      # Ajustar colores
      scale_fill_gradient2(low = "#3ab54a", high = "red", mid = "yellow", midpoint = max(poligoneiros$num_centros)/2) +
      
      # Añadir etiquetas de porcentaje y número de centros
      geom_text(data = frecuencias, aes(x = c(67.5, 92.5, 67.5, 92.5), y = c(12.5, 12.5, 87.5, 87.5), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
  
  
  
}

renderFiltrosJugador <- function(datos, fasedj, equipo, player) {
  renderPlot({
    pases_filtrados <- datos %>% filter(team == equipo, name == player, fasedejuego == fasedj, type.displayName == "Pass", x > 65, endX - x >= 10, abs(endY - y) < 15, between(y, 35, 65), outcomeType.displayName == "Successful")
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(50, 100)) +
      
      # Añadir los pases
      geom_point(data = pases_filtrados, aes(x = x, y = y), color = "black", size = 3) +
      geom_segment(data = pases_filtrados, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +

      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
    # p + annotation_custom(foto_jug, xmin = 50, xmax = 70, ymin = 75, ymax = 100)
  })
  
  
  
}

renderCambiosJugador <- function(datos, fasedj, equipo, player) {
  renderPlot({
    cambios_orientacion <- datos %>% mutate(prev_x = lag(x), prev_y = lag(y), next_x = lead(x), next_y = lead(y), next_pass_x = lead(endX), next_pass_y = lead(endY)) %>% filter(name == player, fasedejuego == fasedj, type.displayName == "Pass", outcomeType.displayName == "Successful", 
                                            ((lag(y) < 20) & between(y, 30, 60) & (endY > 80)) | ((lag(y) < 20) & between(y, 30, 60) & (endY < 80) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) > 80)) |
                                            ((lag(y) > 80) & between(y, 30, 60) & (endY < 20)) | ((lag(y) > 80) & between(y, 30, 60) & (endY > 20) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) < 20))) 
    
    cambios_orientacion$posesion <- as.factor(cambios_orientacion$posesion)
    
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      
      # Añadir los cambios
      geom_segment(data = cambios_orientacion, aes(x = prev_x, y = prev_y, xend = x, yend = y), linetype = "dashed", color = "red") +
      
      
      geom_segment(data = cambios_orientacion, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      
      geom_segment(data = cambios_orientacion, aes(x = endX, y = endY, xend = next_x, yend = next_y), linetype = "dashed", color = "blue") +
      
      geom_segment(data = cambios_orientacion, aes(x = next_x, y = next_y, xend = next_pass_x, yend = next_pass_y),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
    # p + annotation_custom(foto_jug, xmin = 50, xmax = 70, ymin = 75, ymax = 100)
  })
  
  
  
}

renderAsistenciasJugador <- function(datos, fasedj, equipo, player) {
  renderPlot({
    asis <- datos %>% filter(fasedejuego == fasedj)
    asistentes_gol <- asis %>% filter(type.displayName == "Goal") %>% mutate(aux = paste(partido, relatedEventId, sep = ", "))  
    name_asis_gol <- asis %>% mutate(aux2 = paste(partido, eventId, sep = ", ")) %>% filter(aux2 %in% asistentes_gol$aux, name == player)
    
    asistentes_gol <- asistentes_gol %>% filter(aux %in% name_asis_gol$aux2)
    
    ggplot() +
    annotate_pitch(colour = "white", fill = "#3ab54a") +
    theme_pitch() +
    
    # Añadir los cambios
    geom_segment(data = name_asis_gol, aes(x = x, y = y, xend = endX, yend = endY), linetype = "dashed", color = "red") +
    
    
    geom_point(data = asistentes_gol, aes(x = x, y = y), color = "green") +
    
    ggrepel::geom_label_repel(
      data = asistentes_gol,
      aes(x = x, y = y, label=name), alpha = 0.5,
      size=3) +
    # Configurar el tema
    theme_pitch() + theme_void() + theme(legend.position = "none") + 
    theme(panel.background = element_rect(fill = "#3ab54a"))
    
    
  })
}


########## TRANSICIÓN OFENSIVA

renderRecuperacionesEquipo <- function(datos, fasedj, equipo){
  renderPlot({
    robos <- datos %>% filter(str_detect(fasedejuego, fasedj), team == equipo) %>% filter(type.displayName %in% c("BallRecovery", "Interception")) 
    
    robos$cuadrante_x <- cut(robos$x, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    robos$cuadrante_y <- cut(robos$y, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    # Contar el número de robos en cada cuadrante
    conteo_robos <- robos %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_robos = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_robos, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_robos), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_robos), color = "black", size = 5, na.rm = TRUE) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_robos))/2) +
      
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}

es_recuperacion_rapida_equipo <- function(datos, tiempo_recuperacion, equipo, pos, partidin) {
  # Filtrar eventos del mismo equipo en los 10 segundos anteriores
  eventos_anteriores <- datos %>% 
    filter(partido == partidin,
           team == equipo, 
           posesion != pos, 
           tiempo >= (tiempo_recuperacion - 10),
           tiempo < tiempo_recuperacion,
           type.displayName %in% c("Pass"))
  
  # Retornar TRUE si hay al menos una posesión en ese intervalo
  return(nrow(eventos_anteriores) > 0)
}


renderRecuperacionesRapidasEquipo <- function(datos, fasedj, equipo) {
  renderValueBox({
    robos <- datos %>% filter(str_detect(fasedejuego, fasedj), team == equipo) %>% filter(type.displayName %in% c("BallRecovery", "Interception")) 
  
    c_datos <- datos %>% filter(team == equipo)
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    robos_rapidos <- robos %>% rowwise() %>% mutate(rapida = es_recuperacion_rapida_equipo(c_datos, minute*60+second, team, posesion, partido))
    robos_rapidos <- robos_rapidos %>% filter(rapida, type.displayName == "BallRecovery")
    
    
    porc_rapidos <- nrow(robos_rapidos) / nrow(robos) * 100
    
    
    valueBox(
      value = formatC(porc_rapidos, digits = 1, format = "f"),
      subtitle = "% of Quick Steals After Loss",
      icon = icon("fire"),
      color = "blue"
    )
  })
  
}

renderRecuperadoresEquipo <- function(datos, fasedj, equipo) {
  renderUI({
    robos <- datos %>% filter(team == equipo, str_detect(fasedejuego, fasedj), type.displayName %in% c("BallRecovery", "Interception"))
    
    robos <- robos %>% group_by(name) %>% summarise(robos = n()) %>% arrange(desc(robos))
    
    robos <- robos[1:min(3, nrow(robos)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(robos)) {
      # Obtener la secuencia y el número de repeticiones
      player <- robos$name[i]
      recoveries <- robos$robos[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(paste(recoveries, "Steals"))))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderPorcVerticalesEquipo <- function(datos, fasedj, equipo){
  renderValueBox({
    transiciones <- datos %>% filter(team == equipo, str_detect(fasedejuego, fasedj)) %>% group_by(partido, posesion) %>% filter(n()>2)
    n_pos <- length(unique(transiciones$posesion))
    
    porc_vert <- transiciones %>% group_by(fasedejuego) %>% summarise(veces = n_distinct(posesion)) %>% mutate(porc_fase = round(veces/n_pos, 2)*100)
    porc_vert <- porc_vert$porc_fase[porc_vert$fasedejuego == "Transicion DEF-AT Vertical"]
    
    
    valueBox(
      value = formatC(porc_vert, digits = 1, format = "f"),
      subtitle = "% Of transitions being VERTICAL",
      icon = icon("angles-right"),
      color = "blue"
    )
  })
}

renderPorcFinalizadasEquipo <- function(datos, fasedj, equipo){
  renderValueBox({
    transiciones <- datos %>% filter(team == equipo, str_detect(fasedejuego, fasedj)) %>% group_by(partido, posesion) %>% filter(n()>2)
    
    porc_final <- transiciones %>% group_by(fasedejuego, posesion) %>% 
      summarise(finalizadas = any(str_detect(type.displayName, "Shot") | type.displayName == "Goal")) %>% 
      group_by(fasedejuego) %>% summarise(porc_fin = round(sum(finalizadas)/n_distinct(posesion), 3)*100)
    
    porc_final <- porc_final$porc_fin[porc_final$fasedejuego == "Transicion DEF-AT Vertical"]
    
    valueBox(
      value = formatC(porc_final, digits = 1, format = "f"),
      subtitle = "% Of VERTICAL transitions ended in Shot",
      icon = icon("arrow-right-to-bracket"),
      color = "blue"
    )
  })
}

renderContrasVerticalesEquipo <- function(datos, equipo){
  
  renderPlot({
    verticales <- datos %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == equipo)
    
    lineas_discontinuas <- verticales %>% group_by(partido, posesion) %>% mutate(next_x = lead(x), next_y = lead(y)) %>% select(endX, endY, next_x, next_y, partido, posesion)
    lineas_discontinuas$posesion <- as.factor(lineas_discontinuas$posesion)
    
    shots <- verticales %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal")
    shots <- shots %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    verticales$posesion <- as.factor(verticales$posesion)
    posesion_colors <- viridis::viridis_pal(option = "D")(length(unique(verticales$posesion)))
    
    ggplot() +
      geom_segment(data = verticales, aes(x = x, y = y, xend = endX, yend = endY, color = posesion),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      geom_segment(data = lineas_discontinuas, aes(x = endX, y = endY, xend = next_x, yend = next_y, color = posesion), linetype = "dashed") +
      
      scale_color_manual(values = posesion_colors) +
      
      geom_point(data = shots, aes(x = x, y = y, fill = esgol), shape = 21, size = 3, color = "black") + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      
      ggrepel::geom_label_repel(
        data = shots,
        aes(x = x, y = y, label=name), alpha = 0.5,
        size=3) +
      
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
  
}


############### FASE DEFENSIVA
renderDefTirosFDA <- function(datos, fasedj, equipo) {
  renderPlot({
    tiros_fda <- datos %>% filter(team != equipo, fasedejuego == fasedj, isShot == T, (x < 82.5) | ((x > 82.5 & y < 20) | (x > 82.5 & y > 80)))
    tiros_fda <- tiros_fda %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    tiros_fda <- tiros_fda %>% mutate(x = 100-x, y = 100-y)
    
    
    goles <- tiros_fda %>% filter(esgol)
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_flip(xlim = c(0, 50), ylim = c(0, 100)) +
      
      stat_binhex(data = tiros_fda, aes(x = x, y = 100-y, label = ..count..), geom = "hex", bins = 15, colour = "darkgrey", alpha=0.5) +
      
      geom_point(data = goles, aes(x = x, y = 100-y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "green")) + 
      
      ggplot2::stat_binhex(data = tiros_fda, aes(x = x, y = 100-y,label = ..count..), geom = "text", bins = 15, colour = "black") +
      scale_fill_gradientn(colours =  c("yellow", "orange", "red")) +
      theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 
    
  })
  
  
  
}


renderDefTirosArea <- function(datos, fasedj, equipo) {
  renderPlot({
    tiros_area <- datos %>% filter(team != equipo, fasedejuego == fasedj, isShot == T, x > 83, between(y, 20, 80))
    tiros_area <- tiros_area %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    tiros_area <- tiros_area %>% mutate(x = 100-x, y = 100-y)
    
    goles <- tiros_area %>% filter(esgol)
    
    ggplot() + 
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_flip(xlim = c(0, 18), ylim = c(20, 80)) +
      
      stat_binhex(data = tiros_area, aes(x = x, y = 100-y, label = ..count..), geom = "hex", bins = 25, colour = "darkgrey", alpha=0.5) +
      
      geom_point(data = goles, aes(x = x, y = 100-y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "green")) + 
      
      ggplot2::stat_binhex(data = tiros_area, aes(x = x, y = 100-y,label = ..count..), geom = "text", bins = 25, colour = "black") +
      scale_fill_gradientn(colours =  c("yellow", "orange", "red")) +
      theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 
    
  })
}

renderDefPorcGoles <- function(datos, fasedj, equipo) {
  renderValueBox({
    
    goles <- datos %>% filter(team != equipo, type.displayName == "Goal") %>% group_by(fasedejuego) %>% summarise(total = n()) %>% mutate(porc = round(total / sum(total) * 100, 2))
    porc_goles_of <- goles %>% filter(fasedejuego == fasedj)
    
    
    
    valueBox(
      value = formatC(ifelse(nrow(porc_goles_of) == 0, 0, porc_goles_of$porc), digits = 1, format = "f"),
      subtitle = "% of Goals Scored in this Rival Phase",
      icon = icon("arrow-right"),
      color = "blue"
    )
    
  })
  
}

renderDefFlechasEquipo <- function(datos, fasedj, equipo){
  renderPlot({
    contras <- datos %>% filter(team != equipo, str_detect(fasedejuego, fasedj)) %>% filter(type.displayName=="Pass")
    
    
    aux <- contras %>% group_by(posesion) %>%
      summarise(
        pen_x = nth(x, -2),
        pen_endX = nth(endX, -2),
        pen_endY = nth(endY, -2),
        ult_x = nth(x, -1),
        pen_y = nth(y, -2),
        ult_y = nth(y, -1)
      ) 
    
    carril <- aux %>% mutate(carril = ifelse(((ult_x > 60) & (pen_y < 35) & (ult_y < 35)) | ((pen_x > 60) & (pen_y < 35) & pen_endX > 82 & between(pen_endY, 20, 80)), "Derecho", 
                                             ifelse(((ult_x > 60) & (pen_y > 65) & (ult_y > 65)) | ((pen_x > 60) & (pen_y > 65) & pen_endX > 82 & between(pen_endY, 20, 80)), "Izquierdo", 
                                                    ifelse((ult_x > 60) & between(pen_y, 35, 65) & between(ult_y, 35, 65), "Centro", "Mixto"))))
    
    carr <- carril %>% select(posesion, carril)
    
    contras <- contras %>% left_join(carr, by = "posesion") %>% filter(carril %in% c("Izquierdo", "Centro", "Derecho"))
    
    # Contar el número de acciones en cada franja
    conteo_acciones <- contras %>%
      group_by(carril) %>%
      summarise(n_acciones = n_distinct(posesion)) %>% 
      mutate(porcentaje = round(n_acciones / sum(n_acciones) * 100, 2))
    
    # Crear un dataframe para las flechas y textos
    flechas <- data.frame(
      carril = c("Derecho", "Centro", "Izquierdo"),
      x_start = 80,
      y_start = c(83.335, 50, 16.665),
      x_end = 20,
      y_end = c(83.335, 50, 16.665),
      label_x = 10,
      label_y = c(83.335, 50, 16.665)
    ) %>%
      left_join(conteo_acciones, by = "carril") %>% mutate_all(~replace(., is.na(.), 0))
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_segment(data = flechas, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = n_acciones, size = n_acciones),
                   arrow = arrow(length = unit(0.3, "inches"))) +
      geom_text(data = flechas, aes(x = label_x, y = label_y, label =  paste(porcentaje, "%"), color = n_acciones), size = 5, fontface = "bold") +
      scale_color_gradient2(low = "yellow2", high = "red", mid = "orange3", midpoint = max(flechas$n_acciones)/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
}

calculo_ppda <- function(datos, equipo) {
  equipo_def <- datos %>% group_by(posesion) %>% filter(any(x < 67))
  
  pases_at <- equipo_def %>% filter(type.displayName %in% c("Pass", "BallTouch", "Aerial", "Interception", "TakeOn", "MissedShots", "SavedShot", "Goal", "ShotOnPost"), outcomeType.displayName == "Successful", team != equipo)
  
  acciones_def <- which(
    (datos$type.displayName == "Foul" & datos$outcomeType.displayName == "Unsuccessful" & datos$team == equipo & datos$x>33) | 
      (datos$type.displayName == "Dispossessed" & datos$outcomeType.displayName == "Successful" & datos$team != equipo & datos$x>33) | 
      (datos$type.displayName == "Aerial" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & lag(datos$team) != equipo & datos$x>33) |
      (datos$type.displayName == "BlockedPass" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33) |
      (datos$type.displayName == "Tackle" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33) |
      (datos$type.displayName == "Interception" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33)
    
  )
  
  return (nrow(pases_at)/length(acciones_def))
}

renderPPDAEquipo <- function(datos, equipo){

  ppda_eq <- datos %>% group_by(partido) %>% group_modify(~ tibble(ppda = calculo_ppda(.x, equipo))) %>% ungroup()
  
  ppda <- mean(ppda_eq$ppda)
  
  renderValueBox({
    valueBox(
      value = formatC(ppda, digits = 1, format = "f"),
      subtitle = "PPDA",
      icon = icon("shield"),
      color = "purple"
    )
  })
}


renderDefCentrosEquipo <- function(datos, fasedj, equipo){
  datos <- datos %>% filter(fasedejuego == fasedj, team != equipo)
  
  centros <- sacar_centros(datos)
  
  centros <- centros %>% mutate(x = 100-x, y = 100-y, endX = 100-endX, endY = 100-endY)
  centros <- centros %>%
    mutate(franja = case_when(
      x <= 50 & x >= 15 & y >= 75 ~ "Far Right",
      x < 15 & y >= 75 ~ "Deep Right",
      x <= 50 & x >= 15 & y <= 25 ~ "Far Left",
      x < 15 & y <= 25 ~ "Deep Left"
    ))
  
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligoneiros <- poligonosDef %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 50)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligoneiros, aes(x = x, y = y, group = franja, fill = num_centros), alpha = 0.5, color = "black") +
      
      # Ajustar colores
      scale_fill_gradient2(low = "#3ab54a", high = "red", mid = "yellow", midpoint = max(poligoneiros$num_centros)/2) +
      
      # Añadir etiquetas de porcentaje y número de centros
      geom_text(data = frecuencias, aes(x = c(32.5, 7.5, 32.5, 7.5), y = c(87.5, 87.5, 12.5, 12.5), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}

renderDefFinalizadas3cuartosEquipo <- function(datos, equipo){
  
  equipo_ata <- datos %>% group_by(posesion) %>% filter(any(x >= 75))
  
  posesiones_tiro <- equipo_ata %>% filter(team != equipo) %>% group_by(posesion) %>% summarise(fin = (any(str_detect(type.displayName, "Shot")) | type.displayName == "Goal"))
  
  porc_finalizadas <- (sum(posesiones_tiro$fin)/nrow(posesiones_tiro))*100
  
  renderValueBox({
    valueBox(
      value = formatC(porc_finalizadas, digits = 1, format = "f"),
      subtitle = "% of Attacks in 3/4 ended in Shot",
      icon = icon("shield"),
      color = "purple"
    )
  })
  
}
renderDefCortadas3cuartosEquipo <- function(datos, equipo){
  
  equipo_ata <- datos %>% group_by(posesion) %>% filter(any(x >= 75))
  
  posesiones_tiro <- equipo_ata %>% filter(team != equipo) %>% group_by(posesion) %>% summarise(fin = (any(str_detect(type.displayName, "Shot")) | type.displayName == "Goal"))
  
  
  acciones_def <- which(
    (equipo_ata$type.displayName == "Foul" & equipo_ata$outcomeType.displayName == "Unsuccessful" & equipo_ata$team == equipo) | 
      (equipo_ata$type.displayName == "Dispossessed" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team != equipo) | 
      (equipo_ata$type.displayName == "Aerial" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo & lag(equipo_ata$team) != equipo) |
      (equipo_ata$type.displayName == "BlockedPass" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo) |
      (equipo_ata$type.displayName == "Tackle" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo) |
      (equipo_ata$type.displayName == "Clearance" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo) |
      (equipo_ata$type.displayName == "Interception" & equipo_ata$outcomeType.displayName == "Successful" & equipo_ata$team == equipo)
  )
  
  porc_cortadas <- (length(acciones_def)/nrow(posesiones_tiro))*100
  renderValueBox({
    valueBox(
      value = formatC(porc_cortadas, digits = 1, format = "f"),
      subtitle = "% of Attacks in 3/4 Solved by the Defense",
      icon = icon("shield"),
      color = "purple"
    )
  })
}

renderProtasDefensivo <- function(datos, fasedj, equipo, pos){
  renderUI({
    minutos_jugador <- datos %>%
      filter(team == equipo) %>%
      group_by(name, partido) %>%
      summarise(
        minutos = case_when(
          any(type.displayName == "SubstitutionOff") ~ min(90, max(minute[type.displayName == "SubstitutionOff"])),
          any(type.displayName == "SubstitutionOn") ~ max(0, min(90, 90 - min(minute[type.displayName == "SubstitutionOn"]))),
          TRUE ~ 90
        ),
        .groups = "drop"
      )
    
    
    minutos_por90 <- minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), por90 = sum(minutos)/90) %>% na.omit()
    
    
    acciones <- datos %>% filter((fasedejuego == fasedj) | (lag(fasedejuego) == fasedj) | (lag(fasedejuego, 2) == fasedj), team == equipo, type.displayName %in% c("BallRecovery", "Aerial", "Clearance", "BlockedPass", "Interception", "Claim"), x < 40)
    
    
    tabla_porc <- acciones %>% group_by(name, type.displayName) %>% summarise(exito = sum(outcomeType.displayName == "Successful"),
                                                                              fracaso = sum(outcomeType.displayName == "Unsuccessful"),
                                                                              total = n(),
                                                                              porc_exito = round(exito / (total) * 100, 2)) %>% ungroup()
    
    tabla_porc <- merge(tabla_porc, minutos_por90, by = "name")
    
    tabla_porc <- tabla_porc %>% mutate(exito_90 = round(exito / por90, 2),
                                        fracaso_90 = round(fracaso / por90, 2),
                                        total_90 = round(total / por90, 2))
    
    tabla_porc <- tabla_porc %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2)
    # acciones exitosas
    por_jug <- tabla_porc %>% group_by(name) %>% summarise(acciones = sum(exito_90)) %>% arrange(desc(acciones))
    
    
    
    prueba <- tabla_porc[tabla_porc$name == por_jug$name[pos], ]
    
    nombre_tio <- prueba$name[1]
    
    foto_url <- jugadores %>% filter(name == nombre_tio, team == equipo) %>% select(foto) %>% pull()
    
    if (is.na(foto_url) || length(foto_url) == 0) {
      foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
    }
    
    
    resultados <- vector(mode = "character", length = nrow(prueba))
    resultados2 <- vector(mode = "character", length = nrow(prueba))
    
    for (i in 1:nrow(prueba)) {
      if (prueba$type.displayName[i] == "Aerial") {
        resultados[i] <- paste(prueba$exito[i], "/", prueba$total[i], " (", prueba$porc_exito[i], "%)", sep = "")
        resultados2[i] <- paste(prueba$exito_90[i], "/", prueba$total_90[i], sep = "")
        
      } else {
        resultados[i] <- prueba$total[i]
        resultados2[i] <- prueba$total_90[i]
      }
    }
    
    categorias <- paste0(prueba$type.displayName, collapse = " & ")
    resultados <- paste0(resultados, collapse = " & ")
    categorias2 <- paste0(paste(prueba$type.displayName, "(per90)"), collapse = " & ")
    resultados2 <- paste0(resultados2, collapse = " & ")
    
    # Crear la tabla personalizada
    tabla_html <- htmltools::HTML(
      paste0(
        '<table class="table table-striped" style="width: 100%; border: 3px solid black;">',
        '<thead>',
        '<tr>',
        '<th colspan="5" style="text-align:center; border-bottom: 3px solid black; font-size: 20px; font-family: Arial, sans-serif;">',
        '<div style="display: flex; align-items: center; justify-content: center; margin-bottom: 10px; margin-top: 10px;">',
        '<img src="', foto_url, '" style="width:100px;height:100px; vertical-align: middle; margin-right: 40px;">',
        nombre_tio,
        '</th>',
        '</tr>',
        '</thead>',
        '<tbody>',
        '<tr>',
        '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias), '</td>',
        '</tr>',
        '<tr>',
        '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados), '</td>',
        '</tr>',
        '<tr>',
        '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias2), '</td>',
        '</tr>',
        '<tr>',
        '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados2), '</td>',
        '</tr>',
        '</tbody>',
        '</table>'
      )
    )
    
    htmltools::HTML(tabla_html)
  })
  
}


################ TRANSICIÓN DEFENSIVA
renderPerdidasEquipo <- function(datos, fasedj, equipo){
  renderPlot({
    perdidas <- datos %>% filter(fasedejuego == fasedj, team == equipo) %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                                                                          (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")
    ) 
    
    perdidas$cuadrante_x <- cut(perdidas$x, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
    perdidas$cuadrante_y <- cut(perdidas$y, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)
    
    
    # Contar el número de perdidas en cada cuadrante
    conteo_perdidas <- perdidas %>%
      group_by(cuadrante_x, cuadrante_y) %>%
      summarise(n_perdidas = n()) %>% 
      ungroup()
    
    # Crear un dataframe para los textos de los cuadrantes
    texto_cuadrantes <- expand.grid(
      cuadrante_x = 1:4,
      cuadrante_y = 1:3
    ) %>%
      left_join(conteo_perdidas, by = c("cuadrante_x", "cuadrante_y")) %>%
      mutate(
        x = (cuadrante_x - 1) * 25 + 12.5,
        y = (cuadrante_y - 1) * 33.33 + 16.665
      )
    
    # Graficar el campo de fútbol
    ggplot() +
      geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_perdidas), width = 25, height = 33.33, alpha = 0.5) +
      geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_perdidas), color = "black", size = 5, na.rm = TRUE) +
      scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_perdidas))/2) +
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}

es_perdida_rapida_equipo <- function(datos, tiempo_recuperacion, equipo, pos, partidin) {
  
  eventos_anteriores <- datos %>% 
    filter(partido == partidin,
           team != equipo, 
           posesion != pos, 
           tiempo >= (tiempo_recuperacion - 10),
           tiempo < tiempo_recuperacion,
           type.displayName %in% c("Pass"))
  
  
  return(nrow(eventos_anteriores) > 0)
}

renderPerdidasRapidasEquipo <- function(datos, fasedj, equipo) {
  renderValueBox({
    perdidas <- datos %>% filter(fasedejuego == fasedj, team == equipo) %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                                                                          (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful"))
  
    c_datos <- datos %>% filter(team != equipo)
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    perdidas_rapidas <- perdidas %>% rowwise() %>% mutate(rapida = es_perdida_rapida_equipo(c_datos, minute*60+second, team, posesion, partido))
    perdidas_rapidas <- perdidas_rapidas %>% filter(rapida, ((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                               (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                               (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                               (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")))
    
    
    porc_rapidas <- nrow(perdidas_rapidas) / nrow(perdidas) * 100
    
    
    valueBox(
      value = formatC(porc_rapidas, digits = 1, format = "f"),
      subtitle = "% of Quick Losses After Steal",
      icon = icon("fire"),
      color = "blue"
    )
  })
  
}


renderPerdedoresEquipo <- function(datos, fasedj, equipo) {
  renderUI({
    perdidas <- datos %>% filter(fasedejuego == fasedj, team == equipo) %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                                                                          (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                                                                          (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")
    ) 
    
    perdidas <- perdidas %>% group_by(name) %>% summarise(perdidas = n()) %>% arrange(desc(perdidas))
    
    perdidas <- perdidas[1:min(3, nrow(perdidas)),]
    
    
    html_list <- list()
    
    for (i in 1:nrow(perdidas)) {
      # Obtener la secuencia y el número de repeticiones
      player <- perdidas$name[i]
      losses <- perdidas$perdidas[i]
      
      image_list <- list()
      
      foto_url <- jugadores %>% filter(name == player, team == equipo) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(player, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      player_div <- tags$div(class = "player", img_tag, name_tag)
      image_list <- append(image_list, list(player_div))
      
      row_html <- tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                           do.call(tagList, image_list), 
                           tags$div(style = "margin-left: 20px; font-size: 40px; font-weight: bold; margin-bottom: 20px;", HTML(paste(losses, "Losses"))))
      
      # Agregar la fila al html_list
      html_list[[i]] <- row_html
      
    }
    
    
    # Devolver la lista de filas de HTML
    do.call(tagList, html_list)
  })
}

renderDefPorcVerticalesEquipo <- function(datos, fasedj, equipo){
  renderValueBox({
    transiciones <- datos %>% filter(team != equipo, str_detect(fasedejuego, fasedj)) %>% group_by(partido, posesion) %>% filter(n()>2)
    n_pos <- length(unique(transiciones$posesion))
    
    porc_vert <- transiciones %>% group_by(fasedejuego) %>% summarise(veces = n_distinct(posesion)) %>% mutate(porc_fase = round(veces/n_pos, 2)*100)
    porc_vert <- porc_vert$porc_fase[porc_vert$fasedejuego == "Transicion DEF-AT Vertical"]
    
    
    valueBox(
      value = formatC(porc_vert, digits = 1, format = "f"),
      subtitle = "% Of transitions being VERTICAL",
      icon = icon("angles-right"),
      color = "blue"
    )
  })
}

renderDefPorcFinalizadasEquipo <- function(datos, fasedj, equipo){
  renderValueBox({
    transiciones <- datos %>% filter(team != equipo, str_detect(fasedejuego, fasedj)) %>% group_by(partido, posesion) %>% filter(n()>2)
    
    porc_final <- transiciones %>% group_by(fasedejuego, posesion) %>% 
      summarise(finalizadas = any(str_detect(type.displayName, "Shot") | type.displayName == "Goal")) %>% 
      group_by(fasedejuego) %>% summarise(porc_fin = round(sum(finalizadas)/n_distinct(posesion), 3)*100)
    
    porc_final <- porc_final$porc_fin[porc_final$fasedejuego == "Transicion DEF-AT Vertical"]
    
    valueBox(
      value = formatC(porc_final, digits = 1, format = "f"),
      subtitle = "% Of VERTICAL transitions ended in Shot",
      icon = icon("arrow-right-to-bracket"),
      color = "blue"
    )
  })
}

renderDefContrasVerticalesEquipo <- function(datos, equipo){
  
  renderPlot({
    verticales <- datos %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team != equipo)
    
    lineas_discontinuas <- verticales %>% group_by(partido, posesion) %>% mutate(next_x = lead(x), next_y = lead(y)) %>% select(endX, endY, next_x, next_y, partido, posesion)
    lineas_discontinuas$posesion <- as.factor(lineas_discontinuas$posesion)
    
    shots <- verticales %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal")
    shots <- shots %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    verticales$posesion <- as.factor(verticales$posesion)
    posesion_colors <- viridis::viridis_pal(option = "D")(length(unique(verticales$posesion)))
    
    
    verticales <- verticales %>% mutate(x = 100-x, y = 100-y, endX = 100-endX, endY = 100-endY)
    lineas_discontinuas <- lineas_discontinuas %>% mutate(endX = 100-endX, endY = 100-endY, next_x = 100-next_x, next_y = 100-next_y)
    shots <- shots %>% mutate(x = 100-x, y = 100-y)
    
    
    ggplot() +
      geom_segment(data = verticales, aes(x = x, y = y, xend = endX, yend = endY, color = posesion),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      geom_segment(data = lineas_discontinuas, aes(x = endX, y = endY, xend = next_x, yend = next_y, color = posesion), linetype = "dashed") +
      
      scale_color_manual(values = posesion_colors) +
      
      geom_point(data = shots, aes(x = x, y = y, fill = esgol), shape = 21, size = 3, color = "black") + 
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      
      annotate_pitch(colour = "white", fill = "#3ab54a", alpha = 0.1) +
      scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100)) +
      theme_pitch() + theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
  
}

############### ABP

##### A FAVOR
renderCornersDerEquipo <- function(datos, equipo, ipt, sev_otro){
  
  renderPlot({
    corners_der <- datos %>% filter(team == equipo, fasedejuego == "ABP", x > 99, y < 1, name %in% ipt[[paste0("jugadores_cornerd_", sev_otro)]])
    
    cd <- datos %>% filter(team == equipo, fasedejuego == "ABP") %>% group_by(partido, posesion) %>% filter(any(x > 99), any(y < 1), any(name %in% ipt[[paste0("jugadores_cornerd_", sev_otro)]]), type.displayName!= "Aerial")
    
    disparos <- cd %>% mutate(pase_x = lag(x), pase_y = lag(y)) %>%  filter((str_detect(type.displayName, "Shot") | type.displayName == "Goal"), between(x, 85, 99), between(y, 25, 75))
    disparos <- disparos %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    pases_zona <- disparos %>% filter(pase_x < 99, pase_y > 1)
    
    
    corners_der <- corners_der %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "1st Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "2nd Post",
        endY < 15 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_der %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 0, 0, 15, 15),
      palo = rep(c("1st Post", "Center", "2nd Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("1st Post", "Center", "2nd Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(75, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_der, aes(x = x, y = y), color = "black", size = 3) +
      
      geom_point(data = disparos, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_segment(data = pases_zona, aes(x = pase_x, y = pase_y, xend = x, yend = y), linetype = "dashed") +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(88.75, 88.75, 88.75, 88.75), y = c(35, 50, 65, 8), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(96.25, 96.25, 96.25), y = c(35, 50, 65), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      
      ggrepel::geom_label_repel(
        data = disparos,
        aes(x = x, y = y, label=name), alpha = 0.5,
        size=3) +
      
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderCornersIzqEquipo <- function(datos, equipo, ipt, sev_otro){
  
  renderPlot({
    corners_izq <- datos %>% filter(team == equipo, fasedejuego == "ABP", x > 99, y > 99, name %in% ipt[[paste0("jugadores_corneri_", sev_otro)]])
    
    ci <- datos %>% filter(team == equipo, fasedejuego == "ABP") %>% group_by(partido, posesion) %>% filter(any(x > 99), any(y > 99), any(name %in% ipt[[paste0("jugadores_corneri_", sev_otro)]]), type.displayName != "Aerial")
    
    disparos <- ci %>% mutate(pase_x = lag(x), pase_y = lag(y)) %>% filter((str_detect(type.displayName, "Shot") | type.displayName == "Goal"), between(x, 85, 99), between(y, 25, 75))
    disparos <- disparos %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    pases_zona <- disparos %>% filter(pase_x < 99, pase_y < 99)
    
    corners_izq <- corners_izq %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "2nd Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "1st Post",
        endY > 85 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("2nd Post", "Center", "1st Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_izq %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 100, 100, 85, 85),
      palo = rep(c("2nd Post", "Center", "1st Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("2nd Post", "Center", "1st Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(75, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_izq, aes(x = x, y = y), color = "black", size = 3) +
      
      geom_point(data = disparos, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_segment(data = pases_zona, aes(x = pase_x, y = pase_y, xend = x, yend = y), linetype = "dashed") +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(88.75, 88.75, 88.75, 88.75), y = c(35, 50, 65, 92), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(96.25, 96.25, 96.25), y = c(35, 50, 65), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      
      ggrepel::geom_label_repel(
        data = disparos,
        aes(x = x, y = y, label=name), alpha = 0.5,
        size=3) +
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderFaltasCentroEquipo <- function(datos, equipo) {
  faltas <- datos %>% filter(team == equipo, fasedejuego == "ABP") %>% 
    group_by(partido, posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(partido, posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  
  # Bandas y centros
  centros_seq <- entiro %>% filter(n()>1, type.displayName == "Pass") 
  
  centros <- entiro %>% filter(n()>1, type.displayName == "Pass") %>% summarise(x = first(x), y = first(y))
  remates <- entiro %>% filter(n()>1, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  
  remates <- remates %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  centros <- centros %>%
    mutate(franja = case_when(
      between(y, 0, 20) ~ "Right",
      between(y, 80, 100) ~ "Left",
      between(y, 20, 80) ~ "Center"
    ))
  
  # Añadir frecuencias al data frame original
  todas_franjas <- data.frame(
    franja = c("Right", "Left", "Center")
  )
  
  # Calcular frecuencias
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligonos <- data.frame(
    x = c(50, 100, 100, 50, 50, 100, 100, 50, 82, 50, 50, 82),
    y = c(0, 0, 20, 20, 80, 80, 100, 100, 20, 20, 80, 80),
    franja = rep(c("Right", "Left", "Center"), each = 4)
  )
  
  poligonos <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(50, 100)) +
      
      # centros
      geom_polygon(data = poligonos, aes(x = x, y = y, group = franja, fill = rescale(num_centros)), alpha = 0.5, color = "black") +
      scale_fill_gradient2(high = "red3", low = "yellow3", mid = "orange3", midpoint = 0.5) +
      
      
      geom_point(data = centros_seq, aes(x = x, y = y), color = "black", size = 3) +
      
      
      geom_segment(data = centros_seq, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      # remates
      geom_point(data = remates, aes(x = x, y = y, color = esgol), size = 3) +
      
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_text(data = frecuencias, aes(x = c(75, 75, 65), y = c(10, 90, 50), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      ggrepel::geom_label_repel(
        data = centros_seq,
        aes(x = x, y = y, label=name), alpha = 0.5, 
        size=3) +
      
      ggrepel::geom_label_repel(
        data = remates,
        aes(x = x, y = y, label=name), alpha = 0.5, 
        size=3) +
      
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}

renderLibreDirectoEquipo <- function(datos, equipo) {
  faltas <- datos %>% filter(team == equipo, fasedejuego == "ABP") %>% 
    group_by(partido, posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(partido, posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  libdir <- entiro %>% filter(n()==1, x < 82, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  libdir <- libdir %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(50, 100)) +
      
      geom_point(data = libdir, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_segment(data = libdir, aes(x = x, y = y, xend = ifelse(is.na(blockedX), 100, blockedX), yend = ifelse(is.na(blockedY), goalMouthY, blockedY)),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      ggrepel::geom_label_repel(
        data = libdir,
        aes(x = x, y = y, label=name), alpha = 0.5, 
        size=3) +
      
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}

renderPenalesEquipo <- function(datos, equipo) {
  penales <- datos %>% filter(team == equipo, fasedejuego == "ABP", x == 88.5, y == 50)
  penales <- penales %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  renderPlot({
    porteria <- data.frame(
      x = c(0, 0, 10, 10),
      y = c(0, 4, 4, 0)
    )
    
    ggplot() +
      # Dibujar la portería
      geom_path(data = porteria, aes(x = x, y = y), color = "black", size = 1) +
      # Añadir los tiros
      geom_point(data = penales, aes(x = 55-goalMouthY, y = goalMouthZ/10, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      ggrepel::geom_label_repel(
        data = penales,
        aes(x = 55-goalMouthY, y = goalMouthZ/10, label=name),
        size=3) +
      
      # Configurar los límites del gráfico
      coord_fixed(ratio = 1) +
      xlim(-1, 11) +
      ylim(0, 5) +
      
      theme_void() + theme(legend.position = "none") + guides(fill = "none") 
    
  })
}


##### EN CONTRA
renderDefCornersDerEquipo <- function(datos, equipo){
  
  renderPlot({
    corners_der <- datos %>% filter(team != equipo, fasedejuego == "ABP", x > 99, y < 1)
    
    cd <- datos %>% filter(team != equipo, fasedejuego == "ABP") %>% group_by(partido, posesion) %>% filter(any(x > 99), any(y < 1))
    
    disparos <- cd %>% filter(type.displayName == "Goal")
    disparos <- disparos %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    
    corners_der <- corners_der %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "1st Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "2nd Post",
        endY < 15 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_der %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 0, 0, 15, 15),
      palo = rep(c("1st Post", "Center", "2nd Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("1st Post", "Center", "2nd Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    
    corners_der <- corners_der %>% mutate(x = 100-x, y = 100-y)
    poligonos_envios <- poligonos_envios %>% mutate(x = 100-x, y = 100-y)
    poligonos_efectividad <- poligonos_efectividad %>% mutate(x = 100-x, y = 100-y)
    
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 25)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_der, aes(x = x, y = y), color = "black", size = 3) +
      
      geom_point(data = disparos, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen")) +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(11.25, 11.25, 11.25, 11.25), y = c(65, 50, 35, 92), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(3.75, 3.75, 3.75), y = c(65, 50, 35), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderDefCornersIzqEquipo <- function(datos, equipo){
  
  renderPlot({
    corners_izq <- datos %>% filter(team != equipo, fasedejuego == "ABP", x > 99, y > 99)
    
    ci <- datos %>% filter(team != equipo, fasedejuego == "ABP") %>% group_by(partido, posesion) %>% filter(any(x > 99), any(y > 99))
    
    disparos <- ci %>%  filter(type.displayName == "Goal")
    disparos <- disparos %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
    
    
    corners_izq <- corners_izq %>%
      mutate(palo = case_when(
        between(endY, 25, 45) ~ "2nd Post",
        between(endY, 45, 55) ~ "Center",
        between(endY, 55, 75) ~ "1st Post",
        endY > 85 ~ "Short",
        TRUE ~ "Lost"
      ))
    
    # Añadir frecuencias al data frame original
    todos_palos <- tibble(
      palo = c("1st Post", "Center", "2nd Post", "Short", "Lost")
    )
    
    # Calcular frecuencias
    frecuencias <- corners_izq %>%
      group_by(palo) %>%
      summarise(num_envios = n(),
                buenos_envios = sum(outcomeType.displayName == "Successful")) %>%
      mutate(porcentaje = num_envios / sum(num_envios) * 100,
             efectividad_centros = buenos_envios / num_envios * 100)
    
    
    # Asegurarse de que todas las franjas estén presentes
    frecuencias <- todos_palos %>%
      left_join(frecuencias, by = "palo") %>%
      replace_na(list(num_envios = 0, buenos_envios = 0, porcentaje = 0, efectividad_centros = 0))
    
    frecuencias <- frecuencias[1:4, ]
    
    poligonos_envios <- data.frame(
      x = c(85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85, 85, 92.5, 92.5, 85),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75, 100, 100, 85, 85),
      palo = rep(c("2nd Post", "Center", "1st Post", "Short"), each = 4)
    )
    
    poligonos_envios <- poligonos_envios %>%
      left_join(frecuencias, by = "palo")
    
    
    poligonos_efectividad <- data.frame(
      x = c(92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5, 92.5, 100, 100, 92.5),
      y = c(25, 25, 45, 45, 45.1, 45.1, 55, 55, 55.1, 55.1, 75, 75),
      palo = rep(c("2nd Post", "Center", "1st Post"), each = 4)
    )
    
    poligonos_efectividad <- poligonos_efectividad %>%
      left_join(frecuencias, by = "palo")
    
    corners_izq <- corners_izq %>% mutate(x = 100-x, y = 100-y)
    poligonos_envios <- poligonos_envios %>% mutate(x = 100-x, y = 100-y)
    poligonos_efectividad <- poligonos_efectividad %>% mutate(x = 100-x, y = 100-y)
    
    
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 25)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligonos_envios, aes(x = x, y = y, group = palo, fill = rescale(num_envios)), alpha = 0.9, color = "black") +
      geom_polygon(data = poligonos_efectividad, aes(x = x, y = y, group = palo, fill = rescale(efectividad_centros)), alpha = 0.9, color = "black") +
      
      scale_fill_gradient2(high = "green3", low = "red3", mid = "yellow3", midpoint = 0.4) +
      
      # Añadir los centros
      geom_point(data = corners_izq, aes(x = x, y = y), color = "black", size = 3) +
      
      geom_point(data = disparos, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen")) +
      
      # Añadir etiquetas de porcentaje y número de envios
      geom_text(data = frecuencias, aes(x = c(11.25, 11.25, 11.25, 11.25), y = c(35, 50, 65, 8), 
                                        label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      geom_text(data = frecuencias[1:3,], aes(x = c(3.75, 3.75, 3.75), y = c(35, 50, 65), 
                                              label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
                color = "black", size = 3, fontface = "bold") +
      
      
      # Configurar el tema
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
}

renderDefFaltasCentroEquipo <- function(datos, equipo) {
  faltas <- datos %>% filter(team != equipo, fasedejuego == "ABP") %>% 
    group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(partido, posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  
  # Bandas y centros
  centros_seq <- entiro %>% filter(n()>1, type.displayName == "Pass") 
  
  centros <- entiro %>% filter(n()>1, type.displayName == "Pass") %>% summarise(x = first(x), y = first(y))
  remates <- entiro %>% filter(n()>1, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  
  remates <- remates %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  centros <- centros %>%
    mutate(franja = case_when(
      between(y, 0, 20) ~ "Right",
      between(y, 80, 100) ~ "Left",
      between(y, 20, 80) ~ "Center"
    ))
  
  # Añadir frecuencias al data frame original
  todas_franjas <- data.frame(
    franja = c("Right", "Left", "Center")
  )
  
  # Calcular frecuencias
  frecuencias <- centros %>%
    group_by(franja) %>%
    summarise(num_centros = n()) %>%
    mutate(porcentaje = num_centros / sum(num_centros) * 100)
  
  # Asegurarse de que todas las franjas estén presentes
  frecuencias <- todas_franjas %>%
    left_join(frecuencias, by = "franja") %>%
    replace_na(list(num_centros = 0, porcentaje = 0))
  
  poligonos <- data.frame(
    x = c(50, 100, 100, 50, 50, 100, 100, 50, 82, 50, 50, 82),
    y = c(0, 0, 20, 20, 80, 80, 100, 100, 20, 20, 80, 80),
    franja = rep(c("Right", "Left", "Center"), each = 4)
  )
  
  poligonos <- poligonos %>%
    left_join(frecuencias, by = "franja")
  
  poligonos <- poligonos %>% mutate(x = 100-x, y = 100-y)
  centros_seq <- centros_seq %>% mutate(x = 100-x, y = 100-y, endX = 100-endX, endY = 100-endY)
  remates <- remates %>% mutate(x = 100-x, y = 100-y)
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 50)) +
      
      # centros
      geom_polygon(data = poligonos, aes(x = x, y = y, group = franja, fill = rescale(num_centros)), alpha = 0.5, color = "black") +
      scale_fill_gradient2(high = "red3", low = "yellow3", mid = "orange3", midpoint = 0.5) +
      
      
      geom_point(data = centros_seq, aes(x = x, y = y), color = "black", size = 3) +
      
      
      geom_segment(data = centros_seq, aes(x = x, y = y, xend = endX, yend = endY),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      # remates
      geom_point(data = remates, aes(x = x, y = y, color = esgol), size = 3) +
      
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_text(data = frecuencias, aes(x = c(25, 25, 35), y = c(90, 10, 50), 
                                        label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
                color = "black", size = 4, fontface = "bold") +
      
      
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}

renderDefLibreDirectoEquipo <- function(datos, equipo) {
  faltas <- datos %>% filter(team == equipo, fasedejuego == "ABP") %>% 
    group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)
  
  
  # FALTAS QUE ACABEN EN TIRO
  entiro <- faltas %>% group_by(partido, posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))
  
  libdir <- entiro %>% filter(n()==1, x < 82, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
  libdir <- libdir %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  libdir <- libdir %>% mutate(x = 100-x, y = 100-y)
  
  renderPlot({
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(0, 50)) +
      
      geom_point(data = libdir, aes(x = x, y = y, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      geom_segment(data = libdir, aes(x = x, y = y, xend = ifelse(is.na(blockedX), 0, 100-blockedX), yend = ifelse(is.na(blockedY), 100-goalMouthY, 100-blockedY)),
                   arrow = arrow(length = unit(0.2, "cm"),
                                 type = "open")) +
      
      theme_pitch() +
      theme_void() + theme(legend.position = "none") + guides(fill = "none") +
      theme(panel.background = element_rect(fill = "#3ab54a")) 
    
  })
  
  
}

renderDefPenalesEquipo <- function(datos, equipo) {
  penales <- datos %>% filter(team != equipo, fasedejuego == "ABP", x == 88.5, y == 50)
  penales <- penales %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))
  
  
  renderPlot({
    porteria <- data.frame(
      x = c(0, 0, 10, 10),
      y = c(0, 4, 4, 0)
    )
    
    ggplot() +
      # Dibujar la portería
      geom_path(data = porteria, aes(x = x, y = y), color = "black", size = 1) +
      # Añadir los tiros
      geom_point(data = penales, aes(x = 55-goalMouthY, y = goalMouthZ/10, color = esgol), size = 3) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
      
      ggrepel::geom_label_repel(
        data = penales,
        aes(x = 55-goalMouthY, y = goalMouthZ/10, label=name),
        size=3) +
      
      # Configurar los límites del gráfico
      coord_fixed(ratio = 1) +
      xlim(-1, 11) +
      ylim(0, 5) +
      
      theme_void() + theme(legend.position = "none") + guides(fill = "none") 
    
  })
}


renderControlPorteria <- function(){
  
  renderPlot({
  
    
    poligono_por <- data.frame(
      x = c(95, 100, 100, 95),
      y = c(40, 40, 60, 60)
    )
    
    ggplot() +
      annotate_pitch(colour = "white", fill = "#3ab54a") +
      theme_pitch() +
      coord_cartesian(xlim = c(82, 100)) +
      
      # Añadir los polígonos de las franjas
      geom_polygon(data = poligono_por, aes(x = x, y = y), alpha = 0.5, color = "black") +
      
      
      # Configurar el tema
      theme_pitch() + theme_void() + theme(legend.position = "none") + 
      theme(panel.background = element_rect(fill = "#3ab54a"))
    
  })
}



#############################################################################
#############################################################################
### APP
ui <- fluidPage(
  useShinyjs(),
    list(tags$head(HTML('<link rel="icon", href="https://seeklogo.com/images/S/sevilla-fc-logo-0D80FA88A5-seeklogo.com.png", 
                                   type="image/png" />
                        <style>
      .tooltip-icon {
        display: inline-block;
        margin-left: 5px;
        cursor: pointer;
        position: relative;
        width: 20px;
        height: 20px;
        border-radius: 50%;
        background-color: blue;
        color: white;
        text-align: center;
        line-height: 20px;
        font-weight: bold;
      }
      .tooltip-text {
        visibility: hidden;
        width: 200px;
        background-color: black;
        color: #fff;
        text-align: center;
        border-radius: 5px;
        padding: 5px;
        position: absolute;
        z-index: 1;
        bottom: 125%; /* Position the tooltip above the icon */
        left: 50%;
        margin-left: -100px;
        opacity: 0;
        transition: opacity 0.3s;
      }
      .tooltip-icon:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
      }
    </style>'))),
  div(style="padding: 10px 0px; width: '30%'",
      titlePanel(
        title="", windowTitle="Sevilla FC Analysis"
      )
  ),
  dashboardPage(skin = "red",
                dashboardHeader(title = div(img(src="https://seeklogo.com/images/S/sevilla-fc-logo-0D80FA88A5-seeklogo.com.png", style = "width:5%; height:5%"),
                                            "Game Analyst Tool"), titleWidth = "40%"),
                dashboardSidebar(
                  sidebarMenu(id = "sidebarid",
                              menuItem("Match Centre", tabName = "match_centre", icon = icon("futbol"),
                                       menuSubItem("Events Display", tabName = "eventing"),
                                       menuSubItem("Local Team Behaviour", tabName = "local_behave"),
                                       menuSubItem("Away Team Behaviour", tabName = "away_behave")),
                              
                              menuItem("Team Analysis", tabName = "team_analysis", icon = icon("futbol"),
                                       menuSubItem("Offensive Organization", tabName = "off_org"),
                                       menuSubItem("Defense-Offense Transition", tabName = "off_tr"),
                                       menuSubItem("Defensive Organization", tabName = "def_org"),
                                       menuSubItem("Offense-Defense Transition", tabName = "def_tr"),
                                       menuSubItem("Set Piece", tabName = "abp")),
                              
                              menuItem("Opponent Analysis", tabName = "opp_analysis", icon = icon("futbol"),
                                       menuSubItem("Offensive Organization", tabName = "off_org2"),
                                       menuSubItem("Defense-Offense Transition", tabName = "off_tr2"),
                                       menuSubItem("Defensive Organization", tabName = "def_org2"),
                                       menuSubItem("Offense-Defense Transition", tabName = "def_tr2"),
                                       menuSubItem("Set Piece", tabName = "abp2"))
                              
                            
                  )),
                dashboardBody(
                  tabItems(
                    tabItem("eventing", fluidRow(
                      column(8, uiOutput("texto_mosaico")),
                      column(4, uiOutput("texto_foto")),
                      column(8, lapply(1:length(imagenes_opciones), function(imagen) {
                        tags$button(id = paste0("liga_", imagen), class = "btn action-button", img(src = imagenes_opciones[imagen], height = '100',  style = "padding-bottom:20px;border-radius: 0px;
                   border-width: 20px"))
                      })
                      ),
                      column(4, plotOutput("myImage", height = 200, width = 200)),
                      column(12, uiOutput("selector_partido")),
                      box(
                        fluidRow(
                          column(6, plotOutput("img_local", height = 200, width = 200)),
                          column(6, uiOutput("marc_loc")),
                          column(12, uiOutput("goles_loc")),
                          column(12, actionButton("ir_a_local_behave", "Team Behaviour", class = "btn btn-primary"))
                        )
                      ),
                      box(
                        fluidRow(
                          column(6, plotOutput("img_visitante", height = 200, width = 200)),
                          column(6, uiOutput("marc_vis")),
                          column(12, uiOutput("goles_vis")),
                          column(12, actionButton("ir_a_away_behave", "Team Behaviour", class = "btn btn-primary"))
                          
                        )
                      )
                    )),
                    
                    tabItem("local_behave", fluidRow(
                      
                      column(12, lapply(c("OFFENSIVE ORGANIZATION", "DEFENSE - OFFENSE TRANSITION", "OFFENSE - DEFENSE TRANSITION", "DEFENSIVE ORGANIZATION", "SET PIECE"), function(fase) {
                        tags$button(id = paste0("local_", fase), class = "btn action-button", style = "width: 181px; height: 40px; font-weight: bold; border-width: 2.5px; border-color: black; margin-bottom: 5px; font-size:11px;", fase)})),
                    
                      column(5, plotOutput("img_local2", height = 350, width = 350)),
                      
                      uiOutput("main_content_local")
                      
                    )),
                    
                    tabItem("away_behave", fluidRow(
                      column(12, lapply(c("OFFENSIVE ORGANIZATION", "DEFENSE - OFFENSE TRANSITION", "OFFENSE - DEFENSE TRANSITION", "DEFENSIVE ORGANIZATION", "SET PIECE"), function(fase) {
                        tags$button(id = paste0("visitante_", fase), class = "btn action-button", style = "width: 181px; height: 40px; font-weight: bold; border-width: 2.5px; border-color: black; margin-bottom: 5px; font-size:11px;", fase)})),
                      
                      column(5, plotOutput("img_visitante2", height = 350, width = 350)),
                      
                      uiOutput("main_content_visitante")
                    )),
                    
                    tabItem("off_org", fluidRow(
                      box(
                      column(2, plotOutput("img_sevilla_of", height = 75, width = 75)),
                      column(4, dateRangeInput("selector_fechas_of", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                      
                      column(6, uiOutput("texto_fase_of")), width = 12
                      ),
                      column(12, selectInput("selector_competi_of", "Competition", choices = c("España-LaLiga", "Europa-Champions-League"))),
                      column(12, uiOutput("partidos_of")),
                      
                      column(6, uiOutput("texto_saques_sevilla")),
                      column(6, uiOutput("texto_defensa_sevilla")),
                      
                      box(
                      column(6, plotOutput("saquesportero_sev", height = 300, width = 400))
                      ),
                      
                      box(
                      column(6, plotOutput("salidasdefensa_sev", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_medio_sevilla")),
                      column(6, uiOutput("texto_tirosfda_sevilla")),
                      
                      box(
                      column(6, plotOutput("salidasmedio_sev", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("tiros_fda_sevilla", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_tirosarea_sevilla")),
                      column(6, uiOutput("texto_finalizadores_sevilla")),
                      box(
                      column(6, plotOutput("tiros_area_sevilla", height = 300, width = 400))
                      ),
                      column(6, uiOutput("finalizadores_sevilla")),
                      
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_centros_sevilla")),
                      column(6, uiOutput("texto_centradores_sevilla")),
                      box(
                      column(6, plotOutput("centros_sevilla", height = 300, width = 400))
                      ),
                      column(6, uiOutput("centradores_sevilla")),
                      
                      column(12, uiOutput("texto_carriles_sevilla")),
                      box(
                      column(12, plotOutput("carriles_sevilla", height = 300, width = 400))
                      ),
                      
                      column(12, renderUI({NULL})),
                      
                      valueBoxOutput("mediapases_sevilla"),
                      valueBoxOutput("verticalidad_sevilla"),
                      valueBoxOutput("porc_goles_faseof_sevilla"),
                      
                      
                      column(6, uiOutput("texto_protagoles_sevilla")),
                      column(6, uiOutput("texto_tirosjugador_sevilla")),
                      column(6, uiOutput("prota_goles")),
                      box(
                      column(6, plotOutput("goles_jugador", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_protacentros_sevilla")),
                      column(6, uiOutput("texto_centrosjugador_sevilla")),
                      column(6, uiOutput("prota_centros")),
                      box(
                      column(6, plotOutput("centros_jugador", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_protacambios_sevilla")),
                      column(6, uiOutput("texto_cambiosjugador_sevilla")),
                      column(6, fluidRow(
                      column(12, uiOutput("prota_cambios")),
                      column(12, materialSwitch("excluir_portero_sevilla", label = "Exclude Goalkeeper", status = "primary", right = TRUE))
                      )),
                      box(
                      column(6, plotOutput("cambios_jugador", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_protaasistencias_sevilla")),
                      column(6, uiOutput("texto_asistenciasjugador_sevilla")),
                      column(6, uiOutput("prota_asistencias")),
                      box(
                      column(6, plotOutput("asistencias_jugador", height = 300, width = 400))
                      )
                      
                      
                    )),
                    
                    tabItem("def_org", fluidRow(
                      box(
                        column(2, plotOutput("img_sevilla_def", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_def", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_def")), width = 12
                      ),
                      column(12, selectInput("selector_competi_def", "Competition", choices = c("España-LaLiga", "Europa-Champions-League"))),
                      
                      column(12, uiOutput("partidos_def")),
                      
                      column(6, uiOutput("texto_deftirosarea_sevilla")),
                      column(6, uiOutput("texto_deftirosfda_sevilla")),
                      box(
                      column(6, plotOutput("deftirosarea_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("deftirosfda_sevilla", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("defgoles_sevilla"),
                      
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_defcarriles_sevilla")),
                      column(6, uiOutput("texto_defcentros_sevilla")),
                      box(
                      column(6, plotOutput("defcarriles_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("defcentros_sevilla", height = 300, width = 400))
                      ),
                      valueBoxOutput("defppda_sevilla"),
                      valueBoxOutput("deffinalizadas34_sevilla"),
                      valueBoxOutput("defcortadas34_sevilla"),
                      
                      column(12, uiOutput("texto_protadefensivo_sevilla")),
                      column(6, uiOutput("protadefensivo_sevilla1")),
                      column(6, uiOutput("protadefensivo_sevilla2")),
                      column(6, uiOutput("protadefensivo_sevilla3")),
                      column(6, uiOutput("protadefensivo_sevilla4"))
                      
                    )),
                    
                    tabItem("off_tr", fluidRow(
                      box(
                        column(2, plotOutput("img_sevilla_tr_da", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_tr_da", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_tr_da")), width = 12
                      ),
                      column(12, selectInput("selector_competi_tr_da", "Competition", choices = c("España-LaLiga", "Europa-Champions-League"))),
                      
                      column(12, uiOutput("partidos_tr_da")),
                      
                      column(6, uiOutput("texto_robos_sevilla")),
                      column(6, uiOutput("texto_recuperadores_sevilla")),
                      box(
                      column(6, plotOutput("robos_sevilla", height = 300, width = 400))
                      ),
                      column(6, uiOutput("recuperadores_sevilla")),
                      
                      column(12, renderUI({NULL})),
                      valueBoxOutput("robosrapidos_sevilla"),
                      column(5, uiOutput("ladron_rapido_sevilla")),
                      
                      column(12, renderUI({NULL})),
                      
                      column(12, uiOutput("texto_carrilestrans_sevilla")),
                      box(
                      column(12, plotOutput("carrilestrans_sevilla", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_tirosfdatrans_sevilla")),
                      column(6, uiOutput("texto_tirosareatrans_sevilla")),
                      box(
                        column(6, plotOutput("tiros_fdatrans_sevilla", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("tiros_areatrans_sevilla", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_protagolestrans_sevilla")),
                      column(6, uiOutput("texto_tirosjugadortrans_sevilla")),
                      column(6, uiOutput("prota_golestrans_sevilla")),
                      box(
                        column(6, plotOutput("goles_jugadortrans_sevilla", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("porc_goles_trans_sevilla"),
                      
                      column(12, renderUI({NULL})),
                      
                      
                      column(12, uiOutput("texto_contrasverticales_sevilla")),
                      box(
                      column(12, plotOutput("contrasverticales_sevilla", height = 300, width = 400))
                      ),
                      valueBoxOutput("porcverticales_sevilla"),
                      valueBoxOutput("porcfinalizadas_sevilla"),
                      valueBoxOutput("porc_goles_transvert_sevilla"),
                      
                      
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_recuperador_vertical")),
                      column(6, uiOutput("texto_finalizador_vertical")),
                      column(6, uiOutput("recuperador_vertical")),
                      column(6, uiOutput("finalizador_vertical")),
                      
                      
                    )),
                    
                    tabItem("def_tr", fluidRow(
                      box(
                        column(2, plotOutput("img_sevilla_tr_ad", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_tr_ad", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_tr_ad")), width = 12
                      ),
                      column(12, selectInput("selector_competi_tr_ad", "Competition", choices = c("España-LaLiga", "Europa-Champions-League"))),
                      
                      column(12, uiOutput("partidos_tr_ad")),
                      
                      
                      column(6, uiOutput("texto_perdidas_sevilla")),
                      column(6, uiOutput("texto_perdedores_sevilla")),
                      box(
                      column(6, plotOutput("perdidas_sevilla", height = 300, width = 400))
                      ),
                      column(6, uiOutput("perdedores_sevilla")),
                      
                      column(12, renderUI({NULL})),
                      valueBoxOutput("perdidasrapidas_sevilla"),
                      column(5, uiOutput("perdedor_rapido_sevilla")),
                      column(12, renderUI({NULL})),
                      
                      column(12, uiOutput("texto_defcarrilestrans_sevilla")),
                      box(
                      column(12, plotOutput("defcarrilestrans_sevilla", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_deftirosareatrans_sevilla")),
                      column(6, uiOutput("texto_deftirosfdatrans_sevilla")),
                      box(
                      column(6, plotOutput("deftirosareatrans_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("deftirosfdatrans_sevilla", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      valueBoxOutput("defgolestrans_sevilla"),
                      
                      
                      column(12, renderUI({NULL})),
                      
                      
                      column(12, uiOutput("texto_defcontrasverticales_sevilla")),
                      box(
                      column(12, plotOutput("defcontrasverticales_sevilla", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("defporcverticales_sevilla"),
                      valueBoxOutput("defporcfinalizadas_sevilla"),
                      valueBoxOutput("defgolesvertical_sevilla")
                      
                      
                    )),
                    
                    tabItem("abp", fluidRow(
                      box(
                        column(2, plotOutput("img_sevilla_abp", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_abp", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_abp")), width = 12
                      ),
                      column(12, selectInput("selector_competi_abp", "Competition", choices = c("España-LaLiga", "Europa-Champions-League"))),
                      
                      column(12, uiOutput("partidos_abp")),
                      
                      column(12, uiOutput("texto_abpfavor_sevilla")),
                      
                      column(6, uiOutput("texto_cornerder_sevilla")),
                      column(6, uiOutput("texto_cornerizq_sevilla")),
                      
                      column(6, uiOutput("jugadores_cornerd_sev")),
                      column(6, uiOutput("jugadores_corneri_sev")),
                      box(
                      column(6, plotOutput("cornerder_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("cornerizq_sevilla", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_faltascentro_sevilla")),
                      column(6, uiOutput("texto_libredirecto_sevilla")),
                      
                      box(
                      column(6, plotOutput("faltascentro_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("libredirecto_sevilla", height = 300, width = 400))
                      ),
                      
                      column(12, uiOutput("texto_penales_sevilla")),
                      box(
                      column(12, plotOutput("penales_sevilla", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("porc_goles_abp_sevilla"),
                      
                      column(12, renderUI({NULL})),
                      
                      column(12, uiOutput("texto_abpcontra_sevilla")),
                      
                      
                      column(6, uiOutput("texto_defcornerder_sevilla")),
                      column(6, uiOutput("texto_defcornerizq_sevilla")),
                      
                      box(
                      column(6, plotOutput("defcornerder_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("defcornerizq_sevilla", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_deffaltascentro_sevilla")),
                      column(6, uiOutput("texto_deflibredirecto_sevilla")),
                      
                      box(
                      column(6, plotOutput("deffaltascentro_sevilla", height = 300, width = 400))
                      ),
                      box(
                      column(6, plotOutput("deflibredirecto_sevilla", height = 300, width = 400))
                      ),
                      
                      column(12, uiOutput("texto_defpenales_sevilla")),
                      box(
                      column(12, plotOutput("defpenales_sevilla", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("porc_goles_defabp_sevilla"),
                      
                      column(12, renderUI({NULL})),
                      column(12, uiOutput("texto_protadefabp_sevilla")),
                      
                      column(6, uiOutput("protadefabp_sevilla1")),
                      column(6, uiOutput("protadefabp_sevilla2")),
                      
                      column(6, fluidRow(
                        column(12, uiOutput("portero_control")),
                        column(12, uiOutput("porccontrol_por_sevilla"))
                      )),
                      column(6, fluidRow(
                        column(12, uiOutput("texto_zonadecontrol_por_sevilla")),
                        column(12, plotOutput("zonadecontrol_por_sevilla", height = 300, width = 150))
                      ))
                      
                    )),
                    
                    ##################################################
                    ################## EQUIPO RIVAL ################## 
                    ##################################################
                    
                    tabItem("off_org2", fluidRow(
                      column(6, selectInput("selector_competi_rival_of", "Competition", choices = competis)),
                      column(6, uiOutput("selector_equipo_rival_of")),
                      
                      box(
                        column(2, plotOutput("img_rival_of", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_of2", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_of2")), width = 12
                      ),
                      column(12, uiOutput("partidos_of2")),
                      
                      column(6, uiOutput("texto_saques_rival")),
                      column(6, uiOutput("texto_defensa_rival")),
                      
                      box(
                        column(6, plotOutput("saquesportero_rival", height = 300, width = 400))
                      ),
                      
                      box(
                        column(6, plotOutput("salidasdefensa_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_medio_rival")),
                      column(6, uiOutput("texto_tirosfda_rival")),
                      
                      box(
                        column(6, plotOutput("salidasmedio_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("tiros_fda_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_tirosarea_rival")),
                      column(6, uiOutput("texto_finalizadores_rival")),
                      box(
                        column(6, plotOutput("tiros_area_rival", height = 300, width = 400))
                      ),
                      column(6, uiOutput("finalizadores_rival")),
                      
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_centros_rival")),
                      column(6, uiOutput("texto_centradores_rival")),
                      box(
                        column(6, plotOutput("centros_rival", height = 300, width = 400))
                      ),
                      column(6, uiOutput("centradores_rival")),
                      
                      column(12, uiOutput("texto_carriles_rival")),
                      box(
                        column(12, plotOutput("carriles_rival", height = 300, width = 400))
                      ),
                      
                      column(12, renderUI({NULL})),
                      
                      valueBoxOutput("mediapases_rival"),
                      valueBoxOutput("verticalidad_rival"),
                      valueBoxOutput("porc_goles_faseof_rival"),
                      
                      
                      column(6, uiOutput("texto_protagoles_rival")),
                      column(6, uiOutput("texto_tirosjugador_rival")),
                      column(6, uiOutput("prota_goles_rival")),
                      box(
                        column(6, plotOutput("goles_jugador_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_protacentros_rival")),
                      column(6, uiOutput("texto_centrosjugador_rival")),
                      column(6, uiOutput("prota_centros_rival")),
                      box(
                        column(6, plotOutput("centros_jugador_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_protacambios_rival")),
                      column(6, uiOutput("texto_cambiosjugador_rival")),
                      
                      column(6, fluidRow(
                        column(12, uiOutput("prota_cambios_rival")),
                        column(12, materialSwitch("excluir_portero_rival", label = "Exclude Goalkeeper", status = "primary", right = TRUE))
                      )),
                      
                      box(
                        column(6, plotOutput("cambios_jugador_rival", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_protaasistencias_rival")),
                      column(6, uiOutput("texto_asistenciasjugador_rival")),
                      column(6, uiOutput("prota_asistencias_rival")),
                      box(
                        column(6, plotOutput("asistencias_jugador_rival", height = 300, width = 400))
                      )
                      
                      
                    )),
                    
                    tabItem("def_org2", fluidRow(
                      column(6, selectInput("selector_competi_rival_def", "Competition", choices = competis)),
                      column(6, uiOutput("selector_equipo_rival_def")),
                      box(
                        column(2, plotOutput("img_rival_def", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_def2", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_def2")), width = 12
                      ),
                      column(12, uiOutput("partidos_def2")),
                      
                      column(6, uiOutput("texto_deftirosarea_rival")),
                      column(6, uiOutput("texto_deftirosfda_rival")),
                      box(
                        column(6, plotOutput("deftirosarea_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("deftirosfda_rival", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("defgoles_rival"),
                      
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_defcarriles_rival")),
                      column(6, uiOutput("texto_defcentros_rival")),
                      box(
                        column(6, plotOutput("defcarriles_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("defcentros_rival", height = 300, width = 400))
                      ),
                      valueBoxOutput("defppda_rival"),
                      valueBoxOutput("deffinalizadas34_rival"),
                      valueBoxOutput("defcortadas34_rival"),
                      
                      column(12, uiOutput("texto_protadefensivo_rival")),
                      column(6, uiOutput("protadefensivo_rival1")),
                      column(6, uiOutput("protadefensivo_rival2")),
                      column(6, uiOutput("protadefensivo_rival3")),
                      column(6, uiOutput("protadefensivo_rival4"))
                      
                    )),
                    
                    tabItem("off_tr2", fluidRow(
                      column(6, selectInput("selector_competi_rival_tr_da", "Competition", choices = competis)),
                      column(6, uiOutput("selector_equipo_rival_tr_da")),
                      box(
                        column(2, plotOutput("img_rival_tr_da", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_tr_da2", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_tr_da2")), width = 12
                      ),
                      
                      column(12, uiOutput("partidos_tr_da2")),
                      
                      column(6, uiOutput("texto_robos_rival")),
                      column(6, uiOutput("texto_recuperadores_rival")),
                      box(
                        column(6, plotOutput("robos_rival", height = 300, width = 400))
                      ),
                      column(6, uiOutput("recuperadores_rival")),
                      
                      column(12, renderUI({NULL})),
                      valueBoxOutput("robosrapidos_rival"),
                      column(5, uiOutput("ladron_rapido_rival")),
                      column(12, renderUI({NULL})),
                      
                      column(12, uiOutput("texto_carrilestrans_rival")),
                      box(
                        column(12, plotOutput("carrilestrans_rival", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_tirosfdatrans_rival")),
                      column(6, uiOutput("texto_tirosareatrans_rival")),
                      box(
                        column(6, plotOutput("tiros_fdatrans_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("tiros_areatrans_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_protagolestrans_rival")),
                      column(6, uiOutput("texto_tirosjugadortrans_rival")),
                      column(6, uiOutput("prota_golestrans_rival")),
                      box(
                        column(6, plotOutput("goles_jugadortrans_rival", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("porc_goles_trans_rival"),
                      
                      column(12, renderUI({NULL})),
                      
                      
                      column(12, uiOutput("texto_contrasverticales_rival")),
                      box(
                        column(12, plotOutput("contrasverticales_rival", height = 300, width = 400))
                      ),
                      valueBoxOutput("porcverticales_rival"),
                      valueBoxOutput("porcfinalizadas_rival"),
                      valueBoxOutput("porc_goles_transvert_rival"),
                      
                      
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_recuperador_vertical_rival")),
                      column(6, uiOutput("texto_finalizador_vertical_rival")),
                      column(6, uiOutput("recuperador_vertical_rival")),
                      column(6, uiOutput("finalizador_vertical_rival")),
                      
                    )),
                    
                    tabItem("def_tr2", fluidRow(
                      column(6, selectInput("selector_competi_rival_tr_ad", "Competition", choices = competis)),
                      column(6, uiOutput("selector_equipo_rival_tr_ad")),
                      box(
                        column(2, plotOutput("img_rival_tr_ad", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_tr_ad2", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_tr_ad2")), width = 12
                      ),
                      
                      column(12, uiOutput("partidos_tr_ad2")),
                      
                      
                      column(6, uiOutput("texto_perdidas_rival")),
                      column(6, uiOutput("texto_perdedores_rival")),
                      box(
                        column(6, plotOutput("perdidas_rival", height = 300, width = 400))
                      ),
                      column(6, uiOutput("perdedores_rival")),
                      
                      column(12, renderUI({NULL})),
                      valueBoxOutput("perdidasrapidas_rival"),
                      column(5, uiOutput("perdedor_rapido_rival")),
                      column(12, renderUI({NULL})),
                      
                      column(12, uiOutput("texto_defcarrilestrans_rival")),
                      box(
                        column(12, plotOutput("defcarrilestrans_rival", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      
                      column(6, uiOutput("texto_deftirosareatrans_rival")),
                      column(6, uiOutput("texto_deftirosfdatrans_rival")),
                      box(
                        column(6, plotOutput("deftirosareatrans_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("deftirosfdatrans_rival", height = 300, width = 400))
                      ),
                      column(12, renderUI({NULL})),
                      valueBoxOutput("defgolestrans_rival"),
                      
                      
                      column(12, renderUI({NULL})),
                      
                      
                      column(12, uiOutput("texto_defcontrasverticales_rival")),
                      box(
                        column(12, plotOutput("defcontrasverticales_rival", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("defporcverticales_rival"),
                      valueBoxOutput("defporcfinalizadas_rival"),
                      valueBoxOutput("defgolesvertical_rival")
                      
                      
                    )),
                    
                    tabItem("abp2", fluidRow(
                      column(6, selectInput("selector_competi_rival_abp", "Competition", choices = competis)),
                      column(6, uiOutput("selector_equipo_rival_abp")),
                      box(
                        column(2, plotOutput("img_rival_abp", height = 75, width = 75)),
                        column(4, dateRangeInput("selector_fechas_abp2", "Date Range", start = "2023-08-01", end = "2024-07-01")),
                        column(6, uiOutput("texto_fase_abp2")), width = 12
                      ),
                      column(12, uiOutput("partidos_abp2")),
                      
                      column(12, uiOutput("texto_abpfavor_rival")),
                      
                      
                      column(6, uiOutput("texto_cornerder_rival")),
                      column(6, uiOutput("texto_cornerizq_rival")),
                      
                      column(6, uiOutput("jugadores_cornerd_rival")),
                      column(6, uiOutput("jugadores_corneri_rival")),
                      box(
                        column(6, plotOutput("cornerder_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("cornerizq_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_faltascentro_rival")),
                      column(6, uiOutput("texto_libredirecto_rival")),
                      
                      box(
                        column(6, plotOutput("faltascentro_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("libredirecto_rival", height = 300, width = 400))
                      ),
                      
                      column(12, uiOutput("texto_penales_rival")),
                      box(
                        column(12, plotOutput("penales_rival", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("porc_goles_abp_rival"),
                      
                      column(12, renderUI({NULL})),
                      
                      column(12, uiOutput("texto_abpcontra_rival")),
                      
                      
                      column(6, uiOutput("texto_defcornerder_rival")),
                      column(6, uiOutput("texto_defcornerizq_rival")),
                      
                      box(
                        column(6, plotOutput("defcornerder_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("defcornerizq_rival", height = 300, width = 400))
                      ),
                      
                      column(6, uiOutput("texto_deffaltascentro_rival")),
                      column(6, uiOutput("texto_deflibredirecto_rival")),
                      
                      box(
                        column(6, plotOutput("deffaltascentro_rival", height = 300, width = 400))
                      ),
                      box(
                        column(6, plotOutput("deflibredirecto_rival", height = 300, width = 400))
                      ),
                      
                      column(12, uiOutput("texto_defpenales_rival")),
                      box(
                        column(12, plotOutput("defpenales_rival", height = 300, width = 400))
                      ),
                      
                      valueBoxOutput("porc_goles_defabp_rival"),
                      
                      column(12, renderUI({NULL})),
                      column(12, uiOutput("texto_protadefabp_rival")),
                      
                      column(6, uiOutput("protadefabp_rival1")),
                      column(6, uiOutput("protadefabp_rival2")),
                      
                      column(6, fluidRow(
                        column(12, uiOutput("portero_control_rival")),
                        column(12, uiOutput("porccontrol_por_rival"))
                      )),
                      column(6, fluidRow(
                        column(12, uiOutput("texto_zonadecontrol_por_rival")),
                        column(12, plotOutput("zonadecontrol_por_rival", height = 300, width = 150))
                      ))
                      
                      
                    ))
                    
                    
                  )
                )
  )
)


server <- function(input, output, session){
  
  ##### PANEL GENERAL DEL PARTIDO
  #####
  
  ## textos
  output$texto_mosaico <- renderUI({
    HTML(paste0("<span style='font-size:20px; font-weight:bold;'>", "Select Competition:", "</span>"))
  })
  
  output$texto_foto <- renderUI({
    HTML(paste0("<span style='font-size:20px; font-weight:bold;'>", "Selected Competition", "</span>"))
  })
  
  
  ###
  selectedImagen <- reactiveVal()
  selectedPartido <- reactiveVal()
  selectedDF <- reactiveVal()
  
  ## SELECTORES EN MOSAICO E IMAGEN DE COMPETICIÓN SELECCIONADA
  lapply(1:length(imagenes_opciones), function(imagen) {
    observeEvent(input[[paste0("liga_", imagen)]], {
      
      selectedImagen(imagen)
      
      datos_ft <- eventing %>% filter(competicion == competis[imagen])
      selectedDF(datos_ft)
      
      output$myImage <- renderPlot({
      # Configura el tamaño del área de trazado
      par(mar = c(0,0,0,0))
      # Muestra la imagen
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
      rasterImage(readPNG(imagenes_locales[imagen]), 0, 0, 1, 1)
      })
      
      output$selector_partido <- renderUI({
        selectInput("selector_partido", "Match:", choices = partidos$partido[partidos$competicion == competis[imagen]])
      })
  
    })
    
  })
  
  observeEvent(input$selector_partido, {
    selectedPartido(input$selector_partido)
  })
  
  active_tab_local <- reactiveVal(NULL)
  active_tab_visitante <- reactiveVal(NULL)
  
  
  ## UNA VEZ SELECCIONADO EL PARTIDO, SU INFORMACIÓN 
  observe({
    req(selectedImagen())
    req(selectedPartido())
    req(selectedDF())
    
    imagen <- selectedImagen()
    partidin <- selectedPartido()
    datos_ft <- selectedDF()
    
    ## escudo local
    output$img_local <- renderPlot({
      mostrar_escudo(paste("Escudos", competis[imagen], logos_equipos[str_detect(str_split(logos_equipos, "\\."), unlist(str_split(partidin, " - "))[1])], sep = "/"))      
    })
    
    ## escudo visitante
    output$img_visitante <- renderPlot({
      mostrar_escudo(paste("Escudos", competis[imagen], logos_equipos[str_detect(str_split(logos_equipos, "\\."), unlist(str_split(partidin, " - "))[2])], sep = "/"))      
    })
    
    
    ## resultado: goles de cada equipo
    output$marc_loc <- renderMarcador(datos_ft, partidin, 1)
    
    output$marc_vis <- renderMarcador(datos_ft, partidin, 2)
    
    ## Goleadores de cada equipo
    output$goles_loc <- renderGoleadores(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
    
    output$goles_vis <- renderGoleadores(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
    
    observeEvent(input$ir_a_local_behave, {
      updateTabItems(session, "sidebarid", "local_behave")
    })
    
    observeEvent(input$ir_a_away_behave, {
      updateTabItems(session, "sidebarid", "away_behave")
    })
    
    
    ## escudos
    output$img_local2 <- renderPlot({
      mostrar_escudo(paste("Escudos", competis[imagen], logos_equipos[str_detect(str_split(logos_equipos, "\\."), unlist(str_split(partidin, " - "))[1])], sep = "/"))      
    })
    
    output$img_visitante2 <- renderPlot({
      mostrar_escudo(paste("Escudos", competis[imagen], logos_equipos[str_detect(str_split(logos_equipos, "\\."), unlist(str_split(partidin, " - "))[2])], sep = "/"))      
    })
    
    ###### 1.1 FASE OFENSIVA
    ##### 
    
    observeEvent(input[["local_OFFENSIVE ORGANIZATION"]], {
      active_tab_local("local_OFFENSIVE ORGANIZATION")
      
      output$separ1 <- NULL
      
      output$texto_hm_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Successful Passes Heatmap:", "</span>"))
      })
      
      output$texto_sh_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots:", "</span>"))
      })
      
      output$texto_seq_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Most Repeated Pass Sequences:", "</span>"))
      })
      
      output$texto_cen_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses:", "</span>"))
      })
      
      output$texto_sho_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Players with the most Shots:", "</span>"))
      })
      
      output$texto_por_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> GoalKeeper Starts:", "</span>"))
      })
      
      output$texto_def_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Deffensive Line Passes:", "</span>"))
      })
      
      output$redpases_local <- renderRedPases(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$heatmap_local <- renderHeatmap(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$shotmap_local <- renderShotmap(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$sequenceGraph_local <- renderSeq(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$centros_local <- renderCentros(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$pasesmedios_local <- renderPasesMedios(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$verticalidad_local <- renderVerticalidad(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$finalizadores_local <- renderFinalizadores(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      
      output$saquesPortero_local <- renderEnvios(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1], linea = "Saques portero", ipt = input, l_v = "l")
      
      
      aux <- datos_ft %>% filter(partido == partidin, fasedejuego == "Construccion Ofensiva", team == unlist(str_split(partidin, " - "))[1])
      aux <- aux %>% filter(type.displayName == "Pass", ((x < 5 & (y > 75 | y < 25)) | (between(x, 5.1, 35))))
      jugadores_lineadf <- unique(aux$name)
      
      output$jugadores_linea_def_l <- renderUI({
        selectInput("jugadores_linea_def_l", "Passes from Player:", choices = jugadores_lineadf, multiple = TRUE, selected = jugadores_lineadf)
      })
      
      output$lineaDef_local <- renderEnvios(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1], linea = "Defensiva", ipt = input, l_v = "l")
      
    })
    
    
    observeEvent(input[["visitante_OFFENSIVE ORGANIZATION"]], {
      active_tab_visitante("visitante_OFFENSIVE ORGANIZATION")
      
      output$separ2 <- NULL
      
      
      output$texto_hm_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Successful Passes Heatmap:", "</span>"))
      })
      
      output$texto_sh_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots:", "</span>"))
      })
      
      output$texto_seq_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Most Repeated Pass Sequences:", "</span>"))
      })
      
      output$texto_cen_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses:", "</span>"))
      })
      
      output$texto_sho_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Players with the most Shots:", "</span>"))
      })
      
      output$texto_por_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> GoalKeeper Starts:", "</span>"))
      })
      
      output$texto_def_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Deffensive Line Passes:", "</span>"))
      })
      
      output$redpases_visitante <- renderRedPases(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$heatmap_visitante <- renderHeatmap(datos_ft, partidin,  fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$shotmap_visitante <- renderShotmap(datos_ft, partidin,  fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$sequenceGraph_visitante <- renderSeq(datos_ft, partidin,  fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$centros_visitante <- renderCentros(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$pasesmedios_visitante <- renderPasesMedios(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$verticalidad_visitante <- renderVerticalidad(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$finalizadores_visitante <- renderFinalizadores(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      
      
      output$saquesPortero_visitante <- renderEnvios(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2], linea = "Saques portero", ipt = input, l_v = "v")
      
      aux <- datos_ft %>% filter(partido == partidin, fasedejuego == "Construccion Ofensiva", team == unlist(str_split(partidin, " - "))[2])
      aux <- aux %>% filter(type.displayName == "Pass", ((x < 5 & (y > 75 | y < 25)) | (between(x, 5.1, 35))))
      jugadores_lineadf <- unique(aux$name)
      
      output$jugadores_linea_def_v <- renderUI({
        selectInput("jugadores_linea_def_v", "Passes from Player:", choices = jugadores_lineadf, multiple = TRUE, selected = jugadores_lineadf)
      })
      
      output$lineaDef_visitante <- renderEnvios(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2], linea = "Defensiva", ipt = input, l_v = "v")
      
    })
    
    ######
    ###### 1.2 TRANSICION DEFENSA-ATAQUE
    ######
    observeEvent(input[["local_DEFENSE - OFFENSE TRANSITION"]], {
      active_tab_local("local_DEFENSE - OFFENSE TRANSITION")
      
      output$blanco1 <- NULL
      
      output$texto_robos_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Steals and <span style='color:cyan;'>Quick Steals</span> Distribution:
                      <span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Steals as steals produced, at most, 10 seconds after losing the ball.</span>
                      </span>
                      </span>"))
      })
      
      output$texto_rob_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Principal Stealers:", "</span>"))
      })
      
      output$texto_ass_t_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shot Assistants:", "</span>"))
      })
      
      output$texto_ass_g_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Goal Assistants:", "</span>"))
      })
      
      output$texto_flec_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Attack Zones:", "</span>"))
      })
      
      output$texto_trans_shot_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots Distribution:", "</span>"))
      })
      
      output$texto_trans_vert_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Vertical Transitions:", "</span>"))
      })
      
      output$separador1 <- NULL
      
      output$alturarobos_local <- renderRecuperaciones(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$recuperadores_local <- renderRecuperadores(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$carrilesataque_local <- renderFlechas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$porcverticales_local <- renderPorcVerticales(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$porcfinalizadas_local <- renderPorcFinalizadas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$mediapases_local <- renderPasesMedios(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$asistentestiro_local <- renderAsistentesTiro(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$asistentesgol_local <- renderAsistentesGol(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$shotmaptrans_local <- renderShotmap(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$contrasverticales_local <- renderContrasVerticales(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      
    })
    
    observeEvent(input[["visitante_DEFENSE - OFFENSE TRANSITION"]], {
      active_tab_visitante("visitante_DEFENSE - OFFENSE TRANSITION")
      
      output$blanco2 <- NULL
      
      
      output$texto_robos_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Steals and <span style='color:cyan;'>Quick Steals</span> Distribution:
                      <span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Steals as steals produced, at most, 10 seconds after losing the ball.</span>
                      </span>
                      </span>"))
      })
      
      output$texto_rob_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Principal Stealers:", "</span>"))
      })
      
      output$texto_ass_t_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shot Assistants:", "</span>"))
      })
      
      output$texto_ass_g_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Goal Assistants:", "</span>"))
      })
      
      output$texto_flec_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Attack Zones:", "</span>"))
      })
      
      output$texto_trans_shot_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots Distribution:", "</span>"))
      })
      
      output$texto_trans_vert_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Vertical Transitions:", "</span>"))
      })
      
      output$separador2 <- NULL
      
      output$alturarobos_visitante <- renderRecuperaciones(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$recuperadores_visitante <- renderRecuperadores(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$carrilesataque_visitante <- renderFlechas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$porcverticales_visitante <- renderPorcVerticales(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$porcfinalizadas_visitante <- renderPorcFinalizadas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$mediapases_visitante <- renderPasesMedios(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$asistentestiro_visitante <- renderAsistentesTiro(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$asistentesgol_visitante <- renderAsistentesGol(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$shotmaptrans_visitante <- renderShotmap(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$contrasverticales_visitante <- renderContrasVerticales(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      
    })
    
    ######
    ###### 1.3 FASE DEFENSIVA
    ######
    observeEvent(input[["local_DEFENSIVE ORGANIZATION"]], {
      active_tab_local("local_DEFENSIVE ORGANIZATION")
      
      output$texto_alt_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Team Defensive Line (Based on Opponent Passes Height):", "</span>"))
      })
      
      output$texto_opp_sh_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Shots:", "</span>"))
      })
      
      output$texto_opp_cen_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses Allowed:", "</span>"))
      })
      
      output$texto_opp_at_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Attack Zones:", "</span>"))
      })
      
      output$texto_opp_ind_l <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br>------------ Players with the most Defensive Actions ------------", "</span>"))
      })
      
      
      
      output$separador3 <- NULL
      
      
      output$alturalinea_local <- renderDefHeatmap(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$oppshotmap_local <- renderDefShotmap(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$oppcentros_local <- renderDefCentros(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$ppda_local <- renderPPDA(datos_ft, partidin, unlist(str_split(partidin, " - "))[1], unlist(str_split(partidin, " - "))[2])
      output$carrilesof_local <- renderDefFlechas(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2])
      output$ataques34finalizados_local <- renderDefFinalizadas3cuartos(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$ataques34cortados_local <- renderDefCortadas3cuartos(datos_ft, partidin, unlist(str_split(partidin, " - "))[1], unlist(str_split(partidin, " - "))[2])
      
      output$protadef1_local <- renderJugadoresProtas(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1], 1)
      output$protadef2_local <- renderJugadoresProtas(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1], 2)
      
    })
    
    
    observeEvent(input[["visitante_DEFENSIVE ORGANIZATION"]], {
      active_tab_visitante("visitante_DEFENSIVE ORGANIZATION")
      
      output$texto_alt_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Team Defensive Line (Based on Opponent Passes Height):", "</span>"))
      })
      
      output$texto_opp_sh_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Shots:", "</span>"))
      })
      
      output$texto_opp_cen_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses Allowed:", "</span>"))
      })
      
      
      output$texto_opp_at_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Attack Zones:", "</span>"))
      })
      
      output$texto_opp_ind_v <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br>------------ Players with the most Defensive Actions ------------", "</span>"))
      })
      
      
      output$separador4 <- NULL
      
      output$alturalinea_visitante <- renderDefHeatmap(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$oppshotmap_visitante <- renderDefShotmap(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$oppcentros_visitante <- renderDefCentros(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$ppda_visitante <- renderPPDA(datos_ft, partidin, unlist(str_split(partidin, " - "))[2], unlist(str_split(partidin, " - "))[1])
      output$carrilesof_visitante <- renderDefFlechas(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[1])
      output$ataques34finalizados_visitante <- renderDefFinalizadas3cuartos(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$ataques34cortados_visitante <- renderDefCortadas3cuartos(datos_ft, partidin, unlist(str_split(partidin, " - "))[2], unlist(str_split(partidin, " - "))[1])
      
      output$protadef1_visitante <- renderJugadoresProtas(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2], 1)
      output$protadef2_visitante <- renderJugadoresProtas(datos_ft, partidin, fasedj = "Construccion Ofensiva", unlist(str_split(partidin, " - "))[2], 2)
      
    })

    # Zonas del área desde donde más le rematan
    
    ######
    ###### 1.4 TRANSICION ATAQUE-DEFENSA
    ######
    
    observeEvent(input[["local_OFFENSE - DEFENSE TRANSITION"]], {
      active_tab_local("local_OFFENSE - DEFENSE TRANSITION")
      
      output$texto_perdidas_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Turnovers and <span style='color:cyan;'>Quick Turnovers</span> Distribution:
                      <span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Turnovers as losses where the team does not have the possession for 10 seconds.</span>
                      </span>
                      </span>"))
      })
      
      output$texto_perd_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Principal Losers:", "</span>"))
      })
      
      output$texto_opp_vert_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Vertical Transitions:", "</span>"))
      })
      
      output$texto_opp_trans_flec_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Attack Zones:", "</span>"))
      })
      
      output$texto_opp_trans_sh_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Shots:", "</span>"))
      })
      
      output$sep <- NULL
      output$separador5 <- NULL
      
      output$perdidas_local <- renderPerdidas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$perdedores_local <- renderPerdedores(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$carrilestrans_local <- renderDefFlechas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$shotmaptrans_local <- renderDefShotmap(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$defverticales_local <- renderDefContrasVerticales(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$oppverticales_local <- renderPorcVerticales(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$oppfinalizadas_local <- renderPorcFinalizadas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
    })
    
    observeEvent(input[["visitante_OFFENSE - DEFENSE TRANSITION"]], {
      active_tab_visitante("visitante_OFFENSE - DEFENSE TRANSITION")
      
      output$texto_perdidas_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Turnovers and <span style='color:cyan;'>Quick Turnovers</span> Distribution:
                      <span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Turnovers as losses where the team does not have the possession for 10 seconds.</span>
                      </span>
                      </span>"))
      })
      
      output$texto_perd_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Principal Losers:", "</span>"))
      })
      
      output$texto_opp_vert_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Vertical Transitions:", "</span>"))
      })
      
      output$texto_opp_trans_flec_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Attack Zones:", "</span>"))
      })
      
      output$texto_opp_trans_sh_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opponent Shots:", "</span>"))
      })
      
      
      output$separador6 <- NULL
      output$sep2 <- NULL
      
      output$perdidas_visitante <- renderPerdidas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$perdedores_visitante <- renderPerdedores(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[2])
      output$carrilestrans_visitante <- renderDefFlechas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$shotmaptrans_visitante <- renderDefShotmap(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$defverticales_visitante <- renderDefContrasVerticales(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$oppverticales_visitante <- renderPorcVerticales(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
      output$oppfinalizadas_visitante <- renderPorcFinalizadas(datos_ft, partidin, fasedj = "Transicion DEF-AT", unlist(str_split(partidin, " - "))[1])
    })
    
    
    ###### 1.5 ABP - favor
    ###### 
    observeEvent(input[["local_SET PIECE"]], {
      active_tab_local("local_SET PIECE")
      
      
      
      output$texto_favor_l <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------  OFFENSIVE SET PIECE ------------------------", "</span>"))
      })
      
      output$texto_cornerder_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Corners From Right", "</span>"))
      })
      
      output$texto_cornerizq_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Corners From Left", "</span>"))
      })
      
      output$separador7 <- NULL
      
      aux <- datos_ft %>% filter(partido == partidin, fasedejuego == "ABP", team == unlist(str_split(partidin, " - "))[1], x > 99, y < 1)
      jugadores_cd_l <- unique(aux$name)
      
      output$jugadores_cornerd_l <- renderUI({
        selectInput("jugadores_cornerd_l", "Corners Taken by:", choices = jugadores_cd_l, multiple = T, selected = jugadores_cd_l[1])
      })
      
      # CÓRNERS DE CADA LADO Y % DE REMATES/INTENTADOS EN CADA PALO
      output$cornerder_local <- renderCornersDer(datos_ft, partidin, unlist(str_split(partidin, " - "))[1], input, "l")
      
      
      aux <- datos_ft %>% filter(partido == partidin, fasedejuego == "ABP", team == unlist(str_split(partidin, " - "))[1], x > 99, y > 99, endX >= 85)
      jugadores_ci_l <- unique(aux$name)
      
      output$jugadores_corneri_l <- renderUI({
        selectInput("jugadores_corneri_l", "Corners Taken by:", choices = jugadores_ci_l, multiple = T, selected = jugadores_ci_l[1])
      })
      
      output$cornerizq_local <- renderCornersIzq(datos_ft, partidin, unlist(str_split(partidin, " - "))[1], input, "l")
      
      
      output$texto_faltacentro_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Fouls Ended in the Box", "</span>"))
      })
      
      output$texto_libdir_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Direct Free Kicks", "</span>"))
      })
      
      output$texto_penal_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Penalties Taken", "</span>"))
      })
      
      # FALTAS AL ÁREA POR CADA LADO Y ¿FRONTAL?
      output$faltascentro_local <- renderFaltasCentro(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$libredirecto_local <- renderLibreDirecto(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$penales_local <- renderPenales(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      
      output$texto_contra_l <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------ DEFENSIVE SET PIECE ------------------------", "</span>"))
      })
      
      
      output$texto_opp_cornerder_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opposite Corners From Right", "</span>"))
      })
      
      output$texto_opp_cornerizq_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Corners From Left", "</span>"))
      })
      
      output$texto_opp_faltacentro_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Fouls Ended in the Box", "</span>"))
      })
      
      output$texto_opp_libdir_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Direct Free Kicks", "</span>"))
      })
      
      output$texto_opp_penal_l <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Penalties Taken", "</span>"))
      })
      
      
      output$separador9 <- NULL
      
      # en contra
      output$defcornerder_local <- renderDefCornersDer(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$defcornerizq_local <- renderDefCornersIzq(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$deffaltascentro_local <- renderDefFaltasCentro(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$deflibredirecto_local <- renderDefLibreDirecto(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$defpenales_local <- renderPenales(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      
      output$texto_ind_abp_l <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ---------- Players with the most Defensive Actions ---------- ", "</span>"))
      })
      
      output$defabp1_ind_l <- renderJugadoresProtas(datos_ft, partidin, fasedj = "ABP", unlist(str_split(partidin, " - "))[1], 1)
      output$defabp2_ind_l <- renderJugadoresProtas(datos_ft, partidin, fasedj = "ABP", unlist(str_split(partidin, " - "))[1], 2)
      
    })
    
    
    
    observeEvent(input[["visitante_SET PIECE"]], {
      active_tab_visitante("visitante_SET PIECE")
      
      output$texto_favor_v <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 5px; margin-bottom: 5px;'>", "<br> <br> ------------------------ OFFENSIVE SET PIECE ------------------------", "</span>"))
      })
      
      output$texto_cornerder_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Corners From Right", "</span>"))
      })
      
      output$texto_cornerizq_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Corners From Left", "</span>"))
      })
      
      output$separador8 <- NULL
      
      
      aux <- datos_ft %>% filter(partido == partidin, fasedejuego == "ABP", team == unlist(str_split(partidin, " - "))[2], x > 99, y < 1, endX >= 85)
      jugadores_cd_v <- unique(aux$name)
      
      output$jugadores_cornerd_v <- renderUI({
        selectInput("jugadores_cornerd_v", "Corners Taken by:", choices = jugadores_cd_v, multiple = T, selected = jugadores_cd_v[1])
      })
      
      
      # CÓRNERS DE CADA LADO Y % DE REMATES/INTENTADOS EN CADA PALO
      output$cornerder_visitante <- renderCornersDer(datos_ft, partidin, unlist(str_split(partidin, " - "))[2], input, "v")
      
      
      aux <- datos_ft %>% filter(partido == partidin, fasedejuego == "ABP", team == unlist(str_split(partidin, " - "))[2], x > 99, y > 99)
      jugadores_ci_v <- unique(aux$name)
      
      output$jugadores_corneri_v <- renderUI({
        selectInput("jugadores_corneri_v", "Corners Taken by:", choices = jugadores_ci_v, multiple = T, selected = jugadores_ci_v[1])
      })
      
      output$cornerizq_visitante <- renderCornersIzq(datos_ft, partidin, unlist(str_split(partidin, " - "))[2], input, "v")
      
      
      output$texto_faltacentro_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Fouls Ended in the Box", "</span>"))
      })
      
      output$texto_libdir_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Direct Free Kicks", "</span>"))
      })
      
      output$texto_penal_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Penalties Taken", "</span>"))
      })
      # FALTAS AL ÁREA POR CADA LADO Y ¿FRONTAL?
      output$faltascentro_visitante <- renderFaltasCentro(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$libredirecto_visitante <- renderLibreDirecto(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      output$penales_visitante <- renderPenales(datos_ft, partidin, unlist(str_split(partidin, " - "))[2])
      
      
      output$texto_contra_v <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------ DEFENSIVE SET PIECE ------------------------", "</span>"))
      })
      
      output$texto_opp_cornerder_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Opposite Corners From Right", "</span>"))
      })
      
      output$texto_opp_cornerizq_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Corners From Left", "</span>"))
      })
      
      output$texto_opp_faltacentro_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Fouls Ended in the Box", "</span>"))
      })
      
      output$texto_opp_libdir_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Direct Free Kicks", "</span>"))
      })
      
      output$texto_opp_penal_v <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Penalties Taken", "</span>"))
      })
      
      output$separador10 <- NULL
      
      # en contra
      output$defcornerder_visitante <- renderDefCornersDer(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$defcornerizq_visitante <- renderDefCornersIzq(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$deffaltascentro_visitante <- renderDefFaltasCentro(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$deflibredirecto_visitante <- renderDefLibreDirecto(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      output$defpenales_visitante <- renderPenales(datos_ft, partidin, unlist(str_split(partidin, " - "))[1])
      
      output$texto_ind_abp_v <- renderUI({
        HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ----------- Players with the most Defensive Actions ----------- ", "</span>"))
      })
      
      output$defabp1_ind_v <- renderJugadoresProtas(datos_ft, partidin, fasedj = "ABP", unlist(str_split(partidin, " - "))[2], 1)
      output$defabp2_ind_v <- renderJugadoresProtas(datos_ft, partidin, fasedj = "ABP", unlist(str_split(partidin, " - "))[2], 2)
      
      
    })

    
    #####
    # LOCALES
    output$main_content_local <- renderUI({
      if (is.null(active_tab_local())) {
        return(NULL)
      }
      
      if (active_tab_local() == "local_OFFENSIVE ORGANIZATION") {
        tagList(
          column(7, plotOutput("redpases_local", height = 350, width = 525)),
          
          column(6, renderUI({NULL})),
          column(6, uiOutput("texto_def_l")),
          column(6, uiOutput("texto_por_l")),
          column(6, uiOutput("jugadores_linea_def_l")),
          box(
            column(6, plotOutput("saquesPortero_local", height = 300, width = 400))
          ),
          box(
            column(6, plotOutput("lineaDef_local", height = 300, width = 400))
          ),
          
          column(12, renderUI({NULL})),
          valueBoxOutput("pasesmedios_local"),
          valueBoxOutput("verticalidad_local"),
          column(12, renderUI({NULL})),
          
          column(6, uiOutput("texto_seq_l")),
          column(6, uiOutput("texto_cen_l")),
          column(6, uiOutput("sequenceGraph_local")),
          box(
            column(6, plotOutput("centros_local", width = 400))
          ),
          column(12, renderUI({NULL})),
          
          column(5, uiOutput("texto_hm_l")),
          column(5, uiOutput("texto_sh_l")),
          column(2, uiOutput("texto_sho_l")),
          box(
          column(5, plotOutput("heatmap_local", height = 250, width = 325)), width = 5
          ),
          box(
          column(5, plotOutput("shotmap_local", height = 250, width = 325)), width = 5
          ),
          column(2, uiOutput("finalizadores_local"))
          
        )
      } else if (active_tab_local() == "local_DEFENSE - OFFENSE TRANSITION") {
        tagList(
          column(7, plotOutput("separador1")),
          column(6, uiOutput("texto_robos_l")),
          column(2, uiOutput("texto_rob_l")),
          column(2, uiOutput("texto_ass_t_l")),
          column(2, uiOutput("texto_ass_g_l")),
          column(12, uiOutput("blanco1")),
          box(
          column(6, plotOutput("alturarobos_local", height = 300, width = 400))
          ),
          column(2, uiOutput("recuperadores_local")),
          column(2, uiOutput("asistentestiro_local")),
          column(2, uiOutput("asistentesgol_local")),
          
          column(12, renderUI({NULL})),
          
          column(6, uiOutput("texto_flec_l")),
          column(6, uiOutput("texto_trans_shot_l")),
          box(
          column(6, plotOutput("carrilesataque_local", height = 300, width = 400))
          ),
          box(
          column(6, plotOutput("shotmaptrans_local", height = 300, width = 400))
          ),
          column(12, uiOutput("texto_trans_vert_l")),
          box(
          column(12, plotOutput("contrasverticales_local", height = 300, width = 400))
          ),
          valueBoxOutput("porcverticales_local"),
          valueBoxOutput("porcfinalizadas_local")
        )
      }
      
      else if (active_tab_local() == "local_DEFENSIVE ORGANIZATION") {
        tagList(
          column(7, plotOutput("separador3")),
          column(6, uiOutput("texto_alt_l")),
          column(6, uiOutput("texto_opp_sh_l")),
          box(
          column(6, plotOutput("alturalinea_local", height = 300, width = 400))
          ),
          box(
          column(6, plotOutput("oppshotmap_local", height = 300, width = 400))
          ),
          column(6, uiOutput("texto_opp_cen_l")),
          column(6, uiOutput("texto_opp_at_l")),
          box(
          column(6, plotOutput("oppcentros_local", height = 300, width = 400))
          ),
          box(
          column(6, plotOutput("carrilesof_local", height = 300, width = 400))
          ),
          
          valueBoxOutput("ppda_local"),
          valueBoxOutput("ataques34finalizados_local"),
          valueBoxOutput("ataques34cortados_local"),
          column(12, uiOutput("texto_opp_ind_l")),
          column(6, uiOutput("protadef1_local")),
          column(6, uiOutput("protadef2_local")),
          
          
        )
      }
      
      else if (active_tab_local() == "local_OFFENSE - DEFENSE TRANSITION") {
        tagList(
          column(7, plotOutput("separador5")),
          column(6, uiOutput("texto_perdidas_l")),
          column(6, uiOutput("texto_perd_l")),
          box(
          column(6, plotOutput("perdidas_local", height = 300, width = 400))
          ),
          column(6, uiOutput("perdedores_local")),
          column(12, uiOutput("sep")),
          column(6, uiOutput("texto_opp_trans_flec_l")),
          column(6, uiOutput("texto_opp_trans_sh_l")),
          box(
            column(6, plotOutput("carrilestrans_local", height = 300, width = 400))
          ),
          box(
            column(6, plotOutput("shotmaptrans_local", height = 300, width = 400))
          ),
          
          column(12, uiOutput("texto_opp_vert_l")),
          box(
            column(12, plotOutput("defverticales_local", height = 300, width = 400))
          ),
          
          valueBoxOutput("oppverticales_local"),
          valueBoxOutput("oppfinalizadas_local")
        )
      }
      
      else if (active_tab_local() == "local_SET PIECE") {
        tagList(
          column(7, plotOutput("separador7")),
          column(12, uiOutput("texto_favor_l")),
          column(6, uiOutput("texto_cornerder_l")),
          column(6, uiOutput("texto_cornerizq_l")),
          column(6, uiOutput("jugadores_cornerd_l")),
          column(6, uiOutput("jugadores_corneri_l")),
          box(
          column(6, plotOutput("cornerder_local", height = 400, width = 300))
          ),
          box(
          column(6, plotOutput("cornerizq_local", height = 400, width = 300))
          ),
          
          column(6, uiOutput("texto_faltacentro_l")),
          column(6, uiOutput("texto_libdir_l")),
          box(
          column(6, plotOutput("faltascentro_local", height = 400, width = 400))
          ),
          box(
          column(6, plotOutput("libredirecto_local", height = 400, width = 400))
          ),
          
          column(12, uiOutput("texto_penal_l")),
          box(
          column(12, plotOutput("penales_local", height = 300, width = 400))
          ),
          #column(6, plotOutput("separador9")),
          
          column(12, uiOutput("texto_contra_l")),
          column(6, uiOutput("texto_opp_cornerder_l")),
          column(6, uiOutput("texto_opp_cornerizq_l")),
          
          box(
          column(6, plotOutput("defcornerder_local", height = 400, width = 300))
          ),
          box(
          column(6, plotOutput("defcornerizq_local", height = 400, width = 300))
          ),
          
          column(6, uiOutput("texto_opp_faltacentro_l")),
          column(6, uiOutput("texto_opp_libdir_l")),
          box(
          column(6, plotOutput("deffaltascentro_local", height = 400, width = 400))
          ),
          box(
          column(6, plotOutput("deflibredirecto_local", height = 400, width = 400))
          ),
          
          column(12, uiOutput("texto_opp_penal_l")),
          box(
          column(12, plotOutput("defpenales_local", height = 300, width = 400))
          ),
          
          column(12, uiOutput("texto_ind_abp_l")),
          column(6, uiOutput("defabp1_ind_l")),
          column(6, uiOutput("defabp2_ind_l"))
          
        )
      }
      
    })
    
    # VISITANTES
    output$main_content_visitante <- renderUI({
      if (is.null(active_tab_visitante())) {
        return(NULL)
      }
      
      if (active_tab_visitante() == "visitante_OFFENSIVE ORGANIZATION") {
        tagList(
          column(7, plotOutput("redpases_visitante", height = 350, width = 525)),
          
          column(6, renderUI({NULL})),
          column(6, uiOutput("texto_def_v")),
          column(6, uiOutput("texto_por_v")),
          column(6, uiOutput("jugadores_linea_def_v")),
          box(
            column(6, plotOutput("saquesPortero_visitante", height = 300, width = 400))
          ),
          box(
            column(6, plotOutput("lineaDef_visitante", height = 300, width = 400))
          ),
          
          column(12, renderUI({NULL})),
          valueBoxOutput("pasesmedios_visitante"),
          valueBoxOutput("verticalidad_visitante"),
          column(12, renderUI({NULL})),
          
          column(6, uiOutput("texto_seq_v")),
          column(6, uiOutput("texto_cen_v")),
          column(6, uiOutput("sequenceGraph_visitante")),
          box(
            column(6, plotOutput("centros_visitante", width = 400))
          ),
          column(12, renderUI({NULL})),
          
          column(5, uiOutput("texto_hm_v")),
          column(5, uiOutput("texto_sh_v")),
          column(2, uiOutput("texto_sho_v")),
          box(
            column(5, plotOutput("heatmap_visitante", height = 250, width = 325)), width = 5
          ),
          box(
            column(5, plotOutput("shotmap_visitante", height = 250, width = 325)), width = 5
          ),
          column(2, uiOutput("finalizadores_visitante"))
          
        )
      } else if (active_tab_visitante() == "visitante_DEFENSE - OFFENSE TRANSITION") {
        tagList(
          column(7, plotOutput("separador2")),
          column(6, uiOutput("texto_robos_v")),
          column(2, uiOutput("texto_rob_v")),
          column(2, uiOutput("texto_ass_t_v")),
          column(2, uiOutput("texto_ass_g_v")),
          column(12, uiOutput("blanco2")),
          box(
            column(6, plotOutput("alturarobos_visitante", height = 300, width = 400))
          ),
          column(2, uiOutput("recuperadores_visitante")),
          column(2, uiOutput("asistentestiro_visitante")),
          column(2, uiOutput("asistentesgol_visitante")),
          column(12, renderUI({NULL})),
          column(6, uiOutput("texto_flec_v")),
          column(6, uiOutput("texto_trans_shot_v")),
          box(
            column(6, plotOutput("carrilesataque_visitante", height = 300, width = 400))
          ),
          box(
            column(6, plotOutput("shotmaptrans_visitante", height = 300, width = 400))
          ),
          column(12, uiOutput("texto_trans_vert_v")),
          box(
            column(12, plotOutput("contrasverticales_visitante", height = 300, width = 400))
          ),
          valueBoxOutput("porcverticales_visitante"),
          valueBoxOutput("porcfinalizadas_visitante")
          
        )
      }
      else if (active_tab_visitante() == "visitante_DEFENSIVE ORGANIZATION") {
        tagList(
          column(7, plotOutput("separador4")),
          column(6, uiOutput("texto_alt_v")),
          column(6, uiOutput("texto_opp_sh_v")),
          box(
          column(6, plotOutput("alturalinea_visitante", height = 300, width = 400))
          ),
          box(
          column(6, plotOutput("oppshotmap_visitante", height = 300, width = 400))
          ),
          column(6, uiOutput("texto_opp_cen_v")),
          column(6, uiOutput("texto_opp_at_v")),
          box(
          column(6, plotOutput("oppcentros_visitante", height = 300, width = 400))
          ),
          box(
          column(6, plotOutput("carrilesof_visitante", height = 300, width = 400))
          ),
          valueBoxOutput("ppda_visitante"),
          valueBoxOutput("ataques34finalizados_visitante"),
          valueBoxOutput("ataques34cortados_visitante"),
          column(12, uiOutput("texto_opp_ind_v")),
          column(6, uiOutput("protadef1_visitante")),
          column(6, uiOutput("protadef2_visitante")),
          
        )
      }
      
      else if (active_tab_visitante() == "visitante_OFFENSE - DEFENSE TRANSITION") {
        tagList(
          column(7, plotOutput("separador6")),
          column(6, uiOutput("texto_perdidas_v")),
          column(6, uiOutput("texto_perd_v")),
          box(
          column(6, plotOutput("perdidas_visitante", height = 300, width = 400))
          ),
          column(6, uiOutput("perdedores_visitante")),
          column(12, uiOutput("sep2")),
          column(6, uiOutput("texto_opp_trans_flec_v")),
          column(6, uiOutput("texto_opp_trans_sh_v")),
          box(
            column(6, plotOutput("carrilestrans_visitante", height = 300, width = 400))
          ),
          box(
            column(6, plotOutput("shotmaptrans_visitante", height = 300, width = 400))
          ),
          
          column(12, uiOutput("texto_opp_vert_v")),
          box(
            column(12, plotOutput("defverticales_visitante", height = 300, width = 400))
          ),
          valueBoxOutput("oppverticales_visitante"),
          valueBoxOutput("oppfinalizadas_visitante")
        )
      }
      
      else if (active_tab_visitante() == "visitante_SET PIECE") {
        tagList(
          column(7, plotOutput("separador8")),
          column(12, uiOutput("texto_favor_v")),
          column(6, uiOutput("texto_cornerder_v")),
          column(6, uiOutput("texto_cornerizq_v")),
          column(6, uiOutput("jugadores_cornerd_v")),
          column(6, uiOutput("jugadores_corneri_v")),
          box(
          column(6, plotOutput("cornerder_visitante", height = 400, width = 300))
          ),
          box(
          column(6, plotOutput("cornerizq_visitante", height = 400, width = 300))
          ),
          
          column(6, uiOutput("texto_faltacentro_v")),
          column(6, uiOutput("texto_libdir_v")),
          box(
          column(6, plotOutput("faltascentro_visitante", height = 400, width = 400))
          ),
          box(
          column(6, plotOutput("libredirecto_visitante", height = 400, width = 400))
          ),
          
          column(12, uiOutput("texto_penal_v")),
          box(
          column(12, plotOutput("penales_visitante", height = 300, width = 400))
          ),
          #column(6, plotOutput("separador10")),
          
          column(12, uiOutput("texto_contra_v")),
          column(6, uiOutput("texto_opp_cornerder_v")),
          column(6, uiOutput("texto_opp_cornerizq_v")),
          
          box(
          column(6, plotOutput("defcornerder_visitante", height = 400, width = 300))
          ),
          box(
          column(6, plotOutput("defcornerizq_visitante", height = 400, width = 300))
          ),
          
          column(6, uiOutput("texto_opp_faltacentro_v")),
          column(6, uiOutput("texto_opp_libdir_v")),
          box(
          column(6, plotOutput("deffaltascentro_visitante", height = 400, width = 400))
          ),
          box(
          column(6, plotOutput("deflibredirecto_visitante", height = 400, width = 400))
          ),
          
          column(12, uiOutput("texto_opp_penal_v")),
          box(
          column(12, plotOutput("defpenales_visitante", height = 300, width = 400))
          ),
          
          column(12, uiOutput("texto_ind_abp_v")),
          column(6, uiOutput("defabp1_ind_v")),
          column(6, uiOutput("defabp2_ind_v"))
          
        )
      }
    })
    
    
  })
    
  
  ##### 2. PANEL DE EQUIPO
  #####
  
  ###################
  ##### SEVILLA #####
  ###################
  
  output$img_sevilla_of <- renderPlot({mostrar_escudo(paste0("Escudos/", competis[1], "/Sevilla.png"))})
  output$texto_fase_of <- renderUI({
    HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-left: 40px'>", "Offensive Organization", "</span>"))
  })
  
  output$img_sevilla_def <- renderPlot({mostrar_escudo(paste0("Escudos/", competis[1], "/Sevilla.png"))})
  output$texto_fase_def <- renderUI({
    HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-left: 40px'>", "Defensive Organization", "</span>"))
  })
  
  output$img_sevilla_tr_da <- renderPlot({mostrar_escudo(paste0("Escudos/", competis[1], "/Sevilla.png"))})
  output$texto_fase_tr_da <- renderUI({
    HTML(paste0("<span style='font-size:30px; font-weight:bold; color:red; margin-left: 40px'>", "Defense-Offense Transition", "</span>"))
  })
  
  output$img_sevilla_tr_ad <- renderPlot({mostrar_escudo(paste0("Escudos/", competis[1], "/Sevilla.png"))})
  output$texto_fase_tr_ad <- renderUI({
    HTML(paste0("<span style='font-size:30px; font-weight:bold; color:red; margin-left: 40px'>", "Offense-Defense Transition", "</span>"))
  })
  
  output$img_sevilla_abp <- renderPlot({mostrar_escudo(paste0("Escudos/", competis[1], "/Sevilla.png"))})
  output$texto_fase_abp <- renderUI({
    HTML(paste0("<span style='font-size:30px; font-weight:bold; color:red; margin-left: 40px'>", "Set Piece", "</span>"))
  })
  #####
  ##### 1.- FASE OFENSIVA
  
  observe({
    
    datos_ft <- eventing %>% filter(str_detect(partido, "Sevilla"), competicion == input$selector_competi_of, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_of[1], input$selector_fechas_of[2]))
    
    output$partidos_of <- renderUI({
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == "Sevilla") {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == "Sevilla") {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  # No debería suceder si Sevilla siempre está en el partido
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
    })
    
    output$texto_saques_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> GoalKeeper Starts:", "</span>"))
    })
    
    # saques del portero
    output$saquesportero_sev <- renderIniciosPortero(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    # salidas de la defensa
    # aux <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == "Sevilla")
    # aux <- aux %>% filter(type.displayName == "Pass", ((x < 5 & (y > 75 | y < 25)) | (between(x, 5.1, 35))))
    
    # output$jugadores_salida_sev <- renderUI({
    #   selectInput("jugadores_salida_sev", "Players to watch", choices = unique(aux$name), selected = unique(aux$name), multiple = T)
    # })
    
    output$texto_defensa_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Defense Construction Zones:", "</span>"))
    })
    
    output$salidasdefensa_sev <- renderSalidasDefensa(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    # salidas del medio centro
    #aux <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == "Sevilla")
    #aux <- aux %>% filter(type.displayName == "Pass", ((between(x, 40, 70) & (y > 85 | y < 15))))
    
    #output$jugadores_medio_sev <- renderUI({
    #  selectInput("jugadores_medio_sev", "Players to watch", choices = unique(aux$name), selected = unique(aux$name), multiple = T)
    #})
    
    output$texto_medio_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Midfielders Distribution Zones:", "</span>"))
    })
    
    output$salidasmedio_sev <- renderSalidasMC(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    
    output$texto_tirosfda_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Out of the Box Shots and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    # tiros fuera del área
    output$tiros_fda_sevilla <- renderTirosFDA(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
  
    output$texto_tirosarea_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    # tiros en el área
    output$tiros_area_sevilla <- renderTirosArea(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
  
    # Centros
    
    output$texto_centros_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses:", "</span>"))
    })
    
    output$centros_sevilla <- renderCentrosEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    
    output$texto_carriles_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Attack Zones:", "</span>"))
    })
    
    # Lineas de ataque
    output$carriles_sevilla <- renderFlechasEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    
    
    # tarjetas de información
    output$mediapases_sevilla <- renderPasesMediosEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    output$verticalidad_sevilla <- renderVerticalidadEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    output$porc_goles_faseof_sevilla <- renderPorcGoles(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")

    
    
    
    output$texto_finalizadores_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Players with the Most Shots:", "</span>"))
    })
    # jugadores más finalizadores
    output$finalizadores_sevilla <- renderFinalizadoresEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    output$texto_centradores_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Players with the Most Crosses:", "</span>"))
    })
    # jugadores más centradores
    output$centradores_sevilla <- renderCentradoresEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    
    ################################### INDIVIDUAL #####################################
    minutos_jugador <- datos_ft %>%
      filter(team == "Sevilla") %>%
      group_by(name, partido) %>%
      summarise(
        minutos = case_when(
          any(type.displayName == "SubstitutionOff") ~ min(90, max(minute[type.displayName == "SubstitutionOff"])),
          any(type.displayName == "SubstitutionOn") ~ max(0, min(90, 90 - min(minute[type.displayName == "SubstitutionOn"]))),
          TRUE ~ 90
        ),
        .groups = "drop"
      )
    
    
    minutos_por90 <- minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), por90 = sum(minutos)/90) %>% na.omit()
    
    
    
    output$texto_protagoles_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Goalscorer (per90):", "</span>"))
    })
    
    # protagonistas (goles (xG, xGOT), asistencias, centros, etc) goles cambios filtrados regates
    output$prota_goles <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", faceta = "goles", input, "sevilla")
  
    
    output$texto_protacentros_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Player with the Better Cross Success Ratio:", "</span>"))
    })
    output$prota_centros <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", faceta = "centros", input, "sevilla")
    
    
    
    output$texto_protafiltrados_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Player with the most Filtered Passes near Rival Box:", "</span>"))
    })
    output$prota_filtrados <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", faceta = "filtrados", input, "sevilla")
    
    
    output$texto_protacambios_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Player with the most Passes Changing Orientation (per90):", "</span>",
                  "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Orientation-Changing Passes as passes where the player received the ball from a side, and passed it to the other side in 2 contacts max.</span></span>
                      </span>"))
    })
    output$prota_cambios <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", faceta = "cambios", input, "sevilla")
    
    
    output$texto_protaasistencias_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Assistant (per90):", "</span>"))
    })
    output$prota_asistencias <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", faceta = "asistencias", input, "sevilla")
    
    
    ## GRÁFICOS PARA LOS: GOLES, CENTROS, FILTRADOS, ASISTENCIAS Y CAMBIOS
    # goles 
    tiros <- datos_ft %>% filter(team == "Sevilla") %>% filter(fasedejuego == "Construccion Ofensiva", type.displayName %in% c("MissedShots", "SavedShot", "Goal", "ShotOnPost"))
    
    conteo_tiros <- tiros %>% group_by(name) %>% summarise(a_puerta = sum(type.displayName %in% c("SavedShot", "Goal", "ShotOnPost")),
                                                           fuera = sum(type.displayName == "MissedShots"),
                                                           total = a_puerta + fuera,
                                                           goles = sum(type.displayName == "Goal"),
                                                           xG = sum(na.omit(xG)),
                                                           xGOT = sum(na.omit(xGOT)))
    
    # por90
    conteo_tiros <- merge(conteo_tiros, minutos_por90, by = "name")
    
    conteo_tiros <- conteo_tiros %>% mutate(goles_90 = round(goles / por90, 2),
                                            xG_90 = round(xG / por90, 2),
                                            xGOT_90 = round(xGOT / por90, 2))
    
    conteo_tiros <- conteo_tiros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(goles_90))
    

    goleador <- conteo_tiros$name[1]
    
    
    output$texto_tirosjugador_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots From ", goleador, "</span>"))
    })
    
    output$goles_jugador <- renderTirosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", player = goleador)
    
    
    
    
    # Centros
    centros <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == "Sevilla")
    centros <- sacar_centros(centros)
    
    centros <- centros %>% group_by(name) %>% summarise(total_centros = n(),
                                                        buenos_centros = sum(outcomeType.displayName == "Successful"),
                                                        malos_centros = sum(outcomeType.displayName == "Unuccessful"),
                                                        ratio = round(buenos_centros / total_centros * 100, 2))
    
    centros <- centros %>% filter(total_centros > max(centros$total_centros)/2) 
    
    centros <- merge(centros, minutos_por90, by = "name")
    
    centros <- centros %>% mutate(total_90 = round(total_centros / por90, 2),
                                  buenos_90 = round(buenos_centros / por90, 2),
                                  malos_90 = round(malos_centros / por90, 2))
    
    conteo_centros <- centros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(ratio))
    
    centrador <- conteo_centros$name[1]
    
    
    output$texto_centrosjugador_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses From ", centrador, "</span>"))
    })
    
    output$centros_jugador <- renderCentrosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", player = centrador)
    
    
    
    
    # Filtrados
    pases_filtrados <- datos_ft %>% filter(team == "Sevilla", fasedejuego == "Construccion Ofensiva", type.displayName == "Pass", x > 65, endX - x >= 10, abs(endY - y) < 15, between(y, 35, 65), outcomeType.displayName == "Successful")
    conteo_filtrados <- pases_filtrados %>% group_by(name) %>% summarise(pases_filtrados = n()) %>% arrange(desc(pases_filtrados))
    
    
    filtrador <- conteo_filtrados$name[1]
    
    output$texto_filtrosjugador_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Filtered Passes From ", filtrador, "</span>"))
    })
    
    output$filtros_jugador <- renderFiltrosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", player = filtrador)
    
    
    
    
    # Cambios
    observeEvent(input$excluir_portero_sevilla, {
      porteros <- ""
      if (input$excluir_portero_sevilla) {
        porteros <- jugadores %>% filter(team == "Sevilla", position == "Goalkeeper")
        porteros <- porteros$name
      }
      
      contactos_zona <- datos_ft %>% filter(team == "Sevilla", fasedejuego == "Construccion Ofensiva", type.displayName == "Pass", outcomeType.displayName == "Successful", !(name %in% porteros),
                                         ((lag(y) < 20) | (lag(y) > 80)) & between(y, 30, 60))
      
      contactos_zona <- contactos_zona %>% group_by(name) %>% summarise(contactos = n())
      
      
      cambios_orientacion <- datos_ft %>% filter(team == "Sevilla", fasedejuego == "Construccion Ofensiva", type.displayName == "Pass", outcomeType.displayName == "Successful", !(name %in% porteros),
                                                 ((lag(y) < 20) & between(y, 30, 60) & (endY > 80)) | ((lag(y) < 20) & between(y, 30, 60) & (endY < 80) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) > 80)) |
                                                 ((lag(y) > 80) & between(y, 30, 60) & (endY < 20)) | ((lag(y) > 80) & between(y, 30, 60) & (endY > 20) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) < 20))) 
      
      conteo_cambios <- cambios_orientacion %>% group_by(name) %>% summarise(cambios = n()) 
      
      conteo_cambios <- merge(conteo_cambios, contactos_zona, by = "name")
      
      conteo_cambios <- merge(conteo_cambios, minutos_por90, by = "name")
      
      conteo_cambios <- conteo_cambios %>% mutate(cambios_90 = round(cambios / por90, 2),
                                                  contactos_90 = round(contactos / por90, 2),
                                                  porc_cambios = round(cambios_90 / contactos_90 * 100, 2))
      
      conteo_cambios <- conteo_cambios %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2, cambios > max(conteo_cambios$cambios)/2) %>% arrange(desc(porc_cambios))
      
      cambiador <- conteo_cambios$name[1]
      
      output$texto_cambiosjugador_sevilla <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Orientation-Changing Passes From ", cambiador, "</span>",
                    "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>The dashed <span style='color:red;'>red</span> line is the previous pass, and the <span style='color:blue;'>blue</span> one is the next pass.</span></span>
                      </span>"))
      })
      
      output$cambios_jugador <- renderCambiosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", player = cambiador)
      
      
    })
    
   
    
    # Asistencias
    asis <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == "Sevilla")
    asistentes_gol <- asis %>% filter(type.displayName == "Goal") %>% mutate(aux = paste(partido, relatedEventId, sep = ", "))  
    name_asis_gol <- asis %>% mutate(aux2 = paste(partido, eventId, sep = ", ")) %>%  filter(aux2 %in% asistentes_gol$aux) %>% select(name)   
    
    
    asistentes <- name_asis_gol %>% group_by(name) %>% summarise(asistencias = n())
    
    asistentes <- merge(asistentes, minutos_por90, by = "name")
    
    asistentes <- asistentes %>% mutate(asistencias_90 = round(asistencias / por90, 2))
    
    asistentes <- asistentes %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(asistencias_90))
    
    asistente <- asistentes$name[1]
    
    
    
    output$texto_asistenciasjugador_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Assists From:", asistente, "</span>"))
    })
    output$asistencias_jugador <- renderAsistenciasJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", player = asistente)
    
    
  })
  
  #####
  ##### 2. TRANSICIÓN DEFENSA-ATAQUE 
  
  observe({
    
    datos_ft <- eventing %>% filter(str_detect(partido, "Sevilla"), competicion == input$selector_competi_tr_da, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_tr_da[1], input$selector_fechas_tr_da[2]))
    
    output$partidos_tr_da <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == "Sevilla") {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == "Sevilla") {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  # No debería suceder si Sevilla siempre está en el partido
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    
    output$porc_goles_trans_sevilla <- renderPorcGoles(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    output$porc_goles_transvert_sevilla <- renderPorcGoles(datos_ft, fasedj = "Transicion DEF-AT Vertical", equipo = "Sevilla")
    
    
    # robos y robos rápidos
    output$texto_robos_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Steals Zones:
                      </span>"))
    })
    
    output$robos_sevilla <- renderRecuperacionesEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    output$robosrapidos_sevilla <- renderRecuperacionesRapidasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
      
    # más recuperadores
    output$texto_recuperadores_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Stealers </span>"))
    })
  
    output$recuperadores_sevilla <- renderRecuperadoresEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    
    robos_rap <- datos_ft %>% filter(str_detect(fasedejuego, "Transicion DEF-AT"), team == "Sevilla") %>% filter(type.displayName %in% c("BallRecovery", "Interception")) 
    
    c_datos <- datos_ft %>% filter(team == "Sevilla")
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    robos_rapidos <- robos_rap %>% rowwise() %>% mutate(rapida = es_recuperacion_rapida_equipo(c_datos, minute*60+second, team, posesion, partido))
    robos_rapidos <- robos_rapidos %>% filter(rapida, type.displayName == "BallRecovery")
    
    ladrones_rapidos <- robos_rapidos %>% group_by(name) %>% summarise(robos_r = n()) %>% arrange(desc(robos_r))
    
    ladron_rapido <- ladrones_rapidos$name[1]
    
    output$ladron_rapido_sevilla <- renderUI({
      foto_url <- jugadores %>% filter(name == ladron_rapido, team == "Sevilla") %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(ladron_rapido, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      
      image_list <- list(tags$div(class = "ladron_rapido", img_tag, name_tag))
      
      tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
               do.call(tagList, image_list), 
               tags$div(style = "margin-left: 20px; font-size: 20px; font-weight: bold; margin-bottom: 20px;", HTML(paste(ladrones_rapidos$robos_r[1], "Quick Steals", "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Steals as steals produced, at most, 10 seconds after losing the ball.</span></span>
                      </span>"))))
    })
    
    
    # líneas de ataque
    output$texto_carrilestrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Attack Zones </span>"))
    })
    output$carrilestrans_sevilla <- renderFlechasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    # % verticales y finalizadas
    output$porcverticales_sevilla <- renderPorcVerticalesEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    output$porcfinalizadas_sevilla <- renderPorcFinalizadasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    
    output$texto_contrasverticales_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Vertical Transitions </span>"))
    })
    # contras verticales
    output$contrasverticales_sevilla <- renderContrasVerticalesEquipo(datos_ft, equipo = "Sevilla")
    ## html: jugador más finalizador, más recuperador en este aspecto
    
    # recuperador
    recup <- datos_ft %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == "Sevilla")
    recup <- recup %>% filter(type.displayName == "BallRecovery") %>% group_by(name) %>% summarise(recuperaciones = n()) %>% arrange(desc(recuperaciones))
    
    recuperador_vertical <- recup$name[1]
    
    output$texto_recuperador_vertical <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Most Recoveries Leading to a <br> Vertical Transition Completed By </span>"))
    })
    
    output$recuperador_vertical <- renderUI({
      foto_url <- jugadores %>% filter(name == recuperador_vertical, team == "Sevilla") %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(recuperador_vertical, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      
      tags$div(class = "recuperador_vertical", img_tag, name_tag)
    })
    
    # finalizador
    fin <- datos_ft %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == "Sevilla")
    fin <- fin %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal") %>% group_by(name) %>% summarise(tiros = n()) %>% arrange(desc(tiros))
    
    finalizador_vertical <- fin$name[1]
    
    output$texto_finalizador_vertical <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Most Shots in <br> Vertical Transitions Made By </span>"))
    })
    
    output$finalizador_vertical <- renderUI({
      foto_url <- jugadores %>% filter(name == finalizador_vertical, team == "Sevilla") %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(finalizador_vertical, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      tags$div(class = "finalizador_vertical", img_tag, name_tag)
    })
    
    
    output$texto_tirosfdatrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Out of the Box Shots and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    # tiros fuera del área
    output$tiros_fdatrans_sevilla <- renderTirosFDA(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    output$texto_tirosareatrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    # tiros en el área
    output$tiros_areatrans_sevilla <- renderTirosArea(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    
    output$texto_protagolestrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Goalscorer (per90):", "</span>"))
    })
    
    # protagonistas (goles (xG, xGOT), asistencias, centros, etc) goles cambios filtrados regates
    minutos_jugador <- datos_ft %>%
      filter(team == "Sevilla") %>%
      group_by(name, partido) %>%
      summarise(
        minutos = case_when(
          any(type.displayName == "SubstitutionOff") ~ min(90, max(minute[type.displayName == "SubstitutionOff"])),
          any(type.displayName == "SubstitutionOn") ~ max(0, min(90, 90 - min(minute[type.displayName == "SubstitutionOn"]))),
          TRUE ~ 90
        ),
        .groups = "drop"
      )
    
    
    minutos_por90 <- minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), por90 = sum(minutos)/90) %>% na.omit()
    
    
    
    output$prota_golestrans_sevilla <- renderProtasOfensivos(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla", faceta = "goles", input, "sevilla")
    
    
    tiros <- datos_ft %>% filter(team == "Sevilla") %>% filter(fasedejuego == "Transicion DEF-AT", type.displayName %in% c("MissedShots", "SavedShot", "Goal", "ShotOnPost"))
    
    conteo_tiros <- tiros %>% group_by(name) %>% summarise(a_puerta = sum(type.displayName %in% c("SavedShot", "Goal", "ShotOnPost")),
                                                           fuera = sum(type.displayName == "MissedShots"),
                                                           total = a_puerta + fuera,
                                                           goles = sum(type.displayName == "Goal"),
                                                           xG = sum(na.omit(xG)),
                                                           xGOT = sum(na.omit(xGOT)))
    
    conteo_tiros <- merge(conteo_tiros, minutos_por90, by = "name")
    
    conteo_tiros <- conteo_tiros %>% mutate(goles_90 = round(goles / por90, 2),
                                            xG_90 = round(xG / por90, 2),
                                            xGOT_90 = round(xGOT / por90, 2))
    
    conteo_tiros <- conteo_tiros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(goles_90))

    goleador <- conteo_tiros$name[1]
    
    
    output$texto_tirosjugadortrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots From ", goleador, "</span>"))
    })
    
    output$goles_jugadortrans_sevilla <- renderTirosJugador(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla", player = goleador)
    
    
  })
  
  
  #####
  ##### 3. FASE DEFENSIVA
  observe({
    datos_ft <- eventing %>% filter(str_detect(partido, "Sevilla"), competicion == input$selector_competi_def, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_def[1], input$selector_fechas_def[2]))
    
    output$partidos_def <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == "Sevilla") {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == "Sevilla") {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  # No debería suceder si Sevilla siempre está en el partido
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    output$texto_deftirosarea_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    # zonas del área donde más les rematan
    output$deftirosarea_sevilla <- renderDefTirosArea(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    # tiros fuera del área
    
    output$texto_deftirosfda_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Out of the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    output$deftirosfda_sevilla <- renderDefTirosFDA(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    # % goles recibidos fase ofensiva rival
    output$defgoles_sevilla <- renderDefPorcGoles(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    output$texto_defcarriles_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Attack Zones:", "</span>"))
    })
    # carriles rival
    output$defcarriles_sevilla <- renderDefFlechasEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    
    # PPDA
    output$defppda_sevilla <- renderPPDAEquipo(datos_ft, equipo = "Sevilla")
    
    output$texto_defcentros_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Crosses:", "</span>"))
    })
    # centros permitidos
    output$defcentros_sevilla <- renderDefCentrosEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla")
    
    
    # ataques en 3/4 en tiro y solventados por la defensa 
    output$deffinalizadas34_sevilla <- renderDefFinalizadas3cuartosEquipo(datos_ft, equipo = "Sevilla")
    output$defcortadas34_sevilla <- renderDefCortadas3cuartosEquipo(datos_ft, equipo = "Sevilla")
    
    
    output$texto_protadefensivo_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Players Defensively:", "</span>"))
    })
    # Protagonistas acciones defensivas
    output$protadefensivo_sevilla1 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", pos = 1)
    output$protadefensivo_sevilla2 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", pos = 2)
    output$protadefensivo_sevilla3 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", pos = 3)
    output$protadefensivo_sevilla4 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = "Sevilla", pos = 4)
    
    
  })
  
  #####
  ##### 4. TRANSICION DEFENSIVA
  observe({
    
    datos_ft <- eventing %>% filter(str_detect(partido, "Sevilla"), competicion == input$selector_competi_tr_ad, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_tr_ad[1], input$selector_fechas_tr_ad[2]))
    
    output$partidos_tr_ad <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == "Sevilla") {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == "Sevilla") {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  # No debería suceder si Sevilla siempre está en el partido
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    output$texto_deftirosareatrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    # zonas del área donde más les rematan
    output$deftirosareatrans_sevilla <- renderDefTirosArea(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    # tiros fuera del área
    
    output$texto_deftirosfdatrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Out of the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    output$deftirosfdatrans_sevilla <- renderDefTirosFDA(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    output$defgolestrans_sevilla <- renderDefPorcGoles(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    output$defgolesvertical_sevilla <- renderDefPorcGoles(datos_ft, fasedj = "Transicion DEF-AT Vertical", equipo = "Sevilla")
    
    
    # pérdidas y pérdidas rápidos
    output$texto_perdidas_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Turnovers Zones:
                      </span>"))
    })
    
    output$perdidas_sevilla <- renderPerdidasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    output$perdidasrapidas_sevilla <- renderPerdidasRapidasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    # más perdedores
    output$texto_perdedores_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Losers </span>"))
    })
    
    output$perdedores_sevilla <- renderPerdedoresEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    
    
    perdidas_rap <- datos_ft %>% filter(fasedejuego == "Transicion DEF-AT", team == "Sevilla") %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                    (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful"))
    
    c_datos <- datos_ft %>% filter(team != "Sevilla")
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    perdidas_rapidas <- perdidas_rap %>% rowwise() %>% mutate(rapida = es_perdida_rapida_equipo(c_datos, minute*60+second, team, posesion, partido))
    perdidas_rapidas <- perdidas_rapidas %>% filter(rapida, ((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                               (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                               (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                               (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")))
    
    
    perdidas_rapidas <- perdidas_rapidas %>% group_by(name) %>% summarise(n_perd = n()) %>% arrange(desc(n_perd))
    perdedor_rapido <- perdidas_rapidas$name[1]
    
    
    output$perdedor_rapido_sevilla <- renderUI({
      foto_url <- jugadores %>% filter(name == perdedor_rapido, team == "Sevilla") %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(perdedor_rapido, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      
      image_list <- list(tags$div(class = "perdedor_rapido", img_tag, name_tag))
      
      tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
               do.call(tagList, image_list), 
               tags$div(style = "margin-left: 20px; font-size: 20px; font-weight: bold; margin-bottom: 20px;", HTML(paste(perdidas_rapidas$n_perd[1], "Quick Turnovers", "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Turnovers as losses where the team does not have the possession for 10 seconds.</span>
                      </span>
                      </span>"))))
    })
    
    
    
    
    
    # líneas de ataque
    output$texto_defcarrilestrans_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Attack Zones </span>"))
    })
    output$defcarrilestrans_sevilla <- renderDefFlechasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    
    # % verticales y finalizadas
    output$defporcverticales_sevilla <- renderDefPorcVerticalesEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    output$defporcfinalizadas_sevilla <- renderDefPorcFinalizadasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = "Sevilla")
    
    
    output$texto_defcontrasverticales_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival vertical Transitions:", "</span>"))
    })
    # contras verticales
    output$defcontrasverticales_sevilla <- renderDefContrasVerticalesEquipo(datos_ft, equipo = "Sevilla")
    
  })
  
  # tiros recibidos
  
  #####
  ##### 5. ABP
  observe({
    
    datos_ft <- eventing %>% filter(str_detect(partido, "Sevilla"), competicion == input$selector_competi_abp, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_abp[1], input$selector_fechas_abp[2]))
    
    output$texto_abpfavor_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------  OFFENSIVE SET PIECE ------------------------", "</span>"))
    })
    
    output$texto_abpcontra_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:35px; font-weight:bold; color:red; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------  DEFENSIVE SET PIECE ------------------------", "</span>"))
    })
    
    output$partidos_abp <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == "Sevilla") {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == "Sevilla") {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  # No debería suceder si Sevilla siempre está en el partido
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    output$porc_goles_abp_sevilla <- renderPorcGoles(datos_ft, fasedj = "ABP", equipo = "Sevilla")
    
    
    ### A FAVOR ###
    aux <- datos_ft %>% filter(fasedejuego == "ABP", team == "Sevilla", x > 99, y < 1)
    jugadores_cd_sev <- unique(aux$name)
    
    output$jugadores_cornerd_sev <- renderUI({
      selectInput("jugadores_cornerd_sev", "Corners Taken by:", choices = jugadores_cd_sev, multiple = T, selected = jugadores_cd_sev[1])
    })
    
    output$texto_cornerder_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Right Side Corners:", "</span>"))
    })
    
    output$cornerder_sevilla <- renderCornersDerEquipo(datos_ft, equipo = "Sevilla", input, "sev")
    
    
    
    aux <- datos_ft %>% filter(fasedejuego == "ABP", team == "Sevilla", x > 99, y > 99)
    jugadores_ci_sev <- unique(aux$name)
    
    output$jugadores_corneri_sev <- renderUI({
      selectInput("jugadores_corneri_sev", "Corners Taken by:", choices = jugadores_ci_sev, multiple = T, selected = jugadores_ci_sev[1])
    })
    
    output$texto_cornerizq_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Left Side Corners:", "</span>"))
    })
    
    output$cornerizq_sevilla <- renderCornersIzqEquipo(datos_ft, equipo = "Sevilla", input, "sev")
      
    output$texto_faltascentro_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Fouls ended in the Box:", "</span>"))
    })
    
    output$faltascentro_sevilla <- renderFaltasCentroEquipo(datos_ft, equipo = "Sevilla")
    
    output$texto_libredirecto_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Direct Free Kicks:", "</span>"))
    })
    output$libredirecto_sevilla <- renderLibreDirectoEquipo(datos_ft, equipo = "Sevilla")
    
    
    output$texto_penales_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Penalties:", "</span>"))
    })
    output$penales_sevilla <- renderPenalesEquipo(datos_ft, equipo = "Sevilla")
    
    
    ### EN CONTRA ###
    output$porc_goles_defabp_sevilla <- renderDefPorcGoles(datos_ft, fasedj = "ABP", equipo = "Sevilla")
    
    
    output$texto_defcornerder_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Right Side Corners:", "</span>"))
    })
    output$defcornerder_sevilla <- renderDefCornersDerEquipo(datos_ft, equipo = "Sevilla")
    
    
    output$texto_defcornerizq_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Left Side Corners:", "</span>"))
    })
    output$defcornerizq_sevilla <- renderDefCornersIzqEquipo(datos_ft, equipo = "Sevilla")
    
    
    output$texto_deffaltascentro_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Fouls ended in the Box:", "</span>"))
    })
    output$deffaltascentro_sevilla <- renderDefFaltasCentroEquipo(datos_ft, equipo = "Sevilla")
    
    
    output$texto_deflibredirecto_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Direct Free Kicks:", "</span>"))
    })
    output$deflibredirecto_sevilla <- renderDefLibreDirectoEquipo(datos_ft, equipo = "Sevilla")
    
    
    output$texto_defpenales_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Penalties:", "</span>"))
    })
    output$defpenales_sevilla <- renderDefPenalesEquipo(datos_ft, equipo = "Sevilla")
    
    # intervenciones del portero en zona de control
    porteros <- jugadores %>% filter(team == "Sevilla", position == "Goalkeeper")
    porteros <- porteros$name
    
    output$portero_control <- renderUI({
      selectInput("portero_control", "Area Control from:", choices = porteros, multiple = F, selected = porteros[1])
    })
    
    observeEvent(input$portero_control, {
      
      output$porccontrol_por_sevilla <- renderUI({
        
        ## porcentaje
        partidos_por <- datos_ft %>% group_by(partido) %>% summarise(esta_portero = input$portero_control %in% name) %>% filter(esta_portero)
        partidos_por <- partidos_por$partido
        
        
        abp_portero <- datos_ft %>% group_by(partido, posesion) %>% filter(partido %in% partidos_por, team != "Sevilla", fasedejuego == "ABP") %>% filter(any(between(endX, 95, 99.9) & between(endY, 40, 60))) %>% mutate(par_pos = paste(partido, posesion, sep = ", "))
        
        
        posesiones_portero_full <- datos_ft %>% mutate(sig_actor = lead(name), sig_accion = lead(type.displayName)) %>% group_by(partido, posesion) %>% mutate(par_pos2 = paste(partido, posesion, sep = ", ")) %>% filter(par_pos2 %in% abp_portero$par_pos)
        
        
        intervenciones <- posesiones_portero_full %>% group_by(partido, posesion) %>% summarise(accion_portero = ifelse(input$portero_control %in% sig_actor, T, F))
        
        porc_acciones <- round(sum(intervenciones$accion_portero)/nrow(intervenciones)*100, 2)
        ###
        
        ### html
        nombre_tio <- input$portero_control
        
        foto_url <- jugadores %>% filter(name == nombre_tio, team == "Sevilla") %>% select(foto) %>% pull()
        
        if (is.na(foto_url) || length(foto_url) == 0) {
          foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
        }
        
        
        categorias <- "% Of Actions in this Zone with GK Presence (Actions/Total)"
        resultados <- paste0(porc_acciones, " (", sum(intervenciones$accion_portero), "/", nrow(intervenciones), ")")
        
        
        # Crear la tabla personalizada
        tabla_html <- htmltools::HTML(
          paste0(
            '<table class="table table-striped" style="width: 100%; border: 3px solid black;">',
            '<thead>',
            '<tr>',
            '<th colspan="5" style="text-align:center; border-bottom: 3px solid black; font-size: 20px; font-family: Arial, sans-serif;">',
            '<div style="display: flex; align-items: center; justify-content: center; margin-bottom: 10px; margin-top: 10px;">',
            '<img src="', foto_url, '" style="width:100px;height:100px; vertical-align: middle; margin-right: 40px;">',
            nombre_tio,
            '</th>',
            '</tr>',
            '</thead>',
            '<tbody>',
            '<tr>',
            '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias), '</td>',
            '</tr>',
            '<tr>',
            '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados), '</td>',
            '</tr>',
            '</tbody>',
            '</table>'
          )
        )
        
        htmltools::HTML(tabla_html)
      })
    })
    
    output$texto_zonadecontrol_por_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Area Control Zone:", "</span>",
                  "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>The Keeper dominates this zone if the % of actions ended in this rectangle in Rival Corners where he appears is high.</span></span>
                      </span>"))
    })
    
    output$zonadecontrol_por_sevilla <- renderControlPorteria()
    
    output$texto_protadefabp_sevilla <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Set Piece Defenders:", "</span>"))
    })
    output$protadefabp_sevilla1 <- renderProtasDefensivo(datos_ft, fasedj = "ABP", equipo = "Sevilla", pos = 1)
    output$protadefabp_sevilla2 <- renderProtasDefensivo(datos_ft, fasedj = "ABP", equipo = "Sevilla", pos = 2)
  })
  
  
  #################
  ##### RIVAL #####
  #################
  selectedTeam_of <- reactiveVal(NULL)
  
  observeEvent(input$selector_competi_rival_of, {
    output$selector_equipo_rival_of <- renderUI({
      selectInput("selector_equipo_rival_of", "Select Rival Team", choices = unique(eventing$team[eventing$competicion == input$selector_competi_rival_of]))
    })
  })
  
  observeEvent(input$selector_equipo_rival_of, {
    selectedTeam_of(input$selector_equipo_rival_of)
    competi <- competis[competis == input$selector_competi_rival_of]
    output$img_rival_of <- renderPlot({mostrar_escudo(paste0("Escudos/", competi, "/", input$selector_equipo_rival_of, ".png"))})
  })

  output$texto_fase_of2 <- renderUI({
    HTML(paste0("<span style='font-size:35px; font-weight:bold; color:black; margin-left: 40px'>", "Offensive Organization", "</span>"))
  })
  
  
  selectedTeam_def <- reactiveVal(NULL)
  
  observeEvent(input$selector_competi_rival_def, {
    output$selector_equipo_rival_def <- renderUI({
      selectInput("selector_equipo_rival_def", "Select Rival Team", choices = unique(eventing$team[eventing$competicion == input$selector_competi_rival_def]))
    })
  })
  
  observeEvent(input$selector_equipo_rival_def, {
    selectedTeam_def(input$selector_equipo_rival_def)
    competi <- competis[competis == input$selector_competi_rival_def]
    output$img_rival_def <- renderPlot({mostrar_escudo(paste0("Escudos/", competi, "/", input$selector_equipo_rival_def, ".png"))})
  })
  
  output$texto_fase_def2 <- renderUI({
    HTML(paste0("<span style='font-size:35px; font-weight:bold; color:black; margin-left: 40px'>", "Defensive Organization", "</span>"))
  })
  
  
  selectedTeam_tr_da <- reactiveVal(NULL)
  
  observeEvent(input$selector_competi_rival_tr_da, {
    output$selector_equipo_rival_tr_da <- renderUI({
      selectInput("selector_equipo_rival_tr_da", "Select Rival Team", choices = unique(eventing$team[eventing$competicion == input$selector_competi_rival_tr_da]))
    })
  })
  
  observeEvent(input$selector_equipo_rival_tr_da, {
    selectedTeam_tr_da(input$selector_equipo_rival_tr_da)
    competi <- competis[competis == input$selector_competi_rival_tr_da]
    output$img_rival_tr_da <- renderPlot({mostrar_escudo(paste0("Escudos/", competi, "/", input$selector_equipo_rival_tr_da, ".png"))})
  })
  
  output$texto_fase_tr_da2 <- renderUI({
    HTML(paste0("<span style='font-size:30px; font-weight:bold; color:black; margin-left: 40px'>", "Defense-Offense Transition", "</span>"))
  })
  
  
  
  selectedTeam_tr_ad <- reactiveVal(NULL)
  
  observeEvent(input$selector_competi_rival_tr_ad, {
    output$selector_equipo_rival_tr_ad <- renderUI({
      selectInput("selector_equipo_rival_tr_ad", "Select Rival Team", choices = unique(eventing$team[eventing$competicion == input$selector_competi_rival_tr_ad]))
    })
  })
  
  observeEvent(input$selector_equipo_rival_tr_ad, {
    selectedTeam_tr_ad(input$selector_equipo_rival_tr_ad)
    competi <- competis[competis == input$selector_competi_rival_tr_ad]
    output$img_rival_tr_ad <- renderPlot({mostrar_escudo(paste0("Escudos/", competi, "/", input$selector_equipo_rival_tr_ad, ".png"))})
  })

  output$texto_fase_tr_ad2 <- renderUI({
    HTML(paste0("<span style='font-size:30px; font-weight:bold; color:black; margin-left: 40px'>", "Offense-Defense Transition", "</span>"))
  })
  
  
  
  
  selectedTeam_abp <- reactiveVal(NULL)
  
  observeEvent(input$selector_competi_rival_abp, {
    output$selector_equipo_rival_abp <- renderUI({
      selectInput("selector_equipo_rival_abp", "Select Rival Team", choices = unique(eventing$team[eventing$competicion == input$selector_competi_rival_abp]))
    })
  })
  
  observeEvent(input$selector_equipo_rival_abp, {
    selectedTeam_abp(input$selector_equipo_rival_abp)
    competi <- competis[competis == input$selector_competi_rival_abp]
    output$img_rival_abp <- renderPlot({mostrar_escudo(paste0("Escudos/", competi, "/", input$selector_equipo_rival_abp, ".png"))})
  })
  
  output$texto_fase_abp2 <- renderUI({
    HTML(paste0("<span style='font-size:30px; font-weight:bold; color:black; margin-left: 40px'>", "Set Piece", "</span>"))
  })
  
  #####
  ##### 1.- FASE OFENSIVA
  observe({
    req(selectedTeam_of())
    
    equipin_rival <- selectedTeam_of()
    
    observeEvent(input$selector_fechas_of2, {
    
      datos_ft <- eventing %>% filter(str_detect(partido, equipin_rival), competicion == input$selector_competi_rival_of, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_of2[1], input$selector_fechas_of2[2]))
      
      output$partidos_of2 <- renderUI({
        
        datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
        partidos <- unique(datos_ft2$partido)
        
        rivales <- sapply(partidos, function(partido) {
          equipos <- str_split(partido, " - ")[[1]]
          if (equipos[1] == equipin_rival) {
            return(paste(equipos[2], "(H)"))
          } else if (equipos[2] == equipin_rival) {
            return(paste(equipos[1], "(A)"))
          } else {
            return(NULL)  
          }
        })
        
        rivales <- rivales[!is.null(rivales)]
        
        HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
        
      })
      
      output$texto_saques_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> GoalKeeper Starts:", "</span>"))
      })
      
      # saques del portero
      output$saquesportero_rival <- renderIniciosPortero(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      # salidas de la defensa
      # aux <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == equipin_rival)
      # aux <- aux %>% filter(type.displayName == "Pass", ((x < 5 & (y > 75 | y < 25)) | (between(x, 5.1, 35))))
      
      # output$jugadores_salida_rival <- renderUI({
      #   selectInput("jugadores_salida_rival", "Players to watch", choices = unique(aux$name), selected = unique(aux$name), multiple = T)
      # })
      
      output$texto_defensa_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Defense Construction Zones:", "</span>"))
      })
      
      output$salidasdefensa_rival <- renderSalidasDefensa(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      # salidas del medio centro
      #aux <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == equipin_rival)
      #aux <- aux %>% filter(type.displayName == "Pass", ((between(x, 40, 70) & (y > 85 | y < 15))))
      
      #output$jugadores_medio_rival <- renderUI({
      #  selectInput("jugadores_medio_rival", "Players to watch", choices = unique(aux$name), selected = unique(aux$name), multiple = T)
      #})
      
      output$texto_medio_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Midfielders Distribution Zones:", "</span>"))
      })
      
      output$salidasmedio_rival <- renderSalidasMC(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      
      output$texto_tirosfda_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Out of the Box Shots and <span style='color:green;'>Goals</span>:", "</span>"))
      })
      
      # tiros fuera del área
      output$tiros_fda_rival <- renderTirosFDA(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      output$texto_tirosarea_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
      })
      
      # tiros en el área
      output$tiros_area_rival <- renderTirosArea(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      # Centros
      
      output$texto_centros_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses:", "</span>"))
      })
      
      output$centros_rival <- renderCentrosEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      
      output$texto_carriles_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Attack Zones:", "</span>"))
      })
      
      # Lineas de ataque
      output$carriles_rival <- renderFlechasEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      
      
      # tarjetas de información
      output$mediapases_rival <- renderPasesMediosEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      output$verticalidad_rival <- renderVerticalidadEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      output$porc_goles_faseof_rival <- renderPorcGoles(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)

      
      output$texto_finalizadores_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Players with the Most Shots:", "</span>"))
      })
      # jugadores más finalizadores
      output$finalizadores_rival <- renderFinalizadoresEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      output$texto_centradores_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Players with the Most Crosses:", "</span>"))
      })
      # jugadores más centradores
      output$centradores_rival <- renderCentradoresEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
      
      
      ######################################## INDIVIDUAL ###############################################
      minutos_jugador <- datos_ft %>%
        filter(team == equipin_rival) %>%
        group_by(name, partido) %>%
        summarise(
          minutos = case_when(
            any(type.displayName == "SubstitutionOff") ~ min(90, max(minute[type.displayName == "SubstitutionOff"])),
            any(type.displayName == "SubstitutionOn") ~ max(0, min(90, 90 - min(minute[type.displayName == "SubstitutionOn"]))),
            TRUE ~ 90
          ),
          .groups = "drop"
        )
      
      
      minutos_por90 <- minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), por90 = sum(minutos)/90) %>% na.omit()
      
      
      
      output$texto_protagoles_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Goalscorer (per90):", "</span>"))
      })
      
      # protagonistas (goles (xG, xGOT), asistencias, centros, etc) goles cambios filtrados regates
      output$prota_goles_rival <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, faceta = "goles", input, "rival")
      
      
      output$texto_protacentros_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Player with the Better Cross Success Ratio:", "</span>"))
      })
      output$prota_centros_rival <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, faceta = "centros", input, "rival")
      
      
      
      output$texto_protafiltrados_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Player with the most Filtered Passes near Rival Box:", "</span>"))
      })
      output$prota_filtrados_rival <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, faceta = "filtrados", input, "rival")
      
      
      output$texto_protacambios_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Player with the most Passes Changing Orientation (per90):", "</span>",
                    "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Orientation-Changing Passes as passes where the player received the ball from a side, and passed it to the other side in 2 contacts max.</span></span>
                      </span>"))
      })
      output$prota_cambios_rival <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, faceta = "cambios", input, "rival")
      
      
      output$texto_protaasistencias_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Assistant (per90):", "</span>"))
      })
      output$prota_asistencias_rival <- renderProtasOfensivos(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, faceta = "asistencias", input, "rival")
      
      
      ## GRÁFICOS PARA LOS: GOLES, CENTROS, FILTRADOS, ASISTENCIAS Y CAMBIOS
      # goles 
      tiros <- datos_ft %>% filter(team == equipin_rival) %>% filter(fasedejuego == "Construccion Ofensiva", type.displayName %in% c("MissedShots", "SavedShot", "Goal", "ShotOnPost"))
      
      conteo_tiros <- tiros %>% group_by(name) %>% summarise(a_puerta = sum(type.displayName %in% c("SavedShot", "Goal", "ShotOnPost")),
                                                             fuera = sum(type.displayName == "MissedShots"),
                                                             total = a_puerta + fuera,
                                                             goles = sum(type.displayName == "Goal"),
                                                             xG = sum(na.omit(xG)),
                                                             xGOT = sum(na.omit(xGOT)))
      
      conteo_tiros <- merge(conteo_tiros, minutos_por90, by = "name")
      
      conteo_tiros <- conteo_tiros %>% mutate(goles_90 = round(goles / por90, 2),
                                              xG_90 = round(xG / por90, 2),
                                              xGOT_90 = round(xGOT / por90, 2))
      
      conteo_tiros <- conteo_tiros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(goles_90))
      
      goleador <- conteo_tiros$name[1]
      
      
      output$texto_tirosjugador_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots From ", goleador, "</span>"))
      })
      
      output$goles_jugador_rival <- renderTirosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, player = goleador)
      
      
      
      # Centros
      centros <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == equipin_rival)
      centros <- sacar_centros(centros)
      
      centros <- centros %>% group_by(name) %>% summarise(total_centros = n(),
                                                          buenos_centros = sum(outcomeType.displayName == "Successful"),
                                                          malos_centros = sum(outcomeType.displayName == "Unuccessful"),
                                                          ratio = round(buenos_centros / total_centros * 100, 2))
      
      centros <- centros %>% filter(total_centros > max(centros$total_centros)/2) 
      
      centros <- merge(centros, minutos_por90, by = "name")
      
      centros <- centros %>% mutate(total_90 = round(total_centros / por90, 2),
                                    buenos_90 = round(buenos_centros / por90, 2),
                                    malos_90 = round(malos_centros / por90, 2))
      
      conteo_centros <- centros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(ratio))
      
      centrador <- conteo_centros$name[1]
      
      
      output$texto_centrosjugador_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Crosses From ", centrador, "</span>"))
      })
      
      output$centros_jugador_rival <- renderCentrosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, player = centrador)
      
      
      # Filtrados
      pases_filtrados <- datos_ft %>% filter(team == equipin_rival, fasedejuego == "Construccion Ofensiva", type.displayName == "Pass", x > 65, endX - x >= 10, abs(endY - y) < 15, between(y, 35, 65), outcomeType.displayName == "Successful")
      conteo_filtrados <- pases_filtrados %>% group_by(name) %>% summarise(pases_filtrados = n()) %>% arrange(desc(pases_filtrados))
      
      
      filtrador <- conteo_filtrados$name[1]
      
      output$texto_filtrosjugador_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Filtered Passes From ", filtrador, "</span>"))
      })
      
      output$filtros_jugador_rival <- renderFiltrosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, player = filtrador)
      
      
      
      # Cambios
      observeEvent(input$excluir_portero_rival, {
        porteros <- ""
        if (input$excluir_portero_rival) {
          porteros <- jugadores %>% filter(team == equipin_rival, position == "Goalkeeper")
          porteros <- porteros$name
        }
        
        
        contactos_zona <- datos_ft %>% filter(team == equipin_rival, fasedejuego == "Construccion Ofensiva", type.displayName == "Pass", outcomeType.displayName == "Successful", !(name %in% porteros),
                                           ((lag(y) < 20) | (lag(y) > 80)) & between(y, 30, 60))
        
        contactos_zona <- contactos_zona %>% group_by(name) %>% summarise(contactos = n())
        
        
        cambios_orientacion <- datos_ft %>% filter(team == equipin_rival, fasedejuego == "Construccion Ofensiva", type.displayName == "Pass", outcomeType.displayName == "Successful", !(name %in% porteros),
                                                   ((lag(y) < 20) & between(y, 30, 60) & (endY > 80)) | ((lag(y) < 20) & between(y, 30, 60) & (endY < 80) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) > 80)) |
                                                     ((lag(y) > 80) & between(y, 30, 60) & (endY < 20)) | ((lag(y) > 80) & between(y, 30, 60) & (endY > 20) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) < 20))) 
        
        conteo_cambios <- cambios_orientacion %>% group_by(name) %>% summarise(cambios = n()) 
        
        conteo_cambios <- merge(conteo_cambios, contactos_zona, by = "name")
        
        conteo_cambios <- merge(conteo_cambios, minutos_por90, by = "name")
        
        conteo_cambios <- conteo_cambios %>% mutate(cambios_90 = round(cambios / por90, 2),
                                                    contactos_90 = round(contactos / por90, 2),
                                                    porc_cambios = round(cambios_90 / contactos_90 * 100, 2))
        
        conteo_cambios <- conteo_cambios %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2, cambios > max(conteo_cambios$cambios)/2) %>% arrange(desc(porc_cambios))
        
        cambiador <- conteo_cambios$name[1]
        
        output$texto_cambiosjugador_rival <- renderUI({
          HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Orientation-Changing Passes From ", cambiador, "</span>",
                      "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>The dashed <span style='color:red;'>red</span> line is the previous pass, and the <span style='color:blue;'>blue</span> one is the next pass.</span></span>
                      </span>"))
        })
        
        output$cambios_jugador_rival <- renderCambiosJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, player = cambiador)
        
        
      })
      
      # Asistencias
      asis <- datos_ft %>% filter(fasedejuego == "Construccion Ofensiva", team == equipin_rival)
      asistentes_gol <- asis %>% filter(type.displayName == "Goal") %>% mutate(aux = paste(partido, relatedEventId, sep = ", "))  
      name_asis_gol <- asis %>% mutate(aux2 = paste(partido, eventId, sep = ", ")) %>%  filter(aux2 %in% asistentes_gol$aux) %>% select(name)   
      
      
      asistentes <- name_asis_gol %>% group_by(name) %>% summarise(asistencias = n())
      
      asistentes <- merge(asistentes, minutos_por90, by = "name")
      
      asistentes <- asistentes %>% mutate(asistencias_90 = round(asistencias / por90, 2))
      
      asistentes <- asistentes %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(asistencias_90))
      
      asistente <- asistentes$name[1]
      
      
      output$texto_asistenciasjugador_rival <- renderUI({
        HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Assists From:", asistente, "</span>"))
      })
      output$asistencias_jugador_rival <- renderAsistenciasJugador(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, player = asistente)
      
      
    })
  })
  
  
  
  
  #####
  ##### 2. TRANSICIÓN DEFENSA-ATAQUE 
  observe({
    req(selectedTeam_tr_da())
    
    equipin_rival <- selectedTeam_tr_da()
    
    observeEvent(input$selector_fechas_tr_da2, {
    
    datos_ft <- eventing %>% filter(str_detect(partido, equipin_rival), competicion == input$selector_competi_rival_tr_da, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_tr_da2[1], input$selector_fechas_tr_da2[2]))
    
    output$partidos_tr_da2 <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == equipin_rival) {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == equipin_rival) {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    
    output$porc_goles_trans_rival <- renderPorcGoles(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    output$porc_goles_transvert_rival <- renderPorcGoles(datos_ft, fasedj = "Transicion DEF-AT Vertical", equipo = equipin_rival)
    
    
    # robos y robos rápidos
    output$texto_robos_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Steals Zones:
                      </span>"))
    })
    
    output$robos_rival <- renderRecuperacionesEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    output$robosrapidos_rival <- renderRecuperacionesRapidasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    # más recuperadores
    output$texto_recuperadores_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Stealers </span>"))
    })
    
    output$recuperadores_rival <- renderRecuperadoresEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    
    
    robos_rap <- datos_ft %>% filter(str_detect(fasedejuego, "Transicion DEF-AT"), team == equipin_rival) %>% filter(type.displayName %in% c("BallRecovery", "Interception")) 
    
    c_datos <- datos_ft %>% filter(team == equipin_rival)
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    robos_rapidos <- robos_rap %>% rowwise() %>% mutate(rapida = es_recuperacion_rapida_equipo(c_datos, minute*60+second, team, posesion, partido))
    robos_rapidos <- robos_rapidos %>% filter(rapida, type.displayName == "BallRecovery")
    
    ladrones_rapidos <- robos_rapidos %>% group_by(name) %>% summarise(robos_r = n()) %>% arrange(desc(robos_r))
    
    ladron_rapido <- ladrones_rapidos$name[1]
    
    output$ladron_rapido_rival <- renderUI({
      foto_url <- jugadores %>% filter(name == ladron_rapido, team == equipin_rival) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(ladron_rapido, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      
      image_list <- list(tags$div(class = "ladron_rapido", img_tag, name_tag))
      
      tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
               do.call(tagList, image_list), 
               tags$div(style = "margin-left: 20px; font-size: 20px; font-weight: bold; margin-bottom: 20px;", HTML(paste(ladrones_rapidos$robos_r[1], "Quick Steals", "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Steals as steals produced, at most, 10 seconds after losing the ball.</span>
                      </span>
                      </span>"))))
    })
    
    
    
    # líneas de ataque
    output$texto_carrilestrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Attack Zones </span>"))
    })
    output$carrilestrans_rival <- renderFlechasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    # % verticales y finalizadas
    output$porcverticales_rival <- renderPorcVerticalesEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    output$porcfinalizadas_rival <- renderPorcFinalizadasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    
    output$texto_contrasverticales_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Vertical Transitions </span>"))
    })
    # contras verticales
    output$contrasverticales_rival <- renderContrasVerticalesEquipo(datos_ft, equipo = equipin_rival)
    ## html: jugador más finalizador, más recuperador en este aspecto
    
    # recuperador
    recup <- datos_ft %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == equipin_rival)
    recup <- recup %>% filter(type.displayName == "BallRecovery") %>% group_by(name) %>% summarise(recuperaciones = n()) %>% arrange(desc(recuperaciones))
    
    recuperador_vertical_rival <- recup$name[1]
    
    output$texto_recuperador_vertical_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Most Recoveries Leading to a <br> Vertical Transition Completed By </span>"))
    })
    
    output$recuperador_vertical_rival <- renderUI({
      foto_url <- jugadores %>% filter(name == recuperador_vertical_rival, team == equipin_rival) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(recuperador_vertical_rival, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      
      tags$div(class = "recuperador_vertical_rival", img_tag, name_tag)
    })
    
    # finalizador
    fin <- datos_ft %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical"), team == equipin_rival)
    fin <- fin %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal") %>% group_by(name) %>% summarise(tiros = n()) %>% arrange(desc(tiros))
    
    finalizador_vertical_rival <- fin$name[1]
    
    output$texto_finalizador_vertical_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Most Shots in <br> Vertical Transitions Made By </span>"))
    })
    
    output$finalizador_vertical_rival <- renderUI({
      foto_url <- jugadores %>% filter(name == finalizador_vertical_rival, team == equipin_rival) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(finalizador_vertical_rival, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      tags$div(class = "finalizador_vertical_rival", img_tag, name_tag)
    })
    
    
    output$texto_tirosfdatrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Out of the Box Shots and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    # tiros fuera del área
    output$tiros_fdatrans_rival <- renderTirosFDA(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    output$texto_tirosareatrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    # tiros en el área
    output$tiros_areatrans_rival <- renderTirosArea(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    
    # máximo goleador fase
    output$texto_protagolestrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Goalscorer (per90):", "</span>"))
    })
    
    # protagonistas (goles (xG, xGOT), asistencias, centros, etc) goles cambios filtrados regates
    minutos_jugador <- datos_ft %>%
      filter(team == equipin_rival) %>%
      group_by(name, partido) %>%
      summarise(
        minutos = case_when(
          any(type.displayName == "SubstitutionOff") ~ min(90, max(minute[type.displayName == "SubstitutionOff"])),
          any(type.displayName == "SubstitutionOn") ~ max(0, min(90, 90 - min(minute[type.displayName == "SubstitutionOn"]))),
          TRUE ~ 90
        ),
        .groups = "drop"
      )
    
    
    minutos_por90 <- minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), por90 = sum(minutos)/90) %>% na.omit()
    
    
    output$prota_golestrans_rival <- renderProtasOfensivos(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival, faceta = "goles", input, "rival")
    
    
    tiros <- datos_ft %>% filter(team == equipin_rival) %>% filter(fasedejuego == "Transicion DEF-AT", type.displayName %in% c("MissedShots", "SavedShot", "Goal", "ShotOnPost"))
    
    conteo_tiros <- tiros %>% group_by(name) %>% summarise(a_puerta = sum(type.displayName %in% c("SavedShot", "Goal", "ShotOnPost")),
                                                           fuera = sum(type.displayName == "MissedShots"),
                                                           total = a_puerta + fuera,
                                                           goles = sum(type.displayName == "Goal"),
                                                           xG = sum(na.omit(xG)),
                                                           xGOT = sum(na.omit(xGOT)))
    
    conteo_tiros <- merge(conteo_tiros, minutos_por90, by = "name")
    
    conteo_tiros <- conteo_tiros %>% mutate(goles_90 = round(goles / por90, 2),
                                            xG_90 = round(xG / por90, 2),
                                            xGOT_90 = round(xGOT / por90, 2))
    
    conteo_tiros <- conteo_tiros %>% filter(minutosjugados > max(minutos_por90$minutosjugados)/2) %>% arrange(desc(goles_90))
    
    goleador <- conteo_tiros$name[1]
    
    
    output$texto_tirosjugadortrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Shots From ", goleador, "</span>"))
    })
    
    output$goles_jugadortrans_rival <- renderTirosJugador(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival, player = goleador)
    
    
    
  })
  })
  
  
  
  
  #####
  ##### 3. FASE DEFENSIVA
  observe({
    req(selectedTeam_def())
    
    equipin_rival <- selectedTeam_def()
    
    observeEvent(input$selector_fechas_def2, {
    datos_ft <- eventing %>% filter(str_detect(partido, equipin_rival), competicion == input$selector_competi_rival_def, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_def2[1], input$selector_fechas_def2[2]))
    
    output$partidos_def2 <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == equipin_rival) {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == equipin_rival) {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    output$texto_deftirosarea_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    # zonas del área donde más les rematan
    output$deftirosarea_rival <- renderDefTirosArea(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
    # tiros fuera del área
    
    output$texto_deftirosfda_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Out of the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    output$deftirosfda_rival <- renderDefTirosFDA(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
    
    # % goles recibidos fase ofensiva rival
    output$defgoles_rival <- renderDefPorcGoles(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
    
    output$texto_defcarriles_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Attack Zones:", "</span>"))
    })
    # carriles rival
    output$defcarriles_rival <- renderDefFlechasEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
    
    
    # PPDA
    output$defppda_rival <- renderPPDAEquipo(datos_ft, equipo = equipin_rival)
    
    output$texto_defcentros_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Crosses:", "</span>"))
    })
    # centros permitidos
    output$defcentros_rival <- renderDefCentrosEquipo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival)
    
    
    # ataques en 3/4 en tiro y solventados por la defensa 
    output$deffinalizadas34_rival <- renderDefFinalizadas3cuartosEquipo(datos_ft, equipo = equipin_rival)
    output$defcortadas34_rival <- renderDefCortadas3cuartosEquipo(datos_ft, equipo = equipin_rival)
    
    
    output$texto_protadefensivo_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Players Defensively:", "</span>"))
    })
    # Protagonistas acciones defensivas
    output$protadefensivo_rival1 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, pos = 1)
    output$protadefensivo_rival2 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, pos = 2)
    output$protadefensivo_rival3 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, pos = 3)
    output$protadefensivo_rival4 <- renderProtasDefensivo(datos_ft, fasedj = "Construccion Ofensiva", equipo = equipin_rival, pos = 4)
    
    
  })
  })
  
  
  
  #####
  ##### 4. TRANSICION DEFENSIVA
  observe({
    req(selectedTeam_tr_ad())
    
    equipin_rival <- selectedTeam_tr_ad()
    
    observeEvent(input$selector_fechas_tr_ad2, {
    
    datos_ft <- eventing %>% filter(str_detect(partido, equipin_rival), competicion == input$selector_competi_rival_tr_ad, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_tr_ad2[1], input$selector_fechas_tr_ad2[2]))
    
    output$partidos_tr_ad2 <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == equipin_rival) {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == equipin_rival) {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    output$texto_deftirosareatrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Inside the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    # zonas del área donde más les rematan
    output$deftirosareatrans_rival <- renderDefTirosArea(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    # tiros fuera del área
    
    output$texto_deftirosfdatrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Shots Out of the Box and <span style='color:green;'>Goals</span>:", "</span>"))
    })
    
    output$deftirosfdatrans_rival <- renderDefTirosFDA(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    output$defgolestrans_rival <- renderDefPorcGoles(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    output$defgolesvertical_rival <- renderDefPorcGoles(datos_ft, fasedj = "Transicion DEF-AT Vertical", equipo = equipin_rival)
    
    
    # pérdidas y pérdidas rápidos
    output$texto_perdidas_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>
                      <br> <br> Turnovers Zones:
                      </span>"))
    })
    
    output$perdidas_rival <- renderPerdidasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    output$perdidasrapidas_rival <- renderPerdidasRapidasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    # más perdedores
    output$texto_perdedores_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Losers </span>"))
    })
    
    output$perdedores_rival <- renderPerdedoresEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    
    perdidas_rap <- datos_ft %>% filter(fasedejuego == "Transicion DEF-AT", team == equipin_rival) %>% filter((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                                                                            (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful"))
    
    c_datos <- datos_ft %>% filter(team != equipin_rival)
    c_datos$tiempo <- c_datos$minute*60 + c_datos$second
    
    perdidas_rapidas <- perdidas_rap %>% rowwise() %>% mutate(rapida = es_perdida_rapida_equipo(c_datos, minute*60+second, team, posesion, partido))
    perdidas_rapidas <- perdidas_rapidas %>% filter(rapida, ((type.displayName == "Pass" & outcomeType.displayName == "Unsuccessful") |
                                                               (type.displayName == "Dispossessed" & outcomeType.displayName == "Successful") |
                                                               (type.displayName == "BallTouch" & outcomeType.displayName == "Unsuccessful") |
                                                               (type.displayName == "TakeOn" & outcomeType.displayName == "Unsuccessful")))
    
    
    perdidas_rapidas <- perdidas_rapidas %>% group_by(name) %>% summarise(n_perd = n()) %>% arrange(desc(n_perd))
    perdedor_rapido <- perdidas_rapidas$name[1]
    
    
    output$perdedor_rapido_rival <- renderUI({
      foto_url <- jugadores %>% filter(name == perdedor_rapido, team == equipin_rival) %>% select(foto) %>% pull()
      # Si la foto no existe, usar un marcador de posición
      if (is.na(foto_url) || length(foto_url) == 0) {
        foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
      }
      
      img_tag <- tags$img(src = foto_url, style = "width: 60px; height: 70px; margin: 0 17px;")
      name_tag <- tags$p(perdedor_rapido, style = "margin-top: 3px; font-size: 11px; font-weight: bold; margin: 0 17px;")
      image_list <- list(tags$div(class = "perdedor_rapido", img_tag, name_tag))
      
      tags$div(style = "display: flex; align-items: center; margin-bottom: 20px;",
               do.call(tagList, image_list), 
               tags$div(style = "margin-left: 20px; font-size: 20px; font-weight: bold; margin-bottom: 20px;", HTML(paste(perdidas_rapidas$n_perd[1], "Quick Losses", "<span class='tooltip-icon'>?
                      <span class='tooltip-text'>Considering Quick Turnovers as losses where the team does not have the possession for 10 seconds.</span>
                      </span>
                      </span>"))))
      
    })
    
    
    
    # líneas de ataque
    output$texto_defcarrilestrans_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Attack Zones </span>"))
    })
    output$defcarrilestrans_rival <- renderDefFlechasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    
    # % verticales y finalizadas
    output$defporcverticales_rival <- renderDefPorcVerticalesEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    output$defporcfinalizadas_rival <- renderDefPorcFinalizadasEquipo(datos_ft, fasedj = "Transicion DEF-AT", equipo = equipin_rival)
    
    
    output$texto_defcontrasverticales_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival vertical Transitions:", "</span>"))
    })
    # contras verticales
    output$defcontrasverticales_rival <- renderDefContrasVerticalesEquipo(datos_ft, equipo = equipin_rival)
    
  })
  })
  
  
  
  
  #####
  ##### 5. ABP
  observe({
    req(selectedTeam_abp())
    
    equipin_rival <- selectedTeam_abp()
    
    observeEvent(input$selector_fechas_abp2, {
    
    datos_ft <- eventing %>% filter(str_detect(partido, equipin_rival), competicion == input$selector_competi_rival_abp, between(as.Date(dia, format = "%d/%m/%Y"), input$selector_fechas_abp2[1], input$selector_fechas_abp2[2]))
    
    output$texto_abpfavor_rival <- renderUI({
      HTML(paste0("<span style='font-size:35px; font-weight:bold; color:black; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------  OFFENSIVE SET PIECE ------------------------", "</span>"))
    })
    
    output$texto_abpcontra_rival <- renderUI({
      HTML(paste0("<span style='font-size:35px; font-weight:bold; color:black; margin-top: 10px; margin-bottom: 10px;'>", "<br> <br> ------------------------  DEFENSIVE SET PIECE ------------------------", "</span>"))
    })
    
    output$partidos_abp2 <- renderUI({
      
      datos_ft2 <- datos_ft %>% arrange(as.Date(dia, format = "%d/%m/%Y"))
      partidos <- unique(datos_ft2$partido)
      
      rivales <- sapply(partidos, function(partido) {
        equipos <- str_split(partido, " - ")[[1]]
        if (equipos[1] == equipin_rival) {
          return(paste(equipos[2], "(H)"))
        } else if (equipos[2] == equipin_rival) {
          return(paste(equipos[1], "(A)"))
        } else {
          return(NULL)  
        }
      })
      
      rivales <- rivales[!is.null(rivales)]
      
      HTML(paste("<b> Rivals: </b>", str_c(rivales, collapse = ", ")))
      
    })
    
    output$porc_goles_abp_rival <- renderPorcGoles(datos_ft, fasedj = "ABP", equipo = equipin_rival)
    
    
    ### A FAVOR ###
    aux <- datos_ft %>% filter(fasedejuego == "ABP", team == equipin_rival, x > 99, y < 1)
    jugadores_cd_rival <- unique(aux$name)
    
    output$jugadores_cornerd_rival <- renderUI({
      selectInput("jugadores_cornerd_rival", "Corners Taken by:", choices = jugadores_cd_rival, multiple = T, selected = jugadores_cd_rival[1])
    })
    
    output$texto_cornerder_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Right Side Corners:", "</span>"))
    })
    
    output$cornerder_rival <- renderCornersDerEquipo(datos_ft, equipo = equipin_rival, input, "rival")
    
    
    
    aux <- datos_ft %>% filter(fasedejuego == "ABP", team == equipin_rival, x > 99, y > 99)
    jugadores_ci_rival <- unique(aux$name)
    
    output$jugadores_corneri_rival <- renderUI({
      selectInput("jugadores_corneri_rival", "Corners Taken by:", choices = jugadores_ci_rival, multiple = T, selected = jugadores_ci_rival[1])
    })
    
    output$texto_cornerizq_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Left Side Corners:", "</span>"))
    })
    
    output$cornerizq_rival <- renderCornersIzqEquipo(datos_ft, equipo = equipin_rival, input, "rival")
    
    output$texto_faltascentro_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Fouls ended in the Box:", "</span>"))
    })
    
    output$faltascentro_rival <- renderFaltasCentroEquipo(datos_ft, equipo = equipin_rival)
    
    output$texto_libredirecto_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Direct Free Kicks:", "</span>"))
    })
    output$libredirecto_rival <- renderLibreDirectoEquipo(datos_ft, equipo = equipin_rival)
    
    
    output$texto_penales_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Penalties:", "</span>"))
    })
    output$penales_rival <- renderPenalesEquipo(datos_ft, equipo = equipin_rival)
    
    
    ### EN CONTRA ###
    output$porc_goles_defabp_rival <- renderDefPorcGoles(datos_ft, fasedj = "ABP", equipo = equipin_rival)
    
    
    output$texto_defcornerder_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Right Side Corners:", "</span>"))
    })
    output$defcornerder_rival <- renderDefCornersDerEquipo(datos_ft, equipo = equipin_rival)
    
    
    output$texto_defcornerizq_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Left Side Corners:", "</span>"))
    })
    output$defcornerizq_rival <- renderDefCornersIzqEquipo(datos_ft, equipo = equipin_rival)
    
    
    output$texto_deffaltascentro_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Fouls ended in the Box:", "</span>"))
    })
    output$deffaltascentro_rival <- renderDefFaltasCentroEquipo(datos_ft, equipo = equipin_rival)
    
    
    output$texto_deflibredirecto_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Direct Free Kicks:", "</span>"))
    })
    output$deflibredirecto_rival <- renderDefLibreDirectoEquipo(datos_ft, equipo = equipin_rival)
    
    
    output$texto_defpenales_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Rival Penalties:", "</span>"))
    })
    output$defpenales_rival <- renderDefPenalesEquipo(datos_ft, equipo = equipin_rival)
    
    # intervenciones del portero en zona de control
    porteros <- jugadores %>% filter(team == equipin_rival, position == "Goalkeeper")
    porteros <- porteros$name
    
    output$portero_control_rival <- renderUI({
      selectInput("portero_control_rival", "Area Control from:", choices = porteros, multiple = F, selected = porteros[1])
    })
    
    observeEvent(input$portero_control_rival, {
      
      output$porccontrol_por_rival <- renderUI({
        
        ## porcentaje
        partidos_por <- datos_ft %>% group_by(partido) %>% summarise(esta_portero = input$portero_control_rival %in% name) %>% filter(esta_portero)
        partidos_por <- partidos_por$partido
        
        
        abp_portero <- datos_ft %>% group_by(partido, posesion) %>% filter(partido %in% partidos_por, team != equipin_rival, fasedejuego == "ABP") %>% filter(any(between(endX, 95, 99.9) & between(endY, 40, 60))) %>% mutate(par_pos = paste(partido, posesion, sep = ", "))
        
        
        posesiones_portero_full <- datos_ft %>% mutate(sig_actor = lead(name), sig_accion = lead(type.displayName)) %>% group_by(partido, posesion) %>% mutate(par_pos2 = paste(partido, posesion, sep = ", ")) %>% filter(par_pos2 %in% abp_portero$par_pos)
        
        
        intervenciones <- posesiones_portero_full %>% group_by(partido, posesion) %>% summarise(accion_portero = ifelse(input$portero_control_rival %in% sig_actor, T, F))
        
        porc_acciones <- round(sum(intervenciones$accion_portero)/nrow(intervenciones)*100, 2)
        ###
        
        ### html
        nombre_tio <- input$portero_control_rival
        
        foto_url <- jugadores %>% filter(name == nombre_tio, team == equipin_rival) %>% select(foto) %>% pull()
        
        if (is.na(foto_url) || length(foto_url) == 0) {
          foto_url <- "https://www.shutterstock.com/image-vector/blank-avatar-photo-place-holder-600nw-1095249842.jpg"
        }
        
        
        categorias <- "% Of Actions in this Zone with GK Presence (Actions/Total)"
        resultados <- paste0(porc_acciones, " (", sum(intervenciones$accion_portero), "/", nrow(intervenciones), ")")
        
        
        # Crear la tabla personalizada
        tabla_html <- htmltools::HTML(
          paste0(
            '<table class="table table-striped" style="width: 100%; border: 3px solid black;">',
            '<thead>',
            '<tr>',
            '<th colspan="5" style="text-align:center; border-bottom: 3px solid black; font-size: 20px; font-family: Arial, sans-serif;">',
            '<div style="display: flex; align-items: center; justify-content: center; margin-bottom: 10px; margin-top: 10px;">',
            '<img src="', foto_url, '" style="width:100px;height:100px; vertical-align: middle; margin-right: 40px;">',
            nombre_tio,
            '</th>',
            '</tr>',
            '</thead>',
            '<tbody>',
            '<tr>',
            '<td style="text-align:center; font-weight:bold">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black; font-weight:bold\">", categorias), '</td>',
            '</tr>',
            '<tr>',
            '<td style="text-align:center;">', gsub(" & ", "</td><td style=\"text-align:center; border-left: 3px solid black;\">", resultados), '</td>',
            '</tr>',
            '</tbody>',
            '</table>'
          )
        )
        
        htmltools::HTML(tabla_html)
      })
    })
    
    output$texto_zonadecontrol_por_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Area Control Zone:", "</span>",
                  "<span style='font-size:15px; font-weight:bold;'><span class='tooltip-icon'>?
                      <span class='tooltip-text'>The Keeper dominates this zone if the % of actions ended in this rectangle in Rival Corners where he appears is high.</span></span>
                      </span>"))
    })
    
    output$zonadecontrol_por_rival <- renderControlPorteria()
    
    output$texto_protadefabp_rival <- renderUI({
      HTML(paste0("<span style='font-size:15px; font-weight:bold;'>", "<br> <br> Top Set Piece Defenders:", "</span>"))
    })
    output$protadefabp_rival1 <- renderProtasDefensivo(datos_ft, fasedj = "ABP", equipo = equipin_rival, pos = 1)
    output$protadefabp_rival2 <- renderProtasDefensivo(datos_ft, fasedj = "ABP", equipo = equipin_rival, pos = 2)
  })
  })
  
  
}




shinyApp(ui, server)
