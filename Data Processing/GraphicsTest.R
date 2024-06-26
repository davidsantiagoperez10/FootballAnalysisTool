library(dplyr)
library(stringr)
library(ggsoccer)
library(ggplot2)
library(plotly)
library(igraph)
library(gganimate)
library(tidyr)
library(scales)

OptaMAPcampofutbol <- function(){
  
  #Creamos la plantilla del tema del gr?fico (esto es de soccermatics con algunos cambios)
  theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="#538032", colour=NA),
      legend.key=element_rect(colour="#538032",fill="#538032"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "#538032", fill = "#538032", size = .5),
      panel.background=element_rect(fill="#538032",colour="#538032"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="#538032",size=size,angle=270),
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
    geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#538032", colour = "#ffffff")  +
    geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#538032", colour = "#ffffff")  +
    geom_segment(aes(x = 10600/2, y = ymin, xend = 10600/2, yend = 7040),colour = "#ffffff") +
    geom_path(data=Dleft, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=Dright, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=center_circle, aes(x=x,y=y), colour = "#ffffff") +
    geom_point(aes(x = penspot , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600-(penspot)) , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600/2) , y = 7040/2), colour = "#ffffff") +
    geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "#000000", size = 1) +
    geom_segment(aes(x = 10600, y = GoalPosts[1], xend = 10600, yend = GoalPosts[2]),colour = "#000000", size = 1)+
    theme(legend.position="bottom")
  
  return(p)
  
}



datos <- readRDS("eventing_confases.rds")
datos_sev <- datos %>% filter(str_detect(partido, "Sevilla"))


fase_of <- datos_sev %>% filter(fasedejuego == "Construccion Ofensiva", partido == "Sevilla - Granada", type.displayName == "Pass", outcomeType.displayName == "Successful") %>% group_by(posesion) %>% filter(sum(teamId == 67) > n()/2)

fase_tr <- datos_sev %>% filter(fasedejuego == "Transicion DEF-AT Vertical", type.displayName == "Pass", outcomeType.displayName == "Successful") %>% group_by(partido, posesion) %>% filter(sum(teamId == 67) > n()/2)

# Secuencias de pases más repetidas
find_sequences <- function(df, k) {
  sequences <- df %>%
    group_by(posesion) %>%
    filter(n() >= k) %>%
    summarise(sequence = list(map(seq_len(n() - k + 1), 
                                  ~paste(name[.x:(.x + k - 1)], collapse = "-"))),
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

View(combinaciones_comunes(fase_of, 3))
View(combinaciones_comunes(fase_tr, 2))


prueba <- combinaciones_comunes(fase_of, 3)
## INSTIGADORES DE CONTRAS: el del ballrecovery y anterior(en caso de haber)/siguiente
# y el que da el pase más largo?




###########
# PASES
campo <- ggplot(data = fase_of, aes(x = x, y = y, xend = endX, yend = endY, text = name)) +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  geom_segment(aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  theme_pitch() + direction_label() +
  theme(panel.background = element_rect(fill = "#3ab54a"))


campo


########
# MAPA DE CALOR

ggplot(fase_of, aes(x, y)) +
  geom_density_2d_filled(alpha = 1, aes(fill = as.numeric(stat(level)))) + 
  geom_point(alpha = .6) +
  scale_fill_gradient2(low = "#3ab54a", mid = "yellow", high = "red", midpoint = 7.5) +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a", alpha = 0.1) +
  scale_x_continuous(limits = c(0,100)) + scale_y_continuous(limits = c(0,100)) +
  theme_pitch() + direction_label() + theme_void() + guides(fill = "none") +
  theme(panel.background = element_rect(fill = "#3ab54a"))


#########
# RED DE PASES 
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
                                     teamId == equipo, between(minute, minutos[1], minutos[length(minutos)]))
  
  para_red <- subset_querido %>% filter(type.displayName == "Pass", outcomeType.displayName == "Successful", teamId == equipo) %>% 
    mutate(destinatario = ifelse(lead(posesion) == posesion, lead(name), NA)) %>% 
    filter(!is.na(destinatario))
  
  para_red <- para_red %>% group_by(name) %>% mutate(x = mean(x), y = mean(y)) 
  
  para_red <- para_red %>% group_by(name, x, y, destinatario) %>% summarise(pases = n())
  
  
  # nodos
  knodes <- para_red %>% group_by(name, x, y) %>% summarise(total = sum(pases))
  
  pasestotales<-sum((para_red$pases))
  
  # Saco la matriz sin portero para calcular el convex hull y sacar profundidad y amplitud
  player_data_gk<-para_red %>%
    filter(name != c("Orjan Nyland")) ################## CAMBIAR LÍNEA POR PORTERO A SECAS
  
  # Me quedo con duplas de m?s de 2 o m?s pases  
  player_data_gk<-player_data_gk %>%
    filter(pases>=2)
  
  # valores para profundidad y amplitud  
  x_max_prof<-max(player_data_gk$x)*106
  x_min_prof<-min(player_data_gk$x)*106
  y_max_ampl<-max(player_data_gk$y)*70.5
  y_min_ampl<-min(player_data_gk$y)*70.5
  
  
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
  
  #Calculamos la densidad del grafo
  
  densidad<-igraph::graph.density(Colleague_Graph_kedges_d,loop=FALSE)
  media<-igraph::mean_distance(Colleague_Graph_kedges_d,directed=FALSE,unconnected=TRUE)
  parejas_combinadas<-dyad_census(Colleague_Graph_kedges_d)$mut
  parejas_1combinadas<-dyad_census(Colleague_Graph_kedges_d)$asym
  parejas_sincombinadas<-dyad_census(Colleague_Graph_kedges_d)$null
  triangulacion<-transitivity(Colleague_Graph_kedges_d, type="global")
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
    geom_polygon(hull_coor, mapping=aes(x= x*106, y=y*70.5), alpha = 0.4,linetype = "dashed",size = 1,show.legend = FALSE) +
    geom_point(data = centro_pol,aes(x = xm*106,y =ym*70.4),shape = 21, colour = "black", size = 3, stroke = 5) +
    geom_segment(data = curva, aes(x = x2*106, y =(y2*70.5), xend = xend*106, yend =( yend*70.5), size = size2,color=to), arrow = arrow, alpha = 1) +
    
    
    scale_color_gradient(low = "white", high = "red")+
    geom_point(data = knodes10,aes(x = x*106,y = (y*70.5),size = size,fill=bet),color='black',shape=21,stroke = 1) +
    
    ggrepel::geom_label_repel(
      data = knodes10,
      aes(x = x*106,y = (y*70.5),label=name),
      nudge_y = 400,
      size=3) +
    
    geom_segment(aes(x = x_min_prof, y = 6900, xend = x_max_prof, yend = 6900),colour = "#000000", size = 1) +
    geom_segment(aes(x = x_min_prof, y = 7025, xend = x_min_prof, yend = 6775),colour = "#000000", size = 1) +
    geom_segment(aes(x = x_max_prof, y = 7025, xend = x_max_prof, yend = 6775),colour = "#000000", size = 1) +
    geom_text(aes(x=(x_max_prof-x_min_prof)/2+x_min_prof,y=6725,label=format((x_max_prof-x_min_prof)/100,digits=2,nsmall=2)), colour="black") +
    
    
    geom_segment(aes(x = 9650, y = y_min_ampl, xend = 9650, yend = y_max_ampl),colour = "#000000", size = 1) +
    geom_segment(aes(x = 9525, y = y_min_ampl, xend = 9725, yend = y_min_ampl),colour = "#000000", size = 1) +
    geom_segment(aes(x = 9525, y = y_max_ampl, xend = 9725, yend = y_max_ampl),colour = "#000000", size = 1) +
    geom_text(aes(x=9450,y=(y_min_ampl-y_max_ampl)/2+y_max_ampl,label=format((y_max_ampl-y_min_ampl)/100,digits=2,nsmall=2)), colour="black") +
    
    scale_size_identity() +
    
    annotate("text", label = paste("Pases Totales en Matriz: ",pasestotales), x = 100, y = 200, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Densidad del grafo: ",format(densidad, digits=2, nsmall=2)), x = 100, y = 500, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Duplas combinadas: ",format(parejas_combinadas, digits=0, nsmall=2)), x = 100, y = 800, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Duplas monodirigidas: ",format(parejas_1combinadas, digits=0, nsmall=2)), x = 100, y = 1100, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Duplas sin combinación: ",format(parejas_sincombinadas, digits=0, nsmall=2)), x = 100, y = 1400, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Triangulación: ",format(triangulacion, digits=0, nsmall=2)), x = 100, y = 1700, size = 5, colour = "black",hjust = 0)+
    theme(legend.position="none")
  
  return(p)
  
}

red_pases(datos_sev, partidos = "Sevilla - Granada", fasedj = "Construccion Ofensiva", equipo = 67, minutos = 0:100)


#######
# CENTROS

sevgra <- datos_sev %>% filter(partido == "Sevilla - Granada") %>% filter(team == "Sevilla")


centros <- sevgra %>% filter(type.displayName == "Pass", (y <= 25 & between(endY, 30+y, 80)) | (y >= 75 & between(endY, 20, y-30)), endX > 85) %>% select(x, y, endX, endY, name, outcomeType.displayName, fasedejuego)

ggplot(data = centros, aes(x = x, y = y, xend = endX, yend = endY, text = name)) +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  geom_segment(aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  theme_pitch() + direction_label() +
  theme(panel.background = element_rect(fill = "#3ab54a"))


# CENTROS EN UN CAMPO CON CUADROS

centros <- centros %>%
  mutate(franja = case_when(
    x >= 50 & x <= 85 & y <= 25 ~ "Far Right",
    x > 85 & y <= 25 ~ "Deep Right",
    x >= 50 & x <= 85 & y >= 75 ~ "Far Left",
    x > 85 & y >= 75 ~ "Deep Left"
  ))

# Añadir frecuencias al data frame original
todas_franjas <- data.frame(
  franja = c("Far Right", "Deep Right", "Far Left", "Deep Left")
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
  x = c(50, 85, 85, 50, 85, 100, 100, 85, 85, 50, 50, 85, 100, 85, 85, 100),
  y = c(0, 0, 25, 25, 0, 0, 25, 25, 75, 75, 100, 100, 75, 75, 100, 100),
  franja = rep(c("Far Right", "Deep Right", "Far Left", "Deep Left"), each = 4)
)

poligonos <- poligonos %>%
  left_join(frecuencias, by = "franja")


# Crear el gráfico
ggplot() +
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() +
  coord_cartesian(xlim = c(50, 100)) +
  
  # Añadir los polígonos de las franjas
  geom_polygon(data = poligonos, aes(x = x, y = y, group = franja, fill = num_centros), alpha = 0.5, color = "black") +
  
  # Ajustar colores
  scale_fill_gradient2(low = "#3ab54a", high = "red", mid = "yellow", midpoint = max(poligonos$num_centros)/2) +
  
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
  theme_minimal() + theme(legend.position = "none") + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


#######
# ALTURA DEFENSA CONTRARIA
sevgra <- datos_sev %>% filter(partido == "Sevilla - Granada") %>% group_by(posesion) %>% filter(sum(team == "Granada") > n()/2)


verificar_secuencia <- function(x, jugador, equipo) {
  n <- length(x)
  altura <- 0
  if (n < 3) return(altura)
  
  for (i in 1:(n-2)) {
    if (x[i] > 40 && x[i+1] > 40 && x[i+2] > 40 &&
        abs(x[i] - x[i+1]) <= 1 && abs(x[i+1] - x[i+2]) <= 10 &&
        equipo[i] == equipo[i+1] && equipo[i+1] == equipo[i+2] &&
        jugador[i] != jugador[i+1] && jugador[i+1] != jugador[i+2]) {
      altura <- x[i+2]
    }
  }
  return(altura)
}



altura_def <- sevgra %>% group_by(posesion) %>% filter(n()>2, x>0) %>% summarize(secuencia = verificar_secuencia(x, name, teamId)) %>% filter(secuencia > 0) 

alturas <- sevgra %>% group_by(posesion) %>% filter(posesion %in% altura_def$posesion)
alturas <- alturas %>% left_join(altura_def, by = "posesion")


posesion_minutes <- alturas %>%
  group_by(posesion) %>%
  summarise(franja_min = paste0(first(minute), ":", sprintf("%02d", first(second)), " - ", last(minute), ":", sprintf("%02d", last(second))))

alturas <- alturas %>% left_join(posesion_minutes, by = "posesion")



campo_altura <- ggplot(data = alturas, aes(x = x, y = y, xend = endX, yend = endY, group = posesion)) +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  geom_segment(aes(x = x, y = y, xend = endX, yend = endY, group = posesion),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  geom_rect(aes(xmin = secuencia + 5, xmax = 100, ymin = 0, ymax = 100), alpha = 0.1, fill = "lightyellow") +
  theme_pitch() + direction_label() + 
  theme(panel.background = element_rect(fill = "#3ab54a"))


anim <- campo_altura + 
  transition_states(posesion, transition_length = 2, state_length = 60) +
  ease_aes('cubic-in-out') +
  labs(title = 'Franja de tiempo: {alturas[which(alturas$posesion == closest_state), "franja_min"][1,]}') 
  


animate(anim)




#####
# nº medio de pases
mediapases <- sevgra %>% filter(fasedejuego == "Construccion Ofensiva") %>% group_by(posesion) %>% summarise(pases = n()) %>% filter(pases > 1)
mean(mediapases$pases)


# verticalidad media: m ganados por pase
verticalidad <- sevgra %>% group_by(posesion) %>% filter(n() > 2) %>% summarise(vert = (last(x)-first(x))/(n()-1)) 
mean(verticalidad$vert)*1.05


# jugadores más finalizadores
tiros <- sevgra %>% filter(str_detect(type.displayName, "Shot") | type.displayName == "Goal")
tiros %>% group_by(name) %>% summarise(tiros = n()) %>% arrange(desc(tiros))




##

# recuperaciones
robos <- sevgra %>% filter(type.displayName %in% c("BallRecovery", "Interception")) %>% filter(team == "Sevilla")

robos$cuadrante_x <- cut(robos$x, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
robos$cuadrante_y <- cut(robos$y, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)


sevgra$tiempo <- sevgra$minute*60 + sevgra$second

es_recuperacion_rapida <- function(tiempo_recuperacion, equipo, pos) {
  # Filtrar eventos del mismo equipo en los 10 segundos anteriores
  eventos_anteriores <- sevgra %>% 
    filter(team == equipo, 
           posesion != pos, 
           tiempo >= (tiempo_recuperacion - 10),
           tiempo < tiempo_recuperacion,
           type.displayName %in% c("Pass"))
  
  # Retornar TRUE si hay al menos una posesión en ese intervalo
  return(nrow(eventos_anteriores) > 0)
}

robos_rapidos <- robos %>% rowwise() %>% mutate(rapida = es_recuperacion_rapida(minute*60+second, team, posesion))
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
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() +
  geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_robos), width = 25, height = 33.33, alpha = 0.5) +
  geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_robos), color = "black", size = 5, na.rm = TRUE) +
  geom_point(data = robos_rapidos, aes(x = x, y = y), color = "cyan", size = 3) +
  scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_robos))/2) +
  theme_minimal() + theme(legend.position = "none") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


## jugadores más recuperadores
robos %>% group_by(name) %>% summarise(robos = n()) %>% arrange(desc(robos))


## campo con flechas de zonas de contraataque
contras <- sevgra %>% filter(fasedejuego == "Transicion DEF-AT") %>% filter(type.displayName=="Pass")


aux <- contras %>% group_by(posesion) %>%
  summarise(
  pen_x = nth(x, -2),
  ult_x = nth(x, -1),
  pen_y = nth(y, -2),
  ult_y = nth(y, -1),
  ) 

carril <- aux %>% mutate(carril = ifelse((ult_x > 60) & (pen_y < 35) & (ult_y < 35), "Derecho", 
                                         ifelse((ult_x > 60) & (pen_y > 65) & (ult_y > 65), "Izquierdo", 
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
  left_join(conteo_acciones, by = "carril")

# Graficar el campo de fútbol
ggplot() +
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() +
  geom_segment(data = flechas, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = n_acciones),
               arrow = arrow(length = unit(0.3, "inches")), size = 2) +
  geom_text(data = flechas, aes(x = label_x, y = label_y, label = n_acciones, color = n_acciones), size = 5, fontface = "bold") +
  scale_color_gradient2(low = "yellow2", high = "red", mid = "orange4", midpoint = max(flechas$n_acciones)/2) +
  theme_minimal() + theme(legend.position = "none") + direction_label() + guides(fill = "none") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())



# Construcciones del portero
portero <- fase_of %>% filter(type.displayName == "Pass", x < 15, between(y, 25, 75))


portero$cuadrante_x <- cut(portero$endX, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
portero$cuadrante_y <- cut(portero$endY, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)

# Contar el número de robos en cada cuadrante
conteo_portero <- portero %>%
  group_by(cuadrante_x, cuadrante_y) %>%
  summarise(n_envios = n()) %>% 
  ungroup()

# Crear un dataframe para los textos de los cuadrantes
texto_cuadrantes <- expand.grid(
  cuadrante_x = 1:4,
  cuadrante_y = 1:3
) %>%
  left_join(conteo_portero, by = c("cuadrante_x", "cuadrante_y")) %>%
  mutate(
    x = (cuadrante_x - 1) * 25 + 12.5,
    y = (cuadrante_y - 1) * 33.33 + 16.665
  )


ggplot() +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_envios), width = 25, height = 33.33, alpha = 0.5) +
  geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_envios), color = "black", size = 5, na.rm = TRUE) +
  geom_segment(data = portero, aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  scale_fill_gradient2(low = "darkgreen", high = "red", mid = "orange", midpoint = max(na.omit(texto_cuadrantes$n_envios))/2) +
  theme_minimal() + theme(legend.position = "none") + guides(fill = "none") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())



# De la línea defensiva
linea_def <- fase_of %>% filter(type.displayName == "Pass", ((x < 15 & (y > 75 | y < 25)) | (between(x, 15, 35))))


linea_def$cuadrante_x <- cut(linea_def$endX, breaks = seq(0, 100, by = 25), labels = FALSE, include.lowest = TRUE)
linea_def$cuadrante_y <- cut(linea_def$endY, breaks = seq(0, 100, by = 33.33), labels = FALSE, include.lowest = TRUE)

# Contar el número de robos en cada cuadrante
conteo_linea_def <- linea_def %>%
  group_by(cuadrante_x, cuadrante_y) %>%
  summarise(n_envios = n()) %>% 
  ungroup()

# Crear un dataframe para los textos de los cuadrantes
texto_cuadrantes <- expand.grid(
  cuadrante_x = 1:4,
  cuadrante_y = 1:3
) %>%
  left_join(conteo_linea_def, by = c("cuadrante_x", "cuadrante_y")) %>%
  mutate(
    x = (cuadrante_x - 1) * 25 + 12.5,
    y = (cuadrante_y - 1) * 33.33 + 16.665
  )


ggplot() +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  geom_tile(data = texto_cuadrantes, aes(x = x, y = y, fill = n_envios), width = 25, height = 33.33, alpha = 0.5) +
  geom_text(data = texto_cuadrantes, aes(x = x, y = y, label = n_envios), color = "black", size = 5, na.rm = TRUE) +
  geom_segment(data = linea_def, aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  theme_minimal() + guides(fill = "none") + theme(legend.position = "none") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())





### TRANSICIONES
transiciones <- sevgra %>% filter(str_detect(fasedejuego,"Transicion DEF-AT")) %>% group_by(posesion) %>% filter(n()>2)

n_pos <- length(unique(transiciones$posesion))

## de cada tipo
a <- transiciones %>% group_by(fasedejuego) %>% summarise(veces = n_distinct(posesion)) %>% mutate(porc_fase = round(veces/n_pos, 2)*100)
a$porc_fase[a$fasedejuego == "Transicion DEF-AT Vertical"]
## finalizadas
transiciones %>% group_by(fasedejuego, posesion) %>% 
  summarise(finalizadas = any(str_detect(type.displayName, "Shot") | type.displayName == "Goal")) %>% 
  group_by(fasedejuego) %>% summarise(porc_fin = round(sum(finalizadas)/n_distinct(posesion), 3)*100)

# asistentes
asistentes_tiro <- transiciones %>% filter(str_detect(type.displayName, "Shot")) %>% select(relatedEventId)
asistentes_gol <- transiciones %>% filter(type.displayName == "Goal") %>% select(relatedEventId)

name_asis_tiro <- transiciones[transiciones$eventId %in% asistentes_tiro$relatedEventId, "name"]
name_asis_gol <- transiciones$name[transiciones$eventId %in% asistentes_gol$relatedEventId]


# incitadores
transiciones %>% select(first(name))


## transiciones verticales
verticales <- sevgra %>% filter(str_detect(fasedejuego,"Transicion DEF-AT Vertical")) 

ggplot() +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  geom_segment(data = verticales, aes(x = x, y = y, xend = endX, yend = endY, color = posesion),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  ggrepel::geom_label_repel(
    data = verticales %>% filter((type.displayName %in% c("Pass", "Goal") | str_detect(type.displayName, "Shot"))),
    aes(x = x, y = y, label = ifelse(name == lead(name), "", name)),
    size=3) +
  ggrepel::geom_label_repel(
    data = verticales %>% filter((type.displayName %in% c("Pass", "Goal") | str_detect(type.displayName, "Shot"))),
    aes(x = endX, y = endY, label = ifelse(lead(posesion) == posesion, lead(name), "")),
    size=3) +
  theme_minimal() + guides(fill = "none") + theme(legend.position = "none") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())



## FASE DEFENSIVA

# PPDA
calculo_ppda <- function(datos, equipo){

  defensivo <- datos %>% group_by(posesion) %>% filter(any(x < 67))
  
  pases_otro <- datos %>% filter(type.displayName %in% c("Pass", "BallTouch", "Aerial", "Interception", "TakeOn", "MissedShots", "SavedShot", "Goal", "ShotOnPost"), outcomeType.displayName == "Successful", team != equipo)
  
  acciones_def <- which(
      (datos$type.displayName == "Foul" & datos$outcomeType.displayName == "Unsuccessful" & datos$team == equipo & datos$x>33) | 
      (datos$type.displayName == "Dispossessed" & datos$outcomeType.displayName == "Successful" & datos$team != equipo & datos$x>33) | 
      (datos$type.displayName == "Aerial" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & lag(datos$team) != equipo & datos$x>33) |
      (datos$type.displayName == "Tackle" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33) |
      (datos$type.displayName == "BlockedPass" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33) |
      (datos$type.displayName == "Clearance" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33) |
      (datos$type.displayName == "Interception" & datos$outcomeType.displayName == "Successful" & datos$team == equipo & datos$x>33)
    
  )
  
  return (nrow(pases_otro)/length(acciones_def))
  
}


equipines <- unique(datos$team[datos$competicion == "España-LaLiga"])

ppdas <- tibble(equipo = as.character(), ppda = as.numeric())


for (eq in equipines) {
  datos_eq <- datos %>% filter(str_detect(partido, eq))
  
  ppda_eq <- datos_eq %>% group_by(partido) %>% group_modify(~ tibble(ppda = calculo_ppda(.x, eq))) %>% ungroup()
  
  ppdas <- rbind(ppdas, tibble(equipo = eq, ppda = mean(ppda_eq$ppda)))
  
}

ppdas %>% arrange(ppda)


## JUGADORES PROTAGONISTAS
# intercepciones, recuperaciones, aerials, clearances, blocked, etc. en mi mitad del campo en fase de construcción of.

def_sevilla <- datos_sev %>% filter((fasedejuego == "Construccion Ofensiva") | (lag(fasedejuego) == "Construccion Ofensiva") | (lag(fasedejuego, 2) == "Construccion Ofensiva"))

acciones <- def_sevilla %>% filter(team == "Sevilla", type.displayName %in% c("BallRecovery", "Aerial", "Clearance", "BlockedPass", "Interception", "Claim"), x < 40)

tabla_porc <- acciones %>% group_by(name, type.displayName) %>% summarise(exito = sum(outcomeType.displayName == "Successful"),
                                                                              fracaso = sum(outcomeType.displayName == "Unsuccessful"),
                                                                              total = n(),
                                                                              porc_exito = round(exito / (total) * 100, 2))

# acciones exitosas
por_jug <- tabla_porc %>% group_by(name) %>% summarise(acciones = sum(exito)) %>% arrange(desc(acciones))

prueba <- tabla_porc[4:7, ]

nombre_tio <- prueba$name[1]
foto <- "https://img.a.transfermarkt.technology/portrait/header/501418-1583506190.jpg?lm=1"


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
    '<img src="', foto, '" style="width:100px;height:100px; vertical-align: middle; margin-right: 40px;">',
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


# Mostrar la tabla en el visor de RStudio (si estás usando RStudio)
htmltools::html_print(htmltools::HTML(tabla_html))




## ABP
# corners
corners_der <- datos_sev %>% filter(partido == "Sevilla - Granada", team == "Sevilla", fasedejuego == "ABP") %>% group_by(partido, posesion) %>% filter(any(x > 99), any(y < 1))

contactos <- corners_der %>% filter(partido == "Sevilla - Granada", (type.displayName == "Pass" | outcomeType.displayName == "Successful"), x < 99, y > 1)
#corners_izq <- sevgra %>% filter(fasedejuego == "ABP", x > 99, y > 99)


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
  #geom_segment(data = corners_der, aes(x = x, y = y, xend = endX, yend = endY),
   #            arrow = arrow(length = unit(0.2, "cm"),
    #                         type = "open")) +
  geom_point(data = contactos, aes(x = x, y = y), color = "black", size = 3) +
  # Añadir etiquetas de porcentaje y número de envios
  geom_text(data = frecuencias, aes(x = c(88.75, 88.75, 88.75, 88.75), y = c(35, 50, 65, 8), 
                                    label = paste(palo, " Freq \n", num_envios, "(", round(porcentaje, 1), "%)", sep = "")), 
            color = "black", size = 3, fontface = "bold") +
  
  geom_text(data = frecuencias[1:3,], aes(x = c(96.25, 96.25, 96.25), y = c(35, 50, 65), 
                                      label = paste("Ended in Contact \n", buenos_envios, "(", round(efectividad_centros, 1), "%)", sep = "")), 
            color = "black", size = 3, fontface = "bold") +
  
  ggrepel::geom_label_repel(
    data = contactos,
    aes(x = x, y = y, label=name), alpha = 0.5,
    size=3) +
  
  # Configurar el tema
  theme_pitch() +
  theme_void() + theme(legend.position = "none") + guides(fill = "none") +
  theme(panel.background = element_rect(fill = "#3ab54a")) 


## resto de faltas al área

# zonas de saque
faltas <- datos_sev %>% filter(team == "Sevilla", fasedejuego == "ABP") %>% 
                      group_by(posesion) %>% filter(all(between(x, 50, 99)), all(between(y, 1, 99)), n() < 10)


# FALTAS QUE ACABEN EN TIRO
entiro <- faltas %>% group_by(partido, posesion) %>% filter(any(str_detect(type.displayName, "Shot")) | any(type.displayName == "Goal"))


# Bandas y centros
centros <- entiro %>% filter(n()>1, type.displayName == "Pass") 
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



ggplot() +
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() +
  coord_cartesian(xlim = c(50, 100)) +
  
  # centros
  geom_polygon(data = poligonos, aes(x = x, y = y, group = franja, fill = rescale(num_centros)), alpha = 0.5, color = "black") +
  scale_fill_gradient2(high = "red3", low = "yellow3", mid = "orange3", midpoint = 0.5) +
  
  
  geom_point(data = centros, aes(x = x, y = y), color = "black", size = 3) +
  geom_segment(data = centros, aes(x = x, y = y, xend = remates$x, yend = remates$y),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  # remates
  geom_point(data = remates, aes(x = x, y = y, color = esgol), size = 3) +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
  
  geom_text(data = frecuencias, aes(x = c(75, 75, 65), y = c(10, 90, 50), 
                                    label = paste(franja, "\n", num_centros, "(", round(porcentaje, 1), "%)", sep = "")), 
            color = "black", size = 4, fontface = "bold") +
  
  theme_pitch() +
  theme_void() + theme(legend.position = "none") + guides(fill = "none") +
  theme(panel.background = element_rect(fill = "#3ab54a")) 


# Libres Directos
libdir <- entiro %>% filter(n()==1, x < 82, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
libdir <- libdir %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))


ggplot() +
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() +
  coord_cartesian(xlim = c(50, 100)) +
  
  geom_point(data = libdir, aes(x = x, y = y, color = esgol), size = 3) +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
  
  geom_segment(data = libdir, aes(x = x, y = y, xend = ifelse(is.na(blockedX), 100, blockedX), yend = ifelse(is.na(blockedY), goalMouthY, blockedY)),
               arrow = arrow(length = unit(0.2, "cm"),
                             type = "open")) +
  
  theme_pitch() +
  theme_void() + theme(legend.position = "none") + guides(fill = "none") +
  theme(panel.background = element_rect(fill = "#3ab54a")) 


# PENALES
penales <- entiro %>% filter(n()==1, x == 88.5, y == 50, (str_detect(type.displayName, "Shot")) | type.displayName == "Goal")
penales <- penales %>% mutate(esgol = ifelse(type.displayName == "Goal", T, F))


# gráfico de portería
# palos en y de 45 a 55 - altura 40
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
  
  # Configurar los límites del gráfico
  coord_fixed(ratio = 1) +
  xlim(-1, 11) +
  ylim(0, 5) +
  
  theme_void() + theme(legend.position = "none") + guides(fill = "none") 



################################################################################
############################### NIVEL DE EQUIPO ################################
################################################################################

## ZONAS DE TIRO FUERA DEL ÁREA
tiros_fda <- datos_sev %>% filter(team == "Sevilla", str_detect(type.displayName, "Shot") | type.displayName == "Goal", x < 82.5)


ggplot() + 
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() + coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  stat_binhex(data=tiros_fda,aes(x=x,y=100-y,label=..count..), geom="hex", bins=15, colour="darkgrey",alpha=0.5) +
  ggplot2::stat_binhex(data=tiros_fda,aes(x=x,y=100-y,label=..count..), geom="text", bins=15, colour="black") +
  scale_fill_gradientn(colours =  c("yellow","orange","red")) +
  theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 




# View(eventing %>% filter(between(as.Date(dia, format = "%d/%m/%Y"), as.Date("13/12/2023", format = "%d/%m/%Y"), as.Date("20/12/2023", format = "%d/%m/%Y"))))

## ZONAS DE TIRO EN EL ÁREA (por jugador)
tiros_area <- datos_sev %>% filter(team == "Sevilla", str_detect(type.displayName, "Shot") | type.displayName == "Goal", x > 82.5)


ggplot() + 
  annotate_pitch(colour = "white", fill = "#3ab54a") +
  theme_pitch() +
  coord_cartesian(xlim = c(82, 100), ylim = c(20, 80)) + 
  stat_binhex(data=tiros_area,aes(x=x,y=y,label=..count..), geom="hex", bins=15, colour="darkgrey",alpha=0.5) +
  ggplot2::stat_binhex(data=tiros_area,aes(x=x,y=y,label=..count..), geom="text", bins=15, colour="black") +
  scale_fill_gradientn(colours =  c("yellow","orange","red")) +
  theme(panel.background = element_rect(fill = "#3ab54a"), legend.position = "none") 



## Goles en cada fase
datos_sev %>% filter(type.displayName == "Goal") %>% group_by(fasedejuego) %>% summarise(total = n()) %>% mutate(porc = round(total / sum(total) * 100, 2))

datos %>% filter(str_detect(partido, "Real Madrid"), team == "Real Madrid", type.displayName == "Goal") %>% group_by(fasedejuego) %>% summarise(total = n()) %>% mutate(porc = round(total / sum(total) * 100, 2))
## Construcciones del portero -> YA ESTÁ

## Linea defensiva -> YA ESTÁ

## Mediocampo (recepciones y pases quizá?)



## nº medio pases y verticalidad -> YA ESTÁ

## centros -> YA ESTÁ 

## Líneas de ataque con porcentaje


# - TRANSICIÓN DEF-AT -

## robos rápidos como % de todos los robos

## jugadores + recuperadores por tiempo jugado


minutos_jugador <- tibble()

datos_sev %>% group_by(name, partido) %>% filter(team == "Sevilla") %>% summarise(minutos = ifelse(any(type.displayName == "SubstitutionOff"), datos_sev$minute[which(datos_sev$type.displayName == "SubstitutionOff")],
                                                                     ielse(any(type.displayName == "SubstitutionOn"), 90 - datos_sev$minute[which(datos_sev$type.displayName == "SubstitutionOn")]), 90))


minutos_jugador <- datos_sev %>%
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


minutos_jugador %>% group_by(name) %>% summarise(minutosjugados = sum(minutos), partidos = n(), media = minutosjugados/partidos) %>% arrange(desc(minutosjugados)) %>% na.omit()


# por90 se hace como: stat / (minutos/90)



## % de verticales, finalizadas, etc. -> YA ESTÁ

## Lineas de ataque con porcentaje


# CONTRAS VERTICALES

## HTML con jugador más finalizador, más iniciador de contras verticales y más asistente
## velocidad media?

## darle una vuelta a la gráfica de ahora (quizá mapa de calor es lo mejor)






# - FASE DEFENSIVA -

## Zonas del área desde donde más le rematan
## Fases en las que reciben más daño (transición, const., abp..., centros)
## % de posesiones en 3/4 en las que el rival remata -> YA ESTÁ
## PPDA -> YA ESTÁ
## Zonas de ataque rival con porcentajes


# - TRANSICIÓN AT-DEF -

## lo mismo que al revés, % de pérdidas rápidas

## lineas de ataque con %

## jugadores + perdedores por partido jugado



# - ABP -

## más protagonismo a jugadores rematadores también

## intervenciones del portero en córners o faltas que van a su zona de control

# zona de control: x(97, 100), y(45, 55)

# posesiones en las que el contrario la pone en su zona de control
abp_portero <- datos_sev %>% group_by(partido, posesion) %>% filter(team != "Sevilla", fasedejuego == "ABP") %>% filter(any(between(endX, 95, 99.9) & between(endY, 45, 55))) %>% mutate(par_pos = paste(partido, posesion, sep = ", "))


posesiones_portero_full <- datos_sev %>% mutate(sig_actor = lead(name), sig_accion = lead(type.displayName)) %>% group_by(partido, posesion) %>% mutate(par_pos2 = paste(partido, posesion, sep = ", ")) %>% filter(par_pos2 %in% abp_portero$par_pos)


intervenciones <- posesiones_portero_full %>% group_by(partido, posesion) %>% summarise(accion_portero = ifelse("Orjan Nyland" %in% sig_actor, T, F))

sum(intervenciones$accion_portero)/nrow(intervenciones)
# ver si hay intervenciones del portero en esas posesiones
# otras intervenciones


################### IDEAS SCOUTING
# detectar si hay muchos patrones en los que el balón viene de una banda,
# el medio centro recibe, pasa a centrales, y los centrales pasan al otro, 
# o al otro mediocentro y este a banda. QUIERO UN MC QUE CAMBIE DE ORIENTACIÓN


# quiero un pivote

# mediocentro meneador y / o filtrador
filtrados <- datos_sev %>% filter(team == "Sevilla", type.displayName == "Pass", x > 75, endX - x >= 10, endY - y < 20, between(y, 25, 75), outcomeType.displayName == "Successful")

filtrados %>% group_by(name) %>% summarise(filtraciones = n()) %>% arrange(desc(filtraciones))


cambios_orientacion <- datos_sev %>% mutate(prev_pass = lag(y), 
                                            next_pass = lead(endY),
                                            next_name = lead(name)) %>% filter(team == "Sevilla", type.displayName == "Pass", outcomeType.displayName == "Successful", 
                                            ((lag(y) < 20) & between(y, 30, 60) & (endY > 80)) | ((lag(y) < 20) & between(y, 30, 60) & (endY < 80) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) > 80)) |
                                            ((lag(y) > 80) & between(y, 30, 60) & (endY < 20)) | ((lag(y) > 80) & between(y, 30, 60) & (endY > 20) & (lead(type.displayName) == "Pass") & (lead(outcomeType.displayName == "Successful")) & between(lead(y), 30, 60) & (lead(endY) < 20))) 


cambios_orientacion <- cambios_orientacion %>% select(x, y, name, endX, endY, prev_pass, next_pass, next_name)

protas <- cambios_orientacion %>% group_by(name) %>% summarise(veces = n()) %>% arrange(desc(veces))

tandems <- cambios_orientacion %>% filter(between(endY, 20, 80)) %>% group_by(name, next_name) %>% summarise(veces = n()) %>% arrange(desc(veces))




regates <- datos_sev %>% filter(team == "Sevilla") %>% mutate(next_event = lead(type.displayName),
                 next_outcome = lead(outcomeType.displayName)) %>% filter(type.displayName == "TakeOn")


conteo_regates <- regates %>% group_by(name) %>% summarise(exito = sum(outcomeType.displayName == "Successful"),
                                         fracaso = sum(outcomeType.displayName == "Unsuccessful"),
                                         continuidad = sum(ifelse(outcomeType.displayName == "Successful" & next_event %in% c("Pass", "SavedShot", "MissedShots", "ShotOnPost", "Goal", "TakeOn", "Foul") & next_outcome == "Successful", T, F)),
                                         porc_continuidad = continuidad/exito)


tiros <- datos %>% filter(team == "Sevilla", type.displayName %in% c("MissedShots", "SavedShot", "Goal", "ShotOnPost"))


conteo_tiros <- tiros %>% group_by(name, shotType) %>% summarise(a_puerta = sum(type.displayName %in% c("SavedShot", "Goal", "ShotOnPost")),
                                                                         fuera = sum(type.displayName == "MissedShots"),
                                                                         total = a_puerta + fuera,
                                                                         goles = sum(type.displayName == "Goal"),
                                                                         xG = sum(na.omit(xG)),
                                                                         xGOT = sum(na.omit(xGOT)))



asistentes_gol <- datos_sev %>% filter(team == "Sevilla") %>% filter(type.displayName == "Goal") %>% mutate(aux = paste(partido, relatedEventId, sep = ", "))  
name_asis_gol <- datos_sev %>% filter(team == "Sevilla") %>% mutate(aux2 = paste(partido, eventId, sep = ", ")) %>%  filter(aux2 %in% asistentes_gol$aux) %>% select(name)   


asistentes <- name_asis_gol %>% group_by(name) %>% summarise(asistencias = n()) %>% arrange(desc(asistencias))


# un defensa con jerarquía


# delantero rematador de centros

# lateral derecho centrador y llegador a fondo


