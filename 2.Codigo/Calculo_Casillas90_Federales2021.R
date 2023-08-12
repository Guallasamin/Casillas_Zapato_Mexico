library(dplyr)
library(tidyr)
library(openxlsx)
library(treemapify)
library(ggplot2)
#install.packages("treemapify")
# Define la ruta del archivo
#ruta <- "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/Federales 2021.dta"

# Carga el archivo
#Federales_2021 <- haven::read_dta(ruta)

# Guardar la base de datos en formato RDS

#saveRDS(Federales_2021, file = "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/Federales_2021.rds")

# Cargar la base de datos RDS
Federales_2021 <- readRDS(file = "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/Federales_2021.rds")

# Filtra los casos donde lista_nominal es igual a 0

Federales_2021 <- Federales_2021[Federales_2021$tipo_casilla != "S", ]
Federales_2021 <- Federales_2021[Federales_2021$total_votos_calculados != 0, ]

# Verificar que se hallan borrado las listas nominales iguales a 0
min <- min(Federales_2021$lista_nominal_casilla)

print(min)

# Verificar los nombres de las columnas en el objeto Federales_2021
names(Federales_2021)

# Calcula la suma de las variables indicadas y crea una nueva variable MORENA
Federales_2021$MorenaPVEMPT_2021 <- rowSums(Federales_2021[, c("pvem", "pt", "morena", "pvemptmorena", "pvempt", "pvemmorena", "ptmorena")])

# Calcula la suma de las variables cc_pvem_pt_morena y crea una nueva variable OTROS
Federales_2021$otros_2021 <- rowSums(Federales_2021[, c("pes", "rsp", "fxm", "ci", "candidatoanoregistradoa")])

# Calcula la suma de la variable no_registradas y crea una nueva variable PAN-PRI-PRD
Federales_2021$panpriprd_2021 <- rowSums(Federales_2021[, c("pan", "pri", "prd", "panpriprd", "panpri", "panprd", "priprd")])

# Calcula la suma de la variable no_registradas y crea una nueva variable MC
Federales_2021$mc_2021 <- rowSums(Federales_2021[, c("mc")])

# Calcula la suma de la variable no_registradas y crea una nueva variable NULOS
Federales_2021$nulos_2021 <- rowSums(Federales_2021[, c("votosnulos")])

# Renombra las variables
names(Federales_2021)[names(Federales_2021) == "lista_nominal_casilla"] <- "lista_nominal_2021"
names(Federales_2021)[names(Federales_2021) == "total_votos_calculados"] <- "total_2021"
names(Federales_2021)[names(Federales_2021) == "nombre_estado"] <- "estado_2021"
names(Federales_2021)[names(Federales_2021) == "id_estado"] <- "idestado_2021"

# Realiza el colapso de las variables sumando por las variables indicadas
Federales_2021 <- aggregate(cbind(MorenaPVEMPT_2021, otros_2021, panpriprd_2021, mc_2021, nulos_2021, lista_nominal_2021, total_2021 )
                            ~ estado_2021 + idestado_2021 + seccion + clave_casilla, Federales_2021, sum)

# Crea la variable año con el valor 2021
Federales_2021$año <- 2021

# Comprobacion
Federales_2021$comprobacion <- rowSums(Federales_2021[, c("MorenaPVEMPT_2021", "otros_2021", "panpriprd_2021", "mc_2021", "nulos_2021")])
suma_comprobación <- sum(Federales_2021$comprobacion)
suma_total_votos <- sum(Federales_2021$total_2021)
print (suma_comprobación)
print (suma_total_votos)

# Participaciones
Federales_2021$PC_MorenaPVEMPT_2021 <- Federales_2021$MorenaPVEMPT_2021/Federales_2021$total_2021
Federales_2021$PC_otros_2021 <- Federales_2021$otros_2021/Federales_2021$total_2021
Federales_2021$PC_panpriprd_2021 <- Federales_2021$panpriprd_2021/Federales_2021$total_2021
Federales_2021$PC_mc_2021 <- Federales_2021$mc_2021/Federales_2021$total_2021
Federales_2021$PC_nulos_2021 <- Federales_2021$nulos_2021/Federales_2021$total_2021
Federales_2021$PC_total_2021 <- Federales_2021$total_2021/Federales_2021$lista_nominal_2021

# Casillas con 90% o mas del voto para un partido, quien gano y en que estados estan.
Federales_2021_90 <- subset(Federales_2021, PC_MorenaPVEMPT_2021 >= 0.9 | 
                                     PC_otros_2021 >= 0.9 | 
                                     PC_panpriprd_2021>= 0.9 | 
                                     PC_mc_2021>= 0.9 | 
                                     PC_nulos_2021>= 0.9)
View(Federales_2021_90)

# Variable partido ganador

Federales_2021_90$partido_ganador <- NA
Federales_2021_90$partido_ganador <- ifelse(
                                  Federales_2021_90$PC_MorenaPVEMPT_2021 > Federales_2021_90$PC_otros_2021 &
                                  Federales_2021_90$PC_MorenaPVEMPT_2021 > Federales_2021_90$PC_panpriprd_2021 &
                                  Federales_2021_90$PC_MorenaPVEMPT_2021 > Federales_2021_90$PC_mc_2021 &
                                  Federales_2021_90$PC_MorenaPVEMPT_2021 > Federales_2021_90$PC_nulos_2021,
                                  "Morena-PVE-PT", Federales_2021_90$partido_ganador)

Federales_2021_90$partido_ganador <- ifelse(
                                  Federales_2021_90$PC_panpriprd_2021 > Federales_2021_90$PC_otros_2021 &
                                  Federales_2021_90$PC_panpriprd_2021 > Federales_2021_90$PC_MorenaPVEMPT_2021 &
                                  Federales_2021_90$PC_panpriprd_2021 > Federales_2021_90$PC_mc_2021 &
                                  Federales_2021_90$PC_panpriprd_2021 > Federales_2021_90$PC_nulos_2021,
                                  "PAN-PRI-PRD", Federales_2021_90$partido_ganador)

Federales_2021_90$partido_ganador <- ifelse(
                                  Federales_2021_90$PC_otros_2021 > Federales_2021_90$PC_panpriprd_2021 &
                                  Federales_2021_90$PC_otros_2021 > Federales_2021_90$PC_MorenaPVEMPT_2021 &
                                  Federales_2021_90$PC_otros_2021 > Federales_2021_90$PC_mc_2021 &
                                  Federales_2021_90$PC_otros_2021 > Federales_2021_90$PC_nulos_2021,
                                  "OTROS", Federales_2021_90$partido_ganador)

Federales_2021_90$partido_ganador <- ifelse(
                                  Federales_2021_90$PC_mc_2021 > Federales_2021_90$PC_panpriprd_2021 &
                                  Federales_2021_90$PC_mc_2021 > Federales_2021_90$PC_MorenaPVEMPT_2021 &
                                  Federales_2021_90$PC_mc_2021 > Federales_2021_90$PC_otros_2021 &
                                  Federales_2021_90$PC_mc_2021 > Federales_2021_90$PC_nulos_2021,
                                  "MC", Federales_2021_90$partido_ganador)

Federales_2021_90$partido_ganador <- ifelse(
                                  Federales_2021_90$PC_nulos_2021 > Federales_2021_90$PC_panpriprd_2021 &
                                  Federales_2021_90$PC_nulos_2021 > Federales_2021_90$PC_MorenaPVEMPT_2021 &
                                  Federales_2021_90$PC_nulos_2021 > Federales_2021_90$PC_otros_2021 &
                                  Federales_2021_90$PC_nulos_2021 > Federales_2021_90$PC_mc_2021,
                                  "NULOS", Federales_2021_90$partido_ganador)

# Verifico que no se crearon missing values en el partido ganador

has_missing <- any(is.na(Federales_2021_90$partido_ganador))
print(has_missing)  # TRUE

# Generar tabla de frecuencia con cruce entre las dos variables
tabla_frecuencia <- table(Federales_2021_90$estado_2021, Federales_2021_90$partido_ganador)
#tabla_frecuencia <- table(Federales_2021_90$partido_ganador)

# Imprimir la tabla de frecuencia
print(tabla_frecuencia)
#####################################################################################################################
####################################### TREEMAP
#####################################################################################################################
Federales_2021_90$contador <- 1

# Realiza el colapso de las variables sumando el contador
Federales_2021_90 <- aggregate(cbind(contador)
                            ~ estado_2021 + partido_ganador, Federales_2021_90, sum)

# Genero la abreviación de Estados

Federales_2021_90$est <- NA
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "AGUASCALIENTES", "AGS", Federales_2021_90$est)

Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "BAJA CALIFORNIA", "BC", Federales_2021_90$est)

Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "CHIAPAS", "CHIS", Federales_2021_90$est)

Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "CHIHUAHUA", "CHIH", Federales_2021_90$est)

Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "CIUDAD DE MÉXICO", "CDMX", Federales_2021_90$est)

Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "COAHUILA", "COAH", Federales_2021_90$est)
    
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "DURANGO", "DGO", Federales_2021_90$est)
      
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "GUANAJUATO", "GTO", Federales_2021_90$est)
        
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "GUERRERO", "GRO", Federales_2021_90$est)
  
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "HIDALGO", "HGO", Federales_2021_90$est)
  
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 == "JALISCO", "JAL", Federales_2021_90$est)
  
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="MÉXICO", "EDOMEX", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="MICHOACÁN", "MICH", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="NAYARIT", "NAY", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="NUEVO LEÓN", "NL", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="OAXACA", "OAX", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="PUEBLA", "PUE", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="QUERÉTARO", "QRO", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="QUINTANA ROO", "ROO", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="SAN LUIS POTOSÍ", "SLP", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="SINALOA", "SIN", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="TABASCO", "TAB", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="TAMAULIPAS", "TAMS", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="VERACRUZ", "VER", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="YUCATÁN", "YUC", Federales_2021_90$est)
Federales_2021_90$est <- ifelse(
  Federales_2021_90$estado_2021 =="ZACATECAS", "ZAC", Federales_2021_90$est)

# Genero mi identificador de partido ganador

Federales_2021_90$color <- 3
Federales_2021_90$color <- ifelse(
  Federales_2021_90$partido_ganador == "PAN-PRI-PRD", 1, Federales_2021_90$color)

Federales_2021_90$color <- ifelse(
  Federales_2021_90$partido_ganador == "Morena-PVE-PT", 2, Federales_2021_90$color)

Federales_2021_90$color <- as.factor(Federales_2021_90$color)

# Generar una paleta de colores basada en los valores de la columna "color"

colores_partido <- unique(Federales_2021_90$color)
paleta_colores <- scales::hue_pal()(length(levels(Federales_2021_90$color)))

# Asignar colores específicos en la paleta

colores_especificos <- c("1" = "green4", "2" = "brown4")
paleta_colores <- ifelse(levels(Federales_2021_90$color) %in% names(colores_especificos), colores_especificos[levels(Federales_2021_90$color)], "gray")

# TREEMAP

ggplot(Federales_2021_90, aes(area = contador, fill = color)) +
  geom_treemap() +
  geom_treemap(color = "black") +
  geom_treemap_text(aes(label = est), colour = "white", place = "centre", size = 15) +
  scale_fill_manual(values = paleta_colores) +
  theme(legend.position = "none",
        text = element_text(family = fuente_deseada))

ggsave("Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/4.Graficas/TREEMAP_FEDERALES2021.jpg", plot = last_plot(),width = 8, height = 6, dpi = 300, limitsize = FALSE)

# Especifica la ruta y el nombre del archivo Excel de salida
ruta_salida <- "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/casillas_votos-90.xls"

# Crea un nuevo archivo de Excel
wb <- createWorkbook()

# Agrega la tabla al archivo de Excel
addWorksheet(wb, "tabla_frecuencia")
writeData(wb, sheet = 1, x = Federales_2021_90, startRow = 1, startCol = 1)

# Guarda el archivo de Excel
saveWorkbook(wb, ruta_salida, overwrite = TRUE)