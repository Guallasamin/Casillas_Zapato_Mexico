library(dplyr)
library(tidyr)
library(readr)
library(treemapify)
library(ggplot2)
library(readxl)
EDOMEX_GUB_2023 <- read_excel(
                  "Dropbox/Tareas_Jonathan/2023.06.12 Casillas Zapato/1. Bases/Resultados_Computos_2023/Resultados_Cขmputos_2023_Casilla-PUBLICACIเN (ok).xlsx")

View(EDOMEX_GUB_2023)
names (EDOMEX_GUB_2023)

EDOMEX_GUB_2023 <- EDOMEX_GUB_2023[EDOMEX_GUB_2023$LISTA_NOMINAL != 0, ]
EDOMEX_GUB_2023 <- EDOMEX_GUB_2023[EDOMEX_GUB_2023$TOTAL != 0, ]

# Verificar que se hallan borrado las listas nominales iguales a 0
min <- min(EDOMEX_GUB_2023$LISTA_NOMINAL)

print(min)

# Verificar los nombres de las columnas en el objeto EDOMEX_GUB_2023
names(EDOMEX_GUB_2023)

# Calcula la suma de las variables indicadas y crea una nueva variable MORENA
EDOMEX_GUB_2023$Alianza_2023 <- rowSums(EDOMEX_GUB_2023[, c("PAN",
                                                            "PRI",
                                                            "PRD",
                                                            "NAEM",
                                                            "PAN_PRI_PRD_NAEM",
                                                            "PAN_PRI_PRD",
                                                            "PAN_PRI_NAEM",
                                                            "PAN_PRD_NAEM",
                                                            "PRI_PRD_NAEM",
                                                            "PAN_PRI",
                                                            "PAN_PRD",
                                                            "PAN_NAEM",
                                                            "PRI_PRD",
                                                            "PRI_NAEM",
                                                            "PRD_NAEM")], na.rm = TRUE)

# Calcula la suma de las variables cc_pvem_pt_morena y crea una nueva variable OTROS
EDOMEX_GUB_2023$Morena_2023 <- rowSums(EDOMEX_GUB_2023[, c("PVEM_PT_MORENA")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable PAN-PRI-PRD
EDOMEX_GUB_2023$Otro_2023 <- rowSums(EDOMEX_GUB_2023[, c("NO_REGISTRADOS")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable MC
EDOMEX_GUB_2023$Nulos_2023 <- rowSums(EDOMEX_GUB_2023[, c("NULOS")], na.rm = TRUE)

# Renombra las variables
names(EDOMEX_GUB_2023)[names(EDOMEX_GUB_2023) == "LISTA_NOMINAL"] <- "lista_nominal_2023"
names(EDOMEX_GUB_2023)[names(EDOMEX_GUB_2023) == "TOTAL"] <- "total_2023"
names(EDOMEX_GUB_2023)[names(EDOMEX_GUB_2023) == "NOMBRE_ESTADO"] <- "estado_2023"

# Realiza el colapso de las variables sumando por las variables indicadas
EDOMEX_GUB_2023 <- aggregate(cbind(Alianza_2023, Morena_2023, Otro_2023, Nulos_2023, lista_nominal_2023, total_2023 )
                             ~ IDCASILLA + CABECERA_DISTRITAL_LOCAL, EDOMEX_GUB_2023, FUN = function(x) sum(x, na.rm = TRUE))

# Crea la variable año con el valor 2023
EDOMEX_GUB_2023$año <- 2023

# Comprobacion
EDOMEX_GUB_2023$comprobacion <- rowSums(EDOMEX_GUB_2023[, c("Morena_2023", "Alianza_2023", "Otro_2023", "Nulos_2023")], na.rm = TRUE)
suma_comprobación <- sum(EDOMEX_GUB_2023$comprobacion, na.rm = TRUE)
suma_total_votos <- sum(EDOMEX_GUB_2023$total_2023, na.rm = TRUE)
print (suma_comprobación)
print (suma_total_votos)

# Participaciones
EDOMEX_GUB_2023$PC_Alianza2023 <- EDOMEX_GUB_2023$Alianza_2023/EDOMEX_GUB_2023$total_2023
EDOMEX_GUB_2023$PC_Morena2023 <- EDOMEX_GUB_2023$Morena_2023/EDOMEX_GUB_2023$total_2023
EDOMEX_GUB_2023$PC_Otro_2023 <- EDOMEX_GUB_2023$Otro_2023/EDOMEX_GUB_2023$total_2023
EDOMEX_GUB_2023$PC_Nulos_2023 <- EDOMEX_GUB_2023$Nulos_2023/EDOMEX_GUB_2023$total_2023
EDOMEX_GUB_2023$PC_total_2023 <- EDOMEX_GUB_2023$total_2023/EDOMEX_GUB_2023$lista_nominal_2023

#####################################################################################################################
####################################### SCATTERPLOT
#####################################################################################################################

ggplot(EDOMEX_GUB_2023) +
  geom_point(aes(x = PC_Morena2023, y = PC_Alianza2023, color = factor((PC_Morena2023 > 0.9 & PC_Alianza2023 <= 0.1) | 
                                                                         (PC_Alianza2023 > 0.9 & PC_Morena2023 <= 0.1) |
                                                                         (PC_Morena2023 > 0.9 & PC_Alianza2023 > 0.9))), size = 2) +
  scale_color_manual(values = c(alpha("black", 0.2), "darkgreen"), 
                     labels = c("Porcentaje de votos", "Alianza (Porcentaje mayor al 90% de votos)")) +
  guides(color = guide_legend(title = NULL))+
  labs(x = "Porcentaje de votos de Morena por casilla", y = "Porcentaje de votos de Alianza por casilla") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = alpha("black", 0.2)),
        legend.position="top")+
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("Dropbox/Tareas_Jonathan/2023.06.12 Casillas Zapato/4.Graficas/SCATTER_PLOT_EDOMEX2023v2.jpg", plot = last_plot(),width = 6, height = 6, dpi = 300, limitsize = FALSE)

# Casillas con 90% o mas del voto para un partido, quien gano y en que estados estan.
EDOMEX_GUB_2023_90 <- subset(EDOMEX_GUB_2023, PC_Morena2023 >= 0.9 | 
                               PC_Otro_2023 >= 0.9 | 
                               PC_Alianza2023>= 0.9 | 
                               PC_Nulos_2023>= 0.9)
View(EDOMEX_GUB_2023_90)

# Variable partido ganador

EDOMEX_GUB_2023_90$partido_ganador <- NA
EDOMEX_GUB_2023_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2023_90$PC_Morena2023 > EDOMEX_GUB_2023_90$PC_Otro_2023 &
    EDOMEX_GUB_2023_90$PC_Morena2023 > EDOMEX_GUB_2023_90$PC_Alianza2023 &
    EDOMEX_GUB_2023_90$PC_Morena2023 > EDOMEX_GUB_2023_90$PC_Nulos_2023,
  "Morena", EDOMEX_GUB_2023_90$partido_ganador)

EDOMEX_GUB_2023_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2023_90$PC_Alianza2023 > EDOMEX_GUB_2023_90$PC_Otro_2023 &
    EDOMEX_GUB_2023_90$PC_Alianza2023 > EDOMEX_GUB_2023_90$PC_Morena2023 &
    EDOMEX_GUB_2023_90$PC_Alianza2023 > EDOMEX_GUB_2023_90$PC_Nulos_2023,
  "Alianza", EDOMEX_GUB_2023_90$partido_ganador)

EDOMEX_GUB_2023_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2023_90$PC_Otro_2023 > EDOMEX_GUB_2023_90$PC_Alianza2023 &
    EDOMEX_GUB_2023_90$PC_Otro_2023 > EDOMEX_GUB_2023_90$PC_Morena2023 &
    EDOMEX_GUB_2023_90$PC_Otro_2023 > EDOMEX_GUB_2023_90$PC_Nulos_2023,
  "OTROS", EDOMEX_GUB_2023_90$partido_ganador)

EDOMEX_GUB_2023_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2023_90$PC_Nulos_2023 > EDOMEX_GUB_2023_90$PC_Alianza2023 &
    EDOMEX_GUB_2023_90$PC_Nulos_2023 > EDOMEX_GUB_2023_90$PC_Morena2023 &
    EDOMEX_GUB_2023_90$PC_Nulos_2023 > EDOMEX_GUB_2023_90$PC_Otro_2023,
  "NULOS", EDOMEX_GUB_2023_90$partido_ganador)

# Verifico que no se crearon missing values en el partido ganador

has_missing <- any(is.na(EDOMEX_GUB_2023_90$partido_ganador))
print(has_missing)  # TRUE

names(EDOMEX_GUB_2023_90)

# Generar tabla de frecuencia con cruce entre las dos variables
tabla_frecuencia <- table(EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL, EDOMEX_GUB_2023_90$partido_ganador)
#tabla_frecuencia <- table(EDOMEX_GUB_2023_90$partido_ganador)

# Imprimir la tabla de frecuencia
print(tabla_frecuencia)

#####################################################################################################################
####################################### TREEMAP
#####################################################################################################################

# Genero la abreviación de Municipios

EDOMEX_GUB_2023_90$est <- NA

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "ATLACOMULCO DE FABELA", "Atlacomulco", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "CD. NEZAHUALCÓYOTL", "Nezahualcóyotl", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "CHALCO DE DÍAZ COVARRUBIAS", "Chalco de Díaz", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "CIUDAD ADOLFO LÓPEZ MATEOS", "López Mateos", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "COACALCO DE BERRIOZÁBAL", "Coacalco", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "CUAUTITLÁN IZCALLI", "Cuautitlán", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "ECATEPEC DE MORELOS", "Ecatepec", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "HUIXQUILUCAN DE DEGOLLADO", "Huixquilucan", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "JILOTEPEC DE ANDRES MOLINA ENRIQUEZ", "Jilotepec", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "METEPEC", "Metepec", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "NAUCALPAN DE JUÁREZ", "Naucalpan", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "SAN MIGUEL ZINACANTEPEC", "Zinacantepec", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "TEJUPILCO DE HIDALGO", "Tejupilco", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "TEXCOCO DE MORA", "Texcoco", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "TLALNEPANTLA DE BAZ", "Tlalnepantla", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "TOLUCA DE LERDO", "Toluca", EDOMEX_GUB_2023_90$est)

EDOMEX_GUB_2023_90$est <- ifelse(
  EDOMEX_GUB_2023_90$CABECERA_DISTRITAL_LOCAL == "VALLE DE CHALCO SOLIDARIDAD", "Valle de Chalco", EDOMEX_GUB_2023_90$est)


EDOMEX_GUB_2023_90$contador <- 1

# Realiza el colapso de las variables sumando por las variables indicadas
EDOMEX_GUB_2023_90 <- aggregate(cbind(contador)
                                ~ est + partido_ganador, EDOMEX_GUB_2023_90, sum)



# Asignar colores específicos en la paleta
colores_especificos <- c("Alianza" = "green4")
paleta_colores <- ifelse(EDOMEX_GUB_2023_90$partido_ganador %in% names(colores_especificos), colores_especificos[EDOMEX_GUB_2023_90$partido_ganador], "gray")

# TREEMAP

ggplot(EDOMEX_GUB_2023_90, aes(area = contador, fill = partido_ganador)) +
  geom_treemap() +
  geom_treemap_text(aes(label = est), colour = "white", place = "centre", size = 20) +
  scale_fill_manual(values = paleta_colores, na.value = "gray") +
  theme(legend.position = "none")

ggsave("Dropbox/Tareas_Jonathan/2023.06.12 Casillas Zapato/4.Graficas/TREEMAP_EDOMEX2023V2.jpg", plot = last_plot(),width = 8, height = 6, dpi = 300, limitsize = FALSE)

# Especifica la ruta y el nombre del archivo Excel de salida
ruta_salida <- "Dropbox/Tareas_Jonathan/2023.06.12 Casillas Zapato/1. Bases/EDOMEXUILA_2023V2.xls"

# Crea un nuevo archivo de Excel
wb <- createWorkbook()

# Agrega la tabla al archivo de Excel
addWorksheet(wb, "tabla_frecuencia")
writeData(wb, sheet = 1, x = EDOMEX_GUB_2023_90, startRow = 1, startCol = 1)

# Guarda el archivo de Excel
saveWorkbook(wb, ruta_salida, overwrite = TRUE)