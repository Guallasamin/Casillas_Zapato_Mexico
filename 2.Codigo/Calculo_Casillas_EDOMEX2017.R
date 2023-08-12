library(dplyr)
library(tidyr)
library(readxl)
library(readr)

EDOMEX_GUB_2017 <- read_excel("Dropbox/Tareas_Jonathan/2023.06.06 Elecciones_Coahuila_EDOMEX/Bases de datos/Originales/Res_Definitivos_Gobernador_2017_por_sección.xlsx")

View(EDOMEX_GUB_2017)
names (EDOMEX_GUB_2017)

EDOMEX_GUB_2017 <- EDOMEX_GUB_2017[EDOMEX_GUB_2017$TOTAL_VOTOS != 0, ]
EDOMEX_GUB_2017 <- EDOMEX_GUB_2017[EDOMEX_GUB_2017$LISTA_NOMINAL != 0, ]

min <- min(EDOMEX_GUB_2017$LISTA_NOMINAL)

print(min)

# Verificar los nombres de las columnas en el objeto EDOMEX_GUB_2017
names(EDOMEX_GUB_2017)

# Calcula la suma de las variables indicadas y crea una nueva variable MORENA
EDOMEX_GUB_2017$PRI_2017 <- rowSums(EDOMEX_GUB_2017[, c("PRI",
                                                        "PRI_PVEM_NVA_ALIANZA_ES",
                                                        "PRI_PVEM_NVA_ALIANZA",
                                                        "PRI_PVEM_ES",
                                                        "PRI_NVA_ALIANZA_ES",
                                                        "PRI_PVEM",
                                                        "PRI_NVA_ALIANZA",
                                                        "PRI_ES",
                                                        "PVEM",
                                                        "NVA_ALIANZA",
                                                        "ES",
                                                        "NVA_ALIANZA_ES",
                                                        "PVEM_NVA_ALIANZA_ES",
                                                        "PVEM_NVA_ALIANZA",
                                                        "PVEM_ES")], na.rm = TRUE)

# Calcula la suma de las variables cc_pvem_pt_morena y crea una nueva variable OTROS
EDOMEX_GUB_2017$Morena_2017 <- rowSums(EDOMEX_GUB_2017[, c("MORENA")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable PAN-PRI-PRD
EDOMEX_GUB_2017$PAN_2017 <- rowSums(EDOMEX_GUB_2017[, c("PAN")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable MC
EDOMEX_GUB_2017$Nulos_2017 <- rowSums(EDOMEX_GUB_2017[, c("NUM_VOTOS_NULOS")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable MC
EDOMEX_GUB_2017$PRD_2017 <- rowSums(EDOMEX_GUB_2017[, c("PRD")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable MC
EDOMEX_GUB_2017$Otros_2017 <- rowSums(EDOMEX_GUB_2017[, c("CAND_IND1", "PT", "NUM_VOTOS_CAN_NREG")], na.rm = TRUE)

# Renombra las variables
names(EDOMEX_GUB_2017)[names(EDOMEX_GUB_2017) == "LISTA_NOMINAL"] <- "lista_nominal_2017"
names(EDOMEX_GUB_2017)[names(EDOMEX_GUB_2017) == "TOTAL_VOTOS"] <- "total_2017"
names(EDOMEX_GUB_2017)[names(EDOMEX_GUB_2017) == "CABECERA_DISTRITAL"] <- "estado_2017"

# Realiza el colapso de las variables sumando por las variables indicadas
EDOMEX_GUB_2017 <- aggregate(cbind(PRI_2017, Morena_2017, PAN_2017, Nulos_2017, PRD_2017, Otros_2017, total_2017, lista_nominal_2017  )
                           ~ estado_2017 + CIRCUNSCRIPCION + SECCION + CASILLAS, EDOMEX_GUB_2017, FUN = function(x) sum(x, na.rm = TRUE))
names(EDOMEX_GUB_2017)
# Crea la variable año con el valor 2023
EDOMEX_GUB_2017$año <- 2017

# Comprobacion
EDOMEX_GUB_2017$comprobacion <- rowSums(EDOMEX_GUB_2017[, c("Morena_2017", "PRI_2017","PAN_2017","Otros_2017", "Nulos_2017", "PRD_2017")], na.rm = TRUE)
suma_comprobación <- sum(EDOMEX_GUB_2017$comprobacion, na.rm = TRUE)
suma_total_votos <- sum(EDOMEX_GUB_2017$total_2017, na.rm = TRUE)
print (suma_comprobación)
print (suma_total_votos)

# Participaciones
EDOMEX_GUB_2017$PC_PRI_2017 <- EDOMEX_GUB_2017$PRI_2017/EDOMEX_GUB_2017$total_2017
EDOMEX_GUB_2017$PC_Morena_2017 <- EDOMEX_GUB_2017$Morena_2017/EDOMEX_GUB_2017$total_2017
EDOMEX_GUB_2017$PC_PAN_2017 <- EDOMEX_GUB_2017$PAN_2017/EDOMEX_GUB_2017$total_2017
EDOMEX_GUB_2017$PC_PRD_2017 <- EDOMEX_GUB_2017$PRD_2017/EDOMEX_GUB_2017$total_2017
EDOMEX_GUB_2017$PC_Nulos_2017 <- EDOMEX_GUB_2017$Nulos_2017/EDOMEX_GUB_2017$total_2017
EDOMEX_GUB_2017$PC_Otros_2017 <- EDOMEX_GUB_2017$Otros_2017/EDOMEX_GUB_2017$total_2017
EDOMEX_GUB_2017$PC_total_2017 <- EDOMEX_GUB_2017$Otros_2017/EDOMEX_GUB_2017$lista_nominal_2017

# SCATTER PLOT
ggplot(EDOMEX_GUB_2017) +
  geom_point(aes(x = PC_PRI_2017, y = PC_Morena_2017, color = factor((PC_PRI_2017 > 0.9 & PC_Morena_2017 <= 0.1) | 
                                                                         (PC_Morena_2017 > 0.9 & PC_PRI_2017 <= 0.1) |
                                                                         (PC_PRI_2017 > 0.9 & PC_Morena_2017 > 0.9))), size = 2) +
  scale_color_manual(values = c(alpha("black", 0.2), "darkgreen"), 
                     labels = c("Porcentaje de votos", "PRI (Porcentaje mayor al 90% de votos)")) +
  guides(color = guide_legend(title = NULL))+
  labs(x = "Porcentaje de votos de PRI por casilla", y = "Porcentaje de votos de Morena por casilla") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = alpha("black", 0.2)),
        legend.position="top")+
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/4.Graficas/SCATTER_PLOT_EDOMEX2017.jpg", plot = last_plot(),width = 6, height = 6, dpi = 300, limitsize = FALSE)


# Calcula la suma de las variables cc_pvem_pt_morena y crea una nueva variable OTROS
EDOMEX_GUB_2017$ALIANZA <- rowSums(EDOMEX_GUB_2017[, c("PAN_2017", "PRD_2017", "PRI_2017")], na.rm = TRUE)
EDOMEX_GUB_2017$PC_ALIANZA <- EDOMEX_GUB_2017$ALIANZA/EDOMEX_GUB_2017$total_2017

# Casillas con 90% o mas del voto para un partido, quien gano y en que estados estan.
EDOMEX_GUB_2017_ALIANZA90 <- subset(EDOMEX_GUB_2017, PC_ALIANZA >= 0.9 | 
                               PC_Morena_2017 >= 0.9 | 
                               PC_Nulos_2017>= 0.9 |
                               PC_Otros_2017>= 0.9 )

View(EDOMEX_GUB_2017_ALIANZA90)

# Casillas con 90% o mas del voto para un partido, quien gano y en que estados estan.
EDOMEX_GUB_2017_90 <- subset(EDOMEX_GUB_2017, PC_PRI_2017 >= 0.9 | 
                             PC_Morena_2017 >= 0.9 | 
                             PC_PAN_2017>= 0.9 |
                             PC_Nulos_2017>= 0.9 |
                             PC_Otros_2017>= 0.9 |
                             PC_PRD_2017>= 0.9)
View(EDOMEX_GUB_2017_90)

# Variable partido ganador

EDOMEX_GUB_2017_90$partido_ganador <- NA
EDOMEX_GUB_2017_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2017_90$PC_Morena_2017 > EDOMEX_GUB_2017_90$PC_PRI_2017 &
    EDOMEX_GUB_2017_90$PC_Morena_2017 > EDOMEX_GUB_2017_90$PC_PAN_2017 &
    EDOMEX_GUB_2017_90$PC_Morena_2017 > EDOMEX_GUB_2017_90$PC_Nulos_2017 &
    EDOMEX_GUB_2017_90$PC_Morena_2017 > EDOMEX_GUB_2017_90$PC_Otros_2017 &
    EDOMEX_GUB_2017_90$PC_Morena_2017 > EDOMEX_GUB_2017_90$PC_PRD_2017,
  "Morena", EDOMEX_GUB_2017_90$partido_ganador)

EDOMEX_GUB_2017_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2017_90$PC_PRI_2017 > EDOMEX_GUB_2017_90$PC_Morena_2017 &
    EDOMEX_GUB_2017_90$PC_PRI_2017 > EDOMEX_GUB_2017_90$PC_PAN_2017 &
    EDOMEX_GUB_2017_90$PC_PRI_2017 > EDOMEX_GUB_2017_90$PC_Nulos_2017 &
    EDOMEX_GUB_2017_90$PC_PRI_2017 > EDOMEX_GUB_2017_90$PC_Otros_2017 &
    EDOMEX_GUB_2017_90$PC_PRI_2017 > EDOMEX_GUB_2017_90$PC_PRD_2017,
  "PRI", EDOMEX_GUB_2017_90$partido_ganador)

EDOMEX_GUB_2017_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2017_90$PC_PAN_2017 > EDOMEX_GUB_2017_90$PC_PRI_2017 &
    EDOMEX_GUB_2017_90$PC_PAN_2017 > EDOMEX_GUB_2017_90$PC_Morena_2017 &
    EDOMEX_GUB_2017_90$PC_PAN_2017 > EDOMEX_GUB_2017_90$PC_Nulos_2017 &
    EDOMEX_GUB_2017_90$PC_PAN_2017 > EDOMEX_GUB_2017_90$PC_Otros_2017 &
    EDOMEX_GUB_2017_90$PC_PAN_2017 > EDOMEX_GUB_2017_90$PC_PRD_2017,
  "PAN", EDOMEX_GUB_2017_90$partido_ganador)

EDOMEX_GUB_2017_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2017_90$PC_PRD_2017 > EDOMEX_GUB_2017_90$PC_PRI_2017 &
    EDOMEX_GUB_2017_90$PC_PRD_2017 > EDOMEX_GUB_2017_90$PC_PAN_2017 &
    EDOMEX_GUB_2017_90$PC_PRD_2017 > EDOMEX_GUB_2017_90$PC_Nulos_2017 &
    EDOMEX_GUB_2017_90$PC_PRD_2017 > EDOMEX_GUB_2017_90$PC_Otros_2017 &
    EDOMEX_GUB_2017_90$PC_PRD_2017 > EDOMEX_GUB_2017_90$PC_Morena_2017,
  "PRD", EDOMEX_GUB_2017_90$partido_ganador)

EDOMEX_GUB_2017_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2017_90$PC_Nulos_2017 > EDOMEX_GUB_2017_90$PC_PRI_2017 &
    EDOMEX_GUB_2017_90$PC_Nulos_2017 > EDOMEX_GUB_2017_90$PC_PAN_2017 &
    EDOMEX_GUB_2017_90$PC_Nulos_2017 > EDOMEX_GUB_2017_90$PC_Morena_2017 &
    EDOMEX_GUB_2017_90$PC_Nulos_2017 > EDOMEX_GUB_2017_90$PC_Otros_2017 &
    EDOMEX_GUB_2017_90$PC_Nulos_2017 > EDOMEX_GUB_2017_90$PC_PRD_2017,
  "Nulos", EDOMEX_GUB_2017_90$partido_ganador)

EDOMEX_GUB_2017_90$partido_ganador <- ifelse(
  EDOMEX_GUB_2017_90$PC_Otros_2017 > EDOMEX_GUB_2017_90$PC_PRI_2017 &
    EDOMEX_GUB_2017_90$PC_Otros_2017 > EDOMEX_GUB_2017_90$PC_PAN_2017 &
    EDOMEX_GUB_2017_90$PC_Otros_2017 > EDOMEX_GUB_2017_90$PC_Nulos_2017 &
    EDOMEX_GUB_2017_90$PC_Otros_2017 > EDOMEX_GUB_2017_90$PC_Morena_2017 &
    EDOMEX_GUB_2017_90$PC_Otros_2017 > EDOMEX_GUB_2017_90$PC_PRD_2017,
  "Otros", EDOMEX_GUB_2017_90$partido_ganador)

# Verifico que no se crearon missing values en el partido ganador

has_missing <- any(is.na(EDOMEX_GUB_2017_90$partido_ganador))
print(has_missing)  # TRUE

# Generar tabla de frecuencia con cruce entre las dos variables
tabla_frecuencia <- table(EDOMEX_GUB_2017_90$municipio, EDOMEX_GUB_2017_90$partido_ganador)
#tabla_frecuencia <- table(EDOMEX_GUB_2017_90$partido_ganador)

# Imprimir la tabla de frecuencia
print(tabla_frecuencia)

# Especifica la ruta y el nombre del archivo Excel de salida
ruta_salida <- "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/COAHUILA_2017.xls"

# Crea un nuevo archivo de Excel
wb <- createWorkbook()

# Agrega la tabla al archivo de Excel
addWorksheet(wb, "tabla_frecuencia")
writeData(wb, sheet = 1, x = EDOMEX_GUB_2017, startRow = 1, startCol = 1)

# Guarda el archivo de Excel
saveWorkbook(wb, ruta_salida, overwrite = TRUE)