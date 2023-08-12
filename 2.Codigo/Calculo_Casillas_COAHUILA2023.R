library(dplyr)
library(tidyr)
library(openxlsx)
library(readr)

COAH_GUB_2023 <- read_csv("Dropbox/Tareas_Jonathan/2023.06.06 Elecciones_Coahuila_EDOMEX/Bases de datos/Originales/20230605_1315_PREP_COAH_NUEVA/20230605_1315_PREP_GUB_COAH/COAH_GUB_2023.csv", 
                          skip = 5)
View(COAH_GUB_2023)
names (COAH_GUB_2023)

COAH_GUB_2023[COAH_GUB_2023 == "SIN DATO"] <- NA
COAH_GUB_2023[COAH_GUB_2023 == "Sin Acta"] <- NA
COAH_GUB_2023[COAH_GUB_2023 == "ILEGIBLE"] <- NA
COAH_GUB_2023[COAH_GUB_2023 == "N/A"] <- NA
COAH_GUB_2023$ENTIDAD <- ifelse(is.na(COAH_GUB_2023$ENTIDAD), "No determinado", COAH_GUB_2023$ENTIDAD)
COAH_GUB_2023$MUNICIPIO <- ifelse(is.na(COAH_GUB_2023$MUNICIPIO), "No determinado", COAH_GUB_2023$MUNICIPIO)

COAH_GUB_2023 <- COAH_GUB_2023[COAH_GUB_2023$TIPO_CASILLA != "S", ]
COAH_GUB_2023 <- COAH_GUB_2023[COAH_GUB_2023$TOTAL_VOTOS_CALCULADO != 0, ]
COAH_GUB_2023 <- COAH_GUB_2023[COAH_GUB_2023$CONTABILIZADA != 0, ]
COAH_GUB_2023 <- COAH_GUB_2023[COAH_GUB_2023$CONTABILIZADA != 2, ]
#COAH_GUB_2023 <- na.omit(COAH_GUB_2023)

COAH_GUB_2023$PAN <- as.numeric(COAH_GUB_2023$PAN)
COAH_GUB_2023$PRI <- as.numeric(COAH_GUB_2023$PRI)
COAH_GUB_2023$PRD <- as.numeric(COAH_GUB_2023$PRD)
COAH_GUB_2023$C_PAN_PRI_PRD <- as.numeric(COAH_GUB_2023$C_PAN_PRI_PRD)
COAH_GUB_2023$C_PAN_PRI <- as.numeric(COAH_GUB_2023$C_PAN_PRI)
COAH_GUB_2023$C_PAN_PRD <- as.numeric(COAH_GUB_2023$C_PAN_PRD)
COAH_GUB_2023$C_PRI_PRD <- as.numeric(COAH_GUB_2023$C_PRI_PRD)
COAH_GUB_2023$C_PAN_PRI <- as.numeric(COAH_GUB_2023$C_PAN_PRI)
COAH_GUB_2023$C_PRI_PRD <- as.numeric(COAH_GUB_2023$C_PRI_PRD)
COAH_GUB_2023$C_PAN_PRD <- as.numeric(COAH_GUB_2023$C_PAN_PRD)
COAH_GUB_2023$MORENA <- as.numeric(COAH_GUB_2023$MORENA)
COAH_GUB_2023$C_PVEM_UDC <- as.numeric(COAH_GUB_2023$C_PVEM_UDC)
COAH_GUB_2023$UDC <- as.numeric(COAH_GUB_2023$UDC)
COAH_GUB_2023$PVEM <- as.numeric(COAH_GUB_2023$PVEM)
COAH_GUB_2023$PT <- as.numeric(COAH_GUB_2023$PT)
COAH_GUB_2023$NULOS <- as.numeric(COAH_GUB_2023$NULOS)
COAH_GUB_2023$NO_REGISTRADAS <- as.numeric(COAH_GUB_2023$NO_REGISTRADAS)
COAH_GUB_2023$TOTAL_VOTOS_CALCULADO <- as.numeric(COAH_GUB_2023$TOTAL_VOTOS_CALCULADO)


# Verificar que se hallan borrado las listas nominales iguales a 0
min <- min(COAH_GUB_2023$LISTA_NOMINAL)

print(min)

# Verificar los nombres de las columnas en el objeto COAH_GUB_2023
names(COAH_GUB_2023)

# Calcula la suma de las variables indicadas y crea una nueva variable MORENA
COAH_GUB_2023$Alianza_2023 <- rowSums(COAH_GUB_2023[, c("PAN", "PRI", "PRD", 
                                                        "C_PAN_PRI_PRD", "C_PAN_PRI", "C_PAN_PRD",
                                                        "C_PRI_PRD")], na.rm = TRUE)

# Calcula la suma de las variables cc_pvem_pt_morena y crea una nueva variable OTROS
COAH_GUB_2023$Morena_2023 <- rowSums(COAH_GUB_2023[, c("MORENA")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable PAN-PRI-PRD
COAH_GUB_2023$Otro_2023 <- rowSums(COAH_GUB_2023[, c("C_PVEM_UDC", "UDC", "PVEM", "PT", "NO_REGISTRADAS")], na.rm = TRUE)

# Calcula la suma de la variable no_registradas y crea una nueva variable MC
COAH_GUB_2023$Nulos_2023 <- rowSums(COAH_GUB_2023[, c("NULOS")], na.rm = TRUE)

# Renombra las variables
names(COAH_GUB_2023)[names(COAH_GUB_2023) == "LISTA_NOMINAL"] <- "lista_nominal_2023"
names(COAH_GUB_2023)[names(COAH_GUB_2023) == "TOTAL_VOTOS_CALCULADO"] <- "total_2023"
names(COAH_GUB_2023)[names(COAH_GUB_2023) == "ENTIDAD"] <- "estado_2023"

# Realiza el colapso de las variables sumando por las variables indicadas
COAH_GUB_2023 <- aggregate(cbind(Alianza_2023, Morena_2023, Otro_2023, Nulos_2023, lista_nominal_2023, total_2023 )
                            ~ MUNICIPIO + CLAVE_CASILLA, COAH_GUB_2023, FUN = function(x) sum(x, na.rm = TRUE))

# Crea la variable año con el valor 2023
COAH_GUB_2023$año <- 2023

# Comprobacion
COAH_GUB_2023$comprobacion <- rowSums(COAH_GUB_2023[, c("Morena_2023", "Alianza_2023", "Otro_2023", "Nulos_2023")], na.rm = TRUE)
suma_comprobación <- sum(COAH_GUB_2023$comprobacion, na.rm = TRUE)
suma_total_votos <- sum(COAH_GUB_2023$total_2023, na.rm = TRUE)
print (suma_comprobación)
print (suma_total_votos)

# Participaciones
COAH_GUB_2023$PC_Alianza2023 <- COAH_GUB_2023$Alianza_2023/COAH_GUB_2023$total_2023
COAH_GUB_2023$PC_Morena2023 <- COAH_GUB_2023$Morena_2023/COAH_GUB_2023$total_2023
COAH_GUB_2023$PC_Otro_2023 <- COAH_GUB_2023$Otro_2023/COAH_GUB_2023$total_2023
COAH_GUB_2023$PC_Nulos_2023 <- COAH_GUB_2023$Nulos_2023/COAH_GUB_2023$total_2023
COAH_GUB_2023$PC_total_2023 <- COAH_GUB_2023$total_2023/COAH_GUB_2023$lista_nominal_2023

# SCATTER PLOT
ggplot(COAH_GUB_2023) +
  geom_point(aes(x = PC_Alianza2023, y = PC_Morena2023, color = factor((PC_Alianza2023 > 0.9 & PC_Morena2023 <= 0.1) | 
                                                                    (PC_Morena2023 > 0.9 & PC_Alianza2023 <= 0.1) |
                                                                    (PC_Alianza2023 > 0.9 & PC_Morena2023 > 0.9))), size = 2) +
  scale_color_manual(values = c(alpha("black", 0.2), "darkgreen"), 
                     labels = c("Porcentaje de votos", "Alianza (Porcentaje mayor al 90% de votos)")) +
  guides(color = guide_legend(title = NULL))+
  labs(x = "Porcentaje de votos de Alianza por casilla", y = "Porcentaje de votos de Morena por casilla") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = alpha("black", 0.2)),
        legend.position="top")+
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/4.Graficas/SCATTER_PLOT_COAHUILA2023.jpg", plot = last_plot(),width = 6, height = 6, dpi = 300, limitsize = FALSE)

# Casillas con 90% o mas del voto para un partido, quien gano y en que estados estan.
COAH_GUB_2023_90 <- subset(COAH_GUB_2023, PC_Morena2023 >= 0.9 | 
                             PC_Otro_2023 >= 0.9 | 
                             PC_Alianza2023>= 0.9 | 
                             PC_Nulos_2023>= 0.9)
View(COAH_GUB_2023_90)

# Variable partido ganador

COAH_GUB_2023_90$partido_ganador <- NA
COAH_GUB_2023_90$partido_ganador <- ifelse(
  COAH_GUB_2023_90$PC_Morena2023 > COAH_GUB_2023_90$PC_Otro_2023 &
    COAH_GUB_2023_90$PC_Morena2023 > COAH_GUB_2023_90$PC_Alianza2023 &
    COAH_GUB_2023_90$PC_Morena2023 > COAH_GUB_2023_90$PC_Nulos_2023,
  "Morena", COAH_GUB_2023_90$partido_ganador)

COAH_GUB_2023_90$partido_ganador <- ifelse(
  COAH_GUB_2023_90$PC_Alianza2023 > COAH_GUB_2023_90$PC_Otro_2023 &
    COAH_GUB_2023_90$PC_Alianza2023 > COAH_GUB_2023_90$PC_Morena2023 &
    COAH_GUB_2023_90$PC_Alianza2023 > COAH_GUB_2023_90$PC_Nulos_2023,
  "Alianza", COAH_GUB_2023_90$partido_ganador)

COAH_GUB_2023_90$partido_ganador <- ifelse(
  COAH_GUB_2023_90$PC_Otro_2023 > COAH_GUB_2023_90$PC_Alianza2023 &
    COAH_GUB_2023_90$PC_Otro_2023 > COAH_GUB_2023_90$PC_Morena2023 &
    COAH_GUB_2023_90$PC_Otro_2023 > COAH_GUB_2023_90$PC_Nulos_2023,
  "OTROS", COAH_GUB_2023_90$partido_ganador)

COAH_GUB_2023_90$partido_ganador <- ifelse(
  COAH_GUB_2023_90$PC_Nulos_2023 > COAH_GUB_2023_90$PC_Alianza2023 &
    COAH_GUB_2023_90$PC_Nulos_2023 > COAH_GUB_2023_90$PC_Morena2023 &
    COAH_GUB_2023_90$PC_Nulos_2023 > COAH_GUB_2023_90$PC_Otro_2023,
  "NULOS", COAH_GUB_2023_90$partido_ganador)

# Verifico que no se crearon missing values en el partido ganador

has_missing <- any(is.na(COAH_GUB_2023_90$partido_ganador))
print(has_missing)  # TRUE

# Generar tabla de frecuencia con cruce entre las dos variables
tabla_frecuencia <- table(COAH_GUB_2023_90$MUNICIPIO, COAH_GUB_2023_90$partido_ganador)
#tabla_frecuencia <- table(COAH_GUB_2023_90$partido_ganador)

# Imprimir la tabla de frecuencia
print(tabla_frecuencia)

# Especifica la ruta y el nombre del archivo Excel de salida
ruta_salida <- "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/COAHUILA_2023.xls"

# Crea un nuevo archivo de Excel
wb <- createWorkbook()

# Agrega la tabla al archivo de Excel
addWorksheet(wb, "tabla_frecuencia")
writeData(wb, sheet = 1, x = COAH_GUB_2023, startRow = 1, startCol = 1)

# Guarda el archivo de Excel
saveWorkbook(wb, ruta_salida, overwrite = TRUE)