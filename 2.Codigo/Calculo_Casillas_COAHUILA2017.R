
# Se cargan las librerias necesarias

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)

# Se cargan la base de datos original de Coahuila 2017

COAH_GUB_2017 <- read_excel("Dropbox/Tareas_Jonathan/2023.06.06 Elecciones_Coahuila_EDOMEX/Bases de datos/Originales/Gobernador_Xcasilla_Coahuila.xlsx")

names (COAH_GUB_2017)

COAH_GUB_2017 <- COAH_GUB_2017[COAH_GUB_2017$TOTAL != 0, ]
COAH_GUB_2017 <- COAH_GUB_2017[COAH_GUB_2017$LISTA_NOMINAL != 0, ]

# Se verifica que no existan missing values

min <- min(COAH_GUB_2017$LISTA_NOMINAL)

print(min)

# Verificar los nombres de las columnas en el objeto COAH_GUB_2017
names(COAH_GUB_2017)

# Calcula la suma de las variables indicadas y crea una nueva variable PRI_2017
COAH_GUB_2017$PRI_2017 <- rowSums(COAH_GUB_2017[, c("PRI-PVEM-PNA-SI-PJ-PRC-PCP",
                                                    "PRI-PVEM-PNA-SI-PJ-PRC",
                                                    "PRI-PVEM-PNA-SI-PJ-PCP",
                                                    "PRI-PVEM-PNA-SI-PRC-PCP",
                                                    "PRI-PVEM-PNA-PJ-PRC-PCP",
                                                    "PRI-PVEM-SI-PJ-PRC-PCP",
                                                    "PRI-PNA-SI-PJ-PRC-PCP",
                                                    "PRI-PVEM-PNA-SI-PJ",
                                                    "PRI-PVEM-PNA-SI-PRC",
                                                    "PRI-PVEM-PNA-SI-PCP",
                                                    "PRI-PVEM-PNA-PJ-PRC",
                                                    "PRI-PVEM-PNA-PJ-PCP",
                                                    "PRI-PVEM-PNA-PRC-PCP",
                                                    "PRI-PVEM-SI-PJ-PRC",
                                                    "PRI-PVEM-SI-PJ-PCP",
                                                    "PRI-PVEM-SI-PRC-PCP",
                                                    "PRI-PVEM-PJ-PRC-PCP",
                                                    "PRI-PNA-SI-PJ-PRC",
                                                    "PRI-PNA-SI-PJ-PCP",
                                                    "PRI-PNA-SI-PRC-PCP",
                                                    "PRI-PNA-PJ-PRC-PCP",
                                                    "PRI-SI-PJ-PRC-PCP",
                                                    "pri",
                                                    "PRI-PVEM-PNA-SI",
                                                    "PRI-PVEM-PNA-PJ",
                                                    "PRI-PVEM-PNA-PRC",
                                                    "PRI-PVEM-PNA-PCP",
                                                    "PRI-PVEM-SI-PJ",
                                                    "PRI-PVEM-SI-PRC",
                                                    "PRI-PVEM-SI-PCP",
                                                    "PRI-PVEM-PJ-PRC",
                                                    "PRI-PVEM-PJ-PCP",
                                                    "PRI-PVEM-PRC-PCP",
                                                    "PRI-PNA-SI-PJ",
                                                    "PRI-PNA-SI-PRC",
                                                    "PRI-PNA-SI-PCP",
                                                    "PRI-PNA-PJ-PRC",
                                                    "PRI-PNA-PJ-PCP",
                                                    "PRI-PNA-PRC-PCP",
                                                    "PRI-SI-PJ-PRC",
                                                    "PRI-SI-PJ-PCP",
                                                    "PRI-SI-PRC-PCP",
                                                    "PRI-PJ-PRC-PCP",
                                                    "PRI-PVEM-PNA",
                                                    "PRI-PVEM-SI",
                                                    "PRI-PVEM-PJ",
                                                    "PRI-PVEM-PRC",
                                                    "PRI-PVEM-PCP",
                                                    "PRI-PNA-SI",
                                                    "PRI-PNA-PJ",
                                                    "PRI-PNA-PRC",
                                                    "PRI-PNA-PCP",
                                                    "PRI-SI-PJ",
                                                    "PRI-SI-PRC",
                                                    "PRI-SI-PCP",
                                                    "PRI-PJ-PRC",
                                                    "PRI-PJ-PCP",
                                                    "PRI-PRC-PCP",
                                                    "PRI-PVEM",
                                                    "PRI-PNA",
                                                    "PRI-SI",
                                                    "PRI-PJ",
                                                    "PRI-PRC",
                                                    "PVEM-PNA-SI-PJ-PRC-PCP",
                                                    "PRI-PCP",
                                                    "PVEM-PNA-SI-PJ-PRC",
                                                    "PVEM-PNA-SI-PJ-PCP",
                                                    "PVEM-PNA-SI-PRC-PCP",
                                                    "PVEM-PNA-PJ-PRC-PCP",
                                                    "PVEM-SI-PJ-PRC-PCP",
                                                    "PNA-SI-PJ-PRC-PCP",
                                                    "PVEM-PNA-SI-PJ",
                                                    "PVEM-PNA-SI-PRC",
                                                    "PVEM-PNA-SI-PCP",
                                                    "PVEM-PNA-PJ-PRC",
                                                    "PVEM-PNA-PJ-PCP",
                                                    "PVEM-PNA-PRC-PCP",
                                                    "PVEM-SI-PJ-PRC",
                                                    "PVEM-SI-PJ-PCP",
                                                    "PVEM-SI-PRC-PCP",
                                                    "PVEM-PJ-PRC-PCP",
                                                    "PNA-SI-PJ-PRC",
                                                    "PNA-SI-PJ-PCP",
                                                    "NA-SI-PRC-PCP",
                                                    "PNA-PJ-PRC-PCP",
                                                    "SI-PJ-PRC-PCP",
                                                    "PVEM-PNA-SI",
                                                    "PVEM-PNA-PJ",
                                                    "PVEM-PNA-PRC",
                                                    "PVEM-PNA-PCP",
                                                    "PVEM-SI-PJ",
                                                    "PVEM-SI-PRC",
                                                    "PVEM-SI-PCP",
                                                    "PVEM-PJ-PRC",
                                                    "PVEM-PJ-PCP",
                                                    "PVEM-PRC-PCP",
                                                    "PNA-SI-PJ",
                                                    "PNA-SI-PRC",
                                                    "PNA-SI-PCP",
                                                    "PNA-PJ-PRC",
                                                    "PNA-PJ-PCP",
                                                    "PNA-PRC-PCP",
                                                    "SI-PJ-PRC",
                                                    "SI-PJ-PCP",
                                                    "SI-PRC-PCP",
                                                    "PJ-PRC-PCP",
                                                    "PVEM-PNA",
                                                    "PVEM-SI",
                                                    "PVEM-PJ",
                                                    "PVEM-PRC",
                                                    "PVEM-PCP",
                                                    "PNA-SI",
                                                    "PNA-PJ",
                                                    "PNA-PRC",
                                                    "PNA-PCP",
                                                    "SI-PJ",
                                                    "SI-PRC",
                                                    "SI-PCP",
                                                    "PJ-PRC",
                                                    "PJ-PCP",
                                                    "PRC-PCP",
                                                    "pvem",
                                                    "pna",
                                                    "si",
                                                    "pj",
                                                    "pcp",
                                                    "prc")], na.rm = TRUE)

# Calcula la suma de las variables indicadas y crea una nueva variable Morena_2017
COAH_GUB_2017$Morena_2017 <- rowSums(COAH_GUB_2017[, c("morena")], na.rm = TRUE)

# Calcula la suma de las variables indicadas y crea una nueva variable PAN_2017
COAH_GUB_2017$PAN_2017 <- rowSums(COAH_GUB_2017[, c("pan",
                                                    "PAN-UDC-PPC-ES",
                                                    "PAN-UDC-PPC",
                                                    "PAN-UDC-ES",
                                                    "PAN-PPC-ES",
                                                    "PAN-UDC",
                                                    "PAN-PPC",
                                                    "PAN-ES",
                                                    "ppc",
                                                    "udc",
                                                    "es",
                                                    "UDC-PPC-ES",
                                                    "UDC-PPC",
                                                    "UDC-ES",
                                                    "PPC-ES")], na.rm = TRUE)

# Calcula la suma de las variables indicadas y crea una nueva variable Nulos
COAH_GUB_2017$Nulos_2017 <- rowSums(COAH_GUB_2017[, c("nulos")], na.rm = TRUE)

# Calcula la suma de las variables indicadas y crea una nueva variable PRD_2017
COAH_GUB_2017$PRD_2017 <- rowSums(COAH_GUB_2017[, c("prd")], na.rm = TRUE)

# Calcula la suma de las variables indicadas y crea una nueva variable Otros_2017
COAH_GUB_2017$Otros_2017 <- rowSums(COAH_GUB_2017[, c("pmc", 
                                                      "pt", 
                                                      "cand_ind1", 
                                                      "cand_ind2", 
                                                      "cand_ind3", 
                                                      "cand_nreg")], na.rm = TRUE)

# Renombra las variables
names(COAH_GUB_2017)[names(COAH_GUB_2017) == "LISTA_NOMINAL"] <- "lista_nominal_2017"
names(COAH_GUB_2017)[names(COAH_GUB_2017) == "TOTAL"] <- "total_2017"
names(COAH_GUB_2017)[names(COAH_GUB_2017) == "ENTIDAD"] <- "estado_2017"

# Realiza el colapso de las variables sumando por las variables indicadas
COAH_GUB_2017 <- aggregate(cbind(PRI_2017, Morena_2017, PAN_2017, Nulos_2017, PRD_2017, Otros_2017, total_2017, lista_nominal_2017  )
                           ~ municipio + cve_cas2, COAH_GUB_2017, FUN = function(x) sum(x, na.rm = TRUE))

# Comprobacion
COAH_GUB_2017$comprobacion <- rowSums(COAH_GUB_2017[, c("Morena_2017", 
                                                        "PRI_2017",
                                                        "PAN_2017",
                                                        "Otros_2017", 
                                                        "Nulos_2017", 
                                                        "PRD_2017")], na.rm = TRUE)
suma_comprobación <- sum(COAH_GUB_2017$comprobacion, na.rm = TRUE)
suma_total_votos <- sum(COAH_GUB_2017$total_2017, na.rm = TRUE)
print (suma_comprobación)
print (suma_total_votos)

# Participaciones
COAH_GUB_2017$PC_PRI_2017 <- COAH_GUB_2017$PRI_2017/COAH_GUB_2017$total_2017
COAH_GUB_2017$PC_Morena_2017 <- COAH_GUB_2017$Morena_2017/COAH_GUB_2017$total_2017
COAH_GUB_2017$PC_PAN_2017 <- COAH_GUB_2017$PAN_2017/COAH_GUB_2017$total_2017
COAH_GUB_2017$PC_PRD_2017 <- COAH_GUB_2017$PRD_2017/COAH_GUB_2017$total_2017
COAH_GUB_2017$PC_Nulos_2017 <- COAH_GUB_2017$Nulos_2017/COAH_GUB_2017$total_2017
COAH_GUB_2017$PC_Otros_2017 <- COAH_GUB_2017$Otros_2017/COAH_GUB_2017$total_2017
COAH_GUB_2017$PC_total_2017 <- COAH_GUB_2017$Otros_2017/COAH_GUB_2017$lista_nominal_2017

# SCATTER PLOT
ggplot(COAH_GUB_2017) +
  geom_point(aes(x = PC_PRI_2017, y = PC_PAN_2017, color = factor((PC_PRI_2017 > 0.9 & PC_PAN_2017 <= 0.1) | 
                                                                     (PC_PAN_2017 > 0.9 & PC_PRI_2017 <= 0.1) |
                                                                     (PC_PRI_2017 > 0.9 & PC_PAN_2017 > 0.9))), size = 2) +
  scale_color_manual(values = c(alpha("black", 0.2), "darkgreen"), 
                     labels = c("Porcentaje de votos", "PRI (Porcentaje mayor al 90% de votos)")) +
  guides(color = guide_legend(title = NULL))+
  labs(x = "Porcentaje de votos de PRI por casilla", y = "Porcentaje de votos de PAN por casilla") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = alpha("black", 0.2)),
        legend.position="top")+
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/4.Graficas/SCATTER_PLOT_COAHUILA2017.jpg", plot = last_plot(),width = 6, height = 6, dpi = 300, limitsize = FALSE)
              
# Casillas con 90% o mas del voto para un partido, quien gano y en que municipio estan.
COAH_GUB_2017_90 <- subset(COAH_GUB_2017, PC_PRI_2017 >= 0.9 | 
                             PC_Morena_2017 >= 0.9 | 
                             PC_PAN_2017>= 0.9 |
                             PC_Nulos_2017>= 0.9 |
                             PC_Otros_2017>= 0.9 |
                             PC_PRD_2017>= 0.9)
View(COAH_GUB_2017_90)

# Variable partido ganador

COAH_GUB_2017_90$partido_ganador <- NA
COAH_GUB_2017_90$partido_ganador <- ifelse(
      COAH_GUB_2017_90$PC_Morena_2017 > COAH_GUB_2017_90$PC_PRI_2017 &
      COAH_GUB_2017_90$PC_Morena_2017 > COAH_GUB_2017_90$PC_PAN_2017 &
      COAH_GUB_2017_90$PC_Morena_2017 > COAH_GUB_2017_90$PC_Nulos_2017 &
      COAH_GUB_2017_90$PC_Morena_2017 > COAH_GUB_2017_90$PC_Otros_2017 &
      COAH_GUB_2017_90$PC_Morena_2017 > COAH_GUB_2017_90$PC_PRD_2017,
      "Morena", COAH_GUB_2017_90$partido_ganador)

COAH_GUB_2017_90$partido_ganador <- ifelse(
      COAH_GUB_2017_90$PC_PRI_2017 > COAH_GUB_2017_90$PC_Morena_2017 &
      COAH_GUB_2017_90$PC_PRI_2017 > COAH_GUB_2017_90$PC_PAN_2017 &
      COAH_GUB_2017_90$PC_PRI_2017 > COAH_GUB_2017_90$PC_Nulos_2017 &
      COAH_GUB_2017_90$PC_PRI_2017 > COAH_GUB_2017_90$PC_Otros_2017 &
      COAH_GUB_2017_90$PC_PRI_2017 > COAH_GUB_2017_90$PC_PRD_2017,
      "PRI", COAH_GUB_2017_90$partido_ganador)

COAH_GUB_2017_90$partido_ganador <- ifelse(
      COAH_GUB_2017_90$PC_PAN_2017 > COAH_GUB_2017_90$PC_PRI_2017 &
      COAH_GUB_2017_90$PC_PAN_2017 > COAH_GUB_2017_90$PC_Morena_2017 &
      COAH_GUB_2017_90$PC_PAN_2017 > COAH_GUB_2017_90$PC_Nulos_2017 &
      COAH_GUB_2017_90$PC_PAN_2017 > COAH_GUB_2017_90$PC_Otros_2017 &
      COAH_GUB_2017_90$PC_PAN_2017 > COAH_GUB_2017_90$PC_PRD_2017,
      "PAN", COAH_GUB_2017_90$partido_ganador)

COAH_GUB_2017_90$partido_ganador <- ifelse(
      COAH_GUB_2017_90$PC_PRD_2017 > COAH_GUB_2017_90$PC_PRI_2017 &
      COAH_GUB_2017_90$PC_PRD_2017 > COAH_GUB_2017_90$PC_PAN_2017 &
      COAH_GUB_2017_90$PC_PRD_2017 > COAH_GUB_2017_90$PC_Nulos_2017 &
      COAH_GUB_2017_90$PC_PRD_2017 > COAH_GUB_2017_90$PC_Otros_2017 &
      COAH_GUB_2017_90$PC_PRD_2017 > COAH_GUB_2017_90$PC_Morena_2017,
      "PRD", COAH_GUB_2017_90$partido_ganador)

COAH_GUB_2017_90$partido_ganador <- ifelse(
      COAH_GUB_2017_90$PC_Nulos_2017 > COAH_GUB_2017_90$PC_PRI_2017 &
      COAH_GUB_2017_90$PC_Nulos_2017 > COAH_GUB_2017_90$PC_PAN_2017 &
      COAH_GUB_2017_90$PC_Nulos_2017 > COAH_GUB_2017_90$PC_Morena_2017 &
      COAH_GUB_2017_90$PC_Nulos_2017 > COAH_GUB_2017_90$PC_Otros_2017 &
      COAH_GUB_2017_90$PC_Nulos_2017 > COAH_GUB_2017_90$PC_PRD_2017,
      "Nulos", COAH_GUB_2017_90$partido_ganador)

COAH_GUB_2017_90$partido_ganador <- ifelse(
      COAH_GUB_2017_90$PC_Otros_2017 > COAH_GUB_2017_90$PC_PRI_2017 &
      COAH_GUB_2017_90$PC_Otros_2017 > COAH_GUB_2017_90$PC_PAN_2017 &
      COAH_GUB_2017_90$PC_Otros_2017 > COAH_GUB_2017_90$PC_Nulos_2017 &
      COAH_GUB_2017_90$PC_Otros_2017 > COAH_GUB_2017_90$PC_Morena_2017 &
      COAH_GUB_2017_90$PC_Otros_2017 > COAH_GUB_2017_90$PC_PRD_2017,
      "Otros", COAH_GUB_2017_90$partido_ganador)

# Verifico que no se crearon missing values en el partido ganador

has_missing <- any(is.na(COAH_GUB_2017_90$partido_ganador))
print(has_missing)

# Generar tabla de frecuencia con cruce entre las dos variables
tabla_frecuencia <- table(COAH_GUB_2017_90$municipio, COAH_GUB_2017_90$partido_ganador)
#tabla_frecuencia <- table(COAH_GUB_2017_90$partido_ganador)

# Imprimir la tabla de frecuencia
print(tabla_frecuencia)

# Especifica la ruta y el nombre del archivo Excel de salida
ruta_salida <- "/Users/jonathan/Dropbox/Tareas_Jonathan/2023.06.12 Elecciones Federales 2021/1. Bases/COAHUILA_2017.xls"

# Crea un nuevo archivo de Excel
wb <- createWorkbook()

# Agrega la tabla al archivo de Excel
addWorksheet(wb, "tabla_frecuencia")
writeData(wb, sheet = 1, x = COAH_GUB_2017, startRow = 1, startCol = 1)

# Guarda el archivo de Excel
saveWorkbook(wb, ruta_salida, overwrite = TRUE)