install.packages("xlsx")
library(xlsx)
library(readxl)
library(tidyverse)

# DIRECTORIO --------------------------------------------------------------
anion <- 2022

carpeta_directorio <- last(list.dirs(paste0("insumos/01_tamanio/", anion, "/nogit/"), recursive = F, full.names = F))
nombre_directorio <- list.files(paste0("insumos/01_tamanio/", anion, "/nogit/", carpeta_directorio),pattern = ".rds")
directorio <- readRDS(paste0("insumos/01_tamanio/", anion, "/nogit/", carpeta_directorio, "/", nombre_directorio))

directorio_2021 <- directorio %>% filter(anio==2021)
#write.xlsx(directorio_2021, "DIRECTORIO_EPMPRESAS_2021.xlsx")


# DIRECTORIO 2021 SIN PEQUEÑAS ---------------------------------------------------------
directorio_2021_sin_micro <- directorio_2021 %>% filter(tamanou_plazas!=1)
#write.xlsx(directorio_2021_sin_peq, "DIRECTORIO SIN PEQ 2021.xlsx")
saveRDS(object = directorio_2021_sin_micro, file = "direcorio20221_sin_micro.rds")

names(directorio_2021_sin_micro)

write.xlsx(cobertura_enesem,"COBERTURA ENESEM.xlsx")



