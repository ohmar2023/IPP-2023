rm(list = ls())

library(tidyverse)
library(readxl)
library(reshape2)
library(janitor)
library(dplyr)
library(readxl)
library(xlsx)
library(openxlsx)

#-------------------------------------------------------------------------------
# LECTURA DEL DIRECTORIO 2009-2021: 20230321: 10306119 número de casos en el 
# directorio
#-------------------------------------------------------------------------------
directorio <- readRDS("DATA/DIRECTORIO/2021/diee_20230315.rds") %>% 
              filter(anio==2021)

#-------------------------------------------------------------------------------
# Codigos productos y actividad princiapal canasta
#-------------------------------------------------------------------------------
canasta <- read_excel("DATA/CANASTA/CANASTA_LISTADO.xlsx", sheet = "ACTUALIZADO")

#-------------------------------------------------------------------------------
# CREANDO MARCO IPP CON EL DIEE 2021
#-------------------------------------------------------------------------------

marco_2021_canasta <- directorio %>% 
                      filter(codigo_actividad_eco %in% canasta$COD_PRODUCTO,
                                          tamanou_plazas!=1) %>% 
  filter(situacion == 1) %>%
  #filtro las empresas no ubicadas
  filter(is.na(empresas_noubicadas)) %>%
  mutate(filtro = ifelse((tamanou_plazas == 4 | tamanou_plazas == 5 ) & 
                           forma_institucional == 2, 1, 0)) %>%
  mutate(dom_m = paste0(tamanou_plazas, codigo_seccion),
         id_empresa = as.character(id_empresa))

#-------------------------------------------------------------------------------
# EXPORTANDO MARCO CANASTA 2021 
#-------------------------------------------------------------------------------
write.xlsx(marco_2021_canasta,"PRODUCTOS/MARCO/marco_IPP.xlsx",overwrite = T)

