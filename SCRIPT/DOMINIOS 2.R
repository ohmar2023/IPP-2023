rm(list=ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)
library(reshape2)

#-------------------------------------------------------------------------------
# CARGAMOS EL MARCOIPP
marco_2021_canasta <- read_excel("PRODUCTOS/MARCO/marco_IPP.xlsx")
#-------------------------------------------------------------------------------

# CARGAMOS EL DIRECTORIO 2020 ---------------------------------------------

# carpeta_old <- last(list.dirs(paste0("insumos/01_tamanio/",anion - 1,"/nogit/"), recursive = F, full.names = F))
# nombre_diee_old <- list.files(paste0("insumos/01_tamanio/",anion - 1,"/nogit/", carpeta_old), pattern = ".rds")
# directorio_2020 <- readRDS(paste0("insumos/01_tamanio/",anion - 1,"/nogit/", carpeta_old, "/", nombre_diee_old)) %>%filter(anio==2020)
# directorio_2020 <- directorio_2020 %>%  mutate(id_empresa=as.character(id_empresa))

#-------------------------------------------------------------------------------
# INCLUSION FORZOSA
#-------------------------------------------------------------------------------
# inc_for <- marco_2021_canasta %>% 
#   mutate(inclusion_forzosa=ifelse(tamanou_plazas==5,1,0)) %>% 
#   filter(inclusion_forzosa==1)
# 
# #-------------------------------------------------------------------------------
# # NO INCLUSION FORZOSA
# #-------------------------------------------------------------------------------
# marco_sin_inc_for <- marco_2021_canasta %>% 
#   mutate(inclusion_forzosa=ifelse(tamanou_plazas==5,1,0)) %>% 
#   filter(inclusion_forzosa==0)

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------
nc=0.9
z=qnorm(nc+(1-nc)/2)
er=0.15

# CALCULO -----------------------------------------------------------------

tamanio_90_10 <- marco_2021_canasta %>% 
  mutate(dominio=codigo_seccion,
         ventas_totales=as.numeric(ventas_totales)) %>% 
  group_by(dominio) %>% 
  summarise(N=n(),
            desv=sd(ventas_totales,na.rm = T),
            ventas=sum(ventas_totales,na.rm = T)) %>% 
  mutate(numerador=(N*desv)^2,
         denominador=((N-1)/N)*((er*ventas/z)^2)+N*(desv^2),
         tam=numerador/denominador) %>% 
  left_join(select(tnr,dominio,tnr_max,tnr_pro, tnr_min),by="dominio") %>% 
  mutate(tnr_max=ifelse(is.na(tnr_max),0,tnr_max/100),
         tnr_pro=ifelse(is.na(tnr_pro),0,tnr_pro/100),
         tnr_min=ifelse(is.na(tnr_min),0,tnr_min/100),
         n1=ceiling(tam/(1-tnr_max)),
         n2=ifelse(n1>N,N,n1),
         n3=ceiling(tam/(1-tnr_pro)),
         n4=ifelse(n3>N,N,n3),
         n5=ceiling(tam/(1-tnr_min)),
         n6=ifelse(n5>N,N,n5))


