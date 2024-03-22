rm(list=ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)
library(reshape2)

#-------------------------------------------------------------------------------
# CARGAMOS 
#-------------------------------------------------------------------------------

base_productos <- read_excel("DATA/TAMANIO PRODUCTO/TOP 5 PRODUCTOS SISTEMA CAB SIPP_21032024_V2.xlsx")%>% 
  clean_names() %>% 
  mutate(cpc = gsub("\\.","",as.character(cpc)),
         precio_actual = as.numeric(precio_actual))

base_productos_2 <- read_excel("DATA/TAMANIO PRODUCTO/REPORTE_PILOTO PRODUCTOS CON PRECIO MUESTRA_21032024_V2.XLSX", 
                               sheet = "TODAS EMPRESAS") %>%
  clean_names() %>% 
  mutate(cpc = gsub("\\.","",as.character(cpc)),
         precio_actual = as.numeric(precio_actual))

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc=0.9
z=qnorm(nc+(1-nc)/2)
er=0.2

#-------------------------------------------------------------------------------
# CALCULO ----------------------------------------------------------------------
#-------------------------------------------------------------------------------

tam <- base_productos_2 %>% 
  mutate(dominio = cpc,
         v_diseno = as.numeric(precio_actual)) %>% 
  group_by(dominio) %>% 
  summarise(N = n(),
            desv = sd(v_diseno,na.rm = T),
            v_diseno_mean = mean(v_diseno,na.rm = T)) %>% 
  mutate(numerador = (N*desv)^2,
         denominador = ((N-1)/N)*((er*v_diseno_mean/z)^2) + N*(desv^2),
         tam = numerador/denominador,
         n = tam/(1+(tam/N)),
         n = ceiling(n)) 

#-------------------------------------------------------------------------------
# EXPORTAR ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

write.xlsx(inc_for,"PRODUCTOS/ENTREGADOS/Inc_For_entrega.xlsx")

write.xlsx(tamanio_90_10,"Tam_Sin_Inc_For.xlsx")
write.xlsx(tamanio_final_90_10,"PRODUCTOS/TAMANIO/Tam_Total.xlsx")
write.xlsx(marco_sin_inc_for,"Marco_sin_inclusión_for.xlsx")




