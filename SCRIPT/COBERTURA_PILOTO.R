rm(list = ls())

library(tidyverse)
library(readxl)
library(reshape2)
library(janitor)
library(dplyr)
library(readxl)
library(xlsx)
library(openxlsx)
library(readxl)
library(janitor)
library(ggplot2)

#--------------------------------------------------------------------------
# LECTURA DE BASES DE DATOS -----------------------------------------------
#--------------------------------------------------------------------------

marco_2021_canasta <- read_excel("PRODUCTOS/MARCO/marco_IPP.xlsx")

data_muestra_f <- read_excel("DATA/PILOTO COBERTURA/Muestra_Empresas_CAB-SIPP_v1.xlsx", 
                                           sheet = "Consolidado Forzosa") %>% clean_names()
data_muestra_f <- data_muestra_f %>% left_join( select(marco_2021_canasta,id_empresa,dom_m),
                              by="id_empresa")

data_muestra_m_p <- read_excel("DATA/PILOTO COBERTURA/Muestra_Empresas_CAB-SIPP_v1.xlsx", 
                               sheet = "Consolidado Pequeñas Medianas") %>% 
  clean_names()

#--------------------------------------------------------------------------
# GRANDES -----------------------------------------------------------------
#--------------------------------------------------------------------------

DB1 <- data_muestra_f %>% select(id_empresa,dom_m,codigo_actividad_eco,
                                 paste0("codigo_producto_",c(1:5))) %>% 
  pivot_longer(
    cols = c(paste0("codigo_producto_",c(1:5))),
    names_to = "Producto",
    values_to = "Codigo")

#--------------------------------------------------------------------------
# MEDIANAS PEQUEÑAS -------------------------------------------------------
#--------------------------------------------------------------------------

DB2 <- data_muestra_m_p %>% select(id_empresa,dom_m,codigo_actividad_eco,
                                 paste0("codigo_producto_",c(1:5))) %>% 
  pivot_longer(
    cols = c(paste0("codigo_producto_",c(1:5))),
    names_to = "Producto",
    values_to = "Codigo") %>% 
  mutate(Codigo = gsub("\\.","",as.character(Codigo)))

#--------------------------------------------------------------------------
# UNIENDO BASES -----------------------------------------------------------
#--------------------------------------------------------------------------

DB <- rbind(DB1,DB2)

#--------------------------------------------------------------------------
# RESUMEN - LO LEVANTADO POR PRODUCTO -------------------------------------
#--------------------------------------------------------------------------

aux <- DB %>% filter(!is.na(Codigo)) %>% 
  group_by(Codigo) %>% summarise(Total = n()) 

table(aux) %>% View()
#--------------------------------------------------------------------------
# GRAFICO POR DOMINIO  ----------------------------------------------------
#--------------------------------------------------------------------------

aux %>% filter(dom_m != "5C")%>% 
  ggplot(aes(x=dom_m,y=Total,fill = dom_m)) + 
  geom_bar(stat="identity")+
  theme(legend.position = "bottom")

#--------------------------------------------------------------------------
# GRAFICO POR PRODUCTO  ---------------------------------------------------
#--------------------------------------------------------------------------

DB %>% filter(!is.na(Codigo)) %>% 
  group_by(Codigo) %>% summarise(Total=n()) %>%
  filter(Total >= 5 ) %>% 
  ggplot(aes(x=Codigo,y=Total,fill = Codigo)) + 
  geom_bar(stat="identity") + 
  theme(legend.position = "bottom")

#--------------------------------------------------------------------------
# RESUMEN - LO LEVANTADO POR DOM  -----------------------------------------
#--------------------------------------------------------------------------

duplicated(DB$id_empresa)
aux_2 <- DB[!duplicated(DB$id_empresa),]

# aux_2 %>% filter()
# group_by(dom_m) %>% summarise(n()) %>% adorn_totals() %>% View()

cobertura_dom <- aux_2 %>% 
  filter(!is.na(Codigo)) %>%
  group_by(dom_m) %>% 
  summarise(total_levantado = n()) 

cobertura_dom %>% adorn_totals() %>% View()

#--------------------------------------------------------------------------
# MUESTRA ENVIADA  --------------------------------------------------------
#--------------------------------------------------------------------------

m1 <- marco_2021_canasta %>% filter(tamanou_plazas=="5") %>% 
  group_by(dom_m) %>% summarise(n_muestra = n())


muestra_enviar <- read_excel("Muestra_enviar.xlsx")

m2 <- muestra_enviar %>% group_by(dom_m) %>% 
  summarise(n_muestra = n())

m <- rbind(m1,m2) 

tabla_levantado <- cobertura_dom %>% right_join(m,by="dom_m") %>% 
  mutate(porcentaje_levantado = round(100*total_levantado/n_muestra,2))

tabla_levantado %>% #filter(grepl(dom_m,pattern = "2")) %>% 
  ggplot(aes(x=dom_m,y=porcentaje_levantado,fill = dom_m)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "")  

tabla_levantado %>% adorn_totals() %>% View()

export(tabla_levantado,"tabla.cobertura.xlsx")

#--------------------------------------------------------------------------
# MUESTRA ENVIADA  --------------------------------------------------------
#--------------------------------------------------------------------------

obs <- DB %>% filter(!is.na(Codigo)) %>% 
  group_by(id_empresa) %>% 
  summarise(n_productos = n_distinct(Producto),
            n_codigo = n_distinct(Codigo)) %>% 
  mutate (Obs = if_else(n_productos==n_codigo,1,0)) %>% 
  filter(Obs==0) 

library(rio)
export(obs,"obs.xlsx")




















