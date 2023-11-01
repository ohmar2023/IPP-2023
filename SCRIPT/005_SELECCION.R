rm(list = ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)

anion <- 2022

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------

tamanio <- read_excel("PRODUCTOS/TAMANIO/005/Tam_Sin_Inc_For.xlsx") 
tamanio[is.na(tamanio)] <- 0
#tamanio %>% View()


# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------

marco_aux <- marco_sin_inc_for %>% 
  filter(!dom_m %in% c("2C","3C","4C")) %>% 
  group_by(dom_m) %>% 
  summarise(N=n(),H = n_distinct(codigo_actividad_eco)) 
  #right_join(marco_sin_inc_for,by="dom_m") 
  
aux <- marco_sin_inc_for %>% 
  filter(!dom_m %in% c("2C","3C","4C")) %>% 
  group_by(dom_m,codigo_actividad_eco) %>% 
  summarise(Nh=n()) %>% 
  left_join(select(tamanio,dom_m=dominio,n4),by="dom_m") %>% 
  left_join(marco_aux,by="dom_m") %>%  
   adorn_totals() 

# PPT 
aux %>% mutate(p = Nh/N,
               PPT = trunc(p*n4),
               PPT = if_else(PPT<1,1,PPT),
               ) %>% View("PPT")

# kish
aux %>% mutate(num = sqrt((1/H^2)*(Nh/N)^2)) %>% 
                 group_by(dom_m) %>% 
                 summarise(n4*num/sum(num)) %>% View("KISH")

# "Valanceado"
aux %>% mutate() %>% 
  group_by(dom_m) %>% 
  summarise(n4*num/sum(num)) %>% View()


# ------------------------------------------------------------------------------
# MUESTRA SIN INCLUSIÓN FOR ----------------------------------------------------
# ------------------------------------------------------------------------------
 
muestra <- read_excel("PRODUCTOS/TAMANIO/005/Marco_sin_inclusión_for.xlsx") %>% 
 mutate(dominio = paste0(tamanou_plazas,codigo_seccion)) %>% 
  # El esceneario ecogido es el número 2 por lo que el n es el n4 (PROMEDIO)
  left_join(select(tamanio,dominio,n=n4),by="dominio") %>% 
  group_by(dominio) %>% 
  sample_n(unique(n)) %>% 
  select(id_empresa,dominio,codigo_provincia) %>% 
  mutate(tipo="muestra") %>% 
  as.data.frame() %>% 
  ungroup() 

inc_for <-  read_excel("IPP_2021_REVISION_OMAR/PRODUCTOS/Inc_For.xlsx")%>% ##GRANDES INCLUSION FOR SE INVESTIGAS SI O SI, TAMBIEN EMPRESAS MEDIANAS B QUE FUERON GRANDES 
  mutate(dominio=paste0(tamanou_plazas,codigo_seccion)) %>% 
  select(id_empresa,dominio,codigo_provincia) %>% 
  mutate(tipo="inc_for") 

muestra_final <- muestra %>% 
  rbind(inc_for)

apoyo <- muestra_final %>% 
  group_by(dominio) %>% 
  summarise(seleccionado=n())

tamanio_final <- read_excel("IPP_2021_REVISION_OMAR/PRODUCTOS/Tam_Total.xlsx") %>% 
  left_join(apoyo,by="dominio") %>%
  #El control se lo realiza con n1 ya que se escogió el escenario 2 20230328
  mutate(control=abs(n_pro-seleccionado))

#En el control no deben existir diferencias entre lo seleccionado y lo planificado
sum(tamanio_final$control)

muestra_envio <- read_excel("IPP_2021_REVISION_OMAR/PRODUCTOS/marco_IPP.xlsx") %>% 
  filter(id_empresa %in% muestra_final$id_empresa) %>% 
  select(-filtro, -dom_m)

muestra_envio %>% mutate(dom_m=paste0(tamanou_plazas,codigo_seccion)) %>% 
  group_by(dom_m) %>% summarise(n()) %>% View()

write.xlsx(muestra_envio,"muestra_enviar.xlsx")
