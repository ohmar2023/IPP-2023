rm(list = ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(reshape2)
library(janitor)

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------

tamanio %>% filter(!dominio %in% c("2C","3C","4C")) %>% adorn_totals() %>% View()

tamanio <- read_excel("PRODUCTOS/TAMANIO/005/Tam_Sin_Inc_For.xlsx") 
tamanio[is.na(tamanio)] <- 0

#-------------------------------------------------------------------------------
# Marco sin dominios de inclusion forozosa
#-------------------------------------------------------------------------------
marco_2021_canasta <- read_excel("PRODUCTOS/MARCO/marco_IPP.xlsx")

marco_sin_inc_for <- marco_2021_canasta %>% 
  mutate(inclusion_forzosa=ifelse(tamanou_plazas==5,1,0)) %>% 
  filter(inclusion_forzosa==0)


# ------------------------------------------------------------------------------
# PPT MINIMO 5 -----------------------------------------------------------------
# En caso de que no hayan 5 en el estrato, se toman todos y el resto del tamaño 
# muetsral se lo hace PPT entre los estratos que tengan empresas disponibles. 
# ------------------------------------------------------------------------------

marco_aux <- marco_sin_inc_for %>% 
  filter(!dom_m %in% c("2C","3C","4C")) %>% 
  group_by(dom_m) %>% 
  summarise(N=n(),H = n_distinct(codigo_actividad_eco)) 

aux <- marco_sin_inc_for %>% 
  filter(!dom_m %in% c("2C","3C","4C")) %>% 
  group_by(dom_m,codigo_actividad_eco) %>% 
  summarise(Nh=n()) %>% 
  left_join(select(tamanio,dom_m=dominio,n4),by="dom_m") %>% 
  left_join(marco_aux,by="dom_m") 

seleccion <- aux %>% mutate(a = if_else(Nh<5,Nh,5),
                              b = Nh-a)
aux_2 <-  seleccion %>%  
  group_by(dom_m) %>% 
  summarise(Nh_b=sum(b),
            n_b = unique(n4)-sum(a)) %>% 
  right_join(seleccion,by="dom_m") %>% 
  mutate(n_b = if_else(n_b<0,0,n_b),
         PPT_b =if_else(Nh_b==0,0,ceiling((n_b)*(b/Nh_b))),
         PPT_b = PPT_b+a,
         Control = Nh - PPT_b)

seleccion <- aux_2                      

# ------------------------------------------------------------------------------
# MUESTRA SIN INCLUSIÓN FOR Y SIN DOMINIOS C -----------------------------------
# ------------------------------------------------------------------------------

muestra_sin_c <- marco_sin_inc_for %>% filter(codigo_seccion!="C")  %>% 
  mutate(dom_m = paste0(tamanou_plazas,codigo_seccion)) %>%
  left_join(select(seleccion,dom_m,codigo_actividad_eco,PPT_b),
            by=c("dom_m","codigo_actividad_eco")) %>% 
  group_by(dom_m,codigo_actividad_eco) %>% 
  sample_n(PPT_b) %>% 
  select(id_empresa,dom_m,codigo_actividad_eco)

# control de muestra: en diff no deben aparecer negativos

muestra_sin_c %>% group_by(dom_m) %>% summarise(n_muestra=n()) %>%
  left_join(select(tamanio,n4,dom_m = dominio),by="dom_m") %>% 
  mutate(dif = n_muestra-n4) %>% adorn_totals() %>% View()

# ------------------------------------------------------------------------------
# Seleccion C-MANUFACTURA: Se hace una seleccion aleatoria  --------------------
# ------------------------------------------------------------------------------

muestra_c <- marco_sin_inc_for %>% filter(codigo_seccion=="C") %>% 
  mutate(dom_m = paste0(tamanou_plazas,codigo_seccion)) %>%
  left_join(select(tamanio,dom_m=dominio,n4),
            by=c("dom_m")) %>% 
  group_by(dom_m) %>% 
  sample_n(n4) %>% 
  select(id_empresa,dom_m,codigo_actividad_eco)

# control de muestra: en diff no deben aparecer negativos

muestra_c %>% group_by(dom_m) %>% summarise(n_muestra=n()) %>%
  left_join(select(tamanio,n4,dom_m = dominio),by="dom_m") %>% 
  mutate(dif = n_muestra-n4) %>% adorn_totals() %>% View()

# ------------------------------------------------------------------------------
# MUESTRA FINAL SIN INCLUSION FORZOSA ------------------------------------------
# ------------------------------------------------------------------------------

muestra <- rbind(muestra_sin_c,muestra_c) %>% arrange(dom_m)
muestra <- as.data.frame(muestra)
#control de la muestra final
muestra %>% group_by(dom_m) %>% summarise(n_f = n()) %>% 
  left_join(select(tamanio,dom_m=dominio,n4),by="dom_m") %>% 
  mutate(dif = n4-n_f) %>% View()

# ------------------------------------------------------------------------------
# CREANDO ARCHIVO PARA EXPORTAR ------------------------------------------------
# ------------------------------------------------------------------------------

muestra_enviar <- muestra %>% select(id_empresa) %>% 
  left_join(marco_sin_inc_for,by="id_empresa") %>% 
  select(id_empresa,ruc_principal,razon_social,nombre_comercial,
         codigo_actividad_eco,codigo_provincia,codigo_canton,
         codigo_parroquia,forma_institucional,calle_principal,
         numero,interseccion,kilometro,urbanizacion,nombre_edificio,
         numero_piso,numero_oficina,ciudadela,barrio,manzana,referencia,
         telefono,nombre_contacto,punto_x,punto_y,zona_censal,sector_censal,
         manzana_censal,tamanou_plazas,dom_m)            

#control a nivel de dominio: La max diferencia es de 12, esta ok.
muestra_enviar %>% group_by(dom_m) %>% summarise(n_f=n()) %>%
  left_join(select(tamanio,dom_m=dominio,n4),by="dom_m") %>% 
  mutate(dif = n4-n_f) %>% adorn_totals() %>% View()

#control a nivel de estrato: No deben existir negativos
muestra_enviar %>% group_by(dom_m,codigo_actividad_eco) %>% 
  summarise(n_f=n()) %>%
  left_join(select(aux,dom_m,codigo_actividad_eco,Nh),
            by=c("dom_m","codigo_actividad_eco")) %>% 
  mutate(dif = Nh-n_f) %>% View()

# exportando --------------------------------------------------------
write.xlsx(muestra_enviar,"Muestra_enviar.xlsx")



