rm(list = ls())
library(rio)
library(dplyr)
library(openxlsx)
library(tidyverse)

anion <- 2022

#Se carga la base de datos de datos de tasas de no respuesta histórica
tnr_old <- readRDS(paste0("intermedios/01_tamanio/", anion -1 , "/tnr_enesem_historica.rds"))

#####################################Cobertura enesem anio anterior#############
nombre_enesem <- list.files(paste0("insumos/03_cobertura/", anion - 1), pattern = ".sav")

cobertura <- import(paste0("insumos/03_cobertura/", anion - 1, "/", nombre_enesem)) %>% 
  as.data.frame() %>% 
  rename(id_empresa=inec_identificador_empresa)

################################Muestra enesem anio anterior####################
nombre_muestra <- list.files(paste0("productos/02_seleccion/", anion - 1), pattern = ".xlsx")

ma <- read.xlsx(paste0("productos/02_seleccion/", anion - 1, "/", nombre_muestra)) %>%
  mutate(dominio = paste0(tamanou_plazas, codigo_seccion)) %>% 
  group_by(dominio) %>% 
  summarise(nm = n())

sum(ma$nm) == dim(cobertura)[1]

#############################Abrimos el archivo de factores de expansión########
nombre_factores <- list.files(paste0("productos/04_factores/", anion - 1), pattern = ".xlsx")

ef <- read.xlsx(paste0("productos/04_factores/", anion - 1, "/", nombre_factores)) %>% 
  select(id_empresa, dominio = dominio_muestra) %>% 
  group_by(dominio) %>% 
  #total de elegibilidad
  summarise(Ielig = n()) %>% 
  mutate(dominio = as.character(dominio))

######Proceso##########
#base de cobertura 2018
cb = cobertura %>%
  #para la formacion del dominio necesito la letra
  #se substrae el primer dígito
  mutate(letra = substr(ciiu4_actividad_principal,1,1),
         #se une el tamanou y la letra se forma el dominio
         dominio = paste0(tamanio_empresa, letra),
         #se desgloza cada categría de la efectividad
         no_efectividad = ifelse(novedad_no_efectiva == "2.1" , "no_ubicadas",
                                 ifelse(novedad_no_efectiva == "2.2" , "rechazo",
                                        ifelse(novedad_no_efectiva == "2.3" , "liquidadas",
                                               #SCI: sin caracteristicas de ingreso
                                               ifelse(novedad_no_efectiva == "2.4" , "sci",
                                                      #SCSNI: sin caracteristicas sector no investigado
                                                      ifelse(novedad_no_efectiva == "2.5" , "scsni",
                                                             #SCRNI: sin caracteristicas rama no investigado
                                                             ifelse(novedad_no_efectiva == "2.6" , "scrni",
                                                                    #SCSC: sin caracteristicas sin contabilidad
                                                                    ifelse(novedad_no_efectiva == "2.7" , "scsc",
                                                                           ifelse(novedad_no_efectiva == "2.8" ,"inactiva" , "efectiva")))))))))


##Generación de OOS
#Estos establecimientos por su propia naturaleza no pueden ser considerados parte de la muestra. 
#Estos incluyen: duplicados en la lista de muestra, establecimientos que están fuera de los negocios, 
#establecimientos que se han fusionado en otro establecimiento y ya no existen por sí mismos, 
#y otros establecimientos que han cambiado de naturaleza de modo que ya no son apropiados 
#para el inspector instrumento.
base  <-  cb %>%
  group_by(dominio) %>% 
  mutate(oos_1 = ifelse(no_efectividad == "liquidadas" | no_efectividad == "sci" | no_efectividad == "scrni" | 
                          no_efectividad == "scsni" | no_efectividad == "scsc",1,0),
         O_1 = ifelse(no_efectividad == "inactiva", 1, 0),
         I_1 = ifelse(no_efectividad == "efectiva", 1, 0),
         R_1 = ifelse(no_efectividad == "rechazo", 1, 0),
         NC_1 = ifelse(no_efectividad == "no_ubicadas", 1, 0))


###TASAS
tasas = base %>% 
  group_by(dominio) %>%
  #mc = n de cobertura (4.093)
  summarise(mc = n(),
            oos = sum(oos_1),
            O = sum(O_1),
            I = sum(I_1),
            R = sum(R_1),
            NC = sum(NC_1)) %>%
  #variable para controlar
  # mutate(control = (oos + O + I + R + NC) == mc) %>%
  #Fuera de la frecuencia de muestreo (OOS) = OOS/(OOS+I+P+R+NC+O)
  mutate(OOS = round(oos/(oos+I+R+NC+O) * 100, 2),
         #tasa de respuesta (RR) = I/(I+P+R+NC+O)
         RR = round(I/(I+R+NC+O) * 100, 2),
         #tasa de rechazo (REF) = R/(I+P+R)
         REF = round(R/(I+R) * 100, 2)) %>% 
  left_join(ef, by = "dominio") %>% 
  #tasa de elegibilidad
  mutate(ELR = round(Ielig / I * 100, 2)) %>% 
  left_join(ma, by = "dominio") %>% 
  #cobertura final sobre el n muestral (4.088)
  mutate(CF = round(Ielig / nm * 100, 2)) %>% 
  select(dominio, m_campo = mc, m_orig = nm, oos, O, I, Ielig, R, NC, OOS, RR, REF, ELR, CF)


#Se guarda la información de tnr de el año 2019
saveRDS(tasas,paste0("intermedios/01_tamanio/", anion, "/tnr_enesem.rds"))


###filtro sin empresas grandes
tasas_sin_grande  <-  tasas %>% 
  filter(substr(dominio, 1, 1) != "5") %>% 
  mutate(letra = substr(dominio, 2, 2)) %>% 
  select(dominio,OOS21=OOS,RR21=RR,REF21=REF,ELR21=ELR,CF21=CF)

tnr_new <- tnr_old %>% 
  left_join(tasas_sin_grande,by="dominio") %>% 
  #Calulamos la tnr mínima y promedio de los últimos tres años
  mutate(tnr_max= 100 - pmin(CF21, CF20, CF19),
         tnr_pro= 100 - (CF21 + CF20 + CF19)/3,
         tnr_min= 100 - pmax(CF21, CF20, CF19)) %>% 
  select(dominio, ends_with("16"), ends_with("17"), ends_with("18"), ends_with("19"),
         ends_with("20"), ends_with("21"), tnr_max, tnr_pro, tnr_min)

plot(tnr_old$tnr_max, tnr_new$tnr_max)
abline(a = 0, b=1)

plot(tnr_old$tnr_pro, tnr_new$tnr_pro)
abline(a = 0, b=1)

tnr_new %>% 
  ggplot() +
  geom_line(aes(y = CF21, x = c(1:34), color = "red")) +
  geom_line(aes(y = CF20, x = c(1:34), color = "blue")) +
  geom_line(aes(y = CF19, x = c(1:34), color = "greem"))

#Se guarda la tasa de no respuesta para la estimación del tamaño muestral
saveRDS(tnr_new,paste0("intermedios/01_tamanio/", anion, "/tnr_enesem_historica.rds"))

write.xlsx(tnr_new,paste0("intermedios/01_tamanio/", anion, "/tnr_enesem_historica.xlsx"))
