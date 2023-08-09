library(readxl)
library(reshape2)
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyverse)

  dir_2021_canasta %>% mutate (aux=if_else(is.na(ventas_totales),0,1)) %>% 
  group_by(tamanou_plazas,codigo_seccion,aux)%>% summarise(Total=n()) %>% 
  dcast(tamanou_plazas+codigo_seccion~aux,value.var = "Total") %>%
  adorn_totals(c("col","row")) %>% 
  mutate("%_NA" = round((`0`/ Total)*100,2),
             "%_Valor" = round((`1`/ Total)*100,2)) %>% View()

  dir_2021_canasta %>% mutate (aux=if_else(is.na(ventas_totales),0,1)) %>% 
    group_by(tamanou_plazas,aux)%>% summarise(Total=n()) %>% 
    dcast(tamanou_plazas~aux,value.var = "Total") %>%
    adorn_totals(c("col","row")) %>% 
    mutate("%_NA" = round((`0`/ Total)*100,2),
           "%_Valor" = round((`1`/ Total)*100,2)) %>% View()
  
  # DIRECTORIO 2009 - 2021 ---- ------------------------------------------------
  
  anion <- 2022
  carpeta <- last(list.dirs(paste0("insumos/01_tamanio/",anion,"/nogit/"), recursive = F, full.names = F))
  emp <- readRDS(paste0("insumos/01_tamanio/",anion,"/nogit/", carpeta, "/", nombre_diee)) %>% 
    mutate(id_empresa = as.character(id_empresa))
  
  
  # DIRECTORIO 2009 - 2020 --------------------------------------------------
  
  carpeta_old <- last(list.dirs(paste0("insumos/01_tamanio/",anion - 1,"/nogit/"), recursive = F, full.names = F))
  nombre_diee_old <- list.files(paste0("insumos/01_tamanio/",anion - 1,"/nogit/", carpeta_old), pattern = ".rds")
  dir_old <- readRDS(paste0("insumos/01_tamanio/",anion - 1,"/nogit/", carpeta_old, "/", nombre_diee_old))
  
  
  # MARCO EMPRESAS - 2020 ---------------------------------------------------
  
  marco <- readRDS(paste0("intermedios/01_tamanio/",anion - 1, "/marco", ".rds")) %>% 
    mutate(marcov=1) %>% 
    select(id_empresa,marcov) #2 VARIABLES
  
  
  # BD DE COBERTURA DEL 2020 ------------------------------------------------
  
  nombre_enesem <- list.files(paste0("insumos/03_cobertura/", anion - 1), pattern = ".sav")
  enesem <- import(paste0("insumos/03_cobertura/", anion - 1, "/", nombre_enesem)) %>% 
    mutate(id_empresa = inec_identificador_empresa)
  
  
  # DIRECTORIO 2020 ---------------------------------------------------------
  
  dir_old_01 <- dir_old %>% 
    filter(anio == anion - 2) %>% #DIEE-2020
    mutate(id_empresa = as.character(id_empresa)) %>% 
    select(id_empresa, tam_old = tamanou_plazas, for_old = forma_institucional)
  
  
  # DIEE 2021 & MARCO 2020 & DIEE 2020 --------------------------------------
  
  dir <- emp %>% 
    filter(anio == anion - 1 ) %>% #DIEE-2021
    mutate(id_empresa = as.character(id_empresa),
           empleo_ult = as.numeric(plazas_ult)) %>% 
    left_join(marco, by="id_empresa") %>% #MARCO DEL 2020 (2 VARIABLES)
    left_join(dir_old_01, by="id_empresa") #DIEE-2020 (3 VARIABLES)
  
  
  tabla1 <- dir %>% #DIEE 2021 & MARCO 2020 & DIEE 2020
    mutate(ventas_totales = as.numeric(ventas_totales)) %>% 
    group_by(tamanou_plazas) %>% 
    summarise(venna = sum(is.na(ventas_totales)),
              ven00 = sum(ventas_totales==0,na.rm = T),
              empna = sum(is.na(empleo_ult))) %>% 
    filter(tamanou_plazas!=1)
  
  apply(tabla1[,-1], 2, sum)
  tabla1 %>% View()
  
  # TABLA 20 RESUMEN   ------------------------------------------------------
  
  tabla20 <- dir %>% 
    mutate(var201=ifelse(forma_institucional %in% c(1,2,3,4,5) & codigo_seccion=="O",1,0),	#Administración pública y defensa, seguridad social
           var202=ifelse(tamanou_plazas %in% c(4,5) & forma_institucional==2,1,0),
           var203=ifelse(tamanou_plazas %in% c(4,5) & forma_institucional==3,1,0),
           var204=ifelse(empleo_ult > 100 & tamanou_plazas < 4, 1, 0),
           var205=ifelse(tamanou_plazas==4 & is.na(empleo_ult), 1, 0),
           var206=ifelse(marcov!=1 & tamanou_plazas>3,1,0),
           var207=ifelse(is.na(tam_old) & tamanou_plazas>3,1,0),
           var208=ifelse(tam_old < 3 & tamanou_plazas>3,1,0),
           var209=ifelse(tam_old > 3 & tamanou_plazas<3,1,0),
           var210=ifelse(for_old == 7 & forma_institucional!=7,1,0),
           var211=ifelse(for_old != 7 & forma_institucional==7,1,0))
  
  # TABLA 21 002 ------------------------------------------------------------
  
  
  tabla21 <- enesem %>% 
    left_join(select(dir, id_empresa, tam_new = tamanou_plazas, sec_new = codigo_seccion),
              by = "id_empresa") %>% 
    mutate(var212 = ifelse(id_empresa %in% dir$id_empresa, 0, 1),
           var213 = ifelse(tamanio_empresa != tam_new, 1, 0),
           var214 = ifelse(sec_new != substr(ciiu4_actividad_principal,1,1), 1, 0))
  
  # TABLA 22 ----------------------------------------------------------------
  
  tabla22 <- dir %>% 
    mutate(var215 = ifelse(tamanou_plazas %in% c(4,5) & is.na(tam_old),1,0))
  

  # TABLA 23 ----------------------------------------------------------------

  tabla23 <- emp %>% 
    filter(anio == anion-2) %>% #DIEE 2020
    mutate(id_empresa = as.character(id_empresa)) %>% 
    left_join(dir_old_01,by="id_empresa") %>% 
    mutate(var216 = ifelse(tamanou_plazas != as.integer(tam_old),1,0))
    

  # TABLA 24 ----------------------------------------------------------------

  tabla24 <- dir %>% 
    mutate(var217 = ifelse(tamanou_plazas %in% c(3,4,5) & codigo_seccion %in% c("D","E"),1,0))

  table(tabla24$var217,useNA = "ifany")
  
  # RESULTADOS --------------------------------------------------------------
  
  res1 <- tabla20 %>% 
    group_by() %>% 
    summarise(var201=sum(var201,na.rm = T),
              var202=sum(var202,na.rm = T),
              var203=sum(var203,na.rm = T),
              var204=sum(var204,na.rm = T),
              var205=sum(var205,na.rm = T),
              var206=sum(var206,na.rm = T),
              var207=sum(var207,na.rm = T),
              var208=sum(var208,na.rm = T),
              var209=sum(var209,na.rm = T),
              var210=sum(var210,na.rm = T),
              var211=sum(var211,na.rm = T))
  
  res2 <- tabla21 %>% 
    group_by() %>% 
    summarise(var212=sum(var212,na.rm = T),
              var213=sum(var213,na.rm = T),
              var214=sum(var214,na.rm = T))
  
  res3 <- tabla22 %>% 
    group_by() %>% 
    summarise(var215=sum(var215,na.rm = T))
  
  res4 <- tabla23 %>% 
    group_by() %>% 
    summarise(var216=sum(var216,na.rm = T))
  
  res5 <- tabla24 %>% 
    group_by() %>% 
    summarise(var217=sum(var217,na.rm = T))
  
  res1
  res2
  res3
  res4
  res5
  
  
  # PREGUNTAS ---------------------------------------------------------------
  
  # 1: ¿PORQUE EL DIR_OLD (2009-2020) Y EMP (2009-2021) NO SON CONSISTENTES CUANDO SE FILTRA 2020 EN EMP
  # 2: ¿dE DONDE SALIÓ EL MARCO 2020  
  # 3: empleo_ult, ¿Que es? ¿Porque tiene decimales?
