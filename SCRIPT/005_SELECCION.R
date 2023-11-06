rm(list = ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(reshape2)
library(janitor)

anion <- 2022

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------

tamanio %>% filter(!dominio %in% c("2C","3C","4C")) %>% adorn_totals() %>% View()

tamanio <- read_excel("PRODUCTOS/TAMANIO/005/Tam_Sin_Inc_For.xlsx") 
tamanio[is.na(tamanio)] <- 0

# ------------------------------------------------------------------------------
# MARCO SIN MANUFACTURA --------------------------------------------------------
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

# ------------------------------------------------------------------------------
# PPT MINIMO 1 - AUMENTANDO TAMAÑO MUESTRAL-------------------------------------
# ------------------------------------------------------------------------------

seleccion_1 <- aux %>% mutate(p = Nh/N,
                              PPT_min_1 = trunc(p*n4),
                              PPT_min_1 = if_else(PPT_min_1<1,1,PPT_min_1)
)

# ------------------------------------------------------------------------------
# PPT MINIMO 5 - AUMENTANDO TAMAÑO MUESTRAL-------------------------------------
# ------------------------------------------------------------------------------

seleccion_2 <- aux %>% mutate(a = if_else(Nh<5,Nh,5),
                              b = Nh-a,
                              p = Nh/N,
                              PPT_min_5 = trunc(p*n4),
                              PPT_min_5 = if_else(PPT_min_5<5 & Nh>=5,5,
                                                  if_else(PPT_min_5<5 & Nh<5, Nh,
                                                          PPT_min_5))
)

seleccion_2 %>% View()
# ------------------------------------------------------------------------------
# SELECCION UNIF  --------------------------------------------------------------
# ------------------------------------------------------------------------------

db <- aux %>% filter(dom_m=="4I")
v <- db$Nh
n <- unique(db$n4)

distr <- function(v,n,db) 
{
  k <- NULL
  r <- rep(0,length(v))
  repeat {
    k <- if_else(length(v[v!=10000])==0,n,ceiling(n/length(v[v!=10000])))
    #k = ceiling(n/length(v[v!=10000]))
    #print(k)
    if(sum(v-k<0)==0)
    {
      r[v!=10000] <- k
      db <- cbind(db,"Unif_Mod" = r)
      return(db)
      break
    }
    else{
      r[v-k<=0] <- v[v-k<=0]
      n = n - abs(sum((v)[v-k<=0]))
      v[!v-k>0] <- 10000
    }
  }
}

# USANDO FUNCION PARA DISTRIBUCION

dom <- aux %>% select(dom_m)
dom <- unique(dom$dom_m) 
r <- NULL
for(i in c(1:length(dom)))
{
  print(i)
  db <- aux %>% filter(dom_m==dom[i])
  v <- (db$Nh)
  n <- db$n4[1]
  r1 <- distr(v,n,db)
  r <- rbind(r,r1)
}
seleccion_3 <- r

# ------------------------------------------------------------------------------
# UNIENDO TODOS LOS RESULTADOS -------------------------------------------------
# ------------------------------------------------------------------------------

tam_selec_final <- seleccion_1 %>% 
  left_join(select(seleccion_2,dom_m,codigo_actividad_eco,PPT_min_5),
            by=c("dom_m","codigo_actividad_eco")) %>% 
  left_join(select(seleccion_3,dom_m,codigo_actividad_eco,Unif_Mod),
            by=c("dom_m","codigo_actividad_eco"))

#CONTROL
tam_selec_final %>% mutate(diff_1 = Nh-Unif_Mod) %>% View()

# ---------------------------------------------------------------------------------

# # kish
# aux %>% mutate(num = sqrt((1/H^2)*(Nh/N)^2)) %>% 
#                  group_by(dom_m) %>% 
#                  summarise(n4*num/sum(num)) %>% View("KISH")

# ------------------------------------------------------------------------------
# MUESTRA SIN INCLUSIÓN FOR ----------------------------------------------------
# ------------------------------------------------------------------------------

muestra <- marco_sin_inc_for %>% filter(codigo_seccion!="C")  %>% 
  mutate(dom_m = paste0(tamanou_plazas,codigo_seccion)) %>%
  left_join(select(tam_selec_final,dom_m,codigo_actividad_eco,Unif_Mod),
             by=c("dom_m","codigo_actividad_eco")) %>% 
  group_by(dom_m,codigo_actividad_eco) %>% 
  sample_n(Unif_Mod) %>% 
  select(id_empresa,dom_m,codigo_actividad_eco)

# control de muetsra

muestra %>% group_by(dom_m) %>% summarise(n_muestra=n()) %>%
  left_join(select(tamanio,n4,dom_m = dominio),by="dom_m") %>% 
  mutate(dif = n_muestra-n4) %>% adorn_totals() %>% View()


tam_selec_final %>% adorn_totals() %>% View()

















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
