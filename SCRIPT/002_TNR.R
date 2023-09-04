
#-------------------------------------------------------------------------------
# TNR DE ENESEM 2021
#-------------------------------------------------------------------------------
tnr_1 <- readRDS("DATA/TNR/tnr_enesem_historica.rds") %>% 
  select(dominio,tnr_max, tnr_pro, tnr_min)

#-------------------------------------------------------------------------------
# TNR PARA EL RESTO DE DOMINIOS: El resto de dominios que no aparecen en ENESEM
# no tienen TNR, por lo que, se define la tasa como el promedio por actividad
# principal. 
#-------------------------------------------------------------------------------
tnr_2 <- tnr_1 %>% mutate(act_principal=substr(dominio,2,2)) %>% 
  group_by(act_principal) %>% 
  summarise_if ( is.numeric , mean , na.rm  =  TRUE ) %>% 
  mutate(dominio=paste0("2",act_principal)) %>% 
  select(dominio,contains("tnr"))

tnr <- rbind(tnr_2,tnr_1)
tnr <- rbind(tnr, marco_sin_inc_for %>% 
               mutate(dom=paste0(tamanou_plazas,codigo_seccion)) %>% 
               filter(!dom %in% tnr$dominio) %>% 
               select(dominio=dom) %>% 
               unique()%>% 
               mutate(tnr_max=20.0, 
                      tnr_pro=20.0,
                      tnr_min=20.0))
