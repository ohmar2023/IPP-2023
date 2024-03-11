library(readxl)
library(janitor)
library(ggplot2)

#--------------------------------------------------------------------------
# LECTURA DE BASES DE DATOS -----------------------------------------------
#--------------------------------------------------------------------------

data_muestra_f <- read_excel("PILOTO COBERTURA/Muestra_Empresas_CAB-SIPP_v1.xlsx", 
                                           sheet = "Consolidado Forzosa") %>% clean_names()

data_muestra_m_p <- read_excel("PILOTO COBERTURA/Muestra_Empresas_CAB-SIPP_v1.xlsx", 
                             sheet = "Consolidado Pequeñas Medianas") %>% clean_names()


#--------------------------------------------------------------------------
# GRANDES -----------------------------------------------------------------
#--------------------------------------------------------------------------

DB1 <- data_muestra_f %>% select(id_empresa,ciiu_letra,codigo_actividad_eco,
                                 paste0("codigo_producto_",c(1:5))) %>% 
  pivot_longer(
    cols = c(paste0("codigo_producto_",c(1:5))),
    names_to = "Producto",
    values_to = "Codigo")

resumen_1 <- DB1 %>% group_by(Codigo) %>% summarise(n_g=n()) 

#--------------------------------------------------------------------------
# MEDIANAS PEQUEÑAS -------------------------------------------------------
#--------------------------------------------------------------------------

DB2 <- data_muestra_m_p %>% select(id_empresa,ciiu_1,codigo_actividad_eco,
                                 paste0("codigo_producto_",c(1:5))) %>% 
  pivot_longer(
    cols = c(paste0("codigo_producto_",c(1:5))),
    names_to = "Producto",
    values_to = "Codigo") %>% 
  mutate(Codigo = gsub("\\.","",as.character(Codigo)))

resumen_2 <- DB2 %>% group_by(Codigo) %>% summarise(n_mp=n()) 

#--------------------------------------------------------------------------
# GRAFICO -----------------------------------------------------------------
#--------------------------------------------------------------------------

aux <- resumen_1 %>% full_join(resumen_2,by="Codigo") %>% 
  adorn_totals(c("col"))

aux %>% filter(!is.na(Codigo),Total>=5) %>%
  ggplot(aes(x=Codigo,y=Total,fill = Codigo)) + 
  geom_bar(stat="identity")+
  theme(legend.position = "bottom")



  