library(readxl)


# LECTURA DE BASES DE DATOS -----------------------------------------------


data_muestra_f <- read_excel("PILOTO COBERTURA/Muestra_Empresas_CAB-SIPP_v1.xlsx", 
                                           sheet = "Consolidado Forzosa") %>% clean_names()

data_muestra_m_p <- read_excel("PILOTO COBERTURA/Muestra_Empresas_CAB-SIPP_v1.xlsx", 
                             sheet = "Consolidado Pequeñas Medianas") %>% clean_names()


# GRANDES -----------------------------------------------------------------

DB1 <- data_muestra_f %>% select(id_empresa,ciiu_letra,codigo_actividad_eco,
                                 paste0("codigo_producto_",c(1:5))) %>% 
  pivot_longer(
    cols = c(paste0("codigo_producto_",c(1:5))),
    names_to = "Producto",
    values_to = "Codigo")

DB1 %>% group_by(Codigo) %>% summarise(n()) %>% adorn_totals() %>% View("DB1")


# MEDIANAS PEQUEÑAS -------------------------------------------------------


DB2 <- data_muestra_m_p %>% select(id_empresa,ciiu_1,codigo_actividad_eco,
                                 paste0("codigo_producto_",c(1:5))) %>% 
  pivot_longer(
    cols = c(paste0("codigo_producto_",c(1:5))),
    names_to = "Producto",
    values_to = "Codigo")

DB2 %>% group_by(Codigo) %>% summarise(n()) %>% adorn_totals() %>% View("DB2")
