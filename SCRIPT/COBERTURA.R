
library(readxl)
library(janitor)

cobertura_ipp <- read_excel("DATA/COBERTURA/COBERTURA_001.xlsx", 
                            sheet = "Formulario IPP-DN") %>% clean_names()

cobertura_emp <- read_excel("DATA/COBERTURA/COBERTURA_001.xlsx", 
                            sheet = "Cobertura Empresa") %>% clean_names()



db1 <- cobertura_ipp %>% left_join(select(cobertura_emp,num_empresa,
                                          ruc),by="num_empresa") %>% 
  left_join(select(directorio,ruc=ruc_principal,codigo_seccion,
                   codigo_actividad_eco,tamanou_plazas),by="ruc") %>% 
  mutate(dom = paste0(tamanou_plazas,codigo_seccion))

db1 %>% group_by(dom) %>% summarise(n_distinct(ciiu)) %>% View()

                                    