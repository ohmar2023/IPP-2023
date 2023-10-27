
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

            

aux <- marco_2021_canasta %>% group_by(codigo_actividad_eco,tamanou_plazas) %>% 
  summarise(N1=n()) %>% 
  mutate(dominio = paste0(tamanou_plazas,substr(codigo_actividad_eco,1,1)))%>%
  group_by(dominio) %>% summarise(N_Prod = n()) %>% 
  left_join(Tam_Total_95_15,by="dominio") %>% select(dominio,N_Prod,n_pro,Total) %>% 
  rename("Dominio"=dominio,"Estratos"=N_Prod)                       

ggplot(aux %>% filter(Dominio!="5C"), aes(x = Dominio, y= n_pro, fill = Estratos)) + 
  geom_bar(stat = "identity")+
  scale_fill_brewer()

