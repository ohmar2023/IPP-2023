library(reshape2)


aux <- marco_2021_canasta %>% group_by(dom_m,codigo_actividad_eco) %>% 
  summarise(Total_Estr = n()) %>% 
  mutate(Proporcion = Total_Estr/sum(Total_Estr)) %>% rename(dominio = dom_m)

aux_2 <- aux %>% left_join(select(tamanio_final_90_10,dominio,n_max),by="dominio") %>% 
  mutate(T_2 = ceiling(Proporcion*n_max),
         #T_3 = if_else(T_2<2,2,T_2),
         Estrato = paste0(dominio,codigo_actividad_eco) ) %>% filter(!grepl("C",dominio))

aux_3 <- marco_2021_canasta %>% mutate(Estrato = paste0(dom_m,codigo_actividad_eco)) %>% 
    left_join(select(aux_2,Estrato,T_2),by = "Estrato") 

aux_4 <- aux_3 %>% filter(!is.na(T_2)) %>% group_by(Estrato) %>% 
  sample_n(unique(T_2))

aux_4 %>% group_by(dominio) %>% summarise(Total_f = n()) %>% 
  left_join(select(tamanio_final_90_10,dominio,n_max),by = "dominio") %>% 
  mutate(din = n_max-Total_f) %>% View()
  
aux_4 %>% mutate(Estrato = paste0(dominio,codigo_actividad_eco)) %>% 
  group_by(Estrato) %>% summarise(Total_f = n()) %>% 
  left_join(select(aux_2,Estrato,T_2),by = "Estrato") %>% 
  adorn_totals() %>% View()


aux_4 %>% group_by(codigo_actividad_eco,tamanou_plazas) %>% summarise(Total = n()) %>% 
  dcast(codigo_actividad_eco~tamanou_plazas, value.var = "Total") %>% 
  adorn_totals(c("row", "col")) %>% 
  View()

write.xlsx(aux_4,"muestra_000.xlsx")


