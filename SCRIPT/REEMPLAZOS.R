
venna_001 <- marco_2021_canasta %>% filter(is.na(ventas_totales) )
venna_002 <- directorio_2020 %>% filter(id_empresa %in% venna_001$id_empresa & tamanou_plazas!=1)  

#Se tiene que 20 empresas-NA-2021 tienen un valor mayor a cero en esta variable en el DIEE-2020.
venna_002 %>% filter(ventas_totales>0) %>% group_by(tamanou_plazas) %>% 
    summarise(Total = n()) %>% adorn_totals(c("row"))

venna_002 %>% filter(ventas_totales>0) %>% select(id_empresa,ventas_totales) %>% View()
