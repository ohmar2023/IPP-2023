
dim(marco_2021_canasta)
marco_prov_inc_for <- marco_2021_canasta %>% filter(tamanou_plazas==5) %>% group_by(codigo_provincia,codigo_canton) %>% 
  summarise(Total = n()) %>% adorn_totals() 

marco_prov_inc_for <- marco_prov_inc_for %>% mutate(codigo_provincia = str_pad(codigo_provincia, 2, "left", pad = "0"),
                              codigo_canton = str_pad(codigo_canton, 4, "left", pad = "0"))


# NOMBRES PROVINCIAS ------------------------------------------------------

cod_prov <- NULL
cod_prov <- data.frame(
  codigo_provincia=c(paste0("0",seq(1,9,1)), seq(10,19,1), seq(21,24,1),90),
  Provincia=c("Azuay", "Bolívar", "Cañar", "Carchi", "Cotopaxi", "Chimborazo",
           "El Oro", "Esmeraldas", "Guayas", "Imbabura", "Loja", "Los Ríos", "Manabí",
           "Morona Santiago", "Napo", "Pastaza", "Pichincha", "Tungurahua", 
           "Zamora Chinchipe", "Sucumbíos", "Orellana", "Santo Domingo de los Tsáchilas",
           "Santa Elena", "Zona no delimitada")
) %>% mutate(codigo_provincia=as.character(codigo_provincia), Provincia=as.character(Provincia))


# match -------------------------------------------------------------------

marco_prov_inc_for <- marco_prov_inc_for %>% left_join(cod_prov,by="codigo_provincia") %>% 
  select(Provincia,codigo_provincia,codigo_canton,Total)
View(marco_prov_inc_for)
unique(marco_prov_inc_for$Provincia)
