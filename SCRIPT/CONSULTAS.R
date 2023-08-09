emp %>% filter(anio==2020) %>% group_by(codigo_seccion) %>% summarise(base_21=n()) %>% 
  left_join(
dir_old %>% filter(anio==2020) %>% group_by(codigo_seccion) %>% summarise(base_20=n()), 
by="codigo_seccion") %>% View()

emp %>% filter(codigo_seccion=="O",!id_empresa %in% dir_old$id_empresa,anio==2020) %>% View()


emp %>% filter(id_empresa=="49680377196") %>% rbind(dir_old %>% filter(id_empresa=="49680377196")) %>% View()

dir_old %>% filter(id_empresa=="49680377196")


# VERIFICANDO PRODUCTOS CANASTA EN EL MARCO -------------------------------

marco_2021_canasta %>% group_by(codigo_actividad_eco) %>% summarise(n()) %>% adorn_totals(c("row")) %>% View()

sum(unique(marco_2021_canasta$codigo_actividad_eco) %in% unique(canasta$COD_PRODUCTO))
sum(!unique(canasta$COD_PRODUCTO) %in% unique(marco_2021_canasta$codigo_actividad_eco))

no_estan <- canasta %>% filter((!unique(canasta$COD_PRODUCTO) %in% unique(marco_2021_canasta$codigo_actividad_eco)))

a1 <- directorio %>% filter( codigo_actividad_eco %in% as.vector(no_estan$COD_PRODUCTO), tamanou_plazas!=1 )

table(a1$anio)

no_estan %>% View()


