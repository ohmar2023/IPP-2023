
# CARGAMOS EL DIRECTORIO 2020 ---------------------------------------------
directorio_2020 <- readRDS("DATA/DIRECTORIO/2020/directorio_20220412.rds") %>%
  filter(anio==2020)

directorio_2020 <- directorio_2020 %>%  mutate(id_empresa=as.character(id_empresa),
                                               dom = paste0(tamanou_plazas,codigo_seccion))

a_1 <- marco_2021_canasta %>% filter(codigo_seccion=="C",tamanou_plazas!=5) %>% 
  group_by(codigo_division,codigo_actividad_eco) %>% 
  summarise(n()) 

n_distinct(a_1$codigo_actividad_eco)

a_1 %>% group_by(codigo_division) %>% summarise(Total_cod_2 = n()) %>%
  left_join(
    marco_2021_canasta %>% filter(codigo_seccion=="C",tamanou_plazas!=5) %>% 
      group_by(codigo_division) %>% 
      summarise(Total_Emp=n()),
    by="codigo_division"
    ) %>% View()

# CALCULO DE TAMAÑO -------------------------------------------------------
nc=0.9
z=qnorm(nc+(1-nc)/2)
er=0.1
base <- marco_2021_canasta %>% filter(codigo_seccion=="C",tamanou_plazas!=5)

tam_c <- base %>% 
  mutate(dominio=paste0(tamanou_plazas,codigo_division),
         ventas_totales=as.numeric(ventas_totales)) %>% 
  group_by(dominio) %>% 
  summarise(N=n(),
            desv=sd(ventas_totales,na.rm = T),
            ventas=sum(ventas_totales,na.rm = T)) %>% 
  mutate(numerador=(N*desv)^2,
         denominador=((N-1)/N)*((er*ventas/z)^2)+N*(desv^2),
         tam=numerador/denominador) %>% 
  left_join(select(tnr,dominio,tnr_max,tnr_pro, tnr_min),by="dominio") %>% 
  mutate(tnr_max=ifelse(is.na(tnr_max),0,tnr_max/100),
         tnr_pro=ifelse(is.na(tnr_pro),0,tnr_pro/100),
         tnr_min=ifelse(is.na(tnr_min),0,tnr_min/100),
         n1=ceiling(tam/(1-tnr_max)),
         n2=ifelse(n1>N,N,n1),
         n3=ceiling(tam/(1-tnr_pro)),
         n4=ifelse(n3>N,N,n3),
         n5=ceiling(tam/(1-tnr_min)),
         n6=ifelse(n5>N,N,n5))

tam_c %>% select(dominio,N,n4) %>% 
  mutate(Porc = n4*100/N) %>% 
  adorn_totals()  %>% View("TAMAÑO")

# GRAFICOS ----------------------------------------------------------------

grafica <- marco_2021_canasta %>% filter(codigo_seccion=="C",tamanou_plazas!=5)

# ggplot(grafica %>% group_by(tamanou_plazas) %>% summarise(Total=sum(ventas_totales,na.rm = T)),
#        aes(Total,fill=as.character(tamanou_plazas)))+geom_bar(position="dodge")

ggplot(grafica,
       aes(codigo_division,fill=as.character(tamanou_plazas)))+geom_bar(position="dodge")

# DESVIACIONES POR DOMINIO ------------------------------------------------

ggplot(grafica,
       aes(codigo_division,fill=as.character(tamanou_plazas)))+geom_bar(position="dodge")

ggplot(tam_c,aes(y=desv,x=dominio))+geom_point()
