#-------------------------------------------------------------------------------
# CARGAMOS LA BASE DE MANUFACTURA
#-------------------------------------------------------------------------------
base_manufactura <- read_excel("DATA/CANASTA/CANASTA_LISTADO.xlsx", 
                              sheet = "MANUFACTURA",range = "A1:G131")

#-------------------------------------------------------------------------------
# TNR: Definimos TNR para estos 5 grupos de la categoria MANUFACTURA
#-------------------------------------------------------------------------------
TNR_C_5_GR <- tnr %>% filter(dominio=="2C")
TNR_C_5_GR$dominio[1] <- as.character(1)
                     
for (i in c(2:5)) {
  
  TNR_C_5_GR <- rbind(TNR_C_5_GR,TNR_C_5_GR[1,])
  TNR_C_5_GR$dominio[i] <- as.character(i)

  }

#-------------------------------------------------------------------------------  
# CALCULO DE TAMAÑO PARA LOS 5 DOMINIOS-MANUFACTURA
#-------------------------------------------------------------------------------
nc=0.9
z=qnorm(nc+(1-nc)/2)
er=0.2
base <- marco_2021_canasta %>% filter(codigo_seccion=="C",tamanou_plazas!=5) %>% 
  left_join(select(base_manufactura,codigo_actividad_eco=COD_PROD,Categorias),
            by="codigo_actividad_eco")

tam_c <- base %>% 
  mutate(dominio=as.character(Categorias),
         ventas_totales=as.numeric(ventas_totales)) %>% 
  group_by(dominio) %>% 
  summarise(N=n(),
            desv=sd(ventas_totales,na.rm = T),
            ventas=sum(ventas_totales,na.rm = T)) %>% 
  mutate(numerador=(N*desv)^2,
         denominador=((N-1)/N)*((er*ventas/z)^2)+N*(desv^2),
         tam=numerador/denominador) %>% 
  left_join(select(TNR_C_5_GR,dominio,tnr_max,tnr_pro, tnr_min),by="dominio") %>% 
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

tam_c <- tam_c %>% select("Dominio"=dominio,"Total"=N,"n_max"=n2,"n_pro"=n4,"n_min"=n6)