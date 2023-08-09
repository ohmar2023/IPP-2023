library(openxlsx)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)
library(reshape2)

marco_2021_canasta <- read_excel("PRODUCTOS/marco_IPP.xlsx")

 
# PRODUCTOS EN EL MARCO POR TAMAÑO EMPRESA --------------------------------

prod_marco <- marco_2021_canasta %>% group_by(codigo_actividad_eco,tamanou_plazas) %>% 
  summarise(N=n()) %>% 
  filter(N>=0)  %>% 
  filter(grepl(codigo_actividad_eco, pattern = "^")) 

prod_marco <- dcast(prod_marco,codigo_actividad_eco~tamanou_plazas, value.var = "N") %>% 
  adorn_totals(c("row", "col"))

prod_marco %>% View("TAM")

# PRODUCTOS EN EL MARCO POR TAMAÑO EMPRESA --------------------------------

prod_dom <- marco_2021_canasta %>% group_by(codigo_actividad_eco,dom_m) %>% 
  summarise(N=n()) %>% 
  filter(N>=0)  %>% 
  filter(grepl(codigo_actividad_eco, pattern = "^")) 

prod_dom <- dcast(prod_dom,codigo_actividad_eco~dom_m, value.var = "N") %>% 
  adorn_totals(c("row", "col"))

prod_dom %>% View()

# PRODUCTOS NO ENCONTRADOS EN EL DIEE -------------------------------------

prod_no <- canasta %>% filter((!unique(canasta$COD_PRODUCTO) %in% 
                      unique(marco_2021_canasta$codigo_actividad_eco))) %>% 
  rename(Producto=COD_PRODUCTO)


# DOMINIOS DE ESTUDIO DEFINIDOS  -----------------------------------------

dom_def <- marco_2021_canasta %>% group_by(dom_m) %>% summarise(Total=n()) %>% 
  rename(Dominio=dom_m) %>% adorn_totals(c("row"))

# ANEXOS DETALLE MARCO ----------------------------------------------------

REPORTE_MARCO_INICIAL <- createWorkbook("Creator of workbook")


addWorksheet(REPORTE_MARCO_INICIAL, sheetName = "ANEXO 001")
addWorksheet(REPORTE_MARCO_INICIAL, sheetName = "ANEXO 002")
addWorksheet(REPORTE_MARCO_INICIAL, sheetName = "ANEXO 003")


writeData(REPORTE_MARCO_INICIAL, sheet = 1, prod_marco ,rowNames = FALSE)
writeData(REPORTE_MARCO_INICIAL, sheet = 2, prod_no ,rowNames = FALSE)
writeData(REPORTE_MARCO_INICIAL, sheet = 3, dom_def ,rowNames = FALSE)

saveWorkbook(REPORTE_MARCO_INICIAL, file =  "REPORTE_MARCO_INICIAL_actualizado.xlsx",
             overwrite = TRUE)




