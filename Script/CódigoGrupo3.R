#Intregrantes: Milka Chasi, Angela Castillo Y Marilyn Suquillo
#Instalar paquetes
library(dplyr)
library(openxlsx)
library (tidyverse)
library(magrittr)
library(ggplot2)
#Cargar la data
empresas<-read.xlsx("Datos/balances_2014.xlsx")
#Convertir a tibble
EMPRESAS_T <-tibble:: as_tibble(empresas)
#Seleccionar y renombrar
empresas_FIL <- EMPRESAS_T %>% 
    select(Empresas = nombre_cia, Status = situacion, Tpo_de_empresa = tipo, País = pais,
         Provincia = provincia, Cantón = canton, Ciudad = ciudad, Actividad_económica = ciiu4_nivel1, Subactividad = ciiu4_nivel6) %>%  view("data_filtrada")
  
