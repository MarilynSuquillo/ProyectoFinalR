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
#Se renombró las variables para calcular los indicadores financieros
#Seleccionar y renombrar columnas a usar
EMPRESAS_FIL <- EMPRESAS_T %>% 
    select(Empresas = nombre_cia, Status = situacion, Tipo_de_empresa = tipo, 
           País = pais, Provincia = provincia, Cantón = canton, Ciudad = ciudad, 
           Actividad_económica = ciiu4_nivel1, Subactividad = ciiu4_nivel6, 
           Activo_corriente = v345, Pasivo_corriente = v539, Pasivo= v599, 
           Activo = v499, Patrimonio = v698, Activo_no_Corriente = v498) %>% #view("data_filtrada")
  
#Calculo de indicadores financieros  #OBSERVACION EL CALCULO
    mutate(Liquidez_corriente = Activo_corriente/Pasivo_corriente) %>% #view() 
    mutate(Endeudamiento_del_Activo = Pasivo/Activo) %>% 
    mutate(Endeudamiento_patrimonial = Pasivo/Patrimonio) %>%  
    mutate(Endeudamiento_del_activo_fijo = Patrimonio/Activo_no_Corriente) %>% 
    mutate(Apalancamiento = Activo/Patrimonio) %>% view("indicadores")

#Quitar las columnas innecesarias para tener lista la data
EMPRESAS_OK <- EMPRESAS_FIL %>% select(-Activo_corriente, -Pasivo_corriente, -Pasivo,
                                       -Activo,-Patrimonio, -Activo_no_Corriente) 

#Eliminar observaciones que contengan NA de la data
EMPRESAS_SIN_NA <- drop_na(EMPRESAS_OK) %>% view("sinNA")

 
