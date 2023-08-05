#Grupo_3 ----
#Integrantes:
#Karla Marilyn Suquillo Morales
#Milka Adriana Chasi Jimenez
#Erika Andrea Neira Zea

#Cargar paquetes -----
library(tidyverse)
library(openxlsx)
library(readr)
library(magrittr)
library(dplyr)
library(readxl)
library(ggplot2)

#Cargar la data
Empresas<-read.xlsx("Datos/balances_2014.xlsx")
#Convertir a tibble
EMPRESAS_T <-tibble:: as_tibble(empresas)

#Construcción data con indicadores financieros----
#Seleccionar y renombrar columnas a usar
EMPRESAS_FIL <- EMPRESAS_T %>% 
  select(Empresas = nombre_cia, Status = situacion, Tipo_de_empresa = tipo,
         País = pais, Provincia = provincia, Cantón = canton, Ciudad = ciudad, 
         Actividad_económica = ciiu4_nivel1, Subactividad = ciiu4_nivel6, 
         Activo_corriente = v345, Pasivo_corriente = v539, Pasivo= v599, 
         Activo = v499, Patrimonio = v698, Activo_no_Corriente = v498) %>% 
  #Calculo de indicadores financieros  
  mutate(Liquidez_corriente = Activo_corriente/Pasivo_corriente) %>% 
  mutate(Endeudamiento_del_Activo = Pasivo/Activo) %>% 
  mutate(Endeudamiento_patrimonial = Pasivo/Patrimonio) %>%  
  mutate(Endeudamiento_del_activo_fijo = Patrimonio/Activo_no_Corriente) %>% 
  mutate(Apalancamiento = Activo/Patrimonio) %>% view("indicadores")

#Quitar las columnas innecesarias 
EMPRESAS_OK <- EMPRESAS_FIL %>% select(-Activo_corriente, -Pasivo_corriente, -Pasivo,
                                       -Activo,-Patrimonio, -Activo_no_Corriente)
#Eliminar observaciones que contengan NA para tener la data lista
EMPRESAS_SIN_NA <- drop_na(EMPRESAS_OK) %>% view("sinNA")


#Construcción de la data empresas para hacer las tablas y gráficas ----
empresas <- EMPRESAS_T %>% 
  select(Empresas = nombre_cia, Status = situacion, Tipo_de_empresa = tipo, Tamaño = tamanio,
         País = pais, Provincia = provincia, Cantón = canton, Ciudad = ciudad, 
         Actividad_económica = ciiu4_nivel1, Subactividad = ciiu4_nivel6, 
         Activo_corriente = v345, Pasivo_corriente = v539, Pasivo= v599, 
         Activo = v499, Patrimonio = v698, Activo_no_Corriente = v498) %>% 
  filter(Pasivo_corriente != 0, Activo != 0, Patrimonio != 0, Activo_no_Corriente != 0) %>% 
  mutate(Liquidez_corriente = Activo_corriente/Pasivo_corriente,
         Endeudamiento_del_Activo = Pasivo/Activo, 
         Endeudamiento_patrimonial = Pasivo/Patrimonio,
         Endeudamiento_del_activo_fijo = Patrimonio/Activo_no_Corriente,
         Apalancamiento = Activo/Patrimonio)
#Eliminar columnas innecesarias para tener lista la data
data_ok <- empresas %>% select(-Activo_corriente, -Pasivo_corriente, -Pasivo,
                               -Activo,-Patrimonio, -Activo_no_Corriente) %>% view()

#Analisis sobre las preguntas ----

# ¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?
#Tabla
Tamaño_End_Act <- data_ok %>% 
  select(Empresas,Tamaño, Endeudamiento_del_Activo) 
Tamaño_End_Act <- Tamaño_End_Act %>% group_by(Tamaño) %>% summarise(Total_End_Act=sum(Endeudamiento_del_Activo, na.rm = TRUE)) 
Tamaño_End_Act <- Tamaño_End_Act %>% filter(Tamaño == "MICRO" | Tamaño == "PEQUEÑA" | Tamaño == "GRANDE")
Tamaño_End_Act <- Tamaño_End_Act %>% spread(key = Tamaño, value = Total_End_Act) %>% 
  mutate(MICRO_PEQUE = MICRO + PEQUEÑA) %>% select(MICRO_PEQUE,GRANDE) %>% view("VS")

Tamaño_End_Act<-Tamaño_End_Act %>% gather(key="Tamaño",value = "Total_End_Act", MICRO_PEQUE:GRANDE) %>% 
  view("VS")

#Gráfica
Tamaño_End_Act %>% ggplot(aes(x = Tamaño, y=Total_End_Act, fill = Tamaño))+
  geom_bar(stat = "identity")+
  labs(title = "Comparacion entre Grande y Micro-pequeña Empresas",x = "Tamaño", y = "Endeudamiento del Activo")+
  theme_bw()+
  theme(legend.position = "none")

#Describe el top 10 de Empresas con mayor Apalancamiento.
#Tabla
top_10_Apal <- data_ok %>% select(Empresas, Apalancamiento) %>% 
  arrange(desc(Apalancamiento)) %>% head(10) %>% view()

#Gráfica
top_10_Apal %>% ggplot(aes(x= n,
                           y = Apalancamiento, fill = Empresas))+
  geom_bar(stat = "identity")+
  labs(title = "Las 10 Mayores Empresas por Apalancamiento",x = "Empresas", y = "Apalancamiento")+
  theme_bw()+
  theme(legend.position = "none")


#Crea una tabla resumiendo el número total de empresas por actividad económica y por actividad económica por cada cantón.

Empresas_Actv_Econ <- data_ok %>% select(Empresas,Actividad_económica,Cantón) %>% group_by(Actividad_económica) %>% count(Empresas) %>% group_by(Actividad_económica) %>% summarise(Total_empresas_por_actv=sum(n, na.rm = TRUE)) %>%  view("1")
Empresas_Actv_Econ_2<- data_ok %>%  select(Empresas,Actividad_económica, Cantón) %>% group_by(Cantón) %>% count(Actividad_económica) %>% view("2")
Union <- left_join(Empresas_Actv_Econ,Empresas_Actv_Econ_2, by ="Actividad_económica") %>% view()

Tabla_Resumida <- tibble::as_tibble(Union)

#Comprobación 

Union %>% ggplot(aes(x= Actividad_económica,
                     y = Total_empresas_por_actv, fill = Cantón))+
  geom_bar(stat = "identity")+
  labs(title = "Resumen de la actividad economica de las empresas por canton",x = "Actividad Economica", y = "Total")+
  theme_bw()+
  theme(legend.position = "none")