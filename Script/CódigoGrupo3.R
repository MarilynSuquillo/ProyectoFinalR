##Integrantes----
#Karla Marilyn Suquillo Morales
#Milka Adriana Chasi Jimenez
#Anshela Melania Castillo Nicolalde

#cargar paquetes----
library(tidyverse)
library(magrittr)
library(openxlsx)
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)

#PARTE 1----------------------------------------------------------------------------------------------
#Construcción data con indicadores financieros----
#Cargar la data base
data_balances<- as_tibble(read.xlsx("Datos/balances_2014.xlsx"))
data_balances


#1
empresas<- as_tibble(data_balances)
dim(empresas)

#Calcular los indicadores financieros
empresas<-mutate(empresas,Liquidez_corriente=v345/v539,
                 Endeudamiento_del_activo=v599/v499,
                 Endeudamiento_patrimonial=v599/v698,
                 Endeudamiento_del_Activo_Fijo=v698/v498,
                 Apalancamiento=v499/v698)

#se determina si existen valores faltantes
sum(is.na(empresas))

#se realiza la limpieza de los datos 

empresas <- empresas[!is.na(empresas$Liquidez_corriente), ]  
empresas <- empresas[!is.na(empresas$Endeudamiento_del_activo), ] 
empresas <- empresas[!is.na(empresas$Endeudamiento_patrimonial), ]  
empresas <- empresas[!is.na(empresas$Endeudamiento_del_Activo_Fijo), ] 
empresas <- empresas[!is.na(empresas$Apalancamiento), ]


empresas <- empresas %>% filter(Liquidez_corriente !=0, 
                                Endeudamiento_del_activo !=0, 
                                Endeudamiento_patrimonial !=0,
                                Endeudamiento_del_Activo_Fijo !=0, 
                                Apalancamiento !=0)

empresas <- empresas[ is.finite(empresas$Liquidez_corriente), ] 
empresas <- empresas[ is.finite(empresas$Endeudamiento_del_activo), ] 
empresas <- empresas[ is.finite(empresas$Endeudamiento_patrimonial), ] 
empresas <- empresas[ is.finite(empresas$Endeudamiento_del_Activo_Fijo), ] 
empresas <- empresas[ is.finite(empresas$Apalancamiento), ]

#Renombrar las columnas
empresas<- select(empresas, Empresas = nombre_cia,
                  Status = situacion,
                  Tipo_de_empresa= tipo,
                  País= pais,
                  Provincia= provincia,
                  Cantón= canton, 
                  Ciudad=  ciudad,
                  Actividad_económica= ciiu4_nivel1, 
                  Subactividad= ciiu4_nivel6, trab_direc,Liquidez_corriente,tamanio,
                  trab_admin,Endeudamiento_del_activo,Endeudamiento_patrimonial,
                  Endeudamiento_del_Activo_Fijo,Apalancamiento) 

empresas

#PARTE 2----------------------------------------------------------------------------------------------
#Respondiendo a las preguntas de investigación

#1. ¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?
PM<-empresas %>% select(tamanio,Endeudamiento_del_activo) %>% filter(tamanio=="PEQUEÑA" | tamanio=="MICRO") 
E_activo_PM<-sum(PM$Endeudamiento_del_activo, na.rm = TRUE)

G<-empresas %>% select(tamanio,Endeudamiento_del_activo) %>% filter(tamanio=="GRANDE") 
E_activo_G<-sum(G$Endeudamiento_del_activo, na.rm = TRUE)

RESULTADOS<-data.frame(
  Tamaño_empresa = c("Micro + Pequeñas", "Grandes"),
  Endeudamiento=c(E_activo_PM,E_activo_G)
)

ggplot(RESULTADOS, aes(x = Tamaño_empresa, y = Endeudamiento, fill = Tamaño_empresa)) +
  geom_bar(stat = "identity") +
  labs(x = "Tamaño empresa", y = "Endeudamiento del activo") +
  theme_minimal() + 
  theme_bw(base_size = 11) +
  facet_wrap(~"Endeudamiento del Activo: Micro + Peque vs Grande") + 
  theme(legend.position = "none")

#2. ¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más 
#de 60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

LIQ_B<-empresas %>% select(Tipo_de_empresa,Liquidez_corriente,trab_direc) %>% 
  group_by(Tipo_de_empresa) %>% filter(trab_direc>=60) 
ResB<-sum(LIQ_B$Liquidez_corriente, na.rm = TRUE)

LIQ_C<-empresas %>% select(Liquidez_corriente,trab_admin,Tipo_de_empresa) %>%
  group_by(Tipo_de_empresa)%>% filter(trab_admin >=100 & trab_admin <=800)
ResC<-sum(LIQ_C_limpio$Liquidez_corriente, na.rm = TRUE)


L_RES<-data.frame(
  Filtros=c("Mayor e igual a 60 trabajadores direct."," 100 a 800 trabajadores administ."),
  liquidez_x_compañía= c(ResB, ResC)
)

ggplot(L_RES, aes(x = Filtros, y = liquidez_x_compañía)) +
  geom_bar(stat = "identity", fill= "blue") +
  labs(x = "Valor", y = "Liquidez") +
  theme_minimal() +
  theme_bw(base_size = 11) +
  facet_wrap(~"Liquidez por tipo de compañia frente a la cantidad de trabajadores") + 
  theme(legend.position = "none")

#3. Describe el top 10 de empresas con mayor apalancamiento.
TOP_APAL<-empresas %>% select(Empresas,Apalancamiento)

TOP_ordenados<-TOP_APAL %>% arrange(desc(Apalancamiento)) 
TOP_10<-head(TOP_ordenados,10)

ggplot(TOP_10, aes(x = Apalancamiento, y = reorder(Empresas,Apalancamiento), fill = Empresas)) +
  geom_bar(stat = "identity") +
  labs(x = "Apalancamiento", y = "Empresas") +
  theme_bw(base_size = 11) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  facet_wrap(~"Las 10 empresas con mejor apalancamiento") 

 #PARTE 3 TAREAS ESPECÍFICAS---------------------------
#Tabla resumida que contiene el número total de empresas por actividad económica y
#por actividad económica por cada cantón.
data_2<- as_tibble(read.xlsx("Datos/ciiu.xlsx"))
data_2<-data_2 %>%select(CODIGO,DESCRIPCION)

data_3<-empresas %>% select(Empresas,Actividad_económica,Cantón) %>% group_by(Actividad_económica) %>% 
  count(Empresas) %>% arrange(desc(Actividad_económica)) %>% 
  summarise(Total_empresas_por_actv=sum(n, na.rm = TRUE)) %>%  view("data_3")

tabla2<-empresas%>% select(Empresas,Actividad_económica,Cantón) %>%   group_by(Cantón) %>% 
  count(Actividad_económica)

TABLA <- left_join(data_3, data_2, by = c("Actividad_económica"="CODIGO"))
TABLA <- left_join(TABLA,tabla2,by = ("Actividad_económica"))

Tabla_Resumen <- TABLA %>% select(Actividad_económica,DESCRIPCION,Cantón,
      Total_empresas_por_cant = n,Total_empresas_por_actv) %>% 
      arrange(Actividad_económica, desc(Total_empresas_por_cant)) %>% 
       view("tabla_resumen")

#Comparativo gráfico de los indicadores financieros de liquidez y
#solvencia por Status y provincia.

ggplot(empresas, aes(x = " ", y = Liquidez_corriente ,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") + 
  labs(title = "Liquidez_corriente por Status y Provincia",
       x = " ", y = "Liquidez_corriente") +
    facet_wrap(~Provincia, scales = "free_y") + 
  theme_bw(base_size = 9) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 3))


ggplot(empresas, aes(x = " ", y = Endeudamiento_del_activo,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Endeudamiento del activo por Status y Provincia",
       x = " ", y = "Endeudamiento del activo") +
   facet_wrap(~Provincia, scales = "free_y") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3))


ggplot(empresas, aes(x = " ", y = Endeudamiento_patrimonial,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Endeudamiento patrimonial por Status y Provincia",
       x = " ", y = "Endeudamiento patrimonial") +
  facet_wrap(~Provincia, scales = "free_y") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3))


ggplot(empresas, aes(x = " ", y = Endeudamiento_del_Activo_Fijo,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Endeudamiento_del_Activo_Fijo por Status y Provincia",
       x = " ", y = "Endeudamiento_del_Activo_Fijo") +
  facet_wrap(~Provincia, scales = "free_y") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3))


ggplot(empresas, aes(x = " ", y = Apalancamiento,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Apalancamiento por Status y Provincia",
       x = " ", y = "Apalancamiento") +
  facet_wrap(~Provincia, scales = "free_y") +
  theme_bw(base_size = 7) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3))


# Comparativo de los indicadores financieros de liquidez y
#solvencia por tipo de empresa.

ggplot(empresas, aes(x = Tipo_de_empresa)) +
  geom_line(aes(y = Liquidez_corriente, group = 1, color = "Liquidez Corriente"), 
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_point(aes(y = Liquidez_corriente, group = 1, color = "Liquidez Corriente"), 
             stat = "summary", fun = "mean", position = "dodge", size = 3) +
  geom_line(aes(y = Endeudamiento_del_activo, group = 1, color = "Endeudamiento del Activo"), 
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_point(aes(y = Endeudamiento_del_activo, group = 1, color = "Endeudamiento del Activo"), 
             stat = "summary", fun = "mean", position = "dodge", size = 3)+
  geom_line(aes(y = Endeudamiento_patrimonial, group = 1, color = "Endeudamiento Patrimonial"), 
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_line(aes(y = Endeudamiento_del_Activo_Fijo, group = 1, color = "Endeudamiento del Activo Fijo"), 
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_line(aes(y = Apalancamiento, group = 1, color = "Apalancamiento"), 
            stat = "summary", fun = "mean", position = "dodge", size = 1) +
  geom_point(aes(y = Endeudamiento_patrimonial, group = 1, color = "Endeudamiento Patrimonial"), 
             stat = "summary", fun = "mean", position = "dodge", size = 3) +
  geom_point(aes(y = Endeudamiento_del_Activo_Fijo, group = 1, color = "Endeudamiento del Activo Fijo"), 
             stat = "summary", fun = "mean", position = "dodge", size = 3) +
  geom_point(aes(y = Apalancamiento, group = 1, color = "Apalancamiento"), 
             stat = "summary", fun = "mean", position = "dodge", size = 3)+
  scale_color_manual(values = c("Liquidez Corriente" = "blue", 
                                "Endeudamiento del Activo" = "red", 
                                "Endeudamiento Patrimonial" = "green", 
                                "Endeudamiento del Activo Fijo" = "orange", 
                                "Apalancamiento" = "purple")) +
  theme_minimal() +
  labs(x = "Tipo de Empresa", y = "Valor",
       color = "Indicadores Financieros") + 
  theme_bw(base_size = 10)+
  facet_wrap(~"Comparativo de los Indicadores Financieros de Liquidez y Solvencia por Tipo de empresa")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") 



