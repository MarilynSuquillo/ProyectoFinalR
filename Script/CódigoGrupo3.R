##Integrantes----
#Karla Marilyn Suquillo Morales
#Milka Adriana Chasi Jimenez
#Anshela Melania Castillo Nicolalde

#cargar paquetes----
library(tidyverse)
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

#se determina si existen valores faltantes
sum(is.na(data_balances))

#se realiza la limpieza de los datos 

n_columnas <- ncol(data_balances)
for (i in 1:n_columnas) {
  data_balances_limpio <- data_balances[!is.na(data_balances[, i]), ]
}
dim(data_balances_limpio)

#1
empresas<- as_tibble(data_balances_limpio)
dim(empresas)

#Calcular los indicadores financieros
empresas<-mutate(empresas,Liquidez_corriente=v345/v539,
                 Endeudamiento_del_activo=v599/v499,
                 Endeudamiento_patrimonial=v599/v698,
                 Endeudamiento_del_Activo_Fijo=v698/v498,
                 Apalancamiento=v499/v698)

#seleccionar las columnas solicitadas
empresas <- select(empresas,nombre_cia , situacion, tipo, pais, 
                   provincia, canton, ciudad, ciiu4_nivel1, ciiu4_nivel6,
                   Liquidez_corriente,Endeudamiento_del_activo,Endeudamiento_patrimonial,
                   Endeudamiento_del_Activo_Fijo,Apalancamiento)

#Renombrar las columnas
empresas<- rename(empresas, Empresas = nombre_cia,
                  Status = situacion,
                  Tipo_de_empresa= tipo,
                  País= pais,
                  Provincia= provincia,
                  Cantón= canton, 
                  Ciudad=  ciudad, 
                  Actividad_económica= ciiu4_nivel1, 
                  Subactividad= ciiu4_nivel6) 


empresas

<<<<<<< HEAD
#PARTE 2----------------------------------------------------------------------------------------------
#Respondiendo a las preguntas de investigación
#Agrego las columnas con las que se necesita operar
empresas$trab_direc<-data_balances_limpio$trab_direc
empresas$tamanio<-data_balances_limpio$tamanio
empresas$trab_admin<-data_balances_limpio$trab_admin


#1. ¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?
PM<-empresas %>% select(tamanio,Endeudamiento_del_activo) %>% filter(tamanio=="PEQUEÑA" | tamanio=="MICRO") 
PM_limpio<-PM [ is.finite(PM$Endeudamiento_del_activo), ]
E_activo_PM<-sum(PM_limpio$Endeudamiento_del_activo, na.rm = TRUE)
=======
#Analisis sobre las preguntas ----

# ¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?
#Tabla
Tamaño_End_Act <- data_ok %>% 
  select(Empresas,Tamaño, Endeudamiento_del_Activo) 
Tamaño_End_Act <- Tamaño_End_Act %>% group_by(Tamaño) %>% summarise(Total_End_Act=sum(Endeudamiento_del_Activo, na.rm = TRUE)) 
Tamaño_End_Act <- Tamaño_End_Act %>% filter(Tamaño == "MICRO" | Tamaño == "PEQUEÑA" | Tamaño == "GRANDE")
Tamaño_End_Act <- Tamaño_End_Act %>% spread(key = Tamaño, value = Total_End_Act) %>% 
  mutate(MICRO_PEQUE = MICRO + PEQUEÑA) %>% select(MICRO_PEQUE,GRANDE) %>% view("VS")

Tamaño_End_Act<-Tamaño_End_Act %>% gather(key="Tamaño",value = "Total_End_Act") %>% view("VS")

#Gráfica
Tamaño_End_Act %>% ggplot(aes(x = Tamaño, y=Total_End_Act, fill = Tamaño))+
  geom_bar(stat = "identity")+
  labs(title = "Comparacion entre Grande y Micro-pequeña Empresas",x = "Tamaño", y = "Endeudamiento del Activo")+
  theme_bw()+
  theme(legend.position = "none")

#Describe el top 10 de Empresas con mayor Apalancamiento.
#Tabla
top_10_Apal <- data_ok %>% select(Empresas, Apalancamiento) %>% 
  arrange(desc(Apalancamiento)) %>% head(10)%>% view()
  
  
#Gráfica
top_10_Apal %>% ggplot(aes(Apalancamiento,
                           y = Empresas, fill = Empresas))+
  geom_bar(stat = "identity")+
  labs(title = "Las 10 Empresas con Mayor Apalancamiento ",x = "Apalancamiento", y = "Empresas")+
  theme_bw()+
  theme(legend.position = "none")

#Crea una tabla resumiendo el número total de empresas por actividad económica y por actividad económica por cada cantón.
>>>>>>> d6c19f162e5b91f1bf6a23566f399aa058f93a11

G<-empresas %>% select(tamanio,Endeudamiento_del_activo) %>% filter(tamanio=="GRANDE") 
E_activo_G<-sum(G$Endeudamiento_del_activo, na.rm = TRUE)

RESULTADOS<-data.frame(
  Tipo_empresa = c("Micro + Pequeñas", "Grandes"),
  Endeudamiento=c(E_activo_PM,E_activo_G)
)

ggplot(RESULTADOS, aes(x = Tipo_empresa, y = Endeudamiento)) +
  geom_bar(stat = "identity", fill= "blue") +
  labs(title = "Endeudamiento del activo en empresas según el tamaño",
       x = "Tamaño empresa", y = "Endeudamiento del activo") +
  theme_minimal()

<<<<<<< HEAD
#2. ¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más 
#de 60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

LIQ_B<-empresas %>% select(Tipo_de_empresa,Liquidez_corriente,trab_direc) %>% 
  group_by(Tipo_de_empresa) %>% filter(trab_direc>=60) 
LIQ_B_limpio<- LIQ_B[ is.finite(LIQ_B$Liquidez_corriente), ]
ResB<-sum(LIQ_B_limpio$Liquidez_corriente)

LIQ_C<-empresas %>% select(Liquidez_corriente,trab_admin,Tipo_de_empresa) %>%
  group_by(Tipo_de_empresa)%>% filter(trab_admin >=100 & trab_admin <=800)
LIQ_C_limpio<- LIQ_C[ is.finite(LIQ_C$Liquidez_corriente), ]
ResC<-sum(LIQ_C_limpio$Liquidez_corriente)


L_RES<-data.frame(
  Filtros=c(">= 60 trabajadores direct."," 100-800 trabajadores administ. "),
  liquidez_x_compañía= c(ResB, ResC)
  
)

ggplot(L_RES, aes(x = Filtros, y = liquidez_x_compañía)) +
  geom_bar(stat = "identity", fill= "gray") +
  labs(title = "Liquidez por tipo compañía vs  # trabajadores",
       x = "", y = "Liquidez") +
  theme_minimal()

#3. Describe el top 10 de empresas con mayor apalancamiento.
TOP_APAL<-empresas %>% select(Empresas,Apalancamiento)
TOP_APAL_limpio<- TOP_APAL[ is.finite(TOP_APAL$Apalancamiento), ]

TOP_ordenados<-TOP_APAL_limpio %>% arrange(desc(Apalancamiento)) 
TOP_10<-head(TOP_ordenados,10)

ggplot(TOP_10, aes(x = Apalancamiento, y = reorder(Empresas,Apalancamiento))) +
  geom_bar(stat = "identity", fill= "red") +
  labs(title = "10 empresas mejor apalancadas",
       x = "Apalancamiento", y = "Empresas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#PARTE 3 TAREAS ESPECÍFICAS---------------------------
#Tabla resumida que contiene el número total de empresas por actividad económica y
#por actividad económica por cada cantón.
data_2<- as_tibble(read.xlsx("Datos/ciiu.xlsx"))
data_2<- data_2 %>% filter(CODIGO=="A" | CODIGO=="B" | CODIGO=="C"| CODIGO=="D"|CODIGO=="E"|
                             CODIGO=="F"|CODIGO=="G"|CODIGO=="H"|CODIGO=="I"|CODIGO=="J"|CODIGO=="K"|
                             CODIGO=="L"|CODIGO=="M"|CODIGO=="N"|CODIGO=="O"|CODIGO=="P"|CODIGO=="Q"|
                             CODIGO=="R"|CODIGO=="S"|CODIGO=="T"|CODIGO=="U"|CODIGO=="Z")
data_2<-data_2 %>%select(CODIGO,DESCRIPCION)

data_3<-empresas %>% select(Actividad_económica)
tabla1<-data_3 %>% group_by(Actividad_económica) %>% summarise(Ntotal_emp_Actividad_eco=n()) %>%
  left_join(data_2,by=c("Actividad_económica"="CODIGO"))  
tabla1<-select(tabla1,Actividad_económica,DESCRIPCION,Ntotal_emp_Actividad_eco)
tabla1 

tabla2<-empresas%>% group_by(Actividad_económica,Cantón) %>% summarise(Ntotal_empresas_ecoycanton=n()) %>% 
  left_join(data_2,by=c("Actividad_económica"="CODIGO"))
tabla2<-select(tabla2,Actividad_económica,DESCRIPCION,Cantón,Ntotal_empresas_ecoycanton)
tabla2

#Comparativo gráfico de los indicadores financieros de liquidez y
#solvencia por Status y provincia.
ggplot(empresas, aes(x =Provincia, y = Liquidez_corriente,fill=Status)) +
  geom_bar(stat = "summary", position = "stack")+ 
  labs(title = "liquidez_corriente por Status y Provincia",
       x = "Provincia", y = "Liquidez_corriente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(empresas, aes(x =Provincia, y = Endeudamiento_del_activo,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Endeudamiento del activo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento del activo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(empresas, aes(x =Provincia, y = Endeudamiento_patrimonial,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Endeudamiento patrimonial por Status y Provincia",
       x = "Provincia", y = "Endeudamiento patrimonial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(empresas, aes(x =Provincia, y = Endeudamiento_del_Activo_Fijo,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Endeudamiento_del_Activo_Fijo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento_del_Activo_Fijo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(empresas, aes(x =Provincia, y = Apalancamiento,fill=Status)) +
  geom_bar(stat = "summary", position = "stack") +
  labs(title = "Comparativo de Apalancamiento por Status y Provincia",
       x = "Provincia", y = "Apalancamiento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  labs(title = "Comparativo de los indicadores financieros de liquidez y solvencia por tipo de empresa",
       x = "Tipo de Empresa", y = "Valor",
       color = "Indicadores Financieros") +  # Cambiar el título de la leyenda
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Union %>% ggplot(aes(x= Actividad_económica,
                     y = Total_empresas_por_actv, fill = Cantón))+
  geom_bar(stat = "identity")+
  labs(title = "Resumen de la actividad economica de las empresas por canton",x = "Actividad Economica", y = "Total")+
  theme_bw()+
  theme(legend.position = "none")

