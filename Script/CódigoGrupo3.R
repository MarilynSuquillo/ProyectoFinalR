#Intregrantes: Milka Chasi, Angela Castillo Y Marilyn Suquillo
library(dplyr)
library(openxlsx)
library (tidyverse)
library(magrittr)
library(ggplot2)

empresas<-read.xlsx("Datos/balances_2014.xlsx")
#Convertir a tibble
EMPRESAS_T <-tibble:: as_tibble(empresas)
