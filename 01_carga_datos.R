

#install.packages("compare")
#install.packages("psych")

rm(list=ls())

setwd("~/DIPLOMADO BIG DATA/PROYECTO FINAL/BASE DATOS")


library(tidyverse)
library(ggplot2)
library(readxl)
library(outreg)
library(plyr)
library(compare)
library(psych)




tipos_variables <- c("text", "numeric", "numeric", "numeric", "text", "text", "text", "text", 
                     "text", "text", "text", "text", 
                     "text", "text", "text", "numeric", 
                     "numeric", "text", "text", 
                     "text", "text", "text", 
                     "numeric", "text", "text", "text", 
                     "text", "text", "text", "text", 
                     "text", "numeric")


tipos_variables_2 <- c("text", "numeric", "numeric", "numeric", "text", "text", "text", "text", 
                       "text", "text", "text", "text", "text", 
                       "text", "text", "text", "numeric", 
                       "numeric", "text", "text", "text", 
                       "text", "text", "text", 
                       "text", "text", "text", "text", 
                       "text", "text", "text", "text", 
                       "text", "numeric")

#6,12,13, 20-25, 27, 30, 33

Microcredito_2015_01 <- read_excel("01 ENERO_CREDITO_2015.xlsx", sheet = "BASE", col_types = tipos_variables )
Microcredito_2015_02 <- read_excel("02 FEBRERO_CREDITO_2015.xlsx", sheet = "Base", col_types = tipos_variables )
Microcredito_2015_03 <- read_excel("03 MARZO_2015_CREDITO.xlsx", sheet = "Base", col_types = tipos_variables)
Microcredito_2015_04 <- read_excel("04 ABRIL_CREDITO_2015.xlsx", sheet = "Base", col_types = tipos_variables)
Microcredito_2015_05 <- read_excel("05 MAYO_2015_CREDITO.xlsx", sheet = "Mayo_15", col_types = tipos_variables)
Microcredito_2015_06 <- read_excel("06 JUNIO_2015_CREDITO.xlsx", sheet = "Base", col_types = tipos_variables)
Microcredito_2015_07 <- read_excel("07 JULIO_CREDITO_2015.xlsx", sheet = "Trabajada", col_types = tipos_variables) 
Microcredito_2015_08 <- read_excel("08 AGOSTO_CREDITO_2015.xlsx", sheet = "Trabajada", col_types = tipos_variables)
Microcredito_2015_09 <- read_excel("09 SEPT_CREDITO_2015.xlsx", sheet = "Septiembre", col_types = tipos_variables)
Microcredito_2015_10 <- read_excel("10 OCT_CREDITO_2015.xlsx", sheet = "Octubre", col_types = tipos_variables)
Microcredito_2015_11 <- read_excel("11 NOV_CREDITO_2015.xlsx", sheet = "Operaciones", col_types = tipos_variables)
Microcredito_2015_12 <- read_excel("12 DIC_CREDITO_2015.xlsx", sheet = "OPERAC", col_types = tipos_variables)

Microcredito_2016_01 <- read_excel("01 ENERO_CREDITO_2016.xlsx", sheet = "Enero", col_types = tipos_variables)
Microcredito_2016_02 <- read_excel("02 FEBRERO_CREDITO_2016.xlsx", sheet = "Febrero", col_types = tipos_variables)
Microcredito_2016_03 <- read_excel("03 MARZO_CREDITO_2016.xlsx", sheet = "Hoja1", col_types = tipos_variables)
Microcredito_2016_04 <- read_excel("04 ABRIL_CREDITO_2016.xlsx", sheet = "Rendición", col_types = tipos_variables)
Microcredito_2016_05 <- read_excel("05 MAYO_CREDITO_2016.xlsx", sheet = "Mayo", col_types = tipos_variables)
Microcredito_2016_06 <- read_excel("06 JUNIO_CREDITO_2016.xlsx", sheet = "Informe_MicroCredito_1907201605", col_types = tipos_variables_2) # DESDE aca se incorporan 2 variables nuevas
Microcredito_2016_07 <- read_excel("07 JULIO_CREDITO_2016.xlsx", sheet = "Informe_MicroCredito_1808201611", col_types = tipos_variables_2)
Microcredito_2016_08 <- read_excel("08 AGOSTO_CREDITO_2016.xlsx", sheet = "Informe_MicroCredito_1609201610", col_types = tipos_variables_2)
Microcredito_2016_09 <- read_excel("09 SEPT_CREDITO_2016.xlsx", sheet = "Septiembre", col_types = tipos_variables_2)
Microcredito_2016_10 <- read_excel("10 OCT_CREDITO_2016.xlsx", sheet = "Informe_MicroCredito_2811201608", col_types = tipos_variables_2)
Microcredito_2016_11 <- read_excel("11 NOV_CREDITO_2016.xlsx", sheet = "Informe_MicroCredito_2112201611", col_types = tipos_variables_2)
Microcredito_2016_12 <- read_excel("12 DIC_CREDITO_2016.xlsx", sheet = "Dic16", col_types = tipos_variables_2)

Microcredito_2017_01 <- read_excel("01 ENERO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_0603201702", col_types = tipos_variables_2)
Microcredito_2017_02 <- read_excel("02 FEBRERO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_2103201712", col_types = tipos_variables_2)
Microcredito_2017_03 <- read_excel("03 MARZO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_1505201703", col_types = tipos_variables_2)
Microcredito_2017_04 <- read_excel("04 ABRIL_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_2205201711", col_types = tipos_variables_2)
Microcredito_2017_05 <- read_excel("05 MAYO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_0407201708", col_types = tipos_variables_2)
Microcredito_2017_06 <- read_excel("06 JUNIO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_0208201712", col_types = tipos_variables_2)
Microcredito_2017_07 <- read_excel("07 JULIO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_0509201709", col_types = tipos_variables_2) #Hay 2 variables distintas a los meses anteriores
Microcredito_2017_08 <- read_excel("08 AGO_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_2509201712", col_types = tipos_variables_2)
Microcredito_2017_09 <- read_excel("09 SEPT_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_1810201711", col_types = tipos_variables_2)
Microcredito_2017_10 <- read_excel("10 OCT_CREDITO_2017.xlsx", sheet = "Informe_MicroCredito_2311201712", col_types = tipos_variables_2)
Microcredito_2017_11 <- read_excel("11 NOV_CREDITO_2017.xlsx", sheet = "Hoja1", col_types = tipos_variables_2)
Microcredito_2017_11_1 <- read_excel("11 NOV_CREDITO_CCMIP_2017.xlsx", sheet = "Informe_Financiamiento+MIPYME-C")
Microcredito_2017_12 <- read_excel("12 DIC_CREDITO_2017.xlsx", sheet = "Informe_Microcrédito-crédito+y+", col_types = tipos_variables_2)
Microcredito_2017_12_1 <- read_excel("12 DIC_CREDITO_CCMIP_2017.xlsx", sheet = "Informe_Financiamiento+MIPYME-C")

Microcredito_2018_01 <- read_excel("1 ENERO_CREDITO_CCMIP_2018.xlsx", sheet = "Informe_Financiamiento+MIPYME-C")

table(Microcredito_2018_01$`Nombre Tamaño Empresa`)

Microcredito_2015_01$anno <- 2015
Microcredito_2015_01$mes <- 1
Microcredito_2015_02$anno <- 2015
Microcredito_2015_02$mes <- 2
Microcredito_2015_03$anno <- 2015
Microcredito_2015_03$mes <- 3
Microcredito_2015_04$anno <- 2015
Microcredito_2015_04$mes <- 4
Microcredito_2015_05$anno <- 2015
Microcredito_2015_05$mes <- 5
Microcredito_2015_06$anno <- 2015
Microcredito_2015_06$mes <- 6
Microcredito_2015_07$anno <- 2015
Microcredito_2015_07$mes <- 7
Microcredito_2015_08$anno <- 2015
Microcredito_2015_08$mes <- 8
Microcredito_2015_09$anno <- 2015
Microcredito_2015_09$mes <- 9
Microcredito_2015_10$anno <- 2015
Microcredito_2015_10$mes <- 10
Microcredito_2015_11$anno <- 2015
Microcredito_2015_11$mes <- 11
Microcredito_2015_12$anno <- 2015
Microcredito_2015_12$mes <- 12

Microcredito_2016_01$anno <- 2016
Microcredito_2016_01$mes <- 1
Microcredito_2016_02$anno <- 2016
Microcredito_2016_02$mes <- 2
Microcredito_2016_03$anno <- 2016
Microcredito_2016_03$mes <- 3
Microcredito_2016_04$anno <- 2016
Microcredito_2016_04$mes <- 4
Microcredito_2016_05$anno <- 2016
Microcredito_2016_05$mes <- 5
Microcredito_2016_06$anno <- 2016
Microcredito_2016_06$mes <- 6
Microcredito_2016_07$anno <- 2016
Microcredito_2016_07$mes <- 7
colnames(Microcredito_2016_07) <- names(Microcredito_2016_06)
Microcredito_2016_08$anno <- 2016
Microcredito_2016_08$mes <- 8
Microcredito_2016_09$anno <- 2016
Microcredito_2016_09$mes <- 9
Microcredito_2016_10$anno <- 2016
Microcredito_2016_10$mes <- 10
Microcredito_2016_11$anno <- 2016
Microcredito_2016_11$mes <- 11
Microcredito_2016_12$anno <- 2016
Microcredito_2016_12$mes <- 12

Microcredito_2017_01$anno <- 2017
Microcredito_2017_01$mes <- 1
Microcredito_2017_02$anno <- 2017
Microcredito_2017_02$mes <- 2
Microcredito_2017_03$anno <- 2017
Microcredito_2017_03$mes <- 3
Microcredito_2017_04$anno <- 2017
Microcredito_2017_04$mes <- 4
Microcredito_2017_05$anno <- 2017
Microcredito_2017_05$mes <- 5
Microcredito_2017_06$anno <- 2017
Microcredito_2017_06$mes <- 6
Microcredito_2017_07$anno <- 2017
Microcredito_2017_07$mes <- 7
Microcredito_2017_08$anno <- 2017
Microcredito_2017_08$mes <- 8
Microcredito_2017_09$anno <- 2017
Microcredito_2017_09$mes <- 9
Microcredito_2017_10$anno <- 2017
Microcredito_2017_10$mes <- 10
Microcredito_2017_11$anno <- 2017
Microcredito_2017_11$mes <- 11
Microcredito_2017_12$anno <- 2017
Microcredito_2017_12$mes <- 12
Microcredito_2017_11_1$anno <- 2017
Microcredito_2017_11_1$mes <- 11
Microcredito_2017_12_1$anno <- 2017
Microcredito_2017_12_1$mes <- 12

Microcredito_2018_01$anno <- 2018
Microcredito_2018_01$mes <- 1


all_equal(Microcredito_2015_01, Microcredito_2015_02, ignore_col_order =F)
all_equal(Microcredito_2015_02, Microcredito_2015_03, ignore_col_order =F)
all_equal(Microcredito_2015_03, Microcredito_2015_04, ignore_col_order =F)
all_equal(Microcredito_2015_04, Microcredito_2015_05, ignore_col_order =F)
all_equal(Microcredito_2015_05, Microcredito_2015_06, ignore_col_order =F)
all_equal(Microcredito_2015_06, Microcredito_2015_07, ignore_col_order =F)
all_equal(Microcredito_2015_06, Microcredito_2015_08, ignore_col_order =F)
all_equal(Microcredito_2015_08, Microcredito_2015_09, ignore_col_order =F)
all_equal(Microcredito_2015_09, Microcredito_2015_10, ignore_col_order =F)
all_equal(Microcredito_2015_10, Microcredito_2015_11, ignore_col_order =F)
all_equal(Microcredito_2015_11, Microcredito_2015_12, ignore_col_order =F)
all_equal(Microcredito_2015_12, Microcredito_2016_01, ignore_col_order =F)
all_equal(Microcredito_2016_01, Microcredito_2016_02, ignore_col_order =F)
all_equal(Microcredito_2016_02, Microcredito_2016_03, ignore_col_order =F)
all_equal(Microcredito_2016_03, Microcredito_2016_04, ignore_col_order =F)
all_equal(Microcredito_2016_04, Microcredito_2016_05, ignore_col_order =F)
all_equal(Microcredito_2016_05, Microcredito_2016_06, ignore_col_order =F)
all_equal(Microcredito_2016_06, Microcredito_2016_07, ignore_col_order =F)
all_equal(Microcredito_2016_07, Microcredito_2016_08, ignore_col_order =F)
all_equal(Microcredito_2016_08, Microcredito_2016_09, ignore_col_order =F)
all_equal(Microcredito_2016_09, Microcredito_2016_10, ignore_col_order =F)
all_equal(Microcredito_2016_10, Microcredito_2016_11, ignore_col_order =F)
all_equal(Microcredito_2016_11, Microcredito_2016_12, ignore_col_order =F)
all_equal(Microcredito_2016_12, Microcredito_2017_01, ignore_col_order =F)
all_equal(Microcredito_2017_01, Microcredito_2017_02, ignore_col_order =F)
all_equal(Microcredito_2017_02, Microcredito_2017_03, ignore_col_order =F)
all_equal(Microcredito_2017_03, Microcredito_2017_04, ignore_col_order =F)
all_equal(Microcredito_2017_04, Microcredito_2017_05, ignore_col_order =F)
all_equal(Microcredito_2017_05, Microcredito_2017_06, ignore_col_order =F)
all_equal(Microcredito_2017_06, Microcredito_2017_07, ignore_col_order =F)
all_equal(Microcredito_2017_07, Microcredito_2017_08, ignore_col_order =F)
all_equal(Microcredito_2017_08, Microcredito_2017_09, ignore_col_order =F)
all_equal(Microcredito_2017_09, Microcredito_2017_10, ignore_col_order =F)
all_equal(Microcredito_2017_10, Microcredito_2017_11, ignore_col_order =F)
all_equal(Microcredito_2017_11, Microcredito_2017_12, ignore_col_order =F)

# Estos son los nuevos datos, son copatibles entre ellos, pero distintos al resto de la data
all_equal(Microcredito_2017_11_1, Microcredito_2017_12_1, ignore_col_order =F)
all_equal(Microcredito_2017_12_1, Microcredito_2018_01, ignore_col_order =F)


bd <- rbind.fill(Microcredito_2015_01, Microcredito_2015_02, Microcredito_2015_03, Microcredito_2015_04, Microcredito_2015_05,
            Microcredito_2015_06, Microcredito_2015_07, Microcredito_2015_08, Microcredito_2015_09, Microcredito_2015_10, 
            Microcredito_2015_11, Microcredito_2015_12,
            Microcredito_2016_01, Microcredito_2016_02, Microcredito_2016_03, Microcredito_2016_04, Microcredito_2016_05, Microcredito_2016_06, Microcredito_2016_07, 
            Microcredito_2016_08, Microcredito_2016_09, Microcredito_2016_10,
            Microcredito_2016_11, Microcredito_2016_12,
            Microcredito_2017_01, Microcredito_2017_02, Microcredito_2017_03, Microcredito_2017_04, Microcredito_2017_05,
            Microcredito_2017_06, Microcredito_2017_07, Microcredito_2017_08, Microcredito_2017_09, Microcredito_2017_10, 
            Microcredito_2017_11, Microcredito_2017_12)


table(bd$mes,bd$anno)

bd$`Ventas Anuales Empresa UF` <-as.numeric(gsub(",", ".", bd$`Ventas Anuales Empresa UF`))
bd$`Número de trabajadores Empresa`<-as.numeric(gsub(",", ".", bd$`Número de trabajadores Empresa`))
bd$`Número de trabajadoras Empresa`<-as.numeric(gsub(",", ".", bd$`Número de trabajadoras Empresa`))
bd$`Tasa de Interés anual`<-as.numeric(gsub(",", ".", bd$`Tasa de Interés anual`))
bd$`Otros Gastos Asociados a la Operación`<-as.numeric(gsub(",", ".", bd$`Otros Gastos Asociados a la Operación`))
bd$`Monto Operación en UF`<-as.numeric(gsub(",", ".", bd$`Monto Operación en UF`))
bd$`Plazo Total Operación meses`<-as.numeric(gsub(",", ".", bd$`Plazo Total Operación meses`))
bd$`Período de gracia meses`<-as.numeric(gsub(",", ".", bd$`Período de gracia meses`))
bd$`Saldo actual Capital Operación en UF`<-as.numeric(gsub(",", ".", bd$`Saldo actual Capital Operación en UF`))
bd$`Plazo Remanente actual de Pago (Cuotas pendientes al día o morosas)`<-as.numeric(gsub(",", ".", bd$`Plazo Remanente actual de Pago (Cuotas pendientes al día o morosas)`))


colnames(bd) <- c("programa",
                  "n",
                  "intermediario",
                  "rut",
                  "tipo_benef",
                  "ventas_uf",
                  "tamano",
                  "acteco",
                  "region",
                  "comuna",
                  "n_trabajadores",
                  "n_trabajadoras",
                  "objeto_op",
                  "tipo_op",
                  "n_op",
                  "fecha_curse",
                  "anno_curse",
                  "tasa_anual",
                  "otros_gastos",
                  "moneda",
                  "monto_op",
                  "plazo_total_op",
                  "per_gracia",
                  "freq_pagos",
                  "n_cuotas",
                  "prog_garan",
                  "op_pag_corfo",
                  "saldo_capital",
                  "estado_actual",
                  "estado_mora",
                  "plazo_remanente",
                  "per_rendic",
                  "anno",
                  "mes",
                  "glosa_region",
                  "mes_curse_op")

# Modficacion de variables

bd$programa[bd$programa=="MICROCRÉDITO-CRÉDITO Y LEASING"] <- "Microcrédito-crédito y leasing"

bd$region[bd$region=="AREA METROPOLITANA"] <- "13"
bd$region[bd$region=="CUARTA REGIÓN"] <- "4"
bd$region[bd$region=="DÉCIMA CUARTA REGIÓN"] <- "14"
bd$region[bd$region=="DÉCIMA PRIMERA REGIÓN"] <- "11"
bd$region[bd$region=="DÉCIMA QUINTA REGIÓN"] <- "15"
bd$region[bd$region=="DÉCIMA REGIÓN"] <- "10"
bd$region[bd$region=="NOVENA REGIÓN"] <- "9"
bd$region[bd$region=="OCTAVA REGIÓN"] <- "8"
bd$region[bd$region=="PRIMERA REGIÓN"] <- "1"
bd$region[bd$region=="QUINTA REGIÓN"] <- "5"
bd$region[bd$region=="SEGUNDA REGIÓN"] <- "2"
bd$region[bd$region=="SÉPTIMA REGIÓN"] <- "7"
bd$region[bd$region=="SEXTA REGIÓN"] <- "6"
bd$region[bd$region=="TERCERA REGIÓN"] <- "3"


bd$res_estado_op <- ""

bd$res_estado_op[bd$estado_actual=="OPERACIÓN AL DÍA"] <- "1 Al dia"
bd$res_estado_op[bd$estado_actual=="OPERACIÓN VIGENTE AL DÍA"] <- "1 Al dia"
bd$res_estado_op[bd$estado_actual=="OPERACIÓN TERMINADA DE PAGAR"] <- "2 Terminada"
bd$res_estado_op[bd$res_estado_op==""] <- "3 En Mora"

bd$periodo <- factor(paste(bd$anno,bd$mes,sep="_"),
                     levels=c("2015_1", "2015_2",  "2015_3",  "2015_4", "2015_5",  "2015_6", 
                              "2015_7",  "2015_8",  "2015_9" ,  "2015_10", "2015_11", "2015_12", 
                              "2016_1",   "2016_2",  "2016_3", 
                              "2016_4",  "2016_5",  "2016_6",  "2016_7",  "2016_8",  "2016_9","2016_10", "2016_11", "2016_12",
                              "2017_1", "2017_2",  "2017_3",  "2017_4", "2017_5",  "2017_6",  "2017_7",  "2017_8",  "2017_9",
                              "2017_10", "2017_11","2017_12"))

saveRDS(bd, "bd.rds")
