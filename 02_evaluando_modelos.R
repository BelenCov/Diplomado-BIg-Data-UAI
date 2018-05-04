

#install.packages("caret")

#install.packages("recipes")
#install.packages("geometry")

rm(list=ls())

#setwd("~/Documentos/diplomado_big_data/trabajo_final/datos")
#setwd("~/capacitaciones/diplomadio_big_data/proyecto_final/datos/BASE DATOS")
setwd("~/DIPLOMADO BIG DATA/PROYECTO FINAL/BASE DATOS")

library(tidyverse)
library(ggplot2)
library(readxl)
library(outreg)
library(plyr)
library(compare)
library(psych) # Para usar describeBy para hacer summaries
library(doBy)
library(dplyr)
library(BalancedSampling) # Esto entrega muestras balanceadas

library(ggcorrplot) # Para graficar correlaciones (requiere ggplot2)

library(gmodels) #Para hacer las tablas de contingencia
library(lattice)
library(caret)

library(ggthemes) # visualization
library(scales) # visualization



# Cargando los datos
######################

bd_ventas_norm_12_train <- readRDS( "bd_ventas_norm_12_train.rds")
bd_ventas_norm_12_eval <- readRDS( "bd_ventas_norm_12_eval.rds")
bd_ventas_norm_12_test <- readRDS( "bd_ventas_norm_12_test.rds")

bd_ventas_norm_6_train <- readRDS( "bd_ventas_norm_6_train.rds")
bd_ventas_norm_6_eval <- readRDS( "bd_ventas_norm_6_eval.rds")
bd_ventas_norm_6_test <- readRDS( "bd_ventas_norm_6_test.rds")



# Creando Baseline
###################
pto_corte_1_12 <- (table (bd_ventas_norm_12_train$tipo_dif_12)[1])/nrow(bd_ventas_norm_12_train)

pto_corte_1_6 <- (table (bd_ventas_norm_6_train$tipo_dif_6)[1])/nrow(bd_ventas_norm_6_train)


set.seed(12345)

bd_ventas_norm_6_eval$aleatorio <- runif(nrow(bd_ventas_norm_6_eval))
bd_ventas_norm_6_eval$base_line <- ifelse(bd_ventas_norm_6_eval$aleatorio<=pto_corte_1_6,"Crece",
                                                 "No Crece")

bd_ventas_norm_12_eval$aleatorio <- runif(nrow(bd_ventas_norm_12_eval))
bd_ventas_norm_12_eval$base_line <- ifelse(bd_ventas_norm_12_eval$aleatorio<=pto_corte_1_12,"Crece",
                                                 "No Crece")




bd_ventas_norm_6_eval$aleatorio <- NULL
bd_ventas_norm_6_eval$base_line <- NULL

bd_ventas_norm_12_eval$aleatorio <- NULL
bd_ventas_norm_12_eval$base_line <- NULL



# Exportando bases a csv
#########################

write.csv(bd_ventas_norm_6_train, file="bd_ventas_norm_6_train.csv")
write.csv(bd_ventas_norm_6_eval, file="bd_ventas_norm_6_eval.csv")
write.csv(bd_ventas_norm_6_test, file="bd_ventas_norm_6_test.csv")

write.csv(bd_ventas_norm_12_train, file="bd_ventas_norm_12_train.csv")
write.csv(bd_ventas_norm_12_eval, file="bd_ventas_norm_12_eval.csv")
write.csv(bd_ventas_norm_12_test, file="bd_ventas_norm_12_test.csv")



