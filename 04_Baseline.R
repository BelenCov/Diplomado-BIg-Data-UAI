

#install.packages("ROCR")
#install.packages("pROC")
#install.packages('PRROC')

#install.packages("rfUtilities")

rm(list=ls())

#setwd("~/Documentos/diplomado_big_data/trabajo_final/datos")
setwd("~/capacitaciones/diplomadio_big_data/proyecto_final/datos/BASE DATOS")

library (ROCR) # Esto calcula las medidas de precision, solo sirve para predicciones binarias
library(pROC)
library(PRROC)


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




bl_6 <- CrossTable(bd_ventas_norm_6_eval$tipo_dif_6, bd_ventas_norm_6_eval$base_line,
                   prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                   dnn = c('Real', 'Predicted'))

bl_12 <-CrossTable(bd_ventas_norm_12_eval$tipo_dif_12, bd_ventas_norm_12_eval$base_line,
                   prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                   dnn = c('Real', 'Predicted'))

bl_6 <- as.data.frame(bl_6[1])
bl_12 <- as.data.frame(bl_12[1])

bl_6

Precision_6 <- (bl_6[2,3]/(bl_6[1,3]+bl_6[2,3])+
                  bl_6[3,3]/(bl_6[3,3]+bl_6[4,3]))/2
Recall_6 <- (bl_6[2,3]/(bl_6[1,3]+bl_6[4,3])+
               bl_6[3,3]/(bl_6[3,3]+bl_6[1,3]))/2

Precision_12 <- (bl_12[2,3]/(bl_12[1,3]+bl_12[2,3])+
                   bl_12[3,3]/(bl_12[3,3]+bl_12[4,3]))/2
Recall_12 <- (bl_12[2,3]/(bl_12[1,3]+bl_12[4,3])+
                bl_12[3,3]/(bl_12[3,3]+bl_12[1,3]))/2


accuracy(bd_ventas_norm_6_eval$base_line, bd_ventas_norm_6_eval$tipo_dif_6)
accuracy(bd_ventas_norm_12_eval$base_line, bd_ventas_norm_12_eval$tipo_dif_12)

roc6 <- roc(bd_ventas_norm_6_eval$tipo_dif_6,
            bd_ventas_norm_6_eval$aleatorio,
            levels=c("No Crece", "Crece"))
plot(roc6)

auc(roc6)

roc12 <- roc(bd_ventas_norm_12_eval$tipo_dif_12,
             bd_ventas_norm_12_eval$aleatorio,
             levels=c("No Crece", "Crece"))
plot(roc12)

auc(roc12)



# Exportando bases a csv
#########################

write.csv(bd_ventas_norm_6_train, file="bd_ventas_norm_6_train.csv")
write.csv(bd_ventas_norm_6_eval, file="bd_ventas_norm_6_eval.csv")
write.csv(bd_ventas_norm_6_test, file="bd_ventas_norm_6_test.csv")

write.csv(bd_ventas_norm_12_train, file="bd_ventas_norm_12_train.csv")
write.csv(bd_ventas_norm_12_eval, file="bd_ventas_norm_12_eval.csv")
write.csv(bd_ventas_norm_12_test, file="bd_ventas_norm_12_test.csv")



