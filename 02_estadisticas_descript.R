
rm(list=ls())

#setwd("~/Documentos/diplomado_big_data/trabajo_final/datos")
setwd("~/capacitaciones/diplomadio_big_data/proyecto_final/datos/BASE DATOS")

library(tidyverse)
library(ggplot2)
library(readxl)
library(outreg)
library(plyr)
library(compare)
library(psych)
library(doBy)

# Datos, ver la calidad de los datos, cuales no sirven (estan correlacionados)

# ver la y 

# Baseline: modelo random con metrica de interes
            #o el modelo del experto,
            #predecir con una sola varialbe (la mas correlacionada) usando arboles dedecision o regresion
            # Regresion  por mayoria

bd <- readRDS("bd.rds")

bd <- subset(bd,anno>2015)

pairs(bd)


summary(bd)

# 


table(bd$programa)


table(bd$tipo_benef)

table(bd$tamano)

table(bd$acteco)

table(bd$region)

table(duplicated(bd$rut))  #False es el numero de beneficiarios
table(duplicated(bd$intermediario))  # False es el numero de intermediarios

summary(bd$ventas_uf)

ggplot(bd, aes(ventas_uf)) +
  geom_histogram()+
  scale_x_log10()+
  ggtitle("Distribucion de Ventas Anuales en UF")
  

summary(bd$monto_op)
ggplot(bd, aes(monto_op)) +
  geom_histogram()+
  scale_x_log10()+
  ggtitle("Distribucion de Monto de Prestamo en UF")


ggplot(data = bd) + 
  stat_count(mapping = aes(x = res_estado_op, y = ..prop.., group = 1))+
  ggtitle("Resumen Estados de Operacion")

table(bd$res_estado_op)




# Esto verifica la llave de la base
#table(duplicated(bd$`N° Intermediario`, bd$rut, bd$periodo))  


ggplot(data = bd) + 
  stat_count(mapping = aes(x = periodo))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Cantidad de Operaciones")

table(bd$periodo, bd$res_estado_op)


# Extraccion de primera transaccion
######################################



# Esta es la llave que encontre, sin embargo, aparecen operaciones terminadas o en mora
bd$llave_0 <- paste(bd$rut,bd$n_op, sep="_")
bd$dup <- duplicated(bd$llave_0)
bd1 <- subset(bd,!dup)
bd1 <- subset(bd1,!res_estado_op=="2 Terminada")
bd1 <- subset(bd1,!res_estado_op=="3 En Mora")

table(bd1$res_estado_op)

ggplot(data = bd1) + 
  stat_count(mapping = aes(x = periodo))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Cantidad de Operaciones")

table(bd1$periodo, bd1$res_estado_op)

table(duplicated(bd1$rut))

# Estudio de tasas
summary(bd1$tasa_anual)
summary(bd1$tasa_anual)
weighted.mean(bd1$tasa_anual, bd1$monto_op, na.rm=TRUE)

ggplot(bd, aes(tasa_anual)) +
  geom_histogram()+
  ggtitle("Distribucion de Tasas de interes")
                
bd1$tasa_f <- as.factor(as.integer(bd1$tasa_anual))

table(as.integer(bd1$tasa_anual))    

ggplot(data = bd1) +
  geom_boxplot(mapping = aes(x = reorder(tasa_f, monto_op, FUN = mean),
                             y = monto_op))+
              theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data = bd1) + 
  geom_point(mapping = aes(y=tasa_anual , x =monto_op), alpha = 1 / 100)+  
  scale_x_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
                


summary(bd1$otros_gastos)

summary(bd1$tasa_anual)

bd1$otros_gastos[is.na(bd1$otros_gastos)] <- 0

bd1$carga_Equiv <- bd1$tasa_anual+bd1$otros_gastos

summary(bd1$carga_Equiv)


# Grafico que relaciona tasa de interes con monto
ggplot(data = bd1) + 
  geom_point(mapping = aes(y=tasa_anual , x =monto_op))+  
  geom_smooth(mapping = aes(y=tasa_anual , x =monto_op))+
  scale_x_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Monto de Operacion y Tasa de Interes")

ggplot(data = bd1) +
  geom_bin2d(mapping = aes(y = tasa_anual, x =monto_op))+
  scale_x_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Monto de Operacion y Tasa de Interes")



# Grafico que relaciona ventas con monto
ggplot(data = bd1) + 
  geom_point(mapping = aes(x=ventas_uf , y =monto_op))+  
  geom_smooth(mapping = aes(x=ventas_uf , y =monto_op))+
  scale_x_log10()+
  scale_y_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Monto de Operacion y Nivel de Ventas")

ggplot(data = bd1) +
  geom_bin2d(mapping = aes(x = ventas_uf, y =monto_op))+
  scale_x_log10()+
  scale_y_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Monto de Operacion y Tasa de Interes")


# Grafico que relaciona Tasa de Interes y ventas
ggplot(data = bd1) + 
  geom_point(mapping = aes(x=ventas_uf , y =tasa_anual))+  
  geom_smooth(mapping = aes(x=ventas_uf , y =tasa_anual))+
  scale_x_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Tasa de Interes y Nivel de Ventas")

ggplot(data = bd1) +
  geom_bin2d(mapping = aes(x = ventas_uf, y =tasa_anual))+
  scale_x_log10()+
  scale_y_log10()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Tasa de Interes y Tasa de Interes")


summary(bd$monto_op)

summarise(bd1,  monto_cred_uf = mean(monto_op, na.rm = TRUE))


summaryBy(ventas_uf+monto_op+tasa_f~ periodo, data=bd1)

table(bd1$tasa_f)              
summary(bd1$tasa_anual)

table(bd$tipo_op)


#Genera estadisticas de mora


bd$tiene_mora <- 0
bd$tiene_mora[bd$res_estado_op=="3 En Mora"] <-1

bd_mora0  <- subset(bd,!res_estado_op=="2 Terminada")

bd_mora <- summaryBy( tiene_mora~ rut, FUN=max, data=bd_mora0)

rm(bd_mora0)

summary(bd_mora$tiene_mora.max)


#Estadisticas
################

#numero de operaciones
nrow(bd1)
#False es el numero de beneficiarios
table(duplicated(bd1$rut))

# False es el numero de intermediarios
table(duplicated(bd$intermediario))  

# Tamano empresas
table(bd1$tamano)
summary(bd1$ventas_uf)
ggplot(bd, aes(ventas_uf)) +
  geom_histogram()+
  scale_x_log10()+
  ggtitle("Distribucion de Ventas en UF")

# Distribucion de Creditos por  Actividad economica
table(bd1$acteco)
# Distribucion de Creditos por Genero
table(bd1$tipo_benef)
# Distribucion de Creditos por Region
table(as.numeric(bd1$region))
# Plazo Promedio
summary(bd1$plazo_total_op)
ggplot(bd, aes(plazo_total_op)) +
  geom_histogram()+
  scale_x_log10()+
  ggtitle("Distribucion de Plazos")
# Tasa Promedio
summary(bd1$tasa_anual)
weighted.mean(bd1$tasa_anual, bd1$monto_op, na.rm=TRUE)
ggplot(bd, aes(tasa_anual)) +
  geom_histogram()+
  ggtitle("Distribucion de Tasas de interes")
# Monto Credito Promedio
summary(bd1$monto_op) 
ggplot(bd, aes(monto_op)) +
  geom_histogram()+
  scale_x_log10()+
  ggtitle("Distribucion de Monto de Creditos en UF")

# Numero de operaciones por beneficiario

bd1$contador <- 1
n_op_rut <- summaryBy( contador~ rut, FUN=sum, data=bd1)
bd1$contador <-NULL
names(n_op_rut) <- c("rut","numero_operaciones")
summary(n_op_rut$numero_operaciones)
table(n_op_rut$numero_operaciones)

bd1_1 <- merge(bd1,n_op_rut,"rut")
merge(mydata,mydata2,index)
bd1_1 <-subset(bd1_1,numero_operaciones>1)
bd1_1 <- bd1_1[order(bd1_1$rut),]

