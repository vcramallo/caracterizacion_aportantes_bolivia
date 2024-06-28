# ============================================================#
# Trabajo Final - Módulo Data Minig                           #
# Cuenca Ramallo Verónica                                     #
# Miranda Gonzales Amilcar                                    #
# Miranda Gonzales Yuri                                       #
# ============================================================#
rm(list=ls())     # Borrar objetos actuales
graphics.off()    # Limpia el espacio para las graficas
# paquetería
install.packages("readxl")
install.packages("tidyverse") # manipulación de data frames
install.packages("dplyr") # manipulación de data frames
install.packages("ggplot2")
install.packages("foreign") # lee archivos de spss y stata
install.packages("descr") # tablas de frecuencias 
install.packages("openxlsx")
install.packages("psych")
install.packages("tidyr")
install.packages("labelled")

library(readxl)
library(openxlsx)
library(foreign)
library(descr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(psych)
library(tidyr)
library(labelled)

EH2021<-read.spss("D:/1_YURI/INFO/Base EH2021/EH2021_Persona.sav", to.data.frame = TRUE)
class(EH2021)
dim(EH2021)
names(EH2021)
str(EH2021)
attr(EH2021, "variable.labels")
valores_unicos <- unique(EH2021$s04f_36 )
valores_unicos

EH2021=filter(EH2021, condact=="p_ocupado", ylab>0, pet=="pet", s01a_03>14)      #edad14=filter(EH2021, s01a_03>14, ylab>0)
tabla1<-freq(ordered(EH2021$pet), plot = FALSE)
tabla1

#Gráfica de histograma ocupados por edades y area
ggplot(EH2021,aes(x=s01a_03))+geom_histogram(bins=13,col="blue", fill="brown") + facet_grid(. ~ area) +
  labs(x="Edad",y="Frecuencia")

#Gráfica de histograma aportantes por edades y  sexo
base_ap=filter(EH2021, condact=="p_ocupado", ylab>0, pet=="pet", s04f_36=="1. Si")
ggplot(base_ap,aes(x=s01a_03))+geom_histogram(bins=15, col="blue", fill="#FFB300") + facet_grid(. ~ s01a_02) +
  labs(title="Histograma 2: Aportantes por edades y sexo ",x="Edad",y="Frecuencia")

# Gráfico de caja de aportantes e ingreso laboral
data2 = filter(EH2021, ylab>0, s04f_36 =="1. Si"|s04f_36 =="2. No", pet=="pet") 
dim(data2)

par(mfrow = c(1, 2))
boxplot(data2$ylab ~ data2$s04f_36, xlab = "Condición de aporte",            # Nombre del eje X
        ylab = "Ingreso laboral", col=c("blue"))       
boxplot(data=base_ap, ylab~s01a_02, 
        xlab = "Sexo", ylab = "Ingreso laboral", col=c("#FFB300"))

#Gráfico de barras Aportantes por género y depto
colores <- c("blue", "#FFCA28")
ggplot(base_ap[!is.na(base_ap$s04f_36),], aes(x = s04f_36, fill = s01a_02)) +  geom_bar(position = "dodge2") +
  facet_grid(. ~ depto) +   scale_fill_manual(values = colores)+ labs(title = "Aportantes a una AFP por departamento según género", x = "Departamento", y = "Aportantes", fill = "Género")

# Gráfico de caja de Ingreso laboral de aportantes a la AFP por area
boxplot(data=base_ap, ylab~area, main="Ingreso laboral de aportantes a la AFP por area",
        xlab = "Area", ylab = "Ingreso laboral", col=c("#FFCA28"))

# Gráfico de caja de Ingreso laboral de aportantes a la AFP por departamento
boxplot(data=base_afi, ylab~depto, main="Ingreso laboral de la PET que aporta a la AFP \n por departamento",
        xlab = "Departamento", ylab = "Ingreso laboral", col=c("#80DEEA"))

par(mfrow = c(1, 2))
boxplot(data=EH2021, ylab~area, main="Ingreso laboral de aportantes a la AFP por area",
        xlab = "Area", ylab = "Ingreso laboral", col=c("#FFCA28"))
boxplot(data=base_ap, ylab~depto, main="Ingreso laboral de la PET que aporta a la AFP \n por departamento",
        xlab = "Departamento", ylab = "Ingreso laboral", col=c("#80DEEA"))
#Gráfico de Densidad
ggplot(base_ap,aes(x=ylab, color=area, fill=area))+geom_density(alpha=0.5)+   scale_fill_manual(values = colores)+ labs(title="Densidad del Ingreso laboral por área", x="Ingreso laboral", y="Densidad")

#Gráfico de Dispersión
ggplot(base_ap, aes(s01a_03,ylab)) + geom_point(aes(col=depto, size=ylab, alpha=0.2))+theme_light()+ scale_fill_manual(values = colores) +labs(title = "Gráfico de dispersión Edad Vs. Ingreso laboral por departamento",x = "Edad",y = "Ingreso laboral")

  # CONSTRUCCIÓN DE VARIABLES 
  # el nombre de la base en esta secciòn cambia a "base", sin embargo se mantiene
  # con las mismas especificaciones, es decir, solo se trabaja con la PET. 
EH2021$sexo = EH2021$s01a_02
EH2021$edad = EH2021$s01a_03
EH2021$rel_parentesco = EH2021$s01a_05
EH2021$afiliado_afp = EH2021$s04f_35
EH2021$aporta_afp = EH2021$s04f_36

EH2021$sexo = EH2021$s01a_02
EH2021$edad = EH2021$s01a_03
EH2021$rel_parentesco = EH2021$s01a_05
EH2021$afiliado_afp = EH2021$s04f_35
EH2021$aporta_afp = EH2021$s04f_36


addmargins(table(EH2021$afiliado_afp, EH2021$aporta_afp))
table(EH2021$aporta_afp)
table(EH2021$afiliado_afp)

# Volver a logaritmo neperiano 
base = EH2021
log_ylab <- log(base$ylab)
log_ylab

base <- data.frame(base,log_ylab)

# Volver cuadrado la variable edad 
edad_2 <- base$s01a_03**2
edad_2

base <- data.frame(base,edad_2)

# variable dummy para los afiliados 1 0 
dim(base)
table(base$s04f_36)
class(base$s04_36)
levels(base$s04_36)
aportantes <- ifelse(base$s04f_36 == 1, 1, 0)
aportantes

base <- data.frame(base,aportantes) # Se une y guarda en la misma base

# Variable dummy para SEXO MUJER 1 HOMBRE 0 
mujeres <- ifelse(base$s01a_02 == 2, 1, 0)
mujeres

base <- data.frame(base,mujeres)
table(base$mujeres)

# Variable dummy para estado CIVIL  CASADO 1 RESTO 0
soltero <- ifelse(base$s01a_10 == 1, 1, 0)
soltero

base <- data.frame(base,soltero)

# Variable dummy para estado AREA URBANO 1 RURAL 0
rural <- ifelse(base$area == 2, 1, 0)
rural

base <- data.frame(base,rural)

# Variable dummy para Seguro de salud  1.publico 2 privado 3 otro 4 ninguno
segsalud <- ifelse(base$cobersalud == 4, 0, 1)
segsalud

base <- data.frame(base,segsalud)

# Variable dummy para sabe leer y escribir 
lee_escribe <- ifelse(base$s03a_01 == 1, 1, 0)
lee_escribe

base <- data.frame(base,lee_escribe)

# Variable dummy para  trabajo al menos 1hr. la semana pasada
trab_hr <- ifelse(base$s04a_01 == 1, 1, 0)
trab_hr

base <- data.frame(base,trab_hr)

# MODELOS 

modelo1 <- glm(aportantes ~ ylab + s01a_03 + area + s01a_02 + niv_ed_g, data=base, family= "binomial")
summary(modelo1)

modelo2 <- glm(aportantes ~ log_ylab + s01a_03 + urbano + mujeres + niv_ed_g + casado + remesas + rentadig + segsalud, data=base, family= "binomial")
summary(modelo2)

modelo3 <- glm(aportantes ~ niv_ed_g, data=base, family= "binomial")
summary(modelo3)

modelo4 <- glm(aportantes ~ log_ylab + s01a_03 + edad_2+ mujeres + urbano + casado + segsalud + lee_escribe + aestudio + trab_hr, data=base, family= "binomial")
summary(modelo4)

modelo5 <- glm(aportantes ~ log_ylab + s01a_03 + edad_2+ mujeres + urbano + casado + segsalud + lee_escribe + aestudio + trab_hr, data=base_limpia, family= "binomial")
summary(modelo5)

modelo6 <- glm(aportantes ~ log_ylab + edad_2+ hombres + niv_ed_g + segsalud + aestudio, data=base, family= "binomial")
summary(modelo6)

modelo7 <- glm(aportantes ~ log_ylab + edad_2+ hombres + niv_ed_g + segsalud + aestudio + desocupado, data=base, family= "binomial")
summary(modelo7)

modelo8 <- glm(aportantes ~ log_ylab + edad_2+ edad_ag + hombres + niv_ed_g + segsalud + aestudio + aestudio_ag, data=base, family= "binomial")
summary(modelo8)

# bueno
# Rural al menos acompaña con el signo
modelo9 <- glm(aportantes ~ log_ylab + s01a_03 + mujeres + soltero + rural + segsalud + aestudio, data=base, family= "binomial")
summary(modelo9)

modelo9 <- glm(aportantes ~ log_ylab + s01a_03 + mujeres + soltero + rural + segsalud + aestudio, 
              data = base, 
             family = "binomial",
            maxit = 1000)  # Aumenta el número máximo de iteraciones

var_ind = base  %>% select(log_ylab , s01a_03 , mujeres , soltero , rural , segsalud , aestudio)
cor( var_ind )

  # Obtener los coeficientes del modelo
  coeficientes <- coef(modelo9)
# Calcular los odds ratios
odds_ratios <- exp(coeficientes)

# Obtener el resumen del modelo para los errores estándar
resumen <- summary(modelo9)

# Calcular los intervalos de confianza para los coeficientes
conf_intervals <- confint(modelo9, level = 0.95)

# Calcular los odds ratios y sus intervalos de confianza
odds_ratios <- exp(coeficientes)
odds_conf_intervals <- exp(conf_intervals)

# Crear una tabla combinada
tabla_odds <- cbind(Odds_Ratio = odds_ratios, 
                    CI_0.95 = odds_conf_intervals[, 1], 
                    CI_0.95 = odds_conf_intervals[, 2])

# Mostrar la tabla de odds ratios
print(tabla_odds)

  mod1<-lm(log_ylab ~ afiliados, data=ingresos)
mod1
summary(mod1) # ver las estimaciones del modelo 

  # La confianza de que se aportante se encuentra entre esta probabilidad 
  # va estar dentro de estos valores la probabilidad de que seas aportante 
  confint(modelo9, level = 0.95) 

# verificacion del modelo 

install.packages("vcd")
library(vcd)

# Valor de clasificaciòn en este caso si pasa el 0.5, mayor a 0.5 es aportante 
# menos o igual a 0.5 no aporta
estimaciones <- ifelse(test=modelo9$fitted.values > 0.5, yes=1, no=0) # clasifica 
estimaciones # estos son los y gorrito del modelo 0 1 para cada afiliado

# el model es parte de la escritura del R (el del medio no se cambia)
matriz_confusion <- table(modelo9$model$aportantes, estimaciones, dnn =c("observados", "estimados"))
matriz_confusion

# Que porcentaje esta bien estimado 
# Diagonal principal suma 
dig_prin <- 325 + 3137
denominador <- 325+118+448+3137
Porcentaje_est <- dig_prin/denominador
Porcentaje_est  # 0.8594836

# las clasificaciones que el modelo hizo correctas el modelo es 162 (diagonal principal)

mosaic(matriz_confusion, shade = TRUE, colorize= TRUE, gp = gpar(fill=matrix(c("#80DEEA", "#FFCA28", "#FFCA28", "#80DEEA"),2),2))

  # ***        4. VERSIÓN REPORTE      *** #
  
  install.packages("stargazer")
library(stargazer)

# Presentación estándar de modelos 

stargazer(modelo9, type="text")

stargazer(tabla_odds, type="text")




