#Librería ----
library(readxl)
library(tidyverse)
library(effsize)
library(openintro)
library(nortest)
library(car)
#data----
df <- read_excel("~/Datos_Medicos/Data/datos_medicos.xlsx")
summary(df)

##transformación ----
df$Presion <- factor(df$Presion)
dfs <- df %>%
  filter(Presion =="PresionSistolica") %>%
  pull(Valores)#para quedarse solamente con los datos
dfd <- df %>%
  filter(Presion =="PresionDiastolica") %>%
  pull(Valores)#para quedarse solamente con los datos
#Resumen
df %>%
  group_by(Presion) %>%
  summarise(n= length(Valores),
            prom = mean(Valores),
            ds = sd(Valores),
            mediana = median(Valores),
            RIC = IQR(Valores),
            min = min(Valores),
            max = max(Valores))

# supuesto de normalidad ----
# (x-media)/ds
#Ho:x ~ N(miu,sigma**2)
#Ha:x !~ N(miu,sigma**2)
ks.test(dfs, "pnorm", mean = mean(dfs), sd = sd(dfs))
ks.test(dfd, "pnorm", mean = mean(dfd), sd = sd(dfd))

#Supuesto de homocedasticidad ----
var.test(dfs,dfd)
#Verificando paso a paso las salidas de var.test

# t student (se puede usar normal porque las muestras son n1=n2=100>=30 y la t student
#con muy altos grados de libertad converge a la distribucion normal)
t.test(x=dfs,y=dfd,var.equal=TRUE,paired=FALSE)
#con distribucion normal dada la ley de los grandes numeros se cumple
#que se obtiene lo mismo
z.test(x = dfs, y = dfd,
       sigma.x = sd(dfs), sigma.y = sd(dfd),
       alternative = "two.sided")
#Verificando paso a paso las salidas de t.test

#Tamaño del efecto ----
cohen.d(dfs,dfd,paired=FALSE) # representa la distancia entre el efecto de cada grupo

#Verificar las salidas con las notas de clase

#Objetivo 2 ----
#Ho:μ(ganancias) = μ(inversion)
#Ha:μ(ganancias) != μ(inversion)

#data 2 ----
df <- read_excel("~/Datos_Medicos/Data/datos_economia.xlsx")
df$`Tipo de cuenta` <- factor(df$`Tipo de cuenta`)

# transformacion 2 ----
dfi <- df %>%
  filter(`Tipo de cuenta`=="Inversion") %>%
  pull(Precio)
dfg <- df %>%
  filter(`Tipo de cuenta`=="Ganancia") %>%
  pull(Precio)
#Supuestos----
#1. normalidad
#Ho:X~N(μ,σ**2)
#Ha:X!~N(μ,σ**2)

#Kolmogorov-smirnov
ks.test(scale(dfi),"pnorm")
ks.test(scale(dfg),"pnorm")

#2. homocedasticidad(homogeneidad) ----
#Ho:σ**2(ganancia)=σ**2(inversion)
#Ha:σ**2(ganancia)=σ(inversion)
#prueba de Welch
var.test(dfi,dfg)
#Tarea verificar las salidas, Y ADEMAS, para probar los resultados, aplicar Bartlett, Levene y
#Fligner-Killeen, explicar en todas cuál estadistico se utiliza, su distribucion e interprete

#t-test con correlacion de Welch ----
t.test(dfi,
       dfg,
       var.equal = FALSE,
       paired=FALSE)
#verificar las salidas, los grados de libertad son los de la formula larguisima

#Tamaño del efecto 2 ----
cohen.d(dfi,
        dfg,
        paired = FALSE)
#ejercicio usar otra prueba diferente a cohen (opcional)

#Comparación de medias entre dos grupos pareados con estadística paramétrica

# Datos de tiempos antes y después de la intervención educativa
datos <- data.frame(
  estudiante = c(1:10),
  antes = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3),
  despues = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)
)
#realizar EDA tarea

#Supuesto de normalidad ----
shapiro.test(datos$antes)
shapiro.test(datos$despues)

#Diferencia entre las medias ----
t.test(x = datos$despues,
       y = datos$antes,
       paired = TRUE)

#tamaño del efecto 3----
cohen.d(datos$despues,
        datos$antes,
        paired = TRUE)
#Usando el teorema de la diferencia de medias para datos pareados verifica la salida
#de la función t.test con los datos de la intervención educativa





#diferencia de medianas con estadística no parmétrica ----
data('births')
df <- births

#Caracterizar el peso del bebé según si la madre es smoker o no smoker(TAREA) ----

#Supuesto de normalidad ----
df %>%
  group_by(smoke) %>%
  summarise(n = length(weight),
            est_ks = ks.test(scale(weight), "pnorm")$statistic, #solo estadístico
            p_ks = ks.test(scale(weight), "pnorm")$p.value,     #solo p-valor
            est_sw = shapiro.test(weight)$statistic,
            p_sw = shapiro.test(weight)$p.value,
            est_l = lillie.test(weight)$statistic,
            p_l = lillie.test(weight)$p.value)

leveneTest(weight ~ smoke, data = df, center = median)

wilcox.test(weight ~ smoke, data = df)

#USAR PRUEBA PARAMETRICA
