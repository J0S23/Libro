#Librería ----
library(readxl)
library(moments)
library(tidyverse)
library(BSDA)
library(effsize)
library(openintro)
library(nortest)
library(car)
#data ----
df <- read_excel("/cloud/project/Data/datos_medicos.xlsx")
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
  summarise(n = length(Valores),
            media = mean(Valores),
            sd = sd(Valores),
            mediana = median(Valores),
            RIC = IQR(Valores),
            Q1 = quantile(Valores, 0.25),
            Q3 = quantile(Valores, 0.75),
            minimo = min(Valores),
            maximo = max(Valores),
            curtosis = kurtosis(Valores),
            Coef_asim = skewness(Valores))

# Visualización EDA ----
boxplot(Valores ~ Presion, data = df,
        main = "Comparación Presión Sistólica vs Diastólica",
        ylab = "mmHg")

df %>%
  ggplot(aes(x = Valores, fill = Presion)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de presión sistólica y diastólica")

# supuesto de normalidad ----
# (x-media)/ds
#Ho:x ~ N(miu,sigma**2)
#Ha:x !~ N(miu,sigma**2)
ks.test(dfs, "pnorm", mean = mean(dfs), sd = sd(dfs))
ks.test(dfd, "pnorm", mean = mean(dfd), sd = sd(dfd))

#Supuesto de homocedasticidad ----
var.test(dfs,dfd)
#Verificando paso a paso las salidas de var.test

# --- Verificación manual: razón de varianzas ---
n1 <- length(dfs); n2 <- length(dfd)
s1_2 <- var(dfs); s2_2 <- var(dfd)

F_manual <- s1_2 / s2_2
df1 <- n1 - 1
df2 <- n2 - 1

p_valor_F <- 2 * min(pf(F_manual, df1, df2), 1 - pf(F_manual, df1, df2))

IC_inf_F <- (s1_2/s2_2) * (1/qf(0.975, df1, df2))
IC_sup_F <- (s1_2/s2_2) * (1/qf(0.025, df1, df2))

F_manual; df1; df2; p_valor_F
c(IC_inf_F, IC_sup_F)
# Comparar estos valores contra la salida de var.test(dfs,dfd)

# t student (se puede usar normal porque las muestras son n1=n2=100>=30 y la t student
#con muy altos grados de libertad converge a la distribucion normal)
t.test(x=dfs,y=dfd,var.equal=TRUE,paired=FALSE)
#con distribucion normal dada la ley de los grandes numeros se cumple
#que se obtiene lo mismo
z.test(x = dfs, y = dfd,
       sigma.x = sd(dfs), sigma.y = sd(dfd),
       alternative = "two.sided")
#Verificando paso a paso las salidas de t.test

# --- Verificación manual: t-test con varianza combinada (pooled) ---
media1 <- mean(dfs); media2 <- mean(dfd)

Sp2 <- ((n1-1)*s1_2 + (n2-1)*s2_2) / (n1 + n2 - 2)

t_manual <- (media1 - media2) / sqrt(Sp2 * (1/n1 + 1/n2))
df_t <- n1 + n2 - 2

p_valor_t <- 2 * (1 - pt(abs(t_manual), df_t))

IC_inf_t <- (media1 - media2) - qt(0.975, df_t) * sqrt(Sp2 * (1/n1 + 1/n2))
IC_sup_t <- (media1 - media2) + qt(0.975, df_t) * sqrt(Sp2 * (1/n1 + 1/n2))

t_manual; df_t; p_valor_t
c(IC_inf_t, IC_sup_t)
# Comparar estos valores contra la salida de t.test(dfs, dfd, var.equal=TRUE)

# --- Verificación manual: diferencia de medias con distribución normal (z) ---
SE_z <- sqrt(s1_2/n1 + s2_2/n2)

z_manual <- (media1 - media2) / SE_z

p_valor_z <- 2 * (1 - pnorm(abs(z_manual)))

IC_inf_z <- (media1 - media2) - qnorm(0.975) * SE_z
IC_sup_z <- (media1 - media2) + qnorm(0.975) * SE_z

z_manual; p_valor_z
c(IC_inf_z, IC_sup_z)
# Comparar estos valores contra la salida de z.test(x=dfs, y=dfd, ...)
# Con n1=n2=100, t_manual y z_manual deben ser prácticamente iguales

#Tamaño del efecto ----
cohen.d(dfs,dfd,paired=FALSE) # representa la distancia entre el efecto de cada grupo

#Verificar las salidas con las notas de clase

#Objetivo 2 ----
#Ho:μ(ganancias) = μ(inversion)
#Ha:μ(ganancias) != μ(inversion)

#data 2 ----
df <- read_excel("/cloud/project/datos_economia.xlsx")
df$`Tipo de cuenta` <- factor(df$`Tipo de cuenta`)

# transformacion 2 ----
dfi <- df %>%
  filter(`Tipo de cuenta`=="Inversion") %>%
  pull(Precio)
dfg <- df %>%
  filter(`Tipo de cuenta`=="Ganancia") %>%
  pull(Precio)
# Supuestos----
#1. normalidad
#Ho:X~N(μ,σ**2)
#Ha:X!~N(μ,σ**2)

# Kolmogorov-smirnov
ks.test(scale(dfi),"pnorm")
ks.test(scale(dfg),"pnorm")

#2. homocedasticidad(homogeneidad) ----
#Ho:σ**2(ganancia)=σ**2(inversion)
#Ha:σ**2(ganancia)=σ(inversion)
#prueba de Welch
var.test(dfi,dfg)
#Tarea verificar las salidas, Y ADEMAS, para probar los resultados, aplicar Bartlett, Levene y
#Fligner-Killeen, explicar en todas cuál estadistico se utiliza, su distribucion e interprete

# t-test con correlacion de Welch ----
#Ho:μ(ganancias) = μ(inversion)
#Ha:μ(ganancias) != μ(inversion)
t.test(dfi,
       dfg,
       var.equal = FALSE,
       paired=FALSE)
# verificar las salidas, los grados de libertad son los de la formula larguisima

# Tamaño del efecto 2 ----
cohen.d(dfi,
        dfg,
        paired = FALSE)
# ejercicio usar otra prueba diferente a cohen (opcional)

# Comparación de medias entre dos grupos pareados con estadística paramétrica ----

# Datos 3 (tiempos antes y después de la intervención educativa) ----
datos <- data.frame(
  estudiante = c(1:10),
  antes = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3),
  despues = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)
)
# realizar EDA tarea

# Supuestos (no se necesita homogeneidad de varianzas) ----
#1. normalidad
#Ho:X~N(μ,σ**2)
#Ha:X!~N(μ,σ**2)

# Se usa Shapiro porque los datos son n=10<=50
shapiro.test(datos$antes)
shapiro.test(datos$despues)

# diferencia entre las medias ----
#Ho:μ(despues) = μ(antes)
#Ha:μ(despues) != μ(antes)
t.test(x=datos$despues,y=datos$antes,paired=TRUE)
# verificar las salidas de t.test con teorema de la diferencia de medias para datos pareados

# --- Verificación manual: t-test pareado ---
d <- datos$despues - datos$antes

media_d <- mean(d)
sd_d <- sd(d)
n_pares <- length(d)

t_pareado_manual <- media_d / (sd_d / sqrt(n_pares))
df_pareado <- n_pares - 1

p_valor_pareado <- 2 * (1 - pt(abs(t_pareado_manual), df_pareado))

IC_inf_pareado <- media_d - qt(0.975, df_pareado) * (sd_d / sqrt(n_pares))
IC_sup_pareado <- media_d + qt(0.975, df_pareado) * (sd_d / sqrt(n_pares))

t_pareado_manual; df_pareado; p_valor_pareado
c(IC_inf_pareado, IC_sup_pareado)
# Comparar estos valores contra la salida de t.test(x=datos$despues, y=datos$antes, paired=TRUE)

# Tamaño del efecto (no es necesario porque el p-value es superior a alpha=0.05)
cohen.d(datos$despues,datos$antes,paired=TRUE)

# Comparación de dos grupos independientes con estadística no paramétrica ----
#dado que se usa el orden, se trabaja con mediana en vez de media

#Ho:mediana(fuman) = mediana(no fuman)
#Ha:mediana(fuman) != mediana(no fuman)

data("births")
df <-births
# Tarea: realizar EDA y caracterizar peso del bebe segun si la madre fuma o no

# Supuestos ----
#1. normalidad
#Ho:X~N(μ,σ**2)
#Ha:X!~N(μ,σ**2)
df %>%
  group_by(smoke) %>%
  summarise(n=length(weight),
            est_ks = ks.test(scale(weight),"pnorm")$statistic, # toma sólo el estadístico
            p_ks = ks.test(scale(weight),"pnorm")$p.value, # toma sólo el p-valor
            est_sw = shapiro.test(weight)$statistic,
            p_sw = shapiro.test(weight)$p.value,
            est_l = lillie.test(weight)$statistic,
            est_l = lillie.test(weight)$statistic)
#Cuando se aplica Lilliefors dado que existen empates en kolmogorov-smirnov, se observa que
#tan solo los datos de las mamás fumadoras pueden tener un comportamiento normal, sin embargo,
#los datos de las no fumadoras son claramente no normales, por lo tanto se debe usar una prueba
#no paramétrica (el criterio es un Y no un O)}

#2. homocedasticidad(homogeneidad) ----
#Ho:σ**2(ganancia)=σ**2(inversion)
#Ha:σ**2(ganancia)=σ(inversion)

#centrado en la media
leveneTest(weight ~ smoke,data = df,center = mean)
#centrado en la mediana
leveneTest(weight ~ smoke,data = df,center = mean)

#diferencia en las medianas
wilcox.test(weight ~ smoke, data=df)
#No hay evidencia significativa entre los pesos de los bebes de las mamas que fuman o no

#No se aplica tamaño del efecto porque no hay diferencias significativas
