#Librería ----
library(readxl)
library(moments)
library(tidyverse)
library(effsize)
library(openintro)
library(nortest)
library(car)
library(coin)
#data----
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
  summarise(n= length(Valores),
            prom = mean(Valores),
            ds = sd(Valores),
            mediana = median(Valores),
            RIC = IQR(Valores),
            min = min(Valores),
            max = max(Valores),
            curtosis = kurtosis(Valores),
            Coef_asim = skewness(Valores))
#Presión Sistólica: Se registraron un total de 100 mediciones de presión sistólica.
#En promedio, la presión sistólica es de aproximadamente 121 mmHg (con una desviación
#estándar de 9.13 mmHg). Por otro lado, el 50% de los pacientes registran una presión sistólica
#de a lo sumo 121 mmHg, con un rango intercuartílico de 11.9 mmHg, siendo el valor más bajo
#registrado de 96.9 mmHg y el más alto de 142 mmHg. Si visualizamos los datos, su forma es
#ligeramente platicúrtica (curtosis = 2.84, menor a 3), es decir, se evidencia una distribución
#algo más aplanada que la normal, con colas más livianas. Además, con un coeficiente de
#asimetría muy cercano a cero (0.0605), la distribución es prácticamente simétrica,
#sin un sesgo relevante hacia ninguno de los dos extremos.

#Presión Diastólica: Se registraron un total de 100 mediciones de presión diastólica. En promedio,
#la presión diastólica es de aproximadamente 78.9 mmHg (con una desviación estándar de 9.48 mmHg).
#Por otro lado, el 50% de los pacientes registran una presión diastólica de a lo sumo 77.8 mmHg,
#con un rango intercuartílico de 12.4 mmHg, siendo el valor más bajo registrado de 59.9 mmHg y el
#más alto de 112 mmHg. Si visualizamos los datos, su forma es ligeramente leptocúrtica
#(curtosis = 3.66, mayor a 3), es decir, se evidencia una distribución algo más apuntada que la
#normal, con colas más pesadas. Además, con un coeficiente de asimetría de 0.639, se observa una
#asimetría o sesgo moderado hacia la derecha, lo que indica la presencia de algunos valores de
#presión diastólica relativamente altos respecto a la mayoría de los pacientes.

#Visualización del EDA de Presión
df %>%
  ggplot(aes(x = Presion, y = Valores))+
  geom_boxplot()+
  labs(title = "Distribución de presión sistólica y diastólica")


#El boxplot muestra que la presión sistólica es consistentemente más alta que la diastólica,
#con una mediana cercana a 120 mmHg frente a 78 mmHg, lo cual es fisiológicamente esperado.
#La dispersión (rango intercuartílico) es similar entre ambos grupos, sin diferencias marcadas
#en variabilidad. Se observa un valor atípico en cada grupo: uno alto en la diastólica
#(~112 mmHg) y uno bajo en la sistólica (~98 mmHg), que podrían corresponder a casos particulares
#a revisar. Ambas cajas lucen razonablemente simétricas, consistente con los coeficientes de
#asimetría calculados previamente.

# supuesto de normalidad ----
# (x-media)/ds
#Ho:x ~ N(miu,sigma**2)
#Ha:x !~ N(miu,sigma**2)
ks.test(dfs, "pnorm", mean = mean(dfs), sd = sd(dfs))
ks.test(dfd, "pnorm", mean = mean(dfd), sd = sd(dfd))

#Supuesto de homocedasticidad ----
var.test(dfs,dfd)

# Verificación manual: razón de varianzas ----
n1 <- length(dfs)
n2 <- length(dfd)
s1_2 <- var(dfs)
s2_2 <- var(dfd)

F_manual <- s1_2 / s2_2
df1 <- n1 - 1
df2 <- n2 - 1

p_valor_F <- 2 * min(pf(F_manual, df1, df2), 1 - pf(F_manual, df1, df2))

IC_inf_F <- (s1_2/s2_2) * (1/qf(0.975, df1, df2))
IC_sup_F <- (s1_2/s2_2) * (1/qf(0.025, df1, df2))

F_manual; df1; df2; p_valor_F
c(IC_inf_F, IC_sup_F)

# t student (se puede usar normal porque las muestras son n1=n2=100>=30 y la t student
#con muy altos grados de libertad converge a la distribucion normal)
t.test(x=dfs,y=dfd,var.equal=TRUE,paired=FALSE)

# Verificación manual: t-test con varianza combinada ----

media1 <- mean(dfs)
media2 <- mean(dfd)

Sp2 <- ((n1-1)*s1_2 + (n2-1)*s2_2) / (n1 + n2 - 2)

t_manual <- (media1 - media2) / sqrt(Sp2 * (1/n1 + 1/n2))
df_t <- n1 + n2 - 2

p_valor_t <- 2 * (1 - pt(abs(t_manual), df_t))

IC_inf_t <- (media1 - media2) - qt(0.975, df_t) * sqrt(Sp2 * (1/n1 + 1/n2))
IC_sup_t <- (media1 - media2) + qt(0.975, df_t) * sqrt(Sp2 * (1/n1 + 1/n2))

t_manual
df_t
p_valor_t
c(IC_inf_t, IC_sup_t)

#con distribucion normal dada la ley de los grandes numeros se cumple
#que se obtiene lo mismo
z.test(x = dfs, y = dfd,
       sigma.x = sd(dfs), sigma.y = sd(dfd),
       alternative = "two.sided")

# Verificación manual: diferencia de medias con distribución normal (z) ----
SE_z <- sqrt(s1_2/n1 + s2_2/n2)

z_manual <- (media1 - media2) / SE_z

p_valor_z <- 2 * (1 - pnorm(abs(z_manual)))

IC_inf_z <- (media1 - media2) - qnorm(0.975) * SE_z
IC_sup_z <- (media1 - media2) + qnorm(0.975) * SE_z

z_manual
p_valor_z
c(IC_inf_z, IC_sup_z)

#Tamaño del efecto ----
cohen.d(dfs,dfd,paired=FALSE) # representa la distancia entre el efecto de cada grupo

#Verificar las salidas con las notas de clase

#Objetivo 2 ----
#Ho:μ(ganancias) = μ(inversion)
#Ha:μ(ganancias) != μ(inversion)

#data 2 ----
df <- read_excel("/cloud/project/Data/datos_economia.xlsx")
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

#Supuesto de normalidad ----
shapiro.test(datos$antes)
shapiro.test(datos$despues)

#Diferencia entre las medias ----
#Ho:μ(despues) = μ(antes)
#Ha:μ(despues) != μ(antes)
t.test(x = datos$despues,
       y = datos$antes,
       paired = TRUE)

# Verificación manual: t-test pareado ----
d <- datos$despues - datos$antes

media_d <- mean(d)
sd_d <- sd(d)
n_pares <- length(d)

t_pareado_manual <- media_d / (sd_d / sqrt(n_pares))
df_pareado <- n_pares - 1

p_valor_pareado <- 2 * (1 - pt(abs(t_pareado_manual), df_pareado))

IC_inf_pareado <- media_d - qt(0.975, df_pareado) * (sd_d / sqrt(n_pares))
IC_sup_pareado <- media_d + qt(0.975, df_pareado) * (sd_d / sqrt(n_pares))

t_pareado_manual
df_pareado
p_valor_pareado
c(IC_inf_pareado, IC_sup_pareado)

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
df <- read.delim("/cloud/project/Data/dataQ.tsv")
head(df)
df <- df %>%
  mutate(across(c(Experimento, Genero,`Nivel.educativo`), as.factor))

#Resumen
df %>%
  group_by(Experimento) %>%
  summarise(n= length(Q1),
            prom = mean(Q1),
            ds = sd(Q1),
            mediana = median(Q1),
            RIC = IQR(Q1),
            min = min(Q1),
            max = max(Q1))
#Supuesto de normalidad ----
df %>%
  group_by(Experimento) %>%
  summarise(n = length(Q1),
            est_ks = ks.test(scale(Q1), "pnorm")$statistic,
            p_ks = ks.test(scale(Q1), "pnorm")$p.value,
            est_sw = shapiro.test(Q1)$statistic,
            p_sw = shapiro.test(Q1)$p.value,
            est_l = lillie.test(Q1)$statistic,
            p_l = lillie.test(Q1)$p.value)
#wilcox asume que los datos no son pareados
pretestq1 <- df %>%
  select(Experimento,Q1) %>%
  filter(Experimento == "Pretest")

posttestq1 <- df %>%
  select(Experimento,Q1) %>%
  filter(Experimento == "Post-test")

wilcox.test(x=pretestq1$Q1, y = posttestq1$Q1, paired=TRUE)
#La prueba de rango del signo de Wilcoxon nos muestra que con una confianza del
#95%, podemos decir que hay diferencias estadísticamente significativas entre
#las percepciones de Q1 antes y después del experimento (V=0, p-valor<0.001).

df %>%
  rstatix::wilcox_effsize(Q1 ~ Experimento, paired=TRUE)
# Con una confianza del 95% se concluye que el tamaño del efecto es grande

