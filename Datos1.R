#Librería ----
library(readxl)
library(tidyverse)
library(effsize) # para cohen.d
library(openintro)
library(nortest)
library(car) # para leveneTest
library(moments)
library(GGally)
library(rstatix)
library(coin)

#EDA (minimo) ----
df <- read_excel("~/Datos_Medicos/Data/datos_economia.xlsx")
df$`Tipo de cuenta` <- factor(df$`Tipo de cuenta`)

summary(df)

df %>%
  group_by(`Tipo de cuenta`) %>%
  summarise(n = length(Precio),
            prom = mean(Precio),
            ds = sd(Precio),
            mediana = median(Precio),
            RIC = IQR(Precio),
            min = min(Precio),
            max = max(Precio),
            curtosis = kurtosis(Precio),
            Coef_asim = skewness(Precio))
#Ganancia: Se registraron un total de 100 mediciones de precio en cuentas de ganancia.
#En promedio, el precio es de aproximadamente $778.491 (con una desviación estándar de
#$193.397). Por otro lado, el 50% de las cuentas registran un precio de a lo sumo $754.834,
#con un rango intercuartílico de $253.790, siendo el valor más bajo registrado de $389.351
#y el más alto de $1.448.208. Si visualizamos los datos, su forma es ligeramente leptocúrtica
#(curtosis = 3.66). Asimismo se evidencia una distribución algo más puntiguada que lo normal,
#con colas más pesadas. Además, con un coeficiente de asimetría de 0.639, se observa una asimetría
#o sesgo moderado hacia la derecha, lo que indica la presencia de algunas cuentas de ganancia
#con precios relativamente altos respecto a la mayoría.

#Inversion: Se registraron un total de 100 mediciones de precio en cuentas de inversión.
#En promedio, el precio es de aproximadamente $5.090.406 (con una desviación estándar de
#$912.816). Por otro lado, el 50% de las cuentas registran un precio de a lo sumo $5.061.756,
#con un rango intercuartílico de $1.185.673, siendo el valor más bajo registrado de $2.690.831
#y el más alto de $7.187.333. Si visualizamos los datos, su forma es ligeramente platicúrtica
#(curtosis = 2.84), lo que evidencia una distribución algo más aplanada que lo normal. Además,
#con un coeficiente de asimetría muy cercano a cero (0.605), la distribución es prácticamente
#simétrica, sin un sesgo relevante hacia ninguno de los dos extremos.

df %>%
  ggplot(aes(x = `Tipo de cuenta`, y = Precio)) +
  geom_boxplot() +
  labs(title = "Distribucion de Precio segun Tipo de cuenta")
#El boxplot muestra que el precio de las cuentas de inversión es consistentemente mucho
#más alto que el de las cuentas de ganancia, con una mediana cercana a $5.06 millones frente
#a apenas $800 mil, lo cual es coherente con la naturaleza de cada tipo de cuenta (una
#inversión acumula capital, mientras que una ganancia refleja un monto puntual). La
#dispersión también es mucho mayor en inversión que en ganancia,
#lo que indica más variabilidad entre las cuentas de ese grupo. Ambas cajas lucen
#razonablemente simétricas, consistente con los coeficientes de asimetría calculados previamente.

#Separar los dos grupos ----
dfi <- df %>% filter(`Tipo de cuenta` == "Inversion") %>% pull(Precio)
dfg <- df %>% filter(`Tipo de cuenta` == "Ganancia")  %>% pull(Precio)

#Supuestos ----
#1. Normalidad
#Ho: X ~ N(mu, sigma^2)
#Ha: X !~ N(mu, sigma^2)
shapiro.test(dfi)
shapiro.test(dfg)

#2. Homocedasticidad (homogeneidad de varianzas)
#Ho: sigma^2(Ganancia) = sigma^2(Inversion)
#Ha: sigma^2(Ganancia) != sigma^2(Inversion)

#F de Fisher (dos grupos, asume normalidad)
var.test(dfi, dfg)

#Bartlett: estadistico Chi-cuadrado, muy sensible a no-normalidad
bartlett.test(list(dfi, dfg))

#Levene: estadistico F, usa desviaciones absolutas respecto a la media (mas robusto que Bartlett ante no-normalidad)
leveneTest(Precio ~ `Tipo de cuenta`, data = df)

#Fligner-Killeen: no parametrico, estadistico Chi-cuadrado, el mas robusto ante outliers/no-normalidad
fligner.test(list(dfi, dfg))

#t-test con correccion de Welch ----
t.test(dfi, dfg, var.equal = FALSE, paired = FALSE)

#Tamano del efecto ----
cohen.d(dfi, dfg, paired = FALSE)

#diferencia de medianas con estadistica no parametrica ----
data('births')
df <- births

#4.3 Peso al nacer: decidir si es valido usar t-test en vez de Mann-Whitney ----

#Supuesto de normalidad por grupo ----
#Ho: X ~ N(mu, sigma^2)
#Ha: X !~ N(mu, sigma^2)
df %>%
  group_by(smoke) %>%
  summarise(n = length(weight),
            est_ks = ks.test(weight)$statistic,
            p_ks = ks.test(weight)$p.value,
            est_sw = shapiro.test(weight)$statistic,
            p_sw = shapiro.test(weight)$p.value,
            est_l = lillie.test(weight)$statistic,
            p_l = lillie.test(weight)$p.value)


df %>%
  group_by(`smoke`) %>%
  summarise(n = length(weight),
            prom = mean(weight),
            ds = sd(weight),
            mediana = median(weight),
            RIC = IQR(weight),
            min = min(weight),
            max = max(weight),
            curtosis = kurtosis(weight),
            Coef_asim = skewness(weight))
#Shapiro-Wilk rechaza normalidad en AMBOS grupos (p < 0.001)

#Homogeneidad de varianzas ----
#Ho: sigma^2(smoker) = sigma^2(nonsmoker)
leveneTest(weight ~ smoke, data = df, center = median)
#No se rechaza homocedasticidad (p > 0.05): las varianzas son iguales

#DECISION: aunque la normalidad no se cumple estrictamente, n1=100 y n2=50
#son muestras suficientemente grandes (regla practica n>=30) para la robustez
#al t-test frente a la falta de normalidad. Sumando a que SI hay homogeneidad
#de varianzas, es válido usar el t-test. Por eso se corre y se compara contra
#Mann-Whitney (Wilcoxon), en vez de descartarlo de entrada.

#Prueba parametrica: t-test de Welch ----
#Ho: mu(smoker) = mu(nonsmoker)
t.test(weight ~ smoke, data = df, var.equal = FALSE)

#Prueba no parametrica: Mann-Whitney (Wilcoxon) ----
#Ho: mediana(smoker) = mediana(nonsmoker)
wilcox.test(weight ~ smoke, data = df)

#Comparacion: ambas pruebas dan p-valores muy similares (~0.13-0.14) y
#llegan a la MISMA conclusion (no se rechaza Ho, no hay diferencia
#significativa). Esta coincidencia respalda que, pese a la no-normalidad,
#el t-test funciono de forma robusta gracias al tamano muestral.

#Tamano del efecto (no parametrico, coherente con Wilcoxon) ----
cliff.delta(weight ~ smoke, data = df)



#Estadística no parametrica pareado----
#url_dat <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQaVafuOSuEnOIiJJoB_OLF6GHib4EGqtAPnFBkNXFj29iB8yex4wYXYAAyIW16eA/pub?gid=1616716040&single=true&output=tsv"

df <- read.delim("~/Metodos_estadisticos/Data/Resultados experimento.xlsx - Datos_ComOrg.tsv")

summary(df)
df %>%
  summarise(across(everything(), ~sum(is.na(.)), .names = "NA_{.col}"))

df$Experimento <- factor(data$Experimento)
df$Genero <- factor(df$Genero)
df$Nivel.educativo <- factor(df$Nivel.educativo)


df %>%
  group_by(Experimento) %>%
  summarise(n = length(Q1),
            est_ks = ks.test(scale(Q1),'pnorm')$statistic,
            p_ks = ks.test(scale(Q1),'pnorm')$p.value,
            est_sw = shapiro.test(Q1)$statistic,
            p_sw = shapiro.test(Q1)$p.value,
            est_l = lillie.test(Q1)$statistic,
            p_l = lillie.test(Q1)$p.value)

pretestq1 <- df %>%
  select(Experimento, Q1) %>%
  filter(Experimento == 'Pretest')

posttestq1 <- df %>%
  select(Experimento, Q1) %>%
  filter(Experimento == 'Post-test')

wilcox.test(x = pretestq1$Q1, y = posttestq1$Q1, paired = TRUE)

#La prueba de rango del signo de Wilcoxon nos muestra...

#Con una confianza del 95%, podemos observar que hay diferencias estadísticamente significativas de
#la pregunta de percepción Q1 según el Experimento (v = 0, p-valor < 0.001)

df %>%
  rstatix::wilcox_effsize(Q1 ~ Experimento, paired = TRUE)
