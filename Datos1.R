#Librería ----
library(readxl)
library(tidyverse)
library(effsize) # para cohen.d
library(openintro)
library(nortest)
library(car) # para leveneTest


#EDA reducido ----
df <- read_excel("~/Metodos_estadisticos/Data/datos_economia.xlsx")
df$`Tipo de cuenta` <- factor(df$`Tipo de cuenta`)

head(df, 5)
str(df)
# datos faltantes
sum(is.na(df))

#Univariado: Precio (numerica) ----
df %>%
  summarise(n = length(Precio),
            media = mean(Precio),
            sd = sd(Precio),
            mediana = median(Precio),
            RIC = IQR(Precio),
            Q1 = quantile(Precio, 0.25),
            Q3 = quantile(Precio, 0.75),
            min = min(Precio),
            max = max(Precio))

df %>%
  ggplot(aes(x = Precio)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#8ecae6", color = "white") +
  geom_density(color = "#023047", linewidth = 1.1) +
  theme_bw()

df %>%
  ggplot(aes(x = "", y = Precio)) +
  geom_boxplot(fill = "#8ecae6", color = "#023047",
               outlier.color = "darkred") +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 3, color = 'black') +
  theme_bw()

#Univariado: Tipo de cuenta (categorica) ----
df %>%
  count(`Tipo de cuenta`, name = "n") %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2))

df %>%
  ggplot(aes(x = `Tipo de cuenta`)) +
  geom_bar(fill = "#008B8B", width = 0.6) +
  theme_bw()

#Bivariado: Precio segun Tipo de cuenta ----
df %>%
  group_by(`Tipo de cuenta`) %>%
  summarise(n = length(Precio),
            media = mean(Precio),
            sd = sd(Precio),
            mediana = median(Precio),
            RIC = IQR(Precio),
            min = min(Precio),
            max = max(Precio))

df %>%
  ggplot(aes(x = `Tipo de cuenta`, y = Precio)) +
  geom_boxplot(fill = "#8ecae6", color = "#023047",
               outlier.color = "darkred") +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 3, color = 'black') +
  theme_bw()

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
            est_sw = shapiro.test(weight)$statistic,
            p_sw = shapiro.test(weight)$p.value,
            est_l = lillie.test(weight)$statistic,
            p_l = lillie.test(weight)$p.value)
#Shapiro-Wilk rechaza normalidad en AMBOS grupos (p < 0.001)

#Homogeneidad de varianzas ----
#Ho: sigma^2(smoker) = sigma^2(nonsmoker)
leveneTest(weight ~ smoke, data = df, center = median)
#No se rechaza homocedasticidad (p > 0.05): las varianzas son iguales

#DECISION: aunque la normalidad no se cumple estrictamente, n1=100 y n2=50
#son muestras suficientemente grandes (regla practica n>=30) para que el
#Teorema del Limite Central haga razonablemente robusto al t-test frente a
#la falta de normalidad. Sumado a que SI hay homogeneidad de varianzas,
#es defendible usar el t-test como aproximacion valida. Por eso se corre
#y se compara contra Mann-Whitney (Wilcoxon), en vez de descartarlo de entrada.

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

