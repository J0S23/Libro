library(tidyverse)

dosis  <- c(1.2, 1.3, 1, 1.4, 1.5, 1.8, 1.2, 1.3, 1.4, 1.3)
tiempo <- c(25, 28, 40, 38, 10, 9, 27, 30, 16, 18)

# 1) Primero armas el dataframe a partir de los vectores
datos <- data.frame(dosis = dosis, tiempo = tiempo)

# 2) La funcion ahora recibe el dataframe como parametro (en vez de
#    depender de un "df" que no existia), y var_x/var_y siguen siendo
#    nombres de columna como texto
graf_dispersion <- function(datos, var_x, var_y) {
  datos %>%
    ggplot(aes(x = .data[[var_x]], y = .data[[var_y]])) +
    geom_point(alpha = 0.4, color = 'darkblue') +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
    theme_bw()
}

# 3) La llamada: se pasa el dataframe, y los nombres de columna ENTRE COMILLAS
graf_dispersion(datos, "dosis", "tiempo")

x <- mean(dosis)
y <- mean(tiempo)

sum_xy <- sum(dosis*tiempo)
sum_x_cua <- sum(dosis^2)

numerador <- sum_xy-10*x*y
denominador <- sum_x_cua-10*(x^2)
B1 <- numerador/denominador
B0 <- y-B1*x

y_gorro <- B0+B1x




#librerias ----
library(tidyverse)
library(nortest)


#Datos -car#Datos ----
df <- read.delim("C:/Users/Acer/Downloads/data_rls.txt")


#EDA ----
summary(df)
df %>%
  summarise(n = length(Resistencia),
            prom = mean(Resistencia),
            ds = sd(Resistencia),
            mediana = median(Resistencia),
            RIC = IQR(Resistencia),
            min = min(Resistencia),
            max = max(Resistencia))

#Gráfico de dispersión

df %>%
  ggplot(aes(x = Edad, y = Resistencia))+
  geom_point()+
  theme_bw()


#Correlación ----
ks.test(scale(df$Edad),'pnorm')
ks.test(scale(df$Resistencia),'pnorm')

shapiro.test(df$Edad)
shapiro.test(df$Resistencia)

lillie.test(df$Edad)
lillie.test(df$Resistencia)

cor(df$Edad, df$Resistencia)

#Se busca errror esperado = 0, normalidad, varianzas constantes (según google,
#homocedasticidad), independencia


# Estimacion paso a paso de beta_0 y beta_1
x = df$Edad
y = df$Resistencia
n = length(x)

xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
sxy = sum((x-xbar)*(y-ybar))

#Estimacion de beta_0 y beta_1
beta_1 = sxy/sxx
beta_0 = ybar - beta_1*xbar

#Funcion para rls

modelo <- lm(Resistencia ~ Edad, data = df)
modelo

#Beta1 es pendiente y Beta0 es el intercepto

#Hallemos sigma2
x = df$Edad
y = df$Resistencia

#Coeficientes estimados
beta_0 = modelo$coefficients[1]
beta_1 = modelo$coefficients[2]

# valores ajustados y residuo
yhat = beta_0 + beta_1*x
e = y - yhat

#Hallemos SSE
sse = sum(e^2)

#Otra forma
error = modelo$residuals
sse = sum(error^2)

#Calculemos MSE
n = nrow(df)
sigma2 = MSE = sse/(n-2)

#Calculamos RMSE (RSE)
rse = sqrt(sigma2)


#Hallemos el coeficiente de determinación -----
#Variables
x = df$Edad
y = df$Resistencia
n = nrow(df)

#medias

xbar = mean(x)
ybar = mean(y)

#Calculemos Sxx, Sxy
SST = sum((y-ybar)^2)
Sxx = sum((x-xbar)^2)
Sxy = sum((x-xbar)*(y-ybar))
beta_1 = Sxy/Sxx

#R^2
R2 = (beta_1*Sxy)/SST

#Halla SSE y calculo despues R2
yhat = beta_0 + beta_1*x
e = y - yhat
SSE = sum(e^2)
R2 = (SST-SSE)/SST

#Coeficiente de determinación
cor(df$Edad, df$Resistencia)^2

#Coeficiente de determinación con la función lm
summary(modelo)$r.squared)
summary(modelo)
