#Librerías ----
library(tidyverse)
library(moments)

#Datos----
diferencia <- c(0.71, 0.67, 1.98, 1.61, 0.67, 1.48, 0.25, 1.44, 1.06, 0.95)
errores <- c(12, 10, 4, 2, 6, 5, 16, 3, 4, 8)
df <- data.frame(diferencia, errores)




graf_dispersion <- function(datos, var_x, var_y) {
  datos %>%
    ggplot(aes(x = .data[[var_x]], y = .data[[var_y]])) +
    geom_point(alpha = 0.4, color = 'darkblue') +
    theme_bw()
}



df %>%
  summarise(
    n = n(),
    prom_diferencia = mean(diferencia),
    ds_diferencia = sd(diferencia),
    mediana_diferencia = median(diferencia),
    RIC_diferencia = IQR(diferencia),
    min_diferencia = min(diferencia),
    max_diferencia = max(diferencia),
    curtosis_diferencia = kurtosis(diferencia),
    asim_diferencia = skewness(diferencia),
  )

df %>%
  summarise(
    prom_errores = mean(errores),
    ds_errores = sd(errores),
    mediana_errores = median(errores),
    RIC_errores = IQR(errores),
    min_errores = min(errores),
    max_errores = max(errores),
    curtosis_errores = kurtosis(errores),
    asim_errores = skewness(errores),
  )


graf_dispersion(df$errores, df$diferencia)
