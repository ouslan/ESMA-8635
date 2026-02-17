#S4-REGRESION LINEAL MULTIPLE
# Ajustar un modelo de regresión lineal múltiple para explicar 
#la proporción de compras anuales (Y) de 50 clientes de una tienda
#ínea, utilizando como variables explicativas el sexo (Sex: 0 mujer, 1 hombre) y 
#el ingreso anual (Income).
# Y representa el número de compras dividido entre 
#el número total de visitas.

#----------------------------------------------
#carga de datos
url <- "https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/master/chap05/input/data-shopping-1.csv"
d <- read.csv(url)
d <- d[, -1]
head(d)
str(d)


#---------------------------------------------
#PASO 1: CHEQUEO DE LA DISTRIBUCION DE LOS DATOS
#---------------------------------------------------

#Primero vamos a  visualizar los datos
# mediante una matriz de diagramas de dispersión 
#(scatterplot matrix). Usaremos las funciones 
#scatterplotMatrix del paquete car, o la función pairs del paquete graphics,
# el cual viene preinstalado.
# Además, se recomienda el uso de la función ggpairs del paquete GGally,
# ya que ofrece una mayor flexibilidad para la personalización de los gráficos.

install.packages("GGally")
library(ggplot2)
library(GGally)

set.seed(123)
#transforma esa columna en un factor, 
#que es la forma en que R representa 
#variables cualitativas o categóricas.
d$Sex <- as.factor(d$Sex)

N_col <- ncol(d)
ggp <- ggpairs(d, upper='blank', diag='blank', lower='blank')

#recorre las columnas del dataframe sex 
#y construye automáticamente gráficos univariados, 
#diferenciados por la variable categórica Sex,
#para colocarlos en una matriz de gráficos
for (i in 1:N_col) {
  x <- d[,i]
  p <- ggplot(data.frame(x, Sex=d$Sex), aes(x)) +
    theme_bw(base_size=14) +
    theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))
  if (class(x) == 'factor') {
    p <- p + geom_bar(aes(fill=Sex), color='grey5')
  } else {
    bw <- (max(x)-min(x))/10
    p <- p + geom_histogram(binwidth=bw, aes(fill=Sex), color='grey5') +
      geom_line(eval(bquote(aes(y=..count..*.(bw)))), stat='density')
  }
  p <- p + geom_label(data=data.frame(x=-Inf, y=Inf, label=colnames(d)[i]), aes(x=x, y=y, label=label), hjust=0, vjust=1) +
    scale_fill_manual(values=alpha(c('white', 'grey40'), 0.5))
  ggp <- putPlot(ggp, p, i, i)
}

zcolat <- seq(-1, 1, length=81)
zcolre <- c(zcolat[1:40]+1, rev(zcolat[41:81]))

install.packages("ellipse")
library(ellipse)

for (i in 1:(N_col-1)) {
  for (j in (i+1):N_col) {
    x <- as.numeric(d[,i])
    y <- as.numeric(d[,j])
    r <- cor(x, y, method='spearman', use='pairwise.complete.obs')
    zcol <- lattice::level.colors(r, at=zcolat, col.regions=grey(zcolre))
    textcol <- ifelse(abs(r) < 0.4, 'grey20', 'white')
    ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
    p <- ggplot(data.frame(ell), aes(x=x, y=y)) +
      theme_bw() + theme(
        plot.background=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border=element_blank(), axis.ticks=element_blank()
      ) +
      geom_polygon(fill=zcol, color=zcol) +
      geom_text(data=NULL, x=.5, y=.5, label=100*round(r, 2), size=6, col=textcol)
    ggp <- putPlot(ggp, p, i, j)
  }
}

for (j in 1:(N_col-1)) {
  for (i in (j+1):N_col) {
    x <- d[,j]
    y <- d[,i]
    p <- ggplot(data.frame(x, y, gr=d$Sex), aes(x=x, y=y, fill=gr, shape=gr)) +
      theme_bw(base_size=14) +
      theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))
    if (class(x) == 'factor') {
      p <- p + geom_boxplot(aes(group=x), alpha=3/6, outlier.shape=NA, fill='white') +
        geom_point(position=position_jitter(w=0.4, h=0), size=2)
    } else {
      p <- p + geom_point(size=2)
    }
    p <- p + scale_shape_manual(values=c(21, 24)) +
      scale_fill_manual(values=alpha(c('white', 'grey40'), 0.5))
    ggp <- putPlot(ggp, p, i, j)
  }
}
#imprime el gráfico
ggp

#conclusion del grafico
#Al analizar el diagrama de dispersión entre income y Y,
#se observa una relación lineal positiva: a mayor ingreso, mayor 
#es la proporción de compra,
#tanto para clientes mujeres (sex=0) como para clientes 
#hombres (sex=1)

#-----------------------------------
#PASO 2: FORMULACION DEL MODELO MATEMÁTICO
#----------------------------------
# Modelo de regresión lineal múltiple:
# Y[n] = b1 + b2 * Sex[n] + b3 * Income[n] + epsilon[n],   n = 1, ..., N
#
# donde el término de error satisface:
# epsilon[n] ~ N(0, sigma)
#
# Y      : variable respuesta
# Sex    : variable explicativa categórica
# Income : variable explicativa continua
# b1     : intercepto
# b2,b3  : coeficientes de regresión
# sigma  : desviación estándar del error

#-----------------------------------------
#PASO 3: Estimación de los parámetros usando MLE
#-------------------------------------------

# Ajustar el modelo de regresión lineal múltiple
modelo_mle <- lm(Y ~ Sex + Income, data = d)
# Resumen del modelo
summary(modelo_mle)
# Coeficientes estimados
coef(modelo_mle)
# Intervalos de confianza al 95%
confint(modelo_mle)
# Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo_mle)
par(mfrow = c(1, 1))

#Residuales vs Valores Ajustados
#Verifica linealidad y homocedasticidad.
#Idealmente:
#los puntos deben dispersarse aleatoriamente alrededor de 0,

#Q-Q Plot de los residuales
#Evalúa la normalidad de los errores
#Los puntos deberían alinearse aproximadamente 
#sobre la recta diagonal.

#Scale–Location (Spread–Location)
#Comprueba si la varianza de los errores es constante: homosedasticidad
#Busca una banda horizontal uniforme.

#----------------------------------------------
#PASO 4: Estimar parametros usando Inferencia Bayesiana
#---------------------------------------------
url <- "https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/master/chap05/input/data-shopping-1.csv"
d <- read.csv(url)
d <- d[, -1]
#Lo primero es cargar el modelo de Stan (model.stan)
library(cmdstanr)
set.seed(123)

# Antes de pasar los datos a Stan, la variable Income se divide por 100.
# Este procedimiento se conoce como escalamiento de datos y tiene como objetivo
# llevar todas las variables y parámetros a una escala similar (del orden de 1).
# El escalamiento es  útil cuando las variables originales tienen
# magnitudes muy diferentes, ya que mejora la eficiencia del muestreo MCMC,
# reduce el costo computacional y facilita la convergencia del algoritmo.
#En este caso, Income se divide por 100 porque su valor máximo es cercano a 100.
# Si el muestreo MCMC converge adecuadamente, el escalamiento no siempre es necesario,
# pero suele ser una buena práctica preventiva.

d$Income <- d$Income/100
N <- nrow(d)
data     <- list(N=N, Sex=d$Sex, Income=d$Income, Y=d$Y)

#Cargamos el modelo de Stan
stan_url <- "https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/master/chap05/model/model5-3.stan"
stan_code <- paste(readLines(stan_url, warn = FALSE), collapse = "\n")
stan_file <- file.path(tempdir(), "model5-3.stan")
writeLines(stan_code, stan_file)
# Compilar modelo
model <- cmdstan_model(stan_file)

#fit ajusta el modelo a los datos observados 
fit     <- model$sample(data=data,     seed=123, parallel_chains=4)
fit 

# Paquetes para extraer y graficar draws
library(posterior)
library(bayesplot)
library(dplyr)
library(tidyr)
library(ggplot2)
#  Extraer draws del ajuste real (fit)
draws <- fit$draws(c("lp__", "b[1]", "b[2]", "b[3]", "sigma"))
varnames <- c("lp__", "b[1]", "b[2]", "b[3]", "sigma")
# DENSIDADES (posterior)
p_dens <- mcmc_dens_overlay(draws, pars = varnames) +
  ggtitle("Densidades posteriores de parámetros (fit)") +
  labs(
    x = "Valor del parámetro",
    y = "Densidad posterior"
  ) +
  theme(
    axis.text.y  = element_text(),   # muestra números
    axis.ticks.y = element_line()    # muestra ticks
  )
graphics.off()

print(p_dens)

library(posterior)
library(dplyr)
library(ggplot2)

library(posterior)
library(tidyr)
library(ggplot2)

#Intervalos de credibilidad
varnames <- c("b[1]", "b[2]", "b[3]")
draws_df <- as_draws_df(fit$draws(varnames))
long <- draws_df |>
  pivot_longer(
    cols = all_of(varnames),
    names_to = "param",
    values_to = "value"
  )

ggplot(long, aes(y = value)) +
  geom_boxplot(width = 0.35, outlier.alpha = 0.25) +
  facet_wrap(~ param, scales = "free_y") +   
  labs(
    title = "Boxplots posteriores por parámetro",
    x = "",
    y = "Valor"
  ) +
  theme_minimal()

#------------------------
#PASO FINAL: SCATERPLOT
#------------------------
# Visualización de la distribución posterior conjunta

# En particular, se recomienda visualizar la matriz de diagramas de dispersión
# (scatterplot matrix) de las muestras MCMC, tal como se hizo anteriormente.
# Este tipo de gráfico permite examinar la distribución posterior conjunta
# de los parámetros del modelo.
#
# A partir de esta visualización se puede obtener información relevante.
# Por ejemplo, algunos coeficientes de regresión pueden presentar correlación
# negativa entre sí, mientras que parámetros como sigma y lp__ pueden no mostrar
# correlaciones fuertes con el resto de los parámetros.
#
# Si se observa algún patrón inesperado o correlaciones inusuales en la matriz
# de dispersión, esto puede indicar posibles problemas en el modelo actual
# o sugerir direcciones para mejorar su especificación.


library(dplyr)
library(ggplot2)
library(GGally)
library(hexbin)

library(dplyr)
library(ggplot2)
library(GGally)
library(lattice)
library(ellipse)
library(hexbin)
library(posterior)

# ---distribuciones conjuntas-----#
d <- fit$draws(format = "df") %>%
  select(`b[1]`, `b[2]`, `b[3]`, sigma) %>%
  as.data.frame()

N_col <- ncol(d)
ggp <- ggpairs(d, upper = "blank", diag = "blank", lower = "blank")

# --- Diagonal: histogramas + curva ---
for (i in 1:N_col) {
  x <- d[, i]
  bw <- (max(x) - min(x)) / 10
  
  p <- ggplot(data.frame(x), aes(x)) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
    geom_histogram(binwidth = bw, fill = "white", color = "grey5") +
    geom_line(eval(bquote(aes(y = ..count.. * .(bw)))), stat = "density") +
    geom_label(
      data = data.frame(x = -Inf, y = Inf, label = colnames(d)[i]),
      aes(x = x, y = y, label = label),
      hjust = 0, vjust = 1
    )
  
  ggp <- putPlot(ggp, p, i, i)
}

# --- Colores para correlación ---
zcolat <- seq(-1, 1, length = 81)
zcolre <- c(zcolat[1:40] + 1, rev(zcolat[41:81]))

# --- Triángulo superior: elipse con rho (Spearman) ---
for (i in 1:(N_col - 1)) {
  for (j in (i + 1):N_col) {
    x <- as.numeric(d[, i])
    y <- as.numeric(d[, j])
    
    r <- cor(x, y, method = "spearman", use = "pairwise.complete.obs")
    zcol <- lattice::level.colors(r, at = zcolat, col.regions = grey(zcolre))
    textcol <- ifelse(abs(r) < 0.4, "grey20", "white")
    
    ell <- ellipse::ellipse(
      r, level = 0.95, type = "l", npoints = 50,
      scale = c(.2, .2), centre = c(.5, .5)
    )
    
    p <- ggplot(data.frame(ell), aes(x = x, y = y)) +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()
      ) +
      geom_polygon(fill = zcol, color = zcol) +
      geom_text(x = .5, y = .5, label = 100 * round(r, 2), size = 6, col = textcol)
    
    ggp <- putPlot(ggp, p, i, j)
  }
}

# --- Triángulo inferior: hexbin ---
for (j in 1:(N_col - 1)) {
  for (i in (j + 1):N_col) {
    x <- d[, j]
    y <- d[, i]
    
    p <- ggplot(data.frame(x, y), aes(x = x, y = y)) +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      geom_hex() +
      scale_fill_gradientn(colours = gray.colors(7, start = 0.1, end = 0.9))
    
    ggp <- putPlot(ggp, p, i, j)
  }
}

ggp


