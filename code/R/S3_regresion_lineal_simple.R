# ============================================================
# Semana 3
# Se deb tener R version 4.4.1
# Preparación de Stan, CmdStanR
# Inferencia Bayesiana de mu con CmdStanR
# Modelo:  Y[n] ~ Normal(mu, 1)
# Prior:   mu ~ Normal(0, 100)
# Salida final: SOLO gráficas de intervalos creíbles + estimaciones de mu
# ============================================================

version
install_cmdstan()
install.packages("bayesplot")
library(bayesplot)
library(posterior)
library(ggplot2)
library(cmdstanr)
cmdstan_version()
cmdstanr::check_cmdstan_toolchain()

###########################################
###carga de datos
url <- "https://raw.githubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/master/chap04/input/data-salary.csv"
d <- read.csv(url)

#resumen de datos
head(d)
summary(d)

# Scatterplot X vs Y
plot(d$X, d$Y,
     xlab = "Años de experiencia (X)",
     ylab = "Ingreso anual (Y)",
     main = "Experiencia vs Ingreso",
     pch = 19)
#----------------------------------------------------
#Estimación por MLE usando lm
#----------------------------------------------------
res_lm <- lm(Y ~ X, data=d)
res_lm

#Obtención de intervalos de confianza para los parametros
#Intervalos de predicción para los nuevos puntos de datos
library(ggplot2)
X_pred <- data.frame(X=0:28)
conf_95 <- predict(res_lm, X_pred, interval='confidence', level=0.95)
pred_95 <- predict(res_lm, X_pred, interval='prediction', level=0.95)
conf_50 <- predict(res_lm, X_pred, interval='confidence', level=0.50)
pred_50 <- predict(res_lm, X_pred, interval='prediction', level=0.50)
conf_95 <- data.frame(X_pred, conf_95)
pred_95 <- data.frame(X_pred, pred_95)
conf_50 <- data.frame(X_pred, conf_50)
pred_50 <- data.frame(X_pred, pred_50)


#los intervalos de confianza del ingreso anual 
#base que el modelo predice a lo largo del rango
#de experiencia laboral, desde 0 hasta 28 años
p <- ggplot() + 
  theme_bw(base_size=18) +
  geom_ribbon(data=conf_95, aes(x=X, ymin=lwr, ymax=upr), alpha=1/6) +
  geom_ribbon(data=conf_50, aes(x=X, ymin=lwr, ymax=upr), alpha=2/6) +
  geom_line(data=conf_50, aes(x=X, y=fit), linewidth=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  scale_y_continuous(breaks=seq(40, 60, 10), limits=c(32, 67)) +
  labs(x='Años de experienca (X)', y='Ingreso anual (Y)')
p

#los intervalos de predicción del ingreso anual estimado 
#por el modelo para el mismo rango de experiencia
q <- ggplot() + 
  theme_bw(base_size=18) +
  geom_ribbon(data=pred_95, aes(x=X, ymin=lwr, ymax=upr), alpha=1/6) +
  geom_ribbon(data=pred_50, aes(x=X, ymin=lwr, ymax=upr), alpha=2/6) +
  geom_line(data=pred_50, aes(x=X, y=fit), linewidth=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  scale_y_continuous(breaks=seq(40, 60, 10), limits=c(32, 67)) +
  labs(x='Años de experienca (X)', y='Ingreso anual (Y)')
q

#---------------------------------------------------
#Estiamcion por bayes
#----------------------------------------------------
# URL "raw" del archivo .stan (importante: usar raw, no el /blob/)
stan_url <- "https://raw.githdata_fileubusercontent.com/MatsuuraKentaro/Bayesian_Statistical_Modeling_with_Stan_R_and_Python/master/chap04/model/model4-4.stan"
# Descargar el código Stan y guardarlo en un archivo temporal
stan_code <- paste(readLines(stan_url, warn = FALSE), collapse = "\n")
stan_file <- file.path(tempdir(), "model4-4.stan")
writeLines(stan_code, stan_file)

# Compilar el modelo
model <- cmdstan_model(stan_file)
#Creando los datos como una lista con nombre 
#para poder pasarlos al bloque data. Aquí, 
#N representa el número total de observaciones, 
data <- list(N=nrow(d), X=d$X, Y=d$Y)
#Los datos se pasan a la función sample, y las muestras se generan
# a partir de la distribución posterior. 
# El objeto fit es un objeto de la clase CmdStanMCMC, y almacena tanto la configuración
# del algoritmo MCMC como las muestras obtenidas como resultado de la estimación.
# Por ejemplo, fit$time() devuelve el tiempo de cómputo.
fit <- model$sample(data=data, seed=123)
fit
fit$cmdstan_summary()
#Graficas de las distribuciones de cada parametro
library(coda)
dev.off()  
par(mar = c(4, 4, 2, 1))  
plot(as_mcmc.list(fit))

#Distribución conjunta 3D
library(plot3D)

d_ms <- fit$draws(format = "df")
N_ms <- nrow(d_ms)


scatter3D(d_ms$a, d_ms$b, d_ms$sigma, 
          bgvar=1:N_ms, bg=jet.col(100, 0.8), 
          bty='b2', type='p', lwd=2, phi=10, theta=30,
          col='black', pch=21,  cex=1, ticktype='detailed', colkey=FALSE,
          xlab='', ylab='', zlab='', zlim=c(0.8, 6.2))
scatter3D(d_ms$a, d_ms$b, rep(0.8, N_ms), colkey=FALSE, add=TRUE,
          type='p', lwd=2, pch=21, 
          col='black', bg='white', alpha=0.8,
          cex=1)

text3D(40, 0.15, 0.9, labels='a', add=TRUE, adj=1)
text3D(47, 0.6, 0.8, labels='b', add=TRUE, adj=1)
text3D(29, 0.3, 3.6, labels=expression(sigma), add=TRUE, adj=1)

#Densidad conjunta 2D
library(ggplot2)
library(patchwork)


d_ms <- fit$draws(format='df')
x_range <- c(32.5, 46.1)
y_range <- c(0.29, 1.11)
x_breaks <- seq(30, 50, 5)
y_breaks <- seq(0.4, 1.0, 0.2)

p_xy <- ggplot(d_ms,aes(x=a,y=b)) +
  theme_bw(base_size=18) +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  geom_point(size=2, shape=21, bg='white') +
  scale_x_continuous(breaks=x_breaks) +
  scale_y_continuous(breaks=y_breaks)

p_x <- ggplot(d_ms, aes(x=a)) +
  theme_bw(base_size=18) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  coord_cartesian(xlim = x_range) +
  geom_histogram(aes(y=..density..), colour='black', fill='white') +
  scale_x_continuous(breaks=x_breaks)

p_y <- ggplot(d_ms, aes(x=b)) +
  theme_bw(base_size=18) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  coord_flip(xlim = y_range) +
  geom_histogram(aes(y=..density..), colour='black', fill='white') +
  scale_x_continuous(breaks=y_breaks)

p <- wrap_plots(
  p_x, plot_spacer(), 
  p_xy, p_y, 
  nrow = 2,
  widths = c(1, 0.3),
  heights = c(0.3, 1)
)
p

#####Intervalos de confianza y de predicción bayesianos
Xp <- seq(0, 28, by = 1)
Np <- length(Xp)

set.seed(123)

yp_base_ms <- matrix(nrow=N_ms, ncol=Np)
yp_ms <- matrix(nrow=N_ms, ncol=Np)
for (n in 1:Np) {
  yp_base_ms[,n] <- d_ms$a + d_ms$b * Xp[n]
  yp_ms[,n] <- rnorm(n=N_ms, mean=yp_base_ms[,n], sd=d_ms$sigma)
}


qua <- apply(yp_base_ms, 2, quantile, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X=Xp, t(qua), check.names = FALSE)

p <- ggplot() +
  theme_bw(base_size=18) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`2.5%`, ymax=`97.5%`), fill='black', alpha=1/6) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), fill='black', alpha=2/6) +
  geom_line(data=d_est, aes(x=X, y=`50%`), linewidth=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  coord_cartesian(ylim = c(32, 67)) +
  scale_y_continuous(breaks=seq(40, 60, 10)) +
  labs(y='Y')
p


qua <- apply(yp_ms, 2, quantile, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X=Xp, t(qua), check.names = FALSE)

q <- ggplot() +
  theme_bw(base_size=18) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`2.5%`, ymax=`97.5%`), fill='black', alpha=1/6) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), fill='black', alpha=2/6) +
  geom_line(data=d_est, aes(x=X, y=`50%`), linewidth=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  coord_cartesian(ylim = c(32, 67)) +
  scale_y_continuous(breaks=seq(40, 60, 10)) +
  labs(y='Y')
q

#---------------------------------
#Other inference Methods
#--------------------------------
#ADVI 
fit <- model$variational(data=data, seed=123)

#MAP Estimation with L-BFGS
fit <- model$optimize(data=data, seed=123)





