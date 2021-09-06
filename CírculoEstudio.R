###############################################################################
###################### DESCARGAR BASE DE Github #######################
###############################################################################
url_github = "https://raw.githubusercontent.com/DillanAS/CirculoEstudio_EstadisticaConR/main/RestaurantVisitors.csv"
df <- read.csv(url_github, header = TRUE)



# Explorar de forma general el dataframe
class(df)
names(df)
View(df)
lapply(df, is.null)

# Revisar el tipo (class) de nuestras variables
lapply(df, class)

###############################################################################
####################### ANÁLISIS EXPLOTATORIO DE DATOS ########################
###############################################################################
summary(df)

#### HISTOGRAMAS
library(ggplot2)
ggplot(data = df, aes(x = spending)) + geom_histogram()  +
  xlab("Total spending") + ylab("Frequency") +
  ggtitle("Total Spending Histogram")

ggplot(data = df) + 
  geom_histogram(aes(x=spending,fill=factor(mkt_strategy)),alpha = 0.5)  +
  xlab("Total spending") + ylab("Frequency") +
  ggtitle("Total Spending Histogram by Strategy")

#### Boxplots
ggplot(data = df) + geom_boxplot(aes(x=total))  +
  xlab("") + ggtitle("Total visitors Boxplot")

ggplot(data = df) + geom_boxplot(aes(x=factor(mkt_strategy), y=total)) + 
  coord_flip()  +
  ylab("") + xlab("Marketing strategy") +
  ggtitle("Total visitors boxplot by Strategy")

ggplot(data = df) + geom_boxplot(aes(x=spending, y=spending))  +
  xlab("") + ggtitle("Total spending Boxplot")

ggplot(data = df) + geom_boxplot(aes(x=factor(mkt_strategy), y=spending)) +
  coord_flip()  +
  ylab("") + xlab("Marketing strategy") +
  ggtitle("Total spending boxplot by Strategy")

#### Gráficas de dispersión
ggplot(data = df) + 
  geom_point(aes(total, spending)) +
  xlab("Total visitors") + ylab("Total spending") +
  ggtitle("Total Spending by visitors")

ggplot(data = df) + 
  geom_point(aes(total, spending, colour = factor(holiday))) +
  xlab("Total visitors") + ylab("Total spending") +
  ggtitle("Total Spending by visitors")

ggplot(data = df) + 
  geom_point(aes(total, spending, colour = factor(mkt_strategy))) +
  xlab("Total visitors") + ylab("Total spending") +
  ggtitle("Marketing strategy impact")

ggplot(data = df) + 
  geom_point(aes(total, spending, colour = factor(mkt_strategy))) +
  facet_grid(~holiday)  +
  xlab("Total visitors") + ylab("Total spending") +
  ggtitle("Marketing strategy impact in holidays")

###############################################################################
######################### PROBABILIDAD E INFERENCIA ###########################
###############################################################################

##### CÁLCULO DE PROBABILIDADES
# ¿Cual es la probabilidad de que un día lleguen menos de 98 clientes en total?
# Prob(Clientes < 98)
mean_clientes <- mean(df$total)
sd_clientes <- sd(df$total)
pnorm(98, mean = mean_clientes, sd = sd_clientes, lower.tail = TRUE)

# ¿Cual es la probabilidad de que un día lleguen al menos 220 clientes en total?
# Prob(Clientes > 220)
pnorm(220, mean = mean_clientes, sd = sd_clientes, lower.tail = FALSE)

# ¿Cuál es la probabilidad de que el gasto total en un día esté entre 1,250,000
# y 2,000,000?
# P(1,250,000 < Gasto < 2,000,000)
mean_spending = mean(df$spending)
sd_spending = sd(df$spending)
p_sup <- pnorm(2000000, mean = mean_spending, sd = sd_spending, lower.tail = TRUE) 
p_inf <- pnorm(1250000, mean = mean_spending, sd = sd_spending, lower.tail = TRUE)
p_sup - p_inf


# ¿Cuál es la probabilidad de que el gasto promedio en un día sea menor a 1,215,790?
# P(GastoPromedio < 1,215,790)
se_spending = sd_spending/sqrt(478)
pnorm(1215790, mean = mean_spending, sd = se_spending, lower.tail = TRUE)

# ¿Cuál es la probabilidad de que en un día lleguen en promedio más de 135 clientes?
# P(ClientesPromedio > 135)
se_clientes = sd_clientes/sqrt(478)
pnorm(135, mean = mean_clientes, sd = se_clientes, lower.tail = FALSE)

# ¿Cómo evaluar la estrategia de mercadotécnica en las ventas?
# Si la estrategia fue exitosa, el gasto debería ser mayor
# PASO 1: Planteamiento de hipotesis:
# H_nula: GastoProm_ConEstrategia <= GastoProm_SinEstrategia
# H_alt: GastoProm_ConEstrategia > GastoProm_SinEstrategia

# PASO 2: Calcular estadístico de prueba:
# Definamos m1 como GastoPromedio_ConEstrategia y m2 como GastoPromedio_SinEstrategia
m1 <- mean(df[df$mkt_strategy == 1, "spending"])
m2 <- mean(df[df$mkt_strategy == 0, "spending"])

var1 <- var(df[df$mkt_strategy == 1, "spending"])
var2 <- var(df[df$mkt_strategy == 0, "spending"])

n1 <- length(df[df$mkt_strategy == 1, "spending"])
n2 <- length(df[df$mkt_strategy == 0, "spending"])

t <- (m1-m2-0)/(sqrt(((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))*sqrt(1/n1+1/n2))
gl <- n1 + n2 - 2

# PASO 3: Calcular P-Value
pvalue <- pt(t, df = gl, lower.tail = FALSE)

# PASO 4: Seleccionar nivel de confianza y concluir
# Usualmente se definen niveles de significancia estándar: 0.1, 0.05 o 0.01
# Si Pvalue < significancia, se rechaza H_nula

# Forma directa:
t.test(x = df[df$mkt_strategy == 1, "spending"], y = df[df$mkt_strategy == 0, "spending"],
       alternative = "greater",
       mu = 0, paired = FALSE, var.equal = TRUE)

# ¿Existe evidencia estadística para concluir que, en promedio, las visitas en
# la tienda 1 son menores a las de la tienda 2?
t.test(x = df$rest1, y = df$rest2,
       alternative = "less",
       mu = 0, paired = FALSE, var.equal = TRUE)

###############################################################################
############################## REGRESIÓN LINEAL ###############################
###############################################################################

# Matriz de dispersión para analizar la relación lineal entre las variables
attach(df)
pairs(~ spending + mkt_strategy + holiday + total, data = df, gap = 0.4, cex.labels = 1.5)

# Estmación del modelo lineal (OLS)
model1 <- lm(spending ~ mkt_strategy + holiday + total)
summary(model1)

# Análisis de valores obs vs ajustados
plot(model1$fitted.values, spending, xlab = "Valores ajustados", ylab = "Spending")
abline(lsfit(model1$fitted.values, spending))

# Análisis de residuos
StanRes <- rstandard(model1)
par(mfrow = c(2, 2))
plot(total, StanRes, ylab = "Residuales Estandarizados")
plot(holiday, StanRes, ylab = "Residuales Estandarizados")
plot(mkt_strategy, StanRes, ylab = "Residuales Estandarizados")
qqnorm(StanRes)
qqline(StanRes)
dev.off()

###############################################################################
########################## SERIES DE TIEMPO Y PREDICCIÓN ######################
###############################################################################
spending.ts <- ts(df$spending, start = 1, freq = 12)
plot(spending.ts, xlab = "Time", ylab = "Total Spending")
title(main = "Total spending Time Series")

# Podemos observar que la series no es estacionaria. Obtenemos la primera 
# diferencia para hacerla estacionaria
plot(diff(spending.ts), xlab = "Time", ylab = "Change in Spending")
title(main = "First difference Spending Time Series")

# Procedemos a analizar el orden AR y MA de la serie de tiempo
acf(diff(spending.ts), main = "Detectar modelo AR(p)")
pacf(diff(spending.ts), main = "Detectar modelo MA(q)")

arima_model <- arima(spending.ts, order = c(2, 1, 4))
arima_model$coef


pred <- predict(arima_model, 30)$pred
ts.plot(cbind(spending.ts, pred), col = c("blue", "red"), xlab = "")
title(main = "Time Series Prediction ARIMA(2,1,4)",
      xlab = "Time",
      ylab = "Total spending")


###############################################################################
############################### DASHBOARD CON SHINY ###########################
###############################################################################

