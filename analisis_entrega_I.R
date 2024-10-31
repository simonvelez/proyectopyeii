# CARGA DE BASE (CSV)----------

# Cargar Readr para la lectura de la base 
library(readr)

# Asignar ruta de acceso - CAMBIAR DEPENDIENDO DE LA UBICACIÓN
ruta_base <- "garments_worker_productivity.csv"

# Carga base, vista de datos
datos_base <- read.csv(ruta_base, header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(datos_base)


# Limpiar la base ----------
# Eliminar la columna 'wip', dado que esta tiene datos vacios
datos_base <- subset(datos_base, select = -wip)
View(datos_base)


# (10) Análisis descriptivo de variables cuantitativas ----------
# Instalar y cargar paquetes si no están instalados
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(psych)) install.packages("psych")
if(!require(corrplot)) install.packages("corrplot")

# Cargar los paquetes previamente instalados
library(ggplot2)
library(dplyr)
library(psych)
library(corrplot)


# Calcular estadísticos de tendencia central
datos_centrales <- datos_base %>%
  summarize(
    media_productividad = mean(targeted_productivity, na.rm = TRUE),
    mediana_productividad = median(targeted_productivity, na.rm = TRUE),
    media_smv = mean(smv, na.rm = TRUE),
    mediana_smv = median(smv, na.rm = TRUE),
    media_over_time = mean(over_time, na.rm = TRUE),
    mediana_over_time = median(over_time, na.rm = TRUE),
    media_incentivo = mean(incentive, na.rm = TRUE),
    mediana_incentivo = median(incentive, na.rm = TRUE)
  )
datos_centrales

# Calcular estadísticos de dispersión
datos_dispersion <- datos_base %>%
  summarize(
    sd_productividad = sd(targeted_productivity, na.rm = TRUE),
    iqr_productividad = IQR(targeted_productivity, na.rm = TRUE),
    sd_smv = sd(smv, na.rm = TRUE),
    iqr_smv = IQR(smv, na.rm = TRUE),
    sd_over_time = sd(over_time, na.rm = TRUE),
    iqr_over_time = IQR(over_time, na.rm = TRUE),
    sd_incentivo = sd(incentive, na.rm = TRUE),
    iqr_incentivo = IQR(incentive, na.rm = TRUE)
  )
datos_dispersion

# Calcular estadísticos de forma usando psych
forma <- psych::describe(datos_base %>% select(targeted_productivity, smv, over_time, incentive))
forma[, c("skew", "kurtosis")]

# Calcular percentiles específicos
percentiles <- datos_base %>%
  summarize(
    p25_productividad = quantile(targeted_productivity, 0.25, na.rm = TRUE),
    p75_productividad = quantile(targeted_productivity, 0.75, na.rm = TRUE),
    p25_smv = quantile(smv, 0.25, na.rm = TRUE),
    p75_smv = quantile(smv, 0.75, na.rm = TRUE),
    p25_over_time = quantile(over_time, 0.25, na.rm = TRUE),
    p75_over_time = quantile(over_time, 0.75, na.rm = TRUE),
    p25_incentivo = quantile(incentive, 0.25, na.rm = TRUE),
    p75_incentivo = quantile(incentive, 0.75, na.rm = TRUE)
  )
percentiles

# Histogramas de variables cuantitativas
ggplot(datos_base, aes(x = targeted_productivity)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Productividad Objetivo")

ggplot(datos_base, aes(x = smv)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
  labs(title = "Histograma de SMV")

# Boxplots para observar valores atípicos
ggplot(datos_base, aes(y = targeted_productivity)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot de Productividad Objetivo")

# Matriz de correlación
correlaciones <- datos_base %>%
  select(targeted_productivity, smv, over_time, incentive) %>%
  cor(use = "complete.obs")

# Visualización de la matriz de correlación
corrplot(correlaciones, method = "circle")

# Diagrama de dispersión entre "targeted_productivity" y "smv"
ggplot(datos_base, aes(x = smv, y = targeted_productivity)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación entre SMV y Productividad Objetivo",
       x = "SMV", y = "Productividad Objetivo")

# Prueba de Shapiro-Wilk para normalidad
shapiro.test(datos_base$targeted_productivity)
shapiro.test(datos_base$smv)
shapiro.test(datos_base$over_time)
shapiro.test(datos_base$incentive)

# QQ-plots para visualizar normalidad
ggplot(datos_base, aes(sample = targeted_productivity)) +
  geom_qq() + geom_qq_line() +
  labs(title = "QQ-plot de Productividad Objetivo")

ggplot(datos_base, aes(sample = smv)) +
  geom_qq() + geom_qq_line() +
  labs(title = "QQ-plot de SMV")

# (10pt) Análisis descriptivo de variables cuantitativas (univariado y bivariado) ----------
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

#
# Categorize incentive into ranges
datos_base <- datos_base %>%
  mutate(incentive_range = cut(incentive,
                               breaks = c(-Inf, 25, 50, 75, 100, Inf),
                               labels = c("0-25", "25-50", "50-75", "75-100", "100+")))

# Plot actual_productivity by day with facet_wrap for day and colored by incentive range
ggplot(datos_base, aes(x = actual_productivity, fill = incentive_range)) +
  geom_histogram(binwidth = 0.05, color = "black", alpha = 0.7) +
  facet_wrap(~ day) +
  labs(title = "Distribución de la Productividad Real según el Día y Rango de Incentivo",
       x = "Productividad Real", y = "Frecuencia") +
  scale_fill_brewer(palette = "Blues", name = "Rango de Incentivo") +
  theme_minimal()


# Densityplot de smv
ggplot(datos_base, aes(x = smv)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(title = "Plot de densidad de SMV", x = "SMV", y = "Density") +
  theme_minimal()

# Violinplot de target_productivity según cada departamento
ggplot(datos_base, aes(x = department, y = targeted_productivity)) +
  geom_violin(fill = "orchid", color = "black", trim = FALSE) +
  labs(title = "Distribución de target_productivity", x = "Departamento", y = "target_productivity") +
  theme_minimal()

# Histograma con la densidad superpuesta de actual_productivity
ggplot(datos_base, aes(x = actual_productivity)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.05, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  labs(title = "Distribución de actual_productivity", x = "actual_productivity", y = "densidad") +
  theme_minimal()

# Mapa de calor de no_of_workers por día y departamento
ggplot(datos_base, aes(x = day, y = department, fill = no_of_workers)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lavender", high = "darkgreen") +
  labs(title = "Mapa de calor de no_of_workers por día y departamento",
       x = "Día de la semana", y = "Departmento", fill = "Número de trabajadores") +
  theme_minimal()


# (8pts) Análisis descriptivo bivariado entre variables categóricas y cuantitativas ----------

# Se instala ggplot si es requerido, y se carga la libreria
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Analisis 1
# Diagrama de caja y bigotes entre productividadd objetivio y equipo
ggplot(datos_base, aes(x = as.factor(team), y = targeted_productivity)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Productividad Objetivo por Equipo",
       x = "Equipo", y = "Productividad Objetivo")

# Analisis 2
# Histograma de productividad objetivo, por cada departamento
ggplot(datos_base, aes(x = targeted_productivity)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  facet_wrap(~ department) +
  labs(title = "Distribución de la Productividad por Departamento",
       x = "Productividad Objetivo", y = "Frecuencia")

# Analisis 3
# Promedio de Promedio de incentivos por departamento
library(dplyr)
avg_incentive <- datos_base %>%
  group_by(department) %>%
  summarize(mean_incentive = mean(incentive, na.rm = TRUE))

ggplot(avg_incentive, aes(x = department, y = mean_incentive)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Incentivo Promedio por Departamento",
       x = "Departamento", y = "Incentivo Promedio")

# LOS ANALISIS 2 Y 3 SON COMPLEMENTARIOS ENTRE SI, ESTOS MUESTRAN QUE A MAYORE PRODUCTIVIDAD, MAYOR INGRESO PROMEDIO