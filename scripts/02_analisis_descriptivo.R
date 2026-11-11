# 1. Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# 2. Cargar los datos
load("data/datos_modelo.RData")

# 3. Resumen agrupado por tipo de equipo
resumen_equipos <- train %>%
  group_by(tipo_equipo) %>%
  summarise(
    total_equipos     = n(),
    media_detencion   = round(mean(tiempo_detencion, na.rm = TRUE), 2),
    sd_detencion      = round(sd(tiempo_detencion, na.rm = TRUE), 2),
    mediana_detencion = round(median(tiempo_detencion, na.rm = TRUE), 2),
    media_temp        = round(mean(temperatura, na.rm = TRUE), 2),
    media_vibracion   = round(mean(vibracion, na.rm = TRUE), 2),
    .groups           = "drop"
  )
print(resumen_equipos)

# 4. Matriz de correlación entre variables numéricas
vars_num <- train %>% 
  select(temperatura, vibracion, presion, horas_operacion, 
         dias_desde_mantencion, carga_trabajo, tiempo_detencion)
matriz_cor <- round(cor(vars_num, use = "complete.obs"), 3)
print(matriz_cor)

# 5. Generar y guardar 3 gráficos
# Gráfico 1: Distribución del tiempo de detención
p1 <- ggplot(train, aes(x = tiempo_detencion)) +
  geom_histogram(bins = 25, fill = "#2B5B84", color = "white", alpha = 0.85) +
  theme_minimal() +
  labs(
    title = "Distribución del Tiempo de Detención",
    x = "Tiempo de Detención (Horas)",
    y = "Frecuencia Absoluta"
  )
ggsave("output/fig_distribucion_detencion.png", plot = p1, width = 7, height = 4.5, dpi = 300)

# Gráfico 2: Boxplot de vibración según condición de falla
p2 <- ggplot(train, aes(x = factor(falla), y = vibracion, fill = factor(falla))) +
  geom_boxplot(alpha = 0.75, outlier.colour = "red", outlier.shape = 16) +
  scale_fill_manual(values = c("#4CAF50", "#E53935")) +
  theme_minimal() +
  labs(
    title = "Nivel de Vibración según Condición de Falla",
    x = "¿Presentó Falla?",
    y = "Nivel de Vibración (mm/s)",
    fill = "Falla"
  )
ggsave("output/fig_boxplot_vibracion_falla.png", plot = p2, width = 7, height = 4.5, dpi = 300)

# Gráfico 3: Dispersión entre vibración y horas de detención
p3 <- ggplot(train, aes(x = vibracion, y = tiempo_detencion)) +
  geom_point(alpha = 0.5, color = "#FF7043") +
  geom_smooth(method = "lm", color = "#1E293B", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Relación entre Vibración Operacional y Horas de Detención",
    x = "Vibración (mm/s)",
    y = "Horas de Detención"
  )
ggsave("output/fig_dispersion_vibracion_tiempo.png", plot = p3, width = 7, height = 4.5, dpi = 300)

cat("\n>>> ¡Listo! Gráficos guardados/.\n")