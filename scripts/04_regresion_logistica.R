library(dplyr)

# 1. Cargar los datos preparados
load("data/datos_modelo.RData")

# 2. Ajustar el modelo de regresión logística
modelo_log <- glm(falla ~ temperatura + vibracion + dias_desde_mantencion,
                  data = train, family = binomial)

# Ver los resultados del modelo
summary(modelo_log)

# 3. Calcular los Odds Ratios 
cat("\n--- ODDS RATIOS ---\n")
odds_ratios <- round(exp(coef(modelo_log)), 4)
print(odds_ratios)

# 4. Predecir probabilidades en el grupo de prueba 
prob_falla <- predict(modelo_log, newdata = test, type = "response")

# 5. Aplicar los dos umbrales de decisión
clase_050 <- ifelse(prob_falla > 0.50, "Sí", "No")
clase_alt <- ifelse(prob_falla > 0.30, "Sí", "No")

# 6. Guardar el archivo .csv 
resultados_entrega <- test
resultados_entrega$probabilidad_estimada <- round(prob_falla, 4)
resultados_entrega$prediccion_umbral_050 <- clase_050
resultados_entrega$prediccion_umbral_030 <- clase_alt

write.csv2(resultados_entrega, "data/predicciones_test.csv", row.names = FALSE, fileEncoding = "latin1")
cat("\n>>> Archivo 'data/predicciones_test.csv' exportado exitosamente.\n")

# Guardar los objetos
save(modelo_log, prob_falla, clase_050, clase_alt, test, 
     file = "data/resultados_logistica.RData")
