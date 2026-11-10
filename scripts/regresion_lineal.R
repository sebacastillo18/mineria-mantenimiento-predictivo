load("data/datos_modelo.RData")

modelo_lm = lm(tiempo_detencion ~ temperatura + vibracion + horas_operacion + dias_desde_mantencion, data = train)

options(scipen = 999) #Desactivar notación científica
summary(modelo_lm)

predicciones <- predict(modelo_lm, newdata = test)

reales <- test$tiempo_detencion
rmse <- sqrt(mean((reales - predicciones)^2))

print(rmse)