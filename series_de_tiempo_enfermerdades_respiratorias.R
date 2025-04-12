
# Cargar librerías necesarias
library(tidyverse)
library(arrow)
library(janitor)
library(forecast)
library(tseries)
library(gganimate)

# 1. Cargar datos
data <- read_parquet("https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet")
data <- janitor::clean_names(data)



# 2. Eliminar la última semana incompleta del 2025
ultima_semana <- data |> 
  filter(anio == 2025) |> 
  summarise(ultima = max(semana_estadistica, na.rm = TRUE)) |> 
  pull(ultima)

data <- data |> 
  filter(!(anio == 2025 & semana_estadistica == ultima_semana))

# 3. Filtrar datos
casos_respiratorios <- data |> 
  filter(tipo_establecimiento == "Hospital", 
         causa %in% c("Neumonía (J12-J18)", "Influenza (J09-J11)")) |> 
  group_by(anio, semana_estadistica, causa) |> 
  summarise(total_casos = sum(num_total), .groups = "drop") |> 
  arrange(anio, semana_estadistica, causa)

# 4. Crear series temporales
crear_ts <- function(df, causa) {
  df |> 
    filter(causa == !!causa) |> 
    arrange(anio, semana_estadistica) |> 
    pull(total_casos) |> 
    ts(frequency = 52, start = c(min(df$anio), min(df$semana_estadistica)))
}

ts_neumonia <- crear_ts(casos_respiratorios, "Neumonía (J12-J18)")
ts_influenza <- crear_ts(casos_respiratorios, "Influenza (J09-J11)")

# 5. Modelado con Redes Neuronales
modelo_nnet_neumonia <- nnetar(ts_neumonia)
modelo_nnet_influenza <- nnetar(ts_influenza)

# 6. Predicción para 20 semanas desde semana 14 de 2025
pred_neumonia <- forecast(modelo_nnet_neumonia, h = 20)
pred_influenza <- forecast(modelo_nnet_influenza, h = 20)

# 7. Evaluación con MAPE y MAE
precision_neumonia_mape <- accuracy(pred_neumonia)[, "MAPE"]
precision_influenza_mape <- accuracy(pred_influenza)[, "MAPE"]

precision_neumonia_mae <- accuracy(pred_neumonia)[, "MAE"]
precision_influenza_mae <- accuracy(pred_influenza)[, "MAE"]

cat("MAPE Neumonía:", round(precision_neumonia_mape, 2), "%\n")
cat("MAE Neumonía:", round(precision_neumonia_mae, 2), "\n\n")

cat("MAPE Influenza:", round(precision_influenza_mape, 2), "%\n")
cat("MAE Influenza:", round(precision_influenza_mae, 2), "\n")




# 8. Gráficos de predicción

# Configurar las dimensiones y márgenes del dispositivo gráfico
par(mfrow = c(2, 1), mar = c(5, 4, 2, 1))  # mfrow: 2 filas, 1 columna; mar: márgenes (abajo, izquierda, arriba, derecha)



plot(pred_neumonia, main = "Predicción Neumonía - Modelo Red Neuronal", col.main = "blue")
plot(pred_influenza, main = "Predicción Influenza - Modelo Red Neuronal", col.main = "red")


#8.1

# Configurar dimensiones del dispositivo gráfico
par(mfrow = c(2, 1), mar = c(5, 4, 3, 1))  # Más margen superior para títulos largos

# Gráfico Neumonía
plot(pred_neumonia,
     main = paste0("Neumonía - MAE: ", round(precision_neumonia_mae, 2), 
                   " | MAPE: ", round(precision_neumonia_mape, 2), "%"),
     col.main = "blue")

# Gráfico Influenza
plot(pred_influenza,
     main = paste0("Influenza - MAE: ", round(precision_influenza_mae, 2), 
                   " | MAPE: ", round(precision_influenza_mape, 2), "%"),
     col.main = "red")






#Gráfico con años y semanas epidemiologica  , linea punteada promedio anual , y dato del peack 

# Librerías necesarias
library(ggplot2)
library(gganimate)
library(scales)
library(dplyr)
library(stringr)

# Arreglo de la columna causa (asegurar consistencia)
casos_respiratorios <- casos_respiratorios %>%
  mutate(causa = tolower(causa)) %>%
  mutate(causa = case_when(
    str_detect(causa, "neum") ~ "neumonia",
    str_detect(causa, "influ") ~ "influenza",
    TRUE ~ causa
  ))

# Cálculo de promedio anual por causa
promedios_anuales <- casos_respiratorios %>%
  group_by(anio, causa) %>%
  summarise(promedio_anual = mean(total_casos, na.rm = TRUE), .groups = "drop")

# Puntos máximos por año y causa para etiquetas
puntos_maximos <- casos_respiratorios %>%
  group_by(anio, causa) %>%
  filter(total_casos == max(total_casos)) %>%
  distinct(anio, causa, .keep_all = TRUE)

# Colores definidos
colores_modernos <- c(
  "neumonia" = "#1f77b4",   # Azul fuerte
  "influenza" = "#ff4136"   # Rojo fuerte
)

# Gráfico animado
grafico_animado <- ggplot(casos_respiratorios, 
                          aes(x = semana_estadistica, y = total_casos, 
                              color = causa, group = causa)) +
  geom_line(size = 1.3) +
  geom_point(size = 2.2) +

  # Línea de promedio
  geom_hline(data = promedios_anuales, 
             aes(yintercept = promedio_anual, color = causa), 
             linetype = "dashed", size = 0.8, show.legend = FALSE) +

  # Etiquetas en máximos
  geom_text(data = puntos_maximos,
            aes(label = total_casos),
            vjust = -1.2, color = "white", size = 3.5, fontface = "bold", show.legend = FALSE) +

  scale_color_manual(values = colores_modernos) +

  labs(
    title = "Casos de Neumonía e Influenza\nAño: {closest_state}",
    subtitle = "Línea punteada = Promedio anual",
    x = "Semana epidemiologica",
    y = "Total de Casos",
    color = "Causa"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(t = 25, r = 20, b = 15, l = 20),  # más espacio arriba
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white", size = 11),
    axis.title = element_text(color = "white", face = "bold", size = 13),
    plot.title = element_text(color = "white", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", size = 13, hjust = 0.5),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black")
  ) +
  transition_states(anio, transition_length = 10, state_length = 3) +
  ease_aes('cubic-in-out')



# Mostrar gráfico (previa)
print(grafico_animado)






# gráfico para los años y semanas epidemiológicas
grafico_semanal_2025 <- function(prediccion, datos_originales, causa_filtrada, titulo, color) {
  
  observados_2025 <- datos_originales %>%
    filter(anio == 2025, causa == tolower(causa_filtrada)) %>%
    arrange(semana_estadistica) %>%
    select(semana = semana_estadistica, casos = total_casos) %>%
    mutate(tipo = "Observado")
  
  # Crear semanas futuras para la predicción
  # Si no hay datos observados, usamos 1 como punto de inicio
  start_week <- if (nrow(observados_2025) == 0) 1 else max(observados_2025$semana)
  semanas_predichas <- start_week + seq_len(length(prediccion$mean))
  
  predichos_2025 <- data.frame(
    semana = semanas_predichas,
    casos = as.numeric(prediccion$mean),
    tipo = "Predicho"
  )
  
  # Unir observados y predichos
  df <- bind_rows(observados_2025, predichos_2025)
  
  # Graficar
  ggplot(df, aes(x = semana, y = casos, color = tipo)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = titulo,
      x = "Semana Epidemiológica 2025",
      y = "Total de Casos",
      color = "Tipo"
    ) +
    scale_x_continuous(breaks = seq(1, 52, by = 2)) +  # Semanas del 1 al 52
    scale_color_manual(values = c("Observado" = "black", "Predicho" = color)) +
    theme_minimal(base_size = 14)
}


grafico_neumonia_2025 <- grafico_semanal_2025(pred_neumonia, casos_respiratorios, "neumonia", "Predicción Neumonía - 2025", "blue")
grafico_influenza_2025 <- grafico_semanal_2025(pred_influenza, casos_respiratorios, "influenza", "Predicción Influenza - 2025", "red")

# Mostrar gráficos
print(grafico_neumonia_2025)
print(grafico_influenza_2025)

# Guardar (opcional)
ggsave("grafico_neumonia_2025.png", grafico_neumonia_2025, width = 10, height = 6)
ggsave("grafico_influenza_2025.png", grafico_influenza_2025, width = 10, height = 6)






# gráficos de prediccion 2025 animado


library(ggplot2)
library(dplyr)
library(gganimate)

grafico_semanal_2025 <- function(prediccion, datos_originales, causa_filtrada, titulo, animar = TRUE) {

  observados_2025 <- datos_originales %>%
    filter(anio == 2025, causa == tolower(causa_filtrada)) %>%
    arrange(semana_estadistica) %>%
    select(semana = semana_estadistica, casos = total_casos) %>%
    mutate(tipo = "Observado")
  
  # Si no hay datos observados, usar 1 como inicio
  start_week <- if (nrow(observados_2025) == 0) 1 else max(observados_2025$semana)
  semanas_predichas <- start_week + seq_len(length(prediccion$mean))
  
  predichos_2025 <- data.frame(
    semana = semanas_predichas,
    casos = as.numeric(prediccion$mean),
    tipo = "Predicho"
  )
  
  # Unir observados y predichos; definir la variable frame para la animación
  df <- bind_rows(observados_2025, predichos_2025) %>%
    mutate(frame = as.integer(semana))
  
  # Colores: Observado en blanco y Predicho según la causa
  color_predicho <- if (grepl("influenza", tolower(causa_filtrada))) "red" else "blue"
  colores_modernos <- c("Observado" = "white", "Predicho" = color_predicho)
  
  grafico <- ggplot(df, aes(x = semana, y = casos, color = tipo)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = titulo,
      x = "Semana Epidemiológica 2025",
      y = "Total de Casos",
      color = "Tipo"
    ) +
    scale_x_continuous(breaks = seq(1, 52, by = 2)) +
    scale_color_manual(values = colores_modernos) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "gray30"),
      panel.grid.minor = element_line(color = "gray20"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white", face = "bold"),
      plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 25, r = 20, b = 20, l = 20),
      legend.title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black")
    )
  
  if (animar) {
    grafico <- grafico + transition_reveal(frame) + ease_aes('linear')
  }
  
  return(grafico)
}


grafico_neumonia_2025 <- grafico_semanal_2025(pred_neumonia, casos_respiratorios, "neumonia", "Predicción Neumonía - 2025", animar = TRUE)
grafico_influenza_2025 <- grafico_semanal_2025(pred_influenza, casos_respiratorios, "influenza", "Predicción Influenza - 2025", animar = TRUE)

# Mostrar animaciones
animate(grafico_neumonia_2025, width = 900, height = 550, fps = 10, duration = 20, end_pause = 10)
animate(grafico_influenza_2025, width = 900, height = 550, fps = 10, duration = 20, end_pause = 10)


