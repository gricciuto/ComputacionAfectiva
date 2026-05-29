library(ggplot2)
library(tidyverse)

# Filtramos por el usuario específico
usuario_df <- escenarios %>%
  filter(identificacion == "Claudia10103Eduardo") %>%
  # Convertimos la hora a formato de tiempo para que se ordene bien
  mutate(Hora_dt = as.POSIXct(paste("2026-05-14", Hora_actividad_redondeada), format="%Y-%m-%d %H:%M:%S"))

# Función para encontrar la emoción con el valor máximo en cada fila
emociones_cols <- c("Preocupación", "Prisa", "Irritación", "Depresión", "Tensión", "Calma", "Disfrute")

usuario_df$Emocion_Dominante <- apply(usuario_df[, emociones_cols], 1, function(x) {
  emociones_cols[which.max(x)]
})
p <- ggplot(usuario_df, aes(x = Hora_dt, y = Etiqueta_Actividad_Resumida)) +
  # Línea que conecta las actividades del día
  geom_line(group = 1, color = "gray80", size = 1) +
  # Puntos que representan cada escenario
  geom_point(aes(color = Emocion_Dominante, shape = Etiqueta_Interacción), size = 5) +
  # Añadimos etiquetas de texto para ver la emoción exacta sobre el punto
  geom_text(aes(label = Emocion_Dominante), vjust = -1.5, size = 3) +
  scale_x_datetime(date_labels = "%H:%M", breaks = "2 hours") +
  theme_minimal() +
  labs(title = "Línea de Tiempo Afectiva: Claudia10103Eduardo",
       subtitle = "Actividad, Interacción Social y Emoción Predominante",
       x = "Hora del Día",
       y = "Actividad",
       color = "Emoción más influyente",
       shape = "Grupo Social") +
  theme(legend.position = "bottom")

print(p)