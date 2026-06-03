library(ggplot2)
library(tidyverse)



# Leer CSV
escenarios <- read.csv(
  "/home/ginoricciuto/computacionAfectiva/datos.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
names(escenarios) <- make.unique(names(escenarios))

nombre <- "Paola1416Mabel"

# Filtrar usuario
usuario_df <- escenarios %>%
  filter(identificacion == nombre) %>%
  
  # Crear fecha-hora
  mutate(
    Hora_dt = as.POSIXct(
      paste("2026-05-14", `Hora actividad redondeada`),
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

# Columnas de emociones
emociones_cols <- c(
  "Preocupación",
  "Prisa",
  "Irritación",
  "Depresión",
  "Tensión",
  "Calma",
  "Disfrute"
)

# Buscar emoción dominante
usuario_df$Emocion_Dominante <- apply(
  usuario_df[, emociones_cols],
  1,
  function(x) {
    emociones_cols[which.max(as.numeric(x))]
  }
)

# Crear gráfico
p <- ggplot(
  usuario_df,
  aes(
    x = Hora_dt,
    y = `Etiqueta Actividad`
    
  )
) +
  
  geom_line(
    aes(group = 1),
    color = "gray80",
    linewidth = 1
  ) +
  
  geom_point(
    aes(
      color = Emocion_Dominante,
      shape = `Etiqueta Interacción`
    ),
    size = 5
  ) +
  
  geom_text(
    aes(label = Emocion_Dominante),
    vjust = -1,
    size = 5
  ) +
  
  scale_x_datetime(
    date_labels = "%H:%M",
    date_breaks = "2 hour"
  ) +
  
  theme_minimal() +
  
  labs(
    title = paste("Línea de Tiempo Afectiva: ", nombre),
    subtitle = "Actividad, Interacción Social y Emoción Predominante",
    x = "Hora del Día",
    y = "Actividad",
    color = "Emoción dominante",
    shape = "Interacción social"
  ) +
  
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10)

  )

print(p)