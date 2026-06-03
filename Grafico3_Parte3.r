library(tidyverse)
library(ggplot2)

# Cargar datos
df <- read.csv(
  "datosMerged.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# =====================================================
# ANALISIS: IMPACTO DE LA COMUNIDAD SEGUN NIVEL SOCIOECONOMICO
# =====================================================

datos_refugio <- df %>%

  filter(!is.na(`Nivel Socioeconómico`)) %>%

  mutate(
    disfrute_ocio = as.numeric(disfrute_ocio),
    disfrute_social_ocio = as.numeric(disfrute_social_ocio)
  ) %>%

  group_by(`Nivel Socioeconómico`) %>%

  summarise(
    Ocio_Individual =
      mean(
        disfrute_ocio,
        na.rm = TRUE
      ),

    Ocio_Social =
      mean(
        disfrute_social_ocio,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) %>%

  mutate(
    Mejora_Social =
      Ocio_Social - Ocio_Individual
  )

print(datos_refugio)

# =====================================================
# GRAFICO
# =====================================================

grafico <- ggplot(
  datos_refugio,
  aes(
    x = reorder(`Nivel Socioeconómico`, Mejora_Social),
    y = Mejora_Social,
    fill = `Nivel Socioeconómico`
  )
) +

  geom_col(show.legend = FALSE) +

  coord_flip() +

  scale_fill_brewer(
    palette = "Pastel1"
  ) +

  theme_minimal(base_size = 14) +

  labs(
    title = "Impacto de la comunidad según nivel socioeconómico",
    subtitle = "Incremento del disfrute al pasar de ocio individual a ocio social",
    x = "Nivel Socioeconómico",
    y = "Puntos extra de disfrute ganados al socializar"
  )

print(grafico)