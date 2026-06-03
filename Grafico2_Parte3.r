library(ggplot2)
library(dplyr)

# Cargar los datos (aseg�rate de que el archivo est� en tu directorio de trabajo)
df <- read.csv("datosMerged.csv")

# 1. Limpieza y preparaci�n de datos
# Convertir columnas a num�rico y calcular el puntaje de Sociabilidad
df_clean <- df %>%
  mutate(
    calma_soc = as.numeric(calma_social_ocio),
    disfrute_soc = as.numeric(disfrute_social_ocio),
    a_gusto_soc = as.numeric(a_gusto_social_ocio),
    obligaciones = as.numeric(tiempo_total_obligatorias)
  ) %>%
  filter(!is.na(calma_soc), !is.na(disfrute_soc), !is.na(a_gusto_soc), !is.na(obligaciones)) %>%
  mutate(Sociabilidad_Score = (calma_soc + disfrute_soc + a_gusto_soc) / 3)

# 2. Dividir por la mediana
mediana_soc <- median(df_clean$Sociabilidad_Score)
df_clean <- df_clean %>%
  mutate(Perfil_Sociabilidad = ifelse(Sociabilidad_Score >= mediana_soc, 
                                      "Alta Sociabilidad", "Baja Sociabilidad"))

# 3. An�lisis Estad�stico (Prueba t de Student)
t_test <- t.test(obligaciones ~ Perfil_Sociabilidad, data = df_clean)
print(t_test)

grafico2<-ggplot(
  df,
  aes(
    x = `Generación`,
    y = tension_prom
  )
) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución del estrés según generación",
    x = "Generación",
    y = "Estrés promedio"
  )
print(grafico2)