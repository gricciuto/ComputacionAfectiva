library(tidyverse)

# Leer CSV
escenarios <- read.csv(
  "/home/ginoricciuto/computacionAfectiva/datos.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(escenarios) <- make.unique(names(escenarios))

# Emociones negativas
emociones_negativas <- c(
  "Preocupación",
  "Prisa",
  "Irritación",
  "Depresión",
  "Tensión"
)

# Convertir emociones a numérico
escenarios[emociones_negativas] <-
  lapply(
    escenarios[emociones_negativas],
    as.numeric
  )

# Actividades obligatorias
actividades_obligatorias <- c(
  "Trabajó",
  "Leyó o estudió",
  "Trámites o Salud"
)
# Grupos Sociales
grupos_sociales <- c(
  "Con compañeros de trabajo / escuela / club",
  "Destinatario de nuestra actividad laboral (Cliente / paciente / alumno)",
  "Personal de servicios, responsables de nuestra actividad, extraños "
)
# Dataset por persona
personas_df <- escenarios %>%
  
  group_by(identificacion) %>%
  
  summarise(
    
    # =====================================================
    # TIEMPO TOTAL ACTIVIDADES OBLIGATORIAS
    # =====================================================
    
    tiempo_total_obligatorias =
      sum(
        `Total en minutos de la actividad`[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        na.rm = TRUE
      ),
    
    
    # =====================================================
    # PROMEDIOS ACTIVIDADES OBLIGATORIAS
    # =====================================================
    
    preocupacion_prom =
      weighted.mean(
        Preocupación[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    prisa_prom =
      weighted.mean(
        Prisa[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    irritacion_prom =
      weighted.mean(
        Irritación[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    depresion_prom =
      weighted.mean(
        Depresión[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    tension_prom =
      weighted.mean(
        Tensión[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    
    # =====================================================
    # PROMEDIOS POR GRUPOS SOCIALES
    # =====================================================
    
    preocupacion_social =
      weighted.mean(
        Preocupación[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    prisa_social =
      weighted.mean(
        Prisa[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    irritacion_social =
      weighted.mean(
        Irritación[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    depresion_social =
      weighted.mean(
        Depresión[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        
        na.rm = TRUE
      ),
    
    tension_social =
      weighted.mean(
        Tensión[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        
        w =
          `Total en minutos de la actividad`[
          `Etiqueta Interacción` %in% grupos_sociales &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        
        na.rm = TRUE
      )
    
  )

# Redondear numéricos a 3 decimales
personas_df <- personas_df %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )

# Exportar CSV
write.csv(
  personas_df,
  "/home/ginoricciuto/computacionAfectiva/personas_dataset.csv",
  row.names = FALSE
)