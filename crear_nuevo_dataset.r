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

# Emociones positivas
emociones_positivas <- c(
  "Calma",
  "Disfrute",
  "A_gusto_en_la_Interacción"
)

# Convertir emociones a numérico
escenarios[emociones_negativas] <-
  lapply(
    escenarios[emociones_negativas],
    as.numeric
  )

escenarios[emociones_positivas] <-
  lapply(
    escenarios[emociones_positivas],
    as.numeric
  )

# Actividades obligatorias
actividades_obligatorias <- c(
  "Trabajó",
  "Leyó o estudió",
  "Trámites o Salud"
)

# Actividades no obligatorias
actividades_no_obligatorias <- setdiff(
  unique(escenarios$`Etiqueta Actividad Resumida`),
  actividades_obligatorias
)

# Grupos sociales actividades obligatorias
grupos_sociales_act_obligatorias <- c(
  "Con compañeros de trabajo / escuela / club",
  "Destinatario de nuestra actividad laboral (Cliente / paciente / alumno)",
  "Personal de servicios, responsables de nuestra actividad, extraños "
)

# Grupos sociales actividades no obligatorias
grupos_sociales_act_no_obligatorias <- c(
  "Con su pareja",
  "Con pareja e hijos",
  "Con amigos",
  "Con sus hijos jóvenes o nietos",
  "Con otros familiares"
)

# Dataset por persona
personas_df <- escenarios %>%

  group_by(identificacion) %>%

  summarise(

    # =====================================================
    # TIEMPOS
    # =====================================================

    tiempo_total_no_obligatorias =
      sum(
        `Total en minutos de la actividad`[
          `Etiqueta Actividad Resumida` %in%
            actividades_no_obligatorias
        ],
        na.rm = TRUE
      ),

    tiempo_total_obligatorias =
      sum(
        `Total en minutos de la actividad`[
          `Etiqueta Actividad Resumida` %in%
            actividades_obligatorias
        ],
        na.rm = TRUE
      ),

    # =====================================================
    # EMOCIONES NEGATIVAS EN ACTIVIDADES OBLIGATORIAS
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
    # EMOCIONES NEGATIVAS + GRUPOS SOCIALES OBLIGATORIOS
    # =====================================================

    preocupacion_social =
      weighted.mean(
        Preocupación[
          `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        na.rm = TRUE
      ),

    prisa_social =
      weighted.mean(
        Prisa[
          `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        na.rm = TRUE
      ),

    irritacion_social =
      weighted.mean(
        Irritación[
          `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        na.rm = TRUE
      ),

    depresion_social =
      weighted.mean(
        Depresión[
          `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        na.rm = TRUE
      ),

    tension_social =
      weighted.mean(
        Tensión[
          `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_obligatorias
          ],
        na.rm = TRUE
      ),

    # =====================================================
    # EMOCIONES POSITIVAS EN OCIO
    # =====================================================

    calma_ocio =
      weighted.mean(
        Calma[
          `Etiqueta Actividad Resumida` %in%
            actividades_no_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_no_obligatorias
          ],
        na.rm = TRUE
      ),

    disfrute_ocio =
      weighted.mean(
        Disfrute[
          `Etiqueta Actividad Resumida` %in%
            actividades_no_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_no_obligatorias
          ],
        na.rm = TRUE
      ),

    a_gusto_ocio =
      weighted.mean(
        A_gusto_en_la_Interacción[
          `Etiqueta Actividad Resumida` %in%
            actividades_no_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Actividad Resumida` %in%
              actividades_no_obligatorias
          ],
        na.rm = TRUE
      ),

    # =====================================================
    # EMOCIONES POSITIVAS + GRUPOS SOCIALES DE OCIO
    # =====================================================

    calma_social_ocio =
      weighted.mean(
        Calma[
          `Etiqueta Interacción` %in% grupos_sociales_act_no_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_no_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_no_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_no_obligatorias
          ],
        na.rm = TRUE
      ),

    disfrute_social_ocio =
      weighted.mean(
        Disfrute[
          `Etiqueta Interacción` %in% grupos_sociales_act_no_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_no_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_no_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_no_obligatorias
          ],
        na.rm = TRUE
      ),

    a_gusto_social_ocio =
      weighted.mean(
        A_gusto_en_la_Interacción[
          `Etiqueta Interacción` %in% grupos_sociales_act_no_obligatorias &
          `Etiqueta Actividad Resumida` %in% actividades_no_obligatorias
        ],
        w =
          `Total en minutos de la actividad`[
            `Etiqueta Interacción` %in% grupos_sociales_act_no_obligatorias &
            `Etiqueta Actividad Resumida` %in% actividades_no_obligatorias
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