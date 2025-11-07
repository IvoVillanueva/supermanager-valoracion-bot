source("rscripts/helpers.R")



# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")


# Estadísticas en contra
stats_encontra <- stats_boxscores %>%
  glimpse() %>%
  group_by(id_match, num_jornada, abb, position, is_local) %>%
  glimpse() %>%
  summarise(
    puntos = sum(points),
    x3pt = sum(x3pt_success),
    rebotes = sum(total_rebound),
    asistencias = sum(asis),
    valoracion = sum(val),
    .groups = "drop"
  ) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(id_match, num_jornada, position) %>%
  mutate(
    puntos_encontra = ifelse(
      is_local == TRUE, puntos[is_local == FALSE],
      puntos[is_local == TRUE]
    ),
    x3pt_encontra = ifelse(
      is_local == TRUE, x3pt[is_local == FALSE],
      x3pt[is_local == TRUE]
    ),
    rebotes_encontra = ifelse(
      is_local == TRUE, rebotes[is_local == FALSE],
      rebotes[is_local == TRUE]
    ),
    asistencias_encontra = ifelse(
      is_local == TRUE, asistencias[is_local == FALSE],
      asistencias[is_local == TRUE]
    ),
    valoracion_encontra = ifelse(
      is_local == TRUE, valoracion[is_local == FALSE],
      valoracion[is_local == TRUE]
    ),
    rival = ifelse(
      is_local == TRUE, abb[is_local == FALSE],
      abb[is_local == TRUE]
    ),
    is_local = ifelse(is_local == TRUE, "Local", "Visitante"),
    position = case_when(
      position == 1 ~ "base",
      position == 3 ~ "alero",
      TRUE ~ "pivot"
    )
  ) %>%
  ungroup() %>%
  select(abb, num_jornada, id_match, rival, position:valoracion_encontra) %>%
  arrange(abb)


# Crear DataFrame con posiciones como columnas


stats_encontra_union <- stats_encontra %>%
  filter(position == "base") %>%
  rename_with(
    ~ paste0("base_", .),
    .cols = puntos:valoracion_encontra
  ) %>%
  select(-position) %>%
  bind_cols(
    stats_encontra %>%
      filter(position == "alero") %>%
      select(-abb, -num_jornada, -id_match, -rival, -position, -is_local) %>%
      rename_with(
        ~ paste0("alero_", .),
        .cols = puntos:valoracion_encontra
      )
  ) %>%
  bind_cols(
    stats_encontra %>%
      filter(position == "pivot") %>%
      select(-abb, -id_match, -num_jornada, -rival, -position, -is_local) %>%
      rename_with(
        ~ paste0("pivot_", .),
        .cols = puntos:valoracion_encontra
      )
  ) %>%
  mutate(id_match = as.character(id_match))

# Crear función para cada equipo
team <- unique(stats_encontra$abb)

team_df <- function(team) {
  stats_encontra_union %>%
    filter(abb == team) %>%
    group_by(abb) %>%
    summarise(across(where(is.numeric), ~ round(mean(.), 0)),
      .groups = "drop"
    ) %>%
    mutate(
      rival = "Todos",
      position = "Todas",
      is_local = "Todas"
    ) %>%
    select(
      abb, rival, is_local, base_puntos:base_valoracion,
      alero_puntos:alero_valoracion,
      pivot_puntos:pivot_valoracion, matches("_encontra")
    ) %>%
    bind_rows(
      stats_encontra_union %>%
        filter(abb == team & is_local == "Local") %>%
        group_by(abb) %>%
        summarise(across(where(is.numeric), ~ round(mean(.), 0)),
          .groups = "drop"
        ) %>%
        mutate(
          rival = "Todos",
          position = "Todas",
          is_local = "Todas Local"
        )
    ) %>%
    select(
      abb, rival, is_local, base_puntos:base_valoracion,
      alero_puntos:alero_valoracion,
      pivot_puntos:pivot_valoracion, matches("_encontra")
    ) %>%
    bind_rows(
      stats_encontra_union %>%
        filter(abb == team & is_local == "Visitante") %>%
        group_by(abb) %>%
        summarise(across(where(is.numeric), ~ round(mean(.), 0)),
          .groups = "drop"
        ) %>%
        mutate(
          rival = "Todos",
          position = "Todas",
          is_local = "Todas  Visitante"
        )
    ) %>%
    select(
      abb, rival, is_local, base_puntos:base_valoracion,
      alero_puntos:alero_valoracion,
      pivot_puntos:pivot_valoracion, matches("_encontra")
    ) %>%
    bind_rows(stats_encontra_union %>%
                filter(abb == team)) %>%
    ungroup()
}

# Crear el DataFrame final
en_contra_df <- map_df(team, team_df) %>%
  mutate(matchweek_number = paste("Jor.", num_jornada)) %>%
  left_join(clubs, join_by(abb)) %>%
  left_join(
    clubs %>% rename(rival = abb, logo_rival = logo_cuadrado),
    join_by(rival)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))


# Guardar CSV
# write_csv(en_contra_df, here::here("2026/playByplay2026/valEnContra/encontra.csv"))
