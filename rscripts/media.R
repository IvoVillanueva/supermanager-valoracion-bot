source("rscripts/helpers.R")

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")


# medias
stats_encontra_mean <- stats_boxscores %>%
  group_by(id_match, num_jornada, abb, position, is_local) %>%
  summarise(
    puntos = mean(points),
    x3pt = mean(x3pt_success),
    rebotes = mean(total_rebound),
    asistencias = mean(asis),
    valoracion = mean(val),
    .groups = "drop"
  ) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(id_match, position) %>%
  mutate(
    puntos_encontra = ifelse(
      is_local == TRUE,
      puntos[is_local == FALSE], puntos[is_local == TRUE]
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

# medias por posicion
stats_encontra_union_mean <- stats_encontra_mean %>%
  filter(position == "base") %>%
  rename_with(
    ~ paste0("base_", .),
    .cols = puntos:valoracion_encontra
  ) %>%
  select(-position) %>%
  bind_cols(
    stats_encontra_mean %>%
      filter(position == "alero") %>%
      select(-abb, -num_jornada, -id_match, -rival, -position, -is_local) %>%
      rename_with(
        ~ paste0("alero_", .),
        .cols = puntos:valoracion_encontra
      )
  ) %>%
  bind_cols(
    stats_encontra_mean %>%
      filter(position == "pivot") %>%
      select(-abb, -num_jornada, -id_match, -rival, -position, -is_local) %>%
      rename_with(
        ~ paste0("pivot_", .),
        .cols = puntos:valoracion_encontra
      )
  ) %>%
  mutate(id_match = as.character(id_match)) %>%
  mutate(across(c(base_puntos:pivot_valoracion_encontra), ~ round(., 1)))

# Crear función para cada equipo
team_mean <- unique(stats_encontra_mean$abb)

team_df_mean <- function(team_mean) {
  stats_encontra_union_mean %>%
    filter(abb == team_mean) %>%
    group_by(abb) %>%
    summarise(across(where(is.numeric), ~ round(mean(.), 0)),
              .groups = "drop") %>%
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
      stats_encontra_union_mean %>%
        filter(abb == team_mean & is_local == "Local") %>%
        group_by(abb) %>%
        summarise(across(where(is.numeric), ~ round(mean(.), 0)),
                  .groups = "drop") %>%
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
      stats_encontra_union_mean %>%
        filter(abb == team_mean & is_local == "Visitante") %>%
        group_by(abb) %>%
        summarise(across(where(is.numeric), ~ round(mean(.), 0)),
                  .groups = "drop") %>%
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
    bind_rows(stats_encontra_union_mean %>%
                filter(abb == team_mean)) %>%
    ungroup()
}
# Crear el DataFrame final
encontra_df_mean <- map_df(team_mean, team_df_mean) %>%
  mutate(matchweek_number = paste("Jor.", num_jornada)) %>%
  left_join(clubs, join_by(abb)) %>%
  left_join(
    clubs %>% rename(rival = abb, logo_rival = logo_cuadrado),
    join_by(rival)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

write_csv(encontra_df_mean, "data/encontra_mean.csv")
