





# =============================================================================
# ARTÍCULO DE INVESTIGACIÓN Q1
# ESTADÍSTICA ESPACIAL AGROPECUARIA EN EL PERÚ
# =============================================================================
#
# Autor: Yoel Incacutipa
# Fuente: Encuesta Nacional Agropecuaria (ENA)
#
# OBJETIVOS:
# OE1. Construcción del panel areal distrital.
# OE2. Análisis de autocorrelación espacial global y local.
# OE3. Modelamiento econométrico espacial SAR.
#
# METODOLOGÍA ESPACIAL:
# - Coordenadas representativas distritales
# - KNN-6
# - Moran Global
# - LISA
# - Moran Scatterplot
# - Modelo OLS
# - Modelo SAR
# - Impactos directos, indirectos y totales
#
# SALIDAS:
# - Tablas Excel
# - Gráficos PNG
# - Resultados RData
# =============================================================================


# =============================================================================
# 0. LIMPIAR ENTORNO
# =============================================================================

rm(list = ls())
graphics.off()

cat("\n")
cat("============================================================\n")
cat("     ANÁLISIS ESPACIAL AGROPECUARIO DEL PERÚ - ENA\n")
cat("============================================================\n\n")


# =============================================================================
# 1. CONFIGURACIÓN GENERAL
# =============================================================================

ruta_ena <- r"(D:\william 10 semestre\estadistica espacial\datos ena\ENA_2014_2024 (1)\ENA_2014_2024.sav)"

# Carpeta principal de resultados
carpeta_salida <- file.path(
  dirname(ruta_ena),
  "RESULTADOS_ARTICULO_Q1"
)

# Subcarpetas
carpeta_tablas <- file.path(carpeta_salida, "TABLAS")
carpeta_figuras <- file.path(carpeta_salida, "FIGURAS")
carpeta_modelos <- file.path(carpeta_salida, "MODELOS")

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
dir.create(carpeta_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(carpeta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(carpeta_modelos, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 2. INSTALAR Y CARGAR LIBRERÍAS
# =============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  haven,
  dplyr,
  tidyr,
  stringr,
  sf,
  spdep,
  spatialreg,
  ggplot2,
  openxlsx,
  gridExtra,
  scales,
  patchwork
)


# =============================================================================
# 3. VERIFICAR ARCHIVO
# =============================================================================

if (!file.exists(ruta_ena)) {
  stop(
    paste0(
      "\nERROR: No se encontró el archivo ENA.\n",
      "Ruta utilizada:\n",
      ruta_ena
    )
  )
}

cat("✔ Archivo ENA localizado correctamente.\n")
cat("Ruta:\n", ruta_ena, "\n\n")


# =============================================================================
# 4. CARGA DE MICRODATOS ENA
# =============================================================================

cols_clave <- c(
  "ANIO",
  "CCDD",
  "NOMBREDD",
  "CCPP",
  "NOMBREPV",
  "CCDI",
  "NOMBREDI",
  "FACTOR",
  "LONGITUD",
  "LATITUD"
)

cat("Cargando microdatos ENA...\n")

data_ena_raw <- haven::read_sav(
  ruta_ena,
  col_select = dplyr::any_of(cols_clave),
  encoding = "latin1"
)

cat(
  "✔ Registros originales:",
  format(nrow(data_ena_raw), big.mark = ","),
  "\n\n"
)


# =============================================================================
# 5. FUNCIÓN AUXILIAR PARA CONVERTIR VARIABLES
# =============================================================================

convertir_numerico <- function(x) {
  
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  suppressWarnings(as.numeric(x))
}


# =============================================================================
# 6. LIMPIEZA Y CONSTRUCCIÓN DEL UBIGEO
# =============================================================================

data_ena_clean <- data_ena_raw %>%
  
  mutate(
    
    anio_num = convertir_numerico(ANIO),
    
    factor_num = convertir_numerico(FACTOR),
    
    lon_num = convertir_numerico(LONGITUD),
    
    lat_num = convertir_numerico(LATITUD),
    
    CCDD_chr = str_pad(
      str_trim(as.character(CCDD)),
      width = 2,
      pad = "0"
    ),
    
    CCPP_chr = str_pad(
      str_trim(as.character(CCPP)),
      width = 2,
      pad = "0"
    ),
    
    CCDI_chr = str_pad(
      str_trim(as.character(CCDI)),
      width = 2,
      pad = "0"
    ),
    
    ubigeo_6d = paste0(
      CCDD_chr,
      CCPP_chr,
      CCDI_chr
    ),
    
    # Longitud válida para Perú
    lon_peru = case_when(
      is.finite(lon_num) &
        abs(lon_num) >= 60 &
        abs(lon_num) <= 85
      ~ -abs(lon_num),
      
      TRUE ~ NA_real_
    ),
    
    # Latitud válida para Perú
    lat_peru = case_when(
      is.finite(lat_num) &
        abs(lat_num) > 0 &
        abs(lat_num) <= 20
      ~ -abs(lat_num),
      
      TRUE ~ NA_real_
    )
  ) %>%
  
  filter(
    anio_num %in% 2021:2024,
    nchar(ubigeo_6d) == 6,
    !is.na(ubigeo_6d)
  )


cat(
  "✔ Registros 2021-2024:",
  format(nrow(data_ena_clean), big.mark = ","),
  "\n\n"
)


# =============================================================================
# 7. TABLA DE COBERTURA DE DATOS
# =============================================================================

tabla_cobertura <- data_ena_clean %>%
  
  group_by(anio_num) %>%
  
  summarise(
    Registros = n(),
    
    Distritos = n_distinct(ubigeo_6d),
    
    Coordenadas_Longitud = sum(
      is.finite(lon_peru)
    ),
    
    Coordenadas_Latitud = sum(
      is.finite(lat_peru)
    ),
    
    Coordenadas_Completas = sum(
      is.finite(lon_peru) &
        is.finite(lat_peru)
    ),
    
    .groups = "drop"
  ) %>%
  
  rename(
    Año = anio_num
  )


print(tabla_cobertura)


# =============================================================================
# 8. CONSTRUCCIÓN DEL PANEL DISTRITAL-AÑO
# =============================================================================

data_panel_distrital <- data_ena_clean %>%
  
  group_by(
    anio_num,
    ubigeo_6d,
    NOMBREDD,
    NOMBREPV,
    NOMBREDI
  ) %>%
  
  summarise(
    
    muestra_encuestas = n(),
    
    poblacion_agricola = sum(
      factor_num,
      na.rm = TRUE
    ),
    
    lon_centroide = mean(
      lon_peru,
      na.rm = TRUE
    ),
    
    lat_centroide = mean(
      lat_peru,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Reemplazar NaN por NA
data_panel_distrital <- data_panel_distrital %>%
  
  mutate(
    
    lon_centroide = ifelse(
      is.nan(lon_centroide),
      NA,
      lon_centroide
    ),
    
    lat_centroide = ifelse(
      is.nan(lat_centroide),
      NA,
      lat_centroide
    )
  )


cat("\n")
cat("============================================================\n")
cat("        PANEL DISTRITAL-AÑO CONSTRUIDO\n")
cat("============================================================\n")

cat(
  "Observaciones Distrito-Año:",
  format(nrow(data_panel_distrital), big.mark = ","),
  "\n"
)

cat(
  "Distritos únicos:",
  format(
    n_distinct(data_panel_distrital$ubigeo_6d),
    big.mark = ","
  ),
  "\n\n"
)


# =============================================================================
# 9. RESUMEN DEL PANEL POR AÑO
# =============================================================================

tabla_panel_anual <- data_panel_distrital %>%
  
  group_by(anio_num) %>%
  
  summarise(
    
    Observaciones_Distrito_Año = n(),
    
    Distritos_Unicos = n_distinct(
      ubigeo_6d
    ),
    
    Poblacion_Agricola_Total = sum(
      poblacion_agricola,
      na.rm = TRUE
    ),
    
    Muestra_Total = sum(
      muestra_encuestas,
      na.rm = TRUE
    ),
    
    Coordenadas_Completas = sum(
      is.finite(lon_centroide) &
        is.finite(lat_centroide)
    ),
    
    .groups = "drop"
  ) %>%
  
  rename(
    Año = anio_num
  )


print(tabla_panel_anual)


# =============================================================================
# 10. CREACIÓN DEL PANEL ESPACIAL
# =============================================================================

centroides_distrito <- data_panel_distrital %>%
  
  filter(
    is.finite(lon_centroide),
    is.finite(lat_centroide)
  ) %>%
  
  group_by(ubigeo_6d) %>%
  
  summarise(
    
    lon_dist = mean(
      lon_centroide,
      na.rm = TRUE
    ),
    
    lat_dist = mean(
      lat_centroide,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


data_panel_geo <- data_panel_distrital %>%
  
  left_join(
    centroides_distrito,
    by = "ubigeo_6d"
  ) %>%
  
  filter(
    is.finite(lon_dist),
    is.finite(lat_dist),
    is.finite(poblacion_agricola),
    poblacion_agricola > 0
  )


spatial_panel <- st_as_sf(
  
  data_panel_geo,
  
  coords = c(
    "lon_dist",
    "lat_dist"
  ),
  
  crs = 4326,
  
  remove = FALSE
) %>%
  
  st_transform(
    crs = 32718
  )


cat(
  "\n✔ Observaciones espaciales:",
  format(nrow(spatial_panel), big.mark = ","),
  "\n"
)


# =============================================================================
# 11. VERIFICAR AÑOS DISPONIBLES PARA ANÁLISIS ESPACIAL
# =============================================================================

tabla_anios_espaciales <- spatial_panel %>%
  
  st_drop_geometry() %>%
  
  group_by(anio_num) %>%
  
  summarise(
    
    Distritos = n(),
    
    Distritos_Unicos = n_distinct(
      ubigeo_6d
    ),
    
    .groups = "drop"
  ) %>%
  
  rename(
    Año = anio_num
  )


cat("\nAños disponibles para análisis espacial:\n")
print(tabla_anios_espaciales)


anios_disponibles <- sort(
  unique(
    spatial_panel$anio_num
  )
)


# =============================================================================
# 12. FUNCIÓN PARA CONSTRUIR KNN-6
# =============================================================================

crear_knn <- function(sf_data, k = 6) {
  
  coords <- st_coordinates(sf_data)
  
  coords <- coords[
    ,
    1:2,
    drop = FALSE
  ]
  
  coords <- apply(
    coords,
    2,
    as.numeric
  )
  
  k_real <- min(
    k,
    nrow(coords) - 1
  )
  
  knn <- spdep::knearneigh(
    coords,
    k = k_real
  )
  
  nb <- spdep::knn2nb(
    knn
  )
  
  lw <- spdep::nb2listw(
    nb,
    style = "W",
    zero.policy = TRUE
  )
  
  list(
    coords = coords,
    knn = knn,
    nb = nb,
    lw = lw
  )
}


# =============================================================================
# 13. MORAN GLOBAL POR AÑO
# =============================================================================

calcular_moran <- function(sf_data, anio) {
  
  df <- sf_data %>%
    filter(
      anio_num == anio
    )
  
  if (nrow(df) < 10) {
    return(NULL)
  }
  
  espacio <- crear_knn(
    df,
    k = 6
  )
  
  y <- as.numeric(
    df$poblacion_agricola
  )
  
  set.seed(123)
  
  moran_mc <- spdep::moran.mc(
    y,
    espacio$lw,
    nsim = 999,
    zero.policy = TRUE
  )
  
  moran_obs <- as.numeric(
    moran_mc$statistic
  )
  
  p_valor <- moran_mc$p.value
  
  interpretacion <- dplyr::case_when(
    
    p_valor < 0.05 &
      moran_obs > 0
    ~ "Autocorrelación espacial positiva significativa",
    
    p_valor < 0.05 &
      moran_obs < 0
    ~ "Autocorrelación espacial negativa significativa",
    
    TRUE
    ~ "Sin autocorrelación espacial significativa"
  )
  
  data.frame(
    
    Año = anio,
    
    Distritos = nrow(df),
    
    Moran_I = round(
      moran_obs,
      4
    ),
    
    p_valor = p_valor,
    
    Significativo = ifelse(
      p_valor < 0.05,
      "Sí",
      "No"
    ),
    
    Interpretacion = interpretacion
  )
}


# =============================================================================
# 14. EJECUTAR MORAN PARA TODOS LOS AÑOS DISPONIBLES
# =============================================================================

lista_moran <- lapply(
  
  anios_disponibles,
  
  function(a) {
    
    calcular_moran(
      spatial_panel,
      a
    )
  }
)


cuadro_1 <- do.call(
  rbind,
  lista_moran
)


rownames(cuadro_1) <- NULL


cat("\n")
cat("============================================================\n")
cat("              MORAN GLOBAL POR AÑO\n")
cat("============================================================\n")

print(cuadro_1)


# =============================================================================
# 15. GRÁFICO: EVOLUCIÓN DEL ÍNDICE DE MORAN
# =============================================================================

g_moran_temporal <- ggplot(
  cuadro_1,
  aes(
    x = Año,
    y = Moran_I
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Evolución temporal del Índice de Moran Global",
    subtitle = "Autocorrelación espacial de la población agrícola estimada",
    x = "Año",
    y = "Índice de Moran Global",
    caption = "Fuente: ENA - INEI. Elaboración propia."
  )


print(g_moran_temporal)


ggsave(
  file.path(
    carpeta_figuras,
    "Figura_Moran_Global_Temporal.png"
  ),
  g_moran_temporal,
  width = 10,
  height = 7,
  dpi = 300
)


# =============================================================================
# 16. LISA PARA CADA AÑO DISPONIBLE
# =============================================================================

calcular_lisa <- function(sf_data, anio) {
  
  df <- sf_data %>%
    filter(
      anio_num == anio
    )
  
  if (nrow(df) < 10) {
    return(NULL)
  }
  
  espacio <- crear_knn(
    df,
    k = 6
  )
  
  y <- as.numeric(
    df$poblacion_agricola
  )
  
  local_m <- spdep::localmoran(
    y,
    espacio$lw,
    zero.policy = TRUE
  )
  
  # Extraer p-valor de forma segura
  nombres_local <- colnames(local_m)
  
  indice_p <- grep(
    "Pr\\(",
    nombres_local
  )
  
  if (length(indice_p) == 0) {
    indice_p <- 5
  } else {
    indice_p <- indice_p[1]
  }
  
  p_val <- local_m[
    ,
    indice_p
  ]
  
  z_var <- as.numeric(
    scale(y)
  )
  
  lag_y <- spdep::lag.listw(
    espacio$lw,
    y,
    zero.policy = TRUE
  )
  
  z_lag <- as.numeric(
    scale(lag_y)
  )
  
  df$z_var <- z_var
  
  df$z_lag <- z_lag
  
  df$p_LISA <- p_val
  
  df$LISA_CLUSTER <- dplyr::case_when(
    
    p_val >= 0.05
    ~ "No Significativo",
    
    z_var > 0 &
      z_lag > 0
    ~ "High-High (Hotspot)",
    
    z_var < 0 &
      z_lag < 0
    ~ "Low-Low (Coldspot)",
    
    z_var > 0 &
      z_lag < 0
    ~ "High-Low (Atípico)",
    
    z_var < 0 &
      z_lag > 0
    ~ "Low-High (Atípico)",
    
    TRUE
    ~ "No Clasificado"
  )
  
  df
}


# =============================================================================
# 17. EJECUTAR LISA PARA TODOS LOS AÑOS
# =============================================================================

lista_lisa <- lapply(
  
  anios_disponibles,
  
  function(a) {
    
    calcular_lisa(
      spatial_panel,
      a
    )
  }
)


lista_lisa <- lista_lisa[
  !sapply(
    lista_lisa,
    is.null
  )
]


names(lista_lisa) <- paste0(
  "Año_",
  anios_disponibles[
    seq_along(lista_lisa)
  ]
)


# =============================================================================
# 18. TABLA GENERAL DE FRECUENCIAS LISA
# =============================================================================

tabla_lisa_anual <- do.call(
  
  rbind,
  
  lapply(
    
    lista_lisa,
    
    function(x) {
      
      x %>%
        
        st_drop_geometry() %>%
        
        count(
          anio_num,
          LISA_CLUSTER,
          name = "Numero_Distritos"
        ) %>%
        
        group_by(
          anio_num
        ) %>%
        
        mutate(
          
          Porcentaje = round(
            Numero_Distritos /
              sum(Numero_Distritos) *
              100,
            2
          )
        ) %>%
        
        ungroup()
    }
  )
)


tabla_lisa_anual <- tabla_lisa_anual %>%
  
  rename(
    Año = anio_num,
    Categoria_LISA = LISA_CLUSTER
  )


cat("\n")
cat("============================================================\n")
cat("               FRECUENCIA LISA POR AÑO\n")
cat("============================================================\n")

print(tabla_lisa_anual)


# =============================================================================
# 19. COLORES LISA
# =============================================================================

colores_lisa <- c(
  
  "High-High (Hotspot)" = "#d7191c",
  
  "Low-Low (Coldspot)" = "#2b83ba",
  
  "High-Low (Atípico)" = "#fdae61",
  
  "Low-High (Atípico)" = "#abd9e9",
  
  "No Significativo" = "grey80",
  
  "No Clasificado" = "black"
)


# =============================================================================
# 20. GENERAR MAPAS LISA PARA TODOS LOS AÑOS
# =============================================================================

graficos_lisa <- list()


for (nombre in names(lista_lisa)) {
  
  datos_lisa <- lista_lisa[[nombre]]
  
  anio_actual <- unique(
    datos_lisa$anio_num
  )
  
  g <- ggplot(
    datos_lisa
  ) +
    
    geom_sf(
      aes(
        color = LISA_CLUSTER
      ),
      size = 1.1,
      alpha = 0.85
    ) +
    
    scale_color_manual(
      values = colores_lisa,
      drop = FALSE
    ) +
    
    theme_minimal() +
    
    labs(
      title = paste(
        "Mapa de Clusters LISA - Perú",
        anio_actual
      ),
      subtitle = paste(
        "Clasificación espacial de la población agrícola estimada"
      ),
      color = "Categoría LISA",
      caption = "Fuente: ENA - INEI. Matriz KNN-6."
    ) +
    
    theme(
      legend.position = "right",
      plot.title = element_text(
        face = "bold"
      )
    )
  
  graficos_lisa[[nombre]] <- g
  
  print(g)
  
  ggsave(
    file.path(
      carpeta_figuras,
      paste0(
        "Figura_Mapa_LISA_",
        anio_actual,
        ".png"
      )
    ),
    g,
    width = 11,
    height = 8,
    dpi = 300
  )
}


# =============================================================================
# 21. MORAN SCATTERPLOT PARA CADA AÑO
# =============================================================================

graficos_scatter <- list()


for (nombre in names(lista_lisa)) {
  
  datos_lisa <- lista_lisa[[nombre]]
  
  anio_actual <- unique(
    datos_lisa$anio_num
  )
  
  g <- ggplot(
    datos_lisa,
    aes(
      x = z_var,
      y = z_lag,
      color = LISA_CLUSTER
    )
  ) +
    
    geom_point(
      alpha = 0.65,
      size = 1.7
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    
    geom_smooth(
      method = "lm",
      se = FALSE,
      color = "black"
    ) +
    
    scale_color_manual(
      values = colores_lisa,
      drop = FALSE
    ) +
    
    theme_minimal() +
    
    labs(
      title = paste(
        "Moran Scatterplot -",
        anio_actual
      ),
      subtitle = "Relación entre valores estandarizados y rezago espacial",
      x = "Variable estandarizada Z",
      y = "Rezago espacial estandarizado",
      color = "Categoría LISA"
    )
  
  graficos_scatter[[nombre]] <- g
  
  print(g)
  
  ggsave(
    file.path(
      carpeta_figuras,
      paste0(
        "Figura_Moran_Scatterplot_",
        anio_actual,
        ".png"
      )
    ),
    g,
    width = 10,
    height = 7,
    dpi = 300
  )
}


# =============================================================================
# 22. GRÁFICO DE DISTRIBUCIÓN DE POBLACIÓN AGRÍCOLA
# =============================================================================

g_distribucion <- ggplot(
  
  data_panel_distrital %>%
    filter(
      poblacion_agricola > 0
    ),
  
  aes(
    x = log1p(
      poblacion_agricola
    )
  )
) +
  
  geom_histogram(
    bins = 50
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Distribución de la población agrícola estimada",
    subtitle = "Escala logarítmica aplicada para reducir la asimetría",
    x = "log(Población agrícola + 1)",
    y = "Frecuencia"
  )


print(g_distribucion)


ggsave(
  file.path(
    carpeta_figuras,
    "Figura_Distribucion_Poblacion_Agricola.png"
  ),
  g_distribucion,
  width = 10,
  height = 7,
  dpi = 300
)


# =============================================================================
# 23. EVOLUCIÓN DE LA POBLACIÓN AGRÍCOLA
# =============================================================================

tabla_evolucion <- data_panel_distrital %>%
  
  group_by(
    anio_num
  ) %>%
  
  summarise(
    
    Poblacion_Total = sum(
      poblacion_agricola,
      na.rm = TRUE
    ),
    
    Poblacion_Media = mean(
      poblacion_agricola,
      na.rm = TRUE
    ),
    
    Poblacion_Mediana = median(
      poblacion_agricola,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  rename(
    Año = anio_num
  )


g_evolucion <- ggplot(
  tabla_evolucion,
  aes(
    x = Año,
    y = Poblacion_Total
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_y_continuous(
    labels = scales::comma
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Evolución de la población agrícola estimada",
    x = "Año",
    y = "Población agrícola estimada",
    caption = "Fuente: ENA - INEI."
  )


print(g_evolucion)


ggsave(
  file.path(
    carpeta_figuras,
    "Figura_Evolucion_Poblacion_Agricola.png"
  ),
  g_evolucion,
  width = 10,
  height = 7,
  dpi = 300
)


# =============================================================================
# 24. SELECCIÓN DEL AÑO BASE PARA MODELO SAR
# =============================================================================

# Se selecciona automáticamente el año con mayor cantidad
# de observaciones espaciales.

anio_modelo <- tabla_anios_espaciales %>%
  
  arrange(
    desc(Distritos)
  ) %>%
  
  slice(1) %>%
  
  pull(Año)


cat(
  "\nAño seleccionado para modelo SAR:",
  anio_modelo,
  "\n"
)


spatial_target <- spatial_panel %>%
  
  filter(
    anio_num == anio_modelo
  )


# =============================================================================
# 25. CREACIÓN DE VARIABLES DEL MODELO
# =============================================================================

data_modelo_fase3 <- spatial_target %>%
  
  mutate(
    
    log_y = log1p(
      as.numeric(
        poblacion_agricola
      )
    ),
    
    log_x = log1p(
      as.numeric(
        muestra_encuestas
      )
    )
  ) %>%
  
  filter(
    is.finite(log_y),
    is.finite(log_x)
  )


cat(
  "Observaciones modelo:",
  nrow(data_modelo_fase3),
  "\n"
)


# =============================================================================
# 26. MATRIZ ESPACIAL KNN-6
# =============================================================================

espacio_modelo <- crear_knn(
  data_modelo_fase3,
  k = 6
)


lw_mod <- espacio_modelo$lw


# =============================================================================
# 27. MODELO MCO
# =============================================================================

modelo_ols <- lm(
  log_y ~ log_x,
  data = data_modelo_fase3
)


# =============================================================================
# 28. MODELO SAR
# =============================================================================

modelo_sar <- spatialreg::lagsarlm(
  
  log_y ~ log_x,
  
  data = data_modelo_fase3,
  
  listw = lw_mod,
  
  zero.policy = TRUE
)


# =============================================================================
# 29. RESUMEN DE MODELOS
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("                    MODELO MCO\n")
cat("============================================================\n")

print(
  summary(
    modelo_ols
  )
)


cat("\n")
cat("============================================================\n")
cat("                    MODELO SAR\n")
cat("============================================================\n")

print(
  summary(
    modelo_sar
  )
)


# =============================================================================
# 30. TABLA COMPARATIVA MCO VS SAR
# =============================================================================

coef_ols <- summary(
  modelo_ols
)$coefficients


coef_sar <- summary(
  modelo_sar
)$Coef


tabla_modelos <- data.frame(
  
  Modelo = c(
    "MCO",
    "SAR"
  ),
  
  Intercepto = c(
    coef_ols[
      "(Intercept)",
      "Estimate"
    ],
    coef_sar[
      "(Intercept)",
      "Estimate"
    ]
  ),
  
  Coef_log_x = c(
    coef_ols[
      "log_x",
      "Estimate"
    ],
    coef_sar[
      "log_x",
      "Estimate"
    ]
  ),
  
  AIC = c(
    AIC(
      modelo_ols
    ),
    AIC(
      modelo_sar
    )
  )
)


print(tabla_modelos)


# =============================================================================
# 31. IMPACTOS ESPACIALES DEL SAR
# =============================================================================

set.seed(123)

impactos_sar <- spatialreg::impacts(
  
  modelo_sar,
  
  listw = lw_mod,
  
  R = 1000
)


cat("\n")
cat("============================================================\n")
cat("               IMPACTOS ESPACIALES SAR\n")
cat("============================================================\n")

print(
  summary(
    impactos_sar,
    zstats = TRUE
  )
)


# =============================================================================
# 32. EXTRAER IMPACTOS
# =============================================================================

# ============================================================
# TABLA DE IMPACTOS ESPACIALES SAR
# ============================================================

impactos_resumen <- summary(
  impactos_sar,
  zstats = TRUE
)

# Extraer matriz de impactos
impactos_mat <- impactos_resumen$impactMat

# Verificar estructura
print(impactos_mat)

# Crear tabla de impactos
tabla_impactos <- data.frame(
  Variable = rownames(impactos_mat),
  Impacto_Directo = as.numeric(
    impactos_mat[, "Direct"]
  ),
  Impacto_Indirecto = as.numeric(
    impactos_mat[, "Indirect"]
  ),
  Impacto_Total = as.numeric(
    impactos_mat[, "Total"]
  ),
  row.names = NULL
)

# Redondear resultados
tabla_impactos <- tabla_impactos %>%
  mutate(
    Impacto_Directo = round(
      Impacto_Directo,
      4
    ),
    Impacto_Indirecto = round(
      Impacto_Indirecto,
      4
    ),
    Impacto_Total = round(
      Impacto_Total,
      4
    )
  )

# Mostrar tabla
cat("\n")
cat("============================================================\n")
cat("              IMPACTOS ESPACIALES SAR\n")
cat("============================================================\n")

print(tabla_impactos)

print(tabla_impactos)


# =============================================================================
# 33. PREDICCIONES MCO Y SAR
# =============================================================================

data_modelo_fase3$pred_ols <- predict(
  modelo_ols
)


data_modelo_fase3$pred_sar <- fitted(
  modelo_sar
)


# =============================================================================
# 34. FIGURA COMPARACIÓN MCO VS SAR
# =============================================================================

g_ajuste <- ggplot(
  
  data_modelo_fase3
) +
  
  geom_point(
    aes(
      x = log_y,
      y = pred_ols,
      color = "MCO"
    ),
    alpha = 0.35,
    size = 1.5
  ) +
  
  geom_point(
    aes(
      x = log_y,
      y = pred_sar,
      color = "SAR"
    ),
    alpha = 0.45,
    size = 1.5
  ) +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  
  scale_color_manual(
    values = c(
      "MCO" = "#e41a1c",
      "SAR" = "#377eb8"
    )
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Comparación del ajuste predictivo: MCO vs SAR",
    x = "Valor observado: log(Población agrícola + 1)",
    y = "Valor predicho",
    color = "Modelo"
  )


print(g_ajuste)


ggsave(
  file.path(
    carpeta_figuras,
    "Figura_Comparacion_MCO_SAR.png"
  ),
  g_ajuste,
  width = 10,
  height = 7,
  dpi = 300
)


# =============================================================================
# 35. RESIDUOS DEL MODELO SAR
# =============================================================================

data_modelo_fase3$residuos_sar <- residuals(
  modelo_sar
)


g_residuos <- ggplot(
  
  data_modelo_fase3,
  
  aes(
    x = pred_sar,
    y = residuos_sar
  )
) +
  
  geom_point(
    alpha = 0.5,
    size = 1.5
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Diagnóstico de residuos del modelo SAR",
    x = "Valores ajustados",
    y = "Residuos"
  )


print(g_residuos)


ggsave(
  file.path(
    carpeta_figuras,
    "Figura_Diagnostico_Residuos_SAR.png"
  ),
  g_residuos,
  width = 10,
  height = 7,
  dpi = 300
)


# =============================================================================
# 36. MORAN DE RESIDUOS SAR
# =============================================================================

set.seed(123)

moran_residuos <- spdep::moran.mc(
  
  residuals(
    modelo_sar
  ),
  
  lw_mod,
  
  nsim = 999,
  
  zero.policy = TRUE
)


tabla_residuos <- data.frame(
  
  Estadistico = "Moran I de residuos SAR",
  
  Moran_I = as.numeric(
    moran_residuos$statistic
  ),
  
  p_valor = moran_residuos$p.value,
  
  Interpretacion = ifelse(
    
    moran_residuos$p.value < 0.05,
    
    "Persistencia de autocorrelación espacial residual",
    
    "No se detecta autocorrelación espacial residual significativa"
  )
)


print(tabla_residuos)


# =============================================================================
# 37. EXPORTAR TODOS LOS RESULTADOS A EXCEL
# =============================================================================

# ============================================================
# EXPORTACIÓN COMPLETA DE TABLAS A EXCEL
# ============================================================

archivo_excel_final <- file.path(
  carpeta_salida,
  "RESULTADOS_COMPLETOS_ANALISIS_ESPACIAL_ENA.xlsx"
)

wb <- createWorkbook()

# ------------------------------------------------------------
# TABLA 1 - COBERTURA
# ------------------------------------------------------------

addWorksheet(
  wb,
  "01_Cobertura"
)

writeData(
  wb,
  "01_Cobertura",
  tabla_cobertura
)

# ------------------------------------------------------------
# TABLA 2 - PANEL ANUAL
# ------------------------------------------------------------

addWorksheet(
  wb,
  "02_Panel_Anual"
)

writeData(
  wb,
  "02_Panel_Anual",
  tabla_panel_anual
)

# ------------------------------------------------------------
# TABLA 3 - AÑOS ESPACIALES
# ------------------------------------------------------------

addWorksheet(
  wb,
  "03_Anios_Espaciales"
)

writeData(
  wb,
  "03_Anios_Espaciales",
  tabla_anios_espaciales
)

# ------------------------------------------------------------
# TABLA 4 - MORAN GLOBAL
# ------------------------------------------------------------

addWorksheet(
  wb,
  "04_Moran_Global"
)

writeData(
  wb,
  "04_Moran_Global",
  cuadro_1
)

# ------------------------------------------------------------
# TABLA 5 - LISA
# ------------------------------------------------------------

addWorksheet(
  wb,
  "05_LISA"
)

writeData(
  wb,
  "05_LISA",
  tabla_lisa_anual
)

# ------------------------------------------------------------
# TABLA 6 - EVOLUCIÓN
# ------------------------------------------------------------

addWorksheet(
  wb,
  "06_Evolucion"
)

writeData(
  wb,
  "06_Evolucion",
  tabla_evolucion
)

# ------------------------------------------------------------
# TABLA 7 - MODELOS
# ------------------------------------------------------------

addWorksheet(
  wb,
  "07_Modelos"
)

writeData(
  wb,
  "07_Modelos",
  tabla_modelos
)

# ------------------------------------------------------------
# TABLA 8 - IMPACTOS SAR
# ------------------------------------------------------------

addWorksheet(
  wb,
  "08_Impactos_SAR"
)

writeData(
  wb,
  "08_Impactos_SAR",
  tabla_impactos
)


# ============================================================
# FORMATO GENERAL
# ============================================================

for (
  hoja in names(wb)
) {
  
  setColWidths(
    wb,
    hoja,
    cols = 1:20,
    widths = "auto"
  )
  
  freezePane(
    wb,
    hoja,
    firstRow = TRUE
  )
}


# ============================================================
# GUARDAR EXCEL FINAL
# ============================================================

saveWorkbook(
  wb,
  archivo_excel_final,
  overwrite = TRUE
)


cat("\n")
cat("============================================================\n")
cat("             ANÁLISIS FINALIZADO CORRECTAMENTE\n")
cat("============================================================\n")

cat(
  "\nResultados guardados en:\n",
  carpeta_salida,
  "\n\n"
)

cat(
  "Excel principal:\n",
  archivo_excel_final,
  "\n\n"
)

cat(
  "Figuras guardadas en:\n",
  carpeta_figuras,
  "\n\n"
)

cat(
  "Tablas individuales guardadas en:\n",
  carpeta_tablas,
  "\n\n"
)

cat(
  "Modelos guardados en:\n",
  carpeta_modelos,
  "\n"
)
# =============================================================================
# 38. GUARDAR MODELOS
# =============================================================================

saveRDS(
  modelo_ols,
  file.path(
    carpeta_modelos,
    "Modelo_MCO.rds"
  )
)


saveRDS(
  modelo_sar,
  file.path(
    carpeta_modelos,
    "Modelo_SAR.rds"
  )
)


saveRDS(
  impactos_sar,
  file.path(
    carpeta_modelos,
    "Impactos_SAR.rds"
  )
)


# =============================================================================
# 39. GUARDAR TODOS LOS OBJETOS
# =============================================================================

save(
  
  data_ena_raw,
  
  data_ena_clean,
  
  data_panel_distrital,
  
  spatial_panel,
  
  cuadro_1,
  
  tabla_lisa_anual,
  
  modelo_ols,
  
  modelo_sar,
  
  impactos_sar,
  
  tabla_impactos,
  
  file = file.path(
    carpeta_salida,
    "Resultados_Completos_ENA_Analisis_Espacial.RData"
  )
)


# =============================================================================
# 40. REPORTE FINAL
# =============================================================================

cat("\n\n")
cat("============================================================\n")
cat("             ANÁLISIS FINALIZADO CORRECTAMENTE\n")
cat("============================================================\n\n")

cat(
  "Carpeta principal:\n",
  carpeta_salida,
  "\n\n"
)

cat(
  "Excel de resultados:\n",
  ruta_excel,
  "\n\n"
)

cat(
  "Año utilizado para el modelo SAR:",
  anio_modelo,
  "\n\n"
)

cat(
  "Número de observaciones SAR:",
  nrow(data_modelo_fase3),
  "\n\n"
)

cat(
  "Figuras guardadas en:\n",
  carpeta_figuras,
  "\n\n"
)

cat(
  "Modelos guardados en:\n",
  carpeta_modelos,
  "\n\n"
)

cat("============================================================\n")
cat("                    FIN DEL ANÁLISIS\n")
cat("============================================================\n")
