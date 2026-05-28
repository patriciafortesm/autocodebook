# =============================================================================
# autocodebook — Helpers Spark-safe para agregacao no relatorio
# =============================================================================
# REGRA DE OURO: nenhuma funcao aqui coleta dados linha-a-linha.
# Toda agregacao acontece no Spark/dplyr, so o sumario (poucas linhas)
# eh trazido pro R com collect().
# =============================================================================

#' @keywords internal
#' @noRd

# -----------------------------------------------------------------------------
# .infer_data_class — heuristica de classe de variavel (numeric / categorical /
# date / id-like). Operates sobre o schema, nao sobre os dados.
# -----------------------------------------------------------------------------

.infer_data_class <- function(.data, var, id_var = NULL) {

  # Pega tipo via schema (sparklyr) ou class (local)
  if (inherits(.data, "tbl_spark")) {
    schema <- tryCatch(sparklyr::sdf_schema(.data),
                       error = function(e) NULL)
    if (is.null(schema) || is.null(schema[[var]])) return("unknown")
    tp <- tolower(schema[[var]]$type)
  } else {
    cls <- class(.data[[var]])[1]
    tp <- switch(cls,
                 "numeric" = "double", "integer" = "integer",
                 "Date" = "date", "POSIXct" = "timestamp",
                 "POSIXt" = "timestamp",
                 "character" = "string", "factor" = "string",
                 "logical" = "boolean",
                 cls)
    tp <- tolower(tp)
  }

  if (!is.null(id_var) && identical(var, id_var)) return("id")

  if (grepl("date|timestamp", tp))                return("date")
  if (grepl("double|float|decimal", tp))          return("numeric")
  if (grepl("int|long|short|byte", tp))           return("integer")
  if (grepl("bool", tp))                          return("logical")
  if (grepl("string|char", tp))                   return("character")
  "unknown"
}

# -----------------------------------------------------------------------------
# .agg_summary_numeric — min/max/mean/sd/median/p25/p75/n_missing
# Roda inteiramente no Spark/dplyr; collect retorna 1 linha.
# -----------------------------------------------------------------------------

.agg_summary_numeric <- function(.data, var) {
  v <- rlang::sym(var)

  out <- .data %>%
    dplyr::summarise(
      n         = dplyr::n(),
      n_missing = sum(as.integer(is.na(!!v)), na.rm = TRUE),
      mean_v    = mean(!!v, na.rm = TRUE),
      sd_v      = stats::sd(!!v, na.rm = TRUE),
      min_v     = min(!!v, na.rm = TRUE),
      max_v     = max(!!v, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  # Quartis: percentile_approx no Spark, quantile local.
  qs <- tryCatch({
    if (inherits(.data, "tbl_spark")) {
      sql_p25 <- paste0("percentile_approx(`", var, "`, 0.25)")
      sql_p50 <- paste0("percentile_approx(`", var, "`, 0.50)")
      sql_p75 <- paste0("percentile_approx(`", var, "`, 0.75)")
      .data %>%
        dplyr::filter(!is.na(!!v)) %>%
        dplyr::summarise(
          q1     = dplyr::sql(!!sql_p25),
          median = dplyr::sql(!!sql_p50),
          q3     = dplyr::sql(!!sql_p75)
        ) %>%
        dplyr::collect()
    } else {
      .data %>%
        dplyr::filter(!is.na(!!v)) %>%
        dplyr::summarise(
          q1     = stats::quantile(!!v, 0.25, na.rm = TRUE, names = FALSE),
          median = stats::quantile(!!v, 0.50, na.rm = TRUE, names = FALSE),
          q3     = stats::quantile(!!v, 0.75, na.rm = TRUE, names = FALSE)
        ) %>%
        dplyr::collect()
    }
  }, error = function(e) NULL)

  if (!is.null(qs) && nrow(qs) == 1) {
    out$q1     <- qs$q1
    out$median <- qs$median
    out$q3     <- qs$q3
    out$min    <- out$min_v
    out$max    <- out$max_v
  }

  out
}

# -----------------------------------------------------------------------------
# .agg_histogram_numeric — bin via ntile no Spark; collect ~50 linhas
# -----------------------------------------------------------------------------

.agg_histogram_numeric <- function(.data, var, n_bins = 30) {
  v <- rlang::sym(var)

  # Pega min/max primeiro
  rng <- .data %>%
    dplyr::filter(!is.na(!!v)) %>%
    dplyr::summarise(min_v = min(!!v, na.rm = TRUE),
                     max_v = max(!!v, na.rm = TRUE)) %>%
    dplyr::collect()

  if (nrow(rng) == 0 || !is.finite(rng$min_v) || !is.finite(rng$max_v) ||
      rng$min_v == rng$max_v) {
    return(tibble::tibble(bin_low = numeric(), bin_high = numeric(),
                          n = integer()))
  }

  min_v <- rng$min_v
  max_v <- rng$max_v
  width <- (max_v - min_v) / n_bins
  max_idx <- n_bins - 1L
  breaks <- seq(min_v, max_v, length.out = n_bins + 1)

  # Calcula bin via floor((x - min)/width); cap em max_idx
  # Usa dplyr::if_else (traduzido para Spark) em vez de pmin (que tambem
  # traduz, mas alguns backends sao instaveis).
  hist_df <- .data %>%
    dplyr::filter(!is.na(!!v)) %>%
    dplyr::mutate(
      bin_idx_raw = as.integer(floor((!!v - !!min_v) / !!width)),
      bin_idx     = dplyr::if_else(bin_idx_raw > !!max_idx,
                                   !!max_idx, bin_idx_raw)
    ) %>%
    dplyr::count(bin_idx) %>%
    dplyr::collect()

  # Preenche bins vazios e adiciona bordas
  full <- tibble::tibble(
    bin_idx  = 0:(n_bins - 1L),
    bin_low  = breaks[-length(breaks)],
    bin_high = breaks[-1]
  )
  out <- dplyr::left_join(full, hist_df, by = "bin_idx") %>%
    dplyr::mutate(n = ifelse(is.na(n), 0L, as.integer(n))) %>%
    dplyr::select(-bin_idx)

  out
}

# -----------------------------------------------------------------------------
# .agg_categorical — contagem por categoria; topo top_n
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .agg_cardinality — conta n_distinct (so valores nao-NA) de uma variavel.
# Barato no Spark (1 numero). Usado pra decidir se uma categorica e de
# alta cardinalidade (ex: cod_munic com milhares de niveis).
# -----------------------------------------------------------------------------

.agg_cardinality <- function(.data, var) {
  v <- rlang::sym(var)
  out <- .data %>%
    dplyr::summarise(
      n_distinct = dplyr::n_distinct(!!v, na.rm = TRUE),
      n_total    = dplyr::n(),
      n_miss     = sum(as.integer(is.na(!!v)), na.rm = TRUE)
    ) %>%
    dplyr::collect()
  # pct_missing derivado (sem nova varredura) — permite reaproveitar como
  # missingness total, evitando um job Spark separado por variavel.
  out$pct_missing <- if (out$n_total[1] > 0) out$n_miss[1] / out$n_total[1] else 0
  out
}

# Converte a saida de .agg_cardinality no formato de .agg_missingness_total
.card_to_miss_total <- function(card) {
  if (is.null(card)) return(NULL)
  tibble::tibble(
    n           = card$n_total[1],
    n_missing   = card$n_miss[1],
    pct_missing = card$pct_missing[1]
  )
}

# -----------------------------------------------------------------------------
# .agg_high_cardinality — para variaveis com muitos niveis: retorna
# n_distinct, n_miss (%), e o top-3 mais frequentes. Tudo agregado no Spark.
# -----------------------------------------------------------------------------

.agg_high_cardinality <- function(.data, var, top_k = 3L) {
  v <- rlang::sym(var)

  card <- .agg_cardinality(.data, var)

  top <- .data %>%
    dplyr::filter(!is.na(!!v)) %>%
    dplyr::count(!!v, sort = TRUE) %>%
    head(top_k) %>%
    dplyr::collect()
  names(top)[1] <- "category"
  top$category <- as.character(top$category)

  list(card = card, top = top)
}

.agg_categorical <- function(.data, var, top_n = 20) {
  v <- rlang::sym(var)

  out <- .data %>%
    dplyr::mutate(
      cat_label = dplyr::coalesce(as.character(!!v), "(NA)")
    ) %>%
    dplyr::count(cat_label, sort = TRUE) %>%
    dplyr::collect()

  # Se tiver mais de top_n niveis, agrega o resto em "(outros)"
  if (nrow(out) > top_n) {
    main   <- out[seq_len(top_n - 1L), ]
    others <- tibble::tibble(cat_label = "(outros)",
                             n    = sum(out$n[top_n:nrow(out)]))
    out    <- dplyr::bind_rows(main, others)
  }

  out <- out %>% dplyr::rename(category = cat_label)
  out
}

# -----------------------------------------------------------------------------
# .agg_date — histograma temporal por ano (ou mes, dependendo do range)
# -----------------------------------------------------------------------------

.agg_date <- function(.data, var) {
  v <- rlang::sym(var)

  rng <- .data %>%
    dplyr::filter(!is.na(!!v)) %>%
    dplyr::summarise(min_d = min(!!v, na.rm = TRUE),
                     max_d = max(!!v, na.rm = TRUE),
                     n_miss = sum(as.integer(is.na(!!v)), na.rm = TRUE)) %>%
    dplyr::collect()

  if (nrow(rng) == 0) {
    return(list(
      range = tibble::tibble(min_d = NA, max_d = NA, n_miss = 0L),
      hist  = tibble::tibble(period = character(), n = integer())
    ))
  }

  # Agrega por ano. Para Spark, usamos sparklyr::year() via SQL.
  # Para data frames locais, usamos as.integer(format(...)).
  hist_df <- if (inherits(.data, "tbl_spark")) {
    .data %>%
      dplyr::filter(!is.na(!!v)) %>%
      dplyr::mutate(year_v = as.integer(
        # year() do Hive funciona em DATE/TIMESTAMP
        dplyr::sql(paste0("year(`", var, "`)"))
      )) %>%
      dplyr::count(year_v) %>%
      dplyr::collect() %>%
      dplyr::arrange(year_v) %>%
      dplyr::rename(period = year_v)
  } else {
    .data %>%
      dplyr::filter(!is.na(!!v)) %>%
      dplyr::mutate(year_v = as.integer(format(!!v, "%Y"))) %>%
      dplyr::count(year_v) %>%
      dplyr::arrange(year_v) %>%
      dplyr::rename(period = year_v)
  }

  list(range = rng, hist = hist_df)
}

# -----------------------------------------------------------------------------
# .agg_intra_id_variation — para cada variavel, conta n_distinct por ID.
# Variaveis "fixas" tipo sexo devem ter n_distinct = 1 sempre.
# n_distinct > 1 = ou variavel realmente varia, ou tem erro de linkage.
# -----------------------------------------------------------------------------

.agg_intra_id_variation <- function(.data, var, id_var) {
  v  <- rlang::sym(var)
  id <- rlang::sym(id_var)

  out <- .data %>%
    dplyr::filter(!is.na(!!v)) %>%
    dplyr::group_by(!!id) %>%
    dplyr::summarise(n_distinct_v = dplyr::n_distinct(!!v),
                     .groups = "drop") %>%
    dplyr::count(n_distinct_v) %>%
    dplyr::collect() %>%
    dplyr::arrange(n_distinct_v) %>%
    dplyr::rename(n_distinct = n_distinct_v)

  out
}

# -----------------------------------------------------------------------------
# .agg_var_by_time — distribuicao de uma variavel por janela temporal.
# Para numericas: media + sd por periodo.
# Para categoricas: proporcao por categoria por periodo.
# -----------------------------------------------------------------------------

.agg_var_by_time_numeric <- function(.data, var, time_var) {
  v <- rlang::sym(var); t <- rlang::sym(time_var)

  .data %>%
    dplyr::filter(!is.na(!!v), !is.na(!!t)) %>%
    dplyr::group_by(!!t) %>%
    dplyr::summarise(
      mean_v = mean(!!v, na.rm = TRUE),
      sd_v   = stats::sd(!!v, na.rm = TRUE),
      n      = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(!!t)
}

.agg_var_by_time_categorical <- function(.data, var, time_var) {
  v <- rlang::sym(var); t <- rlang::sym(time_var)

  .data %>%
    dplyr::mutate(cat_label = dplyr::coalesce(as.character(!!v), "(NA)")) %>%
    dplyr::filter(!is.na(!!t)) %>%
    dplyr::count(!!t, cat_label) %>%
    dplyr::collect() %>%
    dplyr::group_by(!!t) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(category = cat_label) %>%
    dplyr::arrange(!!t)
}

# -----------------------------------------------------------------------------
# .agg_missingness_by_time — % de missing por periodo, util pra detectar
# variaveis que so foram medidas a partir de certo ano.
# -----------------------------------------------------------------------------

.agg_missingness_by_time <- function(.data, var, time_var) {
  v <- rlang::sym(var); t <- rlang::sym(time_var)

  out <- .data %>%
    dplyr::filter(!is.na(!!t)) %>%
    dplyr::group_by(!!t) %>%
    dplyr::summarise(
      n         = dplyr::n(),
      n_missing = sum(as.integer(is.na(!!v)), na.rm = TRUE),
      pct_missing = sum(as.integer(is.na(!!v)), na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(!!t)

  # Padroniza nome da coluna temporal como "period" para os plots
  names(out)[1] <- "period"
  out
}

# -----------------------------------------------------------------------------
# .agg_missingness_total — % missing global de uma variavel (1 numero)
# -----------------------------------------------------------------------------

.agg_missingness_total <- function(.data, var) {
  v <- rlang::sym(var)
  .data %>%
    dplyr::summarise(
      n         = dplyr::n(),
      n_missing = sum(as.integer(is.na(!!v)), na.rm = TRUE),
      pct_missing = sum(as.integer(is.na(!!v)), na.rm = TRUE) / dplyr::n()
    ) %>%
    dplyr::collect()
}

# -----------------------------------------------------------------------------
# .agg_categorical_by_time — proporcao de cada categoria por periodo
# Output: tibble (period, category, n, prop) — usa nome generico "period"
# pro plot ser agnostico ao nome da coluna temporal.
# -----------------------------------------------------------------------------

.agg_categorical_by_time <- function(.data, var, time_var, top_n = 8) {
  v <- rlang::sym(var)
  t <- rlang::sym(time_var)

  # Calcula top_n categorias globais (pra evitar legendas enormes em
  # variaveis com muitos niveis). Outras categorias vao pra "(outros)".
  top_cats <- .data %>%
    dplyr::filter(!is.na(!!v)) %>%
    dplyr::mutate(cat_label = as.character(!!v)) %>%
    dplyr::count(cat_label, sort = TRUE) %>%
    dplyr::collect() %>%
    utils::head(top_n) %>%
    dplyr::pull(cat_label)

  # Agrega por periodo+categoria (incluindo NA como "(NA)")
  agg <- .data %>%
    dplyr::filter(!is.na(!!t)) %>%
    dplyr::mutate(cat_raw = dplyr::if_else(is.na(!!v),
                                           "(NA)",
                                           as.character(!!v))) %>%
    dplyr::count(!!t, cat_raw) %>%
    dplyr::collect()

  # Renomeia coluna temporal pra "period"
  names(agg)[1] <- "period"

  # Agrupa categorias fora do top_n em "(outros)"
  agg <- agg %>%
    dplyr::mutate(
      category = dplyr::if_else(
        cat_raw %in% c(top_cats, "(NA)"), cat_raw, "(outros)"
      )
    ) %>%
    dplyr::group_by(period, category) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    dplyr::group_by(period) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(period, category)

  agg
}

# -----------------------------------------------------------------------------
# .agg_numeric_by_time_quantiles — mediana + p25 + p75 por periodo
# Spark traduz median/percentile_approx via SQL. Para local usa stats::quantile.
# -----------------------------------------------------------------------------

.agg_numeric_by_time_quantiles <- function(.data, var, time_var) {
  v <- rlang::sym(var); t <- rlang::sym(time_var)

  if (inherits(.data, "tbl_spark")) {
    # No Spark, usar percentile_approx (Hive UDF) que sparklyr expõe via SQL
    sql_p25 <- paste0("percentile_approx(`", var, "`, 0.25)")
    sql_p50 <- paste0("percentile_approx(`", var, "`, 0.50)")
    sql_p75 <- paste0("percentile_approx(`", var, "`, 0.75)")
    out <- .data %>%
      dplyr::filter(!is.na(!!v), !is.na(!!t)) %>%
      dplyr::group_by(!!t) %>%
      dplyr::summarise(
        n      = dplyr::n(),
        p25    = dplyr::sql(!!sql_p25),
        median = dplyr::sql(!!sql_p50),
        p75    = dplyr::sql(!!sql_p75),
        mean_v = mean(!!v, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::collect()
  } else {
    out <- .data %>%
      dplyr::filter(!is.na(!!v), !is.na(!!t)) %>%
      dplyr::group_by(!!t) %>%
      dplyr::summarise(
        n      = dplyr::n(),
        p25    = stats::quantile(!!v, 0.25, na.rm = TRUE, names = FALSE),
        median = stats::quantile(!!v, 0.50, na.rm = TRUE, names = FALSE),
        p75    = stats::quantile(!!v, 0.75, na.rm = TRUE, names = FALSE),
        mean_v = mean(!!v, na.rm = TRUE),
        .groups = "drop"
      )
  }

  names(out)[1] <- "period"
  out %>% dplyr::arrange(period)
}

# -----------------------------------------------------------------------------
# .agg_date_by_time — frequencia de uma variavel data por periodo (ja existe
# .agg_date global; aqui agregamos por time_var em vez de pelo proprio ano da
# data). Util quando time_var eh "wave" e a data eh outra (ex: dt_evento).
# -----------------------------------------------------------------------------

.agg_date_by_time <- function(.data, var, time_var) {
  v <- rlang::sym(var); t <- rlang::sym(time_var)
  .data %>%
    dplyr::filter(!is.na(!!v), !is.na(!!t)) %>%
    dplyr::group_by(!!t) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::collect() %>%
    dplyr::rename(period = !!time_var) %>%
    dplyr::arrange(period)
}
