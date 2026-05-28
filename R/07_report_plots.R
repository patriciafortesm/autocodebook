# =============================================================================
# autocodebook — Plots do relatorio
# =============================================================================
# Usa ggplot2 como base (compativel com patchwork). tidyplots opcional como
# fallback alternativo para plots individuais; mas a montagem em paineis
# (transversal e longitudinal) usa patchwork direto.
#
# REGRA: todas as funcoes recebem tibbles JA AGREGADAS. Nenhum plot toca em
# tbl_spark.
# =============================================================================

#' @keywords internal
#' @noRd

.has_ggplot2   <- function() requireNamespace("ggplot2",   quietly = TRUE)
.has_patchwork <- function() requireNamespace("patchwork", quietly = TRUE)
.has_scales    <- function() requireNamespace("scales",    quietly = TRUE)

.check_plot_deps <- function() {
  if (!.has_ggplot2()) {
    stop("[autocodebook] Pacote 'ggplot2' necessario para o relatorio. ",
         "Instale com: install.packages('ggplot2')",
         call. = FALSE)
  }
}

# -----------------------------------------------------------------------------
# Tema padrao do relatorio (compacto, neutro)
# -----------------------------------------------------------------------------

.theme_report <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = base_size + 1,
                                               face = "bold"),
      plot.subtitle    = ggplot2::element_text(size = base_size - 1,
                                               color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(size = base_size - 1),
      legend.text      = ggplot2::element_text(size = base_size - 1),
      legend.key.size  = grid::unit(0.4, "cm"),
      axis.title       = ggplot2::element_text(size = base_size - 1),
      axis.text        = ggplot2::element_text(size = base_size - 1)
    )
}

# Paleta categórica acessivel (Okabe-Ito) — boa para 8+ categorias
.palette_categorical <- c(
  "#0072B2", "#E69F00", "#009E73", "#CC79A7",
  "#56B4E9", "#D55E00", "#F0E442", "#999999",
  "#882255", "#117733"
)

.fmt_big <- function() {
  function(x) format(x, big.mark = ",", scientific = FALSE)
}

# =============================================================================
# FLUXOGRAMA DE ELEGIBILIDADE
# =============================================================================

#' @keywords internal
.plot_eligibility_funnel <- function(tr) {
  .check_plot_deps()
  if (nrow(tr) == 0) return(NULL)

  df <- tr %>%
    dplyr::mutate(
      step_ord = factor(step, levels = rev(step)),
      label_n  = format(n_ids, big.mark = ",", scientific = FALSE),
      label_rm = ifelse(n_removed > 0,
                        paste0(" (-", format(n_removed, big.mark = ","), ")"),
                        "")
    )

  ggplot2::ggplot(df, ggplot2::aes(x = n_ids, y = step_ord)) +
    ggplot2::geom_col(fill = "#0072B2", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(label_n, label_rm)),
                       hjust = -0.05, size = 3, color = "gray20") +
    ggplot2::scale_x_continuous(
      labels = .fmt_big(),
      expand = ggplot2::expansion(mult = c(0, 0.30))
    ) +
    ggplot2::labs(title = "Fluxograma de elegibilidade",
                  x = "N individuos unicos", y = NULL) +
    .theme_report() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# =============================================================================
# PAINEL TRANSVERSAL: distribuicao + tabela compacta de stats
# =============================================================================

#' @keywords internal
.plot_var_panel_cross <- function(agg, var, var_class, label = NULL,
                                  miss_total = NULL) {

  .check_plot_deps()
  ttl <- if (!is.null(label) && nzchar(label)) label else var

  # PAINEL A: distribuicao principal
  p_dist <- switch(var_class,
    numeric   = .panel_hist_numeric(agg$hist, var),
    integer   = .panel_hist_numeric(agg$hist, var),
    date      = .panel_hist_date(agg$hist, var),
    character = .panel_bar_categorical(agg$cat, var),
    logical   = .panel_bar_categorical(agg$cat, var),
    .empty_panel("Classe nao suportada")
  )

  # PAINEL B: stats compactos (tibble -> tabela visual)
  p_stats <- .panel_stats_text(agg$summary, var_class, miss_total)

  if (.has_patchwork()) {
    out <- patchwork::wrap_plots(
      list(p_dist, p_stats),
      ncol = 2, widths = c(2.5, 1)
    ) +
      patchwork::plot_annotation(
        title = ttl,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 11, face = "bold")
        )
      )
    return(out)
  }

  # fallback sem patchwork: so a distribuicao
  p_dist + ggplot2::labs(title = ttl)
}

# =============================================================================
# PAINEL LONGITUDINAL: 3 paineis lado-a-lado
#   A. Distribuicao ao longo do tempo (estilo depende da classe)
#   B. Missingness por periodo + linha do total
#   C. Variacao intra-ID (n_distinct por individuo)
# =============================================================================

#' @keywords internal
.plot_var_panel_longitudinal <- function(agg, var, var_class, time_var,
                                          label = NULL, miss_total = NULL,
                                          high_card = FALSE, hc = NULL) {

  .check_plot_deps()
  ttl <- if (!is.null(label) && nzchar(label)) label else var

  # PAINEL A: distribuicao por tempo (ou aviso se alta cardinalidade)
  p_dist <- if (isTRUE(high_card)) {
    .mini_high_card_notice(hc, var, label)
  } else {
    switch(var_class,
      numeric   = .panel_numeric_quantiles_by_time(agg$by_time, var, time_var),
      integer   = .panel_numeric_quantiles_by_time(agg$by_time, var, time_var),
      character = .panel_categorical_by_time(agg$by_time, var, time_var),
      logical   = .panel_categorical_by_time(agg$by_time, var, time_var),
      date      = .panel_date_by_time(agg$by_time, var, time_var),
      .empty_panel("Classe nao suportada")
    )
  }

  # PAINEL B: missingness por tempo + linha do total
  p_miss <- .panel_missingness_by_time(agg$miss_by_time, var, time_var,
                                        miss_total = miss_total)

  # PAINEL C: variacao intra-ID
  p_intra <- .panel_intra_id(agg$intra, var)

  if (.has_patchwork()) {
    out <- patchwork::wrap_plots(
      list(p_dist, p_miss, p_intra),
      ncol = 3, widths = c(1.2, 1, 0.9)
    ) +
      patchwork::plot_annotation(
        title    = ttl,
        subtitle = sprintf("Classe: %s", var_class),
        theme = ggplot2::theme(
          plot.title    = ggplot2::element_text(size = 11, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 9, color = "gray40")
        )
      )
    return(out)
  }

  # fallback: 3 plots em coluna
  list(dist = p_dist, miss = p_miss, intra = p_intra)
}

# =============================================================================
# PAINEIS INDIVIDUAIS (helpers internos chamados pelos painel-wrappers)
# =============================================================================

# =============================================================================
# MINI-PAINEIS TRANSVERSAIS (para grid 3-col)
#   Cada variavel = 1 mini-painel compacto com titulo embutido.
#   Continua  -> boxplot horizontal
#   Categorica -> barras de proporcao (%)
# =============================================================================

#' @keywords internal
.plot_var_mini_cross <- function(agg, var, var_class, label = NULL,
                                  miss_total = NULL) {
  .check_plot_deps()
  ttl <- if (!is.null(label) && nzchar(label)) label else var

  miss_pct <- if (!is.null(miss_total) && nrow(miss_total) == 1) {
    sprintf("%.1f%% miss", 100 * miss_total$pct_missing)
  } else ""

  p <- switch(var_class,
    numeric   = .mini_boxplot(agg, var),
    integer   = .mini_boxplot(agg, var),
    character = .mini_bar_prop(agg$cat, var),
    logical   = .mini_bar_prop(agg$cat, var),
    date      = .mini_date_hist(agg, var),
    .empty_panel("?")
  )

  p +
    ggplot2::labs(title = ttl, subtitle = miss_pct) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 9, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 7, color = "gray50")
    )
}

# ---- mini boxplot (continua) -----------------------------------------------
# Usa o summary agregado (min/q1/median/q3/max) — calculado no Spark.

#' @keywords internal
.mini_boxplot <- function(agg, var) {
  s <- agg$summary
  if (is.null(s) || nrow(s) == 0) return(.empty_panel("Sem dados"))

  # summary tem colunas: min, q1, median, q3, max (nomes podem variar)
  nm <- names(s)
  pick <- function(cands) {
    hit <- cands[cands %in% nm]
    if (length(hit) > 0) s[[hit[1]]][1] else NA_real_
  }
  ymin <- pick(c("min", "minimum", "p0"))
  q1   <- pick(c("q1", "p25", "quartil1"))
  med  <- pick(c("median", "p50", "mediana"))
  q3   <- pick(c("q3", "p75", "quartil3"))
  ymax <- pick(c("max", "maximum", "p100"))

  # fallback: se nao houver quartis, usa hist pra aproximar
  if (any(is.na(c(q1, med, q3)))) {
    return(.panel_hist_numeric(agg$hist, var))
  }

  bx <- data.frame(x = var, ymin = ymin, lower = q1, middle = med,
                   upper = q3, ymax = ymax)

  ggplot2::ggplot(bx) +
    ggplot2::geom_boxplot(
      ggplot2::aes(x = x, ymin = ymin, lower = lower, middle = middle,
                   upper = upper, ymax = ymax),
      stat = "identity", fill = "#56B4E9", color = "#0072B2",
      width = 0.5, linewidth = 0.3
    ) +
    ggplot2::scale_y_continuous(labels = .fmt_big()) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL) +
    .theme_report() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}

# ---- aviso de alta cardinalidade -------------------------------------------

#' @keywords internal
.mini_high_card_notice <- function(hc, var, label = NULL) {
  msg <- "Gr\u00e1fico de propor\u00e7\u00e3o\nn\u00e3o exibido\n(alta cardinalidade)"

  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                      hjust = 0.5, vjust = 0.5, size = 2.9,
                      color = "#888780", lineheight = 1.2) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}

#' @keywords internal
.mini_high_card_notice_titled <- function(hc, var, label = NULL) {
  ttl <- if (!is.null(label) && nzchar(label)) label else var
  .mini_high_card_notice(hc, var, label) +
    ggplot2::labs(title = ttl) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold")
    )
}

#' @keywords internal
.mini_date_hist <- function(agg, var) {
  h <- agg$hist
  if (is.null(h) || nrow(h) == 0 || !("period" %in% names(h))) {
    return(.empty_panel("Sem dados"))
  }
  h$period <- suppressWarnings(as.integer(h$period))
  h <- h[!is.na(h$period), , drop = FALSE]
  if (nrow(h) == 0) return(.empty_panel("Sem dados"))

  ggplot2::ggplot(h, ggplot2::aes(x = period, y = n)) +
    ggplot2::geom_col(fill = "#56B4E9", color = "#0072B2", linewidth = 0.2,
                      width = 0.85) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
    ggplot2::scale_y_continuous(labels = .fmt_big()) +
    ggplot2::labs(x = NULL, y = NULL) +
    .theme_report()
}

# ---- mini barras de proporcao (categorica) ---------------------------------

#' @keywords internal
.mini_bar_prop <- function(cat_df, var) {
  if (is.null(cat_df) || nrow(cat_df) == 0) return(.empty_panel("Sem dados"))
  df <- cat_df %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(prop)) %>%
    dplyr::mutate(category = factor(category, levels = rev(category)))

  ggplot2::ggplot(df, ggplot2::aes(x = prop, y = category)) +
    ggplot2::geom_col(fill = "#0072B2") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f%%", 100 * prop)),
      hjust = -0.1, size = 2.3, color = "gray30"
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    .theme_report()
}

# ---- Hist numerico (transversal) -------------------------------------------

#' @keywords internal
.panel_hist_numeric <- function(hist_df, var) {
  if (nrow(hist_df) == 0) return(.empty_panel("Sem dados"))
  df <- hist_df %>%
    dplyr::mutate(bin_mid = (bin_low + bin_high) / 2,
                  width   = bin_high - bin_low)

  ggplot2::ggplot(df, ggplot2::aes(x = bin_mid, y = n)) +
    ggplot2::geom_col(width = df$width * 0.95, fill = "#0072B2") +
    ggplot2::scale_y_continuous(labels = .fmt_big()) +
    ggplot2::labs(x = var, y = "Freq") +
    .theme_report()
}

# ---- Date hist (transversal) -----------------------------------------------

#' @keywords internal
.panel_hist_date <- function(date_df, var) {
  if (nrow(date_df) == 0) return(.empty_panel("Sem dados"))
  ggplot2::ggplot(date_df, ggplot2::aes(x = period, y = n)) +
    ggplot2::geom_col(fill = "#009E73") +
    ggplot2::scale_y_continuous(labels = .fmt_big()) +
    ggplot2::labs(x = var, y = "Freq") +
    .theme_report()
}

# ---- Bar categorico (transversal) ------------------------------------------

#' @keywords internal
.panel_bar_categorical <- function(cat_df, var) {
  if (nrow(cat_df) == 0) return(.empty_panel("Sem dados"))
  df <- cat_df %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(category = factor(category, levels = rev(category)))

  ggplot2::ggplot(df, ggplot2::aes(x = n, y = category)) +
    ggplot2::geom_col(fill = "#0072B2") +
    ggplot2::scale_x_continuous(labels = .fmt_big()) +
    ggplot2::labs(x = "Freq", y = NULL) +
    .theme_report()
}

# ---- Stats text (transversal) ----------------------------------------------

#' @keywords internal
.panel_stats_text <- function(summary, var_class, miss_total = NULL) {

  if (is.null(summary) || (is.data.frame(summary) && nrow(summary) == 0)) {
    return(.empty_panel("Sem stats"))
  }

  lines <- character()
  if (var_class %in% c("numeric", "integer")) {
    lines <- c(
      lines,
      sprintf("N     : %s", format(summary$n,        big.mark = ",")),
      sprintf("Miss  : %s", format(summary$n_missing, big.mark = ",")),
      sprintf("Mean  : %s", .fmt_num(summary$mean_v)),
      sprintf("SD    : %s", .fmt_num(summary$sd_v)),
      sprintf("Min   : %s", .fmt_num(summary$min_v)),
      sprintf("Max   : %s", .fmt_num(summary$max_v))
    )
  } else if (var_class == "date") {
    lines <- c(
      lines,
      sprintf("Min   : %s", as.character(summary$min_d)),
      sprintf("Max   : %s", as.character(summary$max_d)),
      sprintf("Miss  : %s", format(summary$n_miss,   big.mark = ","))
    )
  } else if (var_class %in% c("character", "logical")) {
    # summary aqui eh a tabela categorica; mostra top-5 + total
    total <- sum(summary$n)
    top   <- utils::head(summary, 5)
    lines <- c(sprintf("N     : %s", format(total, big.mark = ",")))
    for (i in seq_len(nrow(top))) {
      lines <- c(lines, sprintf("  %s : %s",
                                substr(as.character(top$category[i]), 1, 14),
                                format(top$n[i], big.mark = ",")))
    }
  }

  if (!is.null(miss_total) && nrow(miss_total) == 1) {
    lines <- c(lines,
               sprintf("Miss%%: %s",
                       scales::percent(miss_total$pct_missing, accuracy = 0.1)))
  }

  txt <- paste(lines, collapse = "\n")

  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 1, label = txt,
                      hjust = 0, vjust = 1, family = "mono", size = 3) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 5))
}

# ---- Categorica por tempo (stacked bar 100%) --------------------------------

#' @keywords internal
.panel_categorical_by_time <- function(df, var, time_var) {
  if (is.null(df) || nrow(df) == 0 || !"period" %in% names(df)) {
    return(.empty_panel("Sem dados"))
  }
  df <- df[!is.na(df$period), , drop = FALSE]
  if (nrow(df) == 0) return(.empty_panel("Sem dados"))

  # Pega categorias na ordem decrescente de freq global
  cat_order <- df %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(tot = sum(n), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tot)) %>%
    dplyr::pull(category)

  df <- df %>%
    dplyr::mutate(category = factor(category, levels = cat_order))

  pal <- .palette_categorical
  if (length(cat_order) > length(pal)) {
    pal <- rep(pal, length.out = length(cat_order))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = period, y = prop, fill = category)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
    ggplot2::labs(title = "Distribuicao por periodo",
                  x = time_var, y = "Proporcao", fill = NULL) +
    .theme_report() +
    ggplot2::theme(legend.position = "bottom",
                   legend.text     = ggplot2::element_text(size = 7))
}

# ---- Numerica por tempo (mediana + IQR) -------------------------------------

#' @keywords internal
.panel_numeric_quantiles_by_time <- function(df, var, time_var) {
  if (is.null(df) || nrow(df) == 0 || !"period" %in% names(df)) {
    return(.empty_panel("Sem dados"))
  }
  df <- df[!is.na(df$period), , drop = FALSE]
  if (nrow(df) == 0) return(.empty_panel("Sem dados"))

  ggplot2::ggplot(df, ggplot2::aes(x = period)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75),
                         fill = "#0072B2", alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       color = "#0072B2", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(y = median),
                        color = "#0072B2", size = 1.2) +
    ggplot2::labs(title = "Mediana + IQR por periodo",
                  x = time_var, y = var) +
    .theme_report()
}

# ---- Data por tempo (frequencia por period) ---------------------------------

#' @keywords internal
.panel_date_by_time <- function(df, var, time_var) {
  if (is.null(df) || nrow(df) == 0 || !"period" %in% names(df)) {
    return(.empty_panel("Sem dados"))
  }
  df <- df[!is.na(df$period), , drop = FALSE]
  if (nrow(df) == 0) return(.empty_panel("Sem dados"))

  ggplot2::ggplot(df, ggplot2::aes(x = period, y = n)) +
    ggplot2::geom_col(fill = "#009E73") +
    ggplot2::scale_y_continuous(labels = .fmt_big()) +
    ggplot2::labs(title = "Frequencia por periodo",
                  x = time_var, y = "N") +
    .theme_report()
}

# ---- Missingness por tempo (com linha do total) -----------------------------

#' @keywords internal
.panel_missingness_by_time <- function(df, var, time_var, miss_total = NULL) {
  if (is.null(df) || nrow(df) == 0 || !"period" %in% names(df)) {
    return(.empty_panel("Sem dados"))
  }

  # Remove linhas com period NA (proteção extra)
  df <- df[!is.na(df$period), , drop = FALSE]
  if (nrow(df) == 0) return(.empty_panel("Sem dados"))

  total_pct <- if (!is.null(miss_total) && nrow(miss_total) == 1) {
    miss_total$pct_missing
  } else NA_real_

  subtitle <- if (!is.na(total_pct)) {
    sprintf("Total: %s missing",
            scales::percent(total_pct, accuracy = 0.1))
  } else NULL

  y_max <- max(c(df$pct_missing, total_pct, 0.05), na.rm = TRUE) * 1.1

  p <- ggplot2::ggplot(df, ggplot2::aes(x = period, y = pct_missing)) +
    ggplot2::geom_col(fill = "#D55E00", alpha = 0.85) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0, y_max)) +
    ggplot2::labs(title = "Missingness por periodo",
                  subtitle = subtitle,
                  x = time_var, y = "% missing") +
    .theme_report()

  if (!is.na(total_pct)) {
    x_min <- min(df$period, na.rm = TRUE)
    p <- p +
      ggplot2::geom_hline(yintercept = total_pct,
                          linetype = "dashed", color = "gray30") +
      ggplot2::annotate(
        "text",
        x = x_min,
        y = total_pct,
        label = "media", hjust = -0.1, vjust = -0.4,
        size = 2.5, color = "gray30"
      )
  }

  p
}

# ---- Variacao intra-ID ------------------------------------------------------

#' @keywords internal
.panel_intra_id <- function(intra_df, var) {
  if (is.null(intra_df) || nrow(intra_df) == 0) {
    return(.empty_panel("Sem dados intra-ID"))
  }

  total <- sum(intra_df$n)
  intra_df <- intra_df %>%
    dplyr::mutate(prop = n / total,
                  flag = ifelse(n_distinct == 1L, "Fixa", "Varia"))

  # Sumarios para o subtitulo
  pct_fixa  <- sum(intra_df$n[intra_df$n_distinct == 1L]) / total
  pct_varia <- 1 - pct_fixa

  subtitle <- sprintf("Fixa: %s   |   Varia: %s",
                       scales::percent(pct_fixa,  accuracy = 0.1),
                       scales::percent(pct_varia, accuracy = 0.1))

  pal <- c(Fixa = "#009E73", Varia = "#D55E00")

  ggplot2::ggplot(intra_df, ggplot2::aes(x = factor(n_distinct), y = n,
                                          fill = flag)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
    ggplot2::scale_y_continuous(labels = .fmt_big()) +
    ggplot2::labs(title = "Variacao intra-individuo",
                  subtitle = subtitle,
                  x = "N valores distintos por individuo",
                  y = "N individuos",
                  fill = NULL,
                  caption = "Fixa = mesmo valor em todas as ondas"
    ) +
    .theme_report() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 7, color = "gray40",
                                            hjust = 0)
    )
}

# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
.empty_panel <- function(msg = "Sem dados") {
  if (!.has_ggplot2()) return(NULL)
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                      size = 3, color = "gray50") +
    ggplot2::theme_void()
}

#' @keywords internal
.fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  if (abs(x) >= 1e6 || (abs(x) < 1e-3 && x != 0)) {
    formatC(x, format = "e", digits = 2)
  } else if (abs(x) >= 100 || x == round(x)) {
    formatC(x, format = "f", digits = 1, big.mark = ",")
  } else {
    formatC(x, format = "f", digits = 3)
  }
}
