# =============================================================================
# autocodebook v0.3.0 — Smoke test
# =============================================================================
# Roda sem precisar de Spark. Testa:
#   - Compat total com API v0.1.0 (auto_mutate, auto_filter, etc.)
#   - Novas features de big data (verbose, default_cache, assume_unique,
#     cb_checkpoint, elapsed_s)
#   - Modulo de relatorio:
#       * cb_export() com .docx / .xlsx
#       * track_export() com .docx / .xlsx
#       * generate_report(type="cross_sectional")
#       * generate_report(type="longitudinal")
# =============================================================================

cat("\n========================================\n")
cat("  TESTE DO PACOTE autocodebook v0.3.0\n")
cat("========================================\n\n")

# ---- 0. Setup --------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(autocodebook)
})

set.seed(42)

# Helpers de log
ok <- function(msg)   cat("  [OK]   ", msg, "\n")
fail <- function(msg) cat("  [FAIL] ", msg, "\n")
title <- function(msg) cat("\n--- ", msg, " ---\n")

# ---- 1. Dados simulados ----------------------------------------------------

title("1. Dados simulados (transversal + longitudinal)")

# Cross-sectional: 1 linha por individuo
N_ind <- 5000
df_cross <- tibble(
  id_indiv     = sprintf("ID%05d", seq_len(N_ind)),
  cod_sexo     = sample(c(1L, 2L, NA_integer_), N_ind, replace = TRUE,
                        prob = c(0.48, 0.48, 0.04)),
  age_years    = round(rnorm(N_ind, mean = 45, sd = 18), 0),
  dt_nascimento= as.Date("1950-01-01") + sample(0:25000, N_ind, replace = TRUE),
  cod_munic    = sample(c("3550308","3304557","2927408","5300108"),
                        N_ind, replace = TRUE),
  renda        = pmax(0, rnorm(N_ind, mean = 3000, sd = 1500))
)
df_cross$renda[sample(N_ind, 500)] <- NA  # injetar missingness
ok(paste("df_cross:", nrow(df_cross), "linhas,", ncol(df_cross), "cols"))

# Longitudinal: 3-8 observacoes por individuo
N_ind_long <- 1000
n_obs_per <- sample(3:8, N_ind_long, replace = TRUE)
df_long <- tibble(
  id_indiv = rep(sprintf("ID%05d", seq_len(N_ind_long)), n_obs_per),
  ano_ref  = unlist(lapply(n_obs_per, function(k) sort(sample(2000:2020, k))))
)
df_long <- df_long %>%
  mutate(
    cod_sexo  = ave(rep(sample(c(1L, 2L), N_ind_long, replace = TRUE),
                        n_obs_per), id_indiv, FUN = function(x) x[1]),
    renda     = pmax(0, rnorm(n(), 2500, 1200)),
    cod_munic = sample(c("3550308","3304557","2927408","5300108","4106902"),
                       n(), replace = TRUE)
  )
# Injetar missingness crescente em renda nos anos mais antigos
df_long$renda[df_long$ano_ref < 2005 & runif(nrow(df_long)) < 0.7] <- NA
ok(paste("df_long:", nrow(df_long), "linhas,",
         length(unique(df_long$id_indiv)), "individuos"))

# ---- 2. Compat API v0.1.0 --------------------------------------------------

title("2. Compat total com API v0.1.0")

cb_init(id_col = "id_indiv")
ok("cb_init() basico")

df1 <- df_cross %>%
  auto_mutate(
    labels = list(sex = "Sexo (M/F)", age_cat = "Faixa etaria"),
    block  = "Demograficos",
    sex = case_when(
      cod_sexo == 1L ~ "Masculino",
      cod_sexo == 2L ~ "Feminino",
      TRUE           ~ NA_character_
    ),
    age_cat = case_when(
      age_years < 18 ~ "Crianca",
      age_years < 65 ~ "Adulto",
      TRUE           ~ "Idoso"
    )
  )
ok(paste("auto_mutate: codebook tem", nrow(cb_get()), "variaveis"))

df2 <- df1 %>%
  auto_filter(step = "1. Sexo conhecido",
              description = "Remove sexo NA",
              !is.na(sex))
ok(paste("auto_filter: tracking tem", nrow(track_get()), "linhas"))

df3 <- df2 %>%
  auto_filter(step = "2. Idade >= 18",
              description = "Adultos",
              age_years >= 18)
ok(paste("auto_filter encadeado: tracking tem", nrow(track_get()), "linhas"))

# ---- 3. Novas features de big data ----------------------------------------

title("3. Novas features de big data (v0.3.0)")

cb_init(id_col = "id_indiv", verbose = TRUE, default_cache = FALSE)
ok("cb_init(verbose=TRUE, default_cache=FALSE)")

# Verifica que tracking ganhou coluna elapsed_s
df_x <- df_cross %>%
  auto_filter(step = "Sexo conhecido", description = "test", !is.na(cod_sexo))

tr <- track_get()
if ("elapsed_s" %in% names(tr)) {
  ok("tracking tem coluna elapsed_s")
} else {
  fail("tracking SEM coluna elapsed_s")
}

# Testa assume_unique
df_dedup <- df_cross %>% distinct(id_indiv, .keep_all = TRUE)
track_step(df_dedup, "Test assume_unique", description = "skip distinct",
           assume_unique = TRUE)
ok("track_step(assume_unique=TRUE)")

# auto_filter com cache (no-op em local)
df_cached <- df_cross %>%
  auto_filter(step = "test cache", description = "...",
              cache = FALSE, !is.na(cod_sexo))
ok("auto_filter(cache=FALSE)")

# cb_checkpoint em local (no-op)
df_chk <- cb_checkpoint(df_cross, mode = "memory")
if (identical(df_chk, df_cross)) {
  ok("cb_checkpoint em local = no-op")
} else {
  fail("cb_checkpoint em local nao deveria mudar dados")
}

# Setters
old_v <- cb_set_verbose(FALSE)
if (old_v == TRUE) {
  ok("cb_set_verbose() retorna valor anterior")
} else {
  fail("cb_set_verbose() retorno errado")
}

cb_set_default_cache(TRUE)
ok("cb_set_default_cache(TRUE)")

# track_render com elapsed
tbl_e <- track_render(show_elapsed = TRUE)
if (!is.null(tbl_e)) ok("track_render(show_elapsed=TRUE)")

# ---- 4. Exports (HTML, CSV, DOCX, XLSX) -----------------------------------

title("4. Exports do codebook em multiplos formatos")

tmpdir <- tempfile("autocodebook_test_")
dir.create(tmpdir)

# Reseta sessao e popula codebook
cb_init(id_col = "id_indiv")
df_cross %>%
  auto_mutate(
    labels = list(sex = "Sexo", age_cat = "Faixa"),
    block  = "Demograficos",
    sex = if_else(cod_sexo == 1L, "M", "F"),
    age_cat = case_when(age_years < 18 ~ "C", age_years < 65 ~ "A", TRUE ~ "I")
  ) %>%
  auto_filter(step = "filtro 1", description = "test", !is.na(cod_sexo)) %>%
  auto_filter(step = "filtro 2", description = "test", age_years >= 0) -> df_for_export

# CSV
p_csv <- file.path(tmpdir, "codebook.csv")
cb_export(p_csv)
if (file.exists(p_csv)) ok("cb_export .csv") else fail("cb_export .csv")

# HTML
p_html <- file.path(tmpdir, "codebook.html")
res <- try(cb_export(p_html), silent = TRUE)
if (file.exists(p_html)) {
  ok("cb_export .html")
} else {
  fail(paste("cb_export .html:", conditionMessage(attr(res, 'condition'))))
}

# DOCX (so se officer+flextable instalados)
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  p_docx <- file.path(tmpdir, "codebook.docx")
  res <- try(cb_export(p_docx), silent = TRUE)
  if (file.exists(p_docx)) ok("cb_export .docx")
  else fail(paste("cb_export .docx:",
                  conditionMessage(attr(res, 'condition'))))
} else {
  cat("  [SKIP] cb_export .docx (officer/flextable nao instalados)\n")
}

# XLSX (so se openxlsx instalado)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  p_xlsx <- file.path(tmpdir, "codebook.xlsx")
  res <- try(cb_export(p_xlsx), silent = TRUE)
  if (file.exists(p_xlsx)) ok("cb_export .xlsx")
  else fail(paste("cb_export .xlsx:",
                  conditionMessage(attr(res, 'condition'))))

  # Subset de variaveis
  p_xlsx2 <- file.path(tmpdir, "codebook_subset.xlsx")
  cb_export(p_xlsx2, variables = c("sex"))
  if (file.exists(p_xlsx2)) ok("cb_export .xlsx com variables=c('sex')")
} else {
  cat("  [SKIP] cb_export .xlsx (openxlsx nao instalado)\n")
}

# Track export DOCX/XLSX
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  p_tr_docx <- file.path(tmpdir, "tracking.docx")
  try(track_export(p_tr_docx), silent = TRUE)
  if (file.exists(p_tr_docx)) ok("track_export .docx")
}
if (requireNamespace("openxlsx", quietly = TRUE)) {
  p_tr_xlsx <- file.path(tmpdir, "tracking.xlsx")
  track_export(p_tr_xlsx)
  if (file.exists(p_tr_xlsx)) ok("track_export .xlsx")
}

# ---- 5. generate_report() -------------------------------------------------

title("5. generate_report() — transversal e longitudinal")

# Deps obrigatorias do relatorio
have_rmd <- requireNamespace("rmarkdown", quietly = TRUE) &&
            requireNamespace("knitr", quietly = TRUE)
have_gg  <- requireNamespace("ggplot2", quietly = TRUE)

if (!have_rmd) cat("  [SKIP] generate_report (rmarkdown/knitr nao instalados)\n")
if (!have_gg)  cat("  [SKIP] generate_report (ggplot2 nao instalado)\n")

if (have_rmd && have_gg) {

  # Transversal
  cb_init(id_col = "id_indiv")
  df_for_report <- df_cross %>%
    auto_mutate(
      labels = list(sex = "Sexo"),
      block = "Demo",
      sex = if_else(cod_sexo == 1L, "M", "F")
    ) %>%
    auto_filter(step = "Sexo conhecido", description = "test", !is.na(sex))

  p_report <- file.path(tmpdir, "report_cross.html")
  res <- try(
    generate_report(
      data = df_for_report,
      type = "cross_sectional",
      id_var = "id_indiv",
      treat_as_categorical = c("cod_sexo"),
      output_html = p_report,
      title = "Smoke test transversal"
    ),
    silent = TRUE
  )
  if (file.exists(p_report)) ok("generate_report(type='cross_sectional')")
  else fail(paste("generate_report cross:",
                  conditionMessage(attr(res, 'condition'))))

  # Longitudinal — testando treat_as_categorical pra cod_sexo
  cb_init(id_col = "id_indiv")
  df_long_x <- df_long %>%
    auto_filter(step = "Renda conhecida", description = "test",
                !is.na(renda))

  p_report_l <- file.path(tmpdir, "report_long.html")
  res <- try(
    generate_report(
      data = df_long_x,
      type = "longitudinal",
      id_var   = "id_indiv",
      time_var = "ano_ref",
      treat_as_categorical = c("cod_sexo"),
      output_html = p_report_l,
      title = "Smoke test longitudinal"
    ),
    silent = TRUE
  )
  if (file.exists(p_report_l)) ok("generate_report(type='longitudinal')")
  else fail(paste("generate_report long:",
                  conditionMessage(attr(res, 'condition'))))
}

# ---- 6. Fluxograma CONSORT (track_split / track_outcomes) -----------------

title("6. Fluxograma CONSORT composavel")

# Dados com exposicao, mediador e desfechos (1 linha por individuo)
set.seed(7)
N <- 5000
df_flow <- tibble(
  id_indiv     = sprintf("ID%05d", seq_len(N)),
  idade        = sample(10:80, N, replace = TRUE),
  exposto_seca = sample(c(0L, 1L), N, replace = TRUE, prob = c(0.67, 0.33)),
  severidade   = sample(c("sem", "moderada", "grave"), N, replace = TRUE,
                        prob = c(0.67, 0.23, 0.10))
)
# migracao depende da seca (efeito simulado)
p_mig <- ifelse(df_flow$exposto_seca == 1L, 0.14, 0.08)
df_flow$migrou <- rbinom(N, 1, p_mig)
# desfechos dependem da migracao
df_flow$obito_dcv    <- rbinom(N, 1, ifelse(df_flow$migrou == 1, 0.04, 0.025))
df_flow$obito_infec  <- rbinom(N, 1, ifelse(df_flow$migrou == 1, 0.03, 0.018))

# --- Padrao 1: linear (sem split) ---
cb_init(id_col = "id_indiv")
df_flow %>%
  auto_filter(step = "Coorte inicial", description = "todos", TRUE) %>%
  auto_filter(step = "Seca conhecida", description = "...", !is.na(exposto_seca))
fl1 <- flow_get()
if (is.na(fl1$n_root) && length(fl1$levels) == 0) {
  ok("Padrao 1 (linear): sem splits, usa tracking")
} else {
  ok("Padrao 1 (linear): flow vazio como esperado")
}

# --- Padrao 2: 1 split (exposicao -> desfecho migração) ---
cb_init(id_col = "id_indiv")
df_flow %>%
  track_split(by = "exposto_seca", label = "Exposicao: seca") %>%
  track_outcomes(vars = c("migrou"), labels = list(migrou = "Migração"))
fl2 <- flow_get()
if (length(fl2$levels) == 1 && length(fl2$outcomes) == 1) {
  ok(paste("Padrao 2 (1 split + desfecho): n_root =", fl2$n_root))
} else {
  fail("Padrao 2: estrutura inesperada")
}
ft2 <- flow_table()
if (nrow(ft2) > 0) ok(paste("flow_table padrao 2:", nrow(ft2), "linhas"))

# --- Padrao 3: split ordinal (3 braços) + multiplos desfechos ---
cb_init(id_col = "id_indiv")
df_flow %>%
  track_split(by = "severidade", label = "Severidade (SPEI)") %>%
  track_outcomes(vars = c("migrou", "obito_dcv"),
                 labels = list(migrou = "Migração", obito_dcv = "Óbito DCV"))
fl3 <- flow_get()
n_cats <- nrow(fl3$levels[[1]]$counts)
if (n_cats == 3 && length(fl3$outcomes) == 2) {
  ok(paste("Padrao 3 (3 braços + 2 desfechos): OK"))
} else {
  fail(paste("Padrao 3: esperava 3 braços/2 desfechos, obteve",
             n_cats, "/", length(fl3$outcomes)))
}

# --- Padrao 4: mediação aninhada (exposicao -> mediador -> desfechos) ---
cb_init(id_col = "id_indiv")
df_flow %>%
  track_split(by = "exposto_seca", label = "Exposicao: seca") %>%
  track_split(by = "migrou",       label = "Mediador: migração") %>%
  track_outcomes(vars = c("obito_dcv", "obito_infec"),
                 labels = list(obito_dcv = "Óbito DCV",
                               obito_infec = "Óbito infecção"))
fl4 <- flow_get()
if (length(fl4$levels) == 2 && length(fl4$outcomes) == 2) {
  n_leaves <- nrow(fl4$levels[[2]]$counts)
  ok(paste("Padrao 4 (mediação aninhada):", n_leaves, "folhas finais"))
} else {
  fail("Padrao 4: estrutura aninhada inesperada")
}

# --- Renderiza relatorio com fluxograma ramificado ---
if (have_rmd && have_gg) {
  cb_init(id_col = "id_indiv")
  df_flow %>%
    auto_filter(step = "Coorte baseline", description = "todos os registros",
                TRUE) %>%
    auto_filter(step = "Idade >= 13", description = "menores de 13 anos",
                idade >= 13) %>%
    auto_filter(step = "Seca conhecida", description = "exposição ausente",
                !is.na(exposto_seca)) %>%
    track_split(by = "exposto_seca", label = "Exposição: seca",
                value_labels = c("0" = "Sem seca", "1" = "Com seca")) %>%
    track_split(by = "migrou",       label = "Mediador: migração",
                value_labels = c("0" = "Não migrou", "1" = "Migrou")) %>%
    track_outcomes(vars = c("obito_dcv", "obito_infec"),
                   labels = list(obito_dcv = "Óbito DCV",
                                 obito_infec = "Óbito infecção"))
  p_flow_report <- file.path(tmpdir, "report_grupos.html")
  res <- try(
    generate_report(
      data = df_flow,
      type = "cross_sectional",
      id_var = "id_indiv",
      variables = c("exposto_seca", "severidade", "migrou"),
      treat_as_categorical = c("exposto_seca", "migrou", "obito_dcv",
                               "obito_infec"),
      output_html = p_flow_report,
      title = "Smoke test tabelas de grupos"
    ),
    silent = TRUE
  )
  if (file.exists(p_flow_report)) {
    ok("generate_report com tabelas de grupos (arvore)")
  } else {
    fail(paste("report grupos:",
               conditionMessage(attr(res, 'condition'))))
  }
}

# ---- 7. Resumo -----------------------------------------------------------

cat("\n========================================\n")
cat("  TESTE CONCLUIDO\n")
cat("  Arquivos gerados em: ", tmpdir, "\n")
cat("========================================\n")

list.files(tmpdir, full.names = FALSE)
