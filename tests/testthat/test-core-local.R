# Testes que rodam SEM Spark/JVM (data frames locais).
# Cobrem o caminho principal: codebook, tracking e flow tree.

test_that("cb_init reseta estado e define id_col", {
  cb_init(id_col = "id_indiv")
  expect_equal(nrow(cb_get()), 0L)
  expect_equal(nrow(track_get()), 0L)
})

test_that("auto_mutate registra variaveis no codebook", {
  cb_init(id_col = "id_indiv")
  df <- tibble::tibble(
    id_indiv = sprintf("ID%03d", 1:100),
    cod_sexo = sample(c(1L, 2L), 100, replace = TRUE),
    idade    = sample(18:80, 100, replace = TRUE)
  )
  out <- df %>%
    auto_mutate(
      labels = list(sexo = "Sexo (M/F)"),
      block  = "Demograficos",
      sexo = dplyr::if_else(cod_sexo == 1L, "M", "F")
    )
  cb <- cb_get()
  expect_true("sexo" %in% cb$variable)
  expect_equal(cb$label[cb$variable == "sexo"], "Sexo (M/F)")
  expect_equal(cb$block[cb$variable == "sexo"], "Demograficos")
  # auto_mutate deve devolver o data frame transformado
  expect_true("sexo" %in% names(out))
})

test_that("auto_filter registra etapa de tracking e conta ids", {
  cb_init(id_col = "id_indiv")
  df <- tibble::tibble(
    id_indiv = sprintf("ID%03d", 1:100),
    idade    = c(rep(10L, 30), rep(40L, 70))
  )
  out <- df %>%
    auto_filter(step = "Adultos", description = "idade >= 18", idade >= 18)
  tr <- track_get()
  expect_equal(nrow(tr), 1L)
  expect_equal(tr$n_ids[1], 70L)
  expect_equal(tr$n_removed[1], 0L)
  expect_equal(nrow(out), 70L)
})

test_that("cb_register adiciona entrada manual e cb_reset limpa", {
  cb_init(id_col = "id")
  cb_register("minha_var", label = "Variavel manual", type = "integer",
              block = "Bloco X", categories = "0 = No; 1 = Yes")
  cb <- cb_get()
  expect_true("minha_var" %in% cb$variable)
  expect_equal(cb$type[cb$variable == "minha_var"], "integer")
  cb_reset()
  expect_equal(nrow(cb_get()), 0L)
})

test_that("track_step com assume_unique conta linhas em vez de distinct", {
  cb_init(id_col = "id_indiv")
  df <- tibble::tibble(id_indiv = sprintf("ID%03d", 1:50))
  n <- track_step(df, "dedup", description = "ja unico", assume_unique = TRUE)
  expect_equal(n, 50L)
})

test_that("cb_checkpoint em data frame local e no-op", {
  df <- tibble::tibble(a = 1:10)
  expect_identical(cb_checkpoint(df, mode = "memory"), df)
})

test_that("cb_set_verbose retorna valor anterior", {
  cb_init(id_col = "id")
  old <- cb_set_verbose(TRUE)
  expect_false(old)
  cur <- cb_set_verbose(FALSE)
  expect_true(cur)
})

test_that("track_split + track_outcomes constroem a arvore de fluxo", {
  cb_init(id_col = "id_indiv")
  set.seed(1)
  N <- 500
  df <- tibble::tibble(
    id_indiv     = sprintf("ID%03d", 1:N),
    exposto      = sample(c(0L, 1L), N, replace = TRUE),
    migrou       = sample(c(0L, 1L), N, replace = TRUE)
  )
  df %>%
    track_split(by = "exposto", label = "Exposicao") %>%
    track_outcomes(vars = c("migrou"), labels = list(migrou = "Migracao"))
  fl <- flow_get()
  expect_equal(length(fl$levels), 1L)
  expect_equal(length(fl$outcomes), 1L)
  ft <- flow_table()
  expect_true(nrow(ft) > 0L)
})

test_that("flow_reset limpa a arvore", {
  cb_init(id_col = "id_indiv")
  df <- tibble::tibble(id_indiv = "ID001", g = 1L)
  df %>% track_split(by = "g")
  flow_reset()
  fl <- flow_get()
  expect_true(is.na(fl$n_root))
  expect_equal(length(fl$levels), 0L)
})
