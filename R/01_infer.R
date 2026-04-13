# =============================================================================
# autocodebook — Funções de introspecção / inferência
# =============================================================================
# Usa apenas funções base R (grepl, regmatches, gregexpr, regexec).
# Nenhuma dependência de stringr.
# =============================================================================

# -----------------------------------------------------------------------------
# .infer_type()
# -----------------------------------------------------------------------------

.infer_type <- function(code_text) {
  ct <- tolower(code_text)
  if (grepl("as\\.date|_date\\b|coalesce.*date", ct)) return("date")
  if (grepl('na_character_|~\\s*"[^"]*"|~\\s*\'[^\']*\'', ct)) return("character")
  if (grepl("na_real_|/\\s*[a-z]", ct)) return("numeric")
  if (grepl("na_integer_|\\b[0-9]+l\\b|~ 0l|~ 1l", ct)) return("integer")
  if (grepl("\\bn\\(\\)|row_number", ct)) return("integer")
  if (grepl("\\bmin\\(|\\bmax\\(", ct))   return("numeric")
  if (grepl("\\bsum\\(|\\bmean\\(", ct))  return("numeric")
  if (grepl("lpad|substring|paste|str_", ct)) return("character")
  if (grepl("case_when", ct)) {
    if (grepl('~\\s*"', ct)) return("character")
    if (grepl("~\\s*[0-9]+l", ct)) return("integer")
  }
  if (grepl("if_else", ct)) {
    if (grepl("1l|0l", ct)) return("integer")
    if (grepl('"', ct)) return("character")
  }
  "character"
}

# -----------------------------------------------------------------------------
# Helpers base R (substituem str_extract_all e str_match_all)
# -----------------------------------------------------------------------------

.base_extract_all <- function(string, pattern) {
  regmatches(string, gregexpr(pattern, string, perl = TRUE))[[1]]
}

.base_match_all <- function(string, pattern) {
  matches <- gregexpr(pattern, string, perl = TRUE)
  matched_strings <- regmatches(string, matches)[[1]]
  if (length(matched_strings) == 0) {
    return(matrix(character(0), ncol = 2))
  }
  do.call(rbind, lapply(matched_strings, function(m) {
    r <- regexec(pattern, m, perl = TRUE)
    regmatches(m, r)[[1]]
  }))
}

# -----------------------------------------------------------------------------
# .infer_source()
# -----------------------------------------------------------------------------

.reserved_words <- c(
  "case_when", "if_else", "ifelse", "coalesce", "between",
  "is.na", "is.null", "is.nan",
  "lag", "lead", "row_number", "rank", "dense_rank", "ntile",
  "n", "n_distinct", "first", "last",
  "min", "max", "sum", "mean", "median", "sd", "var",
  "lpad", "rpad", "substring", "substr", "nchar", "trim",
  "paste", "paste0", "str_c", "str_detect", "str_replace",
  "as.character", "as.integer", "as.numeric", "as.double", "as.Date",
  "grepl", "grep", "gsub", "sub",
  "abs", "sqrt", "log", "exp", "round", "ceiling", "floor",
  "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf",
  "NA_character_", "NA_real_", "NA_integer_", "NA_complex_",
  "c", "in", "L",
  "sql", "lit", "col",
  "na.rm", "fixed"
)

.infer_source <- function(code_text, existing_cols) {
  tokens <- unique(.base_extract_all(code_text, "[A-Za-z_][A-Za-z0-9_.]*"))
  tokens <- tokens[!grepl("^[0-9]", tokens)]
  tokens <- setdiff(tokens, .reserved_words)
  found <- intersect(tokens, existing_cols)
  if (length(found) == 0) return("(expressao calculada)")
  paste(found, collapse = ", ")
}

# -----------------------------------------------------------------------------
# .infer_categories()
# -----------------------------------------------------------------------------

.infer_categories <- function(code_text) {
  m <- .base_match_all(code_text, '~\\s*"([^"]*)"')
  vals <- if (ncol(m) >= 2 && nrow(m) > 0) m[, 2] else character(0)

  if (length(vals) == 0) {
    m2 <- .base_match_all(code_text, "~\\s*'([^']*)'")
    vals <- if (ncol(m2) >= 2 && nrow(m2) > 0) m2[, 2] else character(0)
  }

  if (length(vals) == 0 && grepl("~\\s*0L|~\\s*1L", code_text)) {
    vals <- c("0 = Nao", "1 = Sim")
  }

  has_na <- grepl("NA_character_|NA_real_|NA_integer_", code_text)
  if (has_na && length(vals) > 0) vals <- c(vals, "NA")

  if (length(vals) == 0) {
    if (grepl("n\\(\\)|row_number", code_text)) return("Contagem inteira >= 1")
    if (grepl("min\\(|max\\(", code_text))      return("Valor continuo")
    if (grepl("/", code_text))                   return("Valor continuo >= 0")
    return("")
  }

  paste(unique(vals), collapse = "; ")
}
