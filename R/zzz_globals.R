# Declara as variaveis usadas em NSE (dplyr/rlang) para silenciar o
# NOTE "no visible binding for global variable" do R CMD check.
# Sao nomes de colunas referenciados dentro de mutate/summarise/filter, nao
# variaveis globais de verdade.
utils::globalVariables(c(
  ".cat_v", ".is_pos", "bin_high", "bin_idx", "bin_idx_raw", "bin_low",
  "bin_mid", "block", "cat_label", "cat_raw", "categoria", "categories",
  "category", "code", "elapsed_s", "flag", "label_n", "label_rm", "lower",
  "middle", "n_distinct_v", "n_ids", "n_removed", "p25", "p75", "pct_missing",
  "period", "prop", "step", "step_ord", "tot", "upper", "variable", "x",
  "year_v"
))
