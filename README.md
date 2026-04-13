# autocodebook <img src="man/figures/logo.png" align="right" height="120" />

Codebook automático para pipelines Spark/dplyr.

**Escreva o `mutate()` → o codebook se escreve sozinho.**

## Instalação

```r
# Via GitHub (recomendado)
remotes::install_github("seu-usuario/autocodebook")

# Ou local, após clonar o repositório
install.packages("/caminho/para/autocodebook", repos = NULL, type = "source")
```

## O problema

Em pipelines de pré-processamento, documentar cada variável no codebook é
trabalho duplicado: você já escreveu o `case_when()`, mas precisa copiar
manualmente o tipo, as categorias, as colunas-fonte e o código para uma
tabela separada.

## A solução

O `autocodebook` substitui `mutate()` por `auto_mutate()` e `summarise()`
por `auto_summarise()`. O pacote usa **introspecção** (`rlang`) para capturar
o código-fonte de cada expressão e inferir automaticamente:

| Campo        | Como é inferido                                        |
|-------------|-------------------------------------------------------|
| **type**       | Palavras-chave no código (`NA_character_`, `0L`, `/`) |
| **source**     | Colunas do df referenciadas na expressão              |
| **categories** | Valores literais extraídos de `case_when` / `if_else` |
| **code**       | Texto literal da expressão R capturada                |

**Você só fornece o `label`** (rótulo descritivo humano).

## Exemplo completo (Spark)

```r
library(sparklyr)
library(dplyr)
library(autocodebook)

sc <- spark_connect(master = "local")
df <- copy_to(sc, my_data, "my_table")

# 1. Inicializa o autocodebook
cb_init(id_col = "id_cidacs_pop100_v2")

# 2. Tracking: registra a base inicial
track_step(df, "1. Base bruta", "Todos os registros antes de filtros")

# 3. Cria variáveis — codebook preenchido automaticamente!
df <- auto_mutate(df,
  labels = list(
    sex        = "Sexo",
    race_color = "Raça/cor da pele",
    crowding   = "Adensamento domiciliar (pessoas/cômodos)"
  ),
  block = "Variáveis demográficas",

  sex = case_when(
    cod_sexo %in% c(0L, 99L) ~ NA_character_,
    cod_sexo == 1L            ~ "Male",
    cod_sexo == 2L            ~ "Female",
    TRUE                      ~ NA_character_
  ),

  race_color = case_when(
    cod_raca == 0L ~ NA_character_,
    cod_raca == 1L ~ "White",
    cod_raca == 2L ~ "Black",
    cod_raca == 3L ~ "Yellow",
    cod_raca == 4L ~ "Brown",
    cod_raca == 5L ~ "Indigenous",
    TRUE           ~ NA_character_
  ),

  crowding = case_when(
    n_pessoas > 0L & n_comodos > 0L ~ n_pessoas / n_comodos,
    TRUE                            ~ NA_real_
  )
)

# 4. Filtra com tracking automático
df <- auto_filter(df,
  step = "2. Remove missing sex",
  description = "Exclui registros sem sexo informado",
  !is.na(sex)
)

# 5. Sumariza com codebook automático
resumo <- df %>%
  group_by(id) %>%
  auto_summarise(
    labels = list(n_registros = "Total de registros por indivíduo"),
    block  = "Resumo individual",
    n_registros = n(),
    .groups = "drop"
  )

# 6. Visualiza e exporta
cb_render()                          # gt table no Viewer
track_render()                       # tracking no Viewer

cb_export("codebook.html")           # HTML
cb_export("codebook.csv")            # CSV para Excel
track_export("tracking_table.html")

# 7. Consulta programática
cb_get()     # tibble com todo o codebook
track_get()  # tibble com todo o tracking

spark_disconnect(sc)
```

## API completa

### Verbos (substituem mutate/summarise/filter)

| Função             | Substitui       | Registra em       |
|-------------------|----------------|-------------------|
| `auto_mutate()`    | `mutate()`      | Codebook          |
| `auto_summarise()` | `summarise()`   | Codebook          |
| `auto_filter()`    | `filter()`      | Tracking          |

### Codebook

| Função         | Descrição                              |
|---------------|---------------------------------------|
| `cb_init()`    | Inicializa sessão, define coluna de ID |
| `cb_register()`| Registro manual (fallback)            |
| `cb_get()`     | Retorna tibble do codebook            |
| `cb_reset()`   | Limpa o codebook                      |
| `cb_render()`  | Renderiza como gt table               |
| `cb_export()`  | Salva em HTML ou CSV                  |

### Tracking

| Função           | Descrição                              |
|-----------------|---------------------------------------|
| `track_step()`   | Registra etapa com contagem de IDs    |
| `track_get()`    | Retorna tibble do tracking            |
| `track_reset()`  | Limpa o tracking                      |
| `track_render()` | Renderiza como gt table               |
| `track_export()` | Salva em HTML ou CSV                  |

## Parâmetros de auto_mutate / auto_summarise

```r
auto_mutate(.data,
  labels = list(var1 = "Rótulo da variável 1"),  # ÚNICO campo obrigatório
  block  = "Nome do bloco",                       # opcional: agrupa no codebook
  var1   = case_when(...)                          # suas expressões normais
)
```

- **`labels`**: named list. Chave = nome da variável, valor = rótulo descritivo.
  Se omitido, usa o próprio nome da variável como rótulo.
- **`block`**: string opcional. Agrupa variáveis por seção no codebook renderizado.

## Compatibilidade

- R ≥ 4.1
- sparklyr (tbl_spark) e data frames locais
- Todas as funções Spark SQL (`lpad`, `substring`, `lag` com `window_order`, etc.)
