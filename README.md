# autocodebook

Automatic codebook and eligibility tracking for Spark/dplyr pipelines.

**Write the `mutate()` — the codebook writes itself.**

## Installation

```r
# From GitHub
devtools::install_github("patriciafortesm/autocodebook")

# Or locally, after cloning
install.packages("path/to/autocodebook", repos = NULL, type = "source")
```

## The problem

In data preprocessing pipelines, documenting each variable in a codebook is duplicated effort: you already wrote the `case_when()`, but then you need to manually copy the type, categories, source columns, and code into a separate table.

## The solution

`autocodebook` replaces `mutate()` with `auto_mutate()` and `summarise()` with `auto_summarise()`. The package uses **introspection** (`rlang`) to capture the source code of each expression and automatically infer:

| Field          | How it's inferred                                      |
|---------------|-------------------------------------------------------|
| **type**       | Keywords in the code (`NA_character_`, `0L`, `/`)     |
| **source**     | Column names referenced in the expression             |
| **categories** | Literal values extracted from `case_when` / `if_else` |
| **code**       | The literal R expression, captured automatically      |

**You only provide the `label`** (a human-readable description).

## Quick example

```r
library(autocodebook)

# 1. Initialize
cb_init(id_col = "person_id")

# 2. Create variables — codebook fills itself!
df <- auto_mutate(df,
  labels = list(
    sex    = "Sex",
    race   = "Self-declared race/ethnicity",
    crowding = "Household crowding (people/rooms)"
  ),
  block = "Demographics",

  sex = case_when(
    cod_sex %in% c(0L, 99L) ~ NA_character_,
    cod_sex == 1L            ~ "Male",
    cod_sex == 2L            ~ "Female",
    TRUE                     ~ NA_character_
  ),

  race = case_when(
    cod_race == 0L ~ NA_character_,
    cod_race == 1L ~ "White",
    cod_race == 2L ~ "Black",
    cod_race == 3L ~ "Yellow",
    cod_race == 4L ~ "Brown",
    cod_race == 5L ~ "Indigenous",
    TRUE           ~ NA_character_
  ),

  crowding = case_when(
    n_people > 0L & n_rooms > 0L ~ n_people / n_rooms,
    TRUE                         ~ NA_real_
  )
)

# 3. View and export
cb_render()                      # gt table in Viewer
cb_export("codebook.html")       # HTML file
cb_export("codebook.csv")        # CSV for Excel
```

## Full example with Spark

```r
library(sparklyr)
library(dplyr)
library(autocodebook)

sc <- spark_connect(master = "local")
df <- copy_to(sc, my_data, "my_table")

# Initialize
cb_init(id_col = "person_id")

# Track baseline
track_step(df, "1. Raw data", "All records before any filter")

# Create variables (codebook auto-generated)
df <- auto_mutate(df,
  labels = list(sex = "Sex", age_cat = "Age category"),
  block  = "Demographics",
  sex = case_when(
    cod_sex == 1L ~ "Male",
    cod_sex == 2L ~ "Female",
    TRUE          ~ NA_character_
  ),
  age_cat = case_when(
    age < 18  ~ "Child",
    age < 65  ~ "Adult",
    TRUE      ~ "Elderly"
  )
)

# Filter with automatic tracking
df <- auto_filter(df,
  step = "2. Remove missing sex",
  description = "Exclude records without sex information",
  !is.na(sex)
)

# Summarise with automatic codebook
summary_df <- df %>%
  group_by(person_id) %>%
  auto_summarise(
    labels = list(n_records = "Total records per individual"),
    block  = "Individual summary",
    n_records = n(),
    .groups = "drop"
  )

# View and export
cb_render()
track_render()
cb_export("codebook.html")
track_export("tracking_table.html")

# Programmatic access
cb_get()      # tibble with full codebook
track_get()   # tibble with full tracking log

spark_disconnect(sc)
```

## What gets auto-detected vs. what you write

| Field          | Who fills it   |
|---------------|---------------|
| **label**      | You            |
| **block**      | You (optional) |
| **type**       | Automatic      |
| **source**     | Automatic      |
| **categories** | Automatic      |
| **code**       | Automatic      |

## API reference

### Verb wrappers (replace mutate/summarise/filter)

| Function           | Replaces      | Registers in |
|-------------------|--------------|-------------|
| `auto_mutate()`    | `mutate()`    | Codebook    |
| `auto_summarise()` | `summarise()` | Codebook    |
| `auto_filter()`    | `filter()`    | Tracking    |

### Codebook functions

| Function       | Description                          |
|---------------|-------------------------------------|
| `cb_init()`    | Initialize session, set ID column    |
| `cb_register()`| Manual registration (fallback)      |
| `cb_get()`     | Returns codebook as tibble           |
| `cb_reset()`   | Clears the codebook                  |
| `cb_render()`  | Renders as gt table                  |
| `cb_export()`  | Saves to HTML or CSV                 |

### Tracking functions

| Function         | Description                          |
|-----------------|-------------------------------------|
| `track_step()`   | Log a step with unique ID count      |
| `track_get()`    | Returns tracking log as tibble       |
| `track_reset()`  | Clears the tracking log              |
| `track_render()` | Renders as gt table                  |
| `track_export()` | Saves to HTML or CSV                 |

## Parameters

```r
auto_mutate(.data,
  labels = list(var1 = "Variable 1 label"),   # only required field
  block  = "Block name",                       # optional: groups in codebook
  var1   = case_when(...)                       # your normal expressions
)
```

- **`labels`**: Named list. Key = variable name, value = human-readable label. If omitted, the variable name itself is used as the label.
- **`block`**: Optional string. Groups variables by section in the rendered codebook.

## Compatibility

- R ≥ 4.1
- Works with both sparklyr (`tbl_spark`) and local data frames
- All Spark SQL functions (`lpad`, `substring`, `lag` with `window_order`, etc.)

## License

MIT
