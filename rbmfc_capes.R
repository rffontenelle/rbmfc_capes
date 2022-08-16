### RBMFC in CAPES
#
# See README.md for an overview of what this analytic code does. It is released 
# under the GNU General Public License v3.0; see LICENSE for more information.
#

# Pick journals ----

# Change this if you want to adapt the analysis to other journals.
# For more information, see README.md and the comments in section
# "Consolidate instances of the same journal"

focal_journals <- list(
  # Revista Brasileira de Medicina de Família e Comunidade
  c("2179-7994", "1809-5909"),
  # Revista de APS
  c("1809-8363", "1516-7704")
)


# Load libraries ----

library(data.table)


# Ancillary functions ----

# This function is intended to use with unique(DS_ISSN).
# It returns a list with the ISSN and the journal title,
# sorted in increasing order or ISSN.
#
get_issn_title <- function(x) {
  pattern = "\\((\\d{4}-\\d{3}[\\dXx])\\)\\s+(.*)$"
  ISSN = sub(pattern, "\\1", x, perl = TRUE)
  # no need for uppercase, except for consistency with original data
  TITLE = sub(pattern, "\\2", x, perl = TRUE)
  list(
    ISSN = ISSN[order(ISSN)],
    TITLE = TITLE[order(ISSN)]
  )
}


# Read data ----

# Data about the postgraduate programs, are provided separately for 
# each year. Data about details of the programs' output is separated by
# output type; we are examining only complete articles published in 
# scholarly journals.
#
data_sources <- data.table(
  name = c("2017", "2018", "2019", "2020", "output"),
  # This can change as data are updated
  url = c(
    "https://dadosabertos.capes.gov.br/dataset/903b4215-ea91-4927-8975-d1484891374f/resource/9835dd45-d4e7-4b1f-b550-eb9b049bacac/download/br-capes-colsucup-prog-2017-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/903b4215-ea91-4927-8975-d1484891374f/resource/f21e8124-d246-4ba3-abfb-cb74fc23ccb5/download/br-capes-colsucup-prog-2018-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/903b4215-ea91-4927-8975-d1484891374f/resource/62f615b8-66d1-4f9b-9014-6ec27687e2d1/download/br-capes-colsucup-prog-2019-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/903b4215-ea91-4927-8975-d1484891374f/resource/ee284b2d-0c33-459d-856b-0a2c055d327c/download/br-capes-colsucup-prog-2020-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/8498a5f7-de52-4fb9-8c62-b827cb27bcf9/resource/6646b204-8db4-4e41-b59f-f24f87eed6e4/download/br-colsucup-prod-detalhe-bibliografica-2017a2020-2022-06-30-artpe.csv"
  ),
  filename = rep(NA_character_, 5),
  md5sum = c(
    "4e7c740e2a963636145cc0d14974a7b3",
    "6423d2626f7b318b68c3f629af0ae47a",
    "03c73f23319727755176fb789152d6c2",
    "07b5ae25b31e968e01b3a197b3d6ecae", 
    "a6595704a05a2fb7e39aa6ff3536a4aa"
  ),
  # You're welcome, future me
  webpage = c(
    rep("https://dadosabertos.capes.gov.br/dataset/2017-a-2020-programas-da-pos-graduacao-stricto-sensu-no-brasil", 4),
    "https://dadosabertos.capes.gov.br/dataset/2017-a-2020-detalhes-da-producao-intelectual-bibliografica-de-programas-de-pos-graduacao"
    ),
  dictionary = c(
    rep("https://metadados.capes.gov.br/index.php/catalog/230/datafile/F2", 4),
    "https://metadados.capes.gov.br/index.php/catalog/240/datafile/F6"
  ),
  key = "name"
)

data_sources[, filename := basename(url)]

if (!dir.exists("data_raw")) dir.create("data_raw")

lapply(data_sources$name, function(nm) {
  path <- file.path("data_raw", data_sources[nm, filename])
  if (!isTRUE(data_sources[nm, md5sum] == tools::md5sum(path))) {
    # Downloading in binary mode to avoid messing with line endings
    download.file(data_sources[nm, url], path, mode = "wb")
  }
}) |> invisible()

data <- file.path("data_raw", data_sources$filename) |> 
  lapply(fread, sep = ";", encoding = "Latin-1")
names(data) <- data_sources$name
data$programs <- rbindlist(data[as.character(2017:2020)]) |> 
  setkey(AN_BASE, CD_PROGRAMA_IES)
data[as.character(2017:2020)] <- NULL
setnames(data$output, old = "AN_BASE_PRODUCAO", new = "AN_BASE")
setkey(data$output, AN_BASE, CD_PROGRAMA_IES)

# Consolidate instances of the same journal ----

# The same journal can have more than one ISSN, eg print and online.
# Both ISSN should occur with the same value of ID_VALOR_LISTA, but 
# most of the time that's not what's happening. The following code 
# has two functions. First, it makes sure the focal journals have the 
# same value of ID_VALOR_LISTA across multiple ISSN. Second, it 
# gathers these ID_VALOR_LISTA values for latter use in section
# "Write aggregated data"
#
focal_journal_ids <- sapply(focal_journals, function(x) {
  issn_pattern <- sprintf("^\\((%s)\\)", paste0(x, collapse = "|"))
  which_rows <- grepl(issn_pattern, data$output$DS_ISSN)
  first_id <- data$output[which_rows, first(ID_VALOR_LISTA)]
  data$output[which_rows, ID_VALOR_LISTA := first_id]
  first_id
})


# Rearrange data ----

# Which postgraduate programs published which articles 
# in which journals in which years
output <- data$output[
  # these values are expected for the whole database
  ID_TIPO_PRODUCAO == 2 & ID_SUBTIPO_PRODUCAO == 25 &
    # exclude works published only as abstracts
    DS_NATUREZA == "TRABALHO COMPLETO" &
    # What journal would have this as its name?!
    DS_ISSN != "-" &
    # exclude officially invalidated production
    IN_GLOSA == 0,
  .(AN_BASE, ID_ADD_PRODUCAO_INTELECTUAL, ID_VALOR_LISTA, CD_PROGRAMA_IES)
]

# (Timeless) data about those journals
journals <- data$output[
  ID_VALOR_LISTA %in% output$ID_VALOR_LISTA, 
  with(get_issn_title(unique(DS_ISSN)), 
       # A couple tens of journals were included both in their
       # printed and their electronic form. Unfortunately, there
       # are plenty journals with more than one ID_VALOR_LISTA,
       # and it's not trivial to reunite all of them.
       .(ISSN = ISSN, TITLE = TITLE, i = seq_along(ISSN))), 
  keyby = ID_VALOR_LISTA
] |> 
  dcast(ID_VALOR_LISTA ~ i, value.var = c("ISSN", "TITLE"))
setcolorder(journals, 
            c("ID_VALOR_LISTA", "ISSN_1", "TITLE_1", "ISSN_2", "TITLE_2"))
journals[, id := .I]

# Data about those postgraduate programs at each year
programs_years <- data$programs[
  unique(output[, .(AN_BASE, CD_PROGRAMA_IES)]),
  .(AN_BASE, SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES, 
    CD_AREA_AVALIACAO, NM_AREA_AVALIACAO),
  key = .(AN_BASE, CD_PROGRAMA_IES)
]
stopifnot(anyDuplicated(programs_years[, .(AN_BASE, CD_PROGRAMA_IES)]) == 0)

# Timeless data about postgraduate programs
programs <- programs_years[
  , .(SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES)
][
  , last(.SD), keyby = CD_PROGRAMA_IES
][
  , id := .I
]

# Timeless data about evaluation areas of the postgraduate programs
areas <- programs_years[
  , .(CD_AREA_AVALIACAO, NM_AREA_AVALIACAO)
][
  , last(.SD), keyby = CD_AREA_AVALIACAO
][
  # There's no CD_AREA_AVALIACAO 43
  , id := .I
]

rm(data)


# Aggregate data ----

journal_cols <- c("ISSN_1", "TITLE_1", "ISSN_2", "TITLE_2")

# Aggregate over evaluation areas
table_areas <- programs_years[
  output, , on = .(AN_BASE, CD_PROGRAMA_IES)
][
  , .N, keyby = .(ID_VALOR_LISTA, CD_AREA_AVALIACAO, NM_AREA_AVALIACAO)
]
table_areas[, prop_within_journal := N / sum(N), by = ID_VALOR_LISTA]
setorder(table_areas, ID_VALOR_LISTA, -prop_within_journal)
table_areas[, cum_prop_within_journal := cumsum(prop_within_journal), by = ID_VALOR_LISTA]
setkey(table_areas, ID_VALOR_LISTA, CD_AREA_AVALIACAO, NM_AREA_AVALIACAO)
table_areas[, prop_within_area := N / sum(N), by = CD_AREA_AVALIACAO]
table_areas[, c(journal_cols) :=  journals[
  .(table_areas$ID_VALOR_LISTA), 
  .SD, 
  .SDcols = c(journal_cols)
]]

# Aggregate over individual postgraduate programs
table_programs <- programs_years[
  output, , on = .(AN_BASE, CD_PROGRAMA_IES)
][
  , 
  .N, 
  keyby = .(ID_VALOR_LISTA, SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES)
]
table_programs[, prop_within_journal := N / sum(N), by = ID_VALOR_LISTA]
setorder(table_programs, ID_VALOR_LISTA, -prop_within_journal)
table_programs[, cum_prop_within_journal := cumsum(prop_within_journal), by = ID_VALOR_LISTA]
setkey(table_programs, ID_VALOR_LISTA, SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES)
table_programs[, prop_within_program := N / sum(N), by = CD_PROGRAMA_IES]
table_programs[, c(journal_cols) :=  journals[
  .(table_programs$ID_VALOR_LISTA), 
  .SD, 
  .SDcols = c(journal_cols)
]]

rm(journal_cols)


# Write aggregated data ----

if (!dir.exists("data")) dir.create("data")

areas_cols <- c(
  "CD_AREA_AVALIACAO",
  "NM_AREA_AVALIACAO",
  "N",
  "prop_within_journal",
  "cum_prop_within_journal",
  "prop_within_area"
)
programs_cols <- c(
  "SG_ENTIDADE_ENSINO",
  "CD_PROGRAMA_IES", 
  "NM_PROGRAMA_IES", 
  "N", 
  "prop_within_journal", 
  "cum_prop_within_journal",
  "prop_within_program"
)

# focal_journal_ids has some values from ID_VALOR_LISTA
for (ivl in focal_journal_ids) {
  issn <- journals[.(ivl), ISSN_1]
  stopifnot(length(issn) == 1)
  table_areas[.(ID_VALOR_LISTA = ivl)] |> 
    subset(select = areas_cols) |> 
    setorder(-N) |> 
    fwrite(file.path("data", sprintf("%s_evaluation_areas.csv", issn)))
  table_programs[.(ID_VALOR_LISTA = ivl)] |> 
    subset(select = programs_cols) |>
    setorder(-N) |> 
    fwrite(file.path("data", sprintf("%s_postgraduate_programs.csv", issn)))
}
message("When you open the CSV files in a spreasheet application, ",
        "choose the Windows Western character encoding (code page 1252), ",
        "and the English locale / dots as decimal sepparators. ", 
        "Format the proportion columns as percentages." )
