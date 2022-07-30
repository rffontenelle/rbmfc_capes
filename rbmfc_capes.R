### RBMFC in CAPES
#
# This code is licensed under the GNU General Public License v3.0.
#
# Context:
#
# Revista Brasileira de Medicina de Família e Comunidade (RBMFC for short),
# ISSN 2179-7994, is the national journal on family and community medicine.
# 
# Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) is the
# governmental organization responsible for coordinating "strict sense" 
# postgraduate programs, that is, those which offer masters' and PhD courses.
#
# CAPES groups postgraduate programs in "evaluation areas" for the purpose of 
# evaluating, regulating and funding the postgraduate programs. A major 
# part of the evaluation consists in assessing the postgraduate programs' 
# scholarly output. In turn, ranking the journals withing the evaluation areas 
# is a major part of assessing the scholarly output.
#

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

# Data about the postgraduate programs, and the courses they hold, are 
# provided separately for each year. Data about details of the 
# programs' output is separated by output type; we are examining only
# complete articles published in scholarly journals. As I write this,
# there's only data about 2017-2018, not 2019-2020 yet.
#
data_sources <- data.table(
  name = c("2017", "2018", "2019", "2020", "output"),
  # This can change as data are updated
  url = c(
    "https://dadosabertos.capes.gov.br/dataset/bdaf1399-29ae-4920-b74f-513f11dbed68/resource/bb5c1258-cba1-4a7d-a8a4-52bd07256059/download/br-capes-colsucup-curso-2017-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/bdaf1399-29ae-4920-b74f-513f11dbed68/resource/9f811690-bce0-4ce3-acda-1870ce4fc87c/download/br-capes-colsucup-curso-2018-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/bdaf1399-29ae-4920-b74f-513f11dbed68/resource/5694418c-20bc-4b55-8154-22b60d8a13c2/download/br-capes-colsucup-curso-2019-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/bdaf1399-29ae-4920-b74f-513f11dbed68/resource/21736daf-9469-43d9-b552-3d58ac37136d/download/br-capes-colsucup-curso-2020-2021-11-10.csv",
    "https://dadosabertos.capes.gov.br/dataset/8498a5f7-de52-4fb9-8c62-b827cb27bcf9/resource/1f3b4c87-bd31-41e2-900d-a3d1f2146461/download/br-colsucup-prod-detalhe-bibliografica-2017a2018-2019-07-05-artpe.csv"
  ),
  filename = rep(NA_character_, 5),
  md5sum = c(
    "dab118c663c6321ec34e5ad3cc315ab1",
    "378ee6b2ea5a20ca7bc59a634fe1585e",
    "bcb6ac9e91ffa95e371b406c433f5dfe",
    "214fc6c5e5585fb709f898f3cd43b1c8", 
    "eea64bc0a71d31ce3d4c44979de37505"
  ),
  # You're welcome, future me
  webpage = c(
    rep("https://dadosabertos.capes.gov.br/dataset/2017-a-2020-cursos-da-pos-graduacao-stricto-sensu-no-brasil", 4),
    "https://dadosabertos.capes.gov.br/dataset/2017-a-2020-detalhes-da-producao-intelectual-bibliografica-de-programas-de-pos-graduacao"
    ),
  dictionary = c(
    rep("https://metadados.capes.gov.br/index.php/catalog/231", 4),
    "https://metadados.capes.gov.br/index.php/catalog/175/datafile/F6"
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
})

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
# most of the time that's not what's happening. Let's fix this for
# the focal journals
data$output[grepl("^\\((2179-7994|1809-5909)\\)", DS_ISSN), 
            ID_VALOR_LISTA := first(ID_VALOR_LISTA)]
data$output[grepl("^\\((1809-8363|1516-7704)\\)", DS_ISSN), 
            ID_VALOR_LISTA := first(ID_VALOR_LISTA)]


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
       # printed and their electronic form
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
][
  , unique(.SD), keyby = .(AN_BASE, CD_PROGRAMA_IES)
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
table_programs[, prop_within_program := N / sum(N), by = CD_PROGRAMA_IES]
table_programs[, c(journal_cols) :=  journals[
  .(table_programs$ID_VALOR_LISTA), 
  .SD, 
  .SDcols = c(journal_cols)
]]

rm(journal_cols)


# Write aggregated data ----

if (!dir.exists("data")) dir.create("data")
if (compareVersion("1.4.3", as.character(packageVersion("data.table"))) > -1) {
  warning("Consider fwrite(..., encoding = \"UTF-8\")")
}

# 1. Revista Brasileira de Medicina de Família e Comunidade
# 2. Revista de APS
focal_journals <- journals[
  ISSN_1 %in% c("2179-7994", "1809-5909", "1809-8363", "1516-7704"), 
  unique(ID_VALOR_LISTA)
]

areas_cols <- c(
  "CD_AREA_AVALIACAO",
  "NM_AREA_AVALIACAO",
  "N",
  "prop_within_journal",
  "prop_within_area"
)
programs_cols <- c(
  "SG_ENTIDADE_ENSINO",
  "CD_PROGRAMA_IES", 
  "NM_PROGRAMA_IES", 
  "N", 
  "prop_within_journal", 
  "prop_within_program"
)
for (ivl in focal_journals) {
  issn <- journals[.(ivl), ISSN_1]
  stopifnot(length(issn) == 1)
  table_areas[.(ID_VALOR_LISTA = ivl)] |> 
    subset(select = areas_cols) |> 
    fwrite(file.path("data", sprintf("%s_evaluation_areas.csv", issn)))
  table_programs[.(ID_VALOR_LISTA = ivl)] |> 
    subset(select = programs_cols) |>
    fwrite(file.path("data", sprintf("%s_postgraduate_programs.csv", issn)))
}
