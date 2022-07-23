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
  Journal = sub(pattern, "\\2", x, perl = TRUE)
  list(
    ISSN = ISSN[order(ISSN)],
    Journal = Journal[order(ISSN)]
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
       .(ISSN = ISSN, Journal = Journal, i = seq_along(ISSN))), 
  keyby = ID_VALOR_LISTA
] |> 
  dcast(ID_VALOR_LISTA ~ i, value.var = c("ISSN", "Journal"))
setcolorder(journals, 
            c("ID_VALOR_LISTA", "ISSN_1", "Journal_1", "ISSN_2", "Journal_2"))
journals[, id := .I]

# Data about those postgraduate programs at each year
programs_years <- data$programs[
  unique(output[, .(AN_BASE, CD_PROGRAMA_IES)]),
  .(AN_BASE, CD_PROGRAMA_IES, NM_PROGRAMA_IES, 
    CD_AREA_AVALIACAO, NM_AREA_AVALIACAO),
  key = .(AN_BASE, CD_PROGRAMA_IES)
][
  , unique(.SD), keyby = .(AN_BASE, CD_PROGRAMA_IES)
]
stopifnot(anyDuplicated(programs_years[, .(AN_BASE, CD_PROGRAMA_IES)]) == 0)

# Timeless data about postgraduate programs
programs <- programs_years[
  , .(CD_PROGRAMA_IES, NM_PROGRAMA_IES)
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
