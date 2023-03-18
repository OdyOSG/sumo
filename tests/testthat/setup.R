# Setup for tests

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("CDMHOST"),
  dbname   = Sys.getenv("CDMDBNAME"),
  user     = Sys.getenv("CDMUSER"),
  password = Sys.getenv("CDMPASSWORD"),
  port     = Sys.getenv("CDMPORT")
)

cdmSchema <- Sys.getenv("CDMSCHEMA")

cdm <- CDMConnector::cdm_from_con(con, cdm_schema = cdmSchema)

rentrez::set_entrez_key(Sys.getenv("EUTILS_API_KEY"))

#code block to run prior
journals <- journalSearch(highImpactJournals())
timeFrame <- dateRange("2018/01/01", "2022/12/31")
terms <- textSearch(text = observationalStudy())
conceptIds <- c("439777")
keyword <- searchOmop(conceptIds = conceptIds, cdm = cdm, explosion = TRUE)
searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, keyword)
hits <- searchPubmed(searchStrategy)
res <- fetchPubmed(hits, searchStrategy)
