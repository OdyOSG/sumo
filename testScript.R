library(sumo)
library(rentrez)
library(dplyr)
library(purrr)

set_entrez_key("apiKey")

journals <- journalSearch(highImpactJournals())

timeFrame <- dateRange("2020/01/01", "2021/12/31")

terms <- textSearch(text = observationalStudy())

searchStrategy <- combineSearchStrategy(terms, timeFrame, journals)

tst <- searchPubmed(searchStrategy)

res <- fetchPubmed(tst)








