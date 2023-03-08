library(sumo)
library(rentrez)
library(dplyr)
library(tidyr)
library(purrr)
library(kableExtra)
library(ggplot2)

set_entrez_key("apiKey")

journals <- journalSearch(highImpactJournals())

timeFrame <- dateRange("2018/01/01", "2022/12/31")

terms <- textSearch(text = observationalStudy())

meshTerm <- meshSearch("anemia")

searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, meshTerm)

hits <- searchPubmed(searchStrategy)

res <- fetchPubmed(hits)

printAbstract(res)







