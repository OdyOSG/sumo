library(sumo)
library(rentrez)
library(devtools)
library(dplyr)
library(tidyr)
library(purrr)
library(kableExtra)
library(data.table)
library(ggplot2)
library(DatabaseConnector)
library(CDMConnector)

##### Setup #####

source("../../ConnDetails/connDetails_Synthea100.R")

dbiconn <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = "synthea100",
                          host = "localhost",
                          port = port_synthea100,
                          user = user_synthea100,
                          password = password_synthea100)

cdmSchema = "cdm_synthea10"

cdm <- cdm_from_con(dbiconn, cdm_schema = cdmSchema)

set_entrez_key(keyring::key_get("PubMed_API"))



##### Search Strategy #####

journals <- journalSearch(highImpactJournals())

timeFrame <- dateRange("2018/01/01", "2022/12/31")

terms <- textSearch(text = observationalStudy())

conceptIds <- c("439777")
keyword <- searchOmop(conceptIds = conceptIds, cdm = cdm, explosion = TRUE)

searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, keyword)

hits <- searchPubmed(searchStrategy)

res <- fetchPubmed(hits)

printAbstract(res)



##### Results Processing #####

resDict <- makeDict(res, cdm)

res <- addDictToRes(res, resDict)

printAbstract(res)



##### Plots - Work In Progress #####

eb <- element_blank()

plotDict <- resDict  %>%
  rowwise() %>%
  mutate(concepts = paste(concept_name,"\n (",
                          concept_id_1,")",
                          collapse="",sep="")) %>%
  filter(domain_id == "Condition") %>%
  arrange(desc(count))

plotDict$concepts

plotDict$concepts <- factor(plotDict$concepts, levels = rev(plotDict$concepts))

N <- 15
N <- min(N,length(plotDict$concepts))

ggplot(plotDict[plotDict$domain_id == "Condition",][1:N,],
       aes(x=count,y=concepts,colour=concepts)) +
  geom_point(size=3) +
  scale_colour_viridis_d() +
  theme(panel.grid.major = eb, panel.grid.minor = eb,
        panel.background = eb, panel.border = eb,
        axis.title.y = eb,
        legend.position = "None") +
  theme(axis.line.x = element_line(color = 'black')) + xlab("Count")

ggplot(plotDict[plotDict$domain_id == "Condition",][1:N,],
       aes(x=count,y=concepts,fill=concepts)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  theme(panel.grid.major = eb, panel.grid.minor = eb,
        panel.background = eb, panel.border = eb,
        axis.title.y = eb,
        legend.position = "None") +
  theme(axis.line.x = element_line(color = 'black')) + xlab("Count")

ggplot(plotDict[plotDict$domain_id == "Condition",][1:N,],
       aes(x=count,y=concepts,fill=concepts)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  theme(panel.grid.major = eb, panel.grid.minor = eb,
        panel.background = eb, panel.border = eb,
        axis.title.y = eb,
        legend.position = "None") +
  theme(axis.line.x = element_line(color = 'black')) + xlab("Count")
