# sumo

<!-- badges: start -->
  [![R-CMD-check](https://github.com/OdyOSG/sumo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OdyOSG/sumo/actions/workflows/R-CMD-check.yaml)
 [![Codecov test coverage](https://codecov.io/gh/OdyOSG/sumo/coverage.svg?branch=main)](https://app.codecov.io/gh/OdyOSG/sumo?branch=main)
<!-- badges: end -->

`sumo` is a R package that maps OMOP vocabulary to MeSH enabling a search of pubmed articles based on concept Ids. 


## Installation 

You can install the development version of sumo from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OdyOSG/sumo")
```

## How To Use

### CDM connection

In order to use `sumo` the user needs a connection to an OMOP CDM. This can be done using the [`CDMConnector`](https://github.com/darwin-eu/CDMConnector) package that sets up the OMOP CDM connection as a single R object. For details on how to use [`CDMConnector`](https://github.com/darwin-eu/CDMConnector) visit the package website. Below is an example of how to connect to a cdm to use `sumo`:

``` r

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = "<hostname>",
  dbname   = "<dbname>",
  user     = "<user>",
  password = "<password>",
  port     = "<port>"
)

cdmSchema = "<cdm_schema>"

cdm <- CDMConnector::cdm_from_con(con, cdm_schema = cdmSchema)

```

### Connect to entrez API

Pubmed articles found using `sumo` are sourced from the NCBI entrez API. This API is accessed using the R package [`rentrez`](https://github.com/ropensci/rentrez), which is heavily used within `sumo`. Users can acquire an API key by [registering with NCBI](https://account.ncbi.nlm.nih.gov/) which will improve search request performance. Follow the [`rentrez`](https://github.com/ropensci/rentrez) vignette for more information.

### Creating a search strategy

Using `sumo` the user can build search strategies to query the pubmed database for articles that refer to certain OMOP concept ids. A search strategy can be built by specifying dates, journals, keywords and MeSH terms. Each of these items has a function that builds the search strategy to run on the API. Below are examples of how each works:

``` r

journals <- journalSearch(c("Science", "Nature"))

timeFrame <- dateRange("2018/01/01", "2022/12/31")

terms <- textSearch(text = c("cohort", "case-control"))

mesh <- meshSearch(mesh = "Anemia")

```

Each of these functions returns a character string with search query constructed to use with rentrez. These additional search terms are helpful to narrow a users search of an OMOP term to only find publications in a specific journal, during a certain time frame and who use key words or MeSH terms. These search terms can be combined as follows:

``` r

searchStrategy <- combineSearchStrategy(mesh, terms, timeFrame, journals)

```

### Searches using the OMOP Vocabulary

The key value add of `sumo` is that it can search based on an concept Id from the OMOP vocabulary. This concept Id is mapped to a mesh term or appropriate keyword which is then used in the search. An example is shown below:

``` r

#high impact journals is a helper function yielding a set of journals for the search
journals <- journalSearch(highImpactJournals())

timeFrame <- dateRange("2018/01/01", "2022/12/31")

#observational study is a helper function yielding a set of terms for the search
terms <- textSearch(text = observationalStudy())

conceptIds <- c("439777") #id for Anemia
keyword <- searchOmop(conceptIds = conceptIds, cdm = cdm, explosion = TRUE)
searchStrategy <- combineSearchStrategy(terms, timeFrame, journals, keyword)

```

Once a search strategy has been established, the user can run this query on pubmed to find the resulting publications. The function `searchPubmed` runs the search strategy on pubmed and returns an `rentrez` object enumerating the number of articles that fit the search criteria. The user can then fetch the meta info of these articles using the function `fetchPubmed`. This returns a `tibble` containing all the information about the articles. Finally, the user can preview the fetched articles using the function `printAbstract` which yields an html preview of the files in the Rstudio Viewer tab. 

``` r
# find article hits
hits <- searchPubmed(searchStrategy)

# return meta info on articles
res <- fetchPubmed(hits)

#print the abstracts from the results to preview
printAbstract(res)

```
