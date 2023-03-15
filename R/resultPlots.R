#' Plot specific OMOP results for a given results object and domain
#' @param conceptDict A dictionary of keywords created via makeDict with
#' attached OMOP concept name and ID information
#' @param domain The specific OMOP domain to be plotted
#' @param N Function will plot the N highest scoring (by count) terms
#' @param res a cdm_reference object created using the CDMConnector package
#' @export
plotResultsBar <- function(res, domain, N, conceptDict){
  eb <- ggplot2::element_blank()

  if(domain %in% c("Condition","Drug","Observation","Procedure","Measurement")) {

    plotDict <- resDict  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concepts = paste(concept_name,"\n (",
                              concept_id_1,")",
                              collapse="",sep="")) %>%
      dplyr::filter(.data$domain_id == domain) %>%
      dplyr::arrange(desc(.data$count))

  } else if(domain %in% c("Study","Country","Statistic")){

    plotDict <- resDict  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concepts = .data$MeSH_term) %>%
      dplyr::filter(.data$domain_id == domain) %>%
      dplyr::arrange(desc(.data$count))

  }

  plotDict$concepts <- factor(plotDict$concepts, levels = rev(plotDict$concepts))

  N <- min(N,length(plotDict$concepts))

  p1 <- ggplot2::ggplot(plotDict[1:N,],
                        ggplot2::aes(x=count,y=concepts,fill=concepts)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb,
                   legend.position = "None") +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = 'black')) +
    ggplot2::xlab("Count") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())

  p1

  return(p1)
}


#' Plot specific OMOP results for a given results object and domain
#' @param conceptDict A dictionary of keywords created via makeDict with
#' attached OMOP concept name and ID information
#' @param domain The specific OMOP domain to be plotted
#' @param res a cdm_reference object created using the CDMConnector package
#' @export
plotCumulative <- function(res, domain, conceptDict){
  eb <- ggplot2::element_blank()

  if(domain %in% c("Condition","Drug","Observation","Procedure","Measurement")) {

    plotDict <- resDict  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concepts = paste(concept_name,"\n (",
                                     concept_id_1,")",
                                     collapse="",sep="")) %>%
      dplyr::filter(.data$domain_id == domain) %>%
      dplyr::arrange(desc(.data$count))

  } else if(domain %in% c("Study","Country","Statistic")){

    plotDict <- resDict  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concepts = .data$MeSH_term) %>%
      dplyr::filter(.data$domain_id == domain) %>%
      dplyr::arrange(desc(.data$count))

  }

  plotDict$concepts <- factor(plotDict$concepts, levels = rev(plotDict$concepts))

  N <- min(N,length(plotDict$concepts))

  p1 <- ggplot2::ggplot(plotDict[1:N,],
                        ggplot2::aes(x=count,y=concepts,fill=concepts)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb,
                   legend.position = "None") +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = 'black')) +
    ggplot2::xlab("Count") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())

  p1

  return(p1)
}
