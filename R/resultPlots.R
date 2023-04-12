makePlotDict <- function(conceptDict, domain){
  if(domain %in% c("Condition","Drug","Observation","Procedure","Measurement")) {

    plotDict <- conceptDict  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concepts = paste(.data$concept_name,"\n (",
                                     .data$concept_id_1,")",
                                     collapse="",sep="")) %>%
      dplyr::filter(.data$domain_id == domain) %>%
      dplyr::arrange(desc(.data$count))

  } else if(domain %in% c("Study","Country","Statistic")){

    plotDict <- conceptDict  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concepts = .data$MeSH_term) %>%
      dplyr::filter(.data$domain_id == domain) %>%
      dplyr::arrange(desc(.data$count))

  }
  return(plotDict)
}


#' Plot specific OMOP results for a given results object and domain
#' @param conceptDict A dictionary of keywords created via makeDict with
#' attached OMOP concept name and ID information
#' @param domain The specific OMOP domain to be plotted
#' @param N Function will plot the N highest scoring (by count) terms
#' @param res a cdm_reference object created using the CDMConnector package
#' @export
plotResultsBar <- function(res, domain, N, conceptDict){
  eb <- ggplot2::element_blank()

  if(domain == "Journal"){

    plotDict <- as.data.frame(table(res$journal))
    colnames(plotDict) <- c("concepts","count")
    plotDict <- plotDict[order(plotDict$count, decreasing = T),]

  } else {

    plotDict <- makePlotDict(conceptDict,domain)

  }

  plotDict$concepts <- factor(plotDict$concepts, levels = rev(plotDict$concepts))

  N <- min(N,length(plotDict$concepts))

  p1 <- ggplot2::ggplot(plotDict[1:N,],
                        ggplot2::aes(x=.data$count,y=.data$concepts,fill=.data$concepts)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb, axis.text.y = ggplot2::element_text(size=10),
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
#' @param N Function will plot the N highest scoring (by count) terms
#' @param res a cdm_reference object created using the CDMConnector package
#' @export
plotCumulative <- function(res, domain, N, conceptDict){
  eb <- ggplot2::element_blank()

  if(domain == "Journal"){

    cumuDate <- cumuDate_Journal(res)

    cumuDate <- cumuDate %>%
      dplyr::mutate(Date = rownames(cumuDate))

    cumuDate <- rbind(cumuDate,cumuDate[length(cumuDate$Date),])

    cumuDate <- cumuDate %>%
      tidyr::pivot_longer(cols = colnames(cumuDate)[-length(colnames(cumuDate))])

    plotDict <- as.data.frame(table(res$journal))
    plotDict <- cbind(plotDict,plotDict[,1])
    colnames(plotDict) <- c("MeSH_term","count","concepts")
    plotDict <- plotDict[order(plotDict$count, decreasing = T),]

  } else {

    cumuDate <- cumuDate(res)

    cumuDate <- cumuDate %>%
      dplyr::mutate(Date = rownames(cumuDate))

    cumuDate <- rbind(cumuDate,cumuDate[length(cumuDate$Date),])

    cumuDate <- cumuDate %>%
      tidyr::pivot_longer(cols = colnames(cumuDate)[-length(colnames(cumuDate))])

    plotDict <- makePlotDict(conceptDict,domain)

  }

  N <- min(N,length(plotDict$concepts))

  plotDate <- cumuDate[cumuDate$name %in% plotDict[1:N,]$MeSH_term,] %>%
    dplyr::left_join(plotDict,by = c("name" = "MeSH_term")) %>%
    dplyr::mutate(Date = lubridate::as_date(.data$Date))

  plotDate_max <- plotDate[plotDate$Date == max(plotDate$Date),]
  plotDate_max$Date <- plotDate_max$Date %m+% lubridate::period("1 month")
  plotDate <- rbind(plotDate,plotDate_max)

  for(i in c(1:length(unique(plotDate$name)))){
    plotDate[plotDate$name == unique(plotDate$name)[i],]$value <-
      (plotDate[plotDate$name == unique(plotDate$name)[i],]$value - 0.15) +
      i*0.3/N
  }

  #nudgeVal_Y <- max(plotDate$value)/25
  #nudgeVal_X <- (max(as.numeric(plotDate$Date))-min(as.numeric(plotDate$Date)))/5

  ggplot2::ggplot(plotDate, ggplot2::aes(x = .data$Date,y=.data$value,group=.data$concepts,colour=.data$concepts)) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::scale_color_viridis_d() +
    #ggplot2::geom_text(data = plotDate[plotDate$Date == max(plotDate$Date),],
    #                   ggplot2::aes(label = .data$concepts), check_overlap = T,
    #                   size = 3, nudge_x = nudgeVal_X, nudge_y = nudgeVal_Y, show.legend = F) +
    ggplot2::scale_x_date(breaks =
                            scales::pretty_breaks(n = length(unique(lubridate::year(plotDate$Date)))+1),
                            limits = c(min(plotDate$Date),
                                       max(plotDate$Date))) +
    ggplot2::geom_point(ggplot2::aes(colour=.data$concepts),shape=15,size=0) +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = 'black'),
                   axis.line.y = ggplot2::element_line(color = "black")) +
    ggplot2::guides(color=ggplot2::guide_legend(title = NULL, override.aes = ggplot2::aes(size=4))) +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb, legend.key = eb, legend.background = eb,
                   legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Count") +
    ggplot2::coord_cartesian(clip = "off")

}

#' Plot a map of the country of origin MeSH term
#' @param conceptDict A dictionary of keywords created via makeDict
#' @export
plotMap <- function(conceptDict){
  eb <- ggplot2::element_blank()

  plotDict <- stats::na.omit(conceptDict[conceptDict$domain_id == "Country",c(3,5)])

  world <- ggplot2::map_data("world") %>%
    dplyr::left_join(plotDict, by = c("region" = "MeSH_term"))

  colnames(world)[7] <- "Results"

  ggplot2::ggplot(world, ggplot2::aes(.data$long,.data$lat,fill=.data$Results,group=.data$Results)) +
    ggplot2::geom_map(map = world, ggplot2::aes(map_id = .data$region)) +
    ggplot2::scale_fill_viridis_c(na.value = "grey80") +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title = eb, axis.text = eb, axis.ticks = eb,
                   axis.line = eb,
                   legend.position = "right")
}

#' Plot a bar chart of rollup terms
#' @param conceptDict A dictionary of keywords created via makeDict
#' @export
plotRollUp <- function(conceptDict, N){
  eb <- ggplot2::element_blank()

  suppressMessages(

  plotDict <- conceptDict %>%
    filter(!is.na(rollup_name)) %>%
    group_by(rollup_name, domain_id) %>%
    summarise(across(.cols = "count", list(sum)))

  )

  plotDict <- plotDict[order(plotDict$count_1, decreasing = T),]
  plotDict$rollup_name <- factor(plotDict$rollup_name, levels = rev(plotDict$rollup_name))

  N <- min(N,length(plotDict$rollup_name))

  p1 <- ggplot2::ggplot(plotDict[1:N,],
                        ggplot2::aes(x=.data$count_1,y=.data$rollup_name,fill=.data$rollup_name)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb, axis.text.y = ggplot2::element_text(size=10),
                   legend.position = "None") +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = 'black')) +
    ggplot2::xlab("Count") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())

  p1

  return(p1)
}
