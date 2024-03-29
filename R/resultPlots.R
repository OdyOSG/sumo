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

  plotDict$concepts <- factor(plotDict$concepts, levels = rev(unique(plotDict$concepts)))

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
#' @param cumuDate Function will plot the N highest scoring (by count) terms
#' @param res a cdm_reference object created using the CDMConnector package
#' @export
plotCumulative <- function(res, domain, N, conceptDict, cumuDate){
  eb <- ggplot2::element_blank()

  if(domain == "Journal"){

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

  plotDate_max <- plotDate_max %>%
    arrange(desc(value))

  plotDate$concepts <- factor(plotDate$concepts, levels = unique(plotDate_max$concepts))

  ggplot2::ggplot(plotDate, ggplot2::aes(x = .data$Date,y=.data$value,group=.data$concepts,colour=.data$concepts)) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_x_date(breaks =
                            scales::pretty_breaks(n = length(unique(lubridate::year(plotDate$Date)))+1),
                            limits = c(min(plotDate$Date),
                                       max(plotDate$Date))) +
    ggplot2::geom_point(ggplot2::aes(colour=.data$concepts),shape=15,size=0) +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = 'black')) +
    ggplot2::guides(color=ggplot2::guide_legend(title = NULL, override.aes = ggplot2::aes(size=4))) +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb, legend.key = eb, legend.background = eb,
                   legend.text = ggplot2::element_text(size = 12),
                   legend.position = "bottom") +
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
plotRollUp <- function(conceptDict, N=10){
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

#' Plot a bar chart of condition/drug overlap
#' @param res A dictionary of keywords created via makeDict
#' @param conceptDict A dictionary of keywords created via makeDict
#' @param N Function will plot the N highest scoring (by count) terms
#' @export
plotOverlap <- function(res, conceptDict, N){
  eb <- ggplot2::element_blank()

  res_reduced <- res[,c(1,8)]
  res_reduced <- res_reduced[!res_reduced$conceptIds=="",]

  res_reduced <- res_reduced %>%
    dplyr::mutate(conceptIds = strsplit(as.character(conceptIds), ";")) %>%
    tidyr::unnest(conceptIds) %>%
    dplyr::mutate(conceptIds = as.numeric(conceptIds))

  res_reduced <- res_reduced %>%
    dplyr::left_join(conceptDict[,c(1,3,4)], by = c("conceptIds"="concept_id_1")) %>%
    dplyr::filter(domain_id %in% c("Condition","Drug"))

  overlapIds <- c()
  overlapNames <- c()

  for(pmid in unique(res_reduced$pmid)){

    res_temp <- res_reduced[res_reduced$pmid == pmid,]

    if("Condition" %in% res_temp$domain_id){
      if("Drug" %in% res_temp$domain_id){
        res_c <- res_temp[res_temp$domain_id=="Condition",]
        res_d <- res_temp[res_temp$domain_id=="Drug",]
        for(conceptC in unique(res_c$conceptIds)){
          for(conceptD in unique(res_d$conceptIds)) {
            overlapId = paste(conceptC,":",conceptD,sep="")
            overlapIds <- c(overlapIds,overlapId)

            overlapName = paste(res_temp[res_temp$conceptIds==conceptC,]$MeSH_term,":",
                                res_temp[res_temp$conceptIds==conceptD,]$MeSH_term,sep="")
            overlapNames <- c(overlapNames,overlapName)
          }
        }
      }
    }
  }

  resIds <- as.data.frame(table(overlapIds)[order(table(overlapIds), decreasing = T)])
  resNames <- as.data.frame(table(overlapNames)[order(table(overlapNames), decreasing = T)])

  N <- 10

  resNames[1:min(N,dim(resNames)[1]),] %>%
    ggplot2::ggplot(ggplot2::aes(x=overlapNames,y=Freq, fill = overlapNames)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(panel.background = eb,
                   axis.title = eb, axis.text.y = ggplot2::element_text(size=10),
                   legend.position = "None",
                   axis.text.x = ggplot2::element_text(angle = 75, vjust = 1,hjust=1, size = 10)) +
    ggplot2::ggtitle("Overlap of Conditions:Drugs")

}


#' Plot specific OMOP results for a given results object and domain
#' @param conceptDict A dictionary of keywords created via makeDict with
#' attached OMOP concept name and ID information
#' @param N Function will plot the N highest scoring (by count) terms
#' @param res a cdm_reference object created using the CDMConnector package
#' @param cumuDate A cumudate object
#' @export
plotCumulativeRollup <- function(res, N = 10, conceptDict, cumuDate) {

  N <- 20

  eb <- ggplot2::element_blank()

  cumuDate <- cumuDate %>%
    dplyr::mutate(Date = rownames(cumuDate))

  cumuDate <- rbind(cumuDate,cumuDate[length(cumuDate$Date),])

  cumuDate <- cumuDate %>%
    tidyr::pivot_longer(cols = colnames(cumuDate)[-length(colnames(cumuDate))])

  cumuDate <- cumuDate[!duplicated(paste(cumuDate$Date,cumuDate$name,cumuDate$value)),]

  plotDict <- conceptDict  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(concepts = paste(.data$rollup_name,"\n (",
                                   .data$rollup_id,")",
                                   collapse="",sep="")) %>%
    dplyr::arrange(desc(.data$count)) %>%
    dplyr::select(rollup_name,MeSH_term)

  plotDict <- plotDict[!is.na(plotDict$rollup_name),]

  plotDate <- cumuDate %>%
    dplyr::inner_join(plotDict,by = c("name" = "MeSH_term")) %>%
    dplyr::mutate(Date = lubridate::as_date(.data$Date))

  plotDate_max <- plotDate[plotDate$Date == max(plotDate$Date),]
  plotDate_max$Date <- plotDate_max$Date %m+% lubridate::period("1 month")
  plotDate <- rbind(plotDate,plotDate_max)

  plotDate_max <- plotDate_max %>%
    dplyr::arrange(desc(value))

  N <- min(N,length(unique(plotDate_max$rollup_name)))
  toPlot <- plotDate_max$rollup_name[1:N]

  plotDate.f <- plotDate[plotDate$rollup_name %in% toPlot,]

  plotDate.f <- plotDate.f %>%
    aggregate(value~Date+rollup_name,sum)

  plotDate_max <- plotDate.f[plotDate.f$Date == max(plotDate.f$Date),]

  plotDate_max <- plotDate_max %>%
    dplyr::arrange(desc(value))

  plotDate.f$rollup_name <- factor(plotDate.f$rollup_name, levels = unique(plotDate_max$rollup_name))

  ggplot2::ggplot(plotDate.f, ggplot2::aes(x = .data$Date,y=.data$value,group=.data$rollup_name,colour=.data$rollup_name)) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_x_date(breaks =
                            scales::pretty_breaks(n = length(unique(lubridate::year(plotDate$Date)))+1),
                          limits = c(min(plotDate$Date),
                                     max(plotDate$Date))) +
    ggplot2::geom_point(ggplot2::aes(colour=.data$rollup_name),shape=15,size=0) +
    ggplot2::theme(axis.line.x = ggplot2::element_line(color = 'black')) +
    ggplot2::guides(color=ggplot2::guide_legend(title = NULL, override.aes = ggplot2::aes(size=4))) +
    ggplot2::theme(panel.grid.major = eb, panel.grid.minor = eb,
                   panel.background = eb, panel.border = eb,
                   axis.title.y = eb, legend.key = eb, legend.background = eb,
                   legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Count") +
    ggplot2::coord_cartesian(clip = "off")
}
