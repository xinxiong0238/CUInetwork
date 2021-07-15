
## Generate interactive circular plot using highcharter ======================
circularInteractive <- function(f1, f2, thr_cos_pop, is_filter,scale_chy,
                                node_now, edge_matrix_full, 
                                dict.combine, color.df){
  node_now_name = dict.combine$Description[match(node_now,dict.combine$Variable)]
  node_now_group = dict.combine$Capinfo2[match(node_now,dict.combine$Variable)]
  node_now_type = dict.combine$type[match(node_now,dict.combine$Variable)]
  edge.ma.now = edge_matrix_full
  loc.node_now = match(node_now, rownames(edge.ma.now))
  if(!is.na(loc.node_now)){
    if(length(node_now_name)>0 & !is.na(node_now_name)){
      select.node = node_now
      data = edge.ma.now[loc.node_now,]
      nodes = names(data[data>thr_cos_pop])
      labels = dict.combine$Description[match(nodes,dict.combine$Variable)]
      groups = dict.combine$Capinfo2[match(nodes,dict.combine$Variable)]
      types = dict.combine$type[match(nodes,dict.combine$Variable)]
      data = data.frame(
        individual=labels,
        group=groups,
        type = types,
        value=data[data>thr_cos_pop]
      )
      data = data %>% dplyr::arrange("type","group", "value")
      data = data[data$value>thr_cos_pop,]
      if(is_filter == TRUE){
        data = data[data$group %in% f1|data$group %in% f2,]
      }
      if(nrow(data)>0){
        df <- data
        df$color = as.factor(color.df$color[match(df$group,color.df$name)])
        df$group_label = paste0("(",df$type,") ",df$group)
        df = df[order(df$type,df$group,df$value),]
        df2 = df[duplicated(df$individual),]
        dupdf = df$individual %in% df2$individual
        df$individual[dupdf] = paste0(df$type[dupdf],", ",
                                      df$individual[dupdf])
        df = df[!duplicated(df$individual),]
        df %>%
          highcharter::hchart(type = "column", highcharter::hcaes(x = "individual", y = "value", group="group_label"), 
                 color=levels(df$color)) %>%
          highcharter::hc_chart(type = 'column', polar = TRUE, inverted = FALSE) %>%
          highcharter::hc_plotOptions(column = list(stacking = "normal", borderWidth=0,
                                       pointPadding=0, groupPadding=0.1)) %>%
          highcharter::hc_yAxis(min=0, max=scale_chy, tickInterval = 0.2, lineWith=1, 
                   title = list(text=""),
                   showLastLabel=TRUE) %>%
          highcharter::hc_xAxis(labels = list(
            allowOverlap=TRUE,
            step=1,
            #align="center",
            rotation=1,
            #autoRotationLimit=1000,
            staggerLines = 2
          ), title = list(text=""))%>%
          highcharter::hc_pane(size= '90%', innerSize='15%',
                  startAngle=10, endAngle=350)
        
      }else{
        highcharter::highchart() %>%
          highcharter::hc_title(
            text = "After filtering, no connected node is left!",
            margin = 20,
            align = "center",
            style = list(color = "black", useHTML = TRUE)
          )
        
      }
    }
  }else{
    highcharter::highchart() %>%
      highcharter::hc_title(
        text = "The node you click is not on the interested list!",
        margin = 20,
        align = "center",
        style = list(color = "black", useHTML = TRUE)
      )
    
  }
}

