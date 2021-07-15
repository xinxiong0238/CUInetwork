

## Generate circular plot using ggplot =======================================
circularPreData <- function(data){
  .data = NULL
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% dplyr::arrange(.data[["group"]])
  data$id=seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data=data %>%
    dplyr::group_by(.data[["group"]]) %>%
    dplyr::summarize(start=min(.data[["id"]]), end=max(.data[["id"]]) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title=mean(c(.data[["start"]], .data[["end"]])))
  
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  if(nrow(grid_data)>1){
    grid_data=grid_data[-1,]
  }else{
    grid_data$start = -1
  }
  return(list(`data` = data, `label_data` = label_data, 
              `grid_data` = grid_data, `base_data` = base_data))
}

circularStatic <- function(f1, f2, thr_cos_pop, is_filter,
                           node_now, edge_matrix_full, 
                           dict.combine, color.df){
  .data = NULL
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
      data = data %>% dplyr::arrange(.data[["type"]],.data[["group"]], .data[["value"]])
      data = data[data$value>thr_cos_pop,]
      if(is_filter == TRUE){
        data = data[data$group %in% f1|data$group %in% f2,]
      }
      if(nrow(data)>0){
        data$value = data$value*100
        data$group = factor(data$group)
        
        circularData = circularPreData(data)
        data = circularData$data
        label_data = circularData$label_data
        grid_data = circularData$grid_data
        base_data = circularData$base_data
        
        # Make the plot
        p = ggplot2::ggplot(data, ggplot2::aes(x=as.factor(.data[["id"]]), y=.data[["value"]], fill=match(.data[["group"]],color.df$name))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
          ggplot2::scale_fill_manual(values = color.df$color) +
          ggplot2::geom_bar(ggplot2::aes(x=as.factor(.data[["id"]]), y=.data[["value"]], fill=.data[["group"]]), stat="identity", alpha=0.5) +
          
          # Add a val=.8/.6/.4/.2 lines. I do it at the beginning to make sur barplots are OVER it.
          ggplot2::geom_segment(data=grid_data, ggplot2::aes(x = .data[["end"]], y = 80, 
                                                             xend = .data[["start"]], yend = 80), 
                                colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          ggplot2::geom_segment(data=grid_data, ggplot2::aes(x = .data[["end"]], y = 60, 
                                                             xend = .data[["start"]], yend = 60), 
                                colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          ggplot2::geom_segment(data=grid_data, ggplot2::aes(x = .data[["end"]], y = 40, 
                                                    xend = .data[["start"]], yend = 40), 
                                colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          ggplot2::geom_segment(data=grid_data, ggplot2::aes(x = .data[["end"]], y = 20, 
                                                    xend = .data[["start"]], yend = 20), 
                                colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          
          # Add text showing the value of each .8/.6/.4/.2 lines
          ggplot2::annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("0.2", "0.4", "0.6", "0.8") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
          
          ggplot2::geom_bar(ggplot2::aes(x=as.factor(.data[["id"]]), y=.data[["value"]], fill=.data[["group"]]), stat="identity", alpha=0.5) +
          ggplot2::ylim(-50,max(stats::na.omit(data$value))+10) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "none",
            axis.text = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            plot.margin = ggplot2::unit(rep(1,4), "cm")
          ) +
          ggplot2::coord_polar() +
          ggplot2::geom_text(data=label_data, ggplot2::aes(x=.data[["id"]], y=.data[["value"]]+3, label=.data[["individual"]], hjust=.data[["hjust"]]), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
          #
          # Add base line information
          ggplot2::geom_segment(data=base_data, ggplot2::aes(x = .data[["start"]], y = -5, xend = .data[["end"]], yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
          ggplot2::geom_text(data=base_data, ggplot2::aes(x = .data[["title"]], y = -10, label=.data[["group"]]), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)
        
        p
      }else{
        plot(1:10,1:10,xaxt="n",yaxt="n",bty="n",type="n",xlab="",ylab="",
             main = "After filtering, no connected node is left!")
      }
    }
  }else{
    plot(1:10,1:10,xaxt="n",yaxt="n",bty="n",type="n",xlab="",ylab="",
         main = "The node you click is not on the interested list!")
    
  }
}

