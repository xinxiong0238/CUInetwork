## Generate network plot using visnetwork ====================================
networkVIS <- function(s1, s2, f1, f2, thr_cos,node_num_cutoff,
                       myconfirmation, slider_text, slider_size,cluster,
                       edge_matrix_full, dict.combine, color.df){
  if(length(s1)+length(s2)!=0 & (!is.null(f1)|!is.null(f2))){
    input.correct = c(s1,s2)
    root.node = match(input.correct,rownames(edge_matrix_full))
    n.root = length(root.node)
    if(n.root==1){
      edge.matrix = t(as.matrix(edge_matrix_full[root.node,]))
      rownames(edge.matrix) = rownames(edge_matrix_full)[root.node]
      a = rownames(edge.matrix)
      edge.matrix[a, a[a %in% colnames(edge.matrix)]] = 0
    }else{
      edge.matrix = edge_matrix_full[root.node,]
      edge.matrix = as.matrix(edge.matrix)
      a = rownames(edge.matrix)
    }
    edge.matrix[edge.matrix<thr_cos]  = 0
    if(sum(edge.matrix)>0){
      draw.data = vis.InOutNodes(root.node, dict.combine, edge_matrix_full, thr_cos,
                                 color.df)
      nodes = draw.data[[1]]
      edges = draw.data[[2]]
      type.legend = draw.data[[3]][[1]]
      ledges = draw.data[[3]][[2]]
      
      iid=4
      
      group.target = draw.data[[iid]][[1]]
      group.other = draw.data[[iid]][[2]]
      text.group.color = draw.data[[iid]][[3]]
      
      nodes$selected = "Not filtered"
      filter_ind = c(f1, f2)
      if(!is.null(filter_ind)){
        nodes$selected[(nodes$capinfo2 %in% filter_ind) |
                         (nodes$iscenter == 1)] = "Filtered"
      }
      edges = edges[!(edges$from %in% unique(nodes$id[nodes$selected=="Not filtered"])) &
                      !(edges$to %in% unique(nodes$id[nodes$selected=="Not filtered"])),]
      
      nodes = nodes[nodes$selected=="Filtered",]
      myconfirm = ifelse(is.null(myconfirmation),1,
                         ifelse(myconfirmation==FALSE,2,3))
      if(nrow(nodes)<node_num_cutoff | myconfirm==3){
        nodes$font.size = unlist(nodes$font.size)
        nodes$font.size[which(nodes$iscenter==0)] = nodes$font.size[which(nodes$iscenter==0)] * slider_text
        nodes$size[nodes$iscenter==0] = nodes$size[nodes$iscenter==0] * slider_size
        if(cluster!="None"){
          visNetwork::visNetwork(nodes, edges, width = "100%",height = "100%") %>%
            visNetwork::visNodes(color = list(background = "lightblue",
                                  border = "darkblue",
                                  highlight = "yellow"),
                     shadow = list(enabled = TRUE, size = 10)) %>%
            visNetwork::visEdges(
              physics = TRUE,
              smooth = FALSE,
              hoverWidth = 2.5) %>%
            visNetwork::visOptions(
              highlightNearest =
                list(enabled = T, degree = 1, hover = T,
                     hideColor = "rgba(200,200,200,0.05)"),
              # selectedBy = list(variable="selected",
              #                    multiple=TRUE,
              #                    selected="Filtered",
              #                    main="Filter by selected groups?",
              #                    hideColor="rgba(0,0,0,0)",
              #                    style='width: 225px; height: 30px'),
              #clickToUse = TRUE,
              collapse = FALSE) %>%
            visNetwork::visLegend(width = 0.09, position = "right",
                      #          addNodes = type.legend,
                      addEdges = ledges,
                      useGroups = FALSE, zoom = FALSE,
                      stepX = 150, stepY = 75,ncol=1) %>%
            visNetwork::visInteraction(hover = TRUE,
                           navigationButtons = TRUE) %>%
            visNetwork::visPhysics(barnesHut = list("avoidOverlap"=0.05,
                                        "sprintConstant"=0.1)) %>%
            visNetwork::visIgraphLayout(layout = "layout_nicely",physics = TRUE,
                            smooth = TRUE) %>%
            
            visNetwork::visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}") %>%
            
            visNetwork::visClusteringByGroup(groups = c(group.target,group.other),
                                 label = "Cluster:\n",
                                 scale_size = TRUE,
                                 shape = c(rep("ellipse",length(group.target)),
                                           rep("ellipse",length(group.other))),
                                 color = c(rep("#9955FF",length(group.target)),
                                           text.group.color),
                                 force = TRUE)%>%
            
            visNetwork::visLayout(randomSeed = 10) # to have always the same network
        }else{
          #nodes$mass[1:length(root.node)]=max(7,min(length(nodes$id)*0.5,30))
          group.legend = list(type.legend[[1]],type.legend[[2]])
          visNetwork::visNetwork(nodes, edges, width = "100%",height = "100%") %>%
            visNetwork::visNodes(color = list(background = "lightblue",
                                  border = "darkblue",
                                  highlight = "yellow"),
                     shadow = list(enabled = TRUE, size = 10)) %>%
            visNetwork::visEdges(
              #physics = FALSE,
              smooth = FALSE,
              hoverWidth = 2.5) %>%
            visNetwork::visOptions(
              highlightNearest =
                list(enabled = T, degree = 2, hover = T,
                     hideColor = "rgba(200,200,200,0.05)"),
              # selectedBy = list(variable="selected",
              #                    multiple=TRUE,
              #                    selected="Filtered",
              #                    main="Filter by selected groups?",
              #                    hideColor="rgba(0,0,0,0)",
              #                    style='width: 225px; height: 30px'),
              #clickToUse = TRUE,
              collapse = FALSE) %>%
            visNetwork::visLegend(width = 0.09, position = "right",
                      addNodes = type.legend,
                      #addEdges = ledges,
                      useGroups = FALSE, zoom = FALSE,
                      stepX = 150, stepY = 75,ncol=1) %>%
            visNetwork::visInteraction(hover = TRUE,
                           navigationButtons = TRUE) %>%
            visNetwork::visPhysics(barnesHut = list("avoidOverlap"=0.05,
                                        "sprintConstant"=0.1)) %>%
            visNetwork::visIgraphLayout(
              layout = "layout_nicely",
              physics = TRUE,
              smooth = TRUE) %>%
            visNetwork::visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}") %>%
            
            visNetwork::visLayout(randomSeed = 10) # to have always the same network
        }
      }
    }
  }else{
    visNetwork::visNetwork(data.frame(), data.frame(), width = "100%",
               main = paste("Try to click some rows in",tagList(icon("table")),"to specify your nodes"))
  }
  
  
}