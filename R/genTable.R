
# Table Section ================================================================
## Generate Summary table ======================================================
summaryTable <- function(s1, s2, f1, f2, thr_cos,
                         edge_matrix_full, dict.combine){
  .data = NULL
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
      a = rownames(edge.matrix)
      edge.matrix = as.matrix(edge.matrix)
    }
    edge.matrix[edge.matrix<thr_cos]  = 0
    if(sum(edge.matrix)>0){
      nodes = colnames(edge.matrix)[colSums(edge.matrix)>thr_cos]
      rhd = dict.combine[match(nodes,dict.combine$Variable), 
                         c("Variable","type","Capinfo2")]
      rhd = rhd[rhd$Capinfo2 %in% f1|rhd$Capinfo2 %in% f2,]
      inst = rhd %>% dplyr::group_by(.data[["type"]], .data[["Capinfo2"]]) %>% dplyr::summarise("count"=length(.data[["Variable"]]))
      colnames(inst) = c("type","class","count")
      inst = rbind(data.frame("type"="Sum","class"="-","count"=sum(inst$count)),
                   inst)
      inst
    }
  }
}

## Generate clicked node info table ======================================================
clickedTable <- function(node_now, edge_matrix_full, dict.combine){
  edge.ma.now = edge_matrix_full
  loc.node_now = match(node_now, rownames(edge.ma.now))
  if(!is.na(loc.node_now)){
    node.id = edge.ma.now[loc.node_now,]!=0
    connected.node_now = colnames(edge.ma.now)[node.id]
    result = data.frame(
      "Type" = dict.combine$type[match(connected.node_now,dict.combine$Variable)],
      "Group" = dict.combine$Capinfo2[match(connected.node_now,dict.combine$Variable)],
      "Neighbor id"=connected.node_now,
      "Description"=dict.combine$Description[match(connected.node_now,dict.combine$Variable)],
      "CosineSim"=edge.ma.now[loc.node_now, node.id]
    )
    result = result[order(result$CosineSim, decreasing = TRUE),]
    result
  }
}

## Clicked node text ============================================================
clickedNodeText <- function(s1, s2, node_now, 
                            edge_matrix_full, dict.combine){
  s = c(s1, s2)
  node_now_name = dict.combine$Description[match(node_now,dict.combine$Variable)]
  node_now_group = dict.combine$Capinfo2[match(node_now,dict.combine$Variable)]
  node_now_type = dict.combine$type[match(node_now,dict.combine$Variable)]
  edge.ma.now = edge_matrix_full
  loc.node_now = match(node_now, rownames(edge.ma.now))
  if(!(node_now %in% s)){
    node_now_center = "nonCenter"
    if(length(node_now_name)>0 & !is.na(loc.node_now)){
      node_now_identity = "in the interested list"
    }else{
      node_now_identity = "not in the interested list"
    }
  }else{
    node_now_identity = "in the interested list"
    node_now_center = "center"
  }
  HTML(paste0("<h3> ",node_now,": ",node_now_name, " </h3>", 
              "<br> More info:", node_now_type, ", ", node_now_group,", ",
              node_now_center,", ",node_now_identity," </br>"))
}


## Generate downloading table================================================
download_table <- function(s1, s2, f1, f2, thr_cos,
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
    }
    edge.matrix[edge.matrix<thr_cos]  = 0
    if(sum(edge.matrix)>0){
      draw.data = vis.InOutNodes(root.node, dict.combine, edge_matrix_full, thr_cos,
                                 color.df)
      nodes = draw.data[[1]]
      edges = draw.data[[2]]
      
      nodes$selected = "Not filtered"
      filter_ind = c(f1, f2)
      if(!is.null(filter_ind)){
        nodes$selected[(nodes$capinfo2 %in% filter_ind) |
                         (nodes$iscenter == 1)] = "Filtered"
      }
      edges = edges[!(edges$from %in% unique(nodes$id[nodes$selected=="Not filtered"])) &
                      !(edges$to %in% unique(nodes$id[nodes$selected=="Not filtered"])),]
      
      nodes = nodes[nodes$selected=="Filtered",]
      file = edges[,c(2,1,4)]
      colnames(file) = c("target","connected","cosine_similarity")
      file$target_label = dict.combine$Description[match(file$target,dict.combine$Variable)]
      file$connected_label = dict.combine$Description[match(file$connected,dict.combine$Variable)]
      file = file[,c("target","target_label","connected","connected_label","cosine_similarity")]
      file = file[order(file$cosine_similarity, decreasing = TRUE),]
    }
  }else{
    file = data.frame("Warning"="Try to click some rows in the 'Possible inputs' box to specify your nodes!")
  }
  return(file)
}

