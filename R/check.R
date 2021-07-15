# Check ====================================================================
checkAbnormalNodes <- function(s1, s2, f1, f2, thr_cos, node_num_cutoff,
                               edge_matrix_full, dict.combine, session){
  .data = NULL
  if(length(s1)+length(s2)!=0){
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
      inst = rhd %>% dplyr::group_by(.data[["type"]], .data[["Capinfo2"]]) %>% dplyr::summarise(count=length(.data[["Variable"]]))
      colnames(inst) = c("type","class","count")
      if(sum(inst$count) > node_num_cutoff){
        shinyWidgets::ask_confirmation(
          inputId = "myconfirmation",
          title = "Confirmation",
          text = paste0("Based on your selection, there will be ",
                        sum(inst$count), "(>",node_num_cutoff, ") connected nodes to be drawn, which 
                          may slow down the app speed. Do you really want to plot them?")
        )
      }else{
        if(sum(inst$count) == 0 | nrow(inst) == 0){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Information",
            text = "There is no connected nodes based on your specified conditions. Try to relax
              some like decreasing the lower bound of cosine similarity.",
            type = "info"
          )
        }
      }
    }else{
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Information",
        text = "There is no connected nodes based on your specified conditions. Try to relax
              some like decreasing the lower bound of cosine similarity.",
        type = "info"
      )
    }
  }
}
