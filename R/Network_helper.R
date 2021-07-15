vis.InOutNodes <- function(root.node, dict.combine, edge_matrix_full, thr_cos, color.df){
  n.root = length(root.node)
  if(n.root==1){
    edge.matrix = t(as.matrix(edge_matrix_full[root.node,]))
    rownames(edge.matrix) = rownames(edge_matrix_full)[root.node]
    a = rownames(edge.matrix)
    edge.matrix[a, a[a %in% colnames(edge.matrix)]] = 0
  }else{
    edge.matrix = edge_matrix_full[root.node,]
    a = rownames(edge.matrix)
    #diag(edge.matrix[a, a[a %in% colnames(edge.matrix)]]) = 0
  }
  edge.matrix[edge.matrix<thr_cos]  = 0
  ### nodes
  nodes = data.frame(id = unique(c(rownames(edge.matrix),
                                   colnames(edge.matrix)[colSums(edge.matrix)>0])))
  n.nonroot = nrow(nodes) - n.root
  nodes$id = as.character(nodes$id)
  node_index = match(nodes$id,dict.combine$Variable)
  title = dict.combine$Description[node_index]
  nodes = cbind(nodes, data.frame(label = dict.combine$Description_s[node_index],
                                  iscenter = c(rep(1,n.root),rep(0,n.nonroot)),
                                  group = dict.combine$group[node_index],
                                  lgroup = dict.combine$lgroup[node_index],
                                  type = dict.combine$type[node_index],
                                  capinfo = dict.combine$Capinfo[node_index],
                                  capinfo2 = dict.combine$Capinfo2[node_index],
                                  shape = c(rep("triangle",n.root),rep("triangle",n.nonroot)),
                                  mass = c(rep(6,n.root),rep(4,n.nonroot)),
                                  font.size = c(rep(50,n.root),rep(32,n.nonroot)),
                                  size =  c(rep(30,n.root),rep(20,n.nonroot)),
                                  font.background = c(rep("#FFFFF0",n.root),rep("",n.nonroot)),
                                  borderWidth = c(rep(3,n.root),rep(2,n.nonroot)),
                                  borderWidthSelected = c(rep(3,n.root),rep(2,n.nonroot))))
  
  nodes$group = as.character(nodes$group)
  nodes$group[1:n.root] = as.character(nodes$label[1:n.root])
  nodes$group[is.null(nodes$group)] = nodes$id[is.null(nodes$group)]
  nodes$group[is.na(nodes$group)] = nodes$id[is.na(nodes$group)]
  nodes$group[nodes$group=="null"] = nodes$id[nodes$group=="null"]
  nodes$cui_group = nodes$noncui_group = nodes$group
  nodes$cui_group[nodes$type=="nonCUI"] = NA
  nodes$noncui_group[nodes$type=="CUI"] = NA
  
  node.used = c()
  edges.dataframe = data.frame()
  for (i in 1:nrow(edge.matrix)) {
    edges = data.frame()
    to = from = rownames(edge.matrix)[i]
    newtofrom = colnames(edge.matrix)[which(edge.matrix[i,]!=0)]
    newtofrom = newtofrom[newtofrom!=to]
    color.opa = c()
    if(length(newtofrom)!=0){
      newtofrom.value = edge.matrix[i,newtofrom]
      rr = rank(newtofrom.value,ties.method = "random")
      newtofrom[rr] = newtofrom
      newtofrom.value = sort(newtofrom.value)
      length=sapply(abs(1-newtofrom.value)*10,function(x) max(x,1))
      color.opa = c(color.opa,newtofrom.value)
      too = as.character(rep(to,length(newtofrom)))
      
      newtofrom.name = dict.combine$Description[match(newtofrom, dict.combine$Variable)]
      to.name = dict.combine$Description[match(too, dict.combine$Variable)]
      to.group = dict.combine$Capinfo2[match(too,dict.combine$Variable)]
      to.type = dict.combine$type[match(too, dict.combine$Variable)]
      
      newtofrom.group = dict.combine$Capinfo2[match(newtofrom, dict.combine$Variable)]
      newtofrom.type = dict.combine$type[match(newtofrom, dict.combine$Variable)]
      
      edges = rbind(edges, data.frame(from=as.character(newtofrom),
                                      to=as.character(rep(to,length(newtofrom))),
                                      edgetype="both directions",
                                      cos=newtofrom.value,
                                      length=length,
                                      hidden=FALSE,
                                      title= paste0("- ", to.name, " (",to.type," ",to.group,")",
                                                    "<br> - ", newtofrom.name," (",newtofrom.type," ",newtofrom.group,")",
                                                    "<br> - Cosine: ", newtofrom.value
                                      )
      )
      )
      edges = edges[edges$from!=edges$to,]
      
    }
    node.used = c(node.used, rownames(edge.matrix)[i])
    color.opa = sapply(color.opa+0.15,function(x) min(max(x,0.2),1))
    if(is.na(color.opa[1])){
      color.opa = 1
    }
    if(nrow(edges)!=0){
      color.color = paste0("rgba(221,221,221,", color.opa,")")
      color.highlight = "rgba(0,0,0,1)"
      edges = cbind(edges, data.frame(color.color=color.color,
                                      color.opa = color.opa,
                                      color.highlight=color.highlight,
                                      smooth = TRUE,
                                      width=2.5,
                                      selectionWidth=3.5))
      edges$length = unlist(edges$length)
      edges.dataframe = rbind(edges.dataframe,edges)
    }
    # edges_id_uni = unique(c(edges$from,edges$to))
    # cod_uni = unique(dict.combine$Capinfo[match(edges_id_uni,dict.combine$Variable)])
    # cod_uni = setdiff(cod_uni,"CUI")
    # for (j in 1:length(cod_uni)) {
    #   nodes_sub = nodes[nodes$phetype==cod_uni[j] & nodes$id %in% edges_id_uni,]
    #   if(nrow(nodes_sub)>=2){
    #     edges_sub = edges[1:(nrow(nodes_sub)-1),]
    #     edges_sub$from = nodes_sub$id[-1]
    #     edges_sub$to = nodes_sub$id[-nrow(nodes_sub)]
    #     edges_sub$color.color = "rgba(0,0,0,0)"
    #     edges_sub = edges_sub[!(edges_sub$from %in% to | edges_sub$to %in% to),]
    #     edges_sub$edgetype = "only display"
    #     edges_sub$length = 0.01
    #     edges_sub$hidden=TRUE
    #     edges_sub$title = NA
    #     edges.dataframe = rbind(edges.dataframe,edges_sub)
    #   }
    # }
  }
  edges = edges.dataframe
  
  # if centers are connected, bold + change color
  a = edges$length[edges$from %in% rownames(edge.matrix) &
                     edges$to %in% rownames(edge.matrix)]
  edges$length[edges$from %in% rownames(edge.matrix) &
                 edges$to %in% rownames(edge.matrix)] = sapply(a, function(x){max(x,100*min(10,nrow(edge.matrix)))})
  edges$width[edges$from %in% rownames(edge.matrix) &
                edges$to %in% rownames(edge.matrix)] = 4.5
  edges$hoverWidth[edges$from %in% rownames(edge.matrix) &
                     edges$to %in% rownames(edge.matrix)] = 
    edges$selectionWidth[edges$from %in% rownames(edge.matrix) &
                           edges$to %in% rownames(edge.matrix)] = 4.5
  edges$smooth[edges$from %in% rownames(edge.matrix) &
                 edges$to %in% rownames(edge.matrix)] = FALSE
  #edges$color.highlight = edges$color.color
  
  
  ##### code colors!!!!
  
  type.legend = rep(list(1),length(unique(color.df$color)))
  for(i in 1:length(unique(color.df$color))){
    type.legend[[i]] = list(label = color.df$name[i],shape=color.df$shape[i],
                            color=paste0(color.df$colors.rgb[i],"1)"),
                            physics = FALSE,size=10)
  }
  nodes.colors = nodes.colors.select = nodes$id
  ledges <- data.frame(color = c("rgb(0,0,0)"),
                       label = c("connected"), 
                       width = 3, arrows = c(""),
                       physics = FALSE,font.align = c("top"),font.size=10)  
  for (i in 1:nrow(color.df)) {
    nodes.colors[which(nodes$capinfo2==color.df$name[i])] = color.df$colors.rgb[i]
    nodes.colors.select[which(nodes$capinfo2==color.df$name[i])] = color.df$colors.rgb[i]
  }
  
  nodes$color.opa = sapply(1:length(nodes$id), function(i){
    if(!(nodes$id[i] %in% edges$from | nodes$id[i] %in% edges$to)){
      1
    }else{
      max(edges$color.opa[nodes$id[i]==edges$from],
          edges$color.opa[nodes$id[i]==edges$to])
    }
  })
  nodes$color.background = paste0(nodes.colors,nodes$color.opa,")")
  nodes$color.highlight.background = paste0(nodes.colors,"1)")
  nodes$color.hover.background = paste0(nodes.colors,"1)")
  
  nodes$color.border = nodes$color.background
  
  nodes$color.highlight.border = nodes$color.highlight.background
  nodes$color.hover.border = nodes$color.hover.background
  
  #nodes$color.background[1:n.root] = nodes$color.highlight.background[1:n.root] =
  #  nodes$color.hover.background[1:n.root] =  nodes$color.border[1:n.root] = "#FF0000"
  nodes$color.highlight.border[1:n.root] = "#FF0000"
  
  nodes$size = nodes$size * nodes$color.opa
  
  nodes$font.size = sapply(nodes$color.opa*30,function(x){max(min(x,25),10)})
  nodes$font.size[stringr::str_length(nodes$label)>40] = 
    nodes$font.size[stringr::str_length(nodes$label)>40]*40/
    stringr::str_length(nodes$label[stringr::str_length(nodes$label)>40])
  nodes$font.size[stringr::str_length(nodes$label)>40] = sapply(nodes$font.size[stringr::str_length(nodes$label)>40],
                                                       function(x){max(x,10)})
  nodes$font.size[1:n.root] = 40
  nodes$shape[nodes$type=="nonCUI"] = "dot"
  ### If cluster by both
  group.target = unique(nodes$group[1:n.root])
  group.other = stats::na.omit(setdiff(unique(nodes$group),group.target))
  text.group.color = NA
  if(length(group.other)!=0){
    text.group.color = paste0("rgba(255,99,71,", sapply(group.other, function(group){
      max(nodes$color.opa[which(nodes$group==group)])}),")")
    
  }
  text.group.color = stats::na.omit(unlist(text.group.color))
  
  
  ### If cluster by CUI
  cui.target = unique(nodes$cui_group[1:n.root])
  cui_group.other = stats::na.omit(setdiff(unique(nodes$cui_group),cui.target))
  text.cui_group.color = NA
  if(length(cui_group.other)!=0){
    text.cui_group.color = paste0("rgba(255,99,71,", sapply(cui_group.other, function(group){
      max(nodes$color.opa[which(nodes$cui_group==group)])}),")")
    
  }
  text.cui_group.color = stats::na.omit(unlist(text.cui_group.color))
  
  
  ### If cluster by phetypes
  noncui.target = unique(nodes$noncui_group[1:n.root])
  noncui_group.other = stats::na.omit(setdiff(unique(nodes$noncui_group),noncui.target))
  text.noncui_group.color = NA
  if(length(noncui_group.other)!=0){
    text.noncui_group.color = paste0("rgba(255,99,71,", sapply(noncui_group.other, function(group){
      max(nodes$color.opa[which(nodes$noncui_group==group)])}),")")
    
  }
  text.cui_group.color = stats::na.omit(unlist(text.cui_group.color))
  
  
  nodes$title = NA
  nodes$title[which(nodes$type=='CUI')] =
    paste0("<b>",title[which(nodes$type=='CUI')],"</b>",
           "<br>Semantic type:", nodes$cui_group[which(nodes$type=='CUI')],
           "<br>Group:", nodes$lgroup[which(nodes$type=='CUI')])
  #nodes = rbind(nodes[((n.root+1):nrow(nodes)),],nodes[(1:n.root),])
  
  nodes$title[which(nodes$type=='nonCUI')] =
    paste0("<b>",title[which(nodes$type=='nonCUI')],"</b>",
           "<br>ID:", nodes$id[which(nodes$type=='nonCUI')],
           "<br>Group:", nodes$noncui_group[which(nodes$type=='nonCUI')])
  nodes$label[nodes$iscenter==1] = paste0(nodes$capinfo2[nodes$iscenter==1],
                                          "\n",nodes$label[nodes$iscenter==1])
  
  nodes[nodes$iscenter==0,] = nodes[nodes$iscenter==0,] %>% dplyr::arrange("group")
  
  return(list(nodes, edges, list(type.legend,ledges), 
              `both_group`=list(group.target, group.other, text.group.color),
              `cui_group`=list(cui.target, cui_group.other, text.cui_group.color),
              `noncui_group`=list(noncui.target, noncui_group.other, text.noncui_group.color)))
}

