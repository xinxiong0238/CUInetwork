# Update section ===============================================================
## Update checkboxinput based on selected rows in table=========================
checkboxUpdateBySelectedRows <- function(s, input_table, session){
  if(length(s)==0 | is.null(s)){
    x <- character(0)
    updateCheckboxGroupInput(session, "inCheckboxGroup1",
                             "Selected CUI(s):",
                             choices = x,
                             selected = x)
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             "Selected Codify node(s):",
                             choices = x,
                             selected = x)
  }
  if(length(s)!=0){
    s_cui = s[stringr::str_detect(input_table$id[s],"^C[0-9]")]
    s_noncui = setdiff(s,s_cui)
    if(length(s_cui)==0 | is.null(s_cui)){
      x <- character(0)
      updateCheckboxGroupInput(session, "inCheckboxGroup1",
                               "Selected CUI(s):",
                               choices = x,
                               selected = x)
    }
    if(length(s_noncui)==0 | is.null(s_noncui)){
      x <- character(0)
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               "Selected Codify node(s):",
                               choices = x,
                               selected = x)
    }
    if(length(s_cui)!=0){
      x_cui = input_table$id[s_cui]
      x_cui.name = input_table$terms[s_cui]
      updateCheckboxGroupInput(session, "inCheckboxGroup1",
                               label = paste(length(x_cui), "Selected cui(s):"),
                               choiceValues = x_cui,
                               choiceNames = paste0(x_cui,": ",x_cui.name),
                               selected = x_cui
      )
    }
    if(length(s_noncui)!=0){
      x_noncui = input_table$id[s_noncui]
      x_noncui.name = input_table$terms[s_noncui]
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               label = paste(length(x_noncui), "Selected noncuicode(s):"),
                               choiceValues = x_noncui,
                               choiceNames = paste0(x_noncui,": ",x_noncui.name),
                               selected = x_noncui
      )
    }
  }
}

## Update checkboxinput if a new node is added by clicking======================
checkboxUpdateByAddButton <- function(s1, s2, node_now,
                                      edge_matrix_full, dict.combine,
                                      session){
  s = c(s1, s2)
  if(length(s)!=0 & length(node_now)!=0){
    edge.ma.now = edge_matrix_full
    loc.node_now = match(node_now, rownames(edge.ma.now))
    if(!(node_now %in% s)){
      node_now_name = rownames(edge.ma.now)[loc.node_now]
      if(length(node_now_name)>0 & !is.na(node_now_name)){
        if(dict.combine$type[match(node_now_name,dict.combine$Variable)]=="CUI"){
          x_cui = cbind(node_now_name, s1)
          x_cui.name = dict.combine$Description[match(x_cui,dict.combine$Variable)]
          updateCheckboxGroupInput(session, "inCheckboxGroup1",
                                   label = paste(length(x_cui), "Selected cui(s):"),
                                   choiceValues = x_cui,
                                   choiceNames = x_cui.name,
                                   selected = x_cui
          )
        }
        
        if(dict.combine$type[match(node_now_name,dict.combine$Variable)]=="nonCUI"){
          x_noncui = cbind(node_now_name,s2)
          x_noncui.name = dict.combine$Description[match(x_noncui,dict.combine$Variable)]
          
          updateCheckboxGroupInput(session, "inCheckboxGroup2",
                                   label = paste(length(x_noncui), "Selected noncuicode(s):"),
                                   choiceValues = x_noncui,
                                   choiceNames = x_noncui.name,
                                   selected = x_noncui
          )
        }
      }
    }
  }
  
}
