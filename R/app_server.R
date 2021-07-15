#' @import shiny
#' @import shinyBS
#' @import Matrix
NULL

app_server_addpath <- function(data_path){
  server <- function( input, output, session ) {
    data = readRDS(data_path)
    edge_matrix_full = data$edge_matrix_full
    dict.combine = data$dict.combine
    input_table = data$input_table
    color.df = data$color.df
    rm(data)
    # Your application server logic 
    s1 = reactive({input$inCheckboxGroup1})
    s2 = reactive({input$inCheckboxGroup2})
    f1 = reactive({input$filter_noncuiGroup})
    f2 = reactive({input$filter_cuiGroup})
    thr_cos = reactive({input$cos[1]})
    thr_cos_pop = reactive({input$cutoff_ind[1]})
    is_filter = reactive({input$is_filter})
    cluster = reactive({input$cluster})
    node_num_cutoff = 500
    
    ## bsModal Event =============================================================
    observeEvent(input$instruct, {
      shinyBS::toggleModal(session, "instruction", toggle = "open")
    })
    
    observeEvent(input$current_node_id$nodes, {
      shinyBS::toggleModal(session, "selectednode", toggle = "open")
    })
    
    ## Check if too many or no nodes are plotted =================================
    observeEvent(input$goButton, {
      checkAbnormalNodes(s1(), s2(), f1(), f2(), 
                         thr_cos(), node_num_cutoff,
                         edge_matrix_full, dict.combine, session)
    })
    
    ## Clicked node text
    output$clicked_node_info <- renderUI({
      node_now = input$current_node_id$nodes[[1]]
      clickedNodeText(s1(), s2(), node_now, 
                      edge_matrix_full, dict.combine)
    })
    
    
    # Table section ===============================================================
    ## Generate input table ===================================================
    output$table <- DT::renderDataTable(DT::datatable({
      input_table
    }, rownames = FALSE,filter = 'top',
    options = list(
      paging = TRUE,
      pageLength = 7,
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = TRUE)),  server = TRUE)
    
    proxy = DT::dataTableProxy('table')
    
    observe({
      input$refresh
      DT::reloadData(
        proxy,
        resetPaging = TRUE,
        clearSelection = c("all"))
    })
    
    
    ## Generate summary table ===================================================
    output$instruct_table <- renderTable({
      summaryTable(s1(), s2(), f1(), f2(), thr_cos(),
                   edge_matrix_full, dict.combine)
    })
    
    ## Generate clicked node info table
    output$shiny_return <-
      DT::renderDataTable(DT::datatable({
        
        node_now = input$current_node_id$nodes[[1]]
        clickedTable(node_now, edge_matrix_full, dict.combine)
        
      }, rownames = FALSE, list(
        paging = FALSE,
        scrollY = "450px",
        scrollCollapse = TRUE)))
    
    
    ## Generate downloading table================================================
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("network_info.csv")
      },
      content = function(path) {
        file = download_table(s1(), s2(), f1(), f2(), thr_cos(),
                              edge_matrix_full, dict.combine, color.df)
        utils::write.csv(file,path,row.names = FALSE)
      }
    )
    
    
    
    
    # Plot section ===============================================================
    ## Generate network plot using visnetwork ====================================
    output$network <- renderUI({
      shinycssloaders::withSpinner(
        visNetwork::visNetworkOutput("network_proxy_nodes",
                                     height =  paste0(shinybrowser::get_height(),"px")), 
        type = 6
      )
    })
    output$network_proxy_nodes <-  visNetwork::renderVisNetwork({
      input$goButton
      s1 = isolate(input$inCheckboxGroup1)
      s2 = isolate(input$inCheckboxGroup2)
      f1 = isolate(input$filter_noncuiGroup)
      f2 = isolate(input$filter_cuiGroup)
      thr_cos = isolate(input$cos[1])
      myconfirmation = input$myconfirmation
      slider_text = input$slider_text
      slider_size = input$slider_size
      networkVIS(s1, s2, f1, f2, thr_cos, node_num_cutoff,
                 myconfirmation, slider_text, slider_size, cluster(),
                 edge_matrix_full, dict.combine, color.df)
    })
    
    ## Generate sunburst plot using plotly =======================================
    output$sun_ui <- renderUI({
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("sun",width="auto",
                             height=paste0(input$scale_sungh,"px")), type = 6
      )
      
    })
    output$sun <- plotly::renderPlotly({
      node_now = input$current_node_id$nodes[[1]]
      changeline = input$changeline
      rotatelabel = input$rotatelabel
      scale_sungh = input$scale_sungh
      sunburstPlotly(f1(), f2(), thr_cos_pop(), is_filter(),
                     changeline, rotatelabel, scale_sungh,
                     node_now, edge_matrix_full, dict.combine)
    })
    
    ## Generate circular plot using ggplot =======================================
    output$circularplot <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput("circular", width = "100%",
                   height = paste0(shinybrowser::get_height(),"px")), type = 6
      )
    })
    output$circular <- renderPlot({
      node_now = input$current_node_id$nodes[[1]]
      circularStatic(f1(), f2(), thr_cos_pop(), is_filter(),
                     node_now, edge_matrix_full, dict.combine,
                     color.df)
    })
    
    ## Generate interactive circular plot using highcharter ======================
    output$circularplot_highcharter <- renderUI({
      shinycssloaders::withSpinner(
        highcharter::highchartOutput("circular_highcharter", 
                                     width = "100%",
                                     height = paste0(input$scale_chg,"px")), type = 6
      )
      
    })
    output$circular_highcharter <- highcharter::renderHighchart({
      scale_chy = input$scale_chy
      node_now = input$current_node_id$nodes[[1]]
      circularInteractive(f1(), f2(), thr_cos_pop(), is_filter(), 
                          scale_chy, node_now, edge_matrix_full, 
                          dict.combine,color.df)
    })
    
    
    
    
    # Update section ===============================================================
    ## Update checkboxinput if refreshing===========================================
    observe({
      input$refresh
      x <- character(0)
      updateCheckboxGroupInput(session, "inCheckboxGroup1",
                               "Selected CUI(s):",
                               choices = x,
                               selected = x)
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               "Selected Codify node(s):",
                               choices = x,
                               selected = x)
      
    })
    
    ## Update checkboxinput based on selected rows in table=========================
    observe({
      s = input$table_rows_selected
      checkboxUpdateBySelectedRows(s, input_table, session)
    })
    
    ## Update checkboxinput if a new node is added by clicking======================
    observe({
      input$addButton
      isolate({
        s1 = input$inCheckboxGroup1
        s2 = input$inCheckboxGroup2
        s = c(s1, s2)
        node_now = input$current_node_id$nodes[[1]]
      })
      checkboxUpdateByAddButton(s1, s2, node_now,
                                edge_matrix_full, dict.combine,
                                session)
    })
    
    ## Update focused center node(s)================================================
    observe({
      s = input$table_rows_selected
      nodename = input_table$id[s]
      if(length(s)!=0){
        x = dict.combine$Description_s[match(nodename,
                                             dict.combine$Variable)]
        x = c(NA,x)
        updateSelectInput(session, "Focus","Choose one node to focus on:",
                          choices = x, selected = NA)
        
      }
    })
    
    observe({
      if(!is.na(input$Focus)){
        id = dict.combine$Variable[match(input$Focus,dict.combine$Description_s)]
        visNetwork::visNetworkProxy("network_proxy_nodes") %>%
          visNetwork::visFocus(id = id, scale = input$scale_id/10)
      }
    })
    
    
  }
  return(server)
}