
#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CUInetwork'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


app_ui_addpath <- function(data_path){
  ui <- function(request) {
    data = readRDS(data_path)
    dict.combine = data$dict.combine
    rm(data)
    tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # Your application UI logic 
      shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "NLP & Codified Network",
                                        disable = FALSE,
                                        
                                        shinydashboard::dropdownMenu(type = "notifications",
                                                                     shinydashboard::notificationItem(
                                                                       text = HTML("<b >Try to use filtering so the total
                                 <p > connected nodes are no more than 500. </p >
                                 <p > Otherwise the displaying speed is slow. </p> </b>"),
                                                                       icon = icon("exclamation-triangle"),
                                                                       status = "warning"
                                                                     )
                                        )
                                        
        ),
        
        ## Sidebar content
        shinydashboard::dashboardSidebar(
          width = "200pt",
          disable = FALSE,
          shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Network", tabName = "Network", icon = icon("dashboard")),
            
            checkboxGroupInput("inCheckboxGroup1", "Selected CUI(s):"),
            checkboxGroupInput("inCheckboxGroup2", "Selected Codify node(s):"),
            fluidRow(column(1),
                     column(3,actionButton("goButton", "Show", 
                                           icon = tags$i(class = "far fa-play-circle",
                                                         style="font-size: 10px"), 
                                           class = "btn-success")),
                     column(3,actionButton('refresh', 'Unselect', 
                                           icon = tags$i(class = "fa fa-refresh", 
                                                         style="font-size: 10px")))),
            
            
            shinyWidgets::materialSwitch(
              inputId = "filter_Switch",
              label = "More options?", 
              value = TRUE,
              status = "success"
            ),
            conditionalPanel(condition =  "input.filter_Switch==1",
                             shinyWidgets::sliderTextInput(
                               inputId = "cos", 
                               label = "Filter edges by cosine similarity (above):",
                               from_min = 0.1, selected = c(0.25,1), to_fixed = 1,
                               choices = seq(0.1,1,by=0.01),
                               grid = TRUE
                             ),
                             shinyWidgets::pickerInput(
                               inputId = "filter_cuiGroup",
                               label = "Filter CUI by semantic type(s):", 
                               choices = unique(dict.combine$Capinfo2[dict.combine$Cap=="U"]),
                               selected = unique(dict.combine$Capinfo2[dict.combine$Cap=="U"]),
                               multiple = TRUE,
                               options = shinyWidgets::pickerOptions(
                                 actionsBox = TRUE,
                                 noneSelectedText = "Do not show CUIs.",
                                 size = 5,
                                 liveSearch = TRUE)
                             ),
                             shinyWidgets::pickerInput(
                               inputId = "filter_noncuiGroup",
                               label = "Filter codified nodes by node type(s):", 
                               choices = unique(dict.combine$Capinfo2[dict.combine$Cap!="U"]),
                               selected = unique(dict.combine$Capinfo2[dict.combine$Cap!="U"]),
                               multiple = TRUE,
                               options = shinyWidgets::pickerOptions(
                                 actionsBox = TRUE,
                                 noneSelectedText = "Do not show codifed nodes.",
                                 size = 5,
                                 liveSearch = TRUE)
                             ),
                             shinyWidgets::pickerInput(
                               inputId = "cluster",
                               label = "Cluster by:", 
                               choices = c("None", "CUIs", "CodifiedNodes", "Both")
                             )
            ),
            shinyWidgets::materialSwitch(
              inputId = "inst_table_bin",
              label = "Summary table?", 
              value = TRUE,
              status = "success"
            ),
            conditionalPanel(condition =  "input.inst_table_bin==1",
                             fluidRow(column(12,align="center",
                                             h4("Connected Nodes"),
                                             tableOutput("instruct_table"))))
          )
        ),
        
        shinydashboard::dashboardBody(
          tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }

                                '))),
          
          
          #shinyDashboardThemes(theme = "onenote"),
          shinyWidgets::useSweetAlert(),
          shinybrowser::detect(),
          shinyBS::bsModal(
            id = "selectednode", title = "Clicked node info:", trigger = FALSE,
            size = "large",
            fluidRow(
              column(6,
                     htmlOutput("clicked_node_info"),
                     actionButton("addButton", "Add to candidates 
                            (if in the interested list)", 
                                  icon = icon("plus-square"),
                                  class="btn-primary active"),
              ),
              column(6,
                     shinyWidgets::sliderTextInput(
                       inputId = "cutoff_ind", 
                       label = "Filter edges by cosine similarity (above):",
                       from_min = 0.1, selected = c(0.5,1), to_fixed = 1,
                       choices = seq(0.1,1,by=0.05),
                       grid = TRUE
                     ),
                     shinyWidgets::switchInput(
                       inputId = "is_filter",
                       value = TRUE,
                       onLabel = "Filtered",
                       offLabel = "Show all"
                     )
              )
            ),
            br(),
            hr(),
            tabsetPanel(id = "select",
                        tabPanel(title = "Sunburst plot",
                                 br(),
                                 fluidRow(column(6,
                                                 shinyWidgets::sliderTextInput("changeline","max Text length on each line (set as 99 if not breaking lines:)",
                                                                               choices = c(5,10,15,20,25,99),selected = 10,grid=TRUE,width = "100%"),
                                                 shinyWidgets::pickerInput(
                                                   inputId = "rotatelabel",
                                                   label = "The orientation of text inside sectors",
                                                   choices = c("Radial", "Tangential")
                                                 )
                                 ),
                                 column(6,sliderInput("scale_sungh","Graph height:", 
                                                      min=500,max=1000,value=750,width = "100%"))
                                 ),
                                 
                                 div(uiOutput("sun_ui"),align="center")
                        ),
                        tabPanel(title = "Circular plot",
                                 br(),
                                 uiOutput("circularplot")
                        ),
                        tabPanel(title = "Interactive circular plot",
                                 br(),
                                 fluidRow(column(6, sliderInput("scale_chy","Max y:", min=0.2,max=1,value=1,step=0.2,width = "100%"),
                                 ),
                                 column(6, sliderInput("scale_chg","Graph height:", min=500,max=1000,value=750,width = "100%"),
                                 )),
                                 
                                 uiOutput("circularplot_highcharter")
                        ),
                        tabPanel(title = "Table of connected nodes",
                                 DT::dataTableOutput("shiny_return")
                        )
            )
          ),
          shinyBS::bsModal(
            id = "instruction", title = "Instruction", trigger = FALSE,
            size = "small",
            p('1. Please refer to "Possible input" box to see appropriate values.'),
            p('2. By filtering, specify the threshold(s) for cosine similarity, or/and restrict nodes
                                 by selecting specific groups(types for codified nodes or semantic types for CUIs.
                                 You can select more than one groups. '),
            p('3. If you select one option in "Cluster by", double click any group node (ellipse) to unfold or
                                  any expanded individual node (square) to fold the group it belongs to. 
                                  Click "Reinitialize clustering" button on the right corner to fold all groups.'),
            p('4. Click any individual node (ellipse) to see its neighbors.')
            
          ),
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "Network",
                                    fluidRow(
                                      column(1,
                                             shinyWidgets::dropdown(
                                               h4("Possible inputs"),
                                               DT::dataTableOutput("table"),
                                               style = "unite", icon = icon("table"),
                                               status = "primary", width = "450px",
                                               animate = shinyWidgets::animateOptions(
                                                 enter = "fadeInDown",
                                                 exit = "fadeOutUp",
                                                 duration = 0
                                               )
                                             )
                                      ),
                                      column(1,       
                                             shinyWidgets::dropdown(
                                               selectInput("Focus", label = "Focus on node:",
                                                           choices = NA,width = "100%"),
                                               sliderInput("scale_id","Focus scale:", min=1,max=10,value=5,width = "100%"),
                                               sliderInput("slider_text", "(non center) Font size times",
                                                           min = 0.1, max = 5, value = 1,step=0.1,width = "100%"),
                                               sliderInput("slider_size", "(non center) Node size times",
                                                           min = 0.1, max = 5, value = 1,step=0.1,width = "100%"),
                                               
                                               style = "unite", icon = icon("eye"),right = FALSE,
                                               status = "primary", width = "400px",
                                               animate = shinyWidgets::animateOptions(
                                                 enter = "fadeInDown",
                                                 exit = "fadeOutUp",
                                                 duration = 0
                                               ))
                                      ),
                                      
                                      column(8),
                                      column(1,shinyWidgets::downloadBttn(outputId = "downloadData", 
                                                                          label = "Download nodes as .csv",
                                                                          style = "material-circle",
                                                                          color = "default")),
                                      column(1,shinyWidgets::actionBttn("instruct", "Help", style = "material-circle",
                                                                        icon = icon("question")))
                                    ), 
                                    fluidRow(
                                      column(width = 12,
                                             
                                             uiOutput("network")
                                      )
                                      
                                    )
                                    
            )
          )
        )
      )
    )
  }
  return(ui)
}