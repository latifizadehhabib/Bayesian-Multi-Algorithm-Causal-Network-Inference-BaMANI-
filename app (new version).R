rm(list = ls())        
library(bnlearn)
library(shiny)
library(shinydashboard)
library(DT)
library(visNetwork)  
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(purrr)
library(shinymanager)
library(keyring)
library(igraph)
library(shinyalert) 
library(shinyjs)

#-----------------------------------
styled_title <- function(text, 
                         bgcolor = "#007BFF", 
                         color = "white", 
                         padding = "6px 6px", 
                         border_radius = "10px", 
                         font_size = "14px", 
                         box_shadow = "2px 2px 10px #888888") {
  
  HTML(paste0('<div style="background-color: ', bgcolor, 
              '; color: ', color, 
              '; padding: ', padding, 
              '; border-radius: ', border_radius, 
              '; font-size: ', font_size,
              '; font-weight: bold; 
                 text-align: center;
                 box-shadow: ', box_shadow, ';">', text, '</div>'))
}
# -----------
load_required_packages <- function(packages) {
  # Load each package in list
  for (package in packages) {
    library(package, character.only = TRUE)
  }
}
# -----------
# load_required_packages <- function(packages) {
#   for (package in packages) {
#     if (!require(package, character.only = TRUE)) {
#       install.packages(package)
#       library(package, character.only = TRUE)
#     }
#   }
# }
# -----------
load_source_files <- function(source_files) {
  for (source_file in source_files) {
    source(source_file)
  }
}
# list of packages and source files
packages <- c("purrr", "parallel", "DT","shinydashboard", "shiny","colorspace", "stats", "bnlearn", "lattice", 
              "Rgraphviz", "MASS", "ggpubr", "snow", "grid", 
              "tidyverse", "plotly", "ggplot2", "reshape2", "metR", "fields", "scatterplot3d", "matrixStats", "rgl", 
              "readr", "igraph", "dplyr", "cowplot", "knitr", "visNetwork", "scales", "RColorBrewer") #, "rlang"

source_files <- c("data_process_Correlation.v2.R", "run_algorithm_directed.R", "run_algorithm_Undirected.R", "augmented_edge_table.R",
                  "uninformative_arcs_removal.R", "finding_threshold_values.R", 
                  
                  "temp.white_thresh.cols.R", 
                  "calculate_loss_npar_table.R", 
                  "calculate_bic.R", "find_min_bic.Parent_whitelist_acyclic.v3.R",
                  "Final.DAG_network_plot_v6.R",
                  "DAG_network_plot.arc.lable.R", 
                  "Diagnostic_plot.v3.R",
                  "diagnostic_plot_White_Final.R", 
                  "plot_Algorithm.Count_arcs.strength.R"
)#, "renderStyledTable.R"

# Check and install missing packages and load source files
load_required_packages(packages)
load_source_files(source_files)
# -----------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = ""),
  
  dashboardSidebar(
    useShinyjs(),  # Initialize shinyjs
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Settings", tabName = "settings", icon = icon("gear")),
      menuItem("User Guide", tabName = "user_guide", icon = icon("file-pdf")),
      
      menuItem("Data", icon = icon("database"),
               menuSubItem("Data characteristics", tabName = "data_characteristics"),
               menuSubItem("BlackList", tabName = "Black_List")
      ),
      
      menuItem("Algorithms", icon = icon("cogs"),
               menuSubItem("Learning Process", tabName = "directed_Undirected_algorithm_messages")
      ),
      
      menuItem("Edges", icon = icon("project-diagram"),
               menuSubItem("Augmented Edge List", tabName = "augmented_edge_list"),
               menuSubItem("Augmented Threshold Columns", tabName = "augmented_thresh_cols"),
               menuSubItem("Possible Seed Arcs Filter", tabName = "possible_seed_arcs_filter")
      ),
      
      menuItem("Diagnostics", icon = icon("vial"),
               menuSubItem("L1 Merged", tabName = "L1_merged"),
               menuSubItem("Npar Merged", tabName = "npar_merged"),
               menuSubItem("BIC Merged Table", tabName = "BIC_merged_table"),
               menuSubItem("BIC Min Table", tabName = "bic_min_table"),
               menuSubItem("WhiteList/ Check acyclicity", tabName = "WhiteList_Check_acyclicity")
      ),
      
      menuItem("Plots", icon = icon("chart-line"),
               menuSubItem("Diagnostic Plot", tabName = "diagnostic_plot"),
               menuSubItem("Algorithm Count & Arcs Strength", tabName = "algorithm_count_arcs_strength"),
               menuSubItem("DAG Network Plot", tabName = "DAG_network_plot"),
               menuSubItem("DAG Network flow", tabName = "DAG_Plot"),
               menuSubItem("Comparative Analysis", tabName = "contour_plot")
      )       
    )
  ),
  # -----------
  dashboardBody(
    # -----------
    useShinyjs(),
    
    tags$style(type = "text/css", 
               ".box { 
                  border: 1px solid #ccc; 
                  box-shadow: 2px 2px 8px #aaa;
                }
                .info-icon {
                  font-size: 16px;
                  color: blue;
                  cursor: pointer;
                }"),
    # -----------
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # -----------
    tags$head(tags$style(HTML("
    .sweet-alert.alert-size-s {
      width: 600px !important; /* Increase width as needed */
    }
    .swal-button--confirm {
      background-color: #3498db;
    }
  "))),
    # -----------
  #   tags$head(tags$script(HTML("
  # $(document).on('click', '#goToTab', function() {
  #   $('a[data-value=\"WhiteList_Check_acyclicity\"]').click();
  #   });
  #                              "))),

  tags$script(HTML("
$(document).on('click', '#goToTab', function() {
  $('a[data-value=\"WhiteList_Check_acyclicity\"]').click();
  // Close the shinyalert dialog
 swal.close();
 });
")),

    # -----------

    # Add Font Awesome link here
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://use.fontawesome.com/releases/v5.15.1/css/all.css")
    ),
    # -----------
    # Initialize Bootstrap Tooltip
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip();
      });
    ")),
    # -----------
    # tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css")),
    # -----------
    tabItems(
      tabItem(tabName = "user_guide",
              #includeHTML("www/User_Guide.html")  # https://xodo.com/convert-pdf-to-html
              tags$iframe(style="height:600px; width:100%", src="User_Guide.pdf")
      ),
      tabItem(tabName = "settings",
              fluidRow(
                # ---------------------
                # UI
                box(
                  width = 4,
                  title = styled_title("Settings", bgcolor = "#4CAF50"), # Apply styled_title
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  # ----------------
                  fluidRow(
                    column(
                      5, # Adjusted column width for better alignment
                      numericInput(inputId = "nboot",
                                   label = "Nboot",
                                   value = 10, min = 10, max = 1000000000000)
                    ),
                    column(
                      1,
                      div(
                        HTML("<i id='info_nboot' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                        tags$script(
                          HTML("
          $(document).on('click', '#info_nboot', function(e) {
            e.stopPropagation();
            Shiny.setInputValue('show_nboot', Math.random());
          });
          ")
                        )
                      )
                    )
                  ),
                  # ----------------
                  fluidRow(
                    column(
                      5, # Adjusted column width for better alignment
                      numericInput(inputId = "threshold_level",
                                   label = "Threshold Level",
                                   value = 5)
                    ),
                    column(
                      1,
                      div(
                        HTML("<i id='info_threshold_level' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                        tags$script(
                          HTML("
          $(document).on('click', '#info_threshold_level', function(e) {
            e.stopPropagation();
            Shiny.setInputValue('show_threshold_level', Math.random());
          });
          ")
                        )
                      )
                    )
                  )
                ),
                # ----------------
                box(
                  width = 4,
                  title = styled_title("File Inputs", bgcolor = "#4CAF50"),
                  # style = "border: 1px solid #ccc; box-shadow: 2px 2px 8px #aaa;",  # Styling Box
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  # ----------------
                  fluidRow(
                    column(
                      9, # Adjusted column width for better alignment
                     uiOutput("dynamicFileInput")
                    ),
                    column(
                      1,
                      div(
                        HTML("<i id='info_dataFile' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                        tags$script(
                          HTML("
          $(document).on('click', '#info_dataFile', function(e) {
            e.stopPropagation(); // Stop the event from propagating up to the fileInput
            Shiny.setInputValue('show_dataFile', Math.random());
          });
          ")
                        )
                      )
                    )
                  ),   
                  # ----------------
                  fluidRow(
                    column(
                      9, # Adjusted column width for better alignment
                      fileInput(
                        inputId = "BlackListFile",
                        label = "BlackList",
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                      )
                    ),
                    column(
                      1,
                      div(
                        HTML("<i id='info_BlackListFile' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                        tags$script(
                          HTML("
          $(document).on('click', '#info_BlackListFile', function(e) {
            e.stopPropagation(); // Stop the event from propagating up to the fileInput
            Shiny.setInputValue('show_BlackListFile', Math.random());
          });
          ")
                        )
                      )
                    )
                  ),
                  # ----------------
                  fluidRow(
                    column(
                      9, # Adjusted column width for better alignment
                      fileInput(
                        inputId = "WhiteListFile",
                        label = "WhiteList",
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                      )
                    ),
                    column(
                      1,
                      div(
                        HTML("<i id='info_WhiteListFile' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                        tags$script(
                          HTML("
          $(document).on('click', '#info_WhiteListFile', function(e) {
            e.stopPropagation(); // Stop the event from propagating up to the fileInput
            Shiny.setInputValue('show_WhiteListFile', Math.random());
          });
          ")
                        )
                      )
                    )
                  )
                ),
                # -----------------------------
                box(
                  width = 4,
                  title = styled_title("Algorithm selection", bgcolor = "#4CAF50"), 
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible = TRUE,
                  # -----------------------------
                  fluidRow(
                    column(
                      5,  # Adjusted column width for better alignment
                      selectInput(
                        inputId = "algorithm_directed", 
                        label = "Directed Algorithms", 
                        choices = c("iamb", "iamb.fdr", "pc.stable", "hc", "tabu", "mmhc", "rsmax2", "gs"), 
                        selected = NULL, 
                        multiple = TRUE
                      )
                    ),
                    column(
                      1,
                      div(HTML("<i id='info_algorithm_directed' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>")),
                      tags$script(
                        HTML("
        $(document).on('click', '#info_algorithm_directed', function(e) {
        e.stopPropagation(); // Stop the event from propagating up to the selectInput
        Shiny.setInputValue('show_Dir_AlgoDescriptions', Math.random());
        });
        ")
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      5,  # Adjusted column width for better alignment
                      selectInput(         
                        inputId = "algorithm_undirected",
                        label = "Undirected Algorithms",
                        choices = c("mmpc", "si.hiton.pc"), selected = NULL, multiple = TRUE
                      )
                    ),
                    column(
                      1,
                      div(HTML("<i id='info_algorithm_undirected' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>")),
                      tags$script(
                        HTML("
        $(document).on('click', '#info_algorithm_undirected', function(e) {
        e.stopPropagation(); // Stop the event from propagating up to the selectInput
        Shiny.setInputValue('show_UnDir_AlgoDescriptions', Math.random());
        });
        ")
                      )
                    )
                  )
                )
              ),
              br(),
              # -----------------------------
              fluidRow(
                div(
                  style = "text-align: center;", 
                  actionButton(inputId = "runButton", 
                               label = tags$span(icon("fas fa-hand-pointer", style="margin-right: 4px;"), "Run Discovery"),
                               style = "background-color: #3498db; color: white; padding: 10px 20px; font-size: 18px; border-radius: 5px; cursor: pointer;")
                )
              )
      ),
      # ------------------------------------------------------------------------              
      
      tabItem(tabName = "data_characteristics", 
              h3("Data Characteristics"), # PANLE DESCRIPTION
              tags$p("Descriptin: Data Characteristics", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),
              # ------------
              tabsetPanel(id = "tabset14",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Data Characteristics",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 900px; height: 700px; overflow: auto;",
                                    verbatimTextOutput("viewDataStructure")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "Black_List", 
              h3("BlackList"), 
              tags$p(
                HTML("The <code>BlackList</code> plays a pivotal role in the <code>structural learning</code> and network inference processes within Bayesian networks, which are illustrated as Directed Acyclic Graphs (DAGs). This component is instrumental in molding the network structure, relying primarily on <code>prior knowledge </code> regarding the causal relationships amongst the data features. This list encompasses specific arcs, or <code>directed edges </code>, that are deliberately excluded from the proposed network, serving as constraints based on established or acknowledged information. This ensures the resultant structure and its causal interactions or arcs amongst nodes are in harmony with existing knowledge and logical constraints, thereby maintaining the integrity and accuracy of the network's representation of causal relationships."), 
                style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset13",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "BlackList",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    DTOutput("Black_List")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "directed_Undirected_algorithm_messages",
              h3("Learning process of directed & Undirected algorithms:"), 
              tags$p("The message printed below displays the progression of the learning process as executed by various structural learning algorithms, encompassing both directed and undirected algorithms.", 
                     br(),
                     HTML("The <code>Directed Algorithm</code> in <code>Bayesian network inference</code> is crucial for deducing the structure and causality in a network, represented as a Directed Acyclic Graph (<code>DAG</code>). Users employ an ensemble of different structural learning algorithms to accurately identify potential causal interactions or arcs among nodes and to estimate the <code>conditional probability distributions</code> from datasets. These algorithms contribute to constructing <code>blacklists</code> and/ or <code>whitelists</code> of arcs, serving as a basis for exclusion and inclusion based on <code>prior knowledge</code> and <code>arc strength</code>."), 
                     HTML("Some algorithms have been utilized for the local discovery of undirected graphs to furnish additional evidence substantiating the existence of an edge between two nodes, in conjunction with the <code>Directed Algorithms</code> ."), 
                     br(),
                     HTML("The table displayed on the righ panel enumerates the arcs learnt through using various structural learning algorithms. The column labeled <code>Edge_No</code> corresponds to the number of the arcs as listed in the table."),
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),
              
              tabBox(
                width = 12,
                id = "tabset1",
                tabPanel(
                  title = "Algorithm Progression",
                  fluidRow(
                    column(
                      7,
                      box(
                        title = "Algorithm Progression",
                        status = "primary",
                        solidHeader = TRUE,
                        div(
                          style = "width: 900px; height: 600px; overflow: auto;",
                          verbatimTextOutput("directed_Undirected_algorithm_messages")
                        ),
                        width = 12,
                        collapsible = TRUE
                      )
                    )
                  )
                ),
                tabPanel(
                  title = "List of Arc",
                  fluidRow(
                    column(
                      5,
                      box(
                        title = "Arcs Leant from Algorithms",
                        status = "info",
                        solidHeader = TRUE,
                        div(
                          style = "width: 600px; height: 600px; overflow: auto;",
                          DTOutput("all_edge_list")
                        ),
                        width = 12,
                        collapsible = TRUE
                      )
                    )
                  )
                )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "augmented_edge_list",
              h3("augmented_edge_list"), 
              tags$p("Descriptin: augmented_edge_list", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset14",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Arcs and thier corresponding info",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 600px; overflow: auto;",
                                    DTOutput("augmented_edge_list")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "possible_seed_arcs_filter",
              h3("possible seed arcs"), 
              tags$p("possible seed arcs filtered by the corresponding arc strength threshold", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),
              
              tabsetPanel(id = "tabset12",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Filtered Arcs by arc strength threshold",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 600px; overflow: auto;",
                                    DTOutput("possible_seed_arcs_filter")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------            
      tabItem(tabName = "augmented_thresh_cols",
              h3("Arcs and thier corresponding info in different algorithm"), 
              tags$p("List of Arcs and thier corresponding arcs strength and thier inhibitive or promotive impact/ correlation sign in different algorithm", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset11",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Arcs and thier corresponding info",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 600px; overflow: auto;",
                                    DTOutput("augmneted.thresh.cols")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "L1_merged", 
              h3("Loss value for each node in final DAG / threshold "), 
              tags$p("Descriptin: Loss value for each node in final DAG / threshold ", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset10",
                          tabPanel(
                            # title = "Loss value for each node / threshold  ",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Loss value for each node / threshold  ",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 600px; overflow: auto;",
                                    DTOutput("L1_merged")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "npar_merged", 
              h3("Number of parents of each node in final DAG / threshold "), 
              tags$p("Descriptin: Number of parents of each node in final DAG / threshold", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),
              # ------------
              tabsetPanel(id = "tabset9",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Number of parents of each node/ threshold",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 600px; overflow: auto;",
                                    DTOutput("npar_merged")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      
      tabItem(tabName = "BIC_merged_table",
              h3("Bayesian Information Criterion (BIC) score for each node/ threshold "), 
              tags$p("Descriptin: Bayesian Information Criterion (BIC) score for each node/ threshold", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset8",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "BIC score/ threshold",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 600px; overflow: auto;",
                                    DTOutput("BIC_merged_table")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "WhiteList_Check_acyclicity", 
              h3("WhiteList_Check_acyclicity"), 
              tags$p("Description: WhiteList_Check_acyclicity", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset81",
                          # First TabPanel
                          tabPanel(
                            title = "WhiteList Check acyclicity",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "WhiteList Check acyclicity",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  fluidRow(
                                    column(3, 

                                           tags$div(
                                             uiOutput('cycleMessage'),
                                             style = "
                                       width: 200px;  /* This line sets the width */
                                       background: linear-gradient(to right, #3498db, #2980b9);
                                       box-shadow: 0 1px 1px 0 rgba(0, 0, 0, 0.2);
                                       color: white;
                                       padding: 1px 1px;
                                       font-size: 15px;
                                       border-radius: 4px;
                                       transition: all 0.3s ease-in-out;
                                       font-family: 'Arial', sans-serif;
                                       border: 1px solid #3498db;  /* This line adds a thicker border */
                                       &:hover {
                                       background: linear-gradient(to right, #2980b9, #3498db);
                                       }  "
                                           ),

                                           uiOutput("checkboxUI"),

                                           fluidRow(
                                             uiOutput("removeButtonUI")
                                           )

                                    ),
                                    column(5, 
                                           # Container for the visNetwork plot and the title
                                           tags$div(style = "position: relative;", 
                                                    # Add padding to the container to push the plot down
                                                    tags$div(visNetworkOutput("initialPlot"), style = "padding-top: 50px;"),
                                                    # Title overlaid at the top of the visNetwork plot, now bold
                                                    tags$div("Graph of possible whitelist", 
                                                             br(),
                                                             style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); background-color: rgba(255,255,255,0.7); padding: 5px; border-radius: 5px; font-weight: bold;")
                                           )
                                    ),
                                    column(4, 
                                           tags$div(style = "position: relative;", 
                                                    # Add padding to the container to push the plot down
                                                    tags$div(visNetworkOutput("plot"), style = "padding-top: 50px;"),
                                                    # Title overlaid at the top of the visNetwork plot, now bold
                                                    tags$div("Final Whitelist after Cycle check", 
                                                             br(),
                                                             style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); background-color: rgba(255,255,255,0.7); padding: 5px; border-radius: 5px; font-weight: bold;")
                                           )
                                    )
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          ),
                          # Second TabPanel
                          tabPanel(
                            title = "Final Whitelist", 
                            fluidRow(
                              column(
                                10,
                                box(
                                  title = "Final Whitelist",
                                  status = "info",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 600px; height: 500px; overflow: auto;",
                                    DTOutput("final_white_list")
                                  ),
                                  width = 6,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "bic_min_table", 
              h3("Min BIC score for a node with min number of parents in corresponding final DAG / threshold "), 
              tags$p("Descriptin: Min BIC score for a node with min number of parents in corresponding final DAG / threshold ", 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset20",
                          tabPanel(
                            title = "Min BIC table",
                            fluidRow(
                              column(
                                box(
                                  title = "Min BIC table",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 700px; height: 600px; overflow: auto;",
                                    DTOutput("bic_min_table")
                                  )
                                ),
                                width = 10,
                                collapsible = TRUE
                              )
                            )
                          ),
                          tabPanel(
                            title = "Possible WhiteList",
                            fluidRow(
                              column(                               
                                box(
                                  title = "Possible WhiteList",
                                  status = "info",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 700px; height: 400px; overflow: auto;",
                                    DTOutput("possible.white.list")
                                    
                                  )
                                ),
                                width = 10,
                                collapsible = TRUE
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------     
      tabItem(tabName = "diagnostic_plot", 
              h3("Diagnostic plot"), 
              tags$p(HTML("The plots below illustrate how the overall network connectivity (top) is influenced by
                          the number of arcs included in the whitelist. A threshold value for edge strength (<code>x-axis</code> )
                          is utilized to select arcs for inclusion in the whitelist (indicated by the <span style='color: blue;'><strong>blue</strong></span> curve), 
                          thereby determining the inferred DAG connectivity (represented by <span style='color: brown;'><strong>brown</strong></span> circles).
                          Additionally, values for the Bayesian Information Criterion (<code>BIC</code>) were calculated for the entire <code>DAG</code> 
                          (bottom - <span style='color: green;'><strong>green</strong></span> circles) and for each node, given the inferred parents, as depicted by the <code>min BIC table</code>.
                          "), 
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset6",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Diagnostic plot",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 400px; overflow: auto;",
                                    plotOutput("Diagnostic_plot")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------            
      tabItem(tabName = "contour_plot", 
              h3(HTML("Comparative Analysis of <span style='color: red;'>Pathological</span> and <span style='color: blue;'>Healthy</span> Data")),
              tags$p(
                HTML("This visualization facilitates exploration of the <code>conditional probability queries</code> of 
                Directed Acyclic Graphs (<code>DAG</code>) in relation to empirical data from both healthy and pathological
                conditions. Users can navigate through the dashboard, opting for a feature from an extensive list; 
                the application then generates a visual portrayal of the chosen feature (illustrated on the <code>y-axis</code>)
                against a normalized significant feature (<code>x-axis</code>). In the resultant plot, samples derived from healthy
                     tissues are denoted by <span style='color: blue;'><strong>blue dots</strong></span>, whereas those from pathological
                     tissues are designated by <span style='color: red;'><strong>red  dots</strong></span>. Given that the pathological
                     feature is a binary variable, the dashboard delineates different features with <span style='color: blue;'><strong>blue contours</strong></span>
                      for instances where <span style='color: blue;'>condition &lt; 0.05</span> and <span style='color: red;'><strong>red contours</strong></span>  for <span style='color: red;'>condition &gt; 0.95</strong></span>."),
                style = "font-family: 'Arial'; font-size: 14px; color: #333;"
              ),
              
              tabsetPanel(id = "tabset4",
                          tabPanel(
                            # title = "Comparative Analysis",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Comparative Analysis",
                                  status = "primary",
                                  solidHeader = TRUE, # decide whether to have a solid header or not.
                                  collapsible = TRUE, # decide whether to make it collapsible or not.
                                  width = 12,
                                  fluidRow(
                                    box(
                                      # title = "Select feature",
                                      width = 3,
                                      selectInput("userSelected_Status", "Choose Status:", choices = NULL),
                                      selectInput("userSelected_key_feature", "Choose Key Feature:", choices = NULL),
                                      selectInput("selectedCellType", "Select a secondary feature:", choices = NULL),
                                      actionButton("updateButton", "Update", icon = icon("sync"), class = "btn-primary")
                                      # selectInput("userSelectedCell", "Choose a Cell Type:", choices = names(data()))
                                      # selectInput("userSelectedCell", "Choose col2:", choices = NULL),  # Empty 
                                      # selectInput("X.axis.cell", "Choose a Cell Type:", choices = NULL)  # Empty 
                                    ),
                                    box(
                                      title = "", # title = "Contour Plot",
                                      width = 9,
                                      plotOutput("contour_plot")
                                    )
                                  )
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      
      tabItem(tabName = "algorithm_count_arcs_strength",
              h3("Algorithm count and arcs strength of each arc"), 
              tags$p(HTML("Edges are organized based on the number of algorithms that identified that an edge was enriched (as depicted by the bar graph on the top) 
              and the strength of the enrichment (bottom). The <code>arc Strength</code>  representing the strength of enrichment, corresponds to the 
              likelihood of a partial correlation between the two nodes of an arc being attributable to random chance, given the remainder 
              of the network. The lines symbolizing the strength of enrichment represent the minimum (bottom: <span style='color: red;'><strong>red line</strong></span>) and maximum (bottom: <span style='color: blue;'><strong>blue line</strong></span>)
              values obtained by the different algorithms for each edge. The color of the bar graph signifies whether an edge was significantly 
              enriched with a clear direction and included within the set of arcs identified at the minimum BIC (depicted in <span style='color: green;'><strong>green</strong></span>), significantly
              enriched without a clear direction but included within the set of arcs identified at the minimum BIC (depicted in <span style='color: brown;'><strong>brown</strong></span>
              ), significantly enriched but without a clear direction (depicted in <span style='color: red;'><strong>red</strong></span>), or excluded from the consensus seed network list (depicted in <span style='color: blue;'><strong>blue</strong></span>)."),
                     style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset7",
                          tabPanel(
                            # title = "Diagnostic plot",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "Algorithm count & arcs strength",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1600px; height: 400px; overflow: auto;",
                                    plotOutput("Plot.Algorithm.Count_arcs.strength")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "DAG_network_plot", 
              h3("Causal DAG network"), 
              tags$p(
                HTML("The visualization presents a Directed Acyclic Graph (DAG) that represents the conditional probability distribution inferred
              by analyzing clinical data and features associated with the samples. Each node in the graph symbolizes a feature, sample attribute, 
              or the prevalence of a specific feature or state, while the edges signify the inferred causal relationships between the nodes. 
              A <span style='color: black;'><strong>black</strong></span> arrow denotes a <span style='color: black;'><strong>positive</strong></span> causal relationship, and a <span style='color: red;'><strong>red</strong></span> arrow indicates 
              a <span style='color: red;'><strong>negative</strong></span> or inhibitory causal relation. The extent of influence of the parental node is annotated numerically adjacent to the edge, 
              with the width of an edge being proportional to the posterior probability of its inclusion in the DAG."),
                # br(),
                HTML("Users can interact with the dashboard by selecting a node corresponding to a feature, subsequently viewing a refined DAG
                   that exclusively reveals the direct causal connections relative to the selected node."), 
                style = "font-family: 'Arial'; font-size: 14px; color: #333;"),

              tabsetPanel(id = "tabset3",
                          tabPanel(
                            title = "DAG Network Plot",
                            fluidRow(
                              column(
                                8,
                                box(
                                  title = "DAG Network Plot",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1400px; height: 500px; overflow: auto;",
                                    visNetworkOutput("DAGNetworkPlot")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          ),
                          tabPanel(
                            title = "Arc Slopes Strength Table",
                            fluidRow(
                              column(
                                8,
                                box(
                                  title = "Arc Slopes Strength Table",
                                  status = "info",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 900px; height: 600px; overflow: auto;",
                                    DTOutput("arc_slopes_strength")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )
      ),
      # ------------------------------------------------------------------------              
      tabItem(tabName = "DAG_Plot",
              h3("DAG Network flow"), 
              tags$p(
                HTML("The visualization presents a Directed Acyclic Graph (DAG) that represents the conditional probability distribution inferred
              by analyzing clinical data and features associated with the samples. Each node in the graph symbolizes a feature, sample attribute, 
              or the prevalence of a specific feature or state, while the edges signify the inferred causal relationships between the nodes. 
              A <span style='color: black;'><strong>black</strong></span> arrow denotes a <span style='color: black;'><strong>positive</strong></span> causal relationship, and a <span style='color: red;'><strong>red</strong></span> arrow indicates 
              a <span style='color: red;'><strong>negative</strong></span> or inhibitory causal relation."),
                # br(),
                style = "font-family: 'Arial'; font-size: 14px; color: #333;"),
              tabsetPanel(id = "tabset3",
                          tabPanel(
                            # title = "DAG Network",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title = "DAG Network",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "width: 1400px; height: 700px; overflow: auto;",
                                    plotOutput("DAG.Plot")
                                  ),
                                  width = 12,
                                  collapsible = TRUE
                                )
                              )
                            )
                          )
              )              
      )
    )
  )
)

# ui <- secure_app(ui)
# ui <- secure_app(ui, enable_admin = TRUE)
# -----------------------------------
# Define server logic
server <- function(input, output, session) {
 
  # Reactive value to store the file input
  fileInputState <- reactiveVal(NULL)
  
  # Function to render the fileInput dynamically
  renderFileInput <- function() {
    output$dynamicFileInput <- renderUI({
      fileInput("dataFile", "Data File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    })
  }
  
  # Initial rendering of fileInput
  renderFileInput()

  
  # Update fileInputState when the fileInput changes
  observeEvent(input$dataFile, {
    fileInputState(input$dataFile)
  })
#-------
  userSelected <- reactiveVal(FALSE)

observeEvent(input$userSelected_Status, {
  userSelected(TRUE) # Set the flag when the user changes the selection
}, ignoreInit = TRUE) # ignoreInit ensures this doesn't trigger at app startup
 
 #------
 # Initialize reactive values to store selections
  selectedStatus <- reactiveVal(NULL)
  selectedKeyFeature <- reactiveVal(NULL)
  selectedSecondaryFeature <- reactiveVal(NULL)
  contour_plot_initial <- reactiveVal(FALSE)

  #-------------
  update_clicked <- reactiveVal(TRUE)
  observeEvent(input$updateButton, {
  update_clicked(TRUE)
})




  #------------------------------------------------
  # Reactive value to track if the dataset contains binary columns
  hasBinaryColumns <- reactiveVal(FALSE)
  cycles_resolved <- reactiveVal(TRUE)
  
 
  observe({
  currentTab <- input$sidebarMenu
  print(paste("Current tab:", currentTab))

  if(currentTab == "contour_plot" && !hasBinaryColumns()) {
    print("Navigated to Comparative Analysis")
    showModal(modalDialog(
      tags$div(
        style = "font-size:18px; color:#34495E; padding: 20px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
        tags$p(
          tags$span(
            style = "color: #d68910;", 
            icon("exclamation-triangle", style = "margin-right: 6px; color: #d68910;"), 
            tags$span(style = "font-weight: bold;", "Analysis Not Possible: ")
          ),
          "Your dataset does not contain any binary columns, which are required for generating Comparative Analysis graphs. Please ensure that your dataset includes at least one binary column to proceed with this analysis."
        )
      ),
      footer = tagList(
        tags$button("Close", type = "button", class = "btn btn-default", `data-dismiss`="modal", 
                    style = "background-color: #58d68d; color: black; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;")
      ),
      easyClose = TRUE,
      size = "m" # Adjust the modal size if needed
    ))
  }

   # Force user back to the "WhiteList Check Acyclicity" tab if cycles are not resolved and the user tries to navigate away
  if (!cycles_resolved() && currentTab != "WhiteList_Check_acyclicity" && currentTab != "settings") {
    print("Unresolved cycles detected, redirecting back...")
    updateTabItems(session, "sidebarMenu", "WhiteList_Check_acyclicity")

    showModal(modalDialog(
      title = "Unresolved Cycles Detected",
       tags$div(
        style = "font-size:18px; color:#34495E; padding: 20px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
        tags$p(
          tags$span(
            style = "color: #c0392b;", 
            icon("exclamation-circle", style = "margin-right: 6px; color: #c0392b;"), 
            tags$span(style = "font-weight: bold;", "Action Required: ")
          ),
          "You must resolve all cycles before proceeding. Please check and resolve any cycles in the network."
        )
      ),
       footer = tagList(
        tags$button("OK", type = "button", class = "btn btn-default", `data-dismiss`="modal", 
                    style = "background-color: #e74c3c; color: white; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;")
      ),
      easyClose = TRUE,
      size = "m" # Adjust the modal size if needed
    ))
  }
})


  # browser()
  # # ----------------------------------
  # # check_credentials directly on sqlite db
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(
  #     sqlite_path,
  #     passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  #     # passphrase = "passphrase_wihtout_keyring"
  #   )
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
  
  # # ----------------------------------
  # call the server part
  # check_credentials returns a function to authenticate users
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
  # ----------------------------------
  
  # Now, the logic to run Final.DAG_network_plot_v6 function upon user's selection change.
  # Assuming Final.DAG_network_plot_v6() is a function that generates your desired plot based on the provided parameters
  # output$contour_plot <- renderPlot({
  #   req(input$userSelected_Status, input$userSelected_key_feature, input$selectedCellType) # Ensure all inputs are selected
  #   # Assuming you have a function to generate the plot based on these inputs
  #   print("yahoo")
  #   print(plot)
  #   plot <- Final.DAG_network_plot_v6(data(), input$userSelected_Status, input$userSelected_key_feature, input$selectedCellType)
  #   plot # This assumes your function returns a plot object
  # })
  #----------------------------------
  observeEvent(input$show_nboot, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"), 
          "Number of bootstrap samples", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "m",
        tagList(
          tags$p(
            HTML("Enter the number of bootstrap samples to be generated, represented by the variable <code>nboot</code>. Bootstrapping is a statistical resampling method used to estimate the distribution of a statistic by repeatedly sampling, with replacement, from the observed data. The <code>nboot</code> value determines how many resampled datasets will be created during this process. Please ensure a numeric value is assigned to <code>nboot</code>; failure to do so or assigning a NULL value may result in errors during execution."), 
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observeEvent(input$show_threshold_level, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"), 
          "Threshold Level", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "m",
        tagList(
          tags$p(
            HTML("
            The <code>Threshold Level = N</code> is used to divide the arc strength interval into <code>N quantiles</code>, each representing a threshold level. The user-inputted <code>Threshold Level</code> in Bayesian network inference is crucial for refining the consensus seed network or <code>whitelist</code> in a Directed Acyclic Graph (<code>DAG</code>). This threshold acts as a filter, determining which arcs, based on strength, are included in the final network. Arcs below the user-defined threshold are typically included, improving both network and node connectivity. The threshold aids in balancing model complexity and regression accuracy, quantified using the Bayesian Information Criterion (<code>BIC</code>). By adjusting the threshold, users can influence BIC values and the inclusion of specific arcs, facilitating the creation of a more refined and accurate model. This also ensures the exclusion of inconsistent arcs, preserving the network's consistency and reliability."), 
            style = "font-size: medium; padding: 5px;")          # tags$p("Specify the threshold level for significance.", style = "font-size: medium; padding: 5px;")
          
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observeEvent(input$show_dataFile, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"), 
          "Data File and format", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "m",
        tagList(
          tags$p(
            HTML("Upload the <code>data</code> file in CSV format."), 
            br(),
            HTML("The user-supplied dataset should be structured with each row corresponding to a unique <code>study sample</code>. 
    The first row must contain the names of the features, serving as column headers. One of these columns should represent a 
    <code>binary</code> feature, often categorizing the samples into distinct classes such as <code>diseased</code>  or <code>normal</code>. 
    This binary feature is
    typically integral, representing categorical data. The remaining columns should be <code>numeric</code>, quantifying the levels or counts
    of various attributes or markers in each sample. These numeric columns can represent a variety of biological or clinical 
    measurements, each providing distinctive insights into the characteristics of the study samples. It is crucial that the
    data is well-structured, with consistent and complete entries for each feature across all samples, ensuring the 
    reliability and accuracy of any subsequent analyses or interpretations."),
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observeEvent(input$show_BlackListFile, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"), 
          "BlackList File  and format", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "m",
        tagList(
          tags$p(
            HTML("Upload the <code>BlackList</code>  file in CSV format."), 
            br(),
            HTML("The <code>BlackList</code> serves as a user-defined input and plays a pivotal role in the <code>structural learning</code> and network inference processes within Bayesian networks, which are illustrated as Directed Acyclic Graphs (DAGs). This component is instrumental in molding the network structure, relying primarily on <code>prior knowledge </code> regarding the causal relationships amongst the data features. This list encompasses specific arcs, or <code>directed edges </code>, that are deliberately excluded from the proposed network, serving as constraints based on established or acknowledged information. This ensures the resultant structure and its causal interactions or arcs amongst nodes are in harmony with existing knowledge and logical constraints, thereby maintaining the integrity and accuracy of the network's representation of causal relationships."), 
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observeEvent(input$show_WhiteListFile, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"), 
          "WhiteList File  and format", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "m",
        tagList(
          tags$p(
            HTML("Upload the <code>WhiteList</code> file in CSV format."), 
            br(),
            HTML("The <code>WhiteList</code> operates as a user-defined input and is crucial in structuring and refining Bayesian networks, illustrated as Directed Acyclic Graphs (DAGs). It is designed to facilitate the inclusion of specific arcs (directed edges) in the proposed network, based on the strength of evidence supporting the existence of an arc, or alternatively, the <code>arc strength</code>. Arcs with strength below a predetermined threshold are incorporated in the <code>Whitelist</code>, ensuring that the evolving network structure is enriched with potential causal relationships that are substantiated by a robust degree of evidence. This approach optimizes the balance between model accuracy and complexity, allowing for a more nuanced and precise representation of causal relationships within the network while maintaining interpretability and coherence. In essence, the \"Whitelist\" serves as an inclusive filter, allowing arcs that meet or exceed certain criteria to shape and refine the network's structure, thereby enhancing the reliability and validity of the inferred causal relationships."),
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observeEvent(input$show_Dir_AlgoDescriptions, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-code-branch", style = "color: white; padding-right: 10px;"), 
          "Directed Algorithm Descriptions", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "l",
        tagList(
          tags$p(HTML("Select Algorithms from the list: 
          Incremental association with false discovery rate control - <code>IAMB.FDR</code>, 
                      Practical constraint - <code>PC.STABLE</code>, 
                      Grow-shrink Markov Blanket - <code>GS</code>,
                      Incremental association Markov Blanket - <code>IAMB</code>,
                      Hill climbing - <code>HC</code>,
                      Tabu search - <code>Tabu</code>,
                      Max-min hill-climbing - <code>MMHC</code>,
                      Restricted maximization - <code>RSMAX2</code>"), 
                 style = "font-size: medium; padding: 5px;"),
          br(),
          
          tags$p(HTML("The \'Directed Algorithm\' in <code>Bayesian network inference</code>  is crucial for deducing the structure and causality in a network, represented as a Directed Acyclic Graph (<code>DAG</code>). Users employ an ensemble of different structural learning algorithms to accurately identify potential causal interactions or arcs among nodes and to estimate the <code>conditional probability distributions</code> from datasets. These algorithms contribute to constructing <code>blacklists</code> and/ or <code>whitelists</code>  of arcs, serving as a basis for exclusion and inclusion based on <code>prior knowledge</code> and <code>arc strength</code>."), 
                 style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observeEvent(input$show_UnDir_AlgoDescriptions, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-code-branch", style = "color: white; padding-right: 10px;"), 
          "Undirected Algorithm Descriptions", 
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ), 
        size = "m",
        tagList(
          tags$p(HTML("* Max-Min Parents and Children - <code>MMPC</code>"), style = "font-size: medium; padding: 5px;"),
          tags$p(HTML("* Stable and Interpretable HITON Parents and Children - <code>SI.HITON.PC</code>"), style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observe({
    print(input$sidebarItemExpanded)
  })
  
  observe({
    print(plot_done())
  })
  # ----------------------------------
  
  datapath <- "./"
  # Initialize reactive values
  data_uploaded <- reactiveVal(FALSE)
  rv <- reactiveValues()
  default_data_used <- reactiveVal(FALSE)  # to track if default data is used
  
  data <- reactiveVal(NULL)
  
  Black_List <- reactiveVal(NULL)
  White_List <- reactiveVal(NULL)
  
  # Check if files are uploaded or not
  data_present <- reactiveVal(FALSE)
  Black_List_present <- reactiveVal(FALSE)
  White_List_present <- reactiveVal(FALSE)
  
  possible_whitelist_reactiveVal <- reactiveVal(NULL)
  final_white_list <- reactiveVal(NULL)
  
  plot_done <- reactiveVal(FALSE)
  # finished_running <- reactiveVal(FALSE)
  Status_var <- reactiveVal(NULL)  

  plots_list = reactiveVal(NULL)

  # -----------------------------------
  # New observeEvent blocks:
      observeEvent(input$dataFile, {
        tryCatch({
          data.check <- input$dataFile
          if (!is.null(data.check$datapath) && file.exists(data.check$datapath)) {
            data(read.csv(data.check$datapath))
            print("Data uploaded")
          }
        }, error = function(e) {
          showNotification(paste("Error reading data File:", e$message), type = "error")
        })
      })
      
      observeEvent(input$BlackListFile, {
        tryCatch({
          Black_List.check <- input$BlackListFile
          if (!is.null(Black_List.check$datapath) && file.exists(Black_List.check$datapath)) {
            Black_List(read.csv(Black_List.check$datapath))
            print("Black_List uploaded")
          }
        }, error = function(e) {
          showNotification(paste("Error reading BlackList File:", e$message), type = "error")
        })
      })
      
      observeEvent(input$WhiteListFile, {
        tryCatch({
          White_List.check <- input$WhiteListFile
          if (!is.null(White_List.check$datapath) && file.exists(White_List.check$datapath)) {
            White_List(read.csv(White_List.check$datapath))
            print("White_List uploaded")
          }
        }, error = function(e) {
          showNotification(paste("Error reading WhiteList File:", e$message), type = "error")
        })
      })
      #---------


  observeEvent(input$runButton, {
    print("Run Discovery clicked")
    
    # browser()
    # --------------
    if((length(input$algorithm_directed) < 2) || is.null(input$algorithm_directed)) {
    showModal(modalDialog(
      tags$div(
        style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
        tags$p(
          tags$span(
            style = "color: #B71C1C;", 
            icon("exclamation-triangle", style = "margin-right: 6px; color: #B71C1C;"), 
            tags$span(style = "font-weight: bold;", "Error: ")
          ), 
          "Please select at least two directed algorithms to proceed."
        )
      ),
      easyClose = TRUE,
      footer = tags$div(
        actionButton("close", "Acknowledge", 
                     style = "background-color: #2980B9; color: white; border: none; padding: 10px 20px; border-radius: 5px; margin: 0px; cursor: pointer; box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
                      &:hover {
                        background-color: #2471A3;
                      }"
        ),
        style = "text-align: center; background-color: #fdfefe; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px; padding: 0px;"
      )
    ))
    }
     else {
      # -------------------
      data_present <- reactive({ !is.null(input$dataFile) })
      Black_List_present <- reactive({ !is.null(input$BlackListFile) })
      White_List_present <- reactive({ !is.null(input$WhiteListFile) })
      
      # Check if data is uploaded and set data_uploaded reactive value

      print("hereeee")
      print(data_present())
      print(Black_List_present())
      print(White_List_present())

      if (data_present() & (Black_List_present() || White_List_present())) {
        print("im here bruh")
        data_uploaded(TRUE)
        print(data_uploaded())
      } else {
        data_uploaded(FALSE)
      }
      
      
      
      # ------------------- main functionality 
    print("im data uploaded")
    print(data_uploaded())
      if (isTRUE(data_uploaded()) || isTRUE(default_data_used())) {


         showModal(modalDialog(
          tags$div(
            style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-radius: 5px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.1);  margin: auto;",
            tags$p(
              tags$span(
                style = "color: #2980b9;", 
                icon("sync", style = "margin-right: 6px; color: #2980b9; animation: spin 2s linear infinite;"), 
                tags$span(style = "font-weight: bold;", "Generating Data: ")
              ),
              "Please wait, the initial plots and tables are getting generated..."
            )
          ),
          footer = NULL,  # No footer as requested
          easyClose = FALSE,  # Prevent closing by clicking outside the modal
          size = "s"  # Small size, but the width is adjusted using custom styles
        ))

        
        cluster <- 6
        # Initial operations
        cl <- makeCluster(cluster, type = "SOCK")  # Create a cluster with specified nodes
        set.seed(2023)  # Set seed for reproducibility
        rand <- clusterEvalQ(cl, runif(10))  # Generate random numbers using cluster
        
        # Data processing
        data_process_result <- data_process_Correlation.v2(data())
        
        # -----------------
        output$viewDataStructure <- renderPrint({
          str(data_process_result)
          # Summary(data_process_result)
        })
        # -----------------
        if("discretized_data" %in% names(data_process_result)) {
          discretized_data <- as.data.frame(data_process_result$discretized_data)
        } else {
          stop("discretized_data not found in data_process_result")
        }
        
        corrcoef<- as.data.frame(data_process_result$corrcoef)
        data<- as.data.frame(data_process_result$data)
        
        # -----------------
        renderStyledTable <- function(table_name, rownames = TRUE, download_version = c()) {
          renderDT({
            datatable(
              table_name,
              extensions = c('Buttons', 'Scroller'),
              options = list(
                dom = 'Bfrtip',
                buttons = download_version,
                pageLength = 10, # Changed this from 10 to 5.
                autoWidth = TRUE,
                scrollX = TRUE, # Already present and set to TRUE.
                scroller = TRUE,
                deferRender = TRUE,
                scrollY = '400px',  # adjust this value to change the visible height of the table.
                scrollCollapse = TRUE
              ),
              rownames = rownames,
              class = 'compact stripe hover row-border order-column'
            ) %>%
              formatStyle(
                columns = names(table_name),
                backgroundColor = styleEqual(c(NA, 1), c("white", "#f7f9f9")),
                color = 'black',
                fontSize = '14px',
                fontWeight = styleEqual(c(NA, 1), c("normal", "bold")),
                lineHeight = '16px',
                textAlign = 'center'
              ) %>%
              formatStyle(
                columns = names(table_name),
                borderTop = '1px solid #dee2e6',
                borderBottom = '1px solid #dee2e6',
                textAlign = 'center'
              )
          }, server = FALSE)
        }
        # -------------------------------
        output$Black_List <- renderStyledTable(Black_List(), rownames = TRUE, download_version = c('csv', 'excel'))
        # -------------------------------
        # Initial edge list
        Blank_edge_list <- data.frame(from = character(), to = character(), Edge_No= integer())
        Edge_count <- 1
        # -------------------------------
        if (!is.null(input$algorithm_directed) & !is.null(input$algorithm_undirected)) {
          
          run_algorithm_directed_with_messages <- function(algorithm_directed, Blank_edge_list, Edge_count, discretized_data, nboot, cl, Black_List, White_List, corrcoef) {
            # Start capturing output
            output_messages <- capture.output(
              result_directed <- run_algorithm_directed(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)
            )
            # Return both result and messages
            return(list(result_directed = result_directed, messages = output_messages))
          }
          
          output_algorithm_directed <- run_algorithm_directed_with_messages(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)
          
          # Extract results
          Arcs.Cor_streng_table.alg <- output_algorithm_directed$result_directed$Arcs.Cor_streng_table.alg
          updated_edge_list_directed <- output_algorithm_directed$result_directed$edge_list
          updated_edge_count_directed <- output_algorithm_directed$result_directed$edge_count
          names(updated_edge_list_directed) <- c("from", "to", "Edge_No")
          
          # -----------
          run_algorithm_Undirected_with_messages <- function(algorithm_undirected= NULL, updated_edge_list_directed, updated_edge_count_directed, discretized_data, nboot, cl, corrcoef) {
            # Start capturing output
            output_messages <- capture.output(
              all_arc_list <- run_algorithm_Undirected(input$algorithm_undirected, updated_edge_list_directed, updated_edge_count_directed, discretized_data, input$nboot, cl, corrcoef)
            )
            # Return both result and messages
            return(list(all_arc_list = all_arc_list, messages = output_messages))
          }
          
          output_algorithm_Undirected <- run_algorithm_Undirected_with_messages(input$algorithm_undirected, updated_edge_list_directed, updated_edge_count_directed, discretized_data, input$nboot, cl, corrcoef)
          
          output$directed_Undirected_algorithm_messages <- renderPrint({
            # "Algorithm Progression Messages Here."
            messages_combined <- c(output_algorithm_directed$messages,
                                   "\n",
                                   output_algorithm_Undirected$messages)
            cat(messages_combined, sep = "\n")
          })
          
          Arcs.Cor_table.alg <- output_algorithm_Undirected$all_arc_list$Arcs.Cor_table.alg
          all_edge_list <- output_algorithm_Undirected$all_arc_list$all_edge_list
          
        } else if(!is.null(input$algorithm_directed)) {
          
          run_algorithm_directed_with_messages <- function(algorithm_directed, Blank_edge_list, Edge_count, discretized_data, nboot, cl, Black_List, White_List, corrcoef) {
            # Start capturing output
            output_messages <- capture.output(
              result_directed <- run_algorithm_directed(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)
            )
            # Return both result and messages
            return(list(result_directed = result_directed, messages = output_messages))
          }
          
          output_algorithm_directed <- run_algorithm_directed_with_messages(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)
          
          # Extract results
          Arcs.Cor_streng_table.alg <- output_algorithm_directed$result_directed$Arcs.Cor_streng_table.alg
          updated_edge_list_directed <- output_algorithm_directed$result_directed$edge_list
          updated_edge_count_directed <- output_algorithm_directed$result_directed$edge_count
          names(updated_edge_list_directed) <- c("from", "to", "Edge_No")
          
          # -----------
          output$directed_Undirected_algorithm_messages <- renderPrint({
            messages_combined <- output_algorithm_directed$messages
            cat(messages_combined, sep = "\n")
          })
          
          Arcs.Cor_table.alg <- Arcs.Cor_streng_table.alg
          all_edge_list <- updated_edge_list_directed
        } else {
          return()
        }
        # ------------------------------------------------------------------------------------------------
        output$all_edge_list <- renderStyledTable(all_edge_list, rownames = FALSE, download_version = list())
        
        augmented_edge_list<- augmented_edge_table(Black_List(),
                                                   all_edge_list, 
                                                   input$algorithm_directed, 
                                                   Arcs.Cor_streng_table.alg,
                                                   input$algorithm_undirected,
                                                   Arcs.Cor_table.alg)
        # ---------------------
        # For each column want to round, check if it's numeric. If it is, round it.
        cols_to_round <- grep("\\.strength|_strength", names(augmented_edge_list))
        
        for(col in cols_to_round){
          augmented_edge_list[[col]] <- signif(as.numeric(augmented_edge_list[[col]]), 2)
        }
        # ---------------------
        output$augmented_edge_list <- renderStyledTable(augmented_edge_list, rownames = TRUE, download_version = c('csv', 'excel'))
        
        possible_seed_arcs_filter<- uninformative_arcs_removal(augmented_edge_list, input$algorithm_undirected)
        # ---------------------
        cols_to_round <- grep("\\.strength|_strength", names(possible_seed_arcs_filter))
        
        for(col in cols_to_round){
          possible_seed_arcs_filter[[col]] <- signif(as.numeric(possible_seed_arcs_filter[[col]]), 2)
        }
        # ---------------------
        output$possible_seed_arcs_filter <- renderStyledTable(possible_seed_arcs_filter, rownames = TRUE, download_version = c('csv', 'excel'))
        
        threshold <- finding_threshold_values(possible_seed_arcs_filter, input$threshold_level)
        # ------------------
        # threshold <- as.numeric(sprintf("%.3f", threshold))
        temp.white_thresh.cols <- temp.white_thresh.cols(possible_seed_arcs_filter, threshold)

        temp_list_merge<- temp.white_thresh.cols$temp_list_merge
        augmneted.thresh.cols <- temp.white_thresh.cols$augmneted.thresh.cols
        
        output$augmneted.thresh.cols <- renderStyledTable(augmneted.thresh.cols, rownames = TRUE, download_version = c('csv', 'excel'))
        
        num.white_thresh <- temp.white_thresh.cols$num.white_thresh
        (num.white_thresh <- as.data.frame(num.white_thresh))
        
        calculate_loss_npar_table <- calculate_loss_npar_table (threshold, 
                                                                temp_list_merge, 
                                                                discretized_data, 
                                                                input$nboot, cl, Black_List()) 
        
        npar_merged  <- calculate_loss_npar_table$npar_merged
        L1_merged  <- calculate_loss_npar_table$L1_merged
        # ------------------
        # For each column want to round, check if it's numeric. If it is, round it.
        for (col in names(L1_merged)) {
          if (is.numeric(L1_merged[[col]])) {
            L1_merged[[col]] <- as.numeric(sprintf("%.2f", L1_merged[[col]]))
          }
        }
        # ------------------
        parents_list_merged  <- calculate_loss_npar_table$parents_list_merged
        
        Total.BIC.thresh  <- calculate_loss_npar_table$Total.BIC.thresh
        Total.BIC.thresh <- as.data.frame(Total.BIC.thresh)
        
        output$L1_merged <- renderStyledTable(L1_merged, rownames = TRUE, download_version = c('csv', 'excel'))
        output$npar_merged <- renderStyledTable(npar_merged, rownames = TRUE, download_version = c('csv', 'excel'))
        
        num_arcs.All.thresh <- calculate_loss_npar_table$num_arcs.All.thresh
        num_arcs.All.thresh <- as.data.frame(num_arcs.All.thresh)
        
        BIC_merged_table <- calculate_bic (discretized_data, npar_merged, L1_merged, threshold) 
        
        # For each column want to round, check if it's numeric. If it is, round it.
        for (col in names(BIC_merged_table)) {
          if (is.numeric(BIC_merged_table[[col]])) {
            BIC_merged_table[[col]] <- as.numeric(sprintf("%.2f", BIC_merged_table[[col]]))
          }
        }

        output$BIC_merged_table <- renderStyledTable(BIC_merged_table, rownames = TRUE, download_version = c('csv', 'excel'))
        # browser()
        
        min_bic.Parent_whitelist_acyclic <- find_min_bic.Parent_whitelist_acyclic.v3(BIC_merged_table, npar_merged, parents_list_merged)
        
        bic_min_table <- min_bic.Parent_whitelist_acyclic$bic_min_table
        possible.white.list <- min_bic.Parent_whitelist_acyclic$possible.white.list
        # final_white_list <- min_bic.Parent_whitelist_acyclic$final_white_list
        
        
        possible.white.list$from <- as.character(possible.white.list$from)
        possible.white.list$to <- as.character(possible.white.list$to)
        
        possible_whitelist_reactiveVal(possible.white.list)
        print("possible whitelist:")
        print(possible.white.list) # we printed data frame with 2 columns 
        
        
        print("possible whitelist reactiveVal:")
        print(possible_whitelist_reactiveVal())# we printed data frame with 2 columns 
        print("Number of Columns of 'possible whitelist reactiveVal':")
        print(length(colnames(possible_whitelist_reactiveVal()))) # we have 2 columns 
        # --------------------------------
        output$bic_min_table <- renderStyledTable(bic_min_table, rownames = TRUE, download_version = c('csv', 'excel'))
        output$possible.white.list <- renderStyledTable(possible_whitelist_reactiveVal(), rownames = TRUE, download_version = c('csv', 'excel'))
        #---------------------------
        g <- reactiveVal({
          if (ncol(possible_whitelist_reactiveVal()) < 2) {
            (graph.empty())
          } else {
            (graph_from_data_frame(possible_whitelist_reactiveVal(), directed = TRUE))
          }
        })
        # -----------------------
        # Convert igraph object to a visNetwork suitable format
        vis_graph <- reactive({
          nodes_data <- data.frame(id = V(g())$name, label = V(g())$name)
          edges_data <- get.data.frame(g(), what = "edges")
          list(nodes = nodes_data, edges = edges_data)
        })


        
        output$initialPlot <- renderVisNetwork({
          visNetwork(nodes = vis_graph()$nodes, edges = vis_graph()$edges) %>%
            visNodes(shape = "circle", 
                     font = list(size = 12,
                                 vadjust = 0,
                                 bold = TRUE,
                                 color = "black")) %>%
            visEdges(arrows = "to",
                     smooth = T,
                     font = list(size = 15, color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
            visOptions(highlightNearest = list(enabled = F, hover = F),
                       nodesIdSelection = F)%>%
            visLayout(randomSeed = 123,
                      improvedLayout = TRUE)  %>%
            visPhysics(solver = "forceAtlas2Based",  # The physics solver
                       forceAtlas2Based = list(gravitationalConstant = -30,  # Adjust as needed
                                               centralGravity = 0.005,  # Adjust as needed
                                               springLength = 100,  # Adjust as needed
                                               springConstant = 0.18))  # Adjust as needed
        })
        # -----------------------
        # plot_done(TRUE)
        # -----------------------
        observeEvent(plot_done(), {
            
            if (plot_done() == TRUE) {
              removeModal()  # Ensure the "Resolving Cycles" modal is removed
              content <- paste0(
                "<div style='font-size:18px; color:#34495E; padding: 3px 5px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;'>",
                "<p>",
                "<span style='color: #FF0000;'>",
                # "<i class='fa fa-exclamation-triangle' style='margin-right: 6px; color: #FF0000;'></i>",
                "<span style='font-weight: bold;'>Proceed to Further Analysis: </span>",  
                "</span>",
                "<br><br>",
                "<span style='font-weight: bold;'> Navigate to the tab 'WhiteList/ Check acyclicity' <a href='#' id='goToTab'>Click here</a>.",
                "</p>",
                "</div>"
              )
              shinyalert::shinyalert(
                # title = "Proceed to Further Analysis",
                text = content,
                html = TRUE,
                showConfirmButton = FALSE,
                # confirmButtonColor = "#3498db" # This is a shade of blue.
                # showConfirmButton = FALSE
              )
                
                

            }
            
        })
        # --------------------------------------2
        FindCycles = function(g) {
          Cycles = NULL
          for(v1 in V(g)) {
            if(igraph::degree(g, v1, mode="in") == 0) { next }
            # if(degree(g, v1, mode="in") == 0) { next }
            GoodNeighbors = neighbors(g, v1, mode="out")
            GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
            for(v2 in GoodNeighbors) {
              TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
              TempCyc = TempCyc[which(sapply(TempCyc, length) > 2)] # can check A->B and B->A as cycle: cycle length of two
              TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
              Cycles  = c(Cycles, TempCyc)
            }
          }
          Cycles
        }
        # -------------------------------------- FIRST SECTION
        observe({
          cycles <- FindCycles(g())
          if (length(cycles) == 0) {
            final_white_list(possible_whitelist_reactiveVal())
            output$final_white_list <- renderStyledTable(final_white_list(), rownames = TRUE, download_version = c('csv', 'excel'))
            
            # Convert igraph object to a visNetwork suitable format
            vis_graph <- reactive({
              nodes_data <- data.frame(id = V(g())$name, label = V(g())$name)
              edges_data <- get.data.frame(g(), what = "edges")
              list(nodes = nodes_data, edges = edges_data)
            })
            
            output$plot <- renderVisNetwork({
              visNetwork(nodes = vis_graph()$nodes, edges = vis_graph()$edges) %>%
                visNodes(shape = "circle", color = list(background = "#87CEEB")) %>%
                visEdges(arrows = "to",
                         smooth = TRUE,
                         font = list(size = 15, color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
                visOptions(highlightNearest = list(enabled = F, hover = F),
                           nodesIdSelection = F)%>%
                visLayout(randomSeed = 123,
                          improvedLayout = TRUE)  %>%
                visPhysics(solver = "forceAtlas2Based",  # The physics solver
                           forceAtlas2Based = list(gravitationalConstant = -50,  # Adjust as needed
                                                   centralGravity = 0.005,  # Adjust as needed
                                                   springLength = 100,  # Adjust as needed
                                                   springConstant = 0.18))  # Adjust as needed
            })
            
            # --------------------------------------
            # Detect binary columns
            # observe({
            # req(data())

            #   binary_cols <- sapply(data(), function(x) all(x %in% c(0, 1)))
            #   binary_col_names <- names(binary_cols[binary_cols])
            #     print(binary_col_names)
            #   print(length(binary_col_names))
            #   if (length(binary_col_names) == 1) {
            #     # If there's exactly one binary column, use it as Status
            #     updateSelectInput(session, "userSelected_Status", choices = binary_col_names, selected = binary_col_names[1])
            #   } else {
            #     # If there are multiple binary columns, let the user choose
            #     updateSelectInput(session, "userSelected_Status", choices = binary_col_names)
            #   }
            # })
            observe({
              req(fileInputState()) # Ensure a file is selected

              binary_cols <- sapply(data(), function(x) all(x %in% c(0, 1)))
              binary_col_names <- names(binary_cols[binary_cols])
              print(binary_col_names)
              print(length(binary_col_names))
              
              if (length(binary_col_names) == 0) {  
                # No binary columns found, update var
                hasBinaryColumns(FALSE)
              } else if (length(binary_col_names) == 1) {

                hasBinaryColumns(TRUE)
                # If there's exactly one binary column, use it as Status
                updateSelectInput(session, "userSelected_Status", choices = binary_col_names, selected = binary_col_names[1])
              } else {
                hasBinaryColumns(TRUE)
                # If there are multiple binary columns, let the user choose
                  current_status <- input$userSelected_Status
                  if (!is.null(current_status) && current_status %in% binary_col_names) {
                  # Current selection is valid, do nothing
                  print("Current selection is already valid.")
                } else {
                  # Update because the current selection is not valid or not set
                  updateSelectInput(session, "userSelected_Status", choices = binary_col_names)
                }
              }
            })

            
            # Assuming 'data' is a reactive expression returning your dataset
          # observe({
          #   # Update choices for 'Choose Status:'
          #   updateSelectInput(session, "userSelected_Status", choices = names(data()))
          # })
  
      # observeEvent(input$updateButton, {
        
      #   # Update reactive values with current selections
      #   selectedStatus(input$userSelected_Status)
      #   selectedKeyFeature(input$userSelected_key_feature)
      #   selectedSecondaryFeature(input$selectedCellType)
  
      #   # For demonstration, let's print the selections. Replace this with your actual logic.
      #   print(paste("Status Selected:", input$userSelected_Status))
      #   print(paste("Key Feature Selected:", input$userSelected_key_feature))
      #   print(paste("Secondary Feature Selected:", input$selectedCellType))

      #   # Now you can trigger any reactive expressions or render functions that depend on these selections.
      #   # E.g., updating a plot, a table, or any computation that uses these inputs.
        
      #   # If those updates were previously in reactive expressions or observe blocks directly tied to the dropdown inputs,
      #   # you'll move that logic here, or better, into reactive expressions or functions called from here.
      # })


  
      observeEvent(input$userSelected_Status, {
        # Fetch all column names except for the selected status
        valid_features <- setdiff(names(data()), input$userSelected_Status)
        
        # Determine valid choices for 'Choose Key Feature:' dropdown
        current_key_feature <- input$userSelected_key_feature
        
        # If the current key feature is not in the list of valid features, update it to the first valid feature
        if (!current_key_feature %in% valid_features && length(valid_features) > 0) {
          current_key_feature <- valid_features[1]
        }
        
        # Update 'Choose Key Feature:' dropdown with valid features and the currently selected key feature
        updateSelectInput(session, "userSelected_key_feature", choices = valid_features, selected = current_key_feature)
        
        # Now update the 'Select a secondary feature:' dropdown based on the newly selected key feature
        valid_secondary_features <- setdiff(names(data()), c(input$userSelected_Status, current_key_feature))
        
        # Preserve current secondary selection if it's still valid; otherwise, update to the first valid choice
        current_secondary_selection <- input$selectedCellType
        if (!current_secondary_selection %in% valid_secondary_features && length(valid_secondary_features) > 0) {
          current_secondary_selection <- valid_secondary_features[1]
        }
        updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
      })


      observeEvent(input$userSelected_key_feature, {
      print("Key feature changed to: ")
      print(input$userSelected_key_feature)
      
      # Determine valid choices for the 'Select a secondary feature:' dropdown
      valid_secondary_features <- setdiff(names(data()), c(input$userSelected_Status, input$userSelected_key_feature))
      print("Valid secondary features: ")
      print(valid_secondary_features)
      
      # Check if the current selection of 'Select a secondary feature:' is still valid
      current_secondary_selection <- input$selectedCellType
      print("Current secondary selection: ")
      print(current_secondary_selection)
      
      if(!current_secondary_selection %in% valid_secondary_features) {
        # If not valid, update to the first valid choice
        current_secondary_selection <- valid_secondary_features[1]
        updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
        print("Secondary selection updated to: ")
        print(current_secondary_selection)
      } else {
        # If still valid, ensure the UI is updated to reflect the current state without changing the selection
        updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
        print("Secondary selection remains unchanged.")
      }
    })

            
            print("I AM WHITE LISTE ")
            print(final_white_list())

            # -----------------------------------
            observe({
            # Ensure possible.white.list is not NULL
            # req(selectedStatus(), selectedKeyFeature(), selectedSecondaryFeature())
            req(final_white_list(), !contour_plot_initial() )

            print("I CAME TO 1")

            Final.DAG_network_plot_v6 <- Final.DAG_network_plot_v6 (augmented_edge_list,
                                                                    possible_seed_arcs_filter,
                                                                    data(), discretized_data,
                                                                    final_white_list(),
                                                                    Black_List(),
                                                                    input$nboot, cl,
                                                                    corrcoef,
                                                                    input$userSelected_Status,
                                                                    input$userSelected_key_feature)
            
             plot_done(TRUE)

            Alg.Count_arcs.strength.table <- Final.DAG_network_plot_v6$Alg.Count_arcs.strength.table
            # plots_list <- Final.DAG_network_plot_v6$plots_list
            plots_list(Final.DAG_network_plot_v6$plots_list)
            
            

            output$DAG.Plot <- renderPlot({
              # Final.DAG_network_plot_v6$DAG.Plot
              Final.DAG_network_plot_v6$plotFunction()
            })
            
            
            # -----------------
            
            arcs <- Final.DAG_network_plot_v6$arcs.BRCA
            P_strength <- Final.DAG_network_plot_v6$P_strength
            
            arc_slopes.strength <- Final.DAG_network_plot_v6$arc_slopes.strength
            
            output$Diagnostic_plot <- renderPlot({
              Diagnostic_plot.v3(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold)
            })
            # --------------
            output$Plot.Algorithm.Count_arcs.strength <- renderPlot({
              plot_Algorithm.Count_arcs.strength(Alg.Count_arcs.strength.table)
            })
            # DAG Network Plot
            output$DAGNetworkPlot <- renderVisNetwork({
              network <- Final.DAG_network_plot_v6$network
            })
            # arc_slopes_strength <- Final.DAG_network_plot_v6$arc_slopes.strength
            final_DAG_detail <- Final.DAG_network_plot_v6$final_DAG_detail
            
            output$arc_slopes_strength <- renderStyledTable(final_DAG_detail, rownames = TRUE, download_version = c('csv', 'excel'))

              # shinyjs::delay(500, {  # Delay in milliseconds
              #   removeModal()  # Ensure the "Resolving Cycles" modal is removed
                
              #   shinyalert::shinyalert(
              #     title = "Cycles Resolved",
              #     text = "All the cycles were removed successfully.",
              #     type = "success"
              #   )

              #  cycles_resolved(TRUE)  # Update the cycle resolution state

              # })


            }
            )} else{
              plot_done(TRUE)
              cycles_resolved(FALSE)
            }
            
        })
        # --------------------------------------2
        output$checkboxUI <- renderUI({
          tryCatch({
            # Inside tryCatch to catch potential errors
            cycles <- FindCycles(g())
            #-------------
            observe({
              print("Cycles found:")
              print(FindCycles(g()))
            })
            #-------------
            print(paste("Number of cycles found:", length(cycles)))
            
            # Check if cycles are empty and Define output$cycleMessage
            output$cycleMessage <- renderUI({
              if (length(cycles) == 0) {
                HTML("<strong>There are no cycles</strong>")
              } else {
                num_cycles <- length(cycles)
                if (num_cycles == 1) {
                  HTML(paste("<strong>There is", num_cycles, "cycle</strong>"))
                } else {
                  HTML(paste("<strong>There are", num_cycles, "cycles</strong>"))
                }
              }
            })
            # --------------------------------------------------
            ids <- paste0("cycle_", seq_along(cycles))
            # Convert the cycles to arcs using the graph for vertex names
            cycle_arcs = lapply(cycles, cycle_to_arcs, g())
            
            ui_elems <- lapply(seq_along(cycle_arcs), function(i) {
              choices = setNames(cycle_arcs[[i]], cycle_arcs[[i]])
              column(6, checkboxGroupInput(ids[i], label = paste0("Cycle ", i), choices = choices))
            })
            
            # Wrap every two cycles in a fluidRow
            rows <- split(ui_elems, ceiling(seq_along(ui_elems)/2))
            tagList(lapply(rows, function(row) fluidRow(row)))
            
          }, error = function(e) {
            # Print the error message
            print(paste("Error in rendering checkbox UI:", e$message))
          })
        })
        # --------------------------------------------------new
        output$removeButtonUI <- renderUI({
          # Check the length of cycles
          cycles <- FindCycles(g())
          
          if (length(cycles) > 0) {
            # If cycles exist, render the button
            div(
              style = "text-align: left;", # attention: change center to left
              actionButton(inputId = "remove", 
                           label = tags$span(icon("fas fa-hand-pointer", style="margin-right: 4px;"), "Remove selected edges </strong>"), # attention: added </strong>
                           style = "background-color: #3498db; color: white; padding: 10px 20px; font-size: 14px; border-radius: 5px; cursor: pointer;")
            )
          } else {            # Else, don't render anything
            return(NULL)
          }
        })
        # --------------------------------------2
        observeEvent(input$remove, {

          # Show the modal indicating the cycle resolution process is starting
          showModal(modalDialog(
            tags$div(
              style = "font-size:18px; color:#34495E; padding: 20px; background-color: #EAECEE; border-radius: 5px; text-align: center;",
              tags$p(
                tags$span(
                  style = "color: #3498db;", 
                  icon("sync", style = "margin-right: 6px; color: #3498db; animation: spin 2s linear infinite;"), 
                  tags$span(style = "font-weight: bold;", "Resolving Cycles: ")
                ),
                "Please wait, we're resolving the cycles..."
              )
            ),
            footer = NULL, # No footer to prevent user from closing the modal
            easyClose = FALSE, # Prevent closing by clicking outside the modal
            size = "s"
          ))
          
          cycles <- FindCycles(g())
          # ----------------
          # Check if there are no cycles
          if (length(cycles) == 0) {
            print("If condition:possible_whitelist_reactiveVal:")
            print(possible_whitelist_reactiveVal())
            print(possible.white.list)
            
          } else {
           
            g_final <- g()
            # List to store all edges to be removed
            edges_to_remove_names <- c()
            
            for (i in seq_along(cycles)) {
              id <- paste0("cycle_", i)
              selected_edges <- input[[id]]
              if(is.null(selected_edges)) {
                edges_to_remove_names <- c(edges_to_remove_names, cycle_to_arcs(cycles[[i]], g())[[1]])
              } else {
                edges_to_remove_names <- c(edges_to_remove_names, selected_edges)
              }
            }
            # Convert edge names to edge indices
            all_edge_names <- getEdgeNames(g_final)
            edges_to_remove_indices <- which(all_edge_names %in% edges_to_remove_names)
            g_final <- delete_edges(g_final, edges_to_remove_indices)
            
            # final_white_list(as.data.frame(get.edgelist(g_final, names = TRUE)))
            g_arcs_final <- as.data.frame(get.edgelist(g_final, names = TRUE))
            colnames(g_arcs_final) <- c("from", "to")
            g_arcs_final$from <- as.character(g_arcs_final$from)
            g_arcs_final$to <- as.character(g_arcs_final$to)
            final_white_list(g_arcs_final)
            
            print("final_white_list:")
            print(final_white_list()) # we printed data frame with 2 columns
            
            output$final_white_list <- renderStyledTable(final_white_list(), rownames = TRUE, download_version = c('csv', 'excel'))

            # Convert igraph object to a visNetwork suitable format
            vis_graph <- reactive({
              nodes_data <- data.frame(id = V(g_final)$name, label = V(g_final)$name)
              edges_data <- get.data.frame(g_final, what = "edges")
              list(nodes = nodes_data, edges = edges_data)
            })
            
            output$plot <- renderVisNetwork({
              visNetwork(nodes = vis_graph()$nodes, edges = vis_graph()$edges) %>%
                visNodes(shape = "circle", color = list(background = "#87CEEB")) %>%
                visEdges(arrows = "to") %>%
                visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE))%>%
                visLayout(randomSeed = 123,
                          improvedLayout = TRUE)  %>%
                visPhysics(solver = "forceAtlas2Based",  # The physics solver
                           forceAtlas2Based = list(gravitationalConstant = -50,  # Adjust as needed
                                                   centralGravity = 0.005,  # Adjust as needed
                                                   springLength = 100,  # Adjust as needed
                                                   springConstant = 0.18))  # Adjust as needed
            })
          }
          # cycles_resolved(TRUE)
          # # Cycle resolution process is completed, remove the modal
          # removeModal()
          
          # # Show another modal or a notification that cycles have been resolved
          #  shinyalert::shinyalert(
          #   title = "Cycles Resolved",
          #   text = "All  the cycles were removed successfully",
          #   type = "success"
          # )

          shinyjs::delay(500, {  # Delay in milliseconds
                removeModal()  # Ensure the "Resolving Cycles" modal is removed
                
                shinyalert::shinyalert(
                  title = "Cycles Resolved",
                  text = "All the cycles were removed successfully.",
                  type = "success"
                )

               cycles_resolved(TRUE)  # Update the cycle resolution state

              })

          })
          # --------------------------------------
          
          # Detect binary columns and set up Status options
  

            observe({
              req(fileInputState()) # Ensure a file is selected


              binary_cols <- sapply(data(), function(x) all(x %in% c(0, 1)))
              binary_col_names <- names(binary_cols[binary_cols])
              print(binary_col_names)
              print(length(binary_col_names))
              
              if (length(binary_col_names) == 0) {
                # No binary columns found, UPDATE VAR
                hasBinaryColumns(FALSE)

              } else if (length(binary_col_names) == 1) {
                hasBinaryColumns(TRUE)

                # If there's exactly one binary column, use it as Status
                updateSelectInput(session, "userSelected_Status", choices = binary_col_names, selected = binary_col_names[1])
              } else {
                hasBinaryColumns(TRUE)
                # If there are multiple binary columns, let the user choose
                  current_status <- input$userSelected_Status
                  if (!is.null(current_status) && current_status %in% binary_col_names) {
                  # Current selection is valid, do nothing
                  print("Current selection is already valid.")
                } else {
                  # Update because the current selection is not valid or not set
                  updateSelectInput(session, "userSelected_Status", choices = binary_col_names)
                }
              }
            })
          
         
          
          # -----------------------------------
#START HERE
          # Reactive expression for network plot
          observe({
              if( update_clicked() || !contour_plot_initial()){
                print("UPDATE IS CLICKED")
            print("I CAME TO 2")
            Final.DAG_network_plot_v6 <- Final.DAG_network_plot_v6 (augmented_edge_list,
                                                                    possible_seed_arcs_filter,
                                                                    data(), discretized_data,
                                                                    final_white_list(),
                                                                    Black_List(),
                                                                    input$nboot, cl,
                                                                    corrcoef,
                                                                    input$userSelected_Status,
                                                                    input$userSelected_key_feature)
            
        
         
          Alg.Count_arcs.strength.table <- Final.DAG_network_plot_v6$Alg.Count_arcs.strength.table
          
          plots_list(Final.DAG_network_plot_v6$plots_list)
          # plots_list(Final.DAG_network_plot_v6$plots_list)
          # DAG.Plot <- Final.DAG_network_plot_v6$DAG.Plot

          output$DAG.Plot <- renderPlot({
            # Final.DAG_network_plot_v6$DAG.Plot
            Final.DAG_network_plot_v6$plotFunction()
          })
          

          # -----------------
          arcs <- Final.DAG_network_plot_v6$arcs.BRCA
          P_strength <- Final.DAG_network_plot_v6$P_strength
          
          arc_slopes.strength <- Final.DAG_network_plot_v6$arc_slopes.strength
          
          output$Diagnostic_plot <- renderPlot({
            Diagnostic_plot.v3(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold)
            
          })
          
          output$Plot.Algorithm.Count_arcs.strength <- renderPlot({
            plot_Algorithm.Count_arcs.strength(Alg.Count_arcs.strength.table)
          })

          # DAG Network Plot
          output$DAGNetworkPlot <- renderVisNetwork({
            network <- Final.DAG_network_plot_v6$network
          })
          
          # arc_slopes_strength <- Final.DAG_network_plot_v6$arc_slopes.strength
          final_DAG_detail <- Final.DAG_network_plot_v6$final_DAG_detail
          
          output$arc_slopes_strength <- renderStyledTable(final_DAG_detail, rownames = TRUE, download_version = c('csv', 'excel'))



  print("I got in the contour plot!!")


  output$contour_plot <- renderPlot({
    print("I AM GROOT")
  # Ensure the user has selected a key feature and plots_list is not null
  # req(input$selectedCellType, !is.null(plots_list()))

  if(hasBinaryColumns()){
  
  showModal(modalDialog(
  tags$div(
    style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-radius: 5px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.1);  margin: auto;",
    tags$p(
      tags$span(
        style = "color: #2980b9;", 
        icon("sync", style = "margin-right: 6px; color: #2980b9; animation: spin 2s linear infinite;"), 
        tags$span(style = "font-weight: bold;", "Generating Contour Plot: ")
      ),
      "Please wait, we're processing the comparative analysis.."
    )
  ),
  footer = NULL,  # No footer as requested
  easyClose = FALSE,  # Prevent closing by clicking outside the modal
  size = "s"  # Small size, but the width is adjusted using custom styles
))
  }
  print("I got in the contour plot!! 2")

  # Access the user-selected key feature
  selectedCellType <- input$selectedCellType

  print("I got in the contour plot!! 3")


  # Debug: Print the selected key feature
  cat("Selected Cell type:", selectedCellType, "\n")
  cat("Selected scondary type:", input$userSelected_key_feature, "\n")


  print("plotslist")
  print( names(plots_list()))

  print("I got in the contour plot!! 4")


  # Debug: Check if the key feature exists in plots_list
  if(selectedCellType %in% names(plots_list())) {
    cat("selected cell type found in plots_list\n")
  } else {
    cat("selected cell type NOT found in plots_list\n")
  }

  # Assuming plots_list contains ggplot objects or similar
  # Render the plot corresponding to the selected key feature
  plot_to_render <- plots_list()[[selectedCellType]]
  if(!is.null(plot_to_render)) {
    print(plot_to_render)
  } else {
    cat("Plot for selected key feature is NULL\n")
  }
  if(hasBinaryColumns()){
    contour_plot_initial(TRUE)
     removeModal()
      # update_clicked(FALSE)
    shinyalert::shinyalert(
      title = "Process Completed",
      text = "The analysis based
       on your selections has been completed successfully!",
      type = "success"
    )
     shinyjs::delay(10000, update_clicked(FALSE))
  }
})}


})

# END HERE
        # --------------------------------------
        # Function to convert cycle to list of arcs using original vertex names
        cycle_to_arcs <- function(cycle, graph) {
          arc_list = c()
          for (i in 1:(length(cycle) - 1)) {
            arc_list = c(arc_list, paste(V(graph)[cycle[i]]$name, "->", V(graph)[cycle[i+1]]$name))
          }
          return(arc_list)
        }
        # --------------------------------------
        getEdgeNames <- function(graph) {
          ends <- get.edges(graph, E(graph))
          paste(V(graph)[ends[, 1]]$name, "->", V(graph)[ends[, 2]]$name)
        }
        # --------------------------------------
        edgeNamesToVertices <- function(graph, edge_names = NULL) {
          if(is.null(edge_names)) {
            edge_names <- getEdgeNames(graph)
          }
          
          edge_names <- as.character(edge_names)  # Ensure it's character
          
          lapply(edge_names, function(edge) {
            vertex_names <- unlist(strsplit(edge, " -> "))
            sapply(vertex_names, function(vname) {
              which(V(graph)$name == vname)
            })
          })
        }
        # --------------------------------------
        edgeNamesToIndices <- function(graph, edge_names = NULL) {
          if (is.null(edge_names)) {
            return(NULL)
          }
          e_names <- getEdgeNames(graph)
          indices <- which(e_names %in% edge_names)
          
          return(indices)
        }
        # -------------
      } else {
        # } else if (!data_present() & (!Black_List_present() | !White_List_present())) {
        # } else if ( !default_data_used() & !data_present() & (!Black_List_present() | !White_List_present())) {
        # } else if (!data_present || !Black_List_present || !White_List_present) {
        showModal(modalDialog(
          # ---------------------
          tags$div(
            style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
            tags$p(
              tags$span(
                style = "color: #d68910 ;", 
                icon("exclamation-triangle", style = "margin-right: 6px; color: #d68910 ;"), 
                tags$span(style = "font-weight: bold;", "Data Notice: ")
              ), 
              "Your data files have not been uploaded. Would you like to proceed with default files to learn about the process?"            )
          ),
          # ----------------
          footer = tagList(
            actionButton("ok", "Yes, Use Default", icon = icon("check-circle"), 
                         style = "background-color: #58d68d; color: black; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;"),
            tags$button("No, Upload My Data", type = "button", class = "btn btn-default shiny-modal-action-button", `data-dismiss`="modal", 
                        icon = icon("upload"), 
                        style = "background-color: #f1948a; color: black; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;")
          ),
          # ----------------
          easyClose = TRUE
        ))
      }
    }
  })
  # -------------------------------
  observeEvent(input$ok, {
    data_default <- read.csv(paste0(datapath, "Current.Data.csv"), header = TRUE)
    data(data_default)  # Set new value for data
    
    Black_List_default <- read.csv(paste0(datapath, "BlackList.csv"), header = TRUE)
    Black_List(Black_List_default)  # Set new value for data
    
    White_List_default <- read.csv(paste0(datapath, "WhiteList.csv"), header = TRUE)
    White_List(White_List_default)  # Set new value for data
    
    default_data_used(TRUE)
    
    removeModal()
    showModal(modalDialog(
      # ---------------------
      tags$div(
        style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
        tags$p(
          tags$span(
            style = "color: #d68910 ;", 
            icon("exclamation-triangle", style = "margin-right: 6px; color: #d68910;"), 
            tags$span(style = "font-weight: bold;", "Notification: ")
          ), 
          "Default files have been successfully loaded.", tags$br(), "Please hit the ", 
          tags$span(tags$span(style = "font-weight: bold; color:#2980B9;", "Run Discovery"), icon("fas fa-hand-pointer fa-rotate-180", style="margin-right: 4px;")), 
          " to proceed."      
        )
      ),
      # ----------------
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # ------------------------------- 
  # observe({
  # req(data())
  # # Detect binary columns
  # binary_cols <- sapply(data(), function(x) all(x %in% c(0, 1)))
  # binary_col_names <- names(binary_cols[binary_cols])
  # print("im in 3")
  # print(binary_col_names)
  # print(length(binary_col_names))

  # if (length(binary_col_names) == 1) {
  #   # If there's exactly one binary column, use it as Status
  #   updateSelectInput(session, "userSelected_Status", choices = binary_col_names, selected = binary_col_names[1])
  # } else {
  #   # If there are multiple binary columns, let the user choose
  #   updateSelectInput(session, "userSelected_Status", choices = binary_col_names)
  # }
  observe({
  req(data()) # Ensure a file is selected

  # Current selection
  current_status <- input$userSelected_Status


  binary_cols <- sapply(data(), function(x) all(x %in% c(0, 1)))
  binary_col_names <- names(binary_cols[binary_cols])
  print(binary_col_names)
  print(length(binary_col_names))
  
  print("hi im current status")
  print(current_status)

  userChangedStatus <- isTRUE(userSelected()) 


  if (length(binary_col_names) == 0) {
      hasBinaryColumns(FALSE)
   
  } else if (length(binary_col_names) == 1) {
      hasBinaryColumns(TRUE)

    # If there's exactly one binary column, use it as Status
    updateSelectInput(session, "userSelected_Status", choices = binary_col_names, selected = binary_col_names[1])
  } else {
      hasBinaryColumns(TRUE)

      # If there are multiple binary columns, let the user choose
        current_status <- input$userSelected_Status
        if (!is.null(current_status) && current_status %in% binary_col_names) {
        # Current selection is valid, do nothing
        print("Current selection is already valid.")
      } else {
        # Update because the current selection is not valid or not set
        updateSelectInput(session, "userSelected_Status", choices = binary_col_names)
      }
    }

  valid_features <- setdiff(names(data()), input$userSelected_Status)
  
  # Preserve current selection if still valid after update
  current_key_feature <- input$userSelected_key_feature
  if (!current_key_feature %in% valid_features && length(valid_features) > 0) {
    current_key_feature <- valid_features[1]
  }
  updateSelectInput(session, "userSelected_key_feature", choices = valid_features, selected = current_key_feature)
  
  valid_secondary_features <- setdiff(names(data()), c(input$userSelected_Status, current_key_feature))
  
  # Preserve current secondary selection if still valid after update
  current_secondary_selection <- input$selectedCellType
  if (!current_secondary_selection %in% valid_secondary_features && length(valid_secondary_features) > 0) {
    current_secondary_selection <- valid_secondary_features[1]
  }
  updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
})

  
  
#   observe({

#   req(final_white_list())

#   print("I got in the contour plot!!")
#   output$contour_plot <- renderPlot({
#   # Ensure the user has selected a key feature and plots_list is not null
#   req(input$selectedCellType, !is.null(plots_list()))

#   print("I got in the contour plot!! 2")

#   # Access the user-selected key feature
#   selectedCellType <- input$selectedCellType

#   print("I got in the contour plot!! 3")


#   # Debug: Print the selected key feature
#   cat("Selected Cell type:", selectedCellType, "\n")
#   cat("Selected scondary type:", input$userSelected_key_feature, "\n")


#   print("plotslist")
#   print( names(plots_list()))

#   print("I got in the contour plot!! 4")


#   # Debug: Check if the key feature exists in plots_list
#   if(selectedCellType %in% names(plots_list())) {
#     cat("selected cell type found in plots_list\n")
#   } else {
#     cat("selected cell type NOT found in plots_list\n")
#   }

#   # Assuming plots_list contains ggplot objects or similar
#   # Render the plot corresponding to the selected key feature
#   plot_to_render <- plots_list()[[selectedCellType]]
#   if(!is.null(plot_to_render)) {
#     print(plot_to_render)
#   } else {
#     cat("Plot for selected key feature is NULL\n")
#   }

#   shinyalert::shinyalert(
#     title = "Process Completed",
#     text = "The analysis based on your selections has been completed successfully.",
#     type = "success"
#   )
# })

#   })



  # -------------------------------
  observeEvent(input$close, {
    # Close modal if "Acknowledge" clicked
    removeModal()
  })
  # --------------------------
}
# -------------------------------
# finished_running(TRUE)
# 
# observeEvent(finished_running(), {
#   if (finished_running() == TRUE) {
#     content <- paste0(
#       "<div style='font-size:18px; color:#34495E; padding: 5px 10px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;'>",
#       "<p>",
#       "<span style='color: #FF0000;'>",
#       "<i class='fa fa-exclamation-triangle' style='margin-right: 6px; color: #FF0000;'></i>",
#       "<span style='font-weight: bold;'>Task Completed </span>",
#       "</span>",
#       "<br><br>",
#       "The app has finished running!",
#       "</p>",
#       "</div>"
#     )
#     shinyalert::shinyalert(
#       # title = "Proceed to Further Analysis",
#       text = content,
#       type = "success"
#     )
#   }
# })
# -------------------------------
shinyApp(ui, server)
