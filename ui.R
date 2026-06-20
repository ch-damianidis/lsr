# UI
# ==============================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Living NMA Tool "),

  tabsetPanel(id = "main_tabs",
              # ---------------- Home ----------------

              tabPanel(
                "Home",
                value = "home",

                fluidRow(
                  column(
                    12,
                    wellPanel(
                      h2("Living Network Meta-Analysis Tool"),
                      p(
                        "This Shiny application supports reproducible and updatable evidence synthesis ",
                        "by streamlining a frequentist network meta-analysis (NMA) workflow."
                      ),
                      p(
                        "It is designed to help you move from structured study-level data to interpretable ",
                        "network estimates and diagnostics in a consistent, repeatable way."
                      ),
                      br(),
                      actionButton("go_to_help", "Open Help & User Guide", icon = icon("question-circle")),
                      tags$span(style = "margin-left:10px;"),

                    )
                  )
                ),

                fluidRow(
                  column(
                    7,
                    wellPanel(
                      h4("What you can do"),
                      tags$ul(
                        tags$li(strong("Import data"), " from REDCap (when configured) or from an uploaded CSV file."),
                        tags$li(strong("Validate and prepare"), " the dataset (basic checks, required fields, and formatting)."),
                        tags$li(strong("Fit NMA models"), " using the ", code("netmeta"), " package (frequentist framework)."),
                        tags$li(strong("Explore the network"), " structure and treatment comparisons with standard plots/tables."),
                        tags$li(strong("Update analyses"), " when new studies become available, preserving a consistent pipeline.")
                      ),
                      p(
                        em("Note: The exact available outputs depend on the selected analysis options and the uploaded dataset.")
                      )
                    )
                  ),

                  column(
                    5,
                    wellPanel(
                      h4("Typical workflow"),
                      tags$ol(
                        tags$li("Load or fetch study-level data (REDCap/CSV)."),
                        tags$li("Check data integrity and resolve input issues."),
                        tags$li("Select the analysis specification (effect measure, model settings)."),
                        tags$li("Run the analysis and inspect estimates, heterogeneity, and diagnostics."),
                        tags$li("Export key results for reporting and reproducibility.")
                      )
                    )
                  )
                ),

                fluidRow(
                  column(
                    12,
                    wellPanel(
                      h4("About this project"),
                      p(
                        "This application was developed as part of an MSc thesis in Health Statistics and Data Analytics ",
                        "at Aristotle University of Thessaloniki."
                      ),
                      p(
                        "The project focuses on operationalizing a living NMA pipeline in a user-facing interface, ",
                        "with emphasis on reproducibility, transparency of analysis choices, and rapid re-analysis ",
                        "as new evidence accumulates."
                      ),
                      p(
                        "Core statistical methods are implemented via established R packages, while the application provides an integrated workflow for practical use."
                      )
                    )
                  )
                )
              )
              ,
              # --------------- Data Overview ---------------
              tabPanel("Data Overview",
                       value = "data_overview",
                       fluidRow(
                         column(12,
                                h3("Data Preview"),
                                DT::dataTableOutput("data_preview"),
                                br(),
                                h4("Data Summary"),
                                # Shows summary stats (studies, pairs/arms, treatments, missingness)
                                verbatimTextOutput("data_summary"),
                                br(),
                                # Message about input layout (arm-level vs pairwise)
                                verbatimTextOutput("data_validation_msg"),
                                br()

                         )
                       )
              ),
              # --------------- Set up Analysis ---------------
              tabPanel("Set up Analysis",
                       value = "setup",
                       fluidRow(
                         column(6,
                                radioButtons(
                                  "cnma_model_type", "Model type:",
                                  choices = c(
                                    "Simple NMA"               = "simple",
                                    "Additive (CNMA)"          = "additive",
                                    "Interaction (CNMA)"       = "interaction"
                                  ),
                                  selected = "simple"
                                ),
                                # Warning text if Interaction is requested on a disconnected network
                                verbatimTextOutput("model_warning")
                         ),
                         column(6,
                                radioButtons("model_type", "Effect model:",
                                             choices = c("Random Effects" = "random",
                                                         "Fixed Effect"   = "fixed"),
                                             selected = "random"
                                )
                         )
                       ),

                       # --- Node Merging (by components) ---
                       hr(),
                       fluidRow(
                         column(12,
                                h4("Node Merging"),
                                selectizeInput("components_to_merge",
                                               "Select components to merge:",
                                               choices = NULL, multiple = TRUE,
                                               # keep ASCII to avoid encoding issues
                                               options = list(placeholder = "Choose >= 2 components")),
                                textInput("new_component_name", "New node name:", ""),
                                actionButton("apply_merge_components", "Apply Node Merge"),
                                actionButton("reset_data", "Reset to Original Data", icon = icon("refresh"))
                         )
                       ),
                       br(), br(),
                       actionButton("run_nma", "Run Analysis"),
                       br(), br(),
                       verbatimTextOutput("nma_status"),
                       br()

              ),



              # --------------- Plots ---------------
              tabPanel("Plots",
                       tabsetPanel(
                         tabPanel("Network Plot", plotOutput("network_plot", height = "600px", width = "100%")),
                         tabPanel("Forest Plot", plotOutput("forest_plot"))
                       )
              ),
            # --------------- Summary ---------------
            tabPanel("Summary",
                     value = "summary",
                     fluidRow(
                       column(12,
                              h3("Model Summary"),
                              verbatimTextOutput("nma_summary")
                       )
                     )
            ),
            # --------------- Funnel Plot ---------------
              tabPanel("Funnel Plot",
                       value = "funnel",
                       fluidRow(
                         column(12,
                                h3("Funnel Plot for Publication Bias"),
                                plotOutput("funnel_plot")
                         )
                       )
              ),
            # --------------- Inconsistency ---------------
              tabPanel("Inconsistency",
                       value = "inconsistency",
                       tabsetPanel(id = "inconsistency_tabs",
                                   tabPanel("Local (Node-splitting)",
                                            br(),
                                            h4("Node-Splitting: Direct vs Indirect Comparison"),
                                            tableOutput("netsplit_table")
                                   ),
                                   tabPanel("Global (Design-by-Treatment)",
                                            br(),
                                            h4("Design-by-Treatment Interaction Model"),
                                            tableOutput("decomp_table")
                                   )
                       )
              ),
            # --------------- Ranking ---------------
            tabPanel(
              "Ranking",
              value = "ranking",
              # League table (top)
              fluidRow(
                column(
                  12,
                  h4("League Table"),
                  div(style = "overflow-x:auto;", tableOutput("league_table"))
                )
              ),
              hr(),
              # Ranking table (below)
              fluidRow(
                column(
                  12,
                  h4("Ranking Table"),
                  div(style = "overflow-x:auto;", tableOutput("ranking_table"))
                )
              )
            )

              ,
            # --------------- Help ---------------
              tabPanel("Help",
                       value = "help",
                       fluidRow(
                         column(10,
                                h3("Help & Documentation"),
                                p("This tool implements network meta-analysis using the netmeta package."),
                                p("Navigate using the tabs above to view network plots, model summaries, funnel plots, and ranking outputs."),
                                p("Models use log(HR) data and standard errors, assuming appropriate study structure."),
                                p("About Node Merging: "),
                                p("To perform node merging, you must **select all studies that contain the components you wish to merge**. Only the selected studies will be included in the merging process. Please ensure that all relevant studies are selected for a valid merge."),
                                p("Contact: charalampos.damianidis@gmail.com"),
                                br(),
                                p("Source code and instructions available on GitHub.")
                         )
                       )
              ),
            tabPanel("Export",
                     value = "export",
                     fluidRow(
                       column(4,
                              selectInput("report_format", "Report format:",
                                          choices = c("Word (docx)" = "word",
                                                      "HTML" = "html")),
                              actionButton("export_report", "Generate Report")
                       ),
                       column(8,
                              downloadButton("download_report", "Download report")

                       ),
                       br(), br(),
                       h4("History of generated reports"),
                       actionButton("refresh_history", "Clear"),
                       DT::dataTableOutput("report_history")

                     )
            )

  )
)
