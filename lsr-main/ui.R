# UI
# ==============================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Living NMA Tool"),

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
            actionButton("go_to_help", "Open Help & User Guide", icon = icon("question-circle"))
          )
        )
      ),

      fluidRow(
        column(
          7,
          wellPanel(
            h4("What you can do"),
            tags$ul(
              tags$li(strong("Import data"), " by uploading a CSV or Excel file, or use the bundled example dataset."),
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
              tags$li("Upload study-level data (CSV/Excel) or use bundled data."),
              tags$li("Check data integrity and resolve input issues."),
              tags$li("Select the analysis specification (model type, effect model, reference treatment)."),
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
              "Core statistical methods are implemented via established R packages, while the application ",
              "provides an integrated workflow for practical use."
            )
          )
        )
      )
    ),

    # --------------- Data Overview ---------------
    tabPanel(
      "Data Overview",
      value = "data_overview",
      mod_data_ui("data")
    ),

    # --------------- Set up Analysis ---------------
    tabPanel(
      "Set up Analysis",
      value = "setup",

      fluidRow(
        column(4,
               radioButtons(
                 "cnma_model_type", "Model type:",
                 choices = c(
                   "Simple NMA"               = "simple",
                   "Additive (CNMA)"          = "additive",
                   "Interaction (CNMA)"       = "interaction"
                 ),
                 selected = "simple"
               ),
               verbatimTextOutput("model_warning")
        ),
        column(4,
               radioButtons("model_type", "Effect model:",
                            choices = c("Random Effects" = "random",
                                        "Fixed Effect"   = "fixed"),
                            selected = "random"
               )
        ),
        column(4,
               selectInput("reference_treatment", "Reference treatment:",
                           choices = c("Auto (alphabetical)" = ""),
                           selected = ""),
               helpText("Select the comparator for forest plots and league tables.")
        )
      ),

      # --- Node Merging ---
      hr(),
      fluidRow(
        column(12,
               mod_merge_ui("merge")
        )
      ),

      br(), br(),
      actionButton("run_nma", "Run Analysis", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("nma_status"),
      br()
    ),

    # --------------- Plots ---------------
    tabPanel(
      "Plots",
      mod_results_plots_ui("results")
    ),

    # --------------- Summary ---------------
    tabPanel(
      "Summary",
      value = "summary",
      mod_results_summary_ui("results")
    ),

    # --------------- Funnel Plot ---------------
    tabPanel(
      "Funnel Plot",
      value = "funnel",
      mod_results_funnel_ui("results")
    ),

    # --------------- Inconsistency ---------------
    tabPanel(
      "Inconsistency",
      value = "inconsistency",
      mod_results_inconsistency_ui("results")
    ),

    # --------------- Ranking ---------------
    tabPanel(
      "Ranking",
      value = "ranking",
      mod_results_ranking_ui("results")
    ),

    # --------------- Help ---------------
    tabPanel(
      "Help",
      value = "help",
      fluidRow(
        column(10,
               h3("Help & Documentation"),
               h4("Data Format"),
               p("The tool accepts ", strong("pairwise contrast-level data"), " with these columns:"),
               tags$ul(
                 tags$li(code("study"), " \u2014 Study identifier"),
                 tags$li(code("treat1"), " \u2014 Treatment in arm 1"),
                 tags$li(code("treat2"), " \u2014 Treatment in arm 2"),
                 tags$li(code("logHR"), " \u2014 Log hazard ratio"),
                 tags$li(code("selogHR"), " \u2014 Standard error of log(HR)")
               ),
               p("Alternatively, ", strong("arm-level data"), " with columns ",
                 code("study"), ", ", code("treatment"), ", ", code("mean"), ", ",
                 code("sd"), ", ", code("n"), " is automatically converted to pairwise format."),
               hr(),
               h4("Analysis Models"),
               tags$ul(
                 tags$li(strong("Simple NMA"), " \u2014 Classical network meta-analysis via ", code("netmeta()")),
                 tags$li(strong("Additive CNMA"), " \u2014 Component NMA assuming additive effects via ", code("netcomb()")),
                 tags$li(strong("Interaction CNMA"), " \u2014 Component NMA with 2-way interaction terms")
               ),
               hr(),
               h4("Node Merging"),
               p("Select \u2265 2 components and provide a new node name. The merge uses ",
                 "inverse-variance weighted pooling. Self-loops are prevented automatically."),
               hr(),
               h4("Reference Treatment"),
               p("Choose a reference treatment for the analysis. All comparisons in the forest plot ",
                 "and league table will be relative to this treatment. Leave as 'Auto' for alphabetical order."),
               hr(),
               p("Contact: charalampos.damianidis@gmail.com"),
               p("Source code and instructions available on ",
                 tags$a(href = "https://github.com/ch-damianidis/lsr", "GitHub"), ".")
        )
      )
    ),

    # --------------- Export ---------------
    tabPanel(
      "Export",
      value = "export",
      mod_export_ui("export")
    )
  )
)
