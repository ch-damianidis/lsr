# ==============================
# Libraries
# ==============================
library(shiny)
library(netmeta)
library(shinythemes)
library(DT)
library(igraph)
library(meta)
library(ggplot2)
library(dplyr)

# ==============================
# Project sources (helpers)
# ==============================
source("utils/netmeta_pipeline.R")
source("utils/redcap_connect.R")
source("utils/data_processing.R")




# ==============================
# UI
# ==============================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Living NMA Tool (Demo Version"),
  
  tabsetPanel(id = "main_tabs",
              # ---------------- Home ----------------
              
              tabPanel("Home",                
                       value = "home",
                       fluidRow(
                         column(8,
                                h3("Welcome to the Living Network Meta-Analysis Tool"),
                                p("This tool allows you to perform and explore a living network meta-analysis."),
                                p("You can analyze data from REDCap or uploaded CSV files."),
                                br(),
                                actionButton("go_to_help", "Help", icon = icon("question-circle")),
                                br(), br()
                         )
                       ),
                       fluidRow(
                         column(6,
                                wellPanel(
                                  h4("About this Project"),
                                  p("This Shiny application was developed as part of an MSc thesis in Health Statistics and Data Analytics at Aristotle University of Thessaloniki."),
                                  p("Implements a Living Network Meta-Analysis (NMA) using data from REDCap and automates the frequentist NMA pipeline via the R package netmeta."),
                                  p("The objective is to facilitate reproducible and updatable evidence synthesis in clinical research.")
                                )
                         ),
                         column(6,
                                wellPanel(
                                  h4("References"),
                                  tags$ul(
                                    tags$li("__________________________ "),
                                    tags$li("________________________-"),
                                    tags$li("_______________________")
                                  )
                                )
                         )
                       )
              ),
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

# ==============================
# Server
# ==============================
server <- function(input, output, session) {
  # ---- Persistent History Log (CSV on disk) ----
  history_file <- "history_log.csv"
  
  if (file.exists(history_file)) {
    history_data <- read.csv(history_file, stringsAsFactors = FALSE)
  } else {
    history_data <- data.frame(
      time = character(),
      format = character(),
      n_studies = numeric(),
      n_treatments = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  history_log <- reactiveVal(history_data)
  ## ----------------------------------
  
  
  # Original inputs as loaded once, for the Data Overview (arm- or pairwise)
  original_data    <- reactiveVal(NULL)   
  # Pairwise baseline kept for reset (used by node merge reset)
  original_pairwise <- reactiveVal(NULL)  
  
  #------
  
  
  
  
  # ---- Analysis options are "committed" when user presses Run Analysis ----
  # We keep the chosen model type/effect model in a stable list so plots/tables
  # use a consistent snapshot rather than continuously reacting to radioButtons.
  
  analysis_opts <- eventReactive(input$run_nma, {
    list(
      cnma_model_type = input$cnma_model_type,
      model_type      = input$model_type
    )
  }, ignoreInit = TRUE)
  
  ###
  
  
  # Working dataset (pairwise). This is the dataset affected by Node Merging.
  raw_data <- reactiveVal(NULL)      
  
  # Keep component choices in sync with current working data
  observe({
    req(raw_data())
    d <- raw_data()
    labs <- unique(c(as.character(d$treat1), as.character(d$treat2)))
    comps <- sort(unique(trimws(unlist(strsplit(labs, "\\s*\\+\\s*")))))
    updateSelectizeInput(session, "components_to_merge",
                         choices = comps, server = TRUE)
  })
  
  
  # ---- Initial data load + identify layout (arm-level or pairwise) ----
  
  observe({
    if (is.null(original_data())) {
      data_raw <- get_data_from_redcap()       # load once
      original_data(data_raw)
      
      # Tell the user what the incoming structure is
      required_cols_arm <- c("study", "treatment", "mean", "sd", "n")
      if (all(required_cols_arm %in% colnames(data_raw))) {
        output$data_validation_msg <- renderText(
          "Arm-level data (original). Data will be converted to pairwise when you click Run Analysis."
        )
      } else {
        output$data_validation_msg <- renderText("Pairwise data (original).")
      }
      
      # Build baseline pairwise and set working dataset
      original_pairwise(convert_to_pairwise(data_raw))
      raw_data(original_pairwise())
    }
  })
  
  
  
  
  
  # ---- Run Analysis button (status message only; models build in reactives) ----
  observeEvent(input$run_nma, {
    output$nma_status <- renderText("Analysis in progress...")
    req(raw_data())  # ensure working dataset exists
    output$nma_status <- renderText("Analysis completed.")
  })
  
  
  
  # ---- Warn if interaction requested on disconnected network ----
  output$model_warning <- renderText({
    req(raw_data(), analysis_opts())
    interaction_chosen <- analysis_opts()$cnma_model_type == "interaction"
    d <- raw_data()
    g <- igraph::graph_from_data_frame(
      data.frame(from = d$treat1, to = d$treat2, stringsAsFactors = FALSE),
      directed = FALSE
    )
    if (interaction_chosen && !igraph::is.connected(g)) {
      return("Interaction model is not available for disconnected networks. Only additive or simple NMA can be run.")
    }
    NULL
  })
  
  
  
  # ---- CNMA/NMA model (main model shown in forest/league; includes CNMA) ----
  cnma_model <- reactive({
    req(raw_data(), analysis_opts())
    model_kind <- analysis_opts()$cnma_model_type
    use_random <- analysis_opts()$model_type == "random"
    
    if (model_kind == "simple") {
      # Plain netmeta
      nma <- tryCatch({
        netmeta::netmeta(
          TE      = raw_data()$logHR,
          seTE    = raw_data()$selogHR,
          treat1  = raw_data()$treat1,
          treat2  = raw_data()$treat2,
          studlab = raw_data()$study,
          sm      = "HR",
          random  = use_random
        )
      }, error = function(e) NULL)
      # Store P-scores if available
      if (!is.null(nma)) {
        nma$pscore <- tryCatch(
          netmeta::netrank(nma, small.values = "desirable")$p.score,
          error = function(e) NULL
        )
      }
      return(nma)
    }
    # CNMA path (additive or interaction; discomb if disconnected)
    interaction_flag <- (model_kind == "interaction")
    nma <- tryCatch({
      run_cnma_analysis(raw_data(), interaction = interaction_flag, random = use_random)
    }, error = function(e) NULL)
    
    if (!is.null(nma)) {
      nma$pscore <- tryCatch(
        netmeta::netrank(nma, small.values = "desirable")$p.score,
        error = function(e) NULL
      )
    }
    nma
  })
  
  
  
  # ---- Treatment-level netmeta (used in network plot, funnel, inconsistency) ----
  nm_model <- reactive({
    req(raw_data(), analysis_opts())
    tryCatch({
      build_nm(raw_data(), random = (analysis_opts()$model_type == "random"))
    }, error = function(e) NULL)
  })
  
  
  
  # ---- Data preview output (original, not modified by merges) ----
  output$data_preview <- DT::renderDataTable({
    req(original_data())
    DT::datatable(head(original_data(), 20), options = list(pageLength = 10))
  })
  
  # ---- Data summary (original dataset) ----
  output$data_summary <- renderPrint({
    req(original_data())
    summ <- summarize_data(original_data())  
    cat("Number of studies: ",    summ$n_studies,    "\n")
    cat("Number of arms/rows: ",  summ$n_arms,       "\n")
    cat("Number of treatments: ", summ$n_treatments, "\n")
    cat("Percent missing data: ", summ$missing_percent, "%\n")
  })
  
  # ---- Network plot (uses treatment-level netmeta) ----
  output$network_plot <- renderPlot({
    req(analysis_opts())
    nm <- nm_model()
    validate(need(!is.null(nm), "Run the analysis first to build the network."))
    
    tryCatch({
      netmeta::netgraph(
        nm,
        number.of.studies = TRUE,
        thickness         = "number.of.studies",
        multiarm          = TRUE,
        points            = TRUE,
        cex.points        = 4,
        cex               = 0.8,
        plastic           = FALSE,
        seq               = sort(nm$trts)
        # lwd = 0.9  # example thickness scale
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Network plot not available:", conditionMessage(e)), cex = 1.1)
    })
  }, res = 144)
  
  
  
  
  
  # ---- Forest plot (uses main CNMA/NMA object) ----
  output$forest_plot <- renderPlot({
    nma <- cnma_model()
    validate(need(!is.null(nma), "Model is not available."))
    tryCatch(
      forest(nma),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, "Forest plot not available", cex = 1.2)
      }
    )
  })
  # ---- Node Merging logic (component labels > new node) ----
  observeEvent(input$apply_merge_components, {
    req(raw_data())
    
    comps <- input$components_to_merge
    newnm <- trimws(input$new_component_name)
    
    validate(
      need(length(comps) >= 2, "Select at least two components to merge."),
      need(nchar(newnm) > 0,   "Provide a new node name.")
    )
    
    d <- raw_data()
    
    # Replace any selected components inside labels like "A + B + C" with the new name
    replace_comps <- function(lbl, comps, newnm) {
      parts <- trimws(unlist(strsplit(as.character(lbl), "\\s*\\+\\s*")))
      parts[parts %in% comps] <- newnm
      parts <- unique(parts)
      paste(parts, collapse = " + ")
    }
    
    # Apply replacement to both sides of each pair
    d <- d %>%
      mutate(
        treat1_new = vapply(treat1, replace_comps, FUN.VALUE = character(1),
                            comps = comps, newnm = newnm),
        treat2_new = vapply(treat2, replace_comps, FUN.VALUE = character(1),
                            comps = comps, newnm = newnm)
      )
    # Warn if any study becomes single-arm after merging (not analyzable)
    df_trts <- dplyr::bind_rows(
      dplyr::select(d, study) %>% dplyr::mutate(trt = d$treat1_new),
      dplyr::select(d, study) %>% dplyr::mutate(trt = d$treat2_new)
    )
    
    single_arm_tbl <- df_trts %>%
      dplyr::group_by(study) %>%
      dplyr::summarise(n_unique_trts = dplyr::n_distinct(trt), .groups = "drop") %>%
      dplyr::filter(n_unique_trts <= 1)
    
    if (nrow(single_arm_tbl) > 0) {
      showNotification(
        paste0(
          "WARNING: After this merge, the following studies become single-arm: ",
          paste(single_arm_tbl$study, collapse = ", "),
          ". Single-arm studies are not analyzable in NMA. Please review the selected components or the new node name."
        ),
        type = "warning", duration = 10
      )
    }
    
    # Prevent A vs A (self-loops) after merging
    if (any(d$treat1_new == d$treat2_new)) {
      showNotification("Self-loop created by merging. Change selection/name and try again.",
                       type = "warning", duration = 6)
      return()
    }
    
    # Normalize pair direction per study & pool duplicate comparisons (weights = 1/se^2)

    pooled <- d %>%
      transmute(
        study,
        t1c   = pmin(treat1_new, treat2_new),
        t2c   = pmax(treat1_new, treat2_new),
        TEc   = ifelse(treat1_new <= treat2_new, logHR, -logHR),
        w     = 1 / (selogHR^2)
      ) %>%
      group_by(study, t1c, t2c) %>%
      summarise(
        logHR   = sum(w * TEc) / sum(w),
        selogHR = sqrt(1 / sum(w)),
        .groups = "drop"
      ) %>%
      rename(treat1 = t1c, treat2 = t2c)
    
    # Commit merged dataset as new working data
    raw_data(pooled)
    
    showNotification(
      paste0("Merged components {", paste(comps, collapse = ", "),
             "} into node '", newnm, "'."),
      type = "message", duration = 6
    )
  })
  # Reset working data to baseline pairwise
  observeEvent(input$reset_data, {
    req(original_pairwise())
    raw_data(original_pairwise())
    showNotification("Working data reset to the original pairwise dataset.", type = "message", duration = 4)
  })
  

  
  
  # ---- League Table (handles CNMA/netmeta objects) ----
  output$league_table <- renderTable({
    req(analysis_opts())
    fit <- cnma_model()
    dat <- raw_data()
    validate(need(!is.null(fit), "Run the analysis first."))
    validate(need(!is.null(dat), "No data available."))
    
    edges <- data.frame(from = dat$treat1, to = dat$treat2, stringsAsFactors = FALSE)
    g <- igraph::graph_from_data_frame(edges, directed = FALSE)
    if (!igraph::is.connected(g) || length(unique(c(dat$treat1, dat$treat2))) < 3) {
      return(data.frame(Message = "League table not applicable to this network"))
    }
    
    use_random <- (analysis_opts()$model_type == "random")
    use_common <- (analysis_opts()$model_type == "fixed")
    
    is_netcomb <- inherits(fit, "netcomb")
    is_discomb <- inherits(fit, "discomb")
    
    lt <- tryCatch({
      if (is_netcomb || is_discomb) {
        netmeta::netleague(
          fit,
          common     = use_common,
          random     = use_random,
          ci         = TRUE,
          backtransf = TRUE,
          bracket    = "(",
          separator  = " - ",
          direct     = FALSE
        )
      } else {
        netmeta::netleague(
          fit,
          common     = use_common,
          random     = use_random,
          ci         = TRUE,
          backtransf = TRUE,
          bracket    = "(",
          separator  = " - "
        )
      }
    }, error = function(e) e)
    
    if (inherits(lt, "error")) {
      return(data.frame(Message = paste("Failed to generate league table:", conditionMessage(lt))))
    }
    
    mat <- if (use_random && !is.null(lt$random)) {
      lt$random
    } else if (use_common && !is.null(lt$common)) {
      lt$common
    } else if (!is.null(lt$random)) {
      lt$random
    } else {
      lt$common
    }
    
    if (is.null(mat)) {
      return(data.frame(Message = "Requested effect model table is unavailable in this league object."))
    }
    
    as.data.frame(mat, stringsAsFactors = FALSE)
  }, rownames = TRUE)
  
  
  
  
  
  
  
  
  # ---- Ranking table (P-score, treatment-level) ----
  output$ranking_table <- renderTable({
    req(analysis_opts())
    nm  <- nm_model()
    dat <- raw_data()
    validate(need(!is.null(nm),  "Run the analysis first."))
    validate(need(!is.null(dat), "No data available."))
    
    edges <- data.frame(from = dat$treat1, to = dat$treat2, stringsAsFactors = FALSE)
    g <- igraph::graph_from_data_frame(edges, directed = FALSE)
    if (!igraph::is.connected(g) || length(nm$trts) < 3) {
      return(data.frame(Message = "Ranking not applicable to this network"))
    }
    
    ps <- tryCatch(
      netmeta::netrank(
        nm,
        small.values = "desirable",
        method       = "P-score"
      ),
      error = function(e) e
    )
    if (inherits(ps, "error")) {
      return(data.frame(Message = paste("Failed to calculate treatment ranking:", conditionMessage(ps))))
    }
    
    use_random <- (analysis_opts()$model_type == "random")
    
    ranking_vec <- if (use_random && !is.null(ps$ranking.random)) {
      ps$ranking.random
    } else if (!use_random && !is.null(ps$ranking.common)) {
      ps$ranking.common
    } else if (!is.null(ps$ranking.random)) {
      ps$ranking.random
    } else {
      ps$ranking.common
    }
    
    if (is.null(ranking_vec)) {
      return(data.frame(Message = "Ranking vector is unavailable in this netmeta version."))
    }
    
    df <- data.frame(
      Treatment = names(ranking_vec),
      P_score   = round(as.numeric(ranking_vec), 3),
      row.names = NULL
    )
    df[order(-df$P_score), , drop = FALSE]
  }, rownames = FALSE)
  
  
  
  
  
  
  
  # ---- Node-splitting (local inconsistency) ----
  output$netsplit_table <- renderTable({
    req(analysis_opts())
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    
    ns <- netmeta::netsplit(nm, random = TRUE, common = FALSE, backtransf = TRUE)
    
    dir <- ns$direct.random[,   c("comparison","TE","lower","upper","p")]
    ind <- ns$indirect.random[, c("comparison","TE","lower","upper","p")]
    cmp <- ns$compare.random[,  c("comparison","z","p")]
    
    res <- merge(dir, ind, by = "comparison", all = TRUE, suffixes = c(".direct",".indirect"))
    res <- merge(res, cmp, by = "comparison", all = TRUE)
    
    res$Q  <- res$z^2
    res$df <- 1L
    
    names(res)[names(res)=="TE.direct"]   <- "Direct"
    names(res)[names(res)=="TE.indirect"] <- "Indirect"
    names(res)[names(res)=="p.x"]         <- "p.direct"
    names(res)[names(res)=="p.y"]         <- "p.indirect"
    names(res)[names(res)=="p"]           <- "p.diff"
    
    out <- res[, c(
      "comparison","Direct","Indirect",
      "lower.direct","upper.direct",
      "lower.indirect","upper.indirect",
      "z","Q","df","p.diff"
    )]
    
    numcols <- c(
      "Direct","Indirect","lower.direct","upper.direct",
      "lower.indirect","upper.indirect","z","Q","p.diff"
    )
    out[numcols] <- lapply(out[numcols], function(x) if (is.numeric(x)) round(x, 3) else x)
    
    out[order(out$comparison), ]
  }, rownames = FALSE)
  
  
  
  
  # ---- Design-by-Treatment (global inconsistency) ----
  output$decomp_table <- renderTable({
    req(analysis_opts())
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    tryCatch({
      decomp <- netmeta::decomp.design(nm)
      round(decomp$Q.decomp, 3)
    }, error = function(e) data.frame(Error = "Global inconsistency analysis failed"))
  }, rownames = TRUE)
  
  
  
  # ---- Funnel plot (comparison-adjusted funnel, if available) ----
  output$funnel_plot <- renderPlot({
    req(analysis_opts())
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    
    if (length(unique(nm$studlab)) < 5) {
      plot.new()
      text(0.5, 0.5, "Not enough studies for a funnel plot", cex = 1.2)
      return()
    }
    
    if ("netfunnel" %in% ls(getNamespace("netmeta"))) {
      netmeta::netfunnel(
        nm,
        order = sort(nm$trts),
        xlab = "Comparison-adjusted effect size",
        contour = TRUE,
        contour.levels = c(0.9, 0.95, 0.99),
        legend.pos = "bottomright"
      )
    } else {
      netmeta:::funnel.netmeta(
        nm,
        order = sort(nm$trts),
        xlab = "Comparison-adjusted effect size",
        contour = TRUE,
        contour.levels = c(0.9, 0.95, 0.99),
        legend.pos = "bottomright"
      )
    }
  })
  
  
  
  
  # ---- Generate report (writes to temp, logs in history) ----
  
  observeEvent(input$export_report, {
    
    # Update on-screen history and persist to CSV
    hist <- history_log()
    summ <- summarize_data(raw_data())
    new_row <- data.frame(
      time = as.character(Sys.time()),
      format = input$report_format,
      n_studies = summ$n_studies,
      n_treatments = summ$n_treatments
    )
    updated <- rbind(hist, new_row)
    write.csv(updated, history_file, row.names = FALSE)
    history_log(updated)
    
    
    # Render to a temporary file; actual download happens via downloadHandler
    tmpFile <- tempfile(fileext = switch(input$report_format,
                                         word = ".docx",
                                         html = ".html"))
    
    
    rmarkdown::render(
      input = "reports/report_template.Rmd",
      output_format = switch(input$report_format,
                             word = "word_document",
                             html = "html_document"),
      output_file = tmpFile,
      params = list(
        nma      = cnma_model(),
        raw_data = raw_data()
      )
      
    )
    
    
  })
  # ---- Clear history table & CSV ----
  observeEvent(input$refresh_history, {
    
    empty_df <- data.frame(
      time = character(),
      format = character(),
      n_studies = numeric(),
      n_treatments = numeric(),
      stringsAsFactors = FALSE
    )
    
    
    history_log(empty_df)
    
    # overwrite history file
    write.csv(empty_df, history_file, row.names = FALSE)
  })
  
  # Render history table
  output$report_history <- DT::renderDataTable({
    history_log()
  })
  
  # ---- Download handler (fresh render to 'file') ----
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".", switch(input$report_format,
                                                word = "docx",
                                                html = "html"))
    },
    content = function(file) {
      rmarkdown::render(
        input = "reports/report_template.Rmd",
        output_format = switch(input$report_format,
                               word = "word_document",
                               html = "html_document"),
        output_file = file,
        params = list(
          nma      = cnma_model(),
          raw_data = raw_data()
        )
      )
    }
  )
  
  
  
  
  
  # ---- Navigation: Home > Help button ----
  observeEvent(input$go_to_help, {
    updateTabsetPanel(session, "main_tabs", selected = "help")
  })
  
  # ---- Textual model summary (with interaction-term details if present) ----
  output$nma_summary <- renderPrint({
    req(cnma_model(), analysis_opts())
    nma <- cnma_model()
    
    forced_add <- isTRUE(attr(nma, "forced_additive"))
    use_common <- (analysis_opts()$model_type == "fixed")
    use_random <- (analysis_opts()$model_type == "random")
    
    cat("========== Model Summary ==========\n")
    cat(
      "Model type: ",
      switch(
        analysis_opts()$cnma_model_type,
        "simple"      = "Simple NMA",
        "additive"    = "Additive CNMA",
        "interaction" = if (forced_add)
          "Interaction requested → Additive CNMA (disconnected network)"
        else
          "Interaction CNMA"
      ),
      "\nEffect type: ",
      if (use_random) "Random effects" else "Fixed effect",
      "\n=================================\n\n",
      sep = ""
    )
    
    s_bt <- tryCatch(
      summary(nma, common = use_common, random = use_random, backtransf = TRUE),
      error = function(e) summary(nma, common = use_common, random = use_random, backtransf = FALSE)
    )
    print(s_bt)
    
    
    # If interaction model successfully ran (netcomb with interactions),
    # print raw interaction terms in both logHR and HR scale.
    if (analysis_opts()$cnma_model_type == "interaction" && inherits(nma, "netcomb") && !forced_add) {
      s_raw <- tryCatch(
        summary(nma, common = use_common, random = use_random, backtransf = FALSE),
        error = function(e) NULL
      )
      if (!is.null(s_raw)) {
        comps <- if (use_common && !is.null(s_raw$components.common)) s_raw$components.common else s_raw$components.random
        if (!is.null(comps) && nrow(comps) > 0) {
          ia_sep <- if (!is.null(nma$sep.ia)) nma$sep.ia else " x "
          ia_idx <- grepl(paste0("\\Q", ia_sep, "\\E"), rownames(comps))
          if (any(ia_idx)) {
            TE    <- suppressWarnings(as.numeric(comps$TE[ia_idx]))
            se    <- suppressWarnings(as.numeric(comps$seTE[ia_idx]))
            pval  <- suppressWarnings(as.numeric(comps$p[ia_idx]))
            ci.lb <- TE - 1.96 * se
            ci.ub <- TE + 1.96 * se
            
            cat("\n--- Interaction effects (logHR & HR) ---\n")
            out <- data.frame(
              Interaction = rownames(comps)[ia_idx],
              logHR = round(TE, 3),
              HR    = round(exp(TE), 3),
              CI.lb = round(exp(ci.lb), 3),
              CI.ub = round(exp(ci.ub), 3),
              pval  = signif(pval, 3),
              row.names = NULL
            )
            print(out, row.names = FALSE)
          } else {
            cat("\n(No interaction terms detected in this model.)\n")
          }
        } else {
          cat("\n(No component estimates available to extract interaction terms.)\n")
        }
      } else {
        cat("\n(Could not compute raw summary for interaction terms.)\n")
      }
    } else if (analysis_opts()$cnma_model_type == "interaction" && forced_add) {
      cat("\n(Interaction not available on disconnected networks → additive CNMA ran; no interaction terms.)\n")
    }
  })
  

  
  
  
  
  
  
  

}

shinyApp(ui = ui, server = server)
  
