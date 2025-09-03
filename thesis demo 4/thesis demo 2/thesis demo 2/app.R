#libraries
library(shiny)
library(netmeta)
library(shinythemes)
library(DT)
library(igraph)
library(meta)
library(ggplot2)


#sources
source("utils/netmeta_pipeline.R")
source("utils/redcap_connect.R")
source("utils/data_processing.R")




#interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Living NMA Tool (CLL – Demo)"),
  
  tabsetPanel(id = "main_tabs",
              tabPanel("Home",                 #Home
                       value = "home",
                       fluidRow(
                         column(8,
                                h3("Welcome to the Living Network Meta-Analysis Tool"),
                                p("This tool allows you to perform and explore a living network meta-analysis."),
                                p("You can analyze data from REDCap or uploaded CSV files."),
                                br(),
                                actionButton("run_nma", "Run Analysis"),
                                br(), br(),
                                verbatimTextOutput("nma_status"),
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
              #Data Overview
              tabPanel("Data Overview",
                       value = "data_overview",
                       fluidRow(
                         column(12,
                                h3("Data Preview"),
                                DT::dataTableOutput("data_preview"),
                                br(),
                                h4("Data Summary"),
                                verbatimTextOutput("data_summary"),# Shows summary stats (studies, arms, treatments, missingness)
                                br(),
                                verbatimTextOutput("data_validation_msg"),  # Message about data format (arm-level or pairwise)
                                br(),
                                h4("Node Merging"),
                                uiOutput("studies_selector"), 
                                actionButton("apply_merge", "Apply Node Merging"),   # Button to trigger node/component merging
                                actionButton("reset_data", "Reset to Original Data", icon = icon("refresh"))
                                
                         )
                       )
              ),
              
              
            #Plots
              tabPanel("Plots",
                       tabsetPanel(
                         tabPanel("Network Plot", plotOutput("network_plot", height = "600px", width = "100%")),
                         tabPanel("Forest Plot", plotOutput("forest_plot"))
                       )
              ),
            #Summary
              tabPanel("Summary",
                       value = "summary",
                       fluidRow(
                         column(12,
                                wellPanel(
                                  fluidRow(
                                    column(6,
                                           radioButtons("cnma_model_type", "CNMA Model:",
                                                        choices = c("Additive (No Interaction)" = "additive", "Interaction" = "interaction"),
                                                        selected = "additive"
                                           ),
                                           verbatimTextOutput("model_warning") # warning message interaction can't be exist
                                    ),
                                    column(6,
                                           radioButtons("model_type", "Effect model:",
                                                        choices = c("Random Effects" = "random", "Fixed Effect" = "fixed"),
                                                        selected = "random"
                                           )
                                    )
                                  )
                                ),
                                h3("Model Summary"),
                                verbatimTextOutput("nma_summary")
                         )
                       )
              ),
              tabPanel("Funnel Plot",         #it doesn't work 
                       value = "funnel",
                       fluidRow(
                         column(12,
                                h3("Funnel Plot for Publication Bias"),
                                plotOutput("funnel_plot")
                         )
                       )
              ),
            #doesn't for star shaped stracture
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
                                   ),
                                   tabPanel("Heatmap (Netheat)",
                                            br(),
                                            h4("Network Heatmap for Inconsistency"),
                                            plotOutput("netheat_plot")
                                   )
                       )
              ),
              tabPanel("Ranking",            #something is going wrong 
                       value = "ranking",
                       fluidRow(
                         column(6, h4("League Table"), tableOutput("league_table")),
                         column(6, h4("Ranking Table"), tableOutput("ranking_table"))
                       )
              ),
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
                                          choices = c("PDF" = "pdf",
                                                      "Word (docx)" = "word",
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

#backend 
server <- function(input, output, session) {
  ## ---- History Log (persistent) ----
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
  
  raw_data <- reactiveVal(NULL)      # Main reactive variable to store uploaded/processed data
  #run nma
  observeEvent(input$run_nma, {
    output$nma_status <- renderText("Analysis in progress...")      
    data <- get_data_from_redcap()      #load data
    data <- convert_to_pairwise(data)     #transform them to pairwise if necessary, See utils/data_processing.R
    raw_data(data)                         #store the data to the reactive variable
    # Detect data format and inform user
    required_cols_arm <- c("study", "treatment", "mean", "sd", "n")
    if (all(required_cols_arm %in% colnames(data))) {
      output$data_validation_msg <- renderText("Arm-level data detected. Data will be converted to pairwise.")
    } else {
      output$data_validation_msg <- renderText("Pairwise data detected.")   #στο data overview αν ειναι pairwise
    }
    output$nma_status <- renderText("Analysis completed.")
  })
  
  #  Warning for Interaction Model to Disconnected Networks
  output$model_warning <- renderText({
    req(raw_data())
    interaction <- input$cnma_model_type == "interaction"
    data <- raw_data()
    edges <- data.frame(from = data$treat1, to = data$treat2)
    g <- igraph::graph_from_data_frame(edges, directed = FALSE)
    if (interaction && !igraph::is.connected(g)) {
      return("Interaction model is not available for disconnected networks. Only additive model can be run.")
    }
    NULL
  })
  # ---- CNMA Model Reactive ----
  cnma_model <- reactive({
    req(raw_data())
    interaction <- input$cnma_model_type == "interaction"
    random <- input$model_type == "random"
    nma <- tryCatch({
      run_cnma_analysis(raw_data(), interaction = interaction, random = random)
    }, error = function(e) {
      return(NULL)
    })
    if (!is.null(nma)) {  # Calculate treatment ranking (P-score) if model is available
      nma$pscore <- tryCatch(netrank(nma, small.values = "good")$p.score, error = function(e) NULL)
    }
    nma
  })
  # ---- Treatment-level NMA (για inconsistency) ----
  nm_model <- reactive({
    req(raw_data())
    tryCatch({
      build_nm(raw_data(), random = (input$model_type == "random"))
    }, error = function(e) {
      NULL
    })
  })
  
  
  # ---- Data preview output ----
  output$data_preview <- DT::renderDataTable({
    req(raw_data())
    DT::datatable(head(raw_data(), 20), options = list(pageLength = 10))
  })
  # ---- Data summary output ----
  output$data_summary <- renderPrint({
    req(raw_data())
    summ <- summarize_data(raw_data())
    cat("Number of studies: ", summ$n_studies, "\n")
    cat("Number of arms: ", summ$n_arms, "\n")
    cat("Number of treatments: ", summ$n_treatments, "\n")
    cat("Percent missing data: ", summ$missing_percent, "%\n")
  })
  # ---- Network plot (netmeta) ----
  output$network_plot <- renderPlot({
    nm <- nm_model()  # treatment-level NMA object (class "netmeta")
    validate(need(!is.null(nm), "Run the analysis first to build the network."))
    
    tryCatch({
      netmeta::netgraph(
        nm,
        number.of.studies = TRUE,                # δείχνει # μελετών σε κάθε ακμή
        thickness         = "number.of.studies", # πάχος ακμής ανά # μελετών
        multiarm          = TRUE,                # σωστή απεικόνιση multi-arm
        points            = TRUE,                # κόμβοι
        cex.points        = 1.4,                 # μέγεθος κόμβων
        cex               = 1.1,                 # μέγεθος ετικετών
        plastic           = FALSE,               # καθαρό layout
        seq               = sort(nm$trts)        # σταθερή σειρά θεραπειών (πιο όμορφο layout)
        # προαιρετικά χρώματα, αν θέλεις:
        # col              = "black",
        # col.points       = "black",
        # col.multiarm     = "grey40"
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Network plot not available:", conditionMessage(e)), cex = 1.1)
    })
  }, res = 144)
  
  # ---- Forest plot ----
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
  # Helper: extract all unique treatment components from treatment names
  get_unique_components <- function(treatments) {
    unique(trimws(unlist(strsplit(treatments, split = "[+/]"))))
  }
  # ---- Node Merging (modal dialog, update data) ----
  observeEvent(input$apply_merge, {
    req(raw_data())
    sel <- input$merge_studies
    
    if (length(sel) == 0) {
      showNotification("Choose at least one study firstly", type = "warning")
      return()
    }
    
    # Gather all treatments from selected studies
    available_treatments <- raw_data() %>%
      filter(study %in% sel) %>%
      summarise(treats = unique(c(treat1, treat2))) %>%
      pull(treats)
    
    available_components <- get_unique_components(available_treatments)
    # Show modal dialog to select components to merge
    showModal(modalDialog(
      title = "Component Merging",
      selectInput("selected_components_to_merge", 
                  "selected components to merge:",
                  choices = available_components, 
                  selected = NULL, 
                  multiple = TRUE),
      textInput("new_family_name", "New node name:", ""),
      footer = tagList(
        modalButton("cancel"),
        actionButton("confirm_merge", "confirm")
      )
    ))
  })
  
  observeEvent(input$confirm_merge, {
    req(input$new_family_name, input$selected_components_to_merge)
    
    sel_studies <- input$merge_studies
    sel_components <- input$selected_components_to_merge
    new_name <- input$new_family_name
    
    merge_components <- function(treatment, components, new_name) {
      parts <- trimws(unlist(strsplit(treatment, split = "[+/]")))
      parts[parts %in% components] <- new_name
      parts <- unique(parts)
      paste(parts, collapse = " + ")
    }
    
    new_data <- raw_data() %>%
      mutate(
        treat1_new = ifelse(study %in% sel_studies, 
                            sapply(treat1, merge_components, components = sel_components, new_name = new_name), 
                            treat1),
        treat2_new = ifelse(study %in% sel_studies, 
                            sapply(treat2, merge_components, components = sel_components, new_name = new_name), 
                            treat2)
      )
    
    self_loops <- new_data %>% filter(treat1_new == treat2_new)
    
    if(nrow(self_loops) > 0){
      showModal(modalDialog(
        title = "Warning!",
        paste("self-loops (identical treatments). Please change the selected components and try again."),
        easyClose = TRUE,
        footer = modalButton("Got it")
      ))
    } else {
      new_data <- new_data %>% 
        select(-treat1, -treat2) %>% 
        rename(treat1 = treat1_new, treat2 = treat2_new)
      
      raw_data(new_data)            # <---- HERE THE MODEL WAS NOT TRIGGERED
      cnma_model()                 # <---- ADD: to run the model again
      
      removeModal()
      
      showNotification(
        paste("Components merged into the node", new_name),
        type = "message"
      )
    }
  })
  # ---- Reset Node Merging ----
  observeEvent(input$reset_data, {
    data <- get_data_from_redcap()
    data <- convert_to_pairwise(data)
    raw_data(data)
    cnma_model()   # rerun the model 
    showNotification("Dataset reset to original.", type = "message")
  })
  

  
  
  
  
  
  # ---- UI for study selector (node merging) ----
  output$studies_selector <- renderUI({
    req(raw_data())
    checkboxGroupInput(
      "merge_studies",
      "Select studies to merge:",
      choices = unique(raw_data()$study),
      selected = NULL
    )
  })
  
  # ---- League table ----
  output$league_table <- renderTable({
    nma <- cnma_model()
    validate(need(!is.null(nma), "Model is not available."))
    
    # dont present league table if we have 3 or less treatments
    # or we have disconnected (discomb) model
    if (inherits(nma, "discomb") || length(nma$trts) < 3) {
      return(data.frame(Message = "League table not applicable to this network"))
    }
    
    tryCatch({
      mat <- netleague(nma)
      # modify to a dataframe
      as.data.frame.matrix(mat)
    }, error = function(e) {
      data.frame(Message = "Failed to generate league table")
    })
  }, rownames = TRUE)
  
  
  # ---- Ranking table ----
  output$ranking_table <- renderTable({
    nma <- cnma_model()
    validate(need(!is.null(nma), "Model is not available."))
    
    # do not try ranking if model is discomb (disconnected) or only 1 treatment
    if (inherits(nma, "discomb") || length(nma$trts) < 3) {
      return(data.frame(Message = "Ranking not applicable to this network"))
    }
    
    tryCatch({
      ps <- netrank(nma)
      data.frame(
        Treatment = names(ps$p.score),
        P_score = round(ps$p.score, 3)
      )
    }, error = function(e) {
      data.frame(Message = "Failed to calculate treatment ranking")
    })
  }, rownames=FALSE)
  
  
  # ---- Node-splitting results table ----
  # Node-splitting
  output$netsplit_table <- renderTable({
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    tryCatch({
      split <- netsplit(nm)
      round(split[, c("Direct","Indirect","Q","df","p")], 3)
    }, error = function(e) data.frame(Error = "Node-splitting failed"))
  }, rownames = TRUE)
  
  # Design-by-Treatment
  output$decomp_table <- renderTable({
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    tryCatch({
      decomp <- decomp.design(nm)
      round(decomp$Q.decomp, 3)
    }, error = function(e) data.frame(Error = "Global inconsistency analysis failed"))
  }, rownames = TRUE)
  
  # Netheat
  output$netheat_plot <- renderPlot({
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    tryCatch(netheat(nm),
             error = function(e) { plot.new(); text(0.5,0.5,"Netheat plot not available", cex=1.2) })
  })
  
  
  # ---- Funnel plot (treatment-level, backward-compatible) ----
  output$funnel_plot <- renderPlot({
    nm <- nm_model()
    validate(need(!is.null(nm), "Model is not available."))
    
    if (length(unique(nm$studlab)) < 5) {
      plot.new(); text(0.5, 0.5, "Not enough studies for a funnel plot", cex = 1.2); return()
    }
    
    # if netfunnel() is exist, use it
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
      # Fallback for previous methods: S3 method funnel.netmeta
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
  
  
  
  
  
  observeEvent(input$export_report, {
    
    ## ---- Record history + save to file ----
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
    ## ---------------------------------------
    
    
    tmpFile <- tempfile(fileext = switch(input$report_format,
                                         pdf = ".pdf",
                                         word = ".docx",
                                         html = ".html"))
    
    rmarkdown::render(
      input = "reports/report_template.Rmd",
      output_format = switch(input$report_format,
                             pdf = "pdf_document",
                             word = "word_document",
                             html = "html_document"),
      output_file = tmpFile,
      params = list(
        nma      = cnma_model(),
        raw_data = raw_data()
      )
      
    )
    
    
  })
  observeEvent(input$refresh_history, {
    ## Clear history: empty dataframe
    empty_df <- data.frame(
      time = character(),
      format = character(),
      n_studies = numeric(),
      n_treatments = numeric(),
      stringsAsFactors = FALSE
    )
    
    # update reactive value
    history_log(empty_df)
    
    # overwrite history file
    write.csv(empty_df, history_file, row.names = FALSE)
  })
  
  
  output$report_history <- DT::renderDataTable({
    history_log()
  })
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".", switch(input$report_format,
                                                pdf="pdf", word="docx", html="html"))
    },
    content = function(file) {
      rmarkdown::render(
        input = "reports/report_template.Rmd",
        output_format = switch(input$report_format,
                               pdf = "pdf_document",
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
  
  
  
  
  
  # ---- Go to Help tab ----
  observeEvent(input$go_to_help, {
    updateTabsetPanel(session, "main_tabs", selected = "help")
  })
  
  # ---- Model summary (print main CNMA/NMA results) ----
  output$nma_summary <- renderPrint({
    req(cnma_model())
    nma <- cnma_model()
    # If user requested interaction but not available, print warning
    if (!is.null(attr(nma, "forced_additive")) && attr(nma, "forced_additive")) {
      cat("Warning: Interaction model not available for disconnected network; ran additive model instead.\n\n")
    }
    # Model type heading
    cat(">>> CNMA Model type: ",
        ifelse(input$cnma_model_type == "interaction", "Interaction", "Additive (No Interaction)"),
        "\n\n", sep = "")
    
    # Print NMA summary object
    tryCatch({
      sum_obj <- if (input$model_type == "fixed") {
        summary(nma, common = TRUE, random = FALSE)
      } else {
        summary(nma, common = FALSE, random = TRUE)
      }
      print(sum_obj)
    }, error = function(e) {
      cat("Error in summary: ", conditionMessage(e), "\n", sep = "")
    })
    
    
    # If interaction model: print interaction effects
    if (input$cnma_model_type == "interaction") {
      cat("\n--- Interaction Effects ---\n")
      if (input$model_type == "fixed" && !is.null(nma$Int.common)) {
        print(nma$Int.common)
      } else if (input$model_type == "random" && !is.null(nma$Int.random)) {
        print(nma$Int.random)
      } else {
        cat("No interaction terms available.\n")
      }
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
  
