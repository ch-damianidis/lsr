# Server (Orchestrator)
# ==============================
# This file wires together the Shiny modules and manages shared
# reactive state (models, analysis options, reference treatment).
# ==============================

server <- function(input, output, session) {

  # ==== Module: Data Loading ====
  data_mod <- mod_data_server("data")

  # Extract reactive accessors from data module
  raw_data          <- data_mod$raw_data
  original_data     <- data_mod$original_data
  original_pairwise <- data_mod$original_pairwise

  # ==== Module: Node Merging ====
  mod_merge_server("merge",
                   raw_data          = raw_data,
                   original_pairwise = original_pairwise)

  # ==== Reference treatment choices (dynamic) ====
  observe({
    req(raw_data())
    d <- raw_data()
    trts <- sort(unique(c(as.character(d$treat1), as.character(d$treat2))))
    updateSelectInput(session, "reference_treatment",
                      choices = c("Auto (alphabetical)" = "", trts))
  })

  # ==== Analysis options committed on Run ====
  analysis_opts <- eventReactive(input$run_nma, {
    ref <- input$reference_treatment
    list(
      cnma_model_type = input$cnma_model_type,
      model_type      = input$model_type,
      reference_group = if (!is.null(ref) && nchar(ref) > 0) ref else NULL
    )
  }, ignoreInit = TRUE)

  # ==== Run Analysis button (validation) ====
  observeEvent(input$run_nma, {
    output$nma_status <- renderText("Analysis in progress...")
    req(raw_data())

    # Validate required columns
    d <- raw_data()
    req_cols <- c("logHR", "selogHR", "treat1", "treat2", "study")
    missing_cols <- setdiff(req_cols, names(d))
    if (length(missing_cols) > 0) {
      output$nma_status <- renderText(
        paste("Missing required columns:", paste(missing_cols, collapse = ", "),
              "\nPlease check your data format.")
      )
      return()
    }

    output$nma_status <- renderText("Analysis completed.")
  })

  # ==== Warn if interaction on disconnected network ====
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

  # ==== CNMA/NMA model (main model for forest/league/summary) ====
  cnma_model <- reactive({
    req(raw_data(), analysis_opts())
    model_kind <- analysis_opts()$cnma_model_type
    use_random <- analysis_opts()$model_type == "random"
    ref_group  <- analysis_opts()$reference_group

    if (model_kind == "simple") {
      nma <- tryCatch({
        netmeta::netmeta(
          TE      = raw_data()$logHR,
          seTE    = raw_data()$selogHR,
          treat1  = raw_data()$treat1,
          treat2  = raw_data()$treat2,
          studlab = raw_data()$study,
          sm      = "HR",
          random  = use_random,
          reference.group = ref_group
        )
      }, error = function(e) {
        showNotification(paste("NMA model error:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      })
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
      run_cnma_analysis(raw_data(),
                        interaction = interaction_flag,
                        random = use_random,
                        reference.group = ref_group)
    }, error = function(e) {
      showNotification(paste("CNMA model error:", conditionMessage(e)),
                       type = "error", duration = 8)
      NULL
    })

    if (!is.null(nma)) {
      nma$pscore <- tryCatch(
        netmeta::netrank(nma, small.values = "desirable")$p.score,
        error = function(e) NULL
      )
    }
    nma
  })

  # ==== Treatment-level netmeta (for network plot, funnel, inconsistency) ====
  nm_model <- reactive({
    req(raw_data(), analysis_opts())
    ref_group <- analysis_opts()$reference_group
    tryCatch({
      build_nm(raw_data(),
               random = (analysis_opts()$model_type == "random"),
               reference.group = ref_group)
    }, error = function(e) {
      showNotification(paste("Treatment-level model error:", conditionMessage(e)),
                       type = "error", duration = 8)
      NULL
    })
  })

  # ==== Module: Results (plots, summary, ranking, inconsistency) ====
  mod_results_server("results",
                     nm_model      = nm_model,
                     cnma_model    = cnma_model,
                     raw_data      = raw_data,
                     analysis_opts = analysis_opts)

  # ==== Module: Export ====
  mod_export_server("export",
                    cnma_model    = cnma_model,
                    raw_data      = raw_data,
                    analysis_opts = analysis_opts)

  # ==== Navigation: Home > Help button ====
  observeEvent(input$go_to_help, {
    updateTabsetPanel(session, "main_tabs", selected = "help")
  })
}
