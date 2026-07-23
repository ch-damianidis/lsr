# modules/mod_export.R
# Report export and history module
# =================================

mod_export_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           selectInput(ns("report_format"), "Report format:",
                       choices = c("HTML" = "html")),
           actionButton(ns("export_report"), "Generate Report")
    ),
    column(8,
           downloadButton(ns("download_report"), "Download report")
    ),
    br(), br(),
    column(12,
           h4("History of generated reports"),
           actionButton(ns("refresh_history"), "Clear"),
           DT::dataTableOutput(ns("report_history"))
    )
  )
}

mod_export_server <- function(id, cnma_model, raw_data, analysis_opts) {
  moduleServer(id, function(input, output, session) {

    # ---- Persistent History Log ----
    history_file <- "history_log.csv"

    if (file.exists(history_file)) {
      history_data <- read.csv(history_file, stringsAsFactors = FALSE)
    } else {
      history_data <- data.frame(
        time = character(), format = character(),
        n_studies = numeric(), n_treatments = numeric(),
        stringsAsFactors = FALSE
      )
    }
    history_log <- reactiveVal(history_data)

    # ---- Log report generation ----
    observeEvent(input$export_report, {
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
      showNotification("Report logged. Click 'Download report' to save.",
                       type = "message", duration = 5)
    })

    # ---- Clear history ----
    observeEvent(input$refresh_history, {
      empty_df <- data.frame(
        time = character(), format = character(),
        n_studies = numeric(), n_treatments = numeric(),
        stringsAsFactors = FALSE
      )
      history_log(empty_df)
      write.csv(empty_df, history_file, row.names = FALSE)
    })

    # ---- History table ----
    output$report_history <- DT::renderDataTable({ history_log() })

    # ---- Download handler ----
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("report_", Sys.Date(), ".html")
      },
      content = function(file) {
        tryCatch({
          rmarkdown::render(
            input = "reports/report_template.Rmd",
            output_format = "html_document",
            output_file = file,
            params = list(
              nma                = cnma_model(),
              raw_data           = raw_data(),
              effect_model       = analysis_opts()$model_type,
              show_funnel        = TRUE,
              show_inconsistency = TRUE,
              show_ranking       = TRUE
            )
          )
        }, error = function(e) {
          showNotification(
            paste("Report generation failed:", conditionMessage(e)),
            type = "error", duration = 10
          )
        })
      }
    )
  })
}
