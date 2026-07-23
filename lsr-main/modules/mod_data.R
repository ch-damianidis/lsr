# modules/mod_data.R
# Data loading, file upload, preview, and validation module
# =========================================================

mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             fileInput(ns("upload_file"),
                       "Upload your data (CSV or Excel):",
                       accept = c(".csv", ".xlsx", ".xls"),
                       placeholder = "No file selected"),
             helpText("Leave empty to use the bundled example dataset.")
      ),
      column(6,
             br(),
             verbatimTextOutput(ns("data_validation_msg"))
      )
    ),
    hr(),
    h3("Data Preview"),
    DT::dataTableOutput(ns("data_preview")),
    br(),
    h4("Data Summary"),
    verbatimTextOutput(ns("data_summary")),
    br()
  )
}

mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ---- Reactive state ----
    original_data     <- reactiveVal(NULL)
    original_pairwise <- reactiveVal(NULL)
    raw_data          <- reactiveVal(NULL)

    # ---- Load bundled data on startup (graceful error) ----
    observe({
      if (is.null(original_data()) && is.null(input$upload_file)) {
        data_raw <- tryCatch({
          get_data_from_redcap()
        }, error = function(e) {
          showNotification(
            paste("Could not load bundled dataset:", conditionMessage(e)),
            type = "error", duration = 10
          )
          NULL
        })
        if (!is.null(data_raw)) {
          load_dataset(data_raw, source_label = "bundled dataset")
        }
      }
    })

    # ---- Handle file upload ----
    observeEvent(input$upload_file, {
      file <- input$upload_file
      req(file)
      ext <- tolower(tools::file_ext(file$name))

      data_raw <- tryCatch({
        if (ext == "csv") {
          read.csv(file$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(file$datapath)
        } else {
          showNotification("Unsupported file format. Please use .csv or .xlsx.",
                           type = "error", duration = 6)
          return(NULL)
        }
      }, error = function(e) {
        showNotification(
          paste("Failed to read file:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

      if (!is.null(data_raw)) {
        load_dataset(data_raw, source_label = file$name)
        showNotification(
          paste("Loaded", nrow(data_raw), "rows from", file$name),
          type = "message", duration = 5
        )
      }
    })

    # ---- Internal: load + detect layout + convert ----
    load_dataset <- function(data_raw, source_label = "file") {
      original_data(data_raw)

      required_cols_arm <- c("study", "treatment", "mean", "sd", "n")
      if (all(required_cols_arm %in% colnames(data_raw))) {
        output$data_validation_msg <- renderText(
          paste0("Arm-level data detected (", source_label, ").\n",
                 "Data will be converted to pairwise format automatically.")
        )
      } else {
        output$data_validation_msg <- renderText(
          paste0("Pairwise data detected (", source_label, ").")
        )
      }

      pw <- tryCatch({
        convert_to_pairwise(data_raw)
      }, error = function(e) {
        showNotification(
          paste("Failed to convert data to pairwise format:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

      if (!is.null(pw)) {
        original_pairwise(pw)
        raw_data(pw)
      }
    }

    # ---- Data preview (original data, not modified by merges) ----
    output$data_preview <- DT::renderDataTable({
      req(original_data())
      DT::datatable(head(original_data(), 50),
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # ---- Data summary ----
    output$data_summary <- renderPrint({
      req(original_data())
      summ <- summarize_data(original_data())
      cat("Number of studies:  ", summ$n_studies,        "\n")
      cat("Number of arms/rows:", summ$n_arms,           "\n")
      cat("Number of treatments:", summ$n_treatments,    "\n")
      cat("Percent missing data:", summ$missing_percent, "%\n")
    })

    # ---- Return reactives (the reactiveVal functions themselves) ----
    list(
      raw_data          = raw_data,
      original_data     = original_data,
      original_pairwise = original_pairwise
    )
  })
}
