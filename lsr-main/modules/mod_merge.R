# modules/mod_merge.R
# Node merging module
# ====================

mod_merge_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Node Merging"),
    selectizeInput(ns("components_to_merge"),
                   "Select components to merge:",
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Choose >= 2 components")),
    textInput(ns("new_component_name"), "New node name:", ""),
    actionButton(ns("apply_merge"), "Apply Node Merge"),
    actionButton(ns("reset_data"), "Reset to Original Data", icon = icon("refresh"))
  )
}

mod_merge_server <- function(id, raw_data, original_pairwise) {
  moduleServer(id, function(input, output, session) {

    # Keep component choices in sync with current working data
    observe({
      req(raw_data())
      d <- raw_data()
      labs <- unique(c(as.character(d$treat1), as.character(d$treat2)))
      comps <- sort(unique(trimws(unlist(strsplit(labs, "\\s*\\+\\s*")))))
      updateSelectizeInput(session, "components_to_merge",
                           choices = comps, server = TRUE)
    })

    # ---- Apply merge ----
    observeEvent(input$apply_merge, {
      req(raw_data())

      comps <- input$components_to_merge
      newnm <- trimws(input$new_component_name)

      validate(
        need(length(comps) >= 2, "Select at least two components to merge."),
        need(nchar(newnm) > 0,   "Provide a new node name.")
      )

      d <- raw_data()

      # Replace selected components inside labels like "A + B + C"
      replace_comps <- function(lbl, comps, newnm) {
        parts <- trimws(unlist(strsplit(as.character(lbl), "\\s*\\+\\s*")))
        parts[parts %in% comps] <- newnm
        parts <- unique(parts)
        paste(parts, collapse = " + ")
      }

      d <- d %>%
        mutate(
          treat1_new = vapply(treat1, replace_comps, FUN.VALUE = character(1),
                              comps = comps, newnm = newnm),
          treat2_new = vapply(treat2, replace_comps, FUN.VALUE = character(1),
                              comps = comps, newnm = newnm)
        )

      # Warn if any study becomes single-arm
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
          paste0("WARNING: Studies become single-arm after merge: ",
                 paste(single_arm_tbl$study, collapse = ", "),
                 ". These are not analyzable in NMA."),
          type = "warning", duration = 10
        )
      }

      # Prevent self-loops
      if (any(d$treat1_new == d$treat2_new)) {
        showNotification("Self-loop created by merging. Change selection/name and try again.",
                         type = "warning", duration = 6)
        return()
      }

      # Normalize pair direction & pool duplicates (inverse-variance weighting)
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

      raw_data(pooled)

      showNotification(
        paste0("Merged components {", paste(comps, collapse = ", "),
               "} into node '", newnm, "'."),
        type = "message", duration = 6
      )
    })

    # ---- Reset to original ----
    observeEvent(input$reset_data, {
      req(original_pairwise())
      raw_data(original_pairwise())
      showNotification("Working data reset to the original pairwise dataset.",
                       type = "message", duration = 4)
    })
  })
}
