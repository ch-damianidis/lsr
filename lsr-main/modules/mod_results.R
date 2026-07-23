# modules/mod_results.R
# Results rendering: plots, summary, ranking, inconsistency, funnel
# ==================================================================
# This module uses multiple UI helper functions so outputs can be
# placed in different tabs while sharing the same module namespace.

# ---- UI helpers (one per tab) ----

mod_results_plots_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Network Plot",
             plotOutput(ns("network_plot"), height = "600px", width = "100%")),
    tabPanel("Forest Plot",
             plotOutput(ns("forest_plot"), height = "800px"))
  )
}

mod_results_summary_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
           h3("Model Summary"),
           verbatimTextOutput(ns("nma_summary"))
    )
  )
}

mod_results_funnel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
           h3("Funnel Plot for Publication Bias"),
           plotOutput(ns("funnel_plot"))
    )
  )
}

mod_results_inconsistency_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("inconsistency_tabs"),
    tabPanel("Local (Node-splitting)",
             br(),
             h4("Node-Splitting: Direct vs Indirect Comparison"),
             tableOutput(ns("netsplit_table"))
    ),
    tabPanel("Global (Design-by-Treatment)",
             br(),
             h4("Design-by-Treatment Interaction Model"),
             tableOutput(ns("decomp_table"))
    )
  )
}

mod_results_ranking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             h4("League Table"),
             div(style = "overflow-x:auto;", tableOutput(ns("league_table")))
      )
    ),
    hr(),
    fluidRow(
      column(12,
             h4("Ranking Table"),
             div(style = "overflow-x:auto;", tableOutput(ns("ranking_table")))
      )
    )
  )
}

# ---- Server ----

mod_results_server <- function(id, nm_model, cnma_model, raw_data, analysis_opts) {
  moduleServer(id, function(input, output, session) {

    # ======== PLOTS ========

    # ---- Network plot ----
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
        )
      }, error = function(e) {
        showNotification(paste("Network plot error:", conditionMessage(e)),
                         type = "error", duration = 8)
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
          showNotification(paste("Forest plot error:", conditionMessage(e)),
                           type = "error", duration = 8)
          plot.new()
          text(0.5, 0.5, "Forest plot not available", cex = 1.2)
        }
      )
    })

    # ---- Funnel plot ----
    output$funnel_plot <- renderPlot({
      req(analysis_opts())
      nm <- nm_model()
      validate(need(!is.null(nm), "Model is not available."))

      if (length(unique(nm$studlab)) < 5) {
        plot.new()
        text(0.5, 0.5, "Not enough studies for a funnel plot (< 5)", cex = 1.2)
        return()
      }

      tryCatch({
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
      }, error = function(e) {
        showNotification(paste("Funnel plot error:", conditionMessage(e)),
                         type = "error", duration = 8)
        plot.new()
        text(0.5, 0.5, paste("Funnel plot not available:", conditionMessage(e)), cex = 1.1)
      })
    })

    # ======== SUMMARY ========

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
            "Interaction requested -> Additive CNMA (disconnected network)"
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

      # Interaction terms (logHR & HR scale)
      if (analysis_opts()$cnma_model_type == "interaction" && inherits(nma, "netcomb") && !forced_add) {
        s_raw <- tryCatch(
          summary(nma, common = use_common, random = use_random, backtransf = FALSE),
          error = function(e) NULL
        )
        if (!is.null(s_raw)) {
          comps <- if (use_common && !is.null(s_raw$components.common)) {
            s_raw$components.common
          } else {
            s_raw$components.random
          }
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
        cat("\n(Interaction not available on disconnected networks; additive CNMA ran instead.)\n")
      }
    })

    # ======== LEAGUE TABLE ========

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
          netmeta::netleague(fit, common = use_common, random = use_random,
                             ci = TRUE, backtransf = TRUE,
                             bracket = "(", separator = " - ",
                             direct = FALSE)
        } else {
          netmeta::netleague(fit, common = use_common, random = use_random,
                             ci = TRUE, backtransf = TRUE,
                             bracket = "(", separator = " - ")
        }
      }, error = function(e) {
        showNotification(paste("League table error:", conditionMessage(e)),
                         type = "error", duration = 8)
        e
      })

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
        return(data.frame(Message = "Requested effect model table is unavailable."))
      }

      as.data.frame(mat, stringsAsFactors = FALSE)
    }, rownames = TRUE)

    # ======== RANKING TABLE ========

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
        netmeta::netrank(nm, small.values = "desirable", method = "P-score"),
        error = function(e) {
          showNotification(paste("Ranking error:", conditionMessage(e)),
                           type = "error", duration = 8)
          e
        }
      )
      if (inherits(ps, "error")) {
        return(data.frame(Message = paste("Failed to calculate ranking:", conditionMessage(ps))))
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

    # ======== INCONSISTENCY ========

    # ---- Node-splitting (local) ----
    output$netsplit_table <- renderTable({
      req(analysis_opts())
      nm <- nm_model()
      validate(need(!is.null(nm), "Model is not available."))

      ns <- tryCatch(
        netmeta::netsplit(nm, random = TRUE, common = FALSE, backtransf = TRUE),
        error = function(e) {
          showNotification(paste("Node-splitting error:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
      validate(need(!is.null(ns), "Node-splitting could not be performed."))

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

      numcols <- c("Direct","Indirect","lower.direct","upper.direct",
                    "lower.indirect","upper.indirect","z","Q","p.diff")
      out[numcols] <- lapply(out[numcols], function(x) if (is.numeric(x)) round(x, 3) else x)

      out[order(out$comparison), ]
    }, rownames = FALSE)

    # ---- Design-by-Treatment (global) ----
    output$decomp_table <- renderTable({
      req(analysis_opts())
      nm <- nm_model()
      validate(need(!is.null(nm), "Model is not available."))
      tryCatch({
        decomp <- netmeta::decomp.design(nm)
        round(decomp$Q.decomp, 3)
      }, error = function(e) {
        showNotification(paste("Global inconsistency error:", conditionMessage(e)),
                         type = "error", duration = 8)
        data.frame(Error = paste("Analysis failed:", conditionMessage(e)))
      })
    }, rownames = TRUE)

  })
}
