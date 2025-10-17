library(netmeta)
library(igraph)

# utils/netmeta_pipeline.R

build_nm <- function(data, random = TRUE) {
  req_cols <- c("logHR", "selogHR", "treat1", "treat2", "study")
  miss <- setdiff(req_cols, names(data))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  netmeta::netmeta(
    TE      = data$logHR,
    seTE    = data$selogHR,
    treat1  = data$treat1,
    treat2  = data$treat2,
    studlab = data$study,
    sm      = "HR",
    random  = random
  )
}

run_cnma_analysis <- function(data, interaction = FALSE, random = TRUE) {
  # Build graph to check if the treatment network is connected
  edges <- data.frame(from = data$treat1, to = data$treat2)
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  connected <- igraph::is.connected(g)
  
  if (connected) {
    # If the network is connected: run netmeta + netcomb (CNMA)
    nm <- netmeta(
      TE     = data$logHR,
      seTE   = data$selogHR,
      treat1 = data$treat1,
      treat2 = data$treat2,
      studlab= data$study,
      sm     = "HR",
      random = random
    )
    
    if (interaction) {  # Extract all unique components from treatment names
      
      all_trts <- unique(c(data$treat1, data$treat2))
      comps <- sort(unique(trimws(unlist(strsplit(all_trts, "\\s*\\+\\s*")))))
      
      # Generate all 2-way interaction terms in the correct format ("X+Y") [SOS]
      comb.ia <- combn(comps, 2, FUN = function(x) paste(x, collapse = "+"))
      # <-- ΑΥΤΟ είναι το fix για το Clb
      inactive_val <- if ("Clb" %in% comps) "Clb" else NULL
      # Create C-matrix including interaction terms
      Cmat_int <- createC(nm, comb.ia = comb.ia,sep.trts = " + ")
      # Run CNMA with interactions
      nc <- netcomb(nm, C.matrix = Cmat_int)
    } else {  
      nc <- netcomb(
        nm,
        sep.trts = " + "
      )
    }
    
    return(nc)
    # If the network is disconnected: use discomb (only additive CNMA possible)
  } else {
    
    if (interaction) {
      warning("Interaction model is not available for disconnected networks. Using additive model instead.")
    }
    dc <- discomb(
      TE     = data$logHR,
      seTE   = data$selogHR,
      treat1 = data$treat1,
      treat2 = data$treat2,
      studlab= data$study,
      data   = data,
      sm     = "HR",
      random = random
    )
    attr(dc, "forced_additive") <- interaction  #Mark that interaction was requested but forced to additive due to disconnection
    return(dc)
  }
}
