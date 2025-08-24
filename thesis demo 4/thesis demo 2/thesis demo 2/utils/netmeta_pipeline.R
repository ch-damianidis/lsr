library(netmeta)
library(igraph)

run_cnma_analysis <- function(data, interaction = FALSE, random = TRUE) {
  # Check if network is connected
  edges <- data.frame(from = data$treat1, to = data$treat2)
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  connected <- igraph::is_connected(g)
  
  if (connected) {
    # Connected: netcomb for additive or interaction CNMA
    nm <- netmeta(
      TE     = data$logHR,
      seTE   = data$selogHR,
      treat1 = data$treat1,
      treat2 = data$treat2,
      studlab= data$study,
      sm     = "HR",
      random = random
    )
    
    if (interaction) {
      comps <- c("Acal","BR","FCR","I","Ibr","O","R","Ven","Zanubrutinib")
      # Όλα τα 2-way interactions σε σωστή μορφή ("X+Y") ΣΟΣ μόνο 2-way interactions
      comb.ia <- combn(comps, 2, FUN = function(x) paste(x, collapse = "+"))
      # Δημιουργία C-matrix με interactions
      Cmat_int <- createC(nm, comb.ia = comb.ia, inactive = "Clb")
      
      nc <- netcomb(nm, C.matrix = Cmat_int)
    } else {
      nc <- netcomb(
        nm,
        sep.trts = " + "
      )
    }
    
    return(nc)
    
  } else {
    # Disconnected: Only discomb (additive CNMA) 
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
    attr(dc, "forced_additive") <- interaction # TRUE αν ζήτησε interaction
    return(dc)
  }
}
