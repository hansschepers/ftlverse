#' visCorMat
#' @export
visCorMat <- function(corMat){
  diag(corMat) <- NA  # hide diagonal
  corMat[lower.tri(corMat)] <- NA
  nodes <- data.table(id = 1:nrow(corMat)
                      , label = colnames(corMat))
  # nodes[, group := "other"]
  # # nodes[grepl("fat", label), group := "fat"]
  # # nodes[grepl("skin", label), group := "skin"]
  # # nodes[grepl("muscle", label), group := "muscle"]
  # nodes[grepl("(head)|(hd)|(brain)", label), group := "head"]
  # nodes[grepl("(tr)", label), group := "trunk"]
  # nodes[grepl("(leg)", label), group := "leg"]
  # nodes[grepl("(arm)", label), group := "arm"]
  # nodes[grepl("(feet)", label), group := "feet"]
  # nodes[grepl("(hand)", label), group := "hand"]
  # nodes[, title := paste0("<p>", label,"<br> tooltip</p>")]
  fn <- fivenum(as.numeric(abs(corMat)))
  cutoff <- fn[3]
  edges <- which(abs(corMat) > cutoff, arr.ind = TRUE)
  abs(corMat)
  edges <- data.frame(from = edges[, 1], to = edges[, 2], value = 1)
  edges <- unique(edges)
  setDT(edges)
  edges[, ind := .I]
  edges[, value := abs(corMat)[from, to], by = ind]
  edges[, ind := NULL]
  vncor <- visNetwork(nodes, edges)
  .vncor <<- vncor
  vncor |>
    visLegend(position = "right")
}
