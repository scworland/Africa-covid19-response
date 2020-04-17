add_functions_to_edges <- function() {
  diagram_edges_label <- function(mat) {
  nms <- rownames(mat)
  i <- which(!is.na(mat), TRUE)
  
  str <- sprintf("'%s' -> '%s' [label = '%s'];",
                   nms[i[,2]], nms[i[,1]], mat[i])

  paste(str, collapse="\n")
}

environment(diagram_edges_label) <- asNamespace('remake')
assignInNamespace("diagram_edges", diagram_edges_label, ns = "remake")
}

add_functions_to_edges()