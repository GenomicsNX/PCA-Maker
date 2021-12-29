var_pcs <- function(pca_analysis, x_axis, y_axis) {
  
  per_var_pc <- per_var(pca_analysis)
  
  var_pcx <- filter(per_var_pc, PC == x_axis)
  var_pcx <- select(var_pcx, VarPer)
  var_pcx <- round(unlist(var_pcx))
  
  var_pcy <- filter(per_var_pc, PC == y_axis)
  var_pcy <- select(var_pcy, VarPer)
  var_pcy <- round(unlist(var_pcy))
  
  list(
    var_pcx = paste0(x_axis, " ", "(", var_pcx, "%", ")"),
    var_pcy = paste0(y_axis, " ", "(", var_pcy, "%", ")")
  )
}