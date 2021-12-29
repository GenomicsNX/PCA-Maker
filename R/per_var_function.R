per_var <- function(pca_analysis) {
  eigen <- pca_analysis$sdev ^ 2 / (nrow(pca_analysis$x) - 1)
  
  mutate(
    as.data.frame(eigen),
    PC_i   = 1:length(eigen),
    PC     = purrr::map_chr(PC_i, function(x) paste0("PC", x)),
    VarPer = eigen * 100 / sum(eigen)
  )
}