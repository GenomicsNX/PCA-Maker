bi_plot <- function(pca_scores, pca_loadings, exp_names, x_axis, y_axis,
                    x_lab, y_lab) {
  
  # Some calculations on loadings
  rng1_y <- max(pca_scores[,y_axis]) - min(pca_scores[,y_axis])
  rng2_y <- max(pca_loadings[,y_axis]) - min(pca_loadings[,y_axis])
  
  rng1_x <- max(pca_scores[,x_axis]) - min(pca_scores[,x_axis])
  rng2_x <- max(pca_loadings[,x_axis]) -  min(pca_loadings[,x_axis])
  
  mult <- min(rng1_y / rng2_y, rng1_x / rng2_x)

  pca_lds_trans <- dplyr::mutate(
    pca_loadings,
    v1 = .7 * mult * pca_loadings[, x_axis],
    v2 = .7 * mult * pca_loadings[, y_axis]
  )
  
  # Biplot
  ggplot(data = pca_scores, aes(.data[[x_axis]], .data[[y_axis]])) +
    geom_point(aes(color = as.factor(unlist(exp_names))), size = 2) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    coord_equal() +
    geom_text(
      data = pca_lds_trans,
      aes(v1, v2, label = rownames(pca_loadings)),
      vjust = -0.7, size = 3
    ) +
    geom_segment(
      data = pca_lds_trans,
      aes(0, 0, xend = v1, yend = v2),
      arrow = arrow(length = unit(0.2, "cm")),
      alpha = 0.75, color = "red"
    ) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_minimal() +
    theme(legend.title = element_blank())
}