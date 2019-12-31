contribution_table <- function(df) {
  df <- expand.grid(x = levels(stuff$Author), y = levels(stuff$Activity))
  for (i in 1:nrow(df)) {
    if (nrow(subset(stuff, Author == df$x[i] & Activity == df$y[i])) > 0) {
      df$Fill[i] <- "grey"
    } else {
      df$Fill[i] <- "white"
    }
  }

  ggplot(df, aes(y, x, fill = I(Fill))) +
    geom_tile(colour = I("black")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0)) +
    xlab("") + ylab("")
}
