#' Generates an author contribution table
#'
#' @param df data frame containing author contribution information
#'   in two columns: Author, Activity. See example for details.
#' @import ggplot2
#' @return ggplot of author contribution table
#' @examples
#' # NK participated in Concept and Design
#' # RW wrote code
#' # JS participated in Concept and Design
#' info <- data.frame(Author = c("NK", "NK", "RW", "JS", "JS"),
#'                    Activity = c("Concept", "Design", "Code", "Concept",
#'                                 "Design"))
#' # grey implies author participation in activity; white does not
#' contribution_table(info)
contribution_table <- function(df) {
  df2 <- expand.grid(x = levels(df$Author), y = levels(df$Activity))
  for (i in 1:nrow(df2)) {
    if (nrow(subset(df, Author == df2$x[i] & Activity == df2$y[i])) > 0) {
      df2$Fill[i] <- "grey"
    } else {
      df2$Fill[i] <- "white"
    }
  }

  ggplot(df2, aes(y, x, fill = I(Fill))) +
    geom_tile(colour = I("black")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0)) +
    xlab("") + ylab("")
}
