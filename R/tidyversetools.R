# tibble-stuff ----

#' turn a matrix into a wide data.frame
#'
#' @param mat a matrix
#' @param nm vector of names for the matrix elements
#'
#' @export
#'
#' @examples
#' matrix(1:4, nrow = 2) %>% matrix_2_wide_df(letters[1:4])
matrix_2_wide_df <- function(mat, nm) {mat %>%
    `dim<-`(NULL) %>%
    t %>%
    as.data.frame(stringsAsFactors = F) %>%
    `names<-`(nm)}



#' Title
#'
#' @param .data tibble
#' @param col1 colname, standard dplyr quotation rules apply
#' @param col2 colname, standard dplyr quotation rules apply
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1:2, rep(NA, 2), 1:2), b = c(rep(NA,2), 3:4, 3:4))
#' df %>% merge_col2_into_col1(a,b)
#' df %>% merge_col2_into_col1(b,a)
merge_col2_into_col1 <- function(.data, col1, col2) {

  c1 <- enquo(col1)
  c2 <- enquo(col2)

  c1name <- quo_name(c1)
  c2name <- quo_name(c1)

  UQ <- rlang::UQ

  mutate(.data, UQ(c1name) := replace(UQ(c1), is.na(UQ(c1)), UQ(c2)[is.na(UQ(c1))])) %>%
    select(-UQ(c2))
}



# ggplot2 ----

#' Remove a geom layer
#'
#' @param x ggplot object
#' @param geom_type Character such as "GeomLine"
#' @param last_only only the topmost layer of this kind is removed
#'
#' @details This function was mainly written by Pedro Aphalo on https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart#51612148
#' I only added the last_only - argument
#'
#' @return the plot with the layers removed
#' @export
remove_geom <- function(x, geom_type, last_only = T) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$geom)[1] == geom_type
                     })
  if(last_only & sum(selector) > 0)
    selector <- max(which(selector))
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}


