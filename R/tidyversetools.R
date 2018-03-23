#' Title
#'
#' @param .data
#' @param col1
#' @param col2
#'
#' @return
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


