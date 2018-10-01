# tibble-stuff ----

#' turn a matrix into a wide data.frame
#'
#' @param mat a matrix
#' @param nm vector of names for the matrix elements
#'
#' @return
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


#' ggsave() with automatically saving metainformation of how the plot was produced
#'
#' @param all taken from ggsave()
#' @param check_overwrite Asks before overwriting a plot
#' @param commit_script Lgl o character. Commit the script which generated th plot. Can be logical or commit message.
#' @param commit_plot Lgl o character. Commit the plot in its git repository. Can be logical or commit message.
#'
#' @export
ggshave <- function (filename, plot = last_plot(), device = NULL, path = NULL,
                     scale = 1, width = NA, height = NA, units = c("in", "cm",
                                                                   "mm"), dpi = 300, limitsize = TRUE, ..., check_overwrite = T,
                     commit_script = F, commit_plot = F)
{


  if (file.exists(filename) & check_overwrite) {
    overwrite <- readline("Plot exists already. Overwrite? (Enter 'n/no' for no.) ")
    if (str_detect(overwrite, "n|(no)"))
      return(NULL)
  }
  ggsave(filename, plot, device, path, scale, width, height,
         units, dpi, limitsize, ...)
  document <- rstudioapi::getActiveDocumentContext()
  rstudioapi::documentSave(document$id)
  if (is.character(commit_script) | commit_script == T) {
    repo <- git2r::repository()
    git2r::add(repo, document$path)
    message <- NULL
    if (is.character(commit_script))
      message <- commit_script
    git2r::commit(repo, message = paste("Generated plot.",
                                        message))
  }
  git_head <- git2r::commits() %>% .[[1]]

  mytable <- tibble(Script = document$path,
                    `Row in script` = document$selection[[1]]$range[[1]][1],
                    `Commit script message` = git_head$message, When = lubridate::as_datetime((git_head$author$when$time +
                                                                                                 git_head$author$when$offset)),
                    `Script Git SHA` = git_head$sha, `Commit plot` = (is.character(commit_plot) |
                                                                        commit_plot == T))
  auxiliary_file <- filename %>% str_replace("(\\.pdf$)|(\\.png$)",
                                             "") %>% paste0(".csv")
  append <- file.exists(auxiliary_file)
  write_csv(mytable, path = auxiliary_file, append = append)
  if (is.character(commit_plot) | commit_plot == T) {
    repo <- git2r::discover_repository(filename)
    git2r::add(repo, filename)
    git2r::add(repo, auxiliary_file)
    message <- NULL
    if (is.character(commit_plot))
      message <- commit_plot
    git2r::commit(repo, message = paste("Generated plot.",
                                        message))
  }
  return(NULL)
}
