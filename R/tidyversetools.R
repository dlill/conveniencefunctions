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






#' ggsave() with automatically saving metainformation of how the plot was produced
#'
#' @param all taken from ggsave()
#' @param check_overwrite Asks before overwriting a plot
#' @param commit_script Lgl o character. Commit the script which generated th plot. Can be logical or commit message.
#' @param commit_plot Lgl o character. Commit the plot in its git repository. Can be logical or commit message.
#'
#' @export
ggshave <- function(filename, plot = last_plot(), device = NULL, path = NULL,
                    scale = 1, width = NA, height = NA, units = c("in", "cm","mm"),
                    dpi = 300, limitsize = TRUE, ..., check_overwrite = T, commit_script = F, commit_plot) {

  # Prevent from overwriting
  if(file.exists(filename) & check_overwrite) {
    overwrite <- readline("Plot exists already. Overwrite? (Enter 'n/no' for no.) ")
    if(str_detect(overwrite, "n|(no)")) return(NULL)
  }

  # Save plot
  ggsave(filename, plot, device, path,
         scale, width, height, units, dpi, limitsize, ...)

  # Get script information
  document <- rstudioapi::getActiveDocumentContext()

  # Commit the script
  if(is.character(commit_script)|commit_script==T) {
    repo <- git2r::repository()
    git2r::add(repo, document$path)
    message <- NULL
    if(is.character(commit_script)) message <- commit_script
    git2r::commit_script(repo, message = paste("Generated plot.", message))
  }

  # Write additional information
  git_head <- git2r::commits() %>% .[[1]]
  mytable <- tibble("Produced by" = document$path,
                    "Row in document" = document$selection[[1]]$range[[1]][1],
                    "Commit message" = git_head@message,
                    "Git SHA" = git_head@sha)
  write_csv(mytable, path = filename %>% str_replace("(\\.pdf$)|(\\.png$)", "") %>% paste0(".csv"))

  # Commit the plot in its git directory
  if(is.character(commit_plot)|commit_plot==T) {
    repo <- git2r::discover_repository(filename)
    git2r::add(repo, filename)
    git2r::add(repo, filename %>% str_replace("(\\.pdf$)|(\\.png$)", "") %>% paste0(".csv"))

    message <- NULL
    if(is.character(commit_plot)) message <- commit_plot
    git2r::commit_plot(repo, message = paste("Generated plot.", message))
  }

  return(NULL)
}
