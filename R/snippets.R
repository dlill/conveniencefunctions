#' Install snippets
#'
#'
#' @return called for its side-effects. Restart RStudio afterwards
#' @export
install_IQRsnippets <- function(){
  
  if (!file.exists(snippets_path())){
    if (!dir.exists(dirname(snippets_path())))
    dir.create(dirname(snippets_path()), recursive = TRUE)
    writeLines("\n", snippets_path())
  }
  .snippetlist <- snippets_read("r", "snippets/r.snippets")
  lapply(names(.snippetlist), function(.x) try(snippet_remove(.x)))
  mapply(snippet_add, name = names(.snippetlist), text = .snippetlist, SIMPLIFY = FALSE)
}



# -------------------------------------------------------------------------#
# Copied from snippr ----
# -------------------------------------------------------------------------#
# * Since snippr is only on github, it is a pita to install it. Luckily, it has license GPL2.0 which means we are allowed to copy and modify it.
# * All functions below are taken from the snippr-package: https://github.com/dgrtwo/snippr/

# * License declaration
# The rest of this file is part of snippr (https://github.com/dgrtwo/snippr/).
#
# snippr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# snippr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with snippr.  If not, see <https://www.gnu.org/licenses/>.

#' copied from snippr
snippets_path <- function (language = "r"){
  path <- file.path("~/.R/snippets", paste0(language, ".snippets"))
  if (!(file.exists(path))) {
    stop("snippets file ", path, "not found")
  }
  path
}

#' copied from snippr
snippet_add <- function (name, text, language = "r", path = "~/.R/snippets/r.snippets"){
  current <- snippets_read(path = path)
  if (!is.null(current[[name]])) {
    message("Replacing existing snippet ", name)
  }
  current[[name]] <- snippet_prepare(text)
  snippets_write(current, path = path)
}

#' copied from snippr
#' @importFrom stringr str_split str_detect
snippet_prepare <- function (text){
  lines <- do.call(c, stringr::str_split(text, "\\n"))
  if (!all(stringr::str_detect(lines[lines != ""], "^\t"))) {
    lines <- paste0("\t", lines)
  }
  paste(lines, collapse = "\n")
}

#' copied from snippr
snippet_remove <- function (name, language = "r", path = "~/.R/snippets/r.snippets",
                            error = TRUE){
  if (file.exists(path)) {
    current <- snippets_read(path = path)
    if (is.null(current[[name]])) {
      if (error) {
        stop("Snippet ", name, " not found to remove")
      }
      return()
    }
  }
  current[[name]] <- NULL
  snippets_write(current, path = path)
}


#' copied from snippr
snippets_read <- function (language = "r", path = "~/.R/snippets/r.snippets"){
  if (file.exists(path)) {
    lines <- readLines(path)
    return(snippets_parse(lines))
  }
  return(NULL)
}


"%>%" <- dplyr::"%>%"


#' copied from snippr
#' @importFrom dplyr data_frame
#' @importFrom stringr str_split str_match
snippets_parse <- function (txt){
  lines <- do.call(c, stringr::str_split(txt, "\\n"))
  d <- dplyr::data_frame(line = lines, snippet = str_match(line,
                                                           "^snippet (.*)")[, 2], group = cumsum(!is.na(snippet)))
  snippets <- d %>% split(d$group) %>% lapply(function(d) paste(d$line[-1],
                                                                collapse = "\n"))
  snippets <- Filter(function(s) s != "", snippets)
  names(snippets) <- d$snippet[!is.na(d$snippet)]
  snippets
}

#' copied from snippr
snippets_write <- function (snippets, path){
  if (!file.exists(path)) {
    cat("",file = path)
  }
  snippet_txt <- paste0("snippet ", names(snippets), "\n",
                        as.character(snippets), collapse = "\n")
  writeLines(snippet_txt, path)
}