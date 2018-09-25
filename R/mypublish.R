#' Standardized outputs for different dMod-Objects
#'
#' @param x Model which should be published
#' @param ... Arguments going to methods
#'
#' @export
mypublish <- function(x,...) {
  UseMethod("mypublish", x)
}


#' Print the odes of an eqnvec in a tex and a docx table
#'
#' @param x eqnlist
#'
#' @export
mypublish.eqnvec <- function(x) {
  mynames <- x %>% names
  x %>%
    replaceSymbols(what = mynames, by = paste0("[",mynames, "]"), x = .) %>%
    {`names<-`(paste0("$ ", ., " $"), paste0("$d/dt[ ", names(.), "]$"))} %>%
    tibble(names(.),.) %>%
    xtable() %>% print(include.rownames = F, include.colnames = F) %>%
    paste("\\documentclass[10pt,a4paper]{article}
\\usepackage[latin1]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\begin{document}", ., "\\end{document}")  %>%
    str_replace_all( "\\\\\\$" , "$")  %>%
    write_lines("odes.tex")
  system2("pandoc", c("-o odes.docx", "odes.tex"))
}

