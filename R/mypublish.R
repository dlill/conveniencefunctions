#' Print the odes of an eqnvec in a tex and a docx table
#'
#' printout of c(x = -k\*x) will look like d/dt\[x] = - k*[x]
#'
#' @param x eqnvec
#'
#' @export
mypublish_eqnvec2wordtable <- function(x, path = "odes") {
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
    write_lines(paste0(path, ".tex"))
  system2("pandoc", c(paste0("-o ", path, ".docx"), paste0(path, ".tex")))
}


#' Print the odes of an eqnvec in a tex and a docx table
#'
#' printout of c(x = -k*x) will look like d/dt\[x\] = - k\*\[x\]
#'
#' @param x eqnvec
#'
#' @export
mypublish_namedvec2wordtable <- function(x, path = "pars") {
  mynames <- x %>% names
  x %>%
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
    write_lines(paste0(path, ".tex"))
  system2("pandoc", c(paste0("-o ", path, ".docx"), paste0(path, ".tex")))
}


#' Print single matrices as matrices in word math environemt
#'
#' The align argument of xtable outside of mypublish mat needs to be ncol(mat) \+ 1
#'
#' @param x a single matrix as xtable \%>\% print(tabluar.environment = "pmatrix)
#' @param filename filename without ending
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list(r_0 = matrix(1:9, ncol = 3),
#'      r_1 = matrix(0,ncol = 3, nrow = 3),
#'      r_opt_1 = matrix(2:10, nrow = 3)) %>%
#'   imap(~.x %>% round(2) %>% xtable(align = rep("",4)) %>%
#'          print(tabular.environment="pmatrix",
#'                include.colnames = F,
#'                include.rownames = F,
#'                floating = F,
#'                hline.after = NULL) %>%
#'          mypublish_mat(.y)
#'        )
#' }
mypublish_matrix2wordmath <- function(x,filename) {

  x %>%
    paste("\\documentclass[10pt,a4paper]{article}
\\usepackage[latin1]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\begin{document}",
          "\\begin{equation*}",
          filename, " = ", .,
          "\\end{equation*}",
          "\\end{document}") %>%
    write_lines(paste0(filename,".tex"))
  system2("pandoc", c(paste0("-o", filename,".docx"), paste0(filename,".tex")))}




#' Print a list of matrices to a single word file with a table of these matrices
#'
#' @param x list of matrices
#' @param path filename
#' @param digits digits for rounding
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list(r_0 = matrix(1:9, ncol = 3),
#'      r_1 = matrix(0,ncol = 3, nrow = 3),
#'      r_opt_1 = matrix(2:10, nrow = 3)) %>%
#'   imap( ~.x %>% round(2) %>%
#'           xtable(align = rep("",4)) %>%
#'           print(tabular.environment="pmatrix",
#'                 include.colnames = F,
#'                 include.rownames = F,
#'                 floating = F,
#'                 hline.after = NULL)) %>%
#'           mypublish_mat("matrices")
#' }
mypublish_matlist2wordtable <- function(x, path, digits = 2) {
  tables <- map(x, . %>% xtable::xtable(digits = digits) %>% {xtable:::print.xtable(invisible(.), include.rownames = F)}) %>%
    paste0(collapse = "\n\n")

  tables %>%
    paste("\\documentclass[10pt,a4paper]{article}
\\usepackage[latin1]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\begin{document}", ., "\\end{document}") %>%
    write_lines(paste0(path,".tex"))
  system2("pandoc", c(paste0("-o ",path,".docx"), paste0(path,".tex")))
}




#' @export
#' @rdname mypublish_eqnvec2wordtable
print2word_eqnvec <- mypublish_eqnvec2wordtable

#' @export
#' @rdname mypublish_matlist2wordtable
print2word_matrixlist <- mypublish_matlist2wordtable

#' @export
#' @rdname mypublish_matrix2wordmath
print2word_matrix <- mypublish_matrix2wordmath

#' @export
#' @rdname mypublish_namedvec2wordtable
print2word_namedvec <- mypublish_namedvec2wordtable






