
# dMod.frame class ----------------------------------------------------------------

#' Generate a dMod.frame object
#'
#' @description The dMod.frame object stores all objects that are needed to reproducibly
#' produce a result, such as a plot or profiles, in one row of a \link{tibble}.
#' Each row corresponds to a distinct hypothesis, e.g. one would have two distinct rows
#' for fitting with and without a prior.
#'
#' Since the dMod.frame is also designed for scripting, the class will not be called
#' "dMod.frame" as I initially planned, but will be c("tbl_df", "tbl", "data.frame").
#' This way, I don't have to copy all the dplyr-verbs.
#'
#'
#' @param hypothesis Character. Description of the hypothesis
#' @param g fn
#' @param x fn
#' @param p fn
#' @param data data.frame or datalist, will be coerced to datalist
#' @param e fn
#' @param ...
#'
#' @return Object of class \code{tbl_df}.
#'
#' @importFrom dplyr tibble rowwise
#'
#' @export
#'
#' @example
#' \dontrun{example("mstrust", run.dontrun = T, ask = F)}
dMod.frame <- function(hypothesis, g, x, p, data, e = NULL,...) {

  enlist <- function(x) {
    if (is.list(x) & (!(is.data.frame(x)|is.datalist(x))) ) return(x)
    else return(list(x))
  }

  out <- tibble(hypothesis = hypothesis,
         g = enlist(g),
         x = enlist(x),
         p = enlist(p),
         data = enlist(as.datalist(data)),
         e = enlist(e),
         ...)
  out <- rowwise(out)

  return(out)
}

as.dMod.frame <- function(x) {
  UseMethod("as.dMod.frame", x)
}

as.dMod.frame.tbl_df <- function(x) {

  if (any(!(c("hypothesis", "g", "x", "p", "data", "e") %in% names(x)))) warning("This frame does not contain all basic columns")

  class(x) <- c("dMod.frame", class(x))
  return(x)
}

as.dMod.frame.data.frame <- function(x) {
  if (any(!(c("hypothesis", "g", "x", "p", "data", "e") %in% names(x)))) warning("This frame does not contain all basic columns")

  return(as.dMod.frame.tbl_df(as.tibble(x)))
}


# myframe <- dMod.frame(hypothesis = "hyp", g, x, p0, data)

# frm <- rbind(myframe, myframe)
# frm$hypothesis[2] <- "hyp2"



#' Title
#'
#' @param dMod.frame
#' @param ...
#' @param keepCalls
#'
#' @return
#' @export
#'
#' @examples
mutatedMod.frame <- function(dMod.frame,
                             ...,
                             keepCalls = F) {

  args <- quos(...)

  if (keepCalls) {
    if (is.null(dMod.frame[[".calls"]])) dMod.frame <- mutate(dMod.frame, .calls = list(NULL))
    return(mutate(rowwise(dMod.frame), UQS(args), .calls = list(c(.calls, list(args)))))
  } else {
    return(mutate(rowwise(dMod.frame), UQS(args)))
  }

}


#' Append an objective function to a basic dMod.frame
#'
#' @param dMod.frame
#' @param prd
#' @param obj_data
#' @param obj
#' @param ...
#' @param keepCalls
#'
#' @importFrom rlang quos enquo UQS
#' @importFrom dplyr mutate rowwise
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' appendObj(myframe) %>%
#' .[[".calls"]] %>%
#'   # str2 %>%
#' {.}
#' }
appendObj <- function(dMod.frame,
                      prd = list(g*x*p),
                      obj_data = list(normL2(data, prd, e)),
                      obj = list(obj_data),
                      pars = list(structure(rnorm(length(getParameters(obj))), names = getParameters(obj))),
                      times = list(seq(min(as.data.frame(data)[["time"]]), max(as.data.frame(data)[["time"]])*1.1, length.out = 100)),
                      ...,
                      keepCalls = F) {

  args <- c(list(prd = enquo(prd),
                 obj_data = enquo(obj_data),
                 obj = enquo(obj),
                 pars = enquo(pars),
                 times = enquo(times)
                 ),
            quos(...))

  mutatedMod.frame(dMod.frame, UQS(args), keepCalls = keepCalls)

}




#' Title
#'
#' @param dMod.frame
#' @param parframes
#' @param ...
#' @param keepCalls
#'
#' @return
#' @export
#'
#' @examples
appendParframes <- function(dMod.frame,
                            parframes = list(as.parframe(fits)),
                            ...,
                            keepCalls = F) {

  args <- c(list(parframes = enquo(parframes)), quos(...))

  mutatedMod.frame(dMod.frame, UQS(args), keepCalls = keepCalls)

}





#' Redo calls that are saved in the .calls - column
#'
#' @param dMod.frame A dMod.frame with a record of the calls
#' @param whichCalls Numeric indices or character to subset .calls by name
#'
#' @description This guy doesn't work because the calls get wrapped into lists again
#'
#' @return
#' @export
#'
#' @importFrom rlang UQS
#' @importFrom dplyr mutate rowwise
#'
#' @examples
reMutateCalls <- function(dMod.frame, whichCalls) {
  if (missing(whichCalls)) whichCalls <- unique(names(unlist(dMod.frame[[".calls"]])))

  do.call(rbind,
          lapply(1:nrow(dMod.frame), function(i) {
            calls <- dMod.frame[[".calls"]][[i]][[1]]
            calls <- calls[whichCalls]
            mutate(rowwise(dMod.frame[i,]), UQS(calls)) # works
          })
  )
}
# reMutateCalls(frm2) %>% print()


# frm2 <- rbind(myframe, myframe) %>% appendObj
# frm2[[".calls"]][[1]][[1]] %>% names




#
# a <- 1
# d <- 2
# myfun <- function(a) {
#   # The quosure quo(a) has environment .GlobalEnv
#   quoa <- enquo(a)
#   print(environment(quoa))
#   a <- 2
#   print(quoa)
#   eval_tidy(quoa) %>% print %>% cat(.,"quoa\n")
#   # expr/enexpr is everything without environments
#   expra <- enexpr(a)
#   print(expra)
#   eval_tidy(expra) %>% cat(.,"expra\n")
#   # The quosure quob has the environment created by the function, but tidy_eval evaluates each quosure IN this quosure in their respective environments?
#   quob <- quo(paste("a", UQ(quoa)))
#   print(environment(quob))
#   eval_tidy(quob) %>% print
#
#   eval_bare(quob) %>% print
#
#   # Except for when it is called in mutate (and probably other dplyr verbs)
#   df <- tibble(a = 3)
#   mutate(df, UQ(quob), d=d)
#
#   # return(NULL)
# }
# myfun(a)

# dplyr:::mutate.tbl_df
# dplyr:::named_quos
# dplyr:::mutate_impl
# dplyr:::`_dplyr_mutate_impl`

# myfun2 <- function(a = 1, b = 2, ...) {
#   match.call()
#   # match.call()$a
# }
# myfun2(a = 2, c = 3)
#
# mymatch <- function (FUN, descend = TRUE)
# {
#   eval.parent(substitute(substitute(FUN)))
#   # if (is.function(FUN))
#   #   return(FUN)
#   # if (!(is.character(FUN) && length(FUN) == 1L || is.symbol(FUN))) {
#   #   FUN <- eval.parent(substitute(substitute(FUN)))
#   #   if (!is.symbol(FUN))
#   #     stop(gettextf("'%s' is not a function, character or symbol",
#   #                   deparse(FUN)), domain = NA)
#   # }
#   # envir <- parent.frame(2)
#   # if (descend)
#   #   FUN <- get(as.character(FUN), mode = "function", envir = envir)
#   # else {
#   #   FUN <- get(as.character(FUN), mode = "any", envir = envir)
#   #   if (!is.function(FUN))
#   #     stop(gettextf("found non-function '%s'", FUN), domain = NA)
#   # }
#   # return(FUN)
# }
# mymatch(myfun)
#



# mypage <- function (x, method = c("dput", "print"), ...)
# {
#   local.file.show <- function(file, title = subx, delete.file = TRUE,
#                               pager = getOption("pager"), ...) file.show(file, title = title,
#                                                                          delete.file = delete.file, pager = pager)
#   local.dput <- function(x, file, title, delete.file, pager,
#                          ...) dput(x, file, ...)
#   local.print <- function(x, title, delete.file, pager, ...) print(x,
#                                                                    ...)
#   if (is.character(x) && length(x) == 1L) {
#     subx <- x
#     parent <- parent.frame()
#     if (exists(subx, envir = parent))
#       x <- get(subx, envir = parent)
#     else stop(gettextf("no object named '%s' to show", x),
#               domain = NA)
#   }
#   else {
#     subx <- deparse(substitute(x))
#   }
#   file <- tempfile("Rpage.")
#   if (match.arg(method) == "dput")
#     local.dput(x, file, ...)
#   else {
#     print(subx)
#
#     sink(file)
#     local.print(x, ...)
#     sink()
#   }
#   local.file.show(file, ...)
# }





#
#
# mutateListCols(myframe,
#                prd = g*x*p,
#                obj_data = normL2(data,prd,e),
#                obj = obj_data)
#
#











