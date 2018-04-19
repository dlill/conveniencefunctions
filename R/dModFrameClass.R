# datalist class

#' @param x
#' @export
#' @rdname datalist
is.datalist <- function(x) {
  "datalist" %in% class(x)
}


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
#' @importFrom dplyr tibble
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
  class(out) <- c("dMod.frame", class(out))

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


#
# dMod.frame %>%
#   mutate(data_0 = map(data_0, as.datalist),
#          data_sig = map(data_sig, as.datalist)) %>%
#   mutate(prd_0 = map(seq_along(x), function(i) cfn(g[[i]], cfn(x[[i]],p_0[[i]]))),
#          prd_sig = map(seq_along(x), function(i) cfn(g[[i]], cfn(x[[i]],p_sig[[i]])))) %>%
#   mutate(obj_0 = map(seq_along(x), function(i) normL2(data_0[[i]], prd_0[[i]])),
#          obj_sig = map(seq_along(x), function(i) cf_normL2(data = data_sig[[i]], prd_sig[[i]], e = e[[i]]))) %>%
#
#   mutate(obj = map(seq_along(x), function(i) obj_sig[[i]]+obj_0[[i]])) %>%
#   mutate(prd = map(seq_along(x), function(i) prd_sig[[i]]+prd_0[[i]])) %>%
#   mutate(data= map(seq_along(x), function(i) data_sig[[i]]+data_0[[i]]))
#


#' Mutate List-Columns in a dMod.frame
#'
#' @description This function serves two purposes:
#' 1. Provide a mutate()-like function for processing list-columns.
#' Instead of having to do mutate(newcol = lapply(1:nrow(.), function(i) x[[i]]))
#' or, which already feels a bit better . %>% rowwise %>% mutate (but here you
#' might not always want to wrap your call in list())
#' 2. Automatically append a column which keeps track of the calls that were used
#' to create new columns.
#'
#' I'm not sure if I have implemented it in the best possible way, even though
#' it has gone through three major iterations already.
#' Main questions are:
#' 1. Would it have been possible to do it neatly in base R?
#' 2. Are quosures the best framework to use or would rlang::exprs() have been a
#' better choice?
#'
#' @param dMod.frame A dMod.frame (also takes a tibble)
#' @param appendExprs Logical. Should the calls be recorded or not?
#' @param ... Expressions passed to mutate
#'
#' @return A dMod.frame with new list-columns added
#' @export
#'
#' @importFrom rlang quos quo UQ UQS
#' @importFrom dplyr mutate rowwise
#' @examples
#' myframe <- readRDS("myframe.rds")
#' mutateListCols(dMod.frame = rbind(myframe, myframe), prd = g*x*p) %>%
#'   mutateListCols(prd2 = g*prd) %>%
#'   mutateListCols(prd3 = prd + prd2) %>%
#'   str2 %>%
#'   {.}
mutateListCols <- function(dMod.frame, ..., appendCalls = T) {
  dots <- quos(...)
  dots <-lapply(dots, function(i) quo(list(UQ(i))))

  if (appendCalls) {
    if (is.null(dMod.frame[[".calls"]])) dMod.frame <- mutate(dMod.frame, .calls = list(NULL))
    return(as.dMod.frame(mutate(rowwise(dMod.frame), UQS(dots), .calls = list(c(.calls, list(dots))))))
  } else {
    return(as.dMod.frame(mutate(rowwise(dMod.frame), UQS(dots))))
  }
}


#' Append an objective function to a basic dMod.frame
#'
#' @param dMod.frame
#' @param prd
#' @param obj_data
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' appendObj(myframe) %>%
#' .[[".calls"]] %>%
#'   # str2 %>%
#' {.}
appendObj <- function(dMod.frame, prd = (g*x*p), obj_data = normL2(data, prd, e), obj = obj_data, ...) {
  args <- c(list(prd = enquo(prd), obj_data = enquo(obj_data), obj = enquo(obj)), quos(...))
  mutateListCols(dMod.frame, UQS(args))
}
#

#' Redo calls that are saved in the .calls - column
#'
#' @description This guy doesn't work because the calls get wrapped into lists again
#'
#' I think it would be best to discard mutateListCols and use . %>% rowwise %>% mutate(list())
#'
#' @param dMod.frame
#'
#' @return
#' @export
#'
#' @examples
reMutateCalls <- function(frm2, whichCalls) {
  if (missing(whichCalls)) whichCalls <- unique(names(unlist(frm2[[".calls"]])))

  lapply(1:nrow(frm2), function(i) {
    calls <- frm2[[".calls"]][[i]][[1]]
    calls <- calls[names(calls)%in%whichCalls]
    # mutateListCols(frm2[i,], UQS(calls), appendCalls = F) #doesnt work
    mutate(rowwise(frm2[i,]), UQS(calls)) # works
  })

}
reMutateCalls(frm2) %>% print()


frm2 <- rbind(myframe, myframe) %>% appendObj
frm2[[".calls"]][[1]][[1]] %>% names





a <- 1
d <- 2
myfun <- function(a) {
  # The quosure quo(a) has environment .GlobalEnv
  quoa <- enquo(a)
  print(environment(quoa))
  a <- 2
  print(quoa)
  eval_tidy(quoa) %>% print %>% cat(.,"quoa\n")
  # expr/enexpr is everything without environments
  expra <- enexpr(a)
  print(expra)
  eval_tidy(expra) %>% cat(.,"expra\n")
  # The quosure quob has the environment created by the function, but tidy_eval evaluates each quosure IN this quosure in their respective environments?
  quob <- quo(paste("a", UQ(quoa)))
  print(environment(quob))
  eval_tidy(quob) %>% print

  eval_bare(quob) %>% print

  # Except for when it is called in mutate (and probably other dplyr verbs)
  df <- tibble(a = 3)
  mutate(df, UQ(quob), d=d)

  # return(NULL)
}
myfun(a)

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











