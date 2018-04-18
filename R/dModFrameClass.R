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
#'
#' @param hypothesis
#' @param g
#' @param x
#' @param p
#' @param data
#' @param ...
#'
#' @return Object of class \code{dMod.frame}.
#'
#' @importFrom dplyr tibble
#'
#' @export
#'
#' @example
#'
#'
#'

# example("mstrust", run.dontrun = T, ask = F)

dMod.frame <- function(hypothesis, g, x, p, data, e = NULL,...) {

  enlist <- function(x) {
    if (is.list(x) & (!(is.data.frame(x)|is.datalist(x))) ) return(x)
    else return(list(x))
  }

  out <- tibble(hypothesis = hypothesis,
         g = enlist(g),
         x = enlist(x),
         p = enlist(p),
         data = enlist(data),
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


myframe <- dMod.frame(hypothesis = "hyp", g, x, p, data)

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


mutateListCols <- function(dMod.frame, ...) {
  # capture dotted expressions, transform them into index-wise expression
  dots <- quos(...)
  # dots <- lapply(dots, function(i) {
  #
  #   myexpr <- quo_text(UQ(i))
  #   mysymbols <- getSymbols(myexpr)
  #   mysymbols <- mysymbols[mysymbols %in% names(dMod.frame)]
  #
  #   # myexpr <- replaceSymbols(mysymbols,paste0(mysymbols, "[[i]]"), myexpr) # version 1
  #   # myexpr <- replaceSymbols(mysymbols,paste0(".$", mysymbols), myexpr)    # version 2
  #   myexpr <- paste0("list(", myexpr, ")")                                   # version 3
  #   parse_quosure(myexpr)
  # })

  dots <-lapply(dots, function(i) quo(list(UQ(i))))                         # version 3.1

  print(dots)
  # dots <- lapply(dots, function(j) {                                      # version 1
  #   environment(j) <- pos.to.env(-1)                                      # version 1
  # nrow <- force(seq_len(nrow(dMod.frame)))                                # version 1
  #   quo(lapply(UQ(nrow), function(i) {UQ(j)}))                            # version 1
  # })                                                                      # version 1
    # mutate(dMod.frame, UQS(dots))                                         # version 1



   # out <- as.dMod.frame(cbind(dMod.frame, do(rowwise(dMod.frame), UQS(dots)))) # not the best solution with cbind?  # version 2
   # return(out)
  dMod.frame %>% rowwise %>% mutate(UQS(dots)) %>% as.dMod.frame() # version 3

}


frm <- tibble(a = list(1,2,3), b = list(1,2,3))
mutateListCols(frm, sm = a+b, dif = a-b) #%>% str2

mutateListCols(dMod.frame = myframe,
               # wupwup= g*x*p,
               prd = g*x*p) #%>% str2


# frm %>% rowwise %>% do(prd = cfn(.$g,cfn(.$x,.$p)))
frm %>% rowwise %>% do(prd = .$g*.$x*.$p)

myframe %>% rowwise %>% mutate(prd = list(g1*x1*p1))

appendObj <- function(dMod.frame, prd = g*x*p, ...) {



}

appendObj(myframe, wupwup= g*x*p, prd2 = g*x*p) %>% str2

