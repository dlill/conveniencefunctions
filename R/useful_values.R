# Implement this "data" properly

#' Useful values
#' 
#' @export
myvec <- structure(1:3, names = letters[1:3])

#' @export
#' 
#' @rdname myvec
mydf <- data.frame(col1 = letters[1:6],
                    col2 = factor(rep(LETTERS[1:3], each  =2)),
                    col3 = 1:6,
                    col4 = rnorm(6),
                    stringsAsFactors = F)

#' @export
#' 
#' @rdname myvec
mymat <- matrix(1, 3,3, F, list(letters[1:3], letters[1:3]))

#' @export
#' 
#' @rdname myvec
mytibble <- tibble::tibble(a = 1:4, b = c(1,1,2,2), d = letters[1:4])

# Include ABC models as dMod.frames
