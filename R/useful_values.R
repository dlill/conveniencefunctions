# Implement this "data" properly

#' @export
myvec <- structure(1:3, names = letters[1:3])

#' @export
mydf <- data.frame(col1 = letters[1:6],
                    col2 = factor(rep(LETTERS[1:3], each  =2)),
                    col3 = 1:6,
                    col4 = rnorm(6),
                    stringsAsFactors = F)
#' @export
mymat <- matrix(1, 3,3, F, list(letters[1:3], letters[1:3]))

#' @export
mytibble <- tibble::tibble(a = 1:4, b = c(1,1,2,2), d = letters[1:4])


# Include ABC models as dMod.frames
