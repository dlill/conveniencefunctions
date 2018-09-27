# Implement this "data" properly

#' @export
.myvec <- structure(1:3, names = letters[1:3])

#' @export
.mydf <- data.frame(col1 = letters[1:6],
                    col2 = factor(rep(LETTERS[1:3], each  =2)),
                    col3 = 1:6,
                    col4 = rnorm(6),
                    stringsAsFactors = F)

# Include ABC models as dMod.frames
