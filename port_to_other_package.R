# Port the IQRtools.R file from dMod/R to IQRtools. ----
# For this, the :: declarations have to be changed

library(conveniencefunctions)


library(IQRtools)
library(dMod)

namespace_IQR_all <- ls(asNamespace("IQRtools"), all = T)
namespace_IQR <- ls(asNamespace("IQRtools"))
namespace_dMod_all <- ls(asNamespace("dMod"), all = T)
namespace_dMod <- ls(asNamespace("dMod"))

dMod_file <- readLines("~/PROJTOOLS/dMod/R/IQRtools.R")


# replace "IQRtools::" by "" ----
dMod_file <- dMod_file %>% str_replace_all(c("IQRtools:{2,3}" = ""))

# select lines which contain a dMod-function ----
contains_dMod_fn <- map_lgl(dMod_file,. %>% str_detect_any(fixed(namespace_dMod)))
mylines <- dMod_file[contains_dMod_fn]

# lines_to_change <- map(mylines, function(i) {
#   print(i)
#   readline("apply dMod:: ?\n")
# })
# saveRDS(lines_to_change, "lines_to_change.rds")
lines_to_change <- readRDS("lines_to_change.rds")
lines_to_change2 <- map_lgl(mylines,. %>% str_detect("theme_dMod")) # overlooked theme_dMod when creating "lines_to_change"
lines_noIQR_fn_definition <- map_lgl(mylines, . %>% str_detect("IQR.*<-") %>% `!`)

lines_to_change <- lines_to_change %>%
  do.call(c,.) %>%
  as.numeric() %>%
  as.logical() %>%
  `|`(lines_to_change2) %>%
  `&`(lines_noIQR_fn_definition)

# which dMod-functions appear in the line?
whichfns <- namespace_dMod %>% map_lgl(. %>% fixed %>% str_detect_any(mylines[lines_to_change],.))
fns_to_change <- namespace_dMod[whichfns]

# do the replacements
replacements <- structure(paste0("dMod::", fns_to_change), names = fns_to_change %>% str_replace("\\.", "\\\\.") %>% paste0("\\b", ., "\\b"))
mylines[lines_to_change] <- str_replace_all(mylines[lines_to_change], replacements)

dMod_file[contains_dMod_fn] <- mylines

dMod_file <- dMod_file %>%
  str_replace_all(c("dMod::([^(]*) =" = "\\1 =")) %>%
  str_replace_all(c("dMod::([^(]*) =" = "\\1 ="))%>%
  str_replace_all(c("\\.dMod::" = ".")) %>%
  str_replace_all(c("\\.dMod::" = "."))


internal_dMod_fns <- namespace_dMod_all[!namespace_dMod_all%in%ls("package:dMod")]

# dMod_file %>% map_lgl(. %>% str_detect_any(paste0("(\\w|\\.)",str_replace_all(internal_dMod_fns, "\\.", "\\\\."), "(\\w|\\.)"))) %>% {dMod_file[.]}
dMod_file %>% map_lgl(. %>% str_detect_any(fixed(internal_dMod_fns))) %>% {dMod_file[.]}



write_lines(dMod_file, "IQRtoolsdMod.R")

