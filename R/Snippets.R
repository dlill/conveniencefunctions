# # Some snippets to insert in edit snippets
#
# snippet section
#   # ------------------------------------------------------------- #
#   # ${1:section} ----
#   # ------------------------------------------------------------- #
#
# snippet explanation
#   #
#   #
#   #
#   #
#
# snippet insert_header
#   `r paste0("library(conveniencefunctions)", "\n",
#             "rm(list = ls())", "\n",
#             "load('workspace.rda')", "\n",
#             "oldwd <- '", getwd(), "'\n",
#             "projwd <- '", rstudioapi::getActiveProject(), "'\n",
#             "scriptwd <- '", dirname(rstudioapi::getActiveDocumentContext()$path), "'\n",
#             "setwd('", projwd, "'))`

