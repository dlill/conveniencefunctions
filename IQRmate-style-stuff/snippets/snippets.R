snippet IQRhead
	#### HEADER ================================================================
	#
	# `r basename(rstudioapi::getSourceEditorContext()$path)`
	#
	# [PURPOSE]
	# ${2:Purpose}
	#
	# [AUTHOR]
	# ${3:Author}
	#
	# [CLEANING]
	rm(list = ls(all.names = TRUE))
	#
	# [INPUT]
	.inputFolder <- "../"
	#
	# [OUTPUT]
	.outputFolder <- "../Output/`r sub("^SCRIPT", "", tools::file_path_sans_ext(basename(rstudioapi::getSourceEditorContext()$path)))`/"
	#
	# [OTHER]
	#

	## Preliminaries ====

	# Default packages (do not load other packages, use "::" instead)
	library(IQRtools)
	library(dplyr)
	library(ggplot2)


	IQRinitCompliance("`r basename(rstudioapi::getSourceEditorContext()$path)`")
	aux_version("IQRtools", "`r installed.packages()["IQRtools", "Version"]`")

snippet IQRsection
	# -------------------------------------------------------------------------#
	# ${1:SectionTitle} ----
	# -------------------------------------------------------------------------#

snippet IQRsubsection
	# -------------------------------------------------------------------------#
	# . ${1:SubsectionTitle} -----
	# -------------------------------------------------------------------------#

snippet IQRsubsubsection
	# -------------------------------------------------------------------------#
	# . . ${1:SubsubsectionTitle} ------
	# -------------------------------------------------------------------------#



snippet unittest
	# context("', ${1:context}, '")
	# test_that("', ${2:description} ,'", {

	#-!Start example code
	${0}

	#-!End example code


	# Define your expectations here

	#})


snippet clist
	# * ${0}

snippet cnllist
	#   ${0}

snippet cslist
	#   * ${0}

snippet csnllist
	#     ${0}

snippet cdodo
	# [] ${0}

snippet csdodo
	#   [] ${0}

snippet subsectionLIST
	# .. ${0} -----

snippet subsubsectionLIST
	# .... ${0} ------

snippet remark
	# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ----
	# >>>> ${0} <<<<<<<< ----
	# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ----

snippet clremark
	# >>>> ${0} <<<<<<<<<<<

snippet scriptdevelopment
	# >> Script development. # [] Remove in final script <<
	${0}

snippet Rprof_profile_line
	rp___ <- tempfile()
	Rprof(rp___)
	${0}
	Rprof(NULL)
	pv___ <- profvis::profvis(prof_input = rp___)
	htmlwidgets::saveWidget(pv___, paste0(rp___, ".html"))
	browseURL(paste0(rp___, ".html"))

snippet setwdDocument
	`r paste0("setwd('", dirname(rstudioapi::getSourceEditorContext()$path), "')")`

