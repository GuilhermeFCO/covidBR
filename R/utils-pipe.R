#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

globalVariables(
	c("accumCases",
		"accumDeaths",
		"ano",
		"city",
		"code",
		"code_meso",
		"code_micro",
		"code_muni",
		"code_region",
		"code_state",
		"geom",
		"gini",
		"idhm",
		"idhm_educ",
		"idhm_long",
		"idhm_renda",
		"ivs",
		"ivs_capital_humano",
		"ivs_infraestrutura_urbana",
		"ivs_renda_e_trabalho",
		"name_meso",
		"name_micro",
		"name_region",
		"name_state",
		"newCases",
		"newDeaths",
		"newFollowup",
		"newRecovered",
		"pop",
		"region",
		"renda_per_capita",
		"state")
)