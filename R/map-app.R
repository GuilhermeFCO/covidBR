library(shiny)
library(dplyr)
library(plotly)
source("./R/get-query.R")

map <- function(df, variable, title, color, legend, direction) {
	p <- df %>%
		ggplot2::ggplot() +
		ggplot2::geom_sf(ggplot2::aes(fill = variable, geometry = geom),
										 color = NA,
										 size=.15) +
		labs(title = title) +
		scale_fill_distiller(palette = color,
												 direction = direction,
												 limits = c(min(df$variable), max(df$variable)),
												 name = legend) +
		theme_void()
	return(p)
}

mapUi <- shiny::fluidPage(
	plotly::plotlyOutput("out")
)

mapServer <- function(input, output, session) {

	rv <- shiny::reactiveValues(
		df = "",
		variable = "",
		title = "",
		color = "",
		legend = "",
		direction = ""
	)

	shiny::observe({
		query <- shiny::parseQueryString(session$clientData$url_search)
		rv$df <- get_query(query, "df")
		rv$variable <- get_query(query, "variable")
		rv$title <- get_query(query, "title")
		rv$color <- get_query(query, "color")
		rv$legend <- get_query(query, "legend")
		rv$direction <- get_query(query, "direction")
	})

	output$out <- shiny::renderText({
		p <- NULL

		if (rv$df != "" &
				rv$variable != "" &
				rv$title != "" &
				rv$color != "" &
				rv$legend != "" &
				rv$direction != "") {
			p <- map(df = rv$df,
							 variable = rv$variable,
							 title = rv$title,
							 color = rv$color,
							 legend = rv$legend,
							 direction = as.numeric(rv$direction))
		}

		return(p)
	})
}

shiny::shinyApp(mapUi, mapServer)
