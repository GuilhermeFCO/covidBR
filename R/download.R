downloadCovidBR <- function(dir = "./") {

	brazil <- covid19br::downloadCovid19(level = "brazil")
	regions <- covid19br::downloadCovid19(level = "regions")
	states <- covid19br::downloadCovid19(level = "states")
	cities <- covid19br::downloadCovid19(level = "cities") %>% dplyr::filter(city != "")

	indexes <- data.table::fread("https://raw.githubusercontent.com/GuilhermeFCO/dataBaseCovid19/main/indices.csv")



}
