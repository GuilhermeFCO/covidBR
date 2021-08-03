#' Download data of covid in brasil
#'
#' @param dir to save data `directory`
#' @param maps with maps or no `boolean()`
#' @param date for maps, if NULL, is the last `date()`
#' @param withCorr for maps, with correlation of data or no `boolean()`
#' 
#' @return `message()`
#' 
#' @author Guilherme Fernandes Castro de Oliveira
#'
#' @export
#'
downloadCovidBR <- function(dir = "./", maps = FALSE, date = NULL, withCorr = FALSE) {
	
	quiet(
		brazil <- covid19br::downloadCovid19(level = "brazil")
		regions <- covid19br::downloadCovid19(level = "regions")
		states <- covid19br::downloadCovid19(level = "states")
		cities <- covid19br::downloadCovid19(level = "cities") %>% dplyr::filter(city != "")
		indexes <- data.table::fread("https://raw.githubusercontent.com/GuilhermeFCO/dataBaseCovid19/main/indexes.csv", encoding = "UTF-8")
		indexes$code <- stringr::str_sub(indexes$code, end = 6)
		
		brazil$code <- "brasil"
		brazil <- brazil %>% dplyr::relocate(code, .before = date)
		
		regions$code <- 0
		regions <- regions %>%
			dplyr::relocate(code, .before = date) %>%
			dplyr::relocate(region, .after = date)
		regions$code[regions$region == "North"] <- 1
		regions$region[regions$region == "North"] <- "Norte"
		
		regions$code[regions$region == "Northeast"] <- 2
		regions$region[regions$region == "Northeast"] <- "Nordeste"
		
		regions$code[regions$region == "Southeast"] <- 3
		regions$region[regions$region == "Southeast"] <- "Sudeste"
		
		regions$code[regions$region == "South"] <- 4
		regions$region[regions$region == "South"] <- "Sul"
		
		regions$code[regions$region == "Midwest"] <- 5
		regions$region[regions$region == "Midwest"] <- "Centro-Oeste"
		
		states$region <- NULL
		colnames(states)[colnames(states) == "state_code"] <- "code"
		states <- states %>% dplyr::relocate(code, .before = state)
		
		cities$region <- NULL
		cities$state <- NULL
		cities$state_code <- NULL
		colnames(cities)[colnames(cities) == "city_code"] <- "code"
		cities <- cities %>%
			dplyr::relocate(code, .before = city) %>%
			dplyr::relocate(date, .after = city)
		
		muni <- data.frame(geobr::lookup_muni(code_muni = "all"))
		muni$code_muni <- as.character(muni$code_muni)
		muni$code_muni <- stringr::str_sub(muni$code_muni, end = 6)
		muni$code_muni <- as.numeric(muni$code_muni)
		muni <- muni %>%
			dplyr::rename("abbrev_state" = "abrev_state")
		muni$name_meso <- stringr::str_to_title(muni$name_meso)
		muni$name_micro <- stringr::str_to_title(muni$name_micro)
		
		regionsAux <- geobr::read_state(code_state = "all", year = 2020)
		regionsAux$geom <- NULL
		regionsAux <- regionsAux %>% dplyr::select(code_state, code_region, name_region, name_state)
		regionsAux$name_region[regionsAux$name_region == "Centro Oeste"] <- "Centro-Oeste"
		
		muni <- dplyr::left_join(muni, (regionsAux %>% dplyr::select(code_state, code_region, name_region)), by = "code_state")
		
		colnames(states)[colnames(states) == "state"] <- "abbrev_state"
		states <- dplyr::left_join(states, regionsAux, by = c("code" = "code_state"))
		colnames(states)[colnames(states) == "name_state"] <- "state"
		states <- states %>% dplyr::relocate(state, .after = code)
		
		cities <- dplyr::left_join(cities, muni, by = c("code" = "code_muni"))
		
		meso <- geobr::read_meso_region(code_meso = "all", year = 2020)
		meso$geom <- NULL
		meso <- dplyr::left_join(meso, (regionsAux %>% dplyr::select(code_state, code_region, name_region)), by = "code_state")
		colnames(meso)[colnames(meso) == "code_meso"] <- "code"
		meso <- meso %>% dplyr::relocate(code, name_meso, .before = code_state)
		rm(regionsAux)
		
		micro <- geobr::read_micro_region(code_micro = "all", year = 2020)
		micro$geom <- NULL
		aux <- muni %>% dplyr::select(code_micro, code_meso, name_meso, code_region, name_region)
		aux <- subset(aux,
									subset = !duplicated(aux$code_micro))
		micro <- dplyr::left_join(micro, aux, by = "code_micro")
		micro <- micro %>% dplyr::relocate(code_micro, name_micro, .before = code_state)
		colnames(micro)[colnames(micro) == "code_micro"] <- "code"
		rm(aux)
		
		
		if (!dir.exists(paste0(dir, "rds/")))
			dir.create(paste0(dir, "rds/"))
		
		brazil <- dplyr::left_join(brazil, indexes, by = "code")
		saveRDS(object = brazil, file = paste0(dir, "rds/brazil.rds"))
		
		regions$code <- as.character(regions$code)
		regions <- dplyr::left_join(regions, indexes, by = "code")
		saveRDS(object = regions, file = paste0(dir, "rds/regions.rds"))
		
		states$code <- as.character(states$code)
		states <- dplyr::left_join(states, indexes, by = "code")
		saveRDS(object = states, file = paste0(dir, "rds/states.rds"))
		
		cities$code <- as.character(cities$code)
		cities <- dplyr::left_join(cities, indexes, by = "code")
		saveRDS(object = cities, file = paste0(dir, "rds/cities.rds"))
		
		aux <- cities %>%
			dplyr::group_by(code_meso, date) %>% 
			dplyr::summarise(pop = sum(pop),
											 accumCases = sum(accumCases),
											 newCases = sum(newCases),
											 accumDeaths = sum(accumDeaths),
											 newDeaths = sum(newDeaths),
											 newRecovered = sum(newRecovered),
											 newFollowup = sum(newFollowup),
											 ano = mean(ano),
											 ivs = mean(ivs, na.rm = TRUE),
											 ivs_infraestrutura_urbana = mean(ivs_infraestrutura_urbana, na.rm = TRUE),
											 ivs_capital_humano = mean(ivs_capital_humano, na.rm = TRUE),
											 ivs_renda_e_trabalho = mean(ivs_renda_e_trabalho, na.rm = TRUE),
											 idhm = mean(idhm, na.rm = TRUE),
											 idhm_long = mean(idhm_long, na.rm = TRUE),
											 idhm_educ = mean(idhm_educ, na.rm = TRUE),
											 idhm_renda = mean(idhm_renda, na.rm = TRUE),
											 renda_per_capita = mean(renda_per_capita, na.rm = TRUE),
											 gini = mean(gini, na.rm = TRUE))
		
		aux <- aux[!is.na(aux$code_meso),]
		meso <- dplyr::left_join(meso, aux, by = c("code" = "code_meso"))
		rm(aux)
		meso <- meso %>% dplyr::relocate(date, .after = name_meso)
		meso$code <- as.character(meso$code)
		saveRDS(object = meso, file = paste0(dir, "rds/meso.rds"))
		
		aux <- cities %>%
			dplyr::group_by(code_micro, date) %>%
			dplyr::summarise(pop = sum(pop),
											 accumCases = sum(accumCases),
											 newCases = sum(newCases),
											 accumDeaths = sum(accumDeaths),
											 newDeaths = sum(newDeaths),
											 newRecovered = sum(newRecovered),
											 newFollowup = sum(newFollowup),
											 ano = mean(ano),
											 ivs = mean(ivs, na.rm = TRUE),
											 ivs_infraestrutura_urbana = mean(ivs_infraestrutura_urbana, na.rm = TRUE),
											 ivs_capital_humano = mean(ivs_capital_humano, na.rm = TRUE),
											 ivs_renda_e_trabalho = mean(ivs_renda_e_trabalho, na.rm = TRUE),
											 idhm = mean(idhm, na.rm = TRUE),
											 idhm_long = mean(idhm_long, na.rm = TRUE),
											 idhm_educ = mean(idhm_educ, na.rm = TRUE),
											 idhm_renda = mean(idhm_renda, na.rm = TRUE),
											 renda_per_capita = mean(renda_per_capita, na.rm = TRUE),
											 gini = mean(gini, na.rm = TRUE))
		aux <- aux[!is.na(aux$code_micro),]
		micro <- dplyr::left_join(micro, aux, by = c("code" = "code_micro"))
		rm(aux)
		micro <- micro %>% dplyr::relocate(date, .after = name_micro)
		micro$code <- as.character(micro$code)
		saveRDS(object = micro, file = paste0(dir, "rds/micro.rds"))
		
		rm(list = c("brazil", "regions", "states", "meso", "micro", "cities"))
		
		if (maps)
			createMaps(dir = dir, date = date, withCorr = withCorr)
	)
		
	return(message("\n\nThe database was downloaded correctly.\n\n"))
}
