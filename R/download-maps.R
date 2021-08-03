#' Create maps with database of covidBR
#'
#' @param dir of your database generated with downloadCovidBR function `directory`
#' @param date for maps, if NULL, is the last `date()`
#' @param withCorr make a correlation with data or no `boolean()`
#'
#' @return `message()`
#'
#' @author Guilherme Fernandes Castro de Oliveira
#' 
#' @export
#' 
createMaps <- function(dir = "./", date = NULL, withCorr = FALSE) {
	
	if (!dir.exists(paste0(dir, "rds")) |
			!file.exists(paste0(dir, "rds/cities.rds")) |
			!file.exists(paste0(dir, "rds/meso.rds")) |
			!file.exists(paste0(dir, "rds/micro.rds")) |
			!file.exists(paste0(dir, "rds/regions.rds")) |
			!file.exists(paste0(dir, "rds/states.rds"))) {
		
		return(message("\n\nPlease use downloadCovidBR function in the same directory first!\n\n"))
	}
	
	quiet(
		if (!dir.exists(paste0(dir, "maps"))) 
			dir.create(paste0(dir, "maps"))
		
		regions <- readRDS(paste0(dir, "rds/regions.rds"))
		
		aux <- data.frame(geobr::read_region(year = 2020))
		aux <- aux %>% dplyr::select(code_region, geom)
		
		if (is.null(date)) {
			regions <- regions %>% dplyr::filter(date == max(regions$date))
		} else {
			regions <- regions %>% dplyr::filter(date == as.date(date))
		}
		
		if (withCorr) {
			auxCities <- readRDS(paste0(dir, "rds/cities.rds"))
			
			if (is.null(date)) {
				auxCities <- auxCities %>% dplyr::filter(date == max(auxCities$date))
			} else {
				auxCities <- auxCities %>% dplyr::filter(date == as.date(date))
			}
			
			regions$corr_accumCases_idhm <- 0
			regions$corr_accumDeaths_idhm <- 0
			regions$corr_newCases_idhm <- 0
			regions$corr_newDeaths_idhm <- 0
			
			regions$corr_accumCases_gini <- 0
			regions$corr_accumDeaths_gini <- 0
			regions$corr_newCases_gini <- 0
			regions$corr_newDeaths_gini <- 0
			
			regions$corr_accumCases_ivs <- 0
			regions$corr_accumDeaths_ivs <- 0
			regions$corr_newCases_ivs <- 0
			regions$corr_newDeaths_ivs <- 0
			
			
			regions$pValue_accumCases_idhm <- 1
			regions$pValue_accumDeaths_idhm <- 1
			regions$pValue_newCases_idhm <- 1
			regions$pValue_newDeaths_idhm <- 1
			
			regions$pValue_accumCases_gini <- 1
			regions$pValue_accumDeaths_gini <- 1
			regions$pValue_newCases_gini <- 1
			regions$pValue_newDeaths_gini <- 1
			
			regions$pValue_accumCases_ivs <- 1
			regions$pValue_accumDeaths_ivs <- 1
			regions$pValue_newCases_ivs <- 1
			regions$pValue_newDeaths_ivs <- 1
			
			for (i in 1:nrow(regions)) {
				auxCorr <- auxCities %>% dplyr::filter(auxCities$code_region == regions$code[i])
				
				regions$corr_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$estimate
				regions$corr_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$estimate
				regions$corr_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$estimate
				regions$corr_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$estimate
				
				regions$corr_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$estimate
				regions$corr_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$estimate
				regions$corr_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$estimate
				regions$corr_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$estimate
				
				regions$corr_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$estimate
				regions$corr_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$estimate
				regions$corr_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$estimate
				regions$corr_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$estimate
				
				
				
				regions$pValue_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$p.value
				regions$pValue_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$p.value
				regions$pValue_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$p.value
				regions$pValue_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$p.value
				
				regions$pValue_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$p.value
				regions$pValue_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$p.value
				regions$pValue_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$p.value
				regions$pValue_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$p.value
				
				regions$pValue_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$p.value
				regions$pValue_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$p.value
				regions$pValue_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$p.value
				regions$pValue_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$p.value
			}
			
			rm(auxCorr)
		}
		
		aux$code_region <- as.character(aux$code_region)
		regions <- dplyr::left_join(regions, aux, by = c("code" = "code_region")) %>% data.frame()
		
		saveRDS(regions, file = paste0(dir, "maps/regions.rds"))
		rm(list = c("regions", "aux"))
		
		
		
		states <- readRDS(paste0(dir, "rds/states.rds"))
		
		aux <- data.frame(geobr::read_state(code_state = "all", year = 2020))
		aux <- aux %>% dplyr::select(code_state, geom)
		
		if (is.null(date)) {
			states <- states %>% dplyr::filter(date == max(states$date))
		} else {
			states <- states %>% dplyr::filter(date == as.date(date))
		}
		
		
		if (withCorr) {
			states$corr_accumCases_idhm <- 0
			states$corr_accumDeaths_idhm <- 0
			states$corr_newCases_idhm <- 0
			states$corr_newDeaths_idhm <- 0
			
			states$corr_accumCases_gini <- 0
			states$corr_accumDeaths_gini <- 0
			states$corr_newCases_gini <- 0
			states$corr_newDeaths_gini <- 0
			
			states$corr_accumCases_ivs <- 0
			states$corr_accumDeaths_ivs <- 0
			states$corr_newCases_ivs <- 0
			states$corr_newDeaths_ivs <- 0
			
			
			states$pValue_accumCases_idhm <- 1
			states$pValue_accumDeaths_idhm <- 1
			states$pValue_newCases_idhm <- 1
			states$pValue_newDeaths_idhm <- 1
			
			states$pValue_accumCases_gini <- 1
			states$pValue_accumDeaths_gini <- 1
			states$pValue_newCases_gini <- 1
			states$pValue_newDeaths_gini <- 1
			
			states$pValue_accumCases_ivs <- 1
			states$pValue_accumDeaths_ivs <- 1
			states$pValue_newCases_ivs <- 1
			states$pValue_newDeaths_ivs <- 1
			
			for (i in 1:nrow(states)) {
				if (states$code[i] == 53)
					next
				
				auxCorr <- auxCities %>% dplyr::filter(auxCities$code_state == states$code[i])
				
				states$corr_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$estimate
				states$corr_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$estimate
				states$corr_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$estimate
				states$corr_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$estimate
				
				states$corr_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$estimate
				states$corr_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$estimate
				states$corr_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$estimate
				states$corr_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$estimate
				
				states$corr_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$estimate
				states$corr_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$estimate
				states$corr_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$estimate
				states$corr_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$estimate
				
				
				
				states$pValue_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$p.value
				states$pValue_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$p.value
				states$pValue_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$p.value
				states$pValue_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$p.value
				
				states$pValue_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$p.value
				states$pValue_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$p.value
				states$pValue_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$p.value
				states$pValue_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$p.value
				
				states$pValue_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$p.value
				states$pValue_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$p.value
				states$pValue_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$p.value
				states$pValue_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$p.value
			}
			
			rm(auxCorr)
		}
		
		aux$code_state <- as.character(aux$code_state)
		states <- dplyr::left_join(states, aux, by = c("code" = "code_state")) %>% data.frame()
		
		saveRDS(states, file = paste0(dir, "maps/states.rds"))
		rm(list = c("states", "aux"))
		
		
		meso <- readRDS(paste0(dir, "rds/meso.rds"))
		
		aux <- data.frame(geobr::read_meso_region(code_meso = "all", year = 2020))
		aux <- aux %>% dplyr::select(code_meso, geom)
		
		if (is.null(date)) {
			meso <- meso %>% dplyr::filter(date == max(meso$date))
		} else {
			meso <- meso %>% dplyr::filter(date == as.date(date))
		}
		
		if (withCorr) {
			meso$corr_accumCases_idhm <- 0
			meso$corr_accumDeaths_idhm <- 0
			meso$corr_newCases_idhm <- 0
			meso$corr_newDeaths_idhm <- 0
			
			meso$corr_accumCases_gini <- 0
			meso$corr_accumDeaths_gini <- 0
			meso$corr_newCases_gini <- 0
			meso$corr_newDeaths_gini <- 0
			
			meso$corr_accumCases_ivs <- 0
			meso$corr_accumDeaths_ivs <- 0
			meso$corr_newCases_ivs <- 0
			meso$corr_newDeaths_ivs <- 0
			
			
			meso$pValue_accumCases_idhm <- 1
			meso$pValue_accumDeaths_idhm <- 1
			meso$pValue_newCases_idhm <- 1
			meso$pValue_newDeaths_idhm <- 1
			
			meso$pValue_accumCases_gini <- 1
			meso$pValue_accumDeaths_gini <- 1
			meso$pValue_newCases_gini <- 1
			meso$pValue_newDeaths_gini <- 1
			
			meso$pValue_accumCases_ivs <- 1
			meso$pValue_accumDeaths_ivs <- 1
			meso$pValue_newCases_ivs <- 1
			meso$pValue_newDeaths_ivs <- 1
			
			for (i in 1:nrow(meso)) {
				if (meso$code_state[i] == 53)
					next
				
				auxCorr <- auxCities %>% dplyr::filter(auxCities$code_meso == meso$code[i])
				
				meso$corr_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$estimate
				meso$corr_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$estimate
				meso$corr_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$estimate
				meso$corr_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$estimate
				
				meso$corr_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$estimate
				meso$corr_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$estimate
				meso$corr_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$estimate
				meso$corr_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$estimate
				
				meso$corr_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$estimate
				meso$corr_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$estimate
				meso$corr_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$estimate
				meso$corr_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$estimate
				
				
				
				meso$pValue_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$p.value
				meso$pValue_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$p.value
				meso$pValue_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$p.value
				meso$pValue_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$p.value
				
				meso$pValue_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$p.value
				meso$pValue_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$p.value
				meso$pValue_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$p.value
				meso$pValue_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$p.value
				
				meso$pValue_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$p.value
				meso$pValue_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$p.value
				meso$pValue_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$p.value
				meso$pValue_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$p.value
			}
			
			rm(auxCorr)
		}
		
		aux$code_meso <- as.character(aux$code_meso)
		meso <- dplyr::left_join(meso, aux, by = c("code" = "code_meso")) %>% data.frame()
		
		saveRDS(meso, file = paste0(dir, "maps/meso.rds"))
		rm(list = c("meso", "aux"))
		
		
		micro <- readRDS(paste0(dir, "rds/micro.rds"))
		
		aux <- data.frame(geobr::read_micro_region(code_micro = "all", year = 2020))
		aux <- aux %>% dplyr::select(code_micro, geom)
		
		if (is.null(date)) {
			micro <- micro %>% dplyr::filter(date == max(micro$date))
		} else {
			micro <- micro %>% dplyr::filter(date == as.date(date))
		}
		
		if (withCorr) {
			micro$corr_accumCases_idhm <- 0
			micro$corr_accumDeaths_idhm <- 0
			micro$corr_newCases_idhm <- 0
			micro$corr_newDeaths_idhm <- 0
			
			micro$corr_accumCases_gini <- 0
			micro$corr_accumDeaths_gini <- 0
			micro$corr_newCases_gini <- 0
			micro$corr_newDeaths_gini <- 0
			
			micro$corr_accumCases_ivs <- 0
			micro$corr_accumDeaths_ivs <- 0
			micro$corr_newCases_ivs <- 0
			micro$corr_newDeaths_ivs <- 0
			
			
			micro$pValue_accumCases_idhm <- 1
			micro$pValue_accumDeaths_idhm <- 1
			micro$pValue_newCases_idhm <- 1
			micro$pValue_newDeaths_idhm <- 1
			
			micro$pValue_accumCases_gini <- 1
			micro$pValue_accumDeaths_gini <- 1
			micro$pValue_newCases_gini <- 1
			micro$pValue_newDeaths_gini <- 1
			
			micro$pValue_accumCases_ivs <- 1
			micro$pValue_accumDeaths_ivs <- 1
			micro$pValue_newCases_ivs <- 1
			micro$pValue_newDeaths_ivs <- 1
			
			for (i in 1:nrow(micro)) {
				if (micro$code_state[i] == 53)
					next
				
				auxCorr <- auxCities %>% dplyr::filter(auxCities$code_micro == micro$code[i])
				
				if (nrow(auxCorr) < 3)
					next
				
				micro$corr_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$estimate
				micro$corr_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$estimate
				micro$corr_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$estimate
				micro$corr_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$estimate
				
				micro$corr_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$estimate
				micro$corr_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$estimate
				micro$corr_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$estimate
				micro$corr_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$estimate
				
				micro$corr_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$estimate
				micro$corr_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$estimate
				micro$corr_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$estimate
				micro$corr_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$estimate
				
				
				
				micro$pValue_accumCases_idhm[i] <- cor.test(auxCorr$accumCases, auxCorr$idhm)$p.value
				micro$pValue_accumDeaths_idhm[i] <- cor.test(auxCorr$accumDeaths, auxCorr$idhm)$p.value
				micro$pValue_newCases_idhm[i] <- cor.test(auxCorr$newCases, auxCorr$idhm)$p.value
				micro$pValue_newDeaths_idhm[i] <- cor.test(auxCorr$newDeaths, auxCorr$idhm)$p.value
				
				micro$pValue_accumCases_gini[i] <- cor.test(auxCorr$accumCases, auxCorr$gini)$p.value
				micro$pValue_accumDeaths_gini[i] <- cor.test(auxCorr$accumDeaths, auxCorr$gini)$p.value
				micro$pValue_newCases_gini[i] <- cor.test(auxCorr$newCases, auxCorr$gini)$p.value
				micro$pValue_newDeaths_gini[i] <- cor.test(auxCorr$newDeaths, auxCorr$gini)$p.value
				
				micro$pValue_accumCases_ivs[i] <- cor.test(auxCorr$accumCases, auxCorr$ivs)$p.value
				micro$pValue_accumDeaths_ivs[i] <- cor.test(auxCorr$accumDeaths, auxCorr$ivs)$p.value
				micro$pValue_newCases_ivs[i] <- cor.test(auxCorr$newCases, auxCorr$ivs)$p.value
				micro$pValue_newDeaths_ivs[i] <- cor.test(auxCorr$newDeaths, auxCorr$ivs)$p.value
			}
			
			rm(auxCorr)
		}
		
		aux$code_micro <- as.character(aux$code_micro)
		micro <- dplyr::left_join(micro, aux, by = c("code" = "code_micro")) %>% data.frame()
		
		saveRDS(micro, file = paste0(dir, "maps/micro.rds"))
		rm(list = c("micro", "aux"))
		
		
		cities <- readRDS(paste0(dir, "rds/cities.rds"))
		
		aux <- data.frame(geobr::read_municipality(code_muni = "all", year = 2020))
		aux <- aux %>% dplyr::select(code_muni, geom)
		
		if (is.null(date)) {
			cities <- cities %>% dplyr::filter(date == max(cities$date))
		} else {
			cities <- cities %>% dplyr::filter(date == as.date(date))
		}
		
		aux$code_muni <- as.character(aux$code_muni)
		aux$code_muni <- stringr::str_sub(aux$code_muni, end = 6)
		cities <- dplyr::left_join(cities, aux, by = c("code" = "code_muni")) %>% data.frame()
		
		saveRDS(cities, file = paste0(dir, "maps/cities.rds"))
		rm(list = c("cities", "aux"))
	)
		
	return(message("\n\nYour maps is been created with success!\n\n"))
}