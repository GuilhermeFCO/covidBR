get_query <- function(query, param){
	p <- query[[param]]

	if(is.null(p))
		p <- ""

	if(p == "true" || p == "false"){
		p <- toupper(p)
		p <- as.logical(p)
	}

	return(p)
}
