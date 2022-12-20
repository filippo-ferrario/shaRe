# ===============================================================================
# Name   	: Package check and install
# Author 	: Filippo Ferrario (filippo.f3rrario@gmail.com ; filippo.ferrario@dfo-mpo.gc.ca)
# Date   	: 24-05-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: Verify if required packages for the course are installed and, if not, install them.
# ===============================================================================


#' Check the presence of and install packages
#' 
#' The function verify if required packages for the course are installed and, if not, install them.
#' Used at the beginning of a project could help retriving the missing packages.
#' 
#' @param pk a character vector with the names of the packages to check
#' @param repos (optional) the URL to a CRAN repository. Default to https://cloud.r-project.org. Get URLs here: https://cran.r-project.org/mirrors.html
#' @param ... extra arguments to [install.packages]
#' 
#' @details
#' 
#' The user is asked if the missing packages should be installed.
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @seealso
#' [install.packages]
#' 
#' @export


#  function to use
checkNinst<-function(pk=NULL, repos='https://cloud.r-project.org/',...){
	if (!is.character(pk)) stop('pk must be a character vector with the names of the packages to check!')
	I<-! pk %in% rownames(installed.packages()) 
	if (length(pk[I])>0){
		print(paste0('package(s) to install: ', paste0(pk[I], collapse=', ')) )
		repl<- readline(prompt="install this package(s) from CRAN ? y/n: ")
		if (! repl %in%c('y','n')) repl<- readline(prompt="install this package(s) from CRAN ? y/ne: ")
		if (repl=='y'){
				if (!is.character(repos)) stop('specify a CRAN repository. get URLs here: https://cran.r-project.org/mirrors.html')
				# install needed packages
				install.packages(pk[I],repos=repos)
				} else {print(paste0('package(s) to install: ', paste0(pk[I], collapse=', ')) )}
		} else { print('you\'re good to go...')}
}


# Bench
# =================

checkNinst('devtools')