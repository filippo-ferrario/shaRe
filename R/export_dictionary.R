# ===========================================================
# Title      : Export dataset dictionary
# Author     : Filippo Ferrario
# Date       : 2020-10-14
# Version    : 0.1
# Aim        : function to save a data dictionary following the specifices fo the CHONe dataverse 
# 				allowing for additional information before the variable table.
# link	     : https://chone2.ca/app/uploads/2020/05/CHONe-Dataverse-User-Guideline_v3.pdf
# ===========================================================


#' Export data dictionary
#' 
#' Export the data dictionary for a dataset with possibility to add additional information (e.g. dataset description, reference system for geographical coordinates, fiel creation date,... ).
#' The data dictionary is formatted based on the CHONe dataverse guideline at <https://chone2.ca/app/uploads/2020/05/CHONe-Dataverse-User-Guideline_v3.pdf>. 
#' Additional information can be added before the data dictionary.
#' 
#' @param description character containing the info additional to the data dictionary
#' @param varTable a data.frame object containing description and metadata of the variables in the related dataset. Typically the dataframe is initated using [initiate_dictionary].
#' @param path a character specifying the path and the name of the file to be saved.
#' 
#' @return 
#' Export the data dictionary file as a .txt or .csv (see Details), or formats that can be saved by [utils::write.table]. 
#' 
#' @details
#' Consider the following format depending on the situation:
#' - ".txt" If exporting both dictionary (i.e. varTable) and additional information (i.e. description). This format is more easily human readable.
#' - ".csv" If exporting only the dictionary (i.e. varTable). If description is present, varTable is appended after
#' 
#' When format is ".csv" dictionary is saved by write.table with row.names=F, col.names=TRUE, sep=',', dec='.', quote=T
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@@gmail.com} 
#' 
#' @seealso 
#' [initiate_dictionary]
#' 
#' @examples
#' # initiate a data dictionary
#' dict_names<-c('field_name','description', 'data_type', 'unit','missing_data_code')
#' dict<- matrix(nrow=5, ncol=length(dict_names), NA)
#' dict<-as.data.frame(dict)
#' names(dict)<-dict_names
#' # fill in a description of the dataset 
#' dict[,'field_name']<-letters[1:5]
#' dict[,'description']<- paste0('this is variable ',LETTERS[1:5])
#' 
#' 
#' # example of additional info
#' v1<-12345
#' daytag<-as.Date(Sys.time())
#' txt<-paste0('R SCRIPT 01-urchin_rocks-data_extraction on ',daytag,'
#' DATASET DESCRIPTION
#' Dataset describing the substrate on which Green Sea Urchin (gsu) where found in Bay Sept-Iles in July 2018
#' Coordinate Reference System
#' EPSG: ',v1,'
#' proj4string: ',v1,'
#' '
#' )
#' 
#' file_name<-tempfile()
#' export_dictionary(description=txt,varTable=dict, path=file_name)
#' # open exported distionary in the text editor
#' edit(file=file_name)
#' 
#' 
#' @export



export_dictionary<-function(description=NULL, varTable=NULL, path=NULL)
{
	# check parameters
	if(!is.null(description) & !is.character(description)) stop('description must be a string')
	if(!is.null(varTable) & !is.data.frame(varTable)) stop('varTable must be a data.frame')
	if(!is.null(path) & !is.character(path)) stop('path must be a string')	
	# save additional info
	write(description, file=path)
	# save data dictionary table conditional to file format
	if (grepl(path, pattern='\\.csv'))
	write.table(varTable, file=path,append=T, row.names=F, col.names=TRUE, sep=',', dec='.', quote=T)
	if (grepl(path, pattern='\\.txt')){
		for (i in 1: nrow(varTable)){
					for (j in 1:ncol(varTable) ) {
						write(paste0(names(varTable)[j],': ',varTable[i,j]), file=path,append=T)
					    }
					
					write('\n', file=path,append=T)
				}

	}
}
