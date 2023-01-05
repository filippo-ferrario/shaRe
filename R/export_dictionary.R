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
#' @param formatTable character. Only considered for .txt output. One of the following two options: 'long', 'table'(DEFAULT). 'long' format will report one variable and its metadata at the time, one after the other. 'table' format will paste metadata as a formatted table.
#' @param overwrite logical. should overwrite an existing file with the same name? DEFAULT to TRUE. If FALSE than the file is appended.
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
#' file_name<-paste0(tempfile(),'.txt')
#' export_dictionary(description=txt,varTable=dict, path=file_name)
#' # open exported distionary in the text editor
#' edit(file=file_name)
#' 
#' file_name<-paste0(tempfile(),'.csv')
#' export_dictionary(description=txt,varTable=dict, path=file_name)
#' # open exported distionary in the text editor
#' edit(file=file_name)
#' 
#' 
#' @export




export_dictionary<-function(description=NULL, varTable=NULL, path=NULL, formatTable='table', overwrite=TRUE)
{
	# check parameters
	if(!is.null(description) & !is.character(description)) stop('description must be a string')
	if(!is.null(varTable) & !is.data.frame(varTable)) stop('varTable must be a data.frame')
	if(!is.null(path) & !is.character(path)) stop('path must be a string')
	if(!is.null(path) & !formatTable %in% c('long','table')) stop('formatTable must be one between \'long\' or \'table\'')	

	# save additional info
	if (!is.null(description)) {
		write(description, file=path, append=!overwrite)
		append_tab<-TRUE
	} else {append_tab<-!overwrite}

	if (!is.null(varTable))
	{# save data dictionary table conditional to file format
		if (grepl(path, pattern='\\.csv'))
		write.table(varTable, file=path,append=append_tab, row.names=F, col.names=TRUE, sep=',', dec='.', quote=T)
		if (grepl(path, pattern='\\.txt')){
			if ( formatTable=='long'){
						for (i in 1: nrow(varTable)){
									append_tab<-ifelse (i==1, append_tab, TRUE)
									for (j in 1:ncol(varTable) ) {
										write(paste0(names(varTable)[j],': ',varTable[i,j]), file=path,append=append_tab)
									    append_tab<-TRUE
									    }
									
									write('\n', file=path,append=T)
								} } 
			if ( formatTable=='table'){
				fmtout<-capture.output(as.data.frame(varTable), type='output')
				fmtout2<-sub(fmtout, pattern='[:space:]+|[[:digit:]+[:space:]]', replacement='')
				write(c('\n',fmtout2), file=path, append=append_tab)		
			}
	
		}
	}
}







# # initiate a data dictionary
# dict_names<-c('field_name','description', 'data_type', 'unit','missing_data_code')
# dict<- matrix(nrow=5, ncol=length(dict_names), NA)
# dict<-as.data.frame(dict)
# names(dict)<-dict_names
# # fill in a description of the dataset 
# dict[,'field_name']<-letters[1:5]
# dict[,'description']<- paste0('this is variable ',LETTERS[1:5])


# # example of additional info
# v1<-12345
# daytag<-as.Date(Sys.time())
# txt<-paste0('R SCRIPT 01-urchin_rocks-data_extraction on ',daytag,'
# DATASET DESCRIPTION
# Dataset describing the substrate on which Green Sea Urchin (gsu) where found in Bay Sept-Iles in July 2018
# Coordinate Reference System
# EPSG: ',v1,'
# proj4string: ',v1,'
# '
# )

# file_name<-paste0(tempfile(),'.txt')
# export_dictionary(description=txt,varTable=NULL, path=file_name, overwrite=T, formatTable='table')
# # open exported distionary in the text editor
# edit(file=file_name)
