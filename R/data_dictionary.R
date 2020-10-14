# ===========================================================
# Title      : dataset dictionary
# Author     : Filippo Ferrario
# Date       : 2020-10-14
# Version    : 0.2
# Aim        : function to create a data dictionary following the specifices fo the CHONe dataverse 
# link	     : https://chone2.ca/app/uploads/2020/05/CHONe-Dataverse-User-Guideline_v3.pdf
# ===========================================================


#' Initiate data dictionary
#' 
#' Initiate the data dictionary to document a dataset.
#' The data dictionary is formatted based on the CHONe dataverse guideline at <https://chone2.ca/app/uploads/2020/05/CHONe-Dataverse-User-Guideline_v3.pdf>. 
#' 
#' @param dataset  a data.frame object to be documented.
#' 
#' @return 
#' The data.frame of the dictionary to be completed and saved along with the dataset.

#' @author Filippo Ferrario, \email{filippo.f3rrario@@gmail.com}
#' 
#' @seealso 
#' [export_dictionary]
#' 
#' @examples
#' 
#' # create a data.frame
#' mydata<-data.frame(ind=LETTERS[1:5],score=1:5)
#' # initiate the dictionary 
#' dict_mydata<-initiate_dictionary(mydata)
#' 
#' dict_mydata
#' 
#' # fill in the dictionary
#' dict_mydata[dict_mydata$field_name=='ind',2:4]<-c('Individual ID','string','N/A')
#' dict_mydata[dict_mydata$field_name=='score',2:4]<-c('score of Ind','numeric','apples')
#' 
#' dict_mydata
#' 
#' @export

initiate_dictionary<-function(dataset)
{
		voc_names<-c('field_name','description', 'data_type', 'unit','missing_data_code')
		res<- matrix(nrow=length(names(dataset)), ncol=length(voc_names), NA)
		res<-as.data.frame(res)
		names(res)<-voc_names
		res$field_name<- names(dataset)
		res
}



