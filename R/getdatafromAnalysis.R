#function for fixing
#fix any errors in sample names, and/or add comments about analysis to comment column here; added to combined_data_group because this is the dataset that gets written to the flatlist.csv file

getmetadatafromAnalysis <- function(dataframe, lowerboundAnalysis, upperboundAnalysis, getmetadatafromAnalysis){
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= upperboundAnalysis, "Identifier 1"] <- first(dataframe[ floor(dataframe$Analysis) == getmetadatafromAnalysis, "Identifier 1"])[1]
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= upperboundAnalysis, "Identifier 2"] <- first(dataframe[ floor(dataframe$Analysis) == getmetadatafromAnalysis, "Identifier 2"])[1]
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= upperboundAnalysis, "Preparation"] <- first(dataframe[ floor(dataframe$Analysis) == getmetadatafromAnalysis, "Preparation"])[1]
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= upperboundAnalysis, "type"] <- first(dataframe[ floor(dataframe$Analysis) == getmetadatafromAnalysis, "type"])[1]
  return(dataframe)
}
