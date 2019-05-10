#function for fixing
#fix any errors in sample names, and/or add comments about analysis to comment column here; added to combined_data_group because this is the dataset that gets written to the flatlist.csv file

getmedadatafromAnalysis <- function(dataframe, lowerboundAnalysis, uperboundAnalysis, getmedadatafromAnalysis){
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= uperboundAnalysis, "Identifier 1"] <- first(dataframe[ floor(dataframe$Analysis) == getmedadatafromAnalysis, "Identifier 1"])[1]
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= uperboundAnalysis, "Identifier 2"] <- first(dataframe[ floor(dataframe$Analysis) == getmedadatafromAnalysis, "Identifier 2"])[1]
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= uperboundAnalysis, "Preparation"] <- first(dataframe[ floor(dataframe$Analysis) == getmedadatafromAnalysis, "Preparation"])[1]
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= uperboundAnalysis, "type"] <- first(dataframe[ floor(dataframe$Analysis) == getmedadatafromAnalysis, "type"])[1]
  return(dataframe)
}