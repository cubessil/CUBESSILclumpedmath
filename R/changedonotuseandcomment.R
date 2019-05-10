change.donotuse.and.comment <- function(dataframe, lowerboundAnalysis, uperboundAnalysis, bool, datacomment){
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= uperboundAnalysis, "Donotuse"] <- bool
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= uperboundAnalysis, "runinfo"] <- datacomment
  return(dataframe)
}