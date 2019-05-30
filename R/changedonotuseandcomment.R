change.donotuse.and.runinfo <- function(dataframe, lowerboundAnalysis, upperboundAnalysis, bool, datacomment){
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= upperboundAnalysis, "Donotuse"] <- bool
  dataframe[dataframe$Analysis >= lowerboundAnalysis & dataframe$Analysis <= upperboundAnalysis, "runinfo"] <- datacomment
  return(dataframe)
}
