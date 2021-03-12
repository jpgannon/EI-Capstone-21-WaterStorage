#created by Mary Martin and Nina Lany

#function to load Campbell datalogger file
readCDL=function(file){
  
  # read data file starting on 5th line
  dat <- read.csv(file, sep=",",header=FALSE,skip=4,stringsAsFactors=F)
  
  # Read in just the header line (l2)
  # unlist the line, and remove quotes 
  h <- readLines(file, n=2)[2]
  n <- as.factor(unlist(strsplit(h, ",")) )
  n2 <- gsub('"', "", n)
  
  # assign column names to dataframe
  colnames(dat) = n2
  
  return(dat)
}