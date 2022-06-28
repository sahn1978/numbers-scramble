# R fuction called "meta_w_qs" is designed to: 1) run psychometrics analysis for quality scores; 2) save latent quality scores for each study; 
# Once quality scores are saved; you can use quality scores to run meta-analysis using quality score as a continuous moderator.

# dataset must be in csv format (column 1: id, column 2: rater, column 3 - : item responses)
# Set the directory so that quality scores can be saved in the folder that you specified.

# install needed packages
install.packages(c("TAM", "WriteMap", "plyr"), dependencies = TRUE)
require(TAM)
require(WrightMap)
require(plyr)


# 1. Import dataset and save it as your preferred name. 
# 2. use function called meta_w_qs to run psychometrics analysis for quality measures and save latent quality scores in directory.
meta_w_qs=function(data, start, end)
{
  model=TAM::tam.mml.mfr(resp=data[,start:end],
                         facets=data[, 2, drop=FALSE],
                         pid=data$id, formulaA=~item*step+rater)
  
  quality=as.matrix(tam.wle(model)$theta)
  names(quality) = c("quality_score")
  write.table(quality, file="quality.csv", sep=",", row.names=FALSE, col.names=FALSE)
  
  my_list=list("summary of model"= summary(model), 
               "Infit and outift for study" = tam.personfit(model), 
               "Reliability" = tam.wle(model), 
               "Item fit by rater" = msq.itemfit(model)$itemfit, 
               "Write map" = IRT.WrightMap(model), 
               plot(model, type="items"), 
               plot(model, type="expected"))

    return(my_list)
 
}

 meta_w_qs(rti, 3, 16)
# refer to mft_rasch.Rmd for more details; and example pdf. 
