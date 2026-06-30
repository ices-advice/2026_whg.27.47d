# data_utilities


######################################## FLCORE function writeIndicesVPA##################################


writeIndicesVPA <- function(FLIndices., file.) {
  
  # opens connection to the output file
  temp <- file(file., "w")
  on.exit(close(temp))
  
  # Enters files title	
  cat(paste(FLIndices.@desc, sep=" "), "\n", file=temp)
  
  # Enters the code specifying the number of fleets
  cat(length(FLIndices.)+100, "\n", file=temp)
  
  for(i in 1:length(FLIndices.)) {
    
    # Retrieves individual fleet from the FLIndices list
    FLIndex. <- FLIndices.[[i]]
    nages <- FLIndex.@range[2] - FLIndex.@range[1] + 1
    nyrs  <- FLIndex.@range[5] - FLIndex.@range[4] + 1
    
    # creates empty matrix for the catch and effort data
    catch  <- matrix(rep(0, (nyrs*nages)+nyrs), nrow=nyrs, ncol=nages+1)
    
    # Retrieves effort data from each FLIndex object
    effort <- matrix(FLIndex.@effort)
    
    # Retrieves index data from each FLIndex object
    index  <- t(matrix(FLIndex.@index, nrow=nages, ncol=nyrs))
    
    # converts index data into catch data and adds to the empty matrix
    for (j in 1:nages) {
      catch[,j+1]  <- index[,j]*effort[j,]
    }
    
    # appends the eff data onto the front of the matrix which now contains catch data
    catch[,1] <- effort
    
    # Writes relevent info for each individual fleet to the file 
    cat(FLIndex.@name, "\n", file=temp)
    cat(FLIndex.@range[4], FLIndex.@range[5], "\n", file=temp, sep="\t")
    cat(1, 1, FLIndex.@range[6], FLIndex.@range[7], "\n", file=temp, sep="\t")
    cat(FLIndex.@range[1], FLIndex.@range[2], "\n", file=temp, sep="\t")
    
    # Appends the data for each individual fleet to the file
    write(t(catch), file=temp, ncolumns=nages+1, sep="\t")
  }
}	# }}}


###################function to compute 95% CIs########################################

getCI <- function(x,w) {
  b1 <- boot.ci(x,index=w, conf=0.95, type="basic")
  ## extract info for all CI types
  tab <- t(sapply(b1[-(1:3)],function(x) tail(c(x),2)))
  ## combine with metadata: CI method, index
  tab <- cbind(w,rownames(tab),as.data.frame(tab))
  colnames(tab) <- c("index","method","lwr","upr")
  tab
}



