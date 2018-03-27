#Takes a sequence, seq, and searches for microsatellites. Returns information on the start 
#position and number of repeats for all microsatellites of length >=lenmin and <=lenmax 
#and with at least repmin repeats.
#
#Args
#seq          A character string (possibly quite long) with the sequence (A, T, G, Cs)
#lenmin       Searches for microsats with at least this many characters in them. Must be at
#               least 2.
#lenmax       Searches for microsats with not more than this many characters in them. Cannot
#               be more than 6.
#repmin       A repeat is only considered a microsat if it has at least this many repeats.
#
#Output - a data frame with these columns:
#microsat     The microsat
#loc          The start position
#numrep       The number of repeats
#
find_microsats<-function(seq,lenmin,lenmax,repmin)
{
  #for each length from lenmin to lenmax, get a vector of all sequences of that length and
  #then combine them into one big vector of sequences, call it potms
  potms<-c()
  for (len in lenmin:lenmax)
  {
    potms<-c(potms,getseqs(len))  
  }
    
  #call find_repeat above with each element of potms, combining results
  res<-data.frame(microsat=character(),loc=integer(),numrep=integer(),stringsAsFactors = F)
  for (counter in 1:length(potms))
  {
    res<-rbind(res,find_repeat(potms[counter],seq,repmin))
  }
  rownames(res)<-NULL
  
  return(res)
}
