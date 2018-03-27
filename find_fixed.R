#Takes a prospective microsatellite, ms, and a sequence, seq, and returns a vector with 
#the start position of all exact matches
#
#Args
#ms       A character string with the prospective microsatellite (A, T, G, Cs)
#seq      A character string (possibly quite long) with the sequence (A, T, G, Cs)
#
#Output
#A vector with the start location of all exact matches
#
#Note: no error checking
#
find_fixed<-function(ms,seq)
{
  ncms<-nchar(ms)
  
  res<-c()
  for (counter in 1:(nchar(seq)-ncms+1))
  {
    if (ms==substr(seq,counter,counter+ncms-1))
    {
      res<-c(res,counter)
    }
  }
  
  return(res)
}
