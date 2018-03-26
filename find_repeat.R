#Gives the start locations and numbers of consecutive repeats in seq of any instances of at least
#repmin consecutive repeats of ms.
#
#Args
#ms       A character string with a prospective microsatellite (A, T, G, Cs)
#seq      A character string (possibly quite long) with the sequence (A, T, G, Cs)
#repmin   Number of repeats, at least this many required for ms to register
#
#Output - a data frame (possibly empty) with these columns:
#microsat     The microsat, ms
#loc          The start position
#numrep       The number of repeats
#
find_repeat<-function(ms,seq,repmin)
{
  #call find_fixed
  
  #use the output of find_fixed to find the repeats
}