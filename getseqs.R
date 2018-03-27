#Converts a nonnegative decimal integer to base b
#
#Args
#num      A nonnegative decimal integer
#b        THe desired base
#
#Output
#A vector of the integers 0, 1, ..., b-1 representing the digits of the base b number
#
#Note; no error checking of inputs
#
dec_to_baseb<-function(num,b)
{
  res<-(num %% b)
  num<-(num-res)/b
  while (num!=0)
  {
    h<-(num %% b)
    res<-c(h,res)
    num<-(num-h)/b
  }
  return(res)
}

#Given a length, finds all sequences of A, T, G, C of that length
#
#Args
#len      A single number
#
#Output
#A vector of all of 'em
#
#Note: no error checking
#
getseqs<-function(len)
{
  bases<-c("A","C","G","T")
  res<-c()
  for (counter in 0:(4^len-1))
  {
    h<-dec_to_baseb(counter,4)
    if (length(h)<len)
    {
      h<-c(rep(0,len-length(h)),h)
    }
    res<-c(res,paste0(bases[h+1],collapse=""))
  }
  return(res)
}
