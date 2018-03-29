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
  locs<-find_fixed(ms,seq)
  
  #use the output of find_fixed to find the repeats
  ncms<-nchar(ms)
  res<-data.frame(microsat=character(),loc=integer(),numrep=integer(),stringsAsFactors = F)
  while (length(locs>0))
  {
    #gets the number of repeats
    numrep<-1
    while ((locs[1]+numrep*ncms) %in% locs)
    {
      numrep<-numrep+1
    }
    
    #store the info
    dr1<-dim(res)[1]
    res[dr1+1,1]<-ms
    res[dr1+1,2:3]<-c(locs[1],numrep)
    
    #throw away the part of locs you have been over
    locs<-locs[locs>=locs[1]+numrep*ncms]
  }
  
  res<-res[res$numrep>=repmin,]
  
  return(res)
}

find_repeat("AG","TAGAGAGTAGCAGAGCTTTTACAGAT",2)
find_fixed("AG","AGTAGCAGAGCTTTTACAGAT")


#alternatively,

my_find_repeat<-function(ms,seq,repmin){
  
  temp<-my_find_fixed(ms,seq)
  ncms<-nchar(ms)
  
  if(temp[1]!=-1){
    
   ms_rep<-rep(ms,repmin)
   ms_repmin<-paste(ms_rep, collapse = "")
   startpos<-my_find_fixed(ms_repmin,seq)
   
   numrep<-c()
   for(i in 1:length(startpos)){
     
     #gets the number of repeats
     num_rep<-1
     while ((startpos[i]+num_rep*ncms) %in% temp){
       num_rep<-num_rep+1
     }
     
     numrep[i]<-num_rep
     
   }
   
  }else{
    startpos<-NA   # no matching pattern found
    numrep<-NA
  }
  
  return(data.frame(ms=ms,
                    loc=startpos,
                    numrep=numrep))
}

my_find_repeat(ms="AG",seq="TAGAGAGTAGCAGAGCTTTTACAGAT",repmin=2)














