require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
require("CoreNLP")
require("stringr")
require("openNLP")
require("arules")
require("arulesViz")
require("NLP")
require("textstem")
require("qdap")#for polarity()

docxml=ldply(xmlToList("Restaurants_Train.xml"), data.frame)
sentence <- as.data.frame(unique(docxml$text))


#concatenate all strings to form 1 sentence
for(i in 1:3038)
  txt <- paste (txt, sentence$Text[i])

#POS tagging
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)}
tagged_str <- tagPOS(txt)

#filter nouns 
grep('NN', tagged_str$POStags)## used this to make vector v1

for(i in 1:9820)##used this to put those words in noun df
  noun <-paste(noun, pos_words[v1[i],])
noun <- as.data.frame(strsplit(noun," "))

#to remove /NN... from our df
noun$word<- as.character(noun$word)
for(i in 1:nrow(noun))
  noun$word[i]<- gsub("/+[[:alpha:]]*","",noun$word[i])

#to remove punct
for (i in 1:nrow(noun))
  noun$word[i]<- gsub("[[:punct:]]", "", noun$word[i])


j<- 3
#Assomatrix contruction      #use try and catch while contructing loops 

for( i in 1: nrow(Assomatrix))
{ k=2
while(isTRUE(grep(noun$word[j],Assomatrix$Text[i])==1))
{ 
  Assomatrix[i,k]<- noun$word[j]
  j=j+1
  k=k+1
}
}

# Remove NA from Assomatrix so no error later
Assomatrix[is.na(Assomatrix)] <- ""

####################################APPROACH # 2#############################################################################################################################
# trying again from assomatrix with less rows

matrix<- as.data.frame(Assomatrix[-c(1000:nrow(Assomatrix)),-c(18:ncol(Assomatrix))])

#has rows = unique aspects , col = potential no. of matches
newmattrix<- as.data.frame(matrix(0,nrow=2794,ncol=142))

#changing row and coloumn names of new matrix

for(i in 1:2794)
  + row.names(newmattrix)[i]<-noun$word[i]

for(i in 1:ncol(newmattrix))
  names(newmattrix)[i]<- i


#need to init this for pasting the sentence number at the correct position
k[1:nrow(newmattrix)]<-1
val<- 0
#to make matrix#2

for(i in 1:nrow(matrix))
  for(j in 2: ncol(matrix))
  {
    if (isTRUE(matrix[i,j]!= ""))
      
    { val <- which(rownames(newmattrix)==matrix[i,j])
    newmattrix[val,k[val]]<- i
    k[val]= k[val]+1
    }
  }



#########################################################APPROACH #1 #################################################################
######to make transaction - aspect dataframe #######

aspects<- as.data.frame(unique(noun))

aspects<- as.data.frame(lemmatize_words(stem_words(aspects$word))) 

transaction<- as.data.frame(t(aspects))

for(i in 1:nrow(Assomatrix))
Assomatrix[i,]<- stem_strings(Assomatrix$Text[i])


#to change col names of transaction
for(i in 1:nrow(aspects))
 names(transaction)[i]<-paste(aspects$`lemmatize_words(stem_words(copy))`[i])



# to put 1,0 in sentence-aspect matrix
for(i in 1:nrow(tran))
  for(j in 1:ncol(tran))
  {
    if(isTRUE(grep(aspects$`lemmatize_words(stem_words(copy))`[j],lemmatize_strings(stem_strings(Assomatrix$Text)[i]))==1))
    { 
      tran[i,j]<- 1
    }
    else
      tran[i,j]<-0
    
  }
#reducin the size of transction data frame to process faster
tran_trim<- tran[-c(1001:3038),]

# create transact 0 filled data frame to make a new matrix

for(i in 1:ncol(tran))
{ k=2
for(j in 1:nrow(tran))
{ 
  if(tran[j,i]==1)
  {
    transact[j,k]<- j
    k=k+1
  }
}
}

### nake transpose of transact

tran_trim<- transact
transact<- t(transact)
transact<-data.frame(transact)

#######################################################################################
#difference b/w inbuilt coreNLP / qdap library and our custom built function is that when we make governor-dependent pairs , inbuilt is not accurate since it looks at the sentence as a whole and does not 
#check it aspect wise . eg polarity("food was good , but the ambience was poor","food"). 

#store the most frequent itemset which we founf out using apriori  http://michael.hahsler.net/research/arules_RUG_2015/demo/
#store all these pairs in df 
#seperate governor dependant pairs from getdependency() which contain aspects
#


#MATRIX #1
#  bread  butter milk coconut
#1   1     0      0     1
#2   1     1      1     0
#3   0     0      1     1

#MATRIX #2
#### bread 1,2
#### butter 4,7
### milk 3,5,6
####coconut 8,9

trans<- as(newmatrix,"transactions")

itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.03, minlen = 2, maxlen=4))

frequent<- as.data.frame(inspect(head(sort(itemsets), n=1000)))

frequent$items<-as.character(frequent$items)

# store result of GetDependency() for each annotatedstring(Assomatri$Text[i])
# only if either governor or dependent is a noun ,
#other should be in polarity
#save Governor, Dependent , Polarity in a data frame
#                                                  ^
#                                                  |
#                                                  |
#                                                  |
# To Do                                            |
#load polarity in data frame                       |
#make a new empty data frame for  ------------------


initCoreNLP() # need to initialize coreNLp for getDependency()

#rownames(lexicon) <- seq(length=nrow(lexicon))



###########################################################NOT WORKING############################################################# 

for(k in 1:nrow(Assomatrix))
{
  tempdf<- as.data.frame(getDependency(annotateString(Assomatrix$Text[k])))#find GD Pairs of sentence k
    
for(j in 1:nrow(tempdf))#############################<---Discard useless GOV-DEP Pairs--->#################
for(i in 1:nrow(noun))
{
     if(isTRUE(grep(tempdf$governor[j],noun$word[i])==1))
          { 
             # for(l in 1:nrow(lexicon))
               # {
                #    if(!isTRUE(grep(lexicon$word[l],tempdf$dependent[j])==1))
                #        {
                 #         tempdf<-tempdf[-c(j),]
                  #      }   
               # }
              #  if(which(tempdf$dependent[j]==lexicon$word) > 0)
                  if(!isTRUE(which(tempdf$dependent[j]==lexicon$word)>0))
                  tempdf<-tempdf[-c(j),]
                    
                #  if(isTRUE(which(tempdf$dependent[j]==lexicon$word)>0))
               #   {Dependency[p,]<-tempdf[c(j),]
                #   p=p+1
                #  }         
                 
          }
      else if(isTRUE(grep(tempdf$dependent[j],noun$word[i])==1))
          { 
                #if(which(tempdf$governor[j]==lexicon$word) > 0)
                  if(!isTRUE(which(tempdf$governor[j]==lexicon$word)>0))
                  tempdf<-tempdf[-c(j),]
                  
                #  if(isTRUE(which(tempdf$governor[j]==lexicon$word)>0))
                     #  {Dependency[p,]<-tempdf[c(j),]
                      # p=p+1
      }
}
  #####################################<---End of Discard--->##################################################
  if(k==1)
   Dependency_final <- tempdf
  else
    Dependency_final<- merge(Dependency_final,tempdf)
}







#
#
#have created tempdf for storing getdependency()
# & temp to compare the results of oother copies with this(removinf gov-dep pairs and will later copy to Dependency)
#& Dependency (adding selected rows directly to dependency)
#
#


#****************************************Trying to append directly to Dependency*******************************************************
  
  for(j in 1:nrow(tempdf))
for(i in 1:nrow(noun))
{
  if(isTRUE(grep(tempdf$governor[j],noun$word[i])==1))
  { 
  
      if(isTRUE(which(tempdf$dependent[j]==lexicon$word)>0))
    Dependency<-merge(Dependency,tempdf[j,])        
    
  }
  else if(isTRUE(grep(tempdf$dependent[j],noun$word[i])==1))
  { 
 
      if(isTRUE(which(tempdf$governor[j]==lexicon$word)>0))
      Dependency<-merge(Dependency,tempdf[j,])
  }
}
#**************************************************************************************************************************************
