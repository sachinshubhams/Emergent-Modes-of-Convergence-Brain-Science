(filter(brain_pub_csv, (scopus_id  == as.numeric(scop_col[i,1]) & co_author_scopus_id == as.numeric(scop_col[i,1])) )[2])

# Filters the brain_pub_csv file and finds where the scopus_id and co_author_scopus_id are the same. Then it saves all eids paper numbers of each individual scopus_id 

#Find the main author and aggregates all paper IDs published by that author


library(dplyr)
library(ggplot2)

setwd('D:\COSC_6323_Statistics_for_Researchers\Project')

data_csv <- read.csv("ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv",stringsAsFactors = FALSE)
#NRegp is numbered 1 thru 4, represents region
#Yp represents year
# NCIPp (1-4 is Neuro/Bio), (5-7 is Health), (8-9 is Sci/Eng)
# scopus_id can be matched back to the author



authdat_txt <- read.delim('AuthorArticleLevel-RegData-ALLSA_Xc_1_NData_864590_LONGXCIP2.txt', header= TRUE,  sep=",", dec=",", stringsAsFactors=FALSE)
?read.delim

setwd('D:\COSC_6323_Statistics_for_Researchers\Project\Network_Intermediate_Files')


brain_aut_csv<-read.csv("brain_author.csv",stringsAsFactors = FALSE)
# filtering brain_aut_csv by scopus_id gives me indiviudal author scopus_id
filter(brain_aut_csv, scopus_id==7102554450)

brain_pub_csv <- read.csv("brain_publication_authors.csv",stringsAsFactors = FALSE)
# filtering brain_pub_csv by scopus_id gives every co-author that co-wrote paper including main author
# if scopus_id matches co_author_scopus_id, then that is the main author for the paper.
# each paper also has an eids identifier and can be used in data_csv to get the DOIp of the paper with the year, Zp, NRegp, and NCIP
filter(brain_pub_csv, scopus_id==7102554450)

brain_pubdets_csv <- read.csv("brain_publication_details.csv",stringsAsFactors = FALSE)


# From brain_Author_CSV, take out: scopus_id, 1st name, last namem 

# when you have the same co-author scopus as scopus_id, it is that person
filter(brain_pub_csv, scopus_id==7102554450)
# need to match eids of specific author paper from brain_pub_csv to
# dataFromCSV to get the region that paper was published from Regionp
# NCIPp (1-4 is Neuro/Bio), (5-7 is Health), (8-9 is Sci/Eng)

2-s2.0-0032939971

filter(data_csv, eidsp == '2-s2.0-0032939971')
# filtering, I get the year published Yp and IRegionRefinedp

# FOR NODES FILE
# 1. for loop brain_aut_csv and get all scopus_id and names
#    and gather in columns
# 2. Match scopus_id in brain_aut_csv to co-author_scopus_id 
#    & scopus_id in brain_pub_csv, gather ALL eids identifiers 
#    related to each author.
# 3. Match eids* identifiers in brain_pub_csv with eidsp in 
#    data_csv, find NCIP of author
# 4. Gather year Yp and add to bin when the author published
#    (y_1999, y_2004, y_2009, y_2014)
# 5. Find for each bin whether author published that year whether 
#    number > 0. If true, make column labeling 1 for author,
#    else if false label 0 for author time bin.



# 1. for loop brain_aut_csv and get all scopus_id and names
#    and gather in columns
# 2. Match scopus_id in brain_pub_csv to co-author_scopus_id
#    and scopus_id in brain_pub_csv and scopus_id. 
#    Same author/coauthor scopus_id is main author. Gather eids 
#    identifier
# 3. Match eids identifier in brain_pub_csv with eidsp in 
#    data_csv
# 4. Gather year Yp, category NCIPp
scop <- brain_aut_csv %>% 
  group_by(scopus_id, firstName, lastName) %>% 
  mutate(nrow = nrow(data.frame(scopus_id)))

dim(scop)[1]

scop_col <- matrix(data = 0, nrow = dim(scop)[1], ncol = 3)
pub_list <- vector("list", dim(scop)[1])


for (i in 1:(dim(scop)[1]) ){
  scop_col[i,1] <- scop[[i,1]]
  scop_col[i,2] <- scop[[i,2]]
  scop_col[i,3] <- scop[[i,3]]
}

for (i in 1:(dim(scop)[1]) ){
  pub_list[i] <- (filter(brain_pub_csv, (scopus_id  == as.numeric(scop_col[i,1]) & co_author_scopus_id == as.numeric(scop_col[i,1])) )[2])
}

(filter(brain_pub_csv, (scopus_id  == as.numeric(scop_col[i,1]) & co_author_scopus_id == as.numeric(scop_col[i,1])) )[2])


pub_mat <- matrix(data = 0, nrow = dim(scop)[1], ncol = as.numeric(filter(brain_aut_csv, num_publications == max(num_publications) )[[13]]) )

for (i in 1:(dim(scop)[1]) ){
  tmp <- (filter(brain_pub_csv, (scopus_id  == as.numeric(scop_col[i,1]) & co_author_scopus_id == as.numeric(scop_col[i,1])) )[2])
  for (j in 1:dim(tmp)[1] ){
    #  pub_list[i] <- (filter(brain_pub_csv, (scopus_id  == as.numeric(scop_col[i,1]) & co_author_scopus_id == as.numeric(scop_col[i,1])) )[2])
    pub_mat[i,j] <- tmp[j,1]
  }
}



