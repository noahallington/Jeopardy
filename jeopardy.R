library(cleanNLP)
library(tidyverse)

cnlp_init_udpipe()

jeopardy<-read.csv('./JEOPARDY_CSV.csv', stringsAsFactors=F)

#Clean Answers: 
#TODO gsub parenthesized answers to allow for splitting into own rows
jeopardy<-jeopardy%>%mutate(Answer=tolower(Answer),qid=row_number())%>%mutate(Answer=gsub('(\\(.* or .*\\)|\\(.* and .*\\))',str_replace_all('\\1','\\(|\\)',''),Answer))

#Identify questions that have multiple answers and create new rows for each answer
one_ofs<-jeopardy%>%filter(grepl('1 of.*',Answer))

one_ofs_2<-one_ofs%>%mutate(Answer=gsub('\\(.*\\)','',Answer))%>%separate_rows(Answer, sep='(,|&)')

#Aggregate Answers
answers<-jeopardy%>%select(Answer)%>%group_by(Answer)%>%summarize(count=n())


#Conduct TF-IDF to pull top words from each Question
j_annotation<-cnlp_annotate(jeopardy[1:1000,]$Question)

tokens<-cnlp_get_token(j_annotation)%>%filter(!(upos %in% c('PUNCT','ADP')))

j_tf_idf<-cnlp_get_tfidf(tokens, type='tfidf',tf_weight='lognorm', idf_weight = 'idf', doc_var = 'id',token_var = 'lemma', vocabulary=unique(tokens$lemma))

j_entities<-cnlp_get_token(j_annotation)

test<-cnlp_get_entity(obama)


#Use ML to correlate keyword combinations to answers
