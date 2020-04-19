library(data.table)
library(tidyr)
#I got these from Google: https://nces.ed.gov/ipeds/cipcode/browse.aspx?y=55
stem_nums30<-c("01", "06", "08", "15", "16", "18", "27", "30", "31", "32")
stem_nums<-c("10", "11", "14", "15", "26", "27", "29", "30", "40", "41")

#manipulate student data
midstud<-midfielddata::midfieldstudents
midstud$first2<-substr(midstud$cip6, start=1, stop=2)
midstud$stem<-ifelse(midstud$first2 %in% stem_nums, ifelse(midstud$first2=="30", ifelse(substr(midstud$cip6, start=3, stop=4) %in% stem_nums30, 1, 0), 1), 0)
midstud<-midstud[,c("id", "institution", "sex", "race", "term_enter", "stem")]

#manipulate degrees
deg<-midfielddata::midfielddegrees
deg<-na.omit(deg)
deg$first2<-substr(deg$cip6, start=1, stop=2)
deg$stem<-ifelse(deg$first2 %in% stem_nums, ifelse(deg$first2=="30", ifelse(substr(deg$cip6, start=3, stop=4) %in% stem_nums30, 1, 0), 1), 0)
deg<-deg[,c("id", "institution", "term_degree", "stem")]
setnames(deg, "stem", "stem_deg")

#Merge students and degrees
m<-merge(deg, midstud)
m<-as.data.table(m)
m<-m[stem_deg == 1]#takes only stem degrees; note that the major attached to student and that to degree don't always match
m<-m[,term_degree:=floor(term_degree/10)] #gets year; I am combining all semesters of a given year (not school year)

m_f<-m[,sex:=ifelse(sex=="Female", 1, 0)]
m_f<-m_f[, keyby = .(term_degree, institution),
  .(prop_fem = ifelse(.N<10, -1, sum(sex)/.N))]#groups by year/institution and summarises proportion if at least 10 graduate (the -1 otherwise is later excluded; you can change this if you want)
m_f<-m_f[prop_fem>=0]
m_f$term_degree<-as.factor(m_f$term_degree)
m_f$institution<-as.factor(m_f$institution)