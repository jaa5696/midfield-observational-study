library(data.table)
library(tidyr)
#I got these from Google: https://nces.ed.gov/ipeds/cipcode/browse.aspx?y=55
stem_nums30<-c("01", "06", "08", "15", "16", "18", "27", "30", "31", "32")
stem_nums<-c("10", "11", "14", "15", "26", "27", "29", "30", "40", "41")

#manipulate student data: get sex and race
midstud<-midfielddata::midfieldstudents
midstud$first2<-substr(midstud$cip6, start=1, stop=2)
midstud$stem<-ifelse(midstud$first2 %in% stem_nums, ifelse(midstud$first2=="30", ifelse(substr(midstud$cip6, start=3, stop=4) %in% stem_nums30, 1, 0), 1), 0)
midstud<-midstud[,c("id", "institution", "sex", "race")]

#manipulate degrees: used to check if they graduate with stem degree
deg<-midfielddata::midfielddegrees
deg<-na.omit(deg)
deg$first2<-substr(deg$cip6, start=1, stop=2)
deg$stem<-ifelse(deg$first2 %in% stem_nums, ifelse(deg$first2=="30", ifelse(substr(deg$cip6, start=3, stop=4) %in% stem_nums30, 1, 0), 1), 0)
deg<-deg[,c("id", "institution", "stem")]
setnames(deg, "stem", "stem_deg")

#Manipulate Courses: i.e. find calc I
courses<-as.data.table(midfielddata::midfieldcourses)

calc1_courses<-c("Calculus-Management Sciences" , "Calculus-Physical Scientists I", "Calculus-Biolog Scientists I"  , "Calculus-Biological Sci. I", "Calculus-Biological Sci. I", "A First Course in Calculus", "Calculus I","Survey of Calculus" ,"One Year Calculus 1B" ,"One Year Calculus IB" , "MA Concepts of Calculus" ,"MA Calculus I" , "Concepts of Calculus", "MA Calculus I with Intensive Review of Precalculus", "Calculus I-Bus, Life, Soc Sci","Calculus & Analyt Geometry I" , "Calc/Analytic Geom I","Calc & Analyt Geom I  (HONORS)"  )                            
Bnums<-c('1300', '1310', '1330')
Jnums<-c('1015', '1016')
Hnums<-c('121', '131', '141')

courses$course<-as.character(courses$course)      
courses<-courses[,calc1:=ifelse((course %in% calc1_courses) | (institution == "Institution A" & abbrev == "MAC" & number == '2311') |  (institution == "Institution B" & abbrev == "MATH" & number %in% Bnums) | (institution == "Institution E" & abbrev == "MAC" && number == '3311') | (institution == "Institution H" & abbrev == "MA" & number %in% Hnums) | (institution == "Institution J" & abbrev == "MATH" & number %in% Jnums) | (institution == "Institution M" & abbrev == "MTHSC" & number == '106'), 1, 0)]
courses<-courses[calc1==1]
calc1_terms<-courses[,c("id", 'institution', "term_course")]


#Manipulate Terms data (so know if they are still in STEM within next 2 semesters)
terms<-as.data.table(midfielddata::midfieldterms)
terms$first2<-substr(terms$cip6, start=1, stop=2)
terms$stem <- ifelse(terms$first2 %in% stem_nums, ifelse(terms$first2=="30", ifelse(substr(terms$cip6, start=3, stop=4) %in% stem_nums30, 1, 0), 1), 0)
terms<-terms[, c("id", "stem", "term")]

gterms<-terms[, .(n=.N), by="term"]
gterms$rank<-rank(gterms$term)
gterms<-gterms[,c("term", "rank")]
terms<-merge(terms, gterms)
terms<-terms[,c("id", "rank", "stem")]
calc1_terms<-merge(calc1_terms, gterms, by.x = 'term_course', by.y= 'term', all.x=TRUE)

#Create a master data file that can be used for glm
master<-merge(calc1_terms, midstud, by=c("id", "institution"), all.x=TRUE)
master<-merge(master, deg, by=c("id", "institution"), all.x=TRUE)

master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)
master<-master[stem==1]

setnames(terms, "stem", "stem_1")
terms$rank<-terms$rank-1
master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)

setnames(terms, "stem_1", "stem_2")
terms$rank<-terms$rank-1
master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)

setnames(terms, "stem_2", "stem_3")
terms$rank<-terms$rank-1
master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)

setnames(terms, "stem_3", "stem_4")
terms$rank<-terms$rank-1
master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)

setnames(terms, "stem_4", "stem_5")
terms$rank<-terms$rank-1
master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)

setnames(terms, "stem_5", "stem_6")
terms$rank<-terms$rank-1
master<-merge(master, terms, by=c("id", "rank"), all.x=TRUE)
master[is.na(master)] <- -1
master<-master[,dropout:=ifelse(stem_deg==1, 0, ifelse(stem_4==1 | stem_5==1 | stem_6==1, 0,1))]
master<-master[,c("id", "institution", "term_course", "sex", "race", "dropout")]
master$sex<-as.factor(master$sex)
master$race<-as.factor(master$race)

#Handles the cases where student repeats Calc I
m<-master[, .(term_course=max(term_course)), by="id"]
master<-merge(m, master, by=c("id", "term_course"))
master<-master[!duplicated(master), ]

fwrite(master, "master_glm.csv")

#PROBLEMS:
#only 3 schools reported course names (and even those aren't exactly clear)
#I'm really only guessing on what the abreviation for math is for some of the schools (only 6 use MATH)

#OPTIONS:
#Take only courses that are clearly and explicitly Calc I
#Take named courses that could be reasonably interpreted as calc I
#Try to guess Calc I course for schools that don't give course titles (most frequently taken 100 level math)
  #Could relatedly try to use the course abbreviations and numbers to link back to a college (I think one MIGHT be Texas A&M, but I don't have strong support)


# M=Clemson: MTHSC 106
# B = University of Colorado at Boulder; MATH 1300, 1310, 1330
# E: in Flordia?? I think MAC 3311 is calc 1
# J: VA Tech; MATH 1015, 1016
# A: also florida: MAC 2311
# H: NC State: MA 121, 131, 141

#K: Not sure, but might also be MAC 3311 (pure guess)
#F: could be Georgia Institute of Tech. but I don't think it is
#G: couldn't find
#I: appears not to exist in data???

