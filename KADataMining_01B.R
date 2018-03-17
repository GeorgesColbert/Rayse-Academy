library(readxl)
library(ggplot2)
library(dplyr)

################ the is the data taken from CCU01B class

Summary.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report -- 03-16-1970 to 03-31-2018.xlsx", sheet = 'Summary')
Mission.specific.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report -- 03-16-1970 to 03-31-2018.xlsx", sheet = "Mission-specific")
Exercises.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report -- 03-16-1970 to 03-31-2018.xlsx", sheet = "Exercises")
Videos.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report -- 03-16-1970 to 03-31-2018.xlsx", sheet = "Videos")
Points.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report -- 03-16-1970 to 03-31-2018.xlsx", sheet = "Points")
Badges.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report -- 03-16-1970 to 03-31-2018.xlsx", sheet = "Badges")
Skills.b <- read_excel("Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Khan Academy Report Skill Progress.xlsx")

str(Summary.b)


####Filter for  only the exercises that are part of the 3rd grade mission 

grade3b.ex <- Exercises.b[Exercises.b$Exercise %in% Skills.b$Exercise,]


#what is the one exercise in the 3rd grade mission that no student has yet attempted?

nrow(Skills.b)

length(unique(grade3b.ex$Exercise))

n <-   unique(grade3b.ex$Exercise)  
p <- Skills.b$Exercise


setdiff(p,n)



######## Create a total minutes spent on Mission Collumn

ccu01b <- Mission.specific.b

m <- grade3b.ex %>% select(Student,`Time Spent (min)`) %>% group_by(Student) %>% summarise(total.minutes = sum(`Time Spent (min)`))

m <- as.data.frame(m)

ccu01b <- full_join(ccu01b,m, by="Student")

 
#####Creating a totalHours Collumn


myHours <- function (x) {
  return (x/60)
}

totalHours <- lapply(ccu01b$total.minutes, myHours)
totalHours <- unlist(totalHours)
ccu01b <- cbind(ccu01b,totalHours)


#### Summary of totalHours

summarise(ccu01b, maxHours=max(ccu01b$totalHours,na.rm=TRUE), medianhours=median(ccu01b$totalHours,na.rm=TRUE), meanHours=mean(ccu01b$totalHours,na.rm=TRUE))



###### Creating a Weeks Performing Collumn

myWeeks <- function (x) {
  return (x/3)
}

totalWeeks <- lapply(ccu01b$totalHours, myWeeks)
totalWeeks <- unlist(totalWeeks)
ccu01b<- cbind(ccu01b,totalWeeks)



summarise(ccu01b, max=max(ccu01b$totalWeeks,na.rm=TRUE), median=median(ccu01b$totalWeeks,na.rm=TRUE), mean=mean(ccu01b$totalWeeks,na.rm=TRUE))




########### Time Spent on Exercises

##### Overall Numbers for Time Spent on mission exercises

summarise(grade3b.ex, max = max(`Time Spent (min)`),
          min = min(`Time Spent (min)`),
          avg = mean(`Time Spent (min)`),)

### Time spent by
i <- grade3b.ex %>% select(Student,`Time Spent (min)` ) %>% group_by(Student) %>% summarise(Ex.time.max = max(`Time Spent (min)`),
                                                                                           Ex.time.min = min(`Time Spent (min)`),
                                                                                           ex.time.avg = mean(`Time Spent (min)`),
                                                                                           ex.time.median = median(`Time Spent (min)`))                                                                                   
                                                                                  

i <- as.data.frame(i)

ccu01b <- full_join(ccu01b,i, by="Student")


##### Summarize time spent per  Excercise

d <- grade3b.ex %>% select(Exercise,`Time Spent (min)` ) %>% group_by(Exercise) %>% summarise(Ex.time.max = max(`Time Spent (min)`),
                                                                                             Ex.time.min = min(`Time Spent (min)`),
                                                                                             ex.time.avg = mean(`Time Spent (min)`),
                                                                                             ex.time.median = median(`Time Spent (min)`)) 

head(d)





##### Create a progress collumn

nSkills <- nrow(Skills.b)

prog <- function (a,b,c,d,e) {
  mastered <- a *4
  level2 <- b *3
  level1 <- c * 1
  practiced <- d*1
  per <- (mastered+level2+level1+practiced)/(4*e)
  return(per)
  
}


Prog.b <- mapply(prog, Mission.specific.b$Mastered,
                   Mission.specific.b$`Level 2`, Mission.specific.b$`Level 1`, Mission.specific.b$Practiced, nSkills)

ccu01b <- cbind(ccu01b,Prog.b)

head(ccu01b)


ccu01b <- ccu01b[order(ccu01b$Prog.b, decreasing = TRUE),]




######################################################  Graphs  ###################################################### 

hist(ccu01b$Prog.b, main= "Distribution of Progress on 3rd grade mission #CCU01B")



plot(ccu01b$ex.time.avg,ccu01b$Prog.b, main= "Does  avg. time per Exercise on Mission. affect Progress #CCU01B", xlab="Ex. Time avg", ylab= " Progress" )

plot(ccu01b$ex.time.median,ccu01b$Prog.b, main= "Does median time per Ex. affect Progress #CCU01B", xlab="Ex. Time median", ylab= " Progress" )


hist(ccu01b$Prog.b, main= " Distribution of Progress #CCU01B")


######### breakdown of  current status
(bean <- table(grade3b.ex$`Current Status`))


ggplot(grade3b.ex,aes(grade3b.ex$`Current Status`))+geom_bar()








########################  Time Series 


##### Time series of Student participation per day

grade3b.ex$`Last Done` <- as.Date(grade3b.ex$`Last Done`) 


d <- grade3b.ex %>% select(`Last Done`,Student ) %>% group_by(`Last Done`) %>% summarise(nstudents= n_distinct(Student))


ggplot(d, aes(`Last Done`, nstudents)) + geom_line() +
  scale_x_date() + xlab("  ") + ylab(" # Students on KA ") + coord_cartesian(ylim = c(0, 11)) 

######## Make a list



a <- select(ccu01b,Student, Prog.b)

a$Email <- paste(a$Student,"@amudc.org", sep="")

write.csv(a, file = "Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/Students.csv", row.names=FALSE)

write.csv(ccu01b, file = "Desktop/Projects/RAYSE ACADEMY/Untitled Folder/CCU01B/grade3b.csv", row.names=FALSE)
