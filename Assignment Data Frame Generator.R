getwd()
setwd("/Users/sanjiwacika/Dropbox/Departemen Ilmu Ekonomi (Teaching Materials)/Statistika II/Assignment/")

n <- 4000
drivers <- runif(n,0,1)
df <- data.frame(drivers)
df %>% group_by(drivers) %>% tally()
df$drivers <- ifelse(df$drivers>0.5,1,0)

#gen age
df$age <- round(rnorm(n,30,2))
summary(df$age)

#gen gender
df$gender <- rnorm(n,0.7,1)
df$gender <- ifelse(df$gender>0,1,0)
summary(df$gender)

#gen income
df$income <- ifelse(df$drivers==1,
                    round(rnorm(length(df[df$drivers==1,]$drivers),339450,60220)),
                    round(rnorm(length(df[df$drivers==0,]$drivers),280250,39500)))
df$income <- ifelse(df$income<=50000, min(df$income), df$income)
summary(df$income)

#gen work hours
df$work_hours <- ifelse(df$drivers==1,
                    round(rnorm(length(df[df$drivers==1,]$drivers),9.8,1.5)),
                    round(rnorm(length(df[df$drivers==0,]$drivers),8.2,1.1)))
df$work_hours <- ifelse(df$work_hours<=1,1, df$work_hours)
summary(df$work_hours)

#gen drive distance
df$distance <- ifelse(df$drivers==1,
                      round(rnorm(length(df[df$drivers==1,]$drivers),84,19)),
                      round(rnorm(length(df[df$drivers==0,]$drivers),80,25)))
df$distance <- ifelse(df$distance<=10,10, df$distance)
summary(df$distance)

#gen bonus
df$bonus <- ifelse(df$drivers==1,
                    round(rnorm(length(df[df$drivers==1,]$drivers),19450,10220)),
                    round(rnorm(length(df[df$drivers==0,]$drivers),25020,26950)))
df$bonus <- ifelse(df$bonus<=10000,10000, df$bonus)
summary(df$bonus)

#gen satisfaction
df$satisfaction <- ifelse(df$drivers==1,
                   round(rnorm(length(df[df$drivers==1,]$drivers),2.9,1.5)),
                   round(rnorm(length(df[df$drivers==0,]$drivers),3.3,0.9)))
df$satisfaction <- ifelse(df$satisfaction<=0,1,
                          ifelse(df$satisfaction>=5,5, df$satisfaction))
summary(df$satisfaction)

#gen leaving 
df$leaving <- ifelse(df$satisfaction>2.5,
                     sample(c(1, 2, 3, 4, 5), 
                            size = length(df[df$satisfaction>2.5,]$satisfaction), 
                            replace = TRUE, prob = c(0.05, 0.05, 0.25, 0.45, 0.2)),
                     sample(c(1, 2, 3, 4, 5), 
                            size = length(df[df$satisfaction<=2.5,]$satisfaction), 
                            replace = TRUE, prob = c(0.2, 0.35, 0.3, 0.1, 0.05)))
summary(df$leaving)


write.csv(df, file = "Ojol.csv")

