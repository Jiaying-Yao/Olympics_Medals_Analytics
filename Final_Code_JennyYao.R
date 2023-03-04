library(ggplot2)
library(dplyr)
library(ggfortify)
library(randomForest)
library(MASS)
library(klaR)
library(rpart)				        
library(rpart.plot)
library(reshape) 
library(purrr)

## Import Data
olympic <- read.csv("../data/Olympic.csv")
olympic =  olympic[olympic$Season == 'Summer',]
olympic_fencing =  olympic[olympic$Sport == 'Fencing',]
attach(olympic_fencing)

#Dummify Sex column
olympic_fencing$Sex[olympic_fencing$Sex == 'M'] = 1
olympic_fencing$Sex[olympic_fencing$Sex == 'F'] = 0
olympic_fencing = transform(olympic_fencing, Sex = as.numeric(olympic_fencing$Sex))
#remove Netherlands Antilles team since it belongs to two countries
olympic_fencing = olympic_fencing[olympic_fencing$Team!='Netherlands Antilles', ]
#combine west germany, east germany, and germany
olympic_fencing[olympic_fencing == 'West Germany'] = 'Germany'
olympic_fencing[olympic_fencing == 'FRG'] = 'GER'
olympic_fencing[olympic_fencing == 'East Germany'] = 'Germany'
olympic_fencing[olympic_fencing == 'GDR'] = 'GER'

#city and country of host
#host = 1 if the athlete's team is the host country
cities = unique(olympic_fencing$City)
countries = c('Sweden', 'China', 'Greece', 'Mexico', 'Spain', 'Great Britain', 'Finland', 'Germany', 'Italy', 'Japan', 'Netherlands', 'Brazil', 'United States', 'Canada','Russia', 'South Korea', 'Germany', 'France', 'Australia', 'Australia', 'Belgium', 'United States', 'United States')
city.country = data.frame(City = cities, Country = countries)
olympic_fencing = merge(x=olympic_fencing, y=city.country, by='City', all.x=TRUE)
olympic_fencing$host = ifelse(olympic_fencing$Team==olympic_fencing$Country, 1, 0)

#TeamEvent = 1 if this event is in teams
foo = data.frame(do.call('rbind', strsplit(as.character(olympic_fencing$Event, remove=TRUE),',',fixed=TRUE)))
olympic_fencing['TeamEvent'] = ifelse(foo[,2]==' Team', 1, 0)

#won = 1 if the athlete gained a medal in this game
olympic_fencing['won'] = ifelse(is.na(olympic_fencing$Medal), 0, 1)
olympic_fencing_nona = subset(olympic_fencing, !is.na(Age)&!is.na(Height)&!is.na(Weight))
attach(olympic_fencing)


## Exploratory Data Analysis
### athlete_df/athlete_df_nona: Athletes-wise dataset
athletes_df = olympic_fencing
cs_medals = athletes_df %>%
        group_by(ID) %>%
        dplyr::mutate(cs = cumsum(won))
athletes_df['n_medals'] = cs_medals['cs']-athletes_df['won']
athletes_df_nona = subset(athletes_df, !is.na(Age)&!is.na(Height)&!is.na(Weight))


#### Profiling
sapply(athletes_df, n_distinct)


athletes_df[,c('City', 'Sex', 'Year', 'Event', 'Medal', 'Country', 'Team', 'host', 'won')] %>%
        sapply(table)

#### Data Visualization
counts <- athletes_df
counts %>%
        count(Event) %>%
        slice_max(n, n = 7, with_ties = FALSE) %>%
        ggplot(aes(Event, n)) + geom_col()

plot_df = athletes_df
plot_df = transform(plot_df, won = as.character(plot_df$won))
plot_df$won=as.factor(plot_df$won)
plot_df = transform(plot_df, host = as.character(plot_df$host))
plot_df$host=as.factor(plot_df$host)
ggplot(plot_df, aes(group=won, x=won, y=Height, fill=won)) + 
        stat_boxplot(geom="errorbar", width=0.25) + geom_boxplot()+
        ylab("Height")

ggplot(plot_df, aes(group=won, x=won, y=Weight, fill=won)) + 
        stat_boxplot(geom="errorbar", width=0.25) + geom_boxplot()+
        ylab("Weight")

ggplot(plot_df, aes(group=won, x=won, y=Age, fill=won)) + 
        stat_boxplot(geom="errorbar", width=0.25) + geom_boxplot()+
        ylab("Age")

ggplot(plot_df, aes(group=won, x=won, y=n_medals, fill=won)) + 
        stat_boxplot(geom="errorbar", width=0.25) + geom_boxplot()+
        ylab("Previous Number of Medals")

plot_df_long <- melt(plot_df[,c('Height', 'Weight', 'Age', 'won')], id = "won")
ggplot(plot_df_long, aes(x = variable, y = value, fill=won)) + geom_boxplot()

ggplot(plot_df, aes(x=n_medals, y=host, col=won)) +
        geom_point()+
        labs(x="Number of Previous Medals", y="Is the Host")

ggplot(plot_df[plot_df$won=='1',], aes(x=n_medals, col=host)) + geom_density()+xlab("Number of Previous Medals")
ggplot(plot_df[plot_df$won=='1',], aes(x=Weight, col=host)) + geom_density()+xlab("Weight")
ggplot(plot_df[plot_df$won=='1',], aes(x=Height, col=host)) + geom_density()+xlab("Weight")
ggplot(plot_df[plot_df$won=='1',], aes(x=Age, col=host)) + geom_density()+xlab("Age")


### Country-wise
foo1 = olympic_fencing_nona[, c( "Age", "Height", "Weight", "Team", "Year")]
foo1 = foo1[!duplicated(foo1), ] %>%
        group_by(Team, Year) %>% summarise(across(c("Age", "Height", "Weight"), mean))

foo2 = olympic_fencing[, c("Sex", "Team", "Year", "ID")]
foo2 = foo2[!duplicated(foo2), ] %>%
        group_by(Team, Year) %>% summarise(n_Males = sum(Sex))

foo3 = olympic_fencing[, c("ID",  "Team", "Year", "Event", "host")] %>%
        group_by(Team, Year) %>% summarise(across(c("host"), mean), n_Events = n_distinct(Event), Team_Size = n_distinct(ID))

foo_41 = olympic_fencing[olympic_fencing$TeamEvent==1,c("Team", "Year", "Medal", "Event", "won")]
foo_42 = olympic_fencing[olympic_fencing$TeamEvent==0,c("Team", "Year", "Medal", "won")]

foo41 = foo_41 %>%
        group_by(Team, Year, Event) %>% summarise(won = max(won))%>% 
        group_by(Team, Year) %>% summarise(n_team_medal = sum(won))
foo42 = foo_42 %>%
        group_by(Team, Year) %>% summarise(n_single_medal = sum(won))
foo4 = left_join(foo41, foo42, by=c("Team", "Year"))
foo4$n_medals = foo4$n_team_medal + foo4$n_single_medal
#foo4 =  within(foo4, rm('movieTitle','movieID','imdbLink'))
countries_df = left_join(foo2, foo1, by=c("Team", "Year"))
countries_df = left_join(countries_df, foo3, by=c("Team", "Year"))
countries_df = left_join(countries_df, foo4, by=c("Team", "Year"))
countries_df[c("n_team_medal", "n_single_medal", "n_medals")][is.na(countries_df[c("n_team_medal", "n_single_medal", "n_medals")])] = 0
countries_df$n_Females = countries_df$Team_Size - countries_df$n_Males

foo = countries_df[,c("Year", "n_medals")] %>% group_by(Year) %>% summarise(total_medals = sum(n_medals))
foo = left_join(countries_df, foo, by=c("Year"))
countries_df = foo%>% mutate(prop_medal = n_medals/total_medals)%>%within(rm(total_medals))

cs_medals = countries_df %>%
        group_by(Team) %>%
        dplyr::mutate(cs = cumsum(n_medals))
countries_df['cum_n_medals'] = cs_medals['cs']-countries_df['n_medals']

#### Profiling
sapply(countries_df, n_distinct)
countries_df[,c('Year', 'Team', 'host')] %>%
        sapply(table)

#### Visualization
plot2_df = countries_df %>% group_by(Year) %>% summarise(count=n())
ggplot(plot2_df, aes(x=Year, y=count)) + geom_line()+ labs(x="Year", y="Number of countries participated") + geom_smooth()
ggplot(countries_df[countries_df$Team %in% c("France",  "United States", "Italy", "Sweden", "Austria", "Great Britain", "Hungary", "Germany"),], aes(x=Year, prop_medal, group=Team, col=Team)) + geom_line()+geom_point()

plot3_df = transform(countries_df, host = as.character(countries_df$host))
plot3_df$host=as.factor(plot3_df$host)
plot3_df = plot3_df[plot3_df$Year >1900,c("Year", "host", "n_medals")]%>%
        group_by(Year, host)%>%summarise(mean_medal = mean(n_medals))
ggplot(plot3_df, aes(x=Year, y=mean_medal, col=host))+
        geom_line()+ xlab("Year")+ylab("Avg Number of Medals")

ggplot(countries_df[countries_df$Team=='France'&countries_df$Year>=1900, ], aes(x=Year, y = prop_medal)) + geom_line() +
        geom_hline(yintercept = mean(
                countries_df[countries_df$Team=='France'&countries_df$Year>1900, ]$prop_medal), 
                color="blue")+
        geom_text(aes(label = round(prop_medal,2)),
                  vjust = "inward", hjust = "inward",
                  show.legend = FALSE)

ggplot(countries_df[countries_df$Team=='Italy'&countries_df$Year>=1900, ], aes(x=Year, y = prop_medal)) + geom_line() +
        geom_hline(yintercept = mean(
                countries_df[countries_df$Team=='Italy'&countries_df$Year>1900, ]$prop_medal), 
                color="blue")+
        geom_text(aes(label = round(prop_medal,2)),
                  vjust = "inward", hjust = "inward",
                  show.legend = FALSE)

ggplot(countries_df[countries_df$Team=='United States'&countries_df$Year>=1900, ], aes(x=Year, y = prop_medal)) + geom_line() +
        geom_hline(yintercept = mean(
                countries_df[countries_df$Team=='United States'&countries_df$Year>1900, ]$prop_medal), 
                color="blue")+
        geom_text(aes(label = round(prop_medal,2)),
                  vjust = "inward", hjust = "inward",
                  show.legend = FALSE)

ggplot(countries_df[countries_df$Team=='France'&countries_df$Year>=1900, ], aes(x=Year, y = n_Events)) + geom_line() +
        geom_hline(yintercept = mean(
                countries_df[countries_df$Team=='France'&countries_df$Year>1900, ]$n_Events), 
                color="blue")+
        geom_text(aes(label = n_Events),
                  vjust = "inward", hjust = "inward",
                  show.legend = FALSE) +
        ylab("Number of Events Participated")

### Principle Component Analysis: importand feature
athletes_num_var = athletes_df_nona[,c("Sex", "Age", "Height", "Weight", "Year", "host", "TeamEvent", "won", "n_medals")]
prcomp(athletes_num_var, scale=TRUE)

athletes_num_var = transform(athletes_num_var, host = as.character(athletes_num_var$host))
athletes_num_var$host=as.factor(athletes_num_var$host)
pca = prcomp(athletes_num_var[,c("Sex", "Age", "Height", "Weight", "Year", "TeamEvent", "won", "n_medals")], scale=TRUE)
autoplot(pca, data = athletes_num_var, loadings = TRUE,  loadings.label = TRUE, colour = 'host' ) + scale_color_manual(values = c("grey","blue"))

### Data Cleaning
boxplot(countries_df$Height, col="yellow", main="Boxplot")
#countries_df[countries_model_df$Height>=200,]
kept_team = c("Great Britain","United States","Italy", "France","Austria","Germany", "Hungary")
`%!in%` <- compose(`!`, `%in%`)


countries_model_df = within(countries_df, rm("n_team_medal", "n_single_medal"))
countries_model_df = countries_model_df[countries_model_df$Year>1950, ]
countries_model_df = countries_model_df[!(countries_model_df$Height <= 160 | countries_model_df$Weight <= 50 | countries_model_df$Weight >= 95 | countries_model_df$Age >= 45 | countries_model_df$prop_medal >= 0.7),]
#countries_model_df = transform(countries_model_df, host = as.character(countries_model_df$host))
countries_model_df$host = as.factor(countries_model_df$host)
countries_model_df = subset(countries_model_df, !is.na(Team)&!is.na(Height)&!is.na(Weight)&!is.na(Age))
countries_model_df[,'Team'][countries_model_df$Team %!in% kept_team,] = 'Other'
countries_model_df$Team = as.factor(countries_model_df$Team)


#athletes_model_df = transform(athletes_df_nona, won = as.character(countries_model_df$won))
athletes_model_df = athletes_df
athletes_model_df$won = as.factor(athletes_model_df$won)
#athletes_model_df = transform(athletes_model_df, host = as.character(athletes_model_df$host))
athletes_model_df$host = as.factor(athletes_model_df$host)
#athletes_model_df = athletes_model_df[!(athletes_model_df$Age>=55 | athletes_model_df$height>=200 | athletes_model_df$Weight >= 100 | athletes_model_df$n_medals >= 10 | athletes_model_df$Year <= 1940), ]
athletes_model_df = athletes_model_df[!(athletes_model_df$Age>=55 | athletes_model_df$Height>=200 | athletes_model_df$Weight >= 100 | athletes_model_df$n_medals >= 10 | athletes_model_df$Year <= 1950), ]
athletes_model_df[,'Team'][athletes_model_df[,'Team'] %!in% kept_team] = 'Other'
athletes_model_df[,'Team'] = as.factor(athletes_model_df[,'Team'])

### Classification Tree: Predict whether an athlete will win a madel
#### best cp
overfittedtree = rpart(won~Sex+Weight+Height+Year+TeamEvent+n_medals+Team, data = athletes_model_df, cp=0.001, na.action=na.omit)
#rpart.plot(overfittedtree)
printcp(overfittedtree)
plotcp(overfittedtree)
opt_cp = overfittedtree$cptable[which.min(overfittedtree$cptable[,"xerror"]), "CP"]

attach(athletes_model_df)
classtree = rpart(won~Sex+Weight+Height+Year+TeamEvent+n_medals+Team+host, data = athletes_model_df, cp=opt_cp, na.action=na.omit)
rpart.plot(classtree)

classforest = randomForest(won~Weight+Height+Year+TeamEvent+n_medals+Team, data = athletes_model_df, cp=0.005, na.action=na.omit, ntree=1000, do.trace=100)
classforest


### Regression Tree: Predict the prop_medal win by each country each year
#### Tune model using OOB
overfittedtree = rpart(prop_medal~n_Males+Age+Height+Weight+n_Events+Team_Size+n_Females+cum_n_medals+Year+Team+host, data=countries_model_df, cp=0.001, na.action = na.omit)
#rpart.plot(overfittedtree)
printcp(overfittedtree)
plotcp(overfittedtree)
opt_cp = overfittedtree$cptable[which.min(overfittedtree$cptable[,"xerror"]), "CP"]

myforest_try=randomForest(prop_medal~n_Males+Age+Height+Weight+n_Events+Team_Size+n_Females+cum_n_medals+Year+Team+host, ntree=1000, data=countries_model_df, importance=TRUE,  na.action = na.omit, do.trace=100, cp=opt_cp)
importance(myforest_try)

myforest=randomForest(prop_medal~n_Males+Height+Weight+n_Events+Team_Size+n_Females+cum_n_medals+Team, ntree=700, data=countries_model_df, na.action = na.omit, cp=opt_cp)
myforest

### Predict French's result in 2024
#Assume Athletes are the same from 2016
#Since we cannot use external data, I have no access to the data for 2022 Tokyo Olympic
#### Create a dataset for 2024
athletes2016_df = athletes_df_nona[athletes_df_nona$Year == 2016, ]
countries2016_df = countries_df[countries_df$Year == 2016, ]
athletes2016_df$Age = athletes2016_df$Age + 8
countries2016_df$Age = countries2016_df$Age + 8
athletes2016_df$Year = athletes2016_df$Year + 8
countries2016_df$Year = countries2016_df$Year + 8

athletes2016_df$n_medals = athletes2016_df$n_medals + athletes2016_df$won
athletes2016_df = within(athletes2016_df, rm("City", "won", "Games", "Medal"))
athletes2016_df$host = ifelse(athletes2016_df$Team == 'France', 1, 0)
athletes2016_df[,'Team'][athletes2016_df$Team %!in% kept_team] = 'Other'
athletes2016_df$Team = as.factor(athletes2016_df$Team)

countries2016_df$cum_n_medals = countries2016_df$n_medals + countries2016_df$cum_n_medals
countries2016_df = within(countries2016_df, rm("n_medals", "n_team_medal", "n_single_medal", "prop_medal"))
countries2016_df_codeTeam = countries2016_df
countries2016_df_codeTeam[,'Team'][countries2016_df_codeTeam$Team %!in% kept_team,] = 'Other'
countries2016_df_codeTeam$Team = as.factor(countries2016_df_codeTeam$Team)
#countries2016_df_codeTeam$host = as.factor(countries2016_df_codeTeam$host)

countries2016_df_France = countries2016_df_codeTeam
countries2016_df_France$host = ifelse(countries2016_df_France$Team == 'France', 1, 0)
countries2016_df_France$host = as.factor(countries2016_df_France$host)
countries2016_df_US = countries2016_df_codeTeam
countries2016_df_US$host = ifelse(countries2016_df_US$Team == 'United States', 1, 0)
countries2016_df_US$host = as.factor(countries2016_df_US$host)

write.csv(athletes2016_df, "../data/Athletes2024.csv", row.names=FALSE)
write.csv(countries2016_df, "../data/Countries2024.csv", row.names=FALSE)


#### Predict Medal portion agained by each team
output_France = data.frame('prop_medal'=predict(myforest, countries2016_df_France))
output_France$Team= countries2016_df$Team
output_France