
### Clear memory
rm(list = ls())

library(dplyr)

#dataset transformation

database <- read.csv2("Airline.csv", header=TRUE)

var_delete <- c('OriginGMT','DestinationGMT','Direction','q11_DepartureOrArrivalIsImportant','q12_IdealDepTime','q13_IdealArrTime','DepartureTimeHours_1','DepartureTimeHours_2','DepartureTimeHours_3','DepartureTimeMins_1','DepartureTimeMins_2','DepartureTimeMins_3','ArrivalTimeHours_1','ArrivalTimeHours_2','ArrivalTimeHours_3','ArrivalTimeMins_1','ArrivalTimeMins_2','ArrivalTimeMins_3','TripPurpose','BestAlternative_1','BestAlternative_2','BestAlternative_3','FlyingTimeHours_1','FlyingTimeHours_2','FlyingTimeHours_3','q16_Income','q19_Occupation',
                'AirplaneFirstFlight_1', 'AirplaneFirstFlight_2', 'AirplaneFirstFlight_3', 'AirplaneSecondFlight_1','AirplaneSecondFlight_2','AirplaneSecondFlight_3',
                'AirlineSecondFlight_1','AirlineSecondFlight_2','q03_WhoPays')

database <- database %>%
  mutate(Choice = BestAlternative_1 * 1 + BestAlternative_2 * 2 + BestAlternative_3 *3,
         StopPenaltyHours_2 = TripTimeHours_2 - FlyingTimeHours_2,
         StopPenaltyHours_3 = TripTimeHours_3 - FlyingTimeHours_3,
         IncomeDummy = ifelse(Cont_Income <= 110, 1,0),
         DiffTime1 = ifelse(q11_DepartureOrArrivalIsImportant ==1, abs((DepartureTimeMins_1 - q12_IdealDepTime)/60),
                            ifelse(q11_DepartureOrArrivalIsImportant ==2, abs((ArrivalTimeMins_1 - q13_IdealArrTime)/60),NA)),
         DiffTime2 = ifelse(q11_DepartureOrArrivalIsImportant ==1, abs((DepartureTimeMins_2 - q12_IdealDepTime)/60),
                            ifelse(q11_DepartureOrArrivalIsImportant ==2, abs((ArrivalTimeMins_2 - q13_IdealArrTime)/60),NA)),
         DiffTime3 = ifelse(q11_DepartureOrArrivalIsImportant ==1, abs((DepartureTimeMins_3 - q12_IdealDepTime)/60),
                            ifelse(q11_DepartureOrArrivalIsImportant ==2, abs((ArrivalTimeMins_3 - q13_IdealArrTime)/60),NA))
         ) %>%
  filter(!(q02_TripPurpose %in% c(-1,99,0)) & !(q16_Income %in% c(-1,99)) & 
          !(q14_PartySize %in% c(-1,99)) & !(q15_Age %in% c(-1,99,1)) &
          !(q17_Gender %in% c(-1,99))  & !(q20_Education %in% c(-1,99)) & 
          !(Cont_Income %in% c(-1,99)) & !(q11_DepartureOrArrivalIsImportant %in% c(-1,0))
         ) %>%
  select (-any_of(var_delete)) %>%
  rename(TripPurpose = q02_TripPurpose,
         PartySize = q14_PartySize, Age = q15_Age,
         Gender = q17_Gender, Education = q20_Education,
         Legroom1 = Legroom_1, Legroom2 = Legroom_2, Legroom3 = Legroom_3) %>%
  mutate(TripPurpose = ifelse(TripPurpose == 3,1,TripPurpose),
         TripPurpose = ifelse(TripPurpose == 4,1,TripPurpose),
         Age = ifelse(Age == 8,6,Age),
         Age = ifelse(Age == 7,6,Age),
         Age = ifelse(Age == 3.5,3,Age),
         Age = ifelse(Age == 4,3,Age),
         Education = ifelse(Education == 1,3,Education),
         Education = ifelse(Education == 2,3,Education),
         Education = ifelse(Education == 4,6,Education),
         Education = ifelse(Education == 5,6,Education),
         Education = ifelse(Education == 8,7,Education),
         Education = ifelse(Education == 9,7,Education))

database$Gender <- factor(database$Gender)
database$TripPurpose <- factor(database$TripPurpose)
database$TripPurpose <- relevel(database$TripPurpose, ref=2)
database$Age <- factor(database$Age)
database$Education <- factor(database$Education)
database$Legroom1 <- factor(database$Legroom1)
database$Legroom2 <- factor(database$Legroom2)
database$Legroom3 <- factor(database$Legroom3)

a<- as.data.frame(model.matrix(Choice ~., data=database)) [,-c(1)]
a<- cbind(a,Choice = database$Choice)

a<- a %>%
  rename(BusinessTrip = TripPurpose1,
         Female = Gender2,
         Undergraduate = Education6,
         Posgraduate = Education7,
         Income = Cont_Income,
         Age25_44 = Age3,
         Age45_54 = Age5,
         Age55 = Age6)

write.csv2(a, file = "Airline_db.csv", row.names = FALSE )
