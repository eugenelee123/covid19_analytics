#Read data into data frames

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
hospital_beds <- read.csv("/Users/eugenelee/Documents/375/data/hospitalbeds.csv")
demographics <- read.csv("/Users/eugenelee/Documents/375/data/demographics.csv")

#Tidy hospital beds
    #Get the most recent hospital bed data for each country
    hospital_beds <- hospital_beds %>% group_by(Country)%>% mutate(maxYear = max(Year)) %>% filter(Year==maxYear)
    #Rename `Hospital.beds..per.10.000.population.` to `beds` and select only Country and the hospital bed data
    hospital_beds <- hospital_beds %>% group_by(Country) %>% summarise(Year = max(Year), beds = Hospital.beds..per.10.000.population.) %>% select(Country,beds)
    #Make `Country` a character to manipulate Country Names
    hospital_beds <- hospital_beds %>% mutate(Country = as.character(Country))

#Tidy Covid-19 Cases and Death data
    deaths <- deaths %>% pivot_longer(c(-Province.State,-Country.Region,-Lat,-Long), names_to = "Date", values_to = "Num_cases") %>% select(-Province.State,-Lat,-Long)
    confirmed <- confirmed %>% pivot_longer(c(-Province.State,-Country.Region,-Lat,-Long), names_to = "Date", values_to = "Num_cases") %>% select(-Province.State,-Lat,-Long)

    #Rename `Country.Region` to just country to help with joins
    confirmed <- confirmed %>% mutate(Country = Country.Region) %>% select(-Country.Region)
    deaths <- deaths %>% mutate(Country = Country.Region) %>% select(-Country.Region)

    #Turn factors in characters
    confirmed <- confirmed %>% mutate(Country = as.character(Country))
    deaths <- deaths %>% mutate(Country = as.character(Country))

    #Get rid of X in `Date`
    deaths <- deaths %>% separate(Date, into = c(NA,"Date"),sep="X")
    confirmed <- confirmed %>% separate(Date, into = c(NA,"Date"),sep="X")

#Tidy demographics
    #Taking "SP" out of the `Series.Code`
   demographics <- demographics %>% separate(Series.Code, into = c(NA,"Series.Code"), sep="SP.")
   demographics <- demographics %>% pivot_wider(names_from = Series.Code, values_from = YR2015)


   #Group male and female in demographics

  #80 and up
  POP.80UP.FE <- demographics %>% select(Country.Name ,POP.80UP.FE) %>% filter(!is.na(POP.80UP.FE)) %>%  group_by(Country.Name)
  POP.80UP.MA <- demographics %>% select(Country.Name ,POP.80UP.MA) %>% filter(!is.na(POP.80UP.MA)) %>%  group_by(Country.Name)
  #Convert from factor to numeric
  POP.80UP.MA <- POP.80UP.MA %>% mutate(POP.80UP.MA = as.numeric(as.character(POP.80UP.MA)))
  POP.80UP.FE <- POP.80UP.FE %>% mutate(POP.80UP.FE = as.numeric(as.character(POP.80UP.FE)))
  #Join male and female together
  POP.80UP <- inner_join(POP.80UP.FE,POP.80UP.MA) %>% mutate(POP.80UP = POP.80UP.FE+POP.80UP.MA) %>% select(Country.Name, POP.80UP)

  #65 and up
  POP.65UP.FE <- demographics %>% select(Country.Name ,POP.65UP.FE.IN) %>% filter(!is.na(POP.65UP.FE.IN)) %>%  group_by(Country.Name)
  POP.65UP.MA <- demographics %>% select(Country.Name ,POP.65UP.MA.IN) %>% filter(!is.na(POP.65UP.MA.IN)) %>%  group_by(Country.Name)
  #Convert from factor to numeric
  POP.65UP.MA_data <- POP.65UP.MA %>% mutate(POP.65UP.MA.IN = as.numeric(as.character(POP.65UP.MA.IN)))
  POP.65UP.FE_data <- POP.65UP.FE %>% mutate(POP.65UP.FE.IN = as.numeric(as.character(POP.65UP.FE.IN)))
  #Join male and female together
  POP.65UP <- inner_join(POP.65UP.FE_data,POP.65UP.MA_data) %>% mutate(POP.65UP = POP.65UP.FE.IN+POP.65UP.MA.IN) %>% select(Country.Name, POP.65UP)


  #15 to 65
  POP.1564.FE <- demographics %>% select(Country.Name ,POP.1564.FE.IN) %>% filter(!is.na(POP.1564.FE.IN)) %>%  group_by(Country.Name)
  POP.1564.MA <- demographics %>% select(Country.Name ,POP.1564.MA.IN) %>% filter(!is.na(POP.1564.MA.IN)) %>%  group_by(Country.Name)
  #Convert from factor to numeric
  POP.1564.MA <- POP.1564.MA %>% mutate(POP.1564.MA = as.numeric(as.character(POP.1564.MA.IN)))
  POP.1564.FE <- POP.1564.FE %>% mutate(POP.1564.FE = as.numeric(as.character(POP.1564.FE.IN)))
  #Join male and female together
  POP.1564 <- inner_join(POP.1564.FE,POP.1564.MA) %>% mutate(POP.1564 = POP.1564.FE+POP.1564.MA) %>% select(Country.Name, POP.1564)

  #0 to 14
  POP.0014.FE <- demographics %>% select(Country.Name ,POP.0014.FE.IN) %>% filter(!is.na(POP.0014.FE.IN)) %>%  group_by(Country.Name)
  POP.0014.MA <- demographics %>% select(Country.Name ,POP.0014.MA.IN) %>% filter(!is.na(POP.0014.MA.IN)) %>%  group_by(Country.Name)
  #Convert from factor to numeric
  POP.0014.MA <- POP.0014.MA %>% mutate(POP.0014.MA = as.numeric(as.character(POP.0014.MA.IN)))
  POP.0014.FE <- POP.0014.FE %>% mutate(POP.0014.FE = as.numeric(as.character(POP.0014.FE.IN)))
  #Join male and female together
  POP.0014 <- inner_join(POP.0014.FE,POP.0014.MA) %>% mutate(POP.0014 = POP.0014.FE+POP.0014.MA) %>% select(Country.Name, POP.0014)

  #Life Expectancy
  lifeExpectancy <- demographics %>% select(Country.Name ,DYN.LE00.IN) %>% na.omit() %>% group_by(Country.Name) %>% summarise(lifeExpectancy = DYN.LE00.IN)
  lifeExpectancyData <- lifeExpectancy %>% mutate(lifeExpectancy = as.numeric(as.character(lifeExpectancy)))

  #Urban Population
  urbanPopulation <- demographics %>% select(Country.Name ,URB.TOTL) %>% na.omit() %>% group_by(Country.Name) %>% summarise(UrbanPOP = URB.TOTL)

  #Mortality
  mortality.FE <- demographics %>% select(Country.Name ,DYN.AMRT.FE) %>% filter(!is.na(DYN.AMRT.FE)) %>%  group_by(Country.Name)
  mortality.MA <- demographics %>% select(Country.Name ,DYN.AMRT.MA) %>% filter(!is.na(DYN.AMRT.MA)) %>%  group_by(Country.Name)
  #Convert from factor to numeric
  mortality.FE <- mortality.FE %>% mutate(mortality.FE = as.numeric(as.character(DYN.AMRT.FE))) %>% select(Country.Name, mortality.FE)
  mortality.MA <- mortality.MA %>% mutate(mortality.MA = as.numeric(as.character(DYN.AMRT.MA))) %>% select(Country.Name, mortality.MA)
  #Join male and female together
  mortality <- inner_join(mortality.FE,mortality.MA) %>% mutate(mortality = mortality.FE+mortality.MA) %>% select(Country.Name, mortality)

  #Total Population
  totalPOP.FE_data <- demographics %>% select(Country.Name ,POP.TOTL.FE.IN) %>% filter(!is.na(POP.TOTL.FE.IN)) %>%  group_by(Country.Name)
  totalPOP.MA_data <- demographics %>% select(Country.Name ,POP.TOTL.MA.IN) %>% filter(!is.na(POP.TOTL.MA.IN)) %>%  group_by(Country.Name)
  #Convert from factor to numeric
  totalPOP.FE_data <- totalPOP.FE %>% mutate(totalPOP.FE = as.numeric(as.character(POP.TOTL.FE.IN))) %>% select(Country.Name, totalPOP.FE)
  totalPOP.MA_data <- totalPOP.MA %>% mutate(totalPOP.MA = as.numeric(as.character(POP.TOTL.MA.IN))) %>% select(Country.Name, totalPOP.MA)
  #Join male and female together
  totalPOP_data <- inner_join(totalPOP.FE_data,totalPOP.MA_data) %>% mutate(totalPOP = totalPOP.FE+totalPOP.MA) %>% select(Country.Name, totalPOP)


  #Join all the subtables together to create tidy demographics
  join <-inner_join(totalPOP_data,mortality)
  join <-inner_join(join,lifeExpectancy)
  join <-inner_join(join,urbanPopulation)
  join <-inner_join(join,POP.0014)
  join <-inner_join(join,POP.1564)
  join <-inner_join(join,POP.65UP)
  join <-inner_join(join,POP.80UP)
  #Save tidy results
  demographics <- join

  #Preparing data for joins
  #Rename mismatching Country Names
    hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country=="Republic of Korea", "South Korea"))
    hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran"))
    hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))

    confirmed <- confirmed %>% mutate(Country = replace(Country, Country=="Korea, South", "South Korea"))
    deaths <- deaths %>% mutate(Country = replace(Country, Country=="Korea, South", "South Korea"))
    confirmed <- confirmed %>% mutate(Country = replace(Country, Country=="Korea, South", "South Korea"))
    deaths <- deaths %>% mutate(Country = replace(Country, Country=="Korea, South", "South Korea"))

    confirmed <- confirmed %>% mutate(cases = Num_cases) %>% select(-Num_cases)
    deaths <- deaths %>% mutate(deaths = cases) %>% select(-cases)

    demographics <- demographics %>% mutate(Country.Name = as.character(Country.Name))
    demographics <- demographics %>% ungroup(Country.Name) %>% mutate(Country.Name = replace(Country.Name, Country.Name=="Korea, Rep.", "South Korea"))
    #Renaming `Country.Name` to `Country`
    demographics <- demographics %>% mutate(Country = Country.Name) %>% select(-Country.Name)
  
  #Join all tidy tables

    join <- inner_join(demographics,hospital_beds)
    join <- inner_join(join,deaths)
    join <- inner_join(join,confirmed)

#Group by Country

tidy_covid_19 <- join


#Transforming predictor variables to try and get better models.
tidy_covid_19 <- tidy_covid_19 %>% mutate(POP.65_prop = POP.65UP/totalPOP)

model <- lm(data=tidy_covid_19, num_deaths~num_cases+POP.0014_prop+POP.65_prop+POP.0014+POP.65UP)

#Evaluation metrics. Got rsquared values from each model.
data <- data.frame("rsquared" = c(0.7922, 0.793, 0.8239, 0.8309, 0.8096, 0.832, 0.002949, 0.01856), "title" = c("Total Cases", "80 and up", "65 and up", "<15 &>65", "80 and up proportion", "<15 & > 65 & < 15 & > 65 proportion", "Beds", "Life Expectancy"))
ggplot(data=data)+geom_col(mapping = aes(x=title,y=rsquared))