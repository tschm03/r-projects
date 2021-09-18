###Section 6 Homework

#recreate boxplot from challenge using new dataset
movies.df <- read.csv("MoreMovies.csv")
#update column names
colnames(movies.df) <- c("Day.of.Week","Director","Genre","Movie.Title",
                         "Release.Date","Studio","AdjGrossMillions",
                         "BudgetMillions","GrossMillions","IMDb.Rating",
                         "MovieLens.Rating","OverseasGrossMillions",
                         "OverseasGrossPercent","ProfitMillions",
                         "ProfitPercent","RuntimeMinutes","USGrossMillions",
                         "USGrossPercent")
#set factors
movies.df$Genre <- factor(movies.df$Genre)
movies.df$Studio <- factor(movies.df$Studio)

#subset data for specific genres & studios
movies.genre <- movies.df[movies.df$Genre=="action"
                              | movies.df$Genre=="adventure"
                              | movies.df$Genre=="animation"
                              | movies.df$Genre=="comedy"
                              | movies.df$Genre=="drama",]
movies.subset <- movies.genre[movies.genre$Studio=="Buena Vista Studios"
                              |movies.genre$Studio=="Fox"
                              |movies.genre$Studio=="Paramount Pictures"
                              |movies.genre$Studio=="Universal"
                              |movies.genre$Studio=="WB",]
str(movies.subset)

#plot graph
p <- ggplot(data=movies.subset, aes(x=Genre, y=USGrossPercent))

q <- p + geom_jitter(aes(color=Studio, size=BudgetMillions)) +
  geom_boxplot(alpha=0.7, outlier.colour=NA) + ylab("Gross % US") +
  xlab("Genre") + ggtitle("Domestic Gross % by Genre") +
  theme(axis.title.x=element_text(size=20, color="Blue"),
        axis.title.y=element_text(size=20, color="Blue"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=15),
        plot.title=element_text(size=20, color="Black", hjust=0.5))

#Final touch
q$labels$size <- "Budget $M"

#Final product
q

