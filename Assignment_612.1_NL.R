gapminder_data <- read.table(file="data/gapminder-FiveYearData.csv",sep=',',header=T)
library(ggplot2)
library(plyr)

life.lab <- expression(paste(Delta,'Life Expectancy'))
gdp.lab <- expression(paste(Delta,'per capita GDP'))
plot.title <- expression(paste('Relationship between ',Delta,'Life Expectancy and ',Delta,'per capita GDP from 1952-2007'))
levels(gapminder_data$country)

diff_GDP <- daply(.data=gapminder_data,
             .variables='country',
             .fun=function(x) max(x$gdpPercap)-min(x$gdpPercap))
diff_Life <- daply(.data=gapminder_data,
                .variables='country',
                .fun=function(x) max(x$lifeExp)-min(x$lifeExp))

ggplot(data=gapminder_data[gapminder_data$year==1952,], aes(x=diff_Life, y=diff_GDP)) +
  geom_point(aes(color=continent))+scale_y_log10()+xlab(life.lab)+
  ylab(gdp.lab)+geom_smooth(method='lm',size=1)+ ggtitle(plot.title)
  