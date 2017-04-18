library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

# read in dataframes
pop <- read.csv('pop.csv', header = TRUE)
le <- read.csv('life_exp.csv', header=TRUE)
fert <- read.csv('fert_rate.csv', header=TRUE)

# move from wide to long
pop <- gather(pop, year, pop, X1960:X2014)[,c("Country.Name","year","pop","Country.Code")]
le <- gather(le, year, life_exp, X1960:X2014)[,c("Country.Name","year","life_exp","Country.Code")]
fert <- gather(fert, year, fert, X1960:X2014)[,c("Country.Name","year","fert","Country.Code")]

# join dfs, get rid of leading Xs in year
plf <- inner_join(pop, inner_join(le, fert)) 
plf$year <- substr(plf$year, 2, nchar(plf$year))

meta <- read.csv('meta.csv', header=TRUE)
plf <- inner_join(plf, meta[,c("Country.Code","Region")]) # join regions
plf <- plf[!(plf$Region==""),] # drop blank regions
plf <- rename(plf, Fertility.Rate = fert, Life.Expectancy=life_exp, Population=pop) # rename cols

ui <- fluidPage(
    mainPanel(
        plotlyOutput("scatter"), 
        sliderInput("year", "Year", min=1960, max=2014,
                    step=1, value = 1960, , width='700px',
                    animate = animationOptions(interval=333), sep=""),
        sliderInput("pop","Population", min=1, max=30, step=1, value=15)
    )
)

server <- function(input, output){
    output$scatter <- renderPlotly({
        subset_plot <- plf[plf$year==input$year,]
        plt <- ggplot(subset_plot, aes(x=Life.Expectancy, y=Fertility.Rate, key=Country.Name, size=Population)) 
        plt <- plt + geom_point(aes(fill=Region), pch=21, alpha=.7)
        plt <- plt + scale_size(range = c(1, input$pop)*2, guide = 'none')
        plt <- plt + scale_fill_manual(values = c("royalblue","orangered",'orange','forestgreen',
                                       'darkmagenta','lightseagreen','indianred'))
        plt <- plt + xlim(10, 90) + ylim(0, 9)
        plt <- plt + ggtitle("Life Expectancy and Fertility Over Time") + xlab("Life Expectancy") + ylab("Fertility")
        plt <- plt + theme(panel.border = element_blank(),axis.line = element_line(colour = "black"))
    })
}

shinyApp(ui = ui, server = server)
