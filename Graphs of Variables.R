library(ggplot2)
library(plotly)
library(gridExtra)

notlogged.panel$Country = factor(notlogged.panel$Country)
onecountry <- filter(notlogged.panel,
                     Country == "Austria")
onecountry$ass <- onecountry$ass/1000

gdpplot <- ggplot(notlogged.panel, aes(y = `gdp`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Real GDP") +
  guides(color = FALSE) +
  theme_bw() 
#  ggtitle("Real GDP over Time") +

houplot <- ggplot(notlogged.panel, aes(y = `hou`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Real House Prices") +
  guides(color = FALSE) +
  theme_bw() 
#  ggtitle("Real GDP over Time") +
  
defplot <- ggplot(notlogged.panel, aes(y = `def`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "GDP Deflator") +
  guides(color = FALSE) +
  theme_bw() 
#  ggtitle("Real GDP over Time") +

resplot <- ggplot(notlogged.panel, aes(y = `res`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Real Residential Investment") +
  theme_bw() 
#  ggtitle("Real GDP over Time") +

intplot <- ggplot(onecountry, aes(y = `int`, x = `yqtr`)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Shadow Policy Rate (%)") +
  theme_bw()

#volplot <- ggplot(onecountry, aes(y = `vol`, x = `yqtr`)) +
#  geom_line(size = 1) +
#  labs(x = "Year", y = "VSTOXX Index (%)") +
#  theme_bw()

assplot <- ggplot(onecountry, aes(y = `ass`, x = `yqtr`)) +
  geom_area() +
  labs(x = "Year", y = "ECB Total Assets (Billions €)") +
  theme_bw()

resplot 

png("~/Thesis/Figures and Graphs/variablesgraph.png")

grid.arrange(gdpplot, houplot, defplot, resplot, intplot, assplot, nrow = 3)
dev.off()
# facet_wrap( ~ group) if i add groups so theyre on their own graphs

ggplotly(gdpplot)

