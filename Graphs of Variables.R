library(ggplot2)
library(plotly)
library(gridExtra)

notlogged.panel$Country = factor(notlogged.panel$Country)
onecountry <- filter(notlogged.panel,
                     Country == "Austria")
onecountry$ass <- onecountry$ass/1000
labsize = 10
gdpplot <- ggplot(notlogged.panel, aes(y = `gdp`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = NULL, y = "Real GDP") +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme_bw() +
  theme(axis.title.y = element_text(size = labsize)) +
  ggtitle("Real GDP by Country over Time") +
  theme(plot.title = element_text(size = 11, face = "bold"))

houplot <- ggplot(notlogged.panel, aes(y = `hou`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = NULL, y = "Real House Prices") +
  guides(color = FALSE) +
  theme_bw() +
  theme(axis.title.y = element_text(size = labsize))+
  ggtitle("Real House Prices by Country over Time") +
  theme(plot.title = element_text(size = 11, face = "bold"))
  
defplot <- ggplot(notlogged.panel, aes(y = `def`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = NULL, y = "GDP Deflator") +
  guides(color = FALSE) +
  theme_bw() +
  theme(axis.title.y = element_text(size = labsize)) +
  ggtitle("Real GDP Deflator by Country over Time") +
  theme(plot.title = element_text(size = 11, face = "bold"))

#  ggtitle("Real GDP over Time") +

resplot <- ggplot(notlogged.panel, aes(y = `res`, x = `yqtr`, color = Country, text = paste("country:", Country))) +
  geom_line(size = 1.2) +
  labs(x = NULL, y = "Real Residential Invesment (log with base 10)") +
  guides(color = FALSE) +
  theme_bw() +
  theme(axis.title.y = element_text(size = labsize)) +
  ggtitle("Real Residential Invesment by Country over Time") +
  theme(plot.title = element_text(size = 11, face = "bold")) +
  scale_y_log10()

resplot
#  ggtitle("Real GDP over Time") +

intplot <- ggplot(onecountry, aes(y = `int`, x = `yqtr`), color = "grey") +
  geom_line(size = 1) +
  labs(x = "Year", y = "Shadow Policy Rate (%)") +
  theme_bw() +
  theme(axis.title.y = element_text(size = labsize)) +
  ggtitle("Shadow Policy Rate over Time") +
  theme(plot.title = element_text(size = 11, face = "bold"))



#volplot <- ggplot(onecountry, aes(y = `vol`, x = `yqtr`)) +
#  geom_line(size = 1) +
#  labs(x = "Year", y = "VSTOXX Index (%)") +
#  theme_bw()

assplot <- ggplot(onecountry, aes(y = `ass`, x = `yqtr`)) +
  geom_area() +
  labs(x = "Year", y = "ECB Total Assets (Billions €)") +
  theme_bw() +
  theme(axis.title.y = element_text(size = labsize)) +
  ggtitle("ECB Total Assets over Time") +
  theme(plot.title = element_text(size = 11, face = "bold"))


#grid.arrange(gdpplot, houplot, defplot, resplot, intplot, assplot, nrow = 3)

png("~/Thesis/Figures and Graphs/variablesgraph.png", width = 750, height = 900)
grid_arrange_shared_legend(gdpplot, houplot, 
                         defplot, resplot, intplot, assplot, nrow = 3, ncol = 2,
                          position = "bottom")
dev.off()

# facet_wrap( ~ group) if i add groups so theyre on their own graphs