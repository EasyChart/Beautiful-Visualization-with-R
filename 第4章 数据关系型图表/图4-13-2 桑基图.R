#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts

library(ggalluvial)
library(ggplot2)

data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq,
           fill = response, label = response)) +
  geom_flow(alpha = 0.7,color = "darkgray") +
  geom_stratum(alpha = 1) +
  geom_text(stat = "stratum", size = 3.5) +
  theme_classic()+
  #coord_flip() +
  theme(legend.position = "none",
        axis.text.x =element_text(color="black",size=12),
        axis.title.x = element_blank(),
        axis.text.y =element_blank(),
        axis.line = element_blank(),
        axis.ticks =element_blank() )# +
#ggtitle("vaccination survey responses at three points in time")

#-------------------------------------------------------------------------
data(Refugees, package = "alluvial")
country_regions <- c(
  Afghanistan = "Middle East",
  Burundi = "Central Africa",
  `Congo DRC` = "Central Africa",
  Iraq = "Middle East",
  Myanmar = "Southeast Asia",
  Palestine = "Middle East",
  Somalia = "Horn of Africa",
  Sudan = "Central Africa",
  Syria = "Middle East",
  Vietnam = "Southeast Asia"
)
Refugees$region <- country_regions[Refugees$country]
ggplot(data = Refugees,
       aes(x = year, weight = refugees, alluvium = country)) +
  geom_alluvium(aes(fill = country, colour = country),
                alpha = .85, decreasing = FALSE) +
  scale_x_continuous(breaks = seq(2003, 2013, 2)) +
  #theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  scale_color_brewer(type = "qual", palette = "Paired") #+
  #theme_void()
 # facet_wrap(~ region, scales = "fixed") +
  #ggtitle("refugee volume by country and region of origin")


