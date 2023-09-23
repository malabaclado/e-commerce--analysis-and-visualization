library(tidyverse)
library(ggplot2)
library(ggthemes)

# Importing data
abc_electronics <- read_csv("ABC_Electronics.csv")

summary(abc_electronics)

# Aggregations

# filtering
filter(abc_electronics, Customer_Age >30)

# selecting
new_table <- select(abc_electronics, Product_Name, Category) 
filter(new_table, Category=="Dell")

#piping
table_2 <- abc_electronics %>%  select(Product_Name, Category) %>% 
  filter(Category=="Dell")


# Visualizations


## Sales Trends Over Time (Line Plot)

# Aggregate date, orders, quantity and total sales
sales <-abc_electronics %>% 
  arrange(Order_Date) %>%
  group_by(Order_Date) %>%
  summarize(
    num_of_orders=n(),
    num_of_quantity=sum(Quantity),
    total_sales=sum(Quantity*Price)
  )

# Plot
ggplot(sales, aes(x=Order_Date, y=total_sales)) +
  geom_line(color="blue", linewidth=1) + 
  labs(
    title="Sales Trends Over Time",
    x="Order Date",
    y="Total Sales"
  ) +
  theme_classic() +
  theme (plot.title=element_text(face = "bold"))

#-------------------------------------------------------------------------

salesbyproduct <- abc_electronics %>%
  arrange(Order_Date) %>%
  group_by(Order_Date, Product_Name) %>%
  summarize(
    num_of_orders=n(),
    num_of_quantity=sum(Quantity),
    total_sales=sum(Quantity*Price)
  )

ggplot(salesbyproduct, aes(x=Order_Date, y=total_sales)) +
  geom_line(linewidth=1, aes(color=Product_Name)) +
  facet_wrap(~Product_Name)+
  theme_classic() +
  theme (plot.title=element_text(face = "bold"))

#-------------------------------------------------------------------------

salesbycompany <- abc_electronics %>%
  arrange(Order_Date) %>%
  group_by(Order_Date, Category) %>%
  summarize(
    num_of_orders=n(),
    num_of_quantity=sum(Quantity),
    total_sales=sum(Quantity*Price), .group='drop'
  )

ggplot(salesbycompany, aes(x=Order_Date, y=total_sales)) +
  geom_line(linewidth=1, aes(color=Category)) +
  facet_wrap(~Category)+
  theme_classic() +
  theme (plot.title=element_text(face = "bold"))


## Top-performing Products (Bar Plot)

top_performing <- abc_electronics %>% 
  group_by(Product_Name, Category) %>%
  summarize(
    num_of_orders=n(),
    num_of_quantity=sum(Quantity),
    total_sales=sum(Quantity*Price), .groups="drop"
  )

head(top_performing)

# ggplot(top_performing, aes(x=Product_Name, y=total_sales)) + geom_bar(stat="identity")\

ggplot(abc_electronics, aes(x=Product_Name, fill=Category)) + geom_bar() +
  labs(
    title="Top Performing Products",
    subtitle="By Number of Orders and Category"
  )


## Customer Age Distribution (Histogram)

#Graph 1
ggplot(abc_electronics, aes(x=Customer_Age,fill=Customer_Gender)) + 
  geom_histogram(
    #fill="red",
    color="black",
    binwidth=10
    ) + 
  labs(
    title="Customer Age Distribution"
  ) + theme_bw()

#Graph 2
ggplot(abc_electronics, aes(x=Customer_Age,fill=Customer_Gender)) + 
  geom_histogram(
    #fill="red",
    color="black",
    binwidth=10
  ) + 
  facet_wrap(~Customer_Country) +
  labs(
    title="Customer Age Distribution"
  ) + theme_bw()

## Customer Gender Distribution (Pie Chart)

gender_dist <- abc_electronics %>%
  group_by(Customer_Gender) %>%
  summarise(count=n(),
            percentage=n()/nrow(abc_electronics))


  
ggplot(gender_dist, aes(x="", y=count, fill=Customer_Gender)) + 
  geom_col(color="black")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(percentage*100), "%")), 
          position = position_stack(vjust=0.5))+
  theme(panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
      ) +
  labs(
    title="Customer Gender Distribution",
    #subtitle="By Number of Orders and Category"
  )
  
## Geographical Sales Distribution (World Map)



library("sf")
library("rnaturalearth")
library("countrycode")
library("ggrepel")

world <- ne_countries(scale="small", returnclass="sf")


# Change map projection
world %>% 
  st_transform(crs="+proj=wintri") %>%
  ggplot() + 
  geom_sf() +
  coord_sf(datum=NA) +
  theme_minimal()

salesbycountry <- abc_electronics %>% 
  group_by(Customer_Country) %>%
  summarize(
    num_of_orders=n(),
    num_of_quantity=sum(Quantity),
    total_sales=sum(Quantity*Price)
  )

salesbycountry_with_iso <- salesbycountry %>% 
  mutate(Iso3 = countrycode::countrycode(
    sourcevar = Customer_Country,
    origin="country.name",
    destination="iso3c"
  ))

data <- world%>% 
  select(geometry, name, iso_a3) %>%
  right_join(salesbycountry_with_iso, by=c("iso_a3"="Iso3"))

world%>%
  filter(admin != "Antartica") %>%
  st_transform(crs="+proj=robin") %>%
  ggplot() + 
  geom_sf(color="darkgrey")+
  geom_sf(data=data, aes(fill=num_of_orders)) + 
  #coord_sf(datum=NA) +
  scale_fill_continuous()+
  theme_minimal()+
  theme(
    plot.title=element_text(face = "bold"),
    axis.text.x=element_blank(),
    #legend.position="none"
    ) +
  ggrepel::geom_label_repel(
    data=data,
    aes(label=Customer_Country, geometry=geometry),
    stat="sf_coordinates",
    min.segment.length=0
  )+
  labs(
    title="Geographical Sales Distribution",
    subtitle="Number of orders per country",
    caption="",
    x=NULL,
    y=NULL,
  )
