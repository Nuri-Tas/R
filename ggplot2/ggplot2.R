library('ggplot2')

png(file = "/Users/Lenovo/Desktop/R/google/bill_depth_mm.png")

# create a smooth graph of bill depth vs bill length of the penguins. Display the different species by different color and show the different sexes in separate plots
ggplot(penguins) + 
  geom_smooth(mapping = aes(x=bill_depth_mm, y=bill_length_mm, method='loess', color = species)) +
  geom_jitter(mapping = aes(x=bill_depth_mm, y=bill_length_mm, color = species)) +
  facet_wrap(~sex)

# save the plot as a png file
dev.off()

# smooth plot
ggplot(penguins) + geom_smooth(mapping = aes(x=bill_length_mm, y=body_mass_g, fill=island)) +
  facet_wrap(~sex)

# diamonds dataset
View(diamonds)

# plot how many diamonds there are in each depth value per cut
ggplot(diamonds) + geom_bar(aes(depth, fill=cut))

# show the relation between carat and price per each cut
ggplot(diamonds) +
  geom_point(aes(x=carat, price, color=cut)) + 
  facet_wrap(~cut)

# display carat vs price for each color
ggplot(diamonds) + 
  geom_point(aes(x=carat, y=price, color=color)) +
  facet_grid(cut~color)

diamonds %>%
  group_by(cut) %>%
  filter(depth == 61 | depth == 62) %>%
  summarise(n = n())

# import hotel bookings dataset 
hotel_bookings <- read.csv("C:/Users/Lenovo/Downloads/hotel_bookings.csv")

# test the hypothesis that those who book early have children
ggplot(hotel_bookings) + geom_point(mapping = aes(x=lead_time, y = children))

# test the hypothesis that guests without children book the most weekend nights
ggplot(hotel_bookings) + 
  geom_point(mapping = aes(x=stays_in_weekend_nights, y = children)) 

# display the number of bookings grouped by customer type by each distribution channel 
ggplot(hotel_bookings) +
  geom_bar(aes(distribution_channel, fill=customer_type)) +
  facet_wrap(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) +
  geom_bar(aes(market_segment, fill=hotel)) +
  facet_wrap(market) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) +
    geom_bar(mapping = aes(x = hotel)) +
    facet_wrap(~market_segment)

onlineta_city_hotels <- filter(hotel_bookings, hotel == 'City Hotel' & market_segment=="Online TA")

ggplot(data = onlineta_city_hotels) +
  geom_point(mapping = aes(x = lead_time, y = children)) +
  labs(title = 'lead time vs children', subtitle = 'City Hotel and Online TA ',
       caption='the dataset is from Google Data Analytics Course')  +
 annotate('text', x=200, y=2.5, label='there is no relation between the lead time and children number')
