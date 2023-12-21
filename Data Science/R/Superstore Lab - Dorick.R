library(tidyverse)
install.packages
library(janitor)
library(readxl)
store <- read_excel("C:/Users/jdorick/Downloads/Superstore.xlsx") 
store |>
  janitor::clean_names()
store <- store |> 
  janitor::clean_names()
store <- store |> 
  select(category, department, customer_name, discount, order_date, order_priority, order_quantity, profit, region, sales, ship_mode, shipping_cost, state)
store <- store |>
  mutate(expenses = (sales - profit), .after = profit)
store |> 
  mutate(large_discount = case_when(discount > 0.10~'Yes', discount <= 0.1~'No'))
store <- store |> 
  mutate(large_discount = case_when(discount > 0.10~'Yes', discount <= 0.1~'No'))
store <- store |> 
  mutate(number_of_words_in_name = (strsplit(customer_name, " +")))
store_names <- store |> 
  distinct(customer_name, number_of_words_in_name) 
middle_names <- store_names |> 
  filter(lengths(number_of_words_in_name == 3))
count(middle_names)
store <- store |> 
  filter(lengths(number_of_words_in_name) != 3)
store <- store |> 
  relocate(category, department, customer_name, order_priority,
           region, ship_mode, state, large_discount, order_date,
           discount, order_quantity, profit, expenses, sales, shipping_cost,
           number_of_words_in_name)
east <- store |>
  filter(region == 'East') |> group_by(state) |>
  summarize(sum(sales))
depart_avg <- store |>
  group_by(state, department) |>
  summarize(mean(profit), .groups = "keep")
months <- store |> 
  group_by(month(order_date)) |> 
  summarize(total_order_quantity = sum(order_quantity))|>
  arrange(desc(total_order_quantity))
discounts <- store |>
  group_by(department) |> 
  summarize(
    average_discount = mean(discount),
    min_discount = min(discount),
    max_discount = max(discount)
  )

space <- ", "
profits_m <- store |>
  mutate(year_month = 
           paste(year(order_date), space, month(order_date), sep = ""),
         year = year(order_date),
         month = month(order_date)
         ) |>
  group_by(department, year_month) |>
  summarize(total_profit = sum(profit),
            year,
            month,
            .groups = 'keep'
            ) |>
  arrange(year, month) |>
  distinct()

ggplot(
  profits_m,
  mapping = aes(x = fct_inorder(year_month), y = total_profit, color = department, 
                group = department)
) + geom_line() +
  geom_point() + labs(
    title = 'Total Profit Over Time by Department', x = 'Year and Month', y = 'Total Profit') +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

profits_m_2 <- store |>
  group_by(department, month(order_date), order_priority) |>
  summarize(
    month = factor(month(order_date)),
  )

ggplot(
  profits_m_2,
  aes(x = month, fill = order_priority)) + 
  geom_bar() + labs(title = 'Count of Order Priority by Month' , x = 'Month', y = 'Count')

ggplot(
  profits_m_2,
  aes(x = department, fill = order_priority)) + 
  geom_bar() + labs(title = 'Count of Order Priority by Department' , x = 'Department', y = 'Count')

reg = lm(discount~sales, data = store)
summary(reg)