temperature <- c("111", "150", "123")


library("magrittr")

out <- as.numeric(temperature)      # 1. Konvertierung in Numeric
out <- subtract(out, 32)            # 2. Subtraktion von 32
out <- multiply_by(out, 5/9)        # 3. Multiplikation mit 5/9
out <- mean(out)                    # 4. Berechnung des Mittelwertes

out

out <- mean(multiply_by(subtract(as.numeric(temperature), 32), 5/9))

temperature |>                  
  as.numeric() |>               # 1. Konvertierung in Numeric
  subtract(32) |>               # 2. Subtraktion von 32
  multiply_by(5/9) |>           # 3. Multiplikation mit 5/9
  mean()                        # 4. Berechnung des Mittelwertes

studierende <- data.frame(
  Matrikel_Nr = c(100002, 100003, 200003),
  Studi = c("Patrick", "Manuela", "Eva"),
  PLZ = c(8006, 8001, 8820)
)

studierende

ortschaften <- data.frame(
  PLZ = c(8003, 8006, 8810, 8820),
  Ortsname = c("Zürich", "Zürich", "Horgen", "Wädenswil")
)

ortschaften

# Load library
library("dplyr")

inner_join(studierende, ortschaften, by = "PLZ")

left_join(studierende, ortschaften, by = "PLZ")

right_join(studierende, ortschaften, by = "PLZ")

full_join(studierende, ortschaften, by = "PLZ")

studierende <- data.frame(
  Matrikel_Nr = c(100002, 100003, 200003),
  Studi = c("Patrick", "Manuela", "Pascal"),
  Wohnort = c(8006, 8001, 8006)
)

left_join(studierende, ortschaften, by = c("Wohnort" = "PLZ"))
