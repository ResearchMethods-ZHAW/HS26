x <- 10.3

x

typeof(x)

y = 7.3
y

z <- 42
typeof(z)
is.integer(z)
is.numeric(z)
is.double(z)

a <- as.integer(z)
is.numeric(a)
is.integer(a)

# Das L steht für Long Integer
c <- 8L
is.integer(c)

3 > 6


sonnig <- TRUE
trocken <- FALSE

# ist es sonnig und trocken?
sonnig & trocken

s <- as.character(3.14)
s
typeof(s)

fname <- "Andrea"
lname <- "Muster"
paste(fname, lname)

fname2 <- "Simon"
fname == fname2

wochentage <- c("Montag", "Dienstag", "Mittwoch")

typeof(wochentage)

wochentage_fac <- as.factor(wochentage)

wochentage
wochentage_fac

factor(wochentage, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"), ordered = TRUE)

datum <- "2017-10-01 13:45:10"

# konvertiert character in POSIXct:
as.POSIXct(datum)

datum <- "01.10.2017 13:45"

# konvertiert character in POSIXct:
as.POSIXct(datum, format = "%d.%m.%Y %H:%M")

datum2 <- as.POSIXct(datum, format = "%d.%m.%Y %H:%M")

strftime(datum2, format = "%m") # extrahiert den Monat als Zahl
strftime(datum2, format = "%b") # extrahiert den Monat mit Namen (abgekürzt)
strftime(datum2, format = "%B") # extrahiert den Monat mit Namen (ausgeschrieben)

library("lubridate")

# extrahiert den Monat als Zahl
month(datum2) 
# extrahiert den Monat mit Namen (abgekürzt)
month(datum2, label = TRUE, abbr = TRUE) 
# extrahiert den Monat mit Namen (ausgeschrieben)
month(datum2, label = TRUE, abbr = FALSE) 

vec <- c(10, 20, 33, 42, 54, 66, 77)
vec

# Mit eckigen Klammern können einzelne oder mehrere Werte extrahiert werden
vec[5]
vec[2:4]

mylist <- list("q", TRUE, 3.14)


mylist2 <- list(fav_letter = "q", fav_boolean = TRUE, fav_number = 3.14)

mylist2


as.data.frame(mylist2)

df <- data.frame(
  Stadt = c("Zürich", "Genf", "Basel", "Bern", "Lausanne"),
  Ankunft = c(
    "1.1.2017 10:00", "1.1.2017 14:00",
    "1.1.2017 13:00", "1.1.2017 18:00", "1.1.2017 21:00"
  )
)

str(df)

df$Stadt

df$Einwohner <- c(400000, 200000, 175000, 14000, 130000)

df$Einwohner <- as.integer(df$Einwohner)


df$Ankunft <- as.POSIXct(df$Ankunft, format = "%d.%m.%Y %H:%M")

df$Ankunft

df$Ankunft_stunde <- hour(df$Ankunft)

df$Ankunft_stunde

df$Groesse[df$Einwohner > 300000] <- "gross"
df$Groesse[df$Einwohner <= 300000 & df$Einwohner > 150000] <- "mittel"
df$Groesse[df$Einwohner <= 150000] <- "klein"

df$Groesse
