library(readr)

#zd1.A
data <- read_delim("~/MN_PROJEKT/ROLN_3179.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
dane1 = data.frame(
  "Polska" = data %>%
    filter(Nazwa == "POLSKA") %>%
    summarise(Polska = `gospodarstwa ogółem;gospodarstwa;2006;[-]`),
  "Łącznie" = data %>%
  filter(!Nazwa == "POLSKA") %>%
  summarise(Wojewodztwa = sum(`gospodarstwa ogółem;gospodarstwa;2006;[-]`)))

lata <- 2006:2018

# Tworzymy pusty wektor na wyniki porównań
wyniki <- logical(length(lata))

# Pętla po latach
for (i in seq_along(lata)) {
  rok <- lata[i]
  
  kolumna <- paste0("gospodarstwa ogółem;gospodarstwa;", rok, ";[-]")
  
  polska <- data %>%
    filter(Nazwa == "POLSKA") %>%
    summarise(Polska = sum(.data[[kolumna]])) %>%
    pull(Polska)
  
  wojewodztwa <- data %>%
    filter(Nazwa != "POLSKA") %>%
    summarise(Wojewodztwa = sum(.data[[kolumna]])) %>%
    pull(Wojewodztwa)
  
  wyniki[i] <- polska == wojewodztwa
}

# Tworzymy tabelę z wynikami
wyniki_df <- data.frame(
  Rok = lata,
  wojewodztwa = wojewodztwa,
  polska = polska,
  Poprawna_suma = wyniki
)



dane1$Wojewodztwa == dane1$Polska

#zad1.B
dane2 <- data[, -1]
indeks_polska <- which(dane2$Nazwa == "POLSKA")
wojewodztwa <- dane2[-indeks_polska, ]
suma_wojewodztw <- colSums(wojewodztwa[, -1])
polska <- dane2[indeks_polska, -1]
porownanie <- suma_wojewodztw == as.numeric(polska)

write.csv(porownanie, file = "pol.csv", row.names = FALSE)


#zad2. 
gospodarstwa <- data[, 3:14]  # liczba gospodarstw
powierzchnia <- data[, 15:26]  # powierzchnia użytków rolnych
srednia_powierzchnia <- powierzchnia / gospodarstwa
wynik <- cbind(Nazwa = data$Nazwa, srednia_powierzchnia)

