
# Ushtrimi 6.1.2

# a) 
# Aktivizojmë paketën "markovchain"
library(markovchain)

# Ndërtojmë matricën e probabiliteteve të kalimit
P <- matrix(c(2/5, 3/5, 0, 1/5, 1/2, 3/10, 0, 2/5, 3/5), byrow = TRUE, nrow = 3)

# Përcaktojmë gjendjet e zinxhirit me emra
emri_gjendje <- c("Gjendja 1", "Gjendja 2", "Gjendja 3")

# Ndërtojmë një objekt zinxhir markovi (ZM)
zinxhir_markovi <- new("markovchain", states = emri_gjendje, byrow = TRUE, transitionMatrix = P)

# I japim një emër zinxhirit të markovit që ndërtuam si objekt 
attr(zinxhir_markovi, "name") <- "Zinxhir Markovi U.6.1.2"

zinxhir_markovi # Do të na japë informacion mbi zinxhirin Markovian që ndërtuam


# b) Vizatoni grafin e kalimeve;

## Mënyra 1 ("igraph")
# Instaloni paketën 'igraph' nëse nuk është e instaluar. 
# Mund të përdorni të njëjtin funksion për paketat e shfrytëzuara më poshtë
if (!requireNamespace("igraph", quietly = TRUE)) {
  install.packages("igraph")
}
library(igraph)

# Ndërtojmë matricën e probabiliteteve të kalimit
P <- matrix(c(2/5, 3/5, 0, 1/5, 1/2, 3/10, 0, 2/5, 3/5), byrow = TRUE, nrow = 3)
P # e thërrasim matricën për t'u siguruar që e kemi ndërtuar saktë

# Përcaktojmë gjendjet e zinxhirit duke përdorur numra
gjendje <- 1:3

# Ndërtojmë një graf bazuar në matricën e probabiliteteve të kalimit
markov_graph <- graph.adjacency(P, mode = "directed", weighted = TRUE, diag = TRUE)

# Vendosim etiketat e kulmeve të grafit që të përputhen me numrat e gjendjeve
V(markov_graph)$label <- gjendje

# Përcaktojmë etiketat harqeve të probabiliteve të kalimit
E(markov_graph)$label <- round(E(markov_graph)$weight, 2)

# Vendosimi ngjyrat e harqeve skajeve për t'i dalluar ato
edge_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink", "gray", "cyan")

# Vizualizojmë grafin e probabiliteteve të kalimit
plot(
  markov_graph,
  layout = layout.circle,
  vertex.size = 30,
  vertex.label.cex = 1.5,
  edge.label.cex = 1.2,
  edge.label.color = "black",
  edge.arrow.size = 0.5,
  edge.color = edge_colors,
  edge.curved = 0.2  # Adjust the curvature as needed
)

## Mënyra 2 ("visNetwork")

# Aktivizojmë librarinë 'visNetwork' të cilën e kemi instaluar më parë

library(visNetwork)

# Ndërtojmë matricën e probabiliteteve të kalimit
P <- matrix(c(2/5, 3/5, 0, 1/5, 1/2, 3/10, 0, 2/5, 3/5), byrow = TRUE, nrow = 3)
P # e thërrasim matricën për t'u siguruar që e kemi ndërtuar saktë

# Përcaktojmë gjendjet e zinxhirit duke përdorur emra dhe numra (sipas nevojës për një vizualizim sa më të qartë)
emri_gjendje <- c("Gjendja 1", "Gjendja 2", "Gjendja 3")
numri_gjendje <- 1:3

# Ndërtojmë një graf bazuar në matricën e probabiliteteve të kalimit
markov_graph <- graph.adjacency(P, mode = "directed", weighted = TRUE, diag = TRUE)  # diag=TRUE përfshin probabilitetet në diagonalen kryesore

# Përcaktojmë etiketat e gjendjeve dhe probabilitetet e kalimit. Mund të rrumbullakosim probabilitetet duke përdorur funksionin round()
E(markov_graph)$label <- paste("P =", round(E(markov_graph)$weight, 2))

# Krijojmë një objekt visNetwork 
vis_markov_graph <- visNetwork(
  nodes = data.frame(id = numri_gjendje, label = emri_gjendje),
  edges = get.data.frame(markov_graph),
  width = "100%",
  height = "500px",
  main = "Grafi i probabiliteteve të kalimit"
)

# Vendosim shigjeta të lakuara për një paraqitje grafike më të qartë
vis_markov_graph <- visEdges(vis_markov_graph, smooth = list(enabled = TRUE, type = "curvedCW"))

# Vizualizojmë grafin e probabiliteteve të kalimit
vis_markov_graph


# d) A është i reduktueshëm? Trego një klasë të pareduktueshme, nëse ka një të tillë;

library(markovchain)
# Shfrytëzojmë objektin që kemi krijuar në kërkesën (a) "zinxhir_markovi"

# Kontrollojmë nëse zinxhiri është i pareduktueshëm (rezultati është TRUE/FALSE) 
irreducible <- is.irreducible(zinxhir_markovi)

# Afishojmë rezultatin të shoqëruar me një tekst
if (irreducible) {
  cat("Zinxhiri i Markovit është i pareduktueshëm.\n")
} else {
  cat("Zinxhiri i Markovit është i reduktueshëm.\n")
}

# e) Trego natyrën e gjendjeve të zinxhirit: kalimtare, rekurrente (rekurrente pozitive, rekurrente zero), thithëse;

## Mënyra 1 (funksion)
# Një funksion i cili mund të na ndihmojë të klasifikojmë gjendjet
klasifiko_gjendjet <- function(matrica_probabiliteteve) {
  n <- nrow(matrica_probabiliteteve)
  gjendjet <- 1:n
  klasifikimi <- character(n)
  
  for (i in gjendjet) {
    if (all(matrica_probabiliteteve[i,] == 0)) {
      klasifikimi[i] <- "Thithëse"
    } else if (all(matrica_probabiliteteve[i,] == 0) && matrica_probabiliteteve[i, i] == 1) {
      klasifikimi[i] <- "Kalimtare"
    } else {
      klasifikimi[i] <- "Rekurrente"
      if (matrica_probabiliteteve[i, i] > 0) {
        klasifikimi[i] <- "Rekurrente Pozitive"
      } else {
        klasifikimi[i] <- "Rekurrente Zero"
      }
    }
  }
  
  return(data.frame(Gjendja = gjendjet, Klasifikimi = klasifikimi))
}

# Aplikojmë funksionin e ndërtuar në matricën e probabiliteteve të kalimit P
rezultati_klasifikimit <- klasifiko_gjendjet(P)

# Afisho rezultatin
rezultati_klasifikimit

## Mënyra 2 "markovchain"
# Mund të përdorim edhe funksione të gatshme të paketës "markovchain" si më poshtë:

recurrentClasses(zinxhir_markovi)
transientClasses(zinxhir_markovi)
absorbingStates(zinxhir_markovi)

# g) Njehso:

# Vektori i probabiliteteve fillestare f_0
f_0 <- c(1/3, 1/3,1/3)  

# i) Njehsojmë: P(X1 = 1 | X0 = 2)
prob_i <- P[2, 1]  # Probabiliteti i kalimit nga gjendja 2 në gjendjen 1 me një hap

# Afisho rezultatin
cat("P(X1 = 1 | X0 = 2) =", prob_i, "\n")

# ii) Njehsojmë: P(X4 = 3 | X3 = 1)
prob_ii <- P[1, 3]  # Probabiliteti i kalimit nga gjendja 1 në gjendjen 3 me një hap

# Afisho rezultatin
cat("P(X4 = 3 | X3 = 1) =", prob_ii, "\n")


# iii) Njehsojmë: P(X2 = 2 | X0 = 2) 
P_2 <-  P %*% P # Probabiliteti i kalimit nga gjendja 2 në gjendjen 2 me dy hapa
prob_iii<-P_2[2,2]
# Afisho rezultatin
cat("P(X2 = 2 | X0 = 2) =", prob_iii, "\n")

# iv) Njehsojmë: P(X3 = 2, X2 = 1, X1 = 3 | X0 = 2)
prob_iv <- P[2, 3] * P[3, 1] * P[1, 2]

# Afisho rezultatin
cat("P(X3 = 2, X2 = 1, X1 = 3 | X0 = 2) =", prob_iv, "\n")


# v) P(X3 = 2, X2 = 1, X1 = 3)

# Vektorii probabiliteteve fillestare f_0
f_0 <- c(1/3, 1/3, 1/3)

# Njehsojmë probabilitetin P(X3 = 2, X2 = 1, X1 = 3)
prob_v <- sum(f_0 * P[, 3] * P[3, 1] * P[1, 2])

# Afisho rezultatin
cat("P(X3 = 2, X2 = 1, X1 = 3) =", prob_v, "\n")



# h) Njehsoni P(X6 = 3, X4 = 1, X3 = 2, X0 = 2)

library(Matrix)
library(expm)

# Njehsojmë probabilitetin P(X6 = 3, X4 = 1, X3 = 2, X0 = 2)
prob_h <- f_0[2] %*% (P %^% 3)[2, 2] %*% P[2, 1] * (P %^% 2)[1, 3] 

# Afisho rezultatin
cat("P(X6 = 3, X4 = 1, X3 = 2, X0 = 2) =", prob_h, "\n")

# i)  Gjej probabilitetin e rikthimit për herë të parë në gjendjen 1:
#  i) me 1 hap; ii) me 2 hapa; iii) me 3 hapa; iv) me 4 hapa.


# Njehso probabilitetin e rikthimit për herë të parë në gjendjen 1 me 1, 2, 3, dhe 4 hapa
prob_rikthimit_1_hap <- P[1, 1]
prob_rikthimit_2_hapa <- P[1, 2] * P[2, 1] 
prob_rikthimit_3_hapa <- P[1, 2] * P[2, 2] * P[2, 1] 
prob_rikthimit_4_hapa <- P[1, 2] * P[2, 3] * P[3, 2] * P[2, 1] + P[1, 2] * P[2, 2] * P[2, 2] * P[2, 1]

# Afisho rezultatin
cat("i) Probabiliteti i rikthimit në gjendjen 1 me 1 hap =", prob_rikthimit_1_hap, "\n")
cat("ii) Probabiliteti i rikthimit në gjendjen 1 me 2 hapa =", prob_rikthimit_2_hapa, "\n")
cat("iii) Probabiliteti i rikthimit në gjendjen 1 me 3 hapa =", prob_rikthimit_3_hapa, "\n")
cat("iv) Probabiliteti i rikthimit në gjendjen 1 me 4 hapa =", prob_rikthimit_4_hapa, "\n")

# j) Gjej shpërndarjen stacionare për zinxhirin e pareduktueshëm.

## Mënyra 1 ("markovchain")
library(markovchain)
steadyStates(zinxhir_markovi)


## Mënyra 2 (vektor vetjak)
# Përdorim matricën P dhe zgjidhim sistemin duke shfrytëzuar vlerat vetjake 
shperndarja_stacionare <- eigen(t(P))$vectors[, 1]  

# Shprehim si probabilitet shpërndarjen stacionare
shperndarja_stacionare <- shperndarja_stacionare / sum(shperndarja_stacionare)

# Afishojmë rezultatin
cat("Shpërndarja stacionare (duke përdorur matricën P):", shperndarja_stacionare, "\n")


## Mënyra 3 (simulim)
# Kujtojmë:
P <- matrix(c(2/5, 3/5, 0, 1/5, 1/2, 3/10, 0, 2/5, 3/5), byrow = TRUE, nrow = 3)
f_0 <- c(1/3, 1/3, 1/3)

# Numri i iteracioneve për simulimin
num_it <- 10000

# Krijojmë një vektor që do të ruajë shpërndarjen
shperndarja_gjendjeve <- rep(0, nrow(P))

# Simulojmë një zinxhir markovi për të vlerësuar shpërndarjen stacionare
for (i in 1:num_it) {
  f_0 <- f_0 %*% P
  shperndarja_gjendjeve <- shperndarja_gjendjeve + f_0
}

# Shprehim si probabilitet shpërndarjen stacionare
shperndarja_stacionare_sim <- shperndarja_gjendjeve / num_it

# Afishojmë rezultatin
cat("Shpërndarja stacionare (duke përdour f_0 dhe simulimin):", shperndarja_stacionare_sim, "\n")
# Mund të rrumbullakosni rezultatin duke përdorur funksionin round()

# Konvergjenca e shpërndarjes stacionare mund të studiohet duke shfrytëzuar grafikët e mëposhtëm:
  
# Specifikoni numrin e hapave
nHapa <- 50

# Specifikoni shp♪7rndarjen fillestare 
f_0 <- c(1/3, 1/3, 1/3)

# Ndërtojmë një matricë që të ruajmë probabilitetet
probs <- matrix(NA, nrow = nHapa, ncol = ncol(P))

# Njehsojmë probabilitetet
probs[1, ] <- f_0
for (n in 2:nHapa) {
  probs[n, ] <- probs[n - 1, ] %*% P
}

# Vizualizojmë grafikisht probabilitetet vs. hapave (sipas gjendjeve)
matplot(probs, type = "l", lty = 1, lwd = 2, col = 1:ncol(P), ylim = c(-0.05, 0.6), xlab = "Hapat", ylab = "Probabiliteti", panel.first = grid())

# Shtojmë legjendën në grafik për të qartësuar konvergjencën sipas gjendjeve
legend("bottomright", c("Gjendje 1", "Gjendje 2", "Gjendje 3"), lty = 1, lwd = 2, col = 1:ncol(P),box.col = NA)

# Për të paraqitur probabilitetet përfundimtare
mesi_hapave <- ceiling(nHapa / 2)

# Llogarisim vlerat përfundimtare të probabiliteteve dhe rrumbullakosim
vlera_perfundimtare <- round(probs[mesi_hapave, ], 2)  

# Shtojmë tekstin në grafik për një paraqitje grafike më të qartë të rezultateve
text(mesi_hapave, vlera_perfundimtare + 0.02, labels = vlera_perfundimtare, pos = 3, offset = 0.2, col = 1:ncol(P))

# k) Gjej kohën mesatare të qëndrimit në secilën nga gjendjet 1, 2 dhe 3.

## Mënyra 1
# Duke shfrytëzuar idhjen midis shp♪7rndarjes stacionare dhe kohës mesatre të rrekurencës.

# Calculate the mean residence time for each state
koha_mes_rrekurences_gjendja1 <- 1 / vlera_perfundimtare[1]
koha_mes_rrekurences_gjendja2 <- 1 / vlera_perfundimtare[2]
koha_mes_rrekurences_gjendja3 <- 1 / vlera_perfundimtare[3]

# Print the results
cat("Koha mesatare e rrekurences ne gjendjen 1:", koha_mes_rrekurences_gjendja1, "\n")
cat("Koha mesatare e rrekurences ne gjendjen 2:", koha_mes_rrekurences_gjendja2, "\n")
cat("Koha mesatare e rrekurences ne gjendjen 3:", koha_mes_rrekurences_gjendja3, "\n")

## Mënyra 2 ("markovchain")
# kujtojmë emrin e objektit ZM të krijuar në fillim të ushtrimit
meanRecurrenceTime(zinxhir_markovi)
