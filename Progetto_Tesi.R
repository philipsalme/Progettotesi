author="Philip Salme"

library(tidyverse)
library(ggplot2)
library(softImpute)
library(MASS)
library(parallel)
library(data.table)

RMSE=function(reali, previsti){
  sqrt(mean((reali - previsti)^2))
}

## Dati MovieLens ######################################


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

movielens_data <- tibble(fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("UserID", "MovieID", "Rating", "timestamp")))

movie_names <- tibble(str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3))

movielens_data=movielens_data[,-4]
movie_cols=c("MovieID","Title","Genres")
colnames(movie_names)=movie_cols
movielens_data                                                                  ## 1'000'199 rating
movie_names                                                                     ## 3'883 film

movielens_data%>%                                                               ## Distribuzione dei voti
  mutate(Rating=as.factor(Rating))%>%
  ggplot(aes(x=Rating))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  labs(title="Distribuzione dei voti")+
  ylab("Frequenza Relativa")+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..)), stat= "count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent)


ml_sparse_matrix=movielens_data%>%
  spread(key="MovieID", value="Rating", fill=NA)
user_idx=ml_sparse_matrix[,1]
ml_sparse_matrix=ml_sparse_matrix[,-1]
image(as.matrix(ml_sparse_matrix), col=c("black", "blue", "green",              ## visualizzazione dei dati
                                         "red", "violet", "yellow"),
      xaxt="n", yaxt="n")

medie_film=colMeans(ml_sparse_matrix, na.rm = T)
which(medie_film==max(medie_film)) ## 2858, 2652
movie_names[colnames(ml_sparse_matrix[,which(medie_film==max(medie_film))]),2] ## 3072, 2858

dim(ml_sparse_matrix)


## film maggiormente recensiti
head(movielens_data %>%
  group_by(MovieID) %>%
  summarise(frequenza=n(), media_voti=mean(Rating)) %>%
  arrange(desc(frequenza)) %>%
  left_join(movie_names, "MovieID"))

## film con media voti più elevata
head(movielens_data %>%
       group_by(MovieID) %>%
       summarise(frequenza=n(), media_voti=mean(Rating)) %>%
       arrange(desc(media_voti)) %>%
       left_join(movie_names, "MovieID")) ## recensiti da pochi utenti

head(movielens_data %>%
       group_by(MovieID) %>%
       summarise(frequenza=n(), media_voti=round(mean(Rating),4)) %>%
       filter(frequenza>100) %>%
       arrange(desc(media_voti)) %>%
       left_join(movie_names, "MovieID"))

## film con media voti più bassa
head(movielens_data %>%
       group_by(MovieID) %>%
       summarise(frequenza=n(), media_voti=mean(Rating)) %>%
       arrange(media_voti) %>%
       left_join(movie_names, "MovieID"))

head(movielens_data %>%
       group_by(MovieID) %>%
       summarise(frequenza=n(), media_voti=mean(Rating)) %>%
       filter(frequenza>100) %>%
       arrange(media_voti) %>%
       left_join(movie_names, "MovieID"))

## utenti più attivi
head(movielens_data %>%
  group_by(UserID) %>%
  summarise(frequenza=n(), media_voti=mean(Rating)) %>%
  arrange(desc(frequenza)))

## Imputazione Media #####################################

inc=as(as.matrix(ml_sparse_matrix), "Incomplete") ## 12 MB

inc1=as.matrix(ml_sparse_matrix) ## 179.3MB

## Tramite l'uso dell'oggetto incomplete la dimensione della matice viene ridotta di circa il
## 93 percento, diminuendo in maniera significativa la quantità di memoria necessaria per le computazioni

rm(inc1)

## campionamento dei dati ################################
set.seed(1234)
shuffle_lb=sample(1:dim(movielens_data)[1])

train_set=movielens_data[shuffle_lb[1:round(0.75*dim(movielens_data)[1],0)],]
val_set=movielens_data[shuffle_lb[(round(0.75*dim(movielens_data)[1],0)+
                                    1):round(0.90*dim(movielens_data)[1],0)],]
test_set=movielens_data[shuffle_lb[(round(0.90*dim(movielens_data)[1],0)+
                                     1):dim(movielens_data)[1]],]
dim(train_set)[1]*100/dim(movielens_data)[1] ## ca. 75%
dim(val_set)[1]*100/dim(movielens_data)[1]   ## ca. 15%
dim(test_set)[1]*100/dim(movielens_data)[1]  ## ca. 10%
test_set=test_set %>%
  filter(UserID %in% unique(train_set$UserID), MovieID %in% unique(
    train_set$MovieID
  ))
val_set=val_set %>%
  filter(UserID %in% unique(bind_rows(train_set, val_set)$UserID), MovieID %in% unique(bind_rows(
    train_set, val_set)$MovieID
  ))
big_train=rbind(train_set, val_set)


train_set %>%                                                                   ## Distribuzione dei voti
  mutate(Rating=as.factor(Rating))%>%
  ggplot(aes(x=Rating))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  labs(title="Distribuzione dei voti")+
  ylab("Frequenza Relativa")+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..)), stat= "count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent)


train_set %>%
  group_by(Rating) %>%
  summarize(n=n()/sum(n())) %>%
  ggplot(aes(x=Rating, y=n)) +
  geom_bar()


## imputazione casuale utilizzando le frequenze relative campionarie ######
set.seed(1234)

p <- function(x, y) mean(y == x)
Rating <- seq(1,5,1)
B <- 10^3 ## numero di iterazioni del calcolo della freq. relativa
M <- replicate(B, {
  s <- sample(train_set$Rating, 100, replace = TRUE)
  sapply(Rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))
prob

y_hat_casuale <- sample(Rating, size = nrow(val_set),   ## imputazione casuale
                        replace = TRUE, prob = prob)

risultati <- tibble(metodo = "Previsione casuale", 
                    RMSE = RMSE(val_set$Rating, y_hat_casuale))
risultati


## imputazione della media e calcolo del RMSE ############

mu=mean(train_set$Rating)
risultati=bind_rows(risultati, tibble(
  metodo = "Imputazione della media",
  RMSE=RMSE(val_set$Rating, mu)
))

risultati


## media per colonna (media dei voti per ogni film)

bi=train_set %>%
  group_by(MovieID) %>%
  summarize(b_i=mean(Rating-mu))
head(bi, 7)

bi %>% ggplot(aes(x = b_i)) + 
  geom_histogram(bins=10, col = I("black")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie effect") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + 
  theme_economist()

y_hat_bi <- mu + val_set %>% 
  left_join(bi, by = "MovieID") %>% 
  .$b_i
y_hat_bi
risultati <- bind_rows(risultati, 
                       tibble(metodo = "Media + Movie Effect", 
                              RMSE = RMSE(val_set$Rating, y_hat_bi)))
risultati

## inserimento dell'effetto user

bu <- train_set %>% 
  left_join(bi, by = 'MovieID') %>%
  group_by(UserID) %>%
  summarize(b_u = mean(Rating - mu - b_i))

# previsione tramite user effect
y_hat_bi_bu <- val_set %>% 
  left_join(bi, by='MovieID') %>%
  left_join(bu, by='UserID') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

risultati <- bind_rows(risultati, 
                       tibble(metodo = "Media + Movie Effect + User effect", 
                              RMSE = RMSE(val_set$Rating, y_hat_bi_bu)))
risultati

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(color = "black") + 
  ggtitle("User Effect Distribution") +
  xlab("User Bias") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + 
  theme_economist()


## Regolarizzazione #########################################

regularization <- function(lambda, train, test){
  
  # Media
  mu <- mean(train$Rating)
  
  # Movie effect (bi)
  b_i <- train %>% 
    group_by(MovieID) %>%
    summarize(b_i = sum(Rating - mu)/(n()+lambda))
  
  # User effect (bu)  
  b_u <- train %>% 
    left_join(b_i, by="MovieID") %>%
    filter(!is.na(b_i)) %>%
    group_by(UserID) %>%
    summarize(b_u = sum(Rating - b_i - mu)/(n()+lambda))
  
  # Previsti: mu + bi + bu  
  predicted_ratings <- test %>% 
    left_join(b_i, by = "MovieID") %>%
    left_join(b_u, by = "UserID") %>%
    filter(!is.na(b_i), !is.na(b_u)) %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$Rating))
}

lambdas <- seq(0, 10, 0.1)  ## 100 diversi valori di lambda

rmses <- lapply(lambdas, 
                regularization, 
                train = train_set, 
                test = val_set)
rmses=unlist(rmses)



plot(x=lambdas, y=rmses,type="l", pch=20, xlab = expression(lambda),
     ylab="RMSE", main = expression(paste("RMSE al variare del parametro ", lambda)))
points(x=lambdas, y=rmses,type="p", pch=20)
abline(h=min(rmses), col="grey", lwd=2)
abline(v=lambdas[which(rmses==min(rmses))], col="grey", lwd=2)
text(x=9.5, y=0.9141, labels=round(min(rmses),3), col="black")
text(x=lambdas[which(rmses==min(rmses))]+0.5, y=0.9149, labels=lambdas[which(rmses==min(rmses))], col="black")
points(x=lambdas[which(rmses==min(rmses))], y=min(rmses), col="red", pch=16)

lambda <- lambdas[which.min(rmses)]
mu <- mean(train_set$Rating)

b_i <- train_set %>% 
  group_by(MovieID) %>%
  summarize(b_i = sum(Rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="MovieID") %>%
  group_by(UserID) %>%
  summarize(b_u = sum(Rating - b_i - mu)/(n()+lambda))

y_hat_reg <- val_set %>% 
  left_join(b_i, by = "MovieID") %>%
  left_join(b_u, by = "UserID") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

risultati <- bind_rows(risultati, 
                       tibble(metodo = "Effetto user e effetto movie regolarizzati", 
                              RMSE = RMSE(val_set$Rating, y_hat_reg)))
risultati

## Matrix completion utilizzando SoftImpute ###########
## creazione della matrice sparsa, formato da tidy a Wide

sparse=train_set %>%                                                            ## Sparse sarà una matrice pivot
  spread(key = "MovieID", value = "Rating")

## la prima colonna di sparse contiene gli item ID, verranno salvati a parte, mantenendo l'ordine
## delle righe si manterrà l'univocità tra il numero della riga e l'Id corrispondente.
head(sparse)
usr_id=sparse[,1]
sparse=sparse[,-1]
movie_id=colnames(sparse)
movie_id=as.numeric(movie_id)


sparse_incomplete=as(as.matrix(sparse), "Incomplete")
sparse_incomplete

a=lambda0(sparse_incomplete,trace.it = T)
ris=softImpute(sparse_incomplete, rank.max = 5,lambda = 0, type = "svd", maxit = 1000)


## trovare un modo per trasformare gli indici da movie_id a j e da usr_id a i secondo le posizioni
## in movie_id e usr_id

which(movie_id==1)
which(val_set$MovieID==1)
df_fin1=data.frame("MovieID"=c(), "index_j"=c())
m_id=data.frame("MovieID"=movie_id,"j"=1:length(movie_id))
u_id=data.frame("UserID"=usr_id,"i"=1:length(unlist(usr_id)))
val_rat=val_set$Rating
val_id=val_set[,-3] %>%
  left_join(m_id,by="MovieID")%>%
  left_join(u_id, by="UserID")

val_id
voti_previsti=c()
for (x in 1:nrow(val_id)) {
  tmp=impute(ris, i=as.integer(val_id[x,4]), j=as.integer(val_id[x,3]))
  voti_previsti=append(voti_previsti,tmp)
}

RMSE(val_rat,voti_previsti)


## selezione del parametro lambda, mantenendo fisso 
lambda_si=seq(from = 0,to = a, length.out = 100)
lambda_si
rmses_si=c()
for (i in 1:length(lambda_si)){
  tmp=softImpute(sparse_incomplete, rank.max = 20, lambda = lambda_si[i],
                 type="svd", maxit=1000)
  voti_previsti=c()
  for (x in 1:nrow(val_id)) {
    tmp1=impute(ris, i=as.integer(val_id[x,4]), j=as.integer(val_id[x,3]))
    voti_previsti=append(voti_previsti,tmp1)
  }
  rmses_si=append(rmses_si, RMSE(val_rat, voti_previsti))
}
rmses
plot(y=rmses[-1], x=a-lambda_si, type="l")
points(y=rmses[-1], x=a-lambda_si, pch=16)
length(1:length(lambda_si))
length(rmses)
abline(v=c(920,1020), col="green", lwd=2)
abline(v=lambda_si[which(rmses_si==min(rmses_si))])
min(rmses_si)
rmses_si
soft=softImpute(sparse_incomplete, lambda = a-0.5,
           type="svd", maxit=1000, rank.max = 20)
soft
voti_previsti=c()
for (x in 1:nrow(val_id)) {
  tmp=impute(soft, i=as.integer(val_id[x,4]), j=as.integer(val_id[x,3]))
  voti_previsti=append(voti_previsti,tmp)
}

RMSE(val_rat,voti_previsti)

rmse_lambda=function(lambda_val, matrice = sparse_incomplete, 
                     rango = 30, iterazioni = 1000, tipo = "svd"){
  soft=softImpute(matrice, lambda=lambda_val, type=tipo,
                  maxit = iterazioni, rank.max = rango)
  previsti=c()
  for (x in 1:nrow(val_id)) {
    tmp=impute(soft, i=val_id[x,4], j=val_id[x,3])
    previsti=append(previsti,tmp)
  }
  x=RMSE(val_rat, previsti)
  return(x)
}



numcores=detectCores()

xx=mclapply(lambda_si, rmse_lambda, mc.cores=numcores) ## parallelizzazione del ciclo for

min(unlist(xx)) ## 0.99878
lambda_si[which(xx==min(unlist(xx)))] ## 14.37

rmse_si_nonscal_30=unlist(xx)

## rango 20
rmse_lambda=function(lambda_val, matrice = sparse_incomplete, 
                     rango = 20, iterazioni = 1000, tipo = "svd"){
  soft=softImpute(matrice, lambda=lambda_val, type=tipo,
                  maxit = iterazioni, rank.max = rango)
  previsti=c()
  for (x in 1:nrow(val_id)) {
    tmp=impute(soft, i=val_id[x,4], j=val_id[x,3])
    previsti=append(previsti,tmp)
  }
  x=RMSE(val_rat, previsti)
  return(x)
}

xx1=mclapply(lambda_si, rmse_lambda, mc.cores=numcores) ## parallelizzazione del ciclo for

min(unlist(xx1)) ## 0.99355
lambda_si[which(xx1==min(unlist(xx1)))] ## 14.37

rmse_si_nonscal_20=unlist(xx1)

## rango 40
rmse_lambda=function(lambda_val, matrice = sparse_incomplete, 
                     rango = 40, iterazioni = 1000, tipo = "svd"){
  soft=softImpute(matrice, lambda=lambda_val, type=tipo,
                  maxit = iterazioni, rank.max = rango)
  previsti=c()
  for (x in 1:nrow(val_id)) {
    tmp=impute(soft, i=val_id[x,4], j=val_id[x,3])
    previsti=append(previsti,tmp)
  }
  x=RMSE(val_rat, previsti)
  return(x)
}

xx2=mclapply(lambda_si, rmse_lambda, mc.cores=numcores) ## parallelizzazione del ciclo for

min(unlist(xx2)) ## 1.00827
lambda_si[which(xx2==min(unlist(xx2)))] ## 14.37
rmse_si_nonscal_40=unlist(xx2) 

## rango 5

## non scalata
xx4=mclapply(lambda_si, rmse_lambda, mc.cores=numcores, rango=5) ## parallelizzazione del ciclo for

min(unlist(xx4)) ## 0.9987335
lambda_si[which(xx4==min(unlist(xx4)))] ## 0
rmse_si_nonscal_5=unlist(xx4) 



## scalata
zz4=mclapply(lambda_scaled_seq, rmse_lambda_scaled, mc.cores=numcores, rango=5)
rmse_si_scaled_5=zz4
min(unlist(rmse_si_scaled_5)) ## 0.871358
lambda_scaled_seq[which(zz4==min(unlist(zz4)))] ## 7.532

## riscalare la matrice sparsa
sparse_rescaled=biScale(sparse_incomplete, trace = T,
                        maxit = 100, col.scale = F)
l0=lambda0(sparse_rescaled) ## 93.21343
rmse_lambda_scaled=function(lambda_val, matrice = sparse_rescaled, 
                            rango = 20, iterazioni = 1000, tipo = "svd") {
  message("Inizio softImpute")
  soft=softImpute(matrice, lambda=lambda_val, type=tipo,
                  maxit = iterazioni, rank.max = rango, trace.it = T)
  previsti=c()
  message("Inizio Imputazione rating")
  for (x in 1:nrow(val_id)) {
    tmp=impute(soft, i=as.integer(val_id[x,4]), j=as.integer(val_id[x,3]), unscale = T)
    previsti=append(previsti,tmp)
    perc=round((100*x)/nrow(val_id))
  }
  z=RMSE(val_rat, previsti)
  return(z)
}

## parallelizzazione del ciclo for
lambda_scaled=lambda0(sparse_rescaled)
lambda_scaled_seq=seq(from=0,to=lambda_scaled, length.out = 100)


## rango 20

zz=mclapply(lambda_scaled_seq, rmse_lambda_scaled, mc.cores=numcores, rango=20)
rmse_si_scaled_20=zz 
min(unlist(rmse_si_scaled_20)) ## 0.8677
lambda_scaled_seq[which(zz==min(unlist(zz)))] ## 18.831


## rango 30

zz1=mclapply(lambda_scaled_seq, rmse_lambda_scaled, mc.cores=numcores, rango=30)
rmse_si_scaled_30=zz1
min(unlist(rmse_si_scaled_30)) ## 0.869839
lambda_scaled_seq[which(zz1==min(unlist(zz1)))] ## 23.53875


## rango 40

zz2=mclapply(lambda_scaled_seq, rmse_lambda_scaled, mc.cores=numcores, rango=40)
rmse_si_scaled_40=zz2
min(unlist(rmse_si_scaled_40)) ## 0.8707676
lambda_scaled_seq[which(zz2==min(unlist(zz2)))] ## 24.4803


## rango 100 matrice non scalata

xx3=mclapply(lambda_si, rmse_lambda, mc.cores=numcores, rango=100) ## parallelizzazione del ciclo for

min(unlist(xx3)) ## 1.013638
lambda_si[which(xx3==min(unlist(xx3)))] ## 28.73007
rmse_si_nonscal_100=unlist(xx3) 


## rango 100 matrice scalata

zz3=mclapply(lambda_scaled_seq, rmse_lambda_scaled, mc.cores=numcores, rango=100)
rmse_si_scaled_100=zz3
min(unlist(rmse_si_scaled_100)) ## 0.87101
lambda_scaled_seq[which(zz3==min(unlist(zz3)))] ## 26.3634


## grafico
rmse_si=zz


plot(x = lambda_scaled_seq ,y = rmse_si, type = "l", xlab=expression(lambda), 
     ylab="RMSE")
points(x = lambda_scaled_seq ,y = rmse_si, pch = 16)
abline(h = min(unlist(rmse_si)), col = "grey")
abline(v = lambda_scaled_seq[which(rmse_si == min(unlist(rmse_si)))], col = "grey")
points(x = lambda_scaled_seq[which(rmse_si == min(unlist(rmse_si)))],
       y = min(unlist(rmse_si)), col = "red", pch=16)
abline(v=c(lambda_scaled_seq[which(rmse_si == min(unlist(rmse_si)))]-3,
           lambda_scaled_seq[which(rmse_si == min(unlist(rmse_si)))]+3), col="green",
       lwd=2)

plot(x = lambda_si, y = rmse_si_nonscal_5, type = "l", xlab=expression(lambda), 
     ylab="RMSE", main= "Matrice non scalata")
lines(x= lambda_si, y=rmse_si_nonscal_20)
lines(x= lambda_si, y=rmse_si_nonscal_30)
lines(x= lambda_si, y=rmse_si_nonscal_40)
lines(x= lambda_si, y=rmse_si_nonscal_100)


plot(x = lambda_scaled_seq, y = rmse_si_scaled_5, type = "l", xlab=expression(lambda), 
     ylab="RMSE", main = "Matrice Scalata")
lines(x = lambda_scaled_seq, y = rmse_si_scaled_5)
lines(x = lambda_scaled_seq, y = rmse_si_scaled_20)
lines(x = lambda_scaled_seq, y = rmse_si_scaled_30)
lines(x = lambda_scaled_seq, y = rmse_si_scaled_40)
lines(x = lambda_scaled_seq, y = rmse_si_scaled_100)

## rango 20 matrice scalata

new_lambda = lambda_scaled_seq[which(zz==min(unlist(zz)))]
new_lambda_seq = seq(from=new_lambda-3, to=new_lambda+3, length.out=100)

ptm = proc.time()
rmse_vic = mclapply(new_lambda_seq, rmse_lambda_scaled, mc.cores=numcores)
proc.time() - ptm

min(unlist(rmse_vic)) ## RMSE: 0.8674926
new_lambda_seq[which(rmse_vic==min(unlist(rmse_vic)))] ## LAMBDA: 20.74009
# risultati: RMSE vs lambda

plot(x = new_lambda_seq ,y = rmse_vic, type = "l", xlab=expression(lambda), 
     ylab="RMSE")
points(x = new_lambda_seq ,y = rmse_vic, pch = 16)
abline(h = min(unlist(rmse_vic)), col = "grey")
abline(v = new_lambda_seq[which(rmse_vic == min(unlist(rmse_vic)))], col = "grey")
points(x = new_lambda_seq[which(rmse_vic == min(unlist(rmse_vic)))],
       y = min(unlist(rmse_vic)), col = "red", pch=16)
abline(v=lambda_scaled_seq[which(zz==min(unlist(zz)))], col="red", lwd=1.5) ## vecchio lambda star


lambda_star=new_lambda_seq[which(rmse_vic==min(unlist(rmse_vic)))]
(rmse_star=rmse_lambda_scaled(lambda_star)) 0.867969

## modello scelto 
mod=softImpute(sparse_rescaled, 20, lambda_star, type="svd", trace.it = T)

risultati <- bind_rows(risultati, 
                       tibble(metodo = "SoftImpute", 
                              RMSE = rmse_star))
risultati



## TEST SET con training in (train set U validation set)

set.seed(1234)

p <- function(x, y) mean(y == x)
Rating <- seq(1,5,1)
B <- 10^3 ## numero di iterazioni del calcolo della freq. relativa
M <- replicate(B, {
  s <- sample(big_train$Rating, 100, replace = TRUE)
  sapply(Rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))
prob

y_hat_casuale <- sample(Rating, size = nrow(test_set),   ## imputazione casuale
                        replace = TRUE, prob = prob)

(risultati_test <- tibble(metodo = "Previsione casuale", 
                    RMSE = RMSE(test_set$Rating, y_hat_casuale)))

## Risultati nel Test-set

mu_big <- mean(big_train$Rating)

# Movie effect (bi)
b_i_big <- big_train %>% 
  group_by(MovieID) %>%
  summarize(b_i = sum(Rating - mu_big)/(n()))

# User effect (bu)
b_u_big <- big_train %>% 
  left_join(b_i_big, by="MovieID") %>%
  group_by(UserID) %>%
  summarize(b_u = sum(Rating - b_i - mu_big)/(n()))

# Previsione
y_hat_big <- test_set %>% 
  left_join(b_i_big, by = "MovieID") %>%
  left_join(b_u_big, by = "UserID") %>%
  mutate(pred = mu_big + b_i + b_u) %>%
  pull(pred)

# Update the results table
risultati_test <- bind_rows(risultati_test, tibble(metodo = "Modello Lineare test set", 
                           RMSE = RMSE(test_set$Rating, y_hat_big)))
risultati_test


## softImpute
## creare test_id come nel caso per val_id


test_rat=test_set$Rating
test_id=test_set[,-3] %>%
  left_join(m_id,by="MovieID")%>%
  left_join(u_id, by="UserID")


rmse_lambda_test=function(lambda_val, matrice = sparse_rescaled, 
                          rango = 20, iterazioni = 1000, tipo = "svd") {
  message("Inizio softImpute")
  soft=softImpute(matrice, lambda=lambda_val, type=tipo,
                  maxit = iterazioni, rank.max = rango, trace.it = T)
  previsti_test=c()
  message("Inizio Imputazione rating")
  for (x in 1:nrow(test_id)) {
    tmp=impute(soft, i=as.integer(test_id[x,4]), j=as.integer(test_id[x,3]), 
               unscale = T)
    previsti_test=append(previsti_test,tmp)
    perc=round((100*x)/nrow(test_id))
  }
  z=RMSE(test_rat, previsti_test)
  return(z)
}



rmse_test=rmse_lambda_test(lambda_star) ## viene utilizzato il valore di lambda trovato nel validation set
risultati_test=bind_rows(risultati_test, 
                         tibble(metodo="softImpute Test set", RMSE=rmse_test))
risultati_test


colori=c("red", "orange", "darkgreen")
# m=matrix(c(1,1,2,2,1,1,2,2,3,3,3,3), ncol=4, byrow = T)
# m
# layout(mat=m)
# par(mar = c(0.5,5,5,0.5))
# plot(risultati$RMSE[c(1,4,6)], pch=16, xlim=c(1,3.85),
#      main = "VALIDATION SET",xaxt="n", type = "p", ylab="RMSE", col=colori)
# text(y=risultati$RMSE[c(1,4,6)], x=c(1:3)+0.2,
#      labels = round(risultati$RMSE[c(1,4,6)],4))
# lines(risultati$RMSE[c(1,4,6)], type="h", col=colori)
# plot(risultati_test$RMSE, pch=16, xlim=c(1,3.85),
#      main = "TEST SET", xaxt="n", type = "p", ylab="RMSE", col=colori)
# text(y=risultati_test$RMSE, x=c(1:3)+0.2,
#      labels = round(risultati_test$RMSE,4))
# lines(risultati_test$RMSE, type="h", col=colori)
# plot(1, type = "n", axes=FALSE, xlab="", ylab="")
# legend(x="top", inset=0 ,legend=c("METODO CASUALE", "IMPUTAZIONE MEDIA", "SOFTIMPUTE"),
#        col=c("black","orange", "green"), pch=16, horiz = T)
# par(mfrow=c(1,1), mar=c(5,5,5,5))

par(mar=c(4,4,4,4), mfrow=c(1,1))
plot(risultati$RMSE[c(1,4,6)], pch=16, xlim=c(1,3.85), xlab="",
     main = "CONFRONTO DEI RISULTATI",xaxt="n", type = "p", ylab="RMSE",
     col=colori, ylim=c(0.6,1.7))
text(y=risultati$RMSE[c(1,4,6)]+0.05, x=c(1:3)+0.1,
     labels = round(risultati$RMSE[c(1,4,6)],3))
lines(risultati$RMSE[c(1,4,6)], type="h", col=colori)
axis(1, at=c(1.25,2.25,3.25), 
     labels = c("METODO CASUALE", "IMPUTAZIONE MEDIA + EFFETTI", "SOFTIMPUTE"))
points(risultati_test$RMSE,x=c(1:3)+0.5, pch=16, col=colori)
text(y=risultati_test$RMSE+0.05, x=c(1:3)+0.6,
     labels = round(risultati_test$RMSE,3))
lines(risultati_test$RMSE,x=c(1:3)+0.5, type="h", col=colori, lty=2)
legend(x=3.1, y=1.7, inset=0 ,legend=c("VALIDATION SET", "TEST SET"),
       lty=c(1,2))

################################################################################
################################### END ########################################
################################################################################
