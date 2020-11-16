K <- 2
N <- 5
L0 <- 0.1
L1 <- 0.2
F01mi0 <- 1
F02mi0 <- 2
F11mi1 <- 1
F12mi1 <- 2
GOOD <- 0
FAKE <- 1

gerarExogenosIniciais <- function(){
    for(i in 1:length(Usuarios)){
        if(L0 > 0){
            evento <- c(i, GOOD, rexp(1,L0), i)
            FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento
        }
        if(L1 > 0 ){
            evento <- c(i, FAKE, rexp(1,L1), i )
            FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento
        }
    }
}

gerarEndogenosIniciais <- function(){
    for(i in 1:length(Usuarios)) {
	    gerarEventosEndogenos(i)
    }
}

atualizarTempos <- function(tempo){
        estado <- c(0,0,0,0)
    	for(usuario in Usuarios){
            if(usuario==0){estado[1] <- estado[1] + 1}
            else if(usuario==1){estado[2] <- estado[2] + 1}
            else if(usuario==2){estado[3] <- estado[3] + 1}
            else if(usuario==3){estado[4] <- estado[4] + 1}
        }
        estadoStr <- paste(estado,collapse='')
	    Tempos[[estadoStr]] <<- Tempos[[estadoStr]] + tempo
}

modificarTimelinesFIFO <- function(e){
    if(e[2] == GOOD){
        if(Usuarios[e[4]] == 0){Usuarios[e[4]] <<- 0}
        else if(Usuarios[e[4]] == 1){Usuarios[e[4]] <<- 2}
        else if(Usuarios[e[4]] == 2){Usuarios[e[4]] <<- 0}
        else if(Usuarios[e[4]] == 3){Usuarios[e[4]] <<- 2}
    }                                                            
    else {
        if(Usuarios[e[4]] == 0){Usuarios[e[4]] <<- 1}
        else if(Usuarios[e[4]] == 1){Usuarios[e[4]] <<- 3}
        else if(Usuarios[e[4]] == 2){Usuarios[e[4]] <<- 1}
        else if(Usuarios[e[4]] == 3){Usuarios[e[4]] <<- 3}    
        }                                              
}

modificarTimelinesRND <- function(e){
    if(e[2] == GOOD){
        if(Usuarios[e[4]]==0){Usuarios[e[4]] <<- 0}
        else if(Usuarios[e[4]]==1){Usuarios[e[4]] <<- sample(c(0,1),1,rep=FALSE)} 
        else if(Usuarios[e[4]]==2){Usuarios[e[4]] <<- sample(c(0,2),1,rep=FALSE)}  
        else if(Usuarios[e[4]]==3){Usuarios[e[4]] <<- sample(c(1,2),1,rep=FALSE)}        
    }
    else {
        if(Usuarios[e[4]]==0){Usuarios[e[4]] <<- sample(c(1,2),1,rep=FALSE)}
        else if(Usuarios[e[4]]==1){Usuarios[e[4]] <<- sample(c(1,3),1,rep=FALSE)}
        else if(Usuarios[e[4]]==2){Usuarios[e[4]] <<- sample(c(2,3),1,rep=FALSE)}
        else if(Usuarios[e[4]]==3){Usuarios[e[4]] <<- 3}    
    }
}

arrumarFilasEventos <- function(){
    FilaDeEventos <<- FilaDeEventos[order(sapply(FilaDeEventos, function(x) x[[3]]))]
}

gerarEventosEndogenos <- function(quem){
    
    ###criarEventosNovos
    if(Usuarios[quem]==0){
        evento <- c(quem, GOOD, rexp(1,F02mi0), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento

    }
    if(Usuarios[quem]==1){
        evento <- c(quem, GOOD, rexp(1,F01mi0), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento
        evento <- c(quem, FAKE, rexp(1,F11mi1), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento   
    }
    if(Usuarios[quem]==2){
        evento <- c(quem, GOOD, rexp(1,F01mi0), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento    
        evento <- c(quem, FAKE, rexp(1,F11mi1), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento       
    }
    if(Usuarios[quem]==3){
        evento <- c(quem, FAKE, rexp(1,F12mi1), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento
       
    }            
}

gerarSample <- function(nquero){
    while(TRUE) {
        num <- sample(1:5,1,rep=FALSE)
        if (num != nquero) break
    }
    return(num)
}

FIM <- 10
FIFO <- TRUE
TOTALEVENTOS <- 1000

guardadorTempoInicial <- matrix(0L, ncol = TOTALEVENTOS, nrow = FIM)
guardadorTempoFinal <- matrix(0L, ncol = TOTALEVENTOS, nrow = FIM)
guardadorTempoInicialHits <- matrix(0L, ncol = TOTALEVENTOS, nrow = FIM)
guardadorTempoFinalHits <- matrix(0L, ncol = TOTALEVENTOS, nrow = FIM)
tempoDosEventos <- matrix(0L, ncol = TOTALEVENTOS, nrow = FIM)

intervaloSupTempoInicial <- numeric(TOTALEVENTOS)
intervaloInfTempoInicial <- numeric(TOTALEVENTOS)

intervaloSupTempoFinal <- numeric(TOTALEVENTOS)
intervaloInfTempoFinal <- numeric(TOTALEVENTOS)

for(i in 1:FIM){
    #evento = (de quem veio, good/fake, tempo, pra onde vai)
    T <- 0 
    Usuarios <- c(1,1,1,1,1)
    Tempos <- numeric(56)
    names(Tempos) <- c('5000','4100','4010','4001','1400','0410','0401',
                    '1040','0140','0041','1004','0104','0014','3200',
                    '3020','3002','2300','0320','0302','2030','0230',
                    '0032','2003','0203','0023','3110','3101','3011',
                    '1310','1301','0311','1130','1031','0131','1103',
                    '1013','0113','2210','2201','2120','2021','2102',
                    '2012','1220','0221','1202','0212','1022','0122',
                    '2111','1211','1121','1112','0500','0050','0005')
    j <- 1
    nEventos <- 0

    while(nEventos < TOTALEVENTOS){
        
        FilaDeEventos <- list()
        gerarEndogenosIniciais()
        gerarExogenosIniciais()
        arrumarFilasEventos()

        evento <- FilaDeEventos[[1]]
        atualizarTempos(evento[3])
        T <- T + evento[3]
        nEventos <- nEventos + 1
        # print(paste(T,nEventos))

    
        if(FIFO){modificarTimelinesFIFO(evento)}else{modificarTimelinesRND(evento)}

        guardadorTempoInicial[i,nEventos] <- Tempos[['5000']]/T
        guardadorTempoFinal[i,nEventos] <- Tempos[['0005']]/T
        tempoDosEventos[i,nEventos] <- T

        if(nEventos > 1){
            if(all(Usuarios==c(0,0,0,0,0))){
                guardadorTempoInicialHits[i,nEventos] <- (guardadorTempoInicialHits[i,nEventos-1] + 1)
            }
            else {
                guardadorTempoInicialHits[i,nEventos] <- guardadorTempoInicialHits[i,nEventos-1]
            }
            
            if(all(Usuarios == c(3,3,3,3,3))){
                guardadorTempoFinalHits[i,nEventos] <- (guardadorTempoFinalHits[i,nEventos-1] + 1)
            } else {
                guardadorTempoFinalHits[i,nEventos] <- guardadorTempoFinalHits[i,nEventos-1]
            }
        }
    }
}

for(i in 1:TOTALEVENTOS){
    intervaloSupTempoInicial[i] <- mean(guardadorTempoInicial[,i]) + (1.96 * sd(guardadorTempoInicial[,i]) / sqrt(FIM))
    intervaloInfTempoInicial[i] <- mean(guardadorTempoInicial[,i]) - (1.96 * sd(guardadorTempoInicial[,i]) / sqrt(FIM))

    intervaloSupTempoFinal[i] <- mean(guardadorTempoFinal[,i]) + (1.96 * sd(guardadorTempoFinal[,i]) / sqrt(FIM))
    intervaloInfTempoFinal[i] <- mean(guardadorTempoFinal[,i]) - (1.96 * sd(guardadorTempoFinal[,i]) / sqrt(FIM))
}


print(paste("Superior",intervaloSupTempoInicial[TOTALEVENTOS]))
print(paste("ALL GOOD tempo",colMeans(guardadorTempoInicial)[TOTALEVENTOS]))
print(paste("Inferior",intervaloInfTempoInicial[TOTALEVENTOS]))

print(paste("Superior",intervaloSupTempoFinal[TOTALEVENTOS]))
print(paste("ALL FAKE tempo",colMeans(guardadorTempoFinal)[TOTALEVENTOS]))
print(paste("Inferior",intervaloInfTempoFinal[TOTALEVENTOS]))

# print(paste("ALL GOOD hits",colMeans(guardadorTempoInicialHits)[TOTALEVENTOS]/TOTALEVENTOS))
# print(paste("ALL FAKE hits",colMeans(guardadorTempoFinalHits)[TOTALEVENTOS]/TOTALEVENTOS))

# print(paste("Tempo médio:", colMeans(tempoDosEventos)[TOTALEVENTOS]))

plot(colMeans(tempoDosEventos), colMeans(guardadorTempoInicial), type="l", xlim=c(0,colMeans(tempoDosEventos)[TOTALEVENTOS]), ylim=c(0,1), main=paste("Estado Inicial:", toString(Usuarios)), xlab="Tempo", ylab="Probabilidade", col="red")
lines(colMeans(tempoDosEventos), intervaloSupTempoInicial, col="#630000")
lines(colMeans(tempoDosEventos), intervaloInfTempoInicial, col="#EA7E7E")
lines(colMeans(tempoDosEventos), colMeans(guardadorTempoFinal), col="blue")
lines(colMeans(tempoDosEventos), intervaloSupTempoFinal, col="#002258")
lines(colMeans(tempoDosEventos), intervaloInfTempoFinal, col="#8BACDF")
abline(h=seq(0,1,by=0.1), lwd=1, lty="dotted", col="lightgray")
abline(v=c(1:colMeans(tempoDosEventos)[TOTALEVENTOS]), lwd=1, lty="dotted", col="lightgray")
legend("topleft",
c("Estado 0005 (ALL FAKE)","Estado 5000 (ALL NON FAKE)"),
fill=c("blue","red")
) 
# print(Tempos*100/T)
# res <- 0
# for(tempo in Tempos){
#     res <- res + tempo
# }
# print(res/T)

