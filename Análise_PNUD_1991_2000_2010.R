bs = read.csv("municipios.csv",header = T,sep=";",dec=",")
bs2010 = bs[bs["ano"] == 2010,]

#1
bs2010 %>%
  select(municipio,ufn,idhm) %>%
  filter(idhm > 0.8) %>%
  arrange(desc(idhm)) %>%
  count(ufn)
# municipio  idhm
# 1     S?O CAETANO DO SUL 0.862
# 2     ?GUAS DE S?O PEDRO 0.854
# 3          FLORIAN?POLIS 0.847
# 4                VIT?RIA 0.845
# 5     BALNE?RIO CAMBORI? 0.845
# 6                 SANTOS 0.840

###_________________________________________________________
#2
bs2 = read.csv("pnud.csv",header = T,sep=";",dec=",")
bs2000 = bs[bs["ano"] == 2000,]
bs2000$poptotal = bs2000$homemtot + bs2000$mulhertot

bs2000 %>%
  select(ufn,espvida,poptotal) %>%
  group_by(ufn) %>%
  summarise(evd_pond_est = weighted.mean(espvida,poptotal)) %>%
  arrange(evd_pond_est)
#O Maranh?o ? a unidade federativa com menor expectativa de vida m?dia, 
#ponderada pela popula??o dos munic?pios em 2000. Essa espectativa ? de 63.9 anos.

###_________________________________________________________
#3
bs1991 = bs[bs["ano"] == 1991,]

a = quantile(bs1991$gini,0.75) + (1.5*(quantile(bs1991$gini,0.75) - quantile(bs1991$gini,0.25)))
b = quantile(bs1991$gini,0.25) - (1.5*(quantile(bs1991$gini,0.75) - quantile(bs1991$gini,0.25)))

bs1991 %>%
  select(municipio,ufn,gini) %>%
  filter(gini > a | gini < b) %>%
  arrange(desc(gini))

###__________________________________________________________
#Complementares
#1 - 1991
bs1991$poptotal = bs1991$homemtot + bs1991$mulhertot
base1991 = bs1991 %>%
  select(municipio,ufn,poptotal) %>%
  arrange(poptotal)
base1991$num = 1:nrow(base1991)

base1991$popacum = NULL
for(i in 1:nrow(base1991)){
  if(i == 1){
    base1991$popacum[i] = base1991$poptotal[i]
  }else{
    base1991$popacum[i] = base1991$poptotal[i] + base1991$popacum[i-1]
  }
}

plot(base1991$num,base1991$popacum,type="l",
     xlab="Número de Cidades",ylab="População Acumulada",lwd = 2)

POP = function(x){
  t = base1991[round(quantile(1:5565,probs=seq(0.01,1,0.01)),0)[x],"popacum"]
  paste(x,"%","municípios menos populosos acumulam uma população de",t,"habitantes")
}
POP(20)

#2 - 2000
base2000 = bs2000 %>%
  select(municipio,ufn,poptotal) %>%
  arrange(poptotal)

base2000$num = 1:nrow(base2000)

base2000$popacum = NULL
for(i in 1:nrow(base2000)){
  if(i == 1){
    base2000$popacum[i] = base2000$poptotal[i]
  }else{
    base2000$popacum[i] = base2000$poptotal[i] + base2000$popacum[i-1]
  }
}

plot(base2000$num,base2000$popacum,type="l",
     xlab="N?mero de Cidades",ylab="Popula??o Acumulada",lwd = 2)

POPP = function(x){
  t = base2000[round(quantile(1:5565,probs=seq(0.01,1,0.01)),0)[x],"popacum"]
  paste(x,"%","munic?pios menos populosos acumulam uma popula??o de",t,"habitantes")
}
POPP(20)

#3 - 2010
bs2010$poptotal = bs2010$homemtot + bs2010$mulhertot
base2010 = bs2010 %>%
  select(municipio,ufn,poptotal) %>%
  arrange(poptotal)

base2010$num = 1:nrow(base2010)

base2010$popacum = NULL
for(i in 1:nrow(base2010)){
  if(i == 1){
    base2010$popacum[i] = base2010$poptotal[i]
  }else{
    base2010$popacum[i] = base2010$poptotal[i] + base2010$popacum[i-1]
  }
}

plot(base2010$num,base2010$popacum,type="l",
     xlab="Número de Cidades",ylab="População Acumulada",lwd = 2)

PPOPP = function(x){
  t = base2010[round(quantile(1:5565,probs=seq(0.01,1,0.01)),0)[x],"popacum"]
  paste(x,"%","munic?pios menos populosos acumulam uma popula??o de",t,"habitantes")
}
PPOPP(100)

#4
#1991
bs1991$rpc = NULL
for(i in 1:nrow(bs1991)){
  if(bs1991$rdpc[i] < quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[1]){
    bs1991$rpc[i] = "Muito Baixa"
  }
  if(bs1991$rdpc[i] >= quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[1] & bs1991$rdpc[i] < quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[2]){
    bs1991$rpc[i] = "Baixa"
  }
  if(bs1991$rdpc[i] >= quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[2] & bs1991$rdpc[i] < quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[3]){
    bs1991$rpc[i] = "Média"
  }
  if(bs1991$rdpc[i] >= quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[3] & bs1991$rdpc[i] < quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[4]){
    bs1991$rpc[i] = "Alta"
  }
  if(bs1991$rdpc[i] >= quantile(bs1991$rdpc,probs=seq(0.2,1,0.2))[4]){
    bs1991$rpc[i] = "Muito Alta"
  }
}

cat = bs1991 %>%
  select(municipio,espvida,rpc) %>%
  group_by(rpc) %>%
  summarise(mean_lifexp_rpc = mean(espvida)) %>%
  arrange(mean_lifexp_rpc)
barplot(cat$mean_lifexp_rpc,names.arg = cat$rpc,col = 1:5,ylim = c(0,70))

#2000
bs2000$rpc = NULL
for(i in 1:nrow(bs2000)){
  if(bs2000$rdpc[i] < quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[1]){
    bs2000$rpc[i] = "Muito Baixa"
  }
  if(bs2000$rdpc[i] >= quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[1] & bs2000$rdpc[i] < quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[2]){
    bs2000$rpc[i] = "Baixa"
  }
  if(bs2000$rdpc[i] >= quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[2] & bs2000$rdpc[i] < quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[3]){
    bs2000$rpc[i] = "Média"
  }
  if(bs2000$rdpc[i] >= quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[3] & bs2000$rdpc[i] < quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[4]){
    bs2000$rpc[i] = "Alta"
  }
  if(bs2000$rdpc[i] >= quantile(bs2000$rdpc,probs=seq(0.2,1,0.2))[4]){
    bs2000$rpc[i] = "Muito Alta"
  }
}

cat2 = bs2000 %>%
  select(municipio,espvida,rpc) %>%
  group_by(rpc) %>%
  summarise(mean_lifexp_rpc = mean(espvida)) %>%
  arrange(mean_lifexp_rpc)
barplot(cat2$mean_lifexp_rpc,names.arg = cat2$rpc,col = 1:5,ylim = c(0,85),ylab = "Expectativa de vida")

#2010
bs2010$rpc = NULL
for(i in 1:nrow(bs2010)){
  if(bs2010$rdpc[i] < quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[1]){
    bs2010$rpc[i] = "Muito Baixa"
  }
  if(bs2010$rdpc[i] >= quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[1] & bs2010$rdpc[i] < quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[2]){
    bs2010$rpc[i] = "Baixa"
  }
  if(bs2010$rdpc[i] >= quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[2] & bs2010$rdpc[i] < quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[3]){
    bs2010$rpc[i] = "Média"
  }
  if(bs2010$rdpc[i] >= quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[3] & bs2010$rdpc[i] < quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[4]){
    bs2010$rpc[i] = "Alta"
  }
  if(bs2010$rdpc[i] >= quantile(bs2010$rdpc,probs=seq(0.2,1,0.2))[4]){
    bs2010$rpc[i] = "Muito Alta"
  }
}

cat3 = bs2010 %>%
  select(municipio,espvida,rpc) %>%
  group_by(rpc) %>%
  summarise(mean_lifexp_rpc = mean(espvida)) %>%
  arrange(mean_lifexp_rpc)
barplot(cat3$mean_lifexp_rpc,names.arg = cat3$rpc,col = 1:5,ylim = c(0,85),ylab = "Expectativa de vida")

#Ao longo dos anos, a renda per capita influencia em melhores condi??es de vida
#As cidades com maior renda per capita tendem a ter expectativas m?dias de vida mais altas em rela??o 
#? cidades com renda per capita m?dia e baixa.

###________________________________________________________
#Você acha que a situação do Brasil está melhorando? 
#Justifique utlizando dados de pelo menos 3 variáveis.

table(bs2$uf)
bs22 = bs2 %>%
  select(ano,uf,ufn,idhm,idhm_e,idhm_l,idhm_r)

bs22$regiao = NULL
for(i in 1:nrow(bs22)){
  if(bs22$uf[i] >= 10 & bs22$uf[i] <= 19){
    bs22$regiao[i] = "Norte"
  }
  if(bs22$uf[i] >= 20 & bs22$uf[i] <= 29){
    bs22$regiao[i] = "Nordeste"
  }
  if(bs22$uf[i] >= 30 & bs22$uf[i] <= 39){
    bs22$regiao[i] = "Sudeste"
  }
  if(bs22$uf[i] >= 40 & bs22$uf[i] <= 49){
    bs22$regiao[i] = "Sul"
  }
  if(bs22$uf[i] >= 50){
    bs22$regiao[i] = "Centro-Oeste"
  }
}
#
plot(bs22$ano[bs22$ufn == "Acre"],bs22$idhm_e[bs22$ufn == "Acre"],
     type = "l",ylab = "IDH_Educação",xlab = "Ano",
     main = "IDH-Educacao dos Estados da Região Norte em 1991, 2000 e 2010",
     ylim = c(0,0.7),lwd = 2)
lines(bs22$ano[bs22$ufn == "Amapá"],bs22$idhm_e[bs22$ufn == "Amapá"],
     col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Amazonas"],bs22$idhm_e[bs22$ufn == "Amazonas"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Pará"],bs22$idhm_e[bs22$ufn == "Pará"],
      col = 4,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rondônia"],bs22$idhm_e[bs22$ufn == "Rondônia"],
      col = 5,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Roraima"],bs22$idhm_e[bs22$ufn == "Roraima"],
      col = 6,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Tocantins"],bs22$idhm_e[bs22$ufn == "Tocantins"],
      col = 7,type="l",lwd=2)
legend("bottomright",c("Acre","Amapá","Amazonas","Pará",
                       "Rondônia","Roraima","Tocantins"),pch=19,
       col=1:7,ncol=2,cex=0.8)

plot(bs22$ano[bs22$ufn == "Maranhão"],bs22$idhm_e[bs22$ufn == "Maranhão"],
     type = "l",ylab = "IDH_Educação",xlab = "Ano",
     main = "IDH-Educacao dos Estados da Região Nordeste em 1991, 2000 e 2010",
     ylim = c(0,0.7),lwd = 2)
lines(bs22$ano[bs22$ufn == "Piauí"],bs22$idhm_e[bs22$ufn == "Piauí"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Ceará"],bs22$idhm_e[bs22$ufn == "Ceará"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rio Grande do Norte"],
      bs22$idhm_e[bs22$ufn == "Rio Grande do Norte"],
      col = 4,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Paraíba"],bs22$idhm_e[bs22$ufn == "Paraíba"],
      col = 5,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Pernambuco"],bs22$idhm_e[bs22$ufn == "Pernambuco"],
      col = 6,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Alagoas"],bs22$idhm_e[bs22$ufn == "Alagoas"],
      col = 7,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Sergipe"],bs22$idhm_e[bs22$ufn == "Sergipe"],
      col = 8,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Bahia"],bs22$idhm_e[bs22$ufn == "Bahia"],
      col = "Pink",type="l",lwd=2)
legend("bottomright",c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
                       "Pernambuco","Alagoas","Sergipe","Bahia"),pch=19,
       col=c(1:8,"Pink"),ncol=2,cex=0.6)

plot(bs22$ano[bs22$ufn == "Espírito Santo"],bs22$idhm_e[bs22$ufn == "Espírito Santo"],
     type = "l",ylab = "IDH_Educação",xlab = "Ano",
     main = "IDH-Educacao dos Estados da Região Sudeste em 1991, 2000 e 2010",
     ylim = c(0.2,0.75),lwd = 2)
lines(bs22$ano[bs22$ufn == "Minas Gerais"],bs22$idhm_e[bs22$ufn == "Minas Gerais"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rio de Janeiro"],bs22$idhm_e[bs22$ufn == "Rio de Janeiro"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "São Paulo"],bs22$idhm_e[bs22$ufn == "São Paulo"],
      col = 4,type="l",lwd=2)
legend("bottomright",c("Espírito Santo","Minas Gerais",
                       "Rio de Janeiro","São Paulo"),pch=19,
       col=1:4,cex=0.9)

plot(bs22$ano[bs22$ufn == "Paraná"],bs22$idhm_e[bs22$ufn == "Paraná"],
     type = "l",ylab = "IDH_Educação",xlab = "Ano",
     main = "IDH-Educacao dos Estados da Região Sul em 1991, 2000 e 2010",
     ylim = c(0.2,0.75),lwd = 2)
lines(bs22$ano[bs22$ufn == "Rio Grande do Sul"],bs22$idhm_e[bs22$ufn == "Rio Grande do Sul"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Santa Catarina"],bs22$idhm_e[bs22$ufn == "Santa Catarina"],
      col = 3,type="l",lwd=2)
legend("bottomright",c("Paraná","Rio Grande do Sul","Santa Catarina"),
       pch=19,col=1:3,cex=1.1)

plot(bs22$ano[bs22$ufn == "Distrito Federal"],bs22$idhm_e[bs22$ufn == "Distrito Federal"],
     type = "l",ylab = "IDH_Educação",xlab = "Ano",
     main = "IDH-Educacao dos Estados da Região Centro-Oeste em 1991, 2000 e 2010",
     ylim = c(0.2,0.75),lwd = 2)
lines(bs22$ano[bs22$ufn == "Goiás"],bs22$idhm_e[bs22$ufn == "Goiás"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Mato Grosso"],bs22$idhm_e[bs22$ufn == "Mato Grosso"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Mato Grosso do Sul"],bs22$idhm_e[bs22$ufn == "Mato Grosso do Sul"],
      col = 4,type="l",lwd=2)
legend("bottomright",c("Distrito Federal","Goias",
                       "Mato Grosso","Mato Grosso do Sul"),pch=19,
       col=1:4,cex=0.9)

#
plot(bs22$ano[bs22$ufn == "Acre"],bs22$idhm_l[bs22$ufn == "Acre"],
     type = "l",ylab = "IDH_Longevidade",xlab = "Ano",
     main = "IDH-Longevidade dos Estados da Região Norte em 1991, 2000 e 2010",
     ylim = c(0.55,0.85),lwd = 2)
lines(bs22$ano[bs22$ufn == "Amapá"],bs22$idhm_l[bs22$ufn == "Amapá"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Amazonas"],bs22$idhm_l[bs22$ufn == "Amazonas"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Pará"],bs22$idhm_l[bs22$ufn == "Pará"],
      col = 4,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rondônia"],bs22$idhm_l[bs22$ufn == "Rondônia"],
      col = 5,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Roraima"],bs22$idhm_l[bs22$ufn == "Roraima"],
      col = 6,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Tocantins"],bs22$idhm_l[bs22$ufn == "Tocantins"],
      col = 7,type="l",lwd=2)
legend("bottomright",c("Acre","Amapá","Amazonas","Pará",
                       "Rondônia","Roraima","Tocantins"),pch=19,
       col=1:7,cex=0.8)

plot(bs22$ano[bs22$ufn == "Maranhão"],bs22$idhm_l[bs22$ufn == "Maranhão"],
     type = "l",ylab = "IDH_Longevidade",xlab = "Ano",
     main = "IDH-Longevidade dos Estados da Região Nordeste em 1991, 2000 e 2010",
     ylim = c(0.55,0.85),lwd = 2)
lines(bs22$ano[bs22$ufn == "Piauí"],bs22$idhm_l[bs22$ufn == "Piauí"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Ceará"],bs22$idhm_l[bs22$ufn == "Ceará"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rio Grande do Norte"],
      bs22$idhm_l[bs22$ufn == "Rio Grande do Norte"],
      col = 4,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Paraíba"],bs22$idhm_l[bs22$ufn == "Paraíba"],
      col = 5,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Pernambuco"],bs22$idhm_l[bs22$ufn == "Pernambuco"],
      col = 6,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Alagoas"],bs22$idhm_l[bs22$ufn == "Alagoas"],
      col = 7,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Sergipe"],bs22$idhm_l[bs22$ufn == "Sergipe"],
      col = 8,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Bahia"],bs22$idhm_l[bs22$ufn == "Bahia"],
      col = "Brown",type="l",lwd=2)
legend("bottomright",c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
                       "Pernambuco","Alagoas","Sergipe","Bahia"),pch=19,
       col=c(1:8,"Brown"),ncol=2,cex=0.6)

plot(bs22$ano[bs22$ufn == "Espírito Santo"],bs22$idhm_l[bs22$ufn == "Espírito Santo"],
     type = "l",ylab = "IDH_Longevidade",xlab = "Ano",
     main = "IDH-Longevidade dos Estados da Região Sudeste em 1991, 2000 e 2010",
     ylim = c(0.65,0.87),lwd = 2)
lines(bs22$ano[bs22$ufn == "Minas Gerais"],bs22$idhm_l[bs22$ufn == "Minas Gerais"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rio de Janeiro"],bs22$idhm_l[bs22$ufn == "Rio de Janeiro"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "São Paulo"],bs22$idhm_l[bs22$ufn == "São Paulo"],
      col = 4,type="l",lwd=2)
legend("bottomright",c("Espírito Santo","Minas Gerais",
                       "Rio de Janeiro","São Paulo"),pch=19,
       col=1:4,cex=0.9)

plot(bs22$ano[bs22$ufn == "Paraná"],bs22$idhm_l[bs22$ufn == "Paraná"],
     type = "l",ylab = "IDH_Longevidade",xlab = "Ano",
     main = "IDH-Longevidade dos Estados da Região Sul em 1991, 2000 e 2010",
     ylim = c(0.65,0.9),lwd = 2)
lines(bs22$ano[bs22$ufn == "Rio Grande do Sul"],bs22$idhm_l[bs22$ufn == "Rio Grande do Sul"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Santa Catarina"],bs22$idhm_l[bs22$ufn == "Santa Catarina"],
      col = 3,type="l",lwd=2)
legend("bottomright",c("Paraná","Rio Grande do Sul","Santa Catarina"),
       pch=19,col=1:3,cex=1.1)

plot(bs22$ano[bs22$ufn == "Distrito Federal"],bs22$idhm_l[bs22$ufn == "Distrito Federal"],
     type = "l",ylab = "IDH_Longevidade",xlab = "Ano",
     main = "IDH-Longevidade dos Estados da Região Centro-Oeste em 1991, 2000 e 2010",
     ylim = c(0.65,0.9),lwd = 2)
lines(bs22$ano[bs22$ufn == "Goiás"],bs22$idhm_l[bs22$ufn == "Goiás"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Mato Grosso"],bs22$idhm_l[bs22$ufn == "Mato Grosso"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Mato Grosso do Sul"],bs22$idhm_l[bs22$ufn == "Mato Grosso do Sul"],
      col = 4,type="l",lwd=2)
legend("bottomright",c("Distrito Federal","Goias",
                       "Mato Grosso","Mato Grosso do Sul"),pch=19,
       col=1:4,cex=0.9)

#
plot(bs22$ano[bs22$ufn == "Acre"],bs22$idhm_r[bs22$ufn == "Acre"],
     type = "l",ylab = "IDH_Renda",xlab = "Ano",
     main = "IDH-Renda dos Estados da Região Norte em 1991, 2000 e 2010",
     ylim = c(0.55,0.75),lwd = 2)
lines(bs22$ano[bs22$ufn == "Amapá"],bs22$idhm_r[bs22$ufn == "Amapá"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Amazonas"],bs22$idhm_r[bs22$ufn == "Amazonas"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Pará"],bs22$idhm_r[bs22$ufn == "Pará"],
      col = 4,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rondônia"],bs22$idhm_r[bs22$ufn == "Rondônia"],
      col = 5,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Roraima"],bs22$idhm_r[bs22$ufn == "Roraima"],
      col = 6,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Tocantins"],bs22$idhm_r[bs22$ufn == "Tocantins"],
      col = 7,type="l",lwd=2)
legend("bottomright",c("Acre","Amapá","Amazonas","Pará",
                       "Rondônia","Roraima","Tocantins"),pch=19,
       col=1:7,ncol=2,cex=0.6)

plot(bs22$ano[bs22$ufn == "Maranhão"],bs22$idhm_r[bs22$ufn == "Maranhão"],
     type = "l",ylab = "IDH_Renda",xlab = "Ano",
     main = "IDH-Renda dos Estados da Região Nordeste em 1991, 2000 e 2010",
     ylim = c(0.45,0.7),lwd = 2)
lines(bs22$ano[bs22$ufn == "Piauí"],bs22$idhm_r[bs22$ufn == "Piauí"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Ceará"],bs22$idhm_r[bs22$ufn == "Ceará"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rio Grande do Norte"],
      bs22$idhm_r[bs22$ufn == "Rio Grande do Norte"],
      col = 4,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Paraíba"],bs22$idhm_r[bs22$ufn == "Paraíba"],
      col = 5,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Pernambuco"],bs22$idhm_r[bs22$ufn == "Pernambuco"],
      col = 6,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Alagoas"],bs22$idhm_r[bs22$ufn == "Alagoas"],
      col = 7,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Sergipe"],bs22$idhm_r[bs22$ufn == "Sergipe"],
      col = 8,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Bahia"],bs22$idhm_r[bs22$ufn == "Bahia"],
      col = "Brown",type="l",lwd=2)
legend("bottomright",c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
                       "Pernambuco","Alagoas","Sergipe","Bahia"),pch=19,
       col=c(1:8,"Brown"),ncol=2,cex=0.6)

plot(bs22$ano[bs22$ufn == "Espírito Santo"],bs22$idhm_r[bs22$ufn == "Espírito Santo"],
     type = "l",ylab = "IDH_Renda",xlab = "Ano",
     main = "IDH-Renda dos Estados da Região Sudeste em 1991, 2000 e 2010",
     ylim = c(0.6,0.8),lwd = 2)
lines(bs22$ano[bs22$ufn == "Minas Gerais"],bs22$idhm_r[bs22$ufn == "Minas Gerais"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Rio de Janeiro"],bs22$idhm_r[bs22$ufn == "Rio de Janeiro"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "São Paulo"],bs22$idhm_r[bs22$ufn == "São Paulo"],
      col = 4,type="l",lwd=2)
legend("bottomright",c("Espírito Santo","Minas Gerais",
                       "Rio de Janeiro","São Paulo"),pch=19,
       col=1:4,cex=0.9)

plot(bs22$ano[bs22$ufn == "Paraná"],bs22$idhm_r[bs22$ufn == "Paraná"],
     type = "l",ylab = "IDH_Renda",xlab = "Ano",
     main = "IDH-Renda dos Estados da Região Sul em 1991, 2000 e 2010",
     ylim = c(0.6,0.8),lwd = 2)
lines(bs22$ano[bs22$ufn == "Rio Grande do Sul"],bs22$idhm_r[bs22$ufn == "Rio Grande do Sul"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Santa Catarina"],bs22$idhm_r[bs22$ufn == "Santa Catarina"],
      col = 3,type="l",lwd=2)
legend("bottomright",c("Paraná","Rio Grande do Sul","Santa Catarina"),
       pch=19,col=1:3,cex=1.1)

plot(bs22$ano[bs22$ufn == "Distrito Federal"],bs22$idhm_r[bs22$ufn == "Distrito Federal"],
     type = "l",ylab = "IDH_Renda",xlab = "Ano",
     main = "IDH-Renda dos Estados da Região Centro-Oeste em 1991, 2000 e 2010",
     ylim = c(0.6,0.9),lwd = 2)
lines(bs22$ano[bs22$ufn == "Goiás"],bs22$idhm_r[bs22$ufn == "Goiás"],
      col = 2,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Mato Grosso"],bs22$idhm_r[bs22$ufn == "Mato Grosso"],
      col = 3,type="l",lwd=2)
lines(bs22$ano[bs22$ufn == "Mato Grosso do Sul"],bs22$idhm_r[bs22$ufn == "Mato Grosso do Sul"],
      col = 4,type="l",lwd=2)
legend("bottomright",c("Distrito Federal","Goias",
                       "Mato Grosso","Mato Grosso do Sul"),pch=19,
       col=1:4,cex=0.7)

###______________________________________________
#No nordeste o aumento de distribuição de renda foi maior do que no sudeste?
bs22 = bs2 %>%
  select(ano,uf,ufn,idhm,idhm_e,idhm_l,idhm_r,gini)
#regiao já rodou na questão anterior

ND = c(mean(bs22$gini[bs22$ano == 1991 & bs22$regiao == "Nordeste"]),
     mean(bs22$gini[bs22$ano == 2000 & bs22$regiao == "Nordeste"]),
     mean(bs22$gini[bs22$ano == 2010 & bs22$regiao == "Nordeste"]))

SD = c(mean(bs22$gini[bs22$ano == 1991 & bs22$regiao == "Sudeste"]),
       mean(bs22$gini[bs22$ano == 2000 & bs22$regiao == "Sudeste"]),
       mean(bs22$gini[bs22$ano == 2010 & bs22$regiao == "Sudeste"]))

nd = c(0,(ND[2]/ND[1] * 100)-100,(ND[3]/ND[2] * 100)-(ND[2]/ND[1] * 100))
sd = c(0,(SD[2]/SD[1] * 100)-100,(SD[3]/SD[2] * 100)-(SD[2]/SD[1] * 100))

plot(bs22$ano[bs22$ufn == "Paraná"],nd,
     type = "b",ylab = "Aumento de Distribuição de Renda",xlab = "Ano",
     main = "Aumento de Distribuição de Renda em relação aos anos de 1991 e 2000",
     ylim = c(-10,3),lwd = 10,col="Green")
lines(bs22$ano[bs22$ufn == "Mato Grosso do Sul"],sd,
      col = 2,type="b",lwd=10)
legend("topright",c("Nordeste","Sudeste"),pch=19,
       col=c("Green","Red"),cex=1)

###_________________________________________
###Medidas Descritivas
##Expectativa de vida
#1991
summary(bs1991$espvida)
boxplot(bs1991$espvida,col="Light Blue",main = "Expectativa de Vida nos municípios do Brasil - 1991",
        ylab = "Expectativa de Vida")
arrange(bs1991[,c(5,6)],bs1991$espvida)
#CUNHATAI, GUABIRUBA - Melhores; SÃO JOSÉ DA TAPERA, ILHA GRANDE E MARCAÇÃO - Piores

summary(bs2$espvida[bs2$ano==1991])
boxplot(bs2$espvida[bs2$ano==1991],col = "Red",main = "Expectativa de Vida nos estados do Brasil - 1991",
        ylab = "Expectativa de Vida")
arrange(bs2[bs2$ano==1991,3:4],desc(bs2$espvida[bs2$ano==1991]))
#SC, DF, SP, RS, MS - Melhores; MA, AL, PB, SE, BA - Piores.

#2000
summary(bs2000$espvida)
boxplot(bs2000$espvida,col="Light Blue",main = "Expectativa de Vida nos municípios do Brasil - 2000",
        ylab = "Expectativa de Vida")
arrange(bs2000[,c(5,6)],desc(bs2000$espvida))
#CUNHATAI, AGUAS DE SÃO PEDRO E SÃO CAETANO DO SUL - Melhores;ROTEIRO, VARZEA NOVA E JUCATI - Piores

summary(bs2$espvida[bs2$ano==2000])
boxplot(bs2$espvida[bs2$ano==2000],col = "Red",main = "Expectativa de Vida nos estados do Brasil - 2000",
        ylab = "Expectativa de Vida")
arrange(bs2[bs2$ano==2000,3:4],desc(bs2$espvida[bs2$ano==2000]))
#DF,SC,RS,SP,ES - Melhores; AL,MA,PB,PI,SE - Piores

#2010
summary(bs2010$espvida)
boxplot(bs2010$espvida,col="Light Blue",main = "Expectativa de Vida nos municípios do Brasil - 2010",
        ylab = "Expectativa de Vida")
arrange(bs2010[,c(5,6)],bs2010$espvida)
#BLUMENAU, BRUSQUE, BALNEÁRIO CAMBORIÚ- Melhores;ROTEIRO, CACIMBAS E OLHO D´ÁGUA GRANDE - Piores

summary(bs2$espvida[bs2$ano==2010])
boxplot(bs2$espvida[bs2$ano==2010],col = "Red",main = "Expectativa de Vida nos estados do Brasil - 2010",
        ylab = "Expectativa de Vida")
arrange(bs2[bs2$ano==2010,3:4],desc(bs2$espvida[bs2$ano==2010]))
#DF,SC,SP,RS,MG - Melhores; AL,MA,PI,AC,SE - Piores

##Expectativa de anos de estudo até os 18 anos de idade
#1991
summary(bs1991$e_anosestudo)
boxplot(bs1991$e_anosestudo,col="Light Blue",main = "Expectativa de anos de estudo até os 18 anos nos municípios do Brasil - 1991",
        ylab = "Exp. anos de estudo")
arrange(bs1991[,c(238,5,14)],desc(bs1991$e_anosestudo))
#SANTO ANTÔNIO DO PLANALTO-RS, SERAFINA CORRÊA-RS E MATOS COSTA-SC - Melhores
#CORONEL JOÃO SÁ-BA E MELGAÇO-PA - Piores

summary(bs2$e_anosestudo[bs2$ano==1991])
boxplot(bs2$e_anosestudo[bs2$ano==1991],col = "Red",main = "Expectativa de anos de estudo até os 18 anos nos estados do Brasil - 1991",
        ylab = "Exp. anos de estudo")
arrange(bs2[bs2$ano==1991,c(3,12)],desc(bs2$e_anosestudo[bs2$ano==1991]))
#RS, SC, SP, PR - Melhores; BA, PI, PB - Piores.

#2000
summary(bs2000$e_anosestudo)
boxplot(bs2000$e_anosestudo,col="Light Blue",main = "Expectativa de anos de estudo até os 18 anos nos municípios do Brasil - 2000",
        ylab = "Exp. anos de estudo")
arrange(bs2000[,c(238,5,14)],bs2000$e_anosestudo)[1:3,]
#NICOLAU VERGUEIRO, ALMIRANTE TAMANDARÉ DO SUL-RS E NOVA CASTILHO-SP - Melhores.
#CHAVES-PA, JORDÃO-AC E UARINI-AM - Piores

summary(bs2$e_anosestudo[bs2$ano==2000])
boxplot(bs2$e_anosestudo[bs2$ano==2000],col = "Red",main = "Expectativa de anos de estudo até os 18 anos nos estados do Brasil - 2000",
        ylab = "Exp. anos de estudo")
arrange(bs2[bs2$ano==2000,c(3,12)],desc(bs2$e_anosestudo[bs2$ano==2000]))
#RS, SC, SP - Melhores; AL, PI, AM - Piores.

#2010
summary(bs2010$e_anosestudo)
boxplot(bs2010$e_anosestudo,col="Light Blue",main = "Expectativa de anos de estudo até os 18 anos nos municípios do Brasil - 2010",
        ylab = "Exp. anos de estudo")
arrange(bs2010[,c(238,5,14)],desc(bs2010$e_anosestudo))[1:3,]
#GODOY MOREIRA-PR, SÃO JOSÉ DA INHACORÁ-RS E SANTO ANTÔNIO DO PARAÍSO-PR - Melhores.
#BARCELOS, ATALAIA DO NORTE-AM E SANTA ROSA DO PURUS-AC - Piores

summary(bs2$e_anosestudo[bs2$ano==2010])
boxplot(bs2$e_anosestudo[bs2$ano==2010],col = "Red",main = "Expectativa de anos de estudo até os 18 anos nos estados do Brasil - 2010",
        ylab = "Exp. anos de estudo")
arrange(bs2[bs2$ano==2010,c(3,12)],desc(bs2$e_anosestudo[bs2$ano==2010]))
#PR, SP, SC - Melhores; PA, AM, BA - Piores.

##Renda per Capita
#1991
summary(bs1991$rdpc)
boxplot(bs1991$rdpc,col="Light Blue",main = "Renda per capita nos municípios do Brasil - 1991",
        ylab = "Renda per capita")
arrange(bs1991[,c(238,5,92)],bs1991$rdpc)[1:3,]
#AGUAS DE SÃO PEDRO, SANTANA DE PARNAÍBA E SÃO CAETANO DO SUL-SP - Melhores
#NOVA COLINAS-MA, SANTA FILOMENA-PE, LAGOA DE SÃO FRANCISCO-PI - Piores

summary(bs2$rdpc[bs2$ano==1991])
boxplot(bs2$rdpc[bs2$ano==1991],col = "Red",main = "Renda per capita nos estados do Brasil - 1991",
        ylab = "Renda per capita")
arrange(bs2[bs2$ano==1991,c(3,90)],desc(bs2$rdpc[bs2$ano==1991]))
#DF, SP, RJ - Melhores; MA, PI, PB - Piores

#2000
summary(bs2000$rdpc)
boxplot(bs2000$rdpc,col="Light Blue",main = "Renda per capita nos municípios do Brasil - 2000",
        ylab = "Renda per capita")
arrange(bs2000[,c(238,5,92)],bs2000$rdpc)[1:3,]
#AGUAS DE SÃO PEDRO, SANTANA DE PARNAÍBA E SÃO CAETANO DO SUL-SP - Melhores
#JORDÃO-AC, ACAUÃ-PI, SANTO AMARO DO MARANHÃO-MA - Piores

summary(bs2$rdpc[bs2$ano==2000])
boxplot(bs2$rdpc[bs2$ano==2000],col = "Red",main = "Renda per capita nos estados do Brasil - 2000",
        ylab = "Renda per capita")
arrange(bs2[bs2$ano==2000,c(3,90)],desc(bs2$rdpc[bs2$ano==2000]))
#DF, SP, RJ - Melhores; MA, PI, AL - Piores

#2010
summary(bs2010$rdpc)
boxplot(bs2010$rdpc,col="Light Blue",main = "Renda per capita nos municípios do Brasil - 2010",
        ylab = "Renda per capita")
arrange(bs2010[,c(238,5,92)],desc(bs2010$rdpc))[1:3,]
#SÃO CAETANO DO SUL-SP, NITEROI, VITORIA - Melhores
#MARAJÁ DO SENA, FERNANDO FALCÃO, BELÁGUA-MA- Piores

summary(bs2$rdpc[bs2$ano==2010])
boxplot(bs2$rdpc[bs2$ano==2010],col = "Red",main = "Renda per capita nos estados do Brasil - 2010",
        ylab = "Renda per capita")
arrange(bs2[bs2$ano==2010,c(3,90)],desc(bs2$rdpc[bs2$ano==2010]))
#DF, SP, RJ - Melhores; MA, PI, AL - Piores

##População com AGUA ENCANADA
#1991
summary(bs1991$t_agua)
boxplot(bs1991$t_agua,col="Light Blue",main = "População com água encanada nos municípios do Brasil - 1991",
        ylab = "Porcentagem")
arrange(bs1991[,c(238,5,139)],bs1991$t_agua)
#AGUAS DE SÃO PEDRO-SP, COCAL DO SUL E PAULO LOPES-SC - Melhores
#Algumas cidades da PB,PI,MA,TO,RN e do Norte- Piores

summary(bs2$t_agua[bs2$ano==1991])
boxplot(bs2$t_agua[bs2$ano==1991],col = "Red",main = "População com água encanada nos estados do Brasil - 1991",
        ylab = "Porcentagem")
arrange(bs2[bs2$ano==1991,c(3,137)],desc(bs2$t_agua[bs2$ano==1991]))
#SC, SP, RJ - Melhores; MA, AC, TO - Piores

#2000
summary(bs2000$t_agua)
boxplot(bs2000$t_agua,col="Light Blue",main = "População com água encanada nos municípios do Brasil - 2000",
        ylab = "Porcentagem")
arrange(bs2000[,c(238,5,139)],desc(bs2000$t_agua))
#AGUAS DE SÃO PEDRO-SP, inajá-sc E SANTA GERTRUDES-SP - Melhores
#4 CIDADES DO PI E DUAS DA PARAIBA - Piores

summary(bs2$t_agua[bs2$ano==2000])
boxplot(bs2$t_agua[bs2$ano==2000],col = "Red",main = "População com água encanada nos estados do Brasil - 2000",
        ylab = "Porcentagem")
arrange(bs2[bs2$ano==2000,c(3,137)],desc(bs2$t_agua[bs2$ano==2000]))
#SC, SP, PR - Melhores; MA, AC, PA - Piores

#2010
summary(bs2010$t_agua)
boxplot(bs2010$t_agua,col="Light Blue",main = "População com água encanada nos municípios do Brasil - 2010",
        ylab = "Porcentagem")
arrange(bs2010[,c(238,5,139)],bs2010$t_agua)
#7 CIDADES(1 MG,4 SP E 2 RS) - Melhores
#BARAÚNA E ASSUNÇÃO-PB E MARCOLÂNDIA-PI - Piores

summary(bs2$t_agua[bs2$ano==2010])
boxplot(bs2$t_agua[bs2$ano==2010],col = "Red",main = "População com água encanada nos estados do Brasil - 2010",
        ylab = "Porcentagem")
arrange(bs2[bs2$ano==2010,c(3,137)],desc(bs2$t_agua[bs2$ano==2010]))
#DF, SP, GO - Melhores; AM, AL, PI - Piores
bs2010
