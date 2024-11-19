# Introdução à métricas de paisagem no R

## SynEco - I Simpósio Brasileiro de Síntese Ecológica

**Docente**  
Prof. Maurício Vancine

**Duração**  
2 horas

**Resumo**  
O curso tem o intuito de oferecer uma introdução teórica e prática sobre o cálculo de métricas de paisagem utilizando a linguagem R através do pacote *landscapemetrics*. Primeiramente são apresentados os principais conceitos de ecologia da paisagem, métricas de paisagem, geoprocessamento, dados geoespaciais, linguagem R e detalhes do pacote *landscapmetrics*.

---

### Informações aos participantes

**Datas e horários**  
27/11/2024 das 08:30h-10:30h

**Contato**  
Para mais informações ou dúvidas, envie e-mail para Maurício Vancine (mauricio.vancine@gmail.com)

---

### Instruções aos participantes

**Hardware**  
Será necessário que todos usem seus notebooks ou desktops

**Softwares**  
Instalar a versão mais recente do 

1. [R (4.2.2)](https://www.r-project.org)
2. [RStudio](https://www.rstudio.com)

- [Vídeo de instalação do R e do RStudio](https://youtu.be/l1bWvZMNMCM)

#### GNU/Linux (Ubuntu e derivados)

```
# r
sudo apt update -qq
apt install --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt install --no-install-recommends r-base

# rstudio
wget -c https://download1.rstudio.org/desktop/bionic/amd64/rstudio-2022.02.3-492-amd64.deb &&
sudo dpkg -i rstudio-2022.02.3-492-amd64.deb &&
sudo apt install -fy && 
rm rstudio-2022.02.3-492-amd64.deb
```

**Pacotes**
Instalem os pacotes do script abaixo. Para mais detalhes, consultar [capítulo 04](https://analises-ecologicas.com/cap4.html#pacotes-1).

---

## Slides

[slides](https://mauriciovancine.github.io/workshop-r-landscapemetrics/01_slides/slides.html#/)

---

## Script

[script](https://github.com/mauriciovancine/workshop-r-landscapemetrics/blob/main/02_script/script.R)

---