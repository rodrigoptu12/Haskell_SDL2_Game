[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/hiWa6Cqc)
# Proposta Projeto Final 2023/02
**Rodrigo Pereira Couto - 190116510**
**Fernando de Alcantara - 190125586**

## Introdução
Nossa proposta visa resolver o desafio emocionante de criar um jogo em 2D no estilo plataforma, inspirado em clássicos como Super Mario, aproveitando o poder do pacote SDL2 com a linguagem de programação Haskell. Este jogo envolverá a implementação de diversos elementos interativos, incluindo um protagonista ágil, inimigos ameaçadores, obstáculos desafiadores, um sistema de pontuação e, claro, a emoção da conclusão do jogo.

## Como Rodar
      -cd game
      -sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev pkg-config
      -stack install
      -stack run

## Objetivos
1. **Desenvolvimento do Personagem Principal:**
   O coração do jogo será a implementação do personagem principal. Este personagem será representado como uma classe Haskell com os seguintes atributos:
   - Posição (x, y) na tela.
   - Velocidade (x, y).
   - Capacidades de salto e detecção de colisões.

2. **Implementação de Inimigos:**
   Para elevar o nível de desafio, criaremos inimigos que o personagem principal deve evitar ou derrotar. Cada inimigo será implementado como uma classe Haskell com os seguintes atributos:
   - Posição (x, y) na tela.
   - Velocidade (x, y).
   - Atributos de colisão.

3. **Criação de Obstáculos:**
   O mundo do jogo estará repleto de obstáculos que o personagem principal deve superar. Esses obstáculos serão implementados como classes Haskell com os seguintes atributos:
   - Posição (x, y) na tela.
   - Atributos de colisão.

4. **Pontuação e Condição de Vitória:**
   Para tornar o jogo mais envolvente, iremos implementar um sistema de pontuação que se baseará na quantidade de moedas coletadas. O jogo terminará quando o personagem principal atingir uma pontuação exigida.

## Recursos e Bibliotecas
Nossa implementação aproveitará as seguintes bibliotecas e recursos-chave:
- **SDL2:** Utilizaremos a robusta biblioteca SDL2, uma ferramenta poderosa para desenvolvimento de jogos em 2D, garantindo gráficos e interações de alta qualidade.
- **MonadIO:** Aproveitaremos o módulo MonadIO para gerenciar operações de entrada e saída em Haskell, tornando a interatividade do jogo suave e responsiva.

## Complexidade e Desafios Antecipados
Este projeto é classificado como de complexidade média. Enfrentaremos desafios significativos, incluindo:
- **Implementação da Física do Jogo:** Será crucial criar uma simulação realista de física, incluindo a modelagem de fenômenos como a gravidade e a detecção precisa de colisões entre objetos.


