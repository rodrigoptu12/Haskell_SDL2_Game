[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/hiWa6Cqc)
# Projeto Final - 2023/2

O propósito do projeto final é implementar um problema de maneira criativa, exercitando as 
ideias tratadas durante o curso.

## Primeira tarefa

1. Montar a sua equipe com 1 a 3 membros.
2. A equipe deve propor o projeto que irá desenvolver e escrever a proposta neste README.

A proposta deve descrever:
1. qual é o problema que você pretende resolver;
2. qual é a abordagem que você planeja utilizar (planejo implementar o algoritmo ZZZ descrito no artigo YYY, planejo implementar uma API com essas rotas interagindo com este banco de dados, etc). Aqui o objetivo é que você pense um pouco sobre as possibilidades de solução do seu problema e já faça uma checagem de qual é a complexidade e as dificuldades que você espera encontrar.
3. quais são as principais bibliotecas que você pretende utilizar (para que você faça uma pesquisa inicial sobre aquilo que você já encontra disponível para utilizar). Por exemplo, você pode escolher entre IHP e Yesod para uma API web, entre pipes e conduit para sistemas que envolvem streaming de dados para processamento, etc. 

Prazo: 29/10/23

As propostas serão validadas pela equipe da disciplina.

### Sobre as equipes

A quantidade de membros na equipe vai determinar a complexidade que esperamos tanto da proposta quanto do entregável para o final do semestre. 
A equipe padrão deve ser de 2 membros, mas serão aceitos 3 membros para projetos mais complexos.

Itens que aumentam a complexidade do projeto:
- projetos com muitas funcionalidades (procure definir uma funcionalidade central específica e, se for o caso depois de implementada, aumentar o escopo do projeto);
- metaprogramação com Template Haskell;
- necessidade de linkar bibliotecas externas escritas em outras linguagens com FFI;
- ter que compreender um projeto open source grande e complexo antes de se tornar produtivo na sua proposta;
- ter que lidar com bibliotecas em que a documentação é pobre ou que tenha tipos muito complexos.

### Sobre a apresentação

A apresentação do projeto será realizada pela entrega de um repositório no github contendo, no mínimo:
- um README que descreve:
  1. a funcionalidade do software desenvolvido
  2. modo de uso da solução para que possamos executar o seu entregável;
  3. qual foi a contribuição de cada membro da equipe.
- os códigos-fontes desenvolvidos;
- link para um vídeo de apresentação de até 10 minutos.

No video de apresentação, mostre o que o seu software faz e discuta brevemente a arquitetura do software (como você estruturou a sua solução, como as peças se encaixam e se comunicam). Não mostre nem comente todo o código-fonte, nós iremos ler ele no seu repositório; mas é interessante comentar se houver alguma ideia ou sacada que você achou importante para a solução ou que foi fruto do estudo na disciplina.

## Ideias de Projeto

O seu projeto não precisa ser uma solução inovadora.
Nossa sugestão é que você tente atacar um problema que você já tem alguma familiaridade, seja por causa do trabalho, estágio, projeto de pesquisa ou hobby.
Se você já resolveu um problema no trabalho em outra linguagem e gostaria de explorar como seria a solução em Haskell, será uma proposta bem vinda.

Segue uma lista de ideias para o seu projeto, caso ainda não tenha um tema:

- Interpretador de linguagem de programação ou de domínio específico (calculadora cientfícica, lisp, subset C);
- Chat Bot (Telegram, Whatsapp, Discord, Twitter/X);
- Compressores de dados/imagens (huffman, dyadic, qoi, qualquer adaptativo);
- Jogos (snake, pacman, jogo da velha, 2048, xadrez, damas, combate);
- Contribuição para um projeto Open Source: implementar 1 nova feature ou 2 PRs de documentação (em Haskell);
- API REST;
- Mini Docker;
- Working Hours Manager;
- Github TODO Tracker;
- [Musializer](https://youtube.com/playlist?list=PLpM-Dvs8t0Vak1rrE2NJn8XYEJ5M7-BqT&si=BMcgzbsGsb7D7y35);
- Replicar alguma ferramenta clássica de linha de comando (grep, find, awk);
- Emulador (JVM, Nintendo64, Atari, GameBoy).

## Bibliotecas em Destaque

Nós esperamos que você utilize bibliotecas de terceiros para agilizar o desenvolvimento da sua solução.
A seguir destacamos algumas que você pode precisar e são consideradas canônicas pela comunidade:

- [Data.List](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html)
- [Data.Text](https://hackage.haskell.org/package/text-2.1/docs/Data-Text.html)
- [Data.Bytestring](https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/Data-ByteString.html)
- [Vector](https://hackage.haskell.org/package/vector)
- [Control.Monad](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html)
- [STM](https://hackage.haskell.org/package/stm)
- [MTL](https://hackage.haskell.org/package/mtl)/[Transformers](https://hackage.haskell.org/package/transformers)
- [Containers](https://hackage.haskell.org/package/containers)
- [Alex](https://hackage.haskell.org/package/alex)/[Happy](https://hackage.haskell.org/package/happy)
- [Megaparsec](https://hackage.haskell.org/package/megaparsec)
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck)/[Tasty](https://hackage.haskell.org/package/tasty)/[Hspec](https://hackage.haskell.org/package/hspec)/[Hedgehog](https://hackage.haskell.org/package/hedgehog)
- [Criterion](https://hackage.haskell.org/package/criterion)
- [OptParse-Applicative](https://hackage.haskell.org/package/optparse-applicative)
- [Brick](https://hackage.haskell.org/package/brick)
- [Conduit](https://hackage.haskell.org/package/conduit)/[Pipe](https://hackage.haskell.org/package/pipes)
- [Gloss](https://hackage.haskell.org/package/gloss)

## Materiais de Suporte

Você pode encontrar muitas outras referências de projetos e de bibliotecas nos links a seguir:

- [post-rfc](https://github.com/Gabriella439/post-rfc/blob/main/sotu.md)
- [awesome-haskell](https://github.com/uhub/awesome-haskell)

## Projetos de Inspiração

A seguir uma lista de projetos feitos por alunos que fizeram o curso em semestres anteriores. Tomem-nos como inspiração e ponto de norteamento para qual a escala que o projeto deve ter.

- [Storm Point Cloud](https://github.com/Prof-Edil/projeto-storm-point-cloud)
- [Banking API](https://github.com/Prof-Edil/projeto-banking-api)
- [Programmatic Generated Images Library](https://github.com/Prof-Edil/projeto-programmatic-generated-images-library)
- [JIT Compiler](https://github.com/Prof-Edil/projeto-JIT-compiler)
- [Chess](https://github.com/Prof-Edil/projeto-chess)
- [Request Parser](https://github.com/Prof-Edil/projeto-request-parser)
