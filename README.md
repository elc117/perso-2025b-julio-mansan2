# Gerador e Quiz de Personagens de Quadrinhos utilizando uma API externa
<br>

## 1. Identificação: 
### Nome: Júlio Augusto de Barros Mansan
### Curso: Sistemas de Informação

<br>

## 2. Tema/Objetivo:

- Desenvolver um programa que contenha duas funções principais: 
    - Gerar um personagem aleatório de quadrinhos quando o usuário clica em um botão;
    - Implementar um quiz onde o usuário recebe uma imagem de determinado personagem e deve escolher corretamente seu nome entre cinco opções. 

## 3. Desenvolvimento:

### 1º Passo: Compreender o funcionamento de API's externas.

- Para este trabalho, cogitei o consumo de dados de uma API externa, pois, dessa maneira, o programa ofertaria uma quantidade mais vasta de dados, o que não seria possível com a criação de uma lista interna.

<br>

- Condizente com o tema do projeto, utilizei a <a href = "https://www.superheroapi.com" target = "_blank">Super Hero API</a>, que contém informações relevantes sobre um grande número de personagens.

<br>

- Após isso, foi necessário compreender o funcionamento de uma API:
    - Em termos técnicos, API é um conjunto de padrões e ferramentas que permite a criação segura e simplificada de aplicações, em razão da integração e comunicação entre software e seus componentes. (<a href="https://www.alura.com.br/artigos/api#bibliografia">Alura</a>); 
    - De forma simplificada, o cliente solicita algo para o servidor através da API e ela retorna alguma coisa;
    - No desenvolvimento web, funciona da seguinte forma:

        <ol>
            <li>O servidor aguarda a requisição;</li>
            <li>O cliente envia uma requisição HTTP para o endpoint do servidor;</li>
            <li>O servidor retorna uma resposta ao cliente (no caso do programa, foi em formato JSON);</li>
            <li>O cliente processa os dados.</li>
        </ol> 

### 2º Passo: Configurar o Web service Scotty como backend.

- Para consumir os dados da API, é necessário fazer as requisições a partir do backend. Para tal processo, utilizei o Scotty, apresentado em aula, que permite a criação de rotas HTTP e retornar arquivos ou JSON. 

<br>

- Visando melhorar a experiência do usuário, configurei algumas rotas que atendem arquivos estáticos (HTML, CSS e JS):
    ````haskell
    -- Página inicial
    get "/" $ file "static/index.html"
    
    -- Arquivos estáticos
    get "/static/script.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "static/script.js"

    get "/static/styles.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/styles.css"

    ````

<br>

- Utilizei duas funções para buscar dados aleatórios da API:
    ````haskell
    randomHeroId :: IO Int
    randomHeroId = randomRIO (1, 731)

    heroUrl :: Int -> String
    heroUrl heroId = "https://superheroapi.com/api/347206d2dac16447fdf234cb9ac6ac76/" ++ show heroId
    ````

<br>

- Configurei rotas endpoint que retornam JSON, e as integrei com a API utilizando ``Wreq``, que permite requisições GET:
    ````haskell
    import qualified Network.Wreq as W

    heroId <- randomHeroId
    r <- W.get (heroUrl heroId)
    ````

<br>

- Para manipular os dados do JSON, foi necessário a utilização de ``Aesons`` e ``Aesons.Lens``;
- O módulo ``Control.Lens`` desempenhou seu papel ao extrair apenas os campos relevantes;
- O operador ``^?`` é usado para acessar um valor dentro de uma estrutura complexa de forma segura, retornando Maybe Value caso o campo exista.
    ````haskell
    import Control.Lens hiding ((.=))
    import Data.Aeson.Lens (key, _String, _Array, values)
    import Data.Aeson (object, (.=), Value)

    getHeroName body = body ^? key "name"
    getBiography body = body ^? key "biography"
    getWork body = body ^? key "work"
    getConnections body = body ^? key "connections"
    getImage body = body ^? key "image"
    
    fetchHero :: IO Value
    fetchHero = do
    heroId <- randomHeroId
    r <- W.get (heroUrl heroId)
    let body = r ^. W.responseBody
    return $ object
        [ "name"        .= getHeroName body
        , "biography"   .= getBiography body
        , "work"        .= getWork body
        , "connections" .= getConnections body
        , "image"       .= getImage body
        ]
    ````
<br>

- O uso de liftIO foi necessário para executar operações IO (requisições HTTP) dentro do monad de Scotty.

<br>

- Para o quiz, não era necessário retornar todos os dados, então utilizei uma função que seleciona apenas a imagem e o nome:
    ````haskell
    selectNameAndImage :: Value -> Value
    selectNameAndImage hero =
    object
      [ "name"  .= (hero ^? key "name")
      , "image" .= (hero ^? key "image")
      ]
    ````

<br>

- Além disso, para o modo quiz, era necessário retornar uma lista com 5 personagens, então utilizei ``replicateM 5`` que retorna uma lista de JSON. Usei ``map selectNameAndImage`` para pegar apenas os atributos necessários.
    ````haskell
    import Control.Monad (replicateM)

    fetchFiveHeroesFiltered :: IO [Value]
    fetchFiveHeroesFiltered = do
    heros <- replicateM 5 fetchHero
    return $ map selectNameAndImage heros
    ````

<br>

- Para construir o projeto, utilizei o Cabal, gerando um executável em Haskell;
- No arquivo ``.cabal`` foram adicionadas as dependências necessárias para execução do projeto.
    ````cabal
    cabal-version:      3.0
    name:               perso-2025b-julio-mansan2
    version:            0.1.0.0
    build-type:         Simple

    executable main
    main-is:          Main.hs
    build-depends:    base,
                      wreq,
                      lens,
                      lens-aeson,
                      aeson,
                      scotty,
                      text,
                      random
    default-language: Haskell2010
    ````

### 3º Passo: Construir o front-end com HTML, CSS e JS, e fazer a conexão com o back-end.

- No front-end, criei duas páginas: ``index.html``, onde o usuário solicita um personagem aleatório e ``quiz.html``, que comporta o quiz.

<br>

- No arquivo ``script.js``, que comporta o JS que manipula o HTML, criei duas funções principais: ``sortearHeroi()`` e ``iniciarQuiz()``:
    <br>

    - ``sortearHeroi()``
        - Faz uma requisição ao backend solicitando o JSON gerado em ``get "/hero"``
            ````javascript
            const response = await fetch('/hero');
            const data = await response.json();   
            ````
        - Mostra os dados na tela para o usuário
            ````javascript
            if (data.image && data.image.url) {
                imgSrc.src = `https://corsproxy.io/?${encodeURIComponent(data.image.url)}`;
            }
            imgSrc.alt = data.name || "Super Herói";
            imgSrc.style.maxWidth = "300px";
            imgSrc.style.height = "auto";

            addInfos(data, informations);

            informations.forEach(info => {
                const listItem = document.createElement("li");
                listItem.textContent = info;
                listInfo.appendChild(listItem);
            });
            ````
        - OBS: as imagens não estavam sendo geradas por bloqueio, então usei Proxy CORS para contorná-lo.
    <br>

    - ``iniciarQuiz()``
        - Essa função inicia o quiz, reseta o placar e chama a função novaRodada, que é responsável por controlar cada etapa do jogo.
            ````javascript
            async function iniciarQuiz() {
                const quizContainer = document.getElementById("quizContainer");
                const quizButton = document.getElementById("inicioQuiz");
                const scoreDisplay = document.getElementById("scoreDisplay");

                quizContainer.innerHTML = "";
                scoreDisplay.textContent = "Score: 0";

                try {
                    quizButton.disabled = true;
                    quizButton.innerHTML = "Ongoing Quiz";
                    let score = 0;
                    await novaRodada(score, quizContainer, scoreDisplay, quizButton);
                } catch (error) {
                    console.error('Erro ao iniciar o quiz:', error);
                }
            }
            ````
    <br>

    - ``novaRodada()``
        - Faz uma requisição ao backend para buscar 5 heróis aleatórios através da rota /quiz.
        - Escolhe aleatoriamente um desses 5 para ser o “correto” e exibe apenas sua imagem.
        - Cria botões com os nomes dos heróis para o usuário tentar adivinhar.
        - Se acertar, incrementa a Score em 1. Caso erre, reseta o quiz.
            ````javascript
            async function novaRodada(score, quizContainer, scoreDisplay, quizButton) {
                const response = await fetch('/quiz');
                const data = await response.json();

                let correto = Math.floor(Math.random() * data.length);

                const imgSrc = document.createElement("img");
                imgSrc.src = `https://corsproxy.io/?${encodeURIComponent(data[correto].image.url)}`;
                quizContainer.appendChild(imgSrc);

                const messageDiv = document.createElement("div");
                quizContainer.appendChild(messageDiv);

                data.forEach((heroi, index) => {
                    const button = document.createElement("button");
                    button.textContent = heroi.name;
                    button.value = index;
                    quizContainer.appendChild(button);
                });

                quizContainer.querySelectorAll("button").forEach(button => {
                button.addEventListener("click", () => {
                quizContainer.querySelectorAll("button").forEach(b => b.disabled = true);

                if (parseInt(button.value) === correto) {
                    score++;
                    scoreDisplay.textContent = `Score: ${score}`;
                    messageDiv.textContent = "✅ Correct!";
                    setTimeout(() => novaRodada(score, quizContainer, scoreDisplay, quizButton), 1000);
                } else {
                    messageDiv.textContent = `❌ Wrong! The correct answer was: ${data[correto].name}. Final Score: ${score}`;
                    setTimeout(() => iniciarQuiz(), 2000);
                }
                    });
                });
            }
            ````

### 4º Passo: Construir os testes para as funções em Haskell.

- Por fim, implementei alguns testes automáticos para algumas funções em Haskell no arquivo ``TestMain.hs``.
- Após isso, adicionei suas dependências dentro do ``.cabal``
    ````cabal
    executable superhero-tests
    main-is:          TestMain.hs
    build-depends:    base,
                      HUnit,
                      aeson,
                      text,
                      lens,
                      lens-aeson
    default-language: Haskell2010
    ````
- Resultado: ``Cases: 4  Tried: 4  Errors: 0  Failures: 0```.

<br>

## 4. Orientação para Execução:

## 5. Resultado:

## 6. Referências: