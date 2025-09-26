async function sortearHeroi() {
    const heroiSorteado = document.getElementById("heroiSorteado");
    const buttonGenerate = document.getElementById("sorteio");
    heroiSorteado.innerHTML = "";
    let informations = [];

    try {
        buttonGenerate.disabled = true;
        buttonGenerate.innerHTML = "Loading...";
        const response = await fetch('/hero');
        const data = await response.json();

        const imgSrc = document.createElement("img");
        const listInfo = document.createElement("ul");

        // Usar proxy CORS para contornar o bloqueio
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

        heroiSorteado.appendChild(imgSrc);
        heroiSorteado.appendChild(listInfo);

    } catch (error) {
        console.error('Erro ao sortear herói:', error);
    }
    buttonGenerate.innerHTML = "Generate character";
    buttonGenerate.disabled = false;
}

function addInfos(data, informations) {
    informations.push(`Name: ${data.name}`);
    informations.push(`Full Name: ${data.biography["full-name"]}`);
    informations.push(`Alter Egos: ${data.biography["alter-egos"]}`);
    informations.push(`Place of Birth: ${data.biography["place-of-birth"]}`);
    informations.push(`First Appearance: ${data.biography["first-appearance"]}`);
    informations.push(`Publisher: ${data.biography["publisher"]}`);
    informations.push(`Alignment: ${data.biography["alignment"]}`);
    informations.push(`Occupation: ${data.work["occupation"]}`);
    informations.push(`Base: ${data.work["base"]}`);
    informations.push(`Group Affiliation: ${data.connections["group-affiliation"]}`);
    informations.push(`Relatives: ${data.connections["relatives"]}`);
}


async function iniciarQuiz() {
    const quizContainer = document.getElementById("quizContainer");
    const quizButton = document.getElementById("inicioQuiz");
    quizContainer.innerHTML = "";

    try {
        quizButton.disabled = true;
        quizButton.innerHTML = "Reset Quiz";

        let score = 0;
        await novaRodada(score, quizContainer);
    } catch (error) {
        console.error('Erro ao iniciar o quiz:', error);
    }
    document.getElementById("inicioQuiz").disabled = false;
}

async function novaRodada(score, quizContainer) {
    quizContainer.innerHTML = "";

    const response = await fetch('/quiz');
    const data = await response.json();

    let correto = Math.floor(Math.random() * data.length);

    const imgSrc = document.createElement("img");
    imgSrc.src = `https://corsproxy.io/?${encodeURIComponent(data[correto].image.url)}`;
    imgSrc.alt = data[correto].name || "Super Herói";
    imgSrc.style.maxWidth = "300px";
    imgSrc.style.height = "auto";
    quizContainer.appendChild(imgSrc);

    data.forEach((heroi, index) => {
        const button = document.createElement("button");
        button.textContent = heroi.name;
        button.value = index;
        quizContainer.appendChild(button);
    });

    quizContainer.querySelectorAll("button").forEach(button => {
        button.addEventListener("click", async () => {
            if (parseInt(button.value) === correto) {
                score++;
                alert(`Correct! Your score is: ${score}`);
                await novaRodada(score, quizContainer);
            } else {
                alert(`Wrong! The correct answer was: ${data[correto].name}. Your final score is: ${score}`);
            }
        });
    });
}
