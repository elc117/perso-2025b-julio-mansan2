async function sortearHeroi() {
    const heroiSorteado = document.getElementById("heroiSorteado");
    heroiSorteado.innerHTML = "";
    let informations = [];

    try {
        document.getElementById("sorteio").disabled = true; 
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
    document.getElementById("sorteio").disabled = false;
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