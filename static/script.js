async function sortearHeroi() {
    try {
        const response = await fetch('/hero');
        const data = await response.json();

        console.log('Herói sorteado:', data.name);

    } catch (error) {
        console.error('Erro ao sortear herói:', error);
    }
}