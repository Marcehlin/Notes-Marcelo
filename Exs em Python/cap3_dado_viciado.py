import numpy as np
import matplotlib.pyplot as plt

# função que gera um número aleatório e verificando em qual intervalo ele cai
def gerar_amostra_por_intervalos(probabilidades, faces):
    u = np.random.uniform(0, 1)  # Gerando um número aleatório uniforme entre 0 e 1
    limite_inferior = 0  # Limite inferior do intervalo, que começa do 0
    
    # Percorrendo as probabilidades e verificando em qual intervalo o número cai
    for i, p in enumerate(probabilidades):
        limite_superior = limite_inferior + p  # Definindo o limite superior do intervalo
        if limite_inferior <= u < limite_superior:
            return faces[i]  # Retorna a face correspondente ao intervalo
        limite_inferior = limite_superior  # Atualiza o limite inferior para o próximo intervalo

#as faces do dado e as probabilidades associadas ( não uniforme ja que é dado viciado)
faces = [1, 2, 3, 4, 5, 6]
probabilidades = [0.05, 0.1, 0.15, 0.2, 0.25, 0.25]
n_lancamentos = 10000
resultados = [gerar_amostra_por_intervalos(probabilidades, faces) for _ in range(n_lancamentos)] #armazenar resultados

# Contando as frequências de cada face
frequencias = [np.sum(np.array(resultados) == face) / n_lancamentos for face in faces]

# Exibindo os resultados da simulação
print(f"Frequências de cada face após {n_lancamentos} lançamentos:")
for face, freq in zip(faces, frequencias):
    print(f"Face {face}: {freq} (em %)")

# Gráfico das frequências obtidas
plt.figure(figsize=(8,6))
plt.bar(faces, frequencias, color='lightcoral', edgecolor='black')
plt.title(f'Simulação de Lançamentos de um Dado Viciado\n{n_lancamentos} lançamentos')
plt.xlabel('Face do Dado')
plt.ylabel('Frequência de Ocorrência')
plt.grid(True)
plt.show()

# Gráfico das probabilidades ajustadas
plt.figure(figsize=(8,6))
plt.bar(faces, probabilidades, color='skyblue', edgecolor='black')
plt.title('Probabilidades Ajustadas para o Dado Viciado')
plt.xlabel('Face do Dado')
plt.ylabel('Probabilidade')
plt.grid(True)
plt.show()