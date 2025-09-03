import numpy as np
import matplotlib.pyplot as plt

n_pontos = 10000

x = np.random.uniform(0, 1, n_pontos)
y = np.random.uniform(0, 1, n_pontos)

distancia = np.sqrt(x**2 + y**2)

dentro_circulo = distancia <= 1
pi_estimado = 4 * np.sum(dentro_circulo) / n_pontos

print(f"Valor estimado de π: {pi_estimado}")

plt.figure(figsize=(6,6))
plt.scatter(x, y, c=dentro_circulo, cmap='coolwarm', s=1)
plt.title(f'Estimativa de π usando Monte Carlo\nValor estimado: {pi_estimado}')
plt.xlabel('x')
plt.ylabel('y')
plt.show()