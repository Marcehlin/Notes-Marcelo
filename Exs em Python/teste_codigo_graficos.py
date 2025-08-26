import matplotlib.pyplot as plt

x = [0, 1, 2, 3, 4]
y = [0, 1, 4, 9, 16]
plt.plot(x, y)
plt.title('Gráfico de Linha')
plt.show()

categorias = ['A', 'B', 'C']
valores = [5, 7, 3]
plt.bar(categorias, valores)
plt.title('Gráfico de Barras')
plt.show()