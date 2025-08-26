import pandas as pd
import matplotlib.pyplot as plt

dados = {
    'Nome':['Ana','Pedro','João'],
    'Idade':[23,34,19],
    'Cidade':[' São Paulo','Rio de Janeiro','Curitiba']
}

df = pd.DataFrame(dados)
print(df)

plt.bar(df['Nome'], df['Idade'])
plt.show()