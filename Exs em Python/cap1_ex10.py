import pandas as pd
import matplotlib.pyplot as pyplot

dados = {
    'Dia': ['SEG','TER','QUA','QUI','SEX','SAB','DOM'],
    'Temp' : [22,24,19,21,23,25,20]
}

df = pd.DataFrame(dados)

print(df)

df.plot(x='Dia',y='Temp',kind='line')
pyplot.title('Temperaturas ao Longo dos Dias')
pyplot.show()