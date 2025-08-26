import pandas as pd
dados = {
    'Nome':['Ana','Pedro','João'],
    'Idade':[23,34,19],
    'Cidade':[' São Paulo','Rio de Janeiro','Curitiba']
}

df = pd.DataFrame(dados)
print(df)

print(df[['Cidade','Nome']])

df_filtrado = df[df['Idade'] > 20]
print(df_filtrado)
print()
df.sort_values('Idade',ascending=False,inplace=True)
print(df)