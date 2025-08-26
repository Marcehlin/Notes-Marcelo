resultado = 2*7
print(f"2*7 é {resultado}")
resultado = resultado**2
print(f"(2*7)^2 é {resultado}")
print(globals())
del(resultado)
print(globals())