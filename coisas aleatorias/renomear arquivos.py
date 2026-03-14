# serve para tirar espaço nos nomes dos arquivos
import os

root_dir = r"D:\umas coisas privadas\D"

for root, dirs, files in os.walk(root_dir):
    for file in files:
        if file.endswith(".txt"):
            old_path = os.path.join(root, file)
            new_name = file.replace(" ", "")
            new_path = os.path.join(root, new_name)

            if old_path != new_path:
                os.rename(old_path, new_path)