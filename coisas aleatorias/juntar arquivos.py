import os

root_dir = r"D:\umas coisas privadas\D"
output_md = os.path.join(root_dir, "dierry.md")

txt_files = []

# 收集所有 txt 文件
for root, dirs, files in os.walk(root_dir):
    for file in files:
        if file.endswith(".txt"):
            full_path = os.path.join(root, file)
            txt_files.append(full_path)

# 按文件名排序（确保 20XXXXXX 顺序正确）
txt_files.sort(key=lambda x: os.path.basename(x))

with open(output_md, "w", encoding="utf-8") as out:
    for i, path in enumerate(txt_files):
        filename = os.path.splitext(os.path.basename(path))[0]

        with open(path, "r", encoding="utf-8") as f:
            content = f.read().strip()

        # 写入 Markdown
        out.write(f"{filename}\n")
        out.write(f"{content}\n")

        # 最后一个文件不写分割线（可选）
        if i != len(txt_files) - 1:
            out.write("\n---\n\n")

print("✅ 合并完成：", output_md)