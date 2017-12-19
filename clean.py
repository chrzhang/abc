import os

for dirpath, dirnames, filenames in os.walk("."):
    for f in filenames:
        if f.endswith(".o"):
            os.remove(dirpath + "/" + f)
        elif f.endswith(".hi"):
            os.remove(dirpath + "/" + f)
        elif f == "p":
            os.remove(dirpath + "/" + f)
