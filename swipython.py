from pyswip import Prolog

# creacion del objeto
p = Prolog()

# hacemos la consulta
p.consult("personalities.pl")

R = ""
# recorremos la consulta
for question in p.query("intro_question(X,"+ R +")"):
    print(question['X'])
    R = input(":")



