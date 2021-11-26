from pyswip import Prolog

# creacion del objeto
p = Prolog()

# hacemos la consulta
p.consult("personalities.pl")

consulta = p.query("main.")


    




