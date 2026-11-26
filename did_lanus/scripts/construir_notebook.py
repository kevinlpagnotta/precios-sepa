"""Convierte did_lanus.R (celdas marcadas con '## MD' y '## CODE') en did_lanus.ipynb con kernel de R."""
import nbformat, re, sys
src = open('did_lanus.R', encoding='utf-8').read()
partes = re.split(r'^## (MD|CODE)\n', src, flags=re.M)[1:]
nb = nbformat.v4.new_notebook()
nb.metadata = {'kernelspec': {'name': 'ir', 'display_name': 'R', 'language': 'R'},
               'language_info': {'name': 'R'}}
for tipo, txt in zip(partes[0::2], partes[1::2]):
    txt = txt.rstrip('\n')
    if tipo == 'MD':
        nb.cells.append(nbformat.v4.new_markdown_cell('\n'.join((l[2:] if l.startswith('# ') else ('' if l == '#' else l)) for l in txt.split('\n'))))
    else:
        nb.cells.append(nbformat.v4.new_code_cell(txt))
nbformat.write(nb, 'did_lanus.ipynb'); print(len(nb.cells), 'celdas')
