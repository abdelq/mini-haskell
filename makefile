rapport: rapport.md
	pandoc -o Rapport_Qbaich-Boivin.pdf rapport.md

clean:
	rm *.pdf
