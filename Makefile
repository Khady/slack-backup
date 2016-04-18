OB = ocamlbuild -use-ocamlfind

all: slack-backup

slack-backup:
	$(OB) slack_backup.native

clean:
	$(OB) -clean
