.PHONY: update-gitignore

update-gitignore:
	curl https://raw.githubusercontent.com/github/gitignore/main/{Global/Emacs,Global/Linux,Global/macOS,OCaml}.gitignore > .gitignore
