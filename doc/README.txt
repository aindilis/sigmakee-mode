----------------------------------------------------
SigmaKEE-mode: Emacs Major Mode 
----------------------------------------------------

INSTALLATION:

In order to use sigmakee-mode with emacs, please check out the files
with:

	git clone https://github.com/aindilis/sigmakee-mode

Then, take the name of that directory that it was cloned to, and add
the following to your .emacs file (careful to change
"/var/lib/myfrdcsa/codebases/minor/sigmakee-mode" to the value of your
directory)

	(setq load-path
	 (cons "/var/lib/myfrdcsa/codebases/minor/sigmakee-mode" load-path))
	(require 'sigmakee-mode)

